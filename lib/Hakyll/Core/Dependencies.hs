--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Hakyll.Core.Dependencies
    ( Dependency (..)
    , makePatternDependency
    , DependencyFacts
    , outOfDate
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                  (foldM, forM_, unless, when)
import           Control.Monad.Reader           (ask)
import           Control.Monad.RWS             (RWST, runRWST)
import qualified Control.Monad.State            as State
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Writer           (tell)
import           Data.Binary                    (Binary (..), getWord8,
                                                 putWord8)
import           Data.List                      (find)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Typeable                  (Typeable)

--------------------------------------------------------------------------------
import           Hakyll.Core.Identifier
import           Hakyll.Core.Identifier.Pattern
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider           (Provider, resourceMetadata)


--------------------------------------------------------------------------------
data Dependency
    = PatternDependency Pattern (Set Identifier)
    | IdentifierDependency Identifier
    | MetadataDependency Identifier Metadata
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Binary Dependency where
    put (PatternDependency p is) = putWord8 0 >> put p >> put is
    put (IdentifierDependency i) = putWord8 1 >> put i
    put (MetadataDependency i meta) = putWord8 2 >> put i >> put (BinaryMetadata meta)
    get = getWord8 >>= \t -> case t of
        0 -> PatternDependency <$> get <*> get
        1 -> IdentifierDependency <$> get
        2 -> (\i meta -> MetadataDependency i $ unBinaryMetadata meta) <$> get <*> get
        _ -> error "Data.Binary.get: Invalid Dependency"

--------------------------------------------------------------------------------
makePatternDependency :: MonadMetadata m => Pattern -> m Dependency
makePatternDependency pattern = do
    matches' <- getMatches pattern
    return $ PatternDependency pattern (S.fromList matches')


--------------------------------------------------------------------------------
type DependencyFacts = Map Identifier [Dependency]


--------------------------------------------------------------------------------
outOfDate
    :: [Identifier]     -- ^ All known identifiers
    -> Provider         -- ^ Current compiler provider
    -> Set Identifier   -- ^ Initially out-of-date resources
    -> DependencyFacts  -- ^ Old dependency facts
    -> IO (Set Identifier, DependencyFacts, [String])
outOfDate universe provider ood oldFacts = do
    (_, state', logs) <- runRWST actions depread depstate
    return ( S.union (dependencyOod state') (dependencyMetadataOod state')
           , dependencyFacts state'
           , logs
           )
  where
    depread = DependencyRead universe provider
    depstate = DependencyState oldFacts ood S.empty
    actions = do
        checkNew
        checkChangedPatterns
        checkChangedMetadata
        bruteForceDirect
        bruteForceMetadata


--------------------------------------------------------------------------------
data DependencyRead = DependencyRead
    { dependencyIds      :: [Identifier]
    , dependencyProvider :: Provider
    }

--------------------------------------------------------------------------------
data DependencyState = DependencyState
    { dependencyFacts       :: DependencyFacts
    , dependencyOod         :: Set Identifier
    , dependencyMetadataOod :: Set Identifier
    } deriving (Show)


--------------------------------------------------------------------------------
type DependencyM a = RWST DependencyRead [String] DependencyState IO a


--------------------------------------------------------------------------------
markOod :: Identifier -> DependencyM ()
markOod id' = State.modify $ \s ->
    s {dependencyOod = S.insert id' $ dependencyOod s}


--------------------------------------------------------------------------------
markMetadataOod :: Identifier -> DependencyM ()
markMetadataOod id' = State.modify $ \s ->
    s {dependencyMetadataOod = S.insert id' $ dependencyMetadataOod s}


--------------------------------------------------------------------------------
directDependenciesFor :: Identifier -> DependencyM [Identifier]
directDependenciesFor id' = do
    facts <- dependencyFacts <$> State.get
    return $ concatMap dependenciesFor' $ fromMaybe [] $ M.lookup id' facts
  where
    dependenciesFor' (IdentifierDependency i) = [i]
    dependenciesFor' (PatternDependency _ is) = S.toList is
    -- Metadata dependencies are nothing because out-of-date metadata handled
    -- separately
    dependenciesFor' (MetadataDependency _ _) = []


--------------------------------------------------------------------------------
metadataDependenciesFor :: Identifier -> DependencyM [Identifier]
metadataDependenciesFor id' = do
    facts <- dependencyFacts <$> State.get
    return $ concatMap dependenciesFor' $ fromMaybe [] $ M.lookup id' facts
  where
    dependenciesFor' (IdentifierDependency _) = []
    dependenciesFor' (PatternDependency _ _) = []
    dependenciesFor' (MetadataDependency i _) = [i]


--------------------------------------------------------------------------------
checkNew :: DependencyM ()
checkNew = do
    universe <- dependencyIds <$> ask
    facts    <- dependencyFacts <$> State.get
    forM_ universe $ \id' -> unless (id' `M.member` facts) $ do
        tell [show id' ++ " is out-of-date because it is new"]
        markOod id'


--------------------------------------------------------------------------------
checkChangedPatterns :: DependencyM ()
checkChangedPatterns = do
    facts <- M.toList . dependencyFacts <$> State.get
    forM_ facts $ \(id', deps) -> do
        deps' <- foldM (changedPatterns id') [] deps
        State.modify $ \s -> s
            {dependencyFacts = M.insert id' deps' $ dependencyFacts s}
  where
    changedPatterns _   ds (IdentifierDependency i) =
        return $ IdentifierDependency i : ds
    changedPatterns id' ds (PatternDependency p ls) = do
        universe <- dependencyIds <$> ask
        let ls' = S.fromList $ filterMatches p universe
        if ls == ls'
            then return $ PatternDependency p ls : ds
            else do
                tell [show id' ++ " is out-of-date because a pattern changed"]
                markOod id'
                return $ PatternDependency p ls' : ds
    changedPatterns _   ds (MetadataDependency i meta) = 
        return $ MetadataDependency i meta : ds


--------------------------------------------------------------------------------
checkChangedMetadata :: DependencyM ()
checkChangedMetadata = do
    facts <- M.toList . dependencyFacts <$> State.get
    forM_ facts $ \(id', deps) -> forM_ deps (changedMetadata id')
  where
    changedMetadata _   (IdentifierDependency _) = 
        return ()
    changedMetadata _   (PatternDependency _ _) = 
        return ()
    changedMetadata id' (MetadataDependency i meta) = do
        provider <- dependencyProvider <$> ask
        meta' <- liftIO $ resourceMetadata provider id'
        if meta == meta'
           then return ()
           else do
               tell [show id' ++ " is out-of-date because the metadata from "
                    ++ show i ++ " changed"]
               markMetadataOod id'
               return ()

--------------------------------------------------------------------------------
bruteForceDirect :: DependencyM ()
bruteForceDirect = do
    todo <- dependencyIds <$> ask
    go todo
  where
    go todo = do
        (todo', changed) <- foldM check ([], False) todo
        when changed (go todo')

    check (todo, changed) id' = do
        deps <- directDependenciesFor id'
        ood  <- dependencyOod <$> State.get
        case find (`S.member` ood) deps of
            Nothing -> return (id' : todo, changed)
            Just d  -> do
                tell [show id' ++ " is out-of-date because " ++
                    show d ++ " is out-of-date"]
                markOod id'
                return (todo, True)


--------------------------------------------------------------------------------
bruteForceMetadata :: DependencyM ()
bruteForceMetadata = do
    todo <- dependencyIds <$> ask
    -- No recursion, just a single pass through to find out-of-date metadata
    forM_ todo checkId
  where
    checkId id' = do
        deps <- metadataDependenciesFor id'
        ood <- dependencyMetadataOod <$> State.get
        case find (`S.member` ood) deps of
            Nothing -> return ()
            Just d  -> do
                tell [show id' ++ " is out-of-date because " ++
                    show d ++ " has out-of-date metadata"]
                markOod id'
                return ()

