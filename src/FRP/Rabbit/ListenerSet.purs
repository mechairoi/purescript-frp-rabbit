module FRP.Rabbit.ListenerSet
  ( ListenerSet
  , empty
  , add
  , notify
  ) where

import qualified Data.Map as Map
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Array(filter)

newtype ListenerSet eff = ListenerSet [Ref (Eff eff Unit)]

empty :: ListenerSet
empty = ListenerSet []

add :: forall eff eff2 eff3. Ref (ListenerSet eff) -> Eff eff Unit -> Eff (ref :: Ref | eff2) (Eff (ref :: Ref | eff2) Unit)
add ref listener = do
  ListenerSet currentList <- readRef ref
  listenerRef <- newRef listener
  writeIORef ref $ ListenerSet $ listenerRef : currentList
  return $ modifyIORef ref (\(ListenerSet ls) -> ListenerSet $ filter (\l == listenerRef) ls)

notify :: ListenerSet -> IO ()
notify (ListenerSet map) = sequence_ (Map.elems map)


-- module FRP.Rabbit.ListenerSet
--   ( ListenerSet
--   , empty
--   , add
--   , notify
--   ) where

-- import qualified Data.Map as Map
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Ref

-- newtype ListenerSet = ListenerSet (Map Int (IO ()))

-- empty :: ListenerSet
-- empty = ListenerSet Map.empty

-- add :: IORef ListenerSet -> IO () -> IO (IO ())
-- add setRef listener = do
--                               ListenerSet currentMap <- readIORef setRef
--                               let

--                                   newKey | Map.null currentMap = minBound
--                                          | otherwise           = succ (fst (Map.findMax currentMap))

--                               writeIORef setRef
--                                          (ListenerSet $ Map.insert newKey listener currentMap)
--                               return $ modifyIORef setRef
--                                                    (\(ListenerSet map) -> ListenerSet $
--                                                                           Map.delete newKey map)

--     notify :: ListenerSet -> IO ()
--     notify (ListenerSet map) = sequence_ (Map.elems map)
