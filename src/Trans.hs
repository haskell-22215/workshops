module Trans where


-- import Control.Monad.Trans.Maybe

-- import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans.State
-- foo :: Int -> StateT [Int] (Reader Int) Int

-- import Control.Monad.State
-- import Control.Monad.Trans.Reader
-- foo :: Int -> ReaderT Int (State [Int]) Int

-- foo i = do
--   baseCounter <- ask
--   let newCounter = baseCounter + i
--   put [baseCounter, newCounter]
--   return newCounter

-- example1 :: MaybeT IO ()
-- example1 = do
--    lift $ putStrLn "Hello, world!"

inc :: Monad m => StateT Int m ()
inc = modify (\s -> s + 1)

-- example2 :: StateT Int (Writer [Int]) ()
example2 :: StateT Int IO ()
example2 = do
  inc
  x <- get
  liftIO $ putStrLn (show x)
  -- tell [x]
  inc
  y <- get
  liftIO $ putStrLn (show y)
  -- tell [y]
  inc
  

-- type Writer l r = WriterT l Identity r
