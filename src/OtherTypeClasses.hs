module OtherTypeClasses where

import Control.Monad
import Control.Monad.Cont

import Control.Monad.Writer

import Control.Applicative

runContId = flip runCont $ id

parseInput :: String -> Maybe String
parseInput name = runContId $
  callCC $ \exit1 -> do
    when (null name) (exit1 Nothing)
    n' <- callCC $ \exit2 -> do
      when (head name == 'A') (exit1 $ return name)
      forM_ name $ \v -> do
        when (v == 'z') (exit1 Nothing)
      exit1 . return $ reverse name
    return . Just $ mconcat ["Welcome, ", name, "!"]

badAction :: IO ()
badAction = do
  let _ = [putStrLn "Hello", putStrLn "World"]
  return ()

goodAction :: IO ()
goodAction = do
  sequence_ [putStrLn "Hello", putStrLn "World"]

forExample :: IO ()
forExample = do
  let xs = ["Hello", "World"]
  forM_ xs (\x -> putStrLn x)

mapM_example3 :: IO ()
mapM_example3 = do
  let xs = ["Hello", "World"]
  mapM_ (\x -> putStrLn x) xs

forWriter :: ((), String)
forWriter = runWriter $ do
  let xs = ["Hello", "World"]
  forM_ xs (\x -> tell $ x ++ "\n")

--traverseExample = do
--  let xs = ["Hello", "World"]
--  _ <- traverse xs (\x -> putStrLn x)
--  return ()

traverseExample1 :: Maybe [Int]
traverseExample1 = traverse Just [1, 2, 3, 4]

traverseExample2 :: Maybe [Int]
traverseExample2 = traverse id [Just 1, Just 2, Just 3, Just 4]

mapM_example :: Maybe [Int]
mapM_example = mapM Just [1, 2, 3, 4]

mapM_example2 :: Maybe [Int]
mapM_example2 = mapM id [Just 1, Just 2, Nothing, Just 4]

-- for = flip traverse

partiallyDefinedFunction :: Double -> Maybe Double
partiallyDefinedFunction x = do
  guard ((abs x) > 0.0000001)
  return (10 / x)
