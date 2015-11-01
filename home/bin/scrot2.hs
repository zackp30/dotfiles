{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Shelly
import Data.Text as T
import System.Directory (removeFile)
import System.Environment (getArgs)
import System.IO.Error
default (T.Text)

cleanTmp :: IO ()
cleanTmp = do
  r <- tryIOError $ removeFile "./tmp.png"
  case r of
    Left err -> putStrLn $ "error: " ++ show err
    Right err -> putStrLn "error: tmp.png does not exist, continuing as normal, this is to be expected"

handleArgs :: [String] -> IO ()
handleArgs ["window"] = shelly $ verbosely $ do
    liftIO $ cleanTmp
    cmd "maim" ["./tmp.png"]
    cmd "shoot" ["./tmp.png"] >>= cmd "xclip" ["-selection", "c"]
handleArgs [] = shelly $ verbosely $ do
    liftIO $ cleanTmp
    cmd "maim" ["-s", "./tmp.png"]
    cmd "shoot" ["./tmp.png"] >>= cmd "xclip" ["-selection", "c"]
handleArgs [x] = putStrLn $ "Unknown argument: " ++ x
handleArgs (x:xs) = do putStrLn $ "Unknown argument: " ++ x; handleArgs xs

main = getArgs >>= handleArgs
