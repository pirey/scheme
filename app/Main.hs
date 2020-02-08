module Main where

import Control.Monad
import Scheme
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
