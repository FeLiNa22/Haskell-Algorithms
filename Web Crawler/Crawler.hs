module Crawler where
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8

import System.Environment
import Network.HTTP

main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
    
