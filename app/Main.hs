{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.ApiImplementation (runApp)

main :: IO ()
main = do
  let appConfigPath = "app.config"
  runApp appConfigPath
