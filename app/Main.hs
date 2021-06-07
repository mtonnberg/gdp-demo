{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api.ServantApp (runApp)

main :: IO ()
main = do
  let appConfigPath = "app.config"
  runApp appConfigPath
