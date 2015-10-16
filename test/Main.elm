module Main where

import Text

import ElmTest.Test exposing (Test, suite)
import ElmTest.Runner.Element as GraphicsRunner
import ElmTest.Runner.Console as ConsoleRunner

import IO.Runner


allTests : Test
allTests =
  suite "Collision" []                

        
port requests : Signal IO.Runner.Request
port requests =
  IO.Runner.run responses (ConsoleRunner.runDisplay allTests)
    

port responses : Signal IO.Runner.Response
