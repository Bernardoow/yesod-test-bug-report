{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiSpec (spec) where
import Data.Aeson
import TestImport
import Network.HTTP.Types.Header

spec :: Spec
spec = withApp $ do

    describe "StartGame" $ do
        it "start the game" $ do
            post StartTheGameR
            statusIs 201         

        it "start the game twice" $ do
            post StartTheGameR
            statusIs 201

            post StartTheGameR
            statusIs 400

    describe "CreateRobotR" $ do
        it "create robot without game created" $ do
            post CreateRobotR
            statusIs 400

        it "create robot with game created and valid position" $ do
            post StartTheGameR
            statusIs 201

            request $ do
                setRequestBody $ encode $ object ["age" .= (10 :: Integer)]
                setMethod "POST"
                setUrl CreateRobotR
                addRequestHeader (hContentType, "application/json")
            
            statusIs 201

        it "create robot with game created and valid positionV2" $ do
            post StartTheGameR
            statusIs 201

            postBody CreateRobotR (encode $ object ["age" .= (10 :: Integer)])
            
            statusIs 201