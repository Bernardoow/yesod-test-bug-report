
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.Api where
import Import
import Data.Aeson (withObject, pairs, decode)

import qualified Data.Aeson.Types as J

data Response1 = Response1 {
      msg :: Text
} deriving (Show, Eq)

instance ToJSON Response1 where
    toJSON (Response1 x) = object [ "msg" .= x ]

data Person = Person 
    { age :: Int
    }   
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "age"

instance ToJSON Person where
  toJSON (Person age) = object ["age" .= age]

  toEncoding (Person age) = pairs ("age" .= age )

postStartTheGameR :: Handler Value
postStartTheGameR = do
    game <- lookupSession "game"
    case game of
        Nothing -> do
            setSession "game"  "1"
            sendStatusJSON status201 (Response1 { msg = "OK"})
        Just _ -> do
            sendStatusJSON status400 (Response1 { msg = "Fail"})


postCreateRobotR :: Handler Value
postCreateRobotR = do
    maybeGameSession <- lookupSession "game"
    case maybeGameSession of 
        Nothing ->
            sendStatusJSON status400 (Response1 { msg = "Fail"})
        Just _ -> do
            maybePosition <- (parseCheckJsonBody :: Handler (J.Result Person))
            case maybePosition of
                J.Error _ -> 
                    sendStatusJSON status400 (Response1 { msg = "Invalid payload. It must contains age."})
                J.Success _ ->
                    sendStatusJSON status201 (Response1 { msg = "OK"})
