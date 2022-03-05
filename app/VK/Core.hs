{-# LANGUAGE GADTs, TemplateHaskell #-}

module VK.Core where

import Data.Text (Text)

import Polysemy (Effect, InterpreterFor, Member, makeSem)
import Polysemy.Http (Http, QueryKey, QueryValue)
import Network.HTTP.Client (BodyReader)

import Data.Aeson

class FromJSON response => VKEndoint endpoint request response | endpoint -> request
                                                               , endpoint -> response where
    path :: endpoint -> Text
    requestToOption :: endpoint -> request -> [(QueryKey, Maybe QueryValue)]
    maxCount :: endpoint -> Maybe Integer

data VK :: Effect where
    ReqVK :: VKEndoint endpoint request response => endpoint -> request -> VK m response

makeSem ''VK
