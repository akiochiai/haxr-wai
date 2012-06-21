{-# LANGUAGE OverloadedStrings #-}
module Network.XmlRpc.Wai (waiXmlRpcServer) where

import Control.Monad.IO.Class
import Control.Monad.Error

import Network.HTTP.Types
import qualified Network.Wai as W

import Data.Conduit (($$), ($=), (=$))
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC

import Data.Monoid
import qualified Blaze.ByteString.Builder as BL

import qualified Network.XmlRpc.Server as XR
import qualified Network.XmlRpc.Internals as XRI


handlerSink :: [(String, XR.XmlRpcMethod)] -> C.Sink ByteString (C.ResourceT IO) W.Response
handlerSink ms = C.sinkState mempty push close
    where
        push builder input = return $ C.StateProcessing (builder <> BL.fromByteString input)
        close :: BL.Builder -> C.ResourceT IO W.Response
        close builder = do let bs = BL.toByteString builder 
                           ebody <- liftIO $ runErrorT $ dispatch $ BC.unpack bs
                           case ebody of
                                Left e     -> do liftIO $ putStrLn e
                                                 return $ W.responseLBS internalServerError500 [] $ LBC.pack ""
                                Right body -> do liftIO $ LBC.putStrLn body
                                                 return $ W.responseLBS ok200 [] body

        dispatch :: String -> XRI.Err IO LBC.ByteString
        dispatch req = do call <- XRI.parseCall req
                          result <- XR.methods ms call
                          let res = XRI.renderResponse result
                          return res

waiXmlRpcServer :: [(String, XR.XmlRpcMethod)] -> W.Application
waiXmlRpcServer ms = app
    where
        app :: W.Request -> C.ResourceT IO W.Response
        app req = (W.requestBody req) $$ (handlerSink ms)
