{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType =
  responseLBS status [(hContentType, renderContentType contentType)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404 

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400 

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  do 
    t <- mkTopic topic
    c <- mkCommentText $ lazyByteStringToStrictText comment
    return $ AddRq t c
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic =
  fmap ViewRq $ mkTopic topic
  
mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq


-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic = resp400 PlainText "Empty Topic"
mkErrorResponse EmptyComment = resp400 PlainText "Empty Comment Text"
mkErrorResponse UnknownRequest = resp404 PlainText "Unknown request type"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest request
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  | requestMethod request == "GET" 
      = let path = pathInfo request
        in case path of 
          ["list"] -> return mkListRequest 
          [t, "view"] -> return $ mkViewRequest t
          _ -> return $ Left UnknownRequest 
  | requestMethod request == "POST"
      = let path = pathInfo request
        in case path of
          [t, "add"] -> mkAddRequest t <$> strictRequestBody request
          _ -> return $ Left UnknownRequest
  | otherwise = return $ Left UnknownRequest 
                          

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "Add comment request not yet implemented"
handleRequest (ViewRq _) = Right $ resp200 PlainText "View request not yet implemented"
handleRequest ListRq  = Right $ resp200 PlainText "List request not yet implemented"
-- handleRequest _ = Left UnknownRequest 

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app req cb =
  do 
    rqTypeE <- mkRequest req
    let response = rqTypeE >>= handleRequest
    case response of 
      Right resp -> cb resp
      Left err -> cb $ mkErrorResponse err
    
runApp :: IO ()
runApp = run 3000 app
