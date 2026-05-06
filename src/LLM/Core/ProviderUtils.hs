module LLM.Core.ProviderUtils
  ( lenientConfig,
    readAll,
    handleStreamResponse,
    normalizeSchemaOpenAI,
    stripJsonFences,
    stripBounds,
  )
where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Vector qualified as V
import LLM.Core.Types (LLMError (HttpError), LLMRes (ResError), LLMResult (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Req (HttpConfig, defaultHttpConfig, httpConfigCheckResponse)
import Network.HTTP.Types.Status (statusCode)

-- | Don't let req throw on non-2xx; we handle errors ourselves
lenientConfig :: HttpConfig
lenientConfig =
  defaultHttpConfig
    { httpConfigCheckResponse = \_ _ _ -> Nothing
    }

-- | Accumulate all chunks from a BodyReader for error messages
readAll :: HC.BodyReader -> IO [BS.ByteString]
readAll br = do
  chunk <- HC.brRead br
  if BS.null chunk then pure [] else (chunk :) <$> readAll br

-- | Handle streaming response: check status, read error body or delegate
-- to the provider-specific stream parser.
handleStreamResponse :: HC.Response HC.BodyReader -> (HC.BodyReader -> IO LLMResult) -> IO LLMResult
handleStreamResponse resp handler = do
  let status = statusCode (HC.responseStatus resp)
  if status /= 200
    then do
      chunks <- readAll (HC.responseBody resp)
      pure $ ResError $ HttpError status (decodeUtf8 (BS.concat chunks))
    else handler (HC.responseBody resp)

-- | Recursively enforce OpenAI structured output constraints:
--   every object node gets additionalProperties:false and all keys in required.
normalizeSchemaOpenAI :: Value -> Value
normalizeSchemaOpenAI (Object o) =
  let o' = fmap normalizeSchemaOpenAI o
      fixed = case KM.lookup "type" o' of
        Just (String "object") ->
          let props = case KM.lookup "properties" o' of
                Just (Object p) -> KM.keys p
                _ -> []
              withAP = KM.insert "additionalProperties" (Bool False) o'
           in -- withReq = KM.insert "required" (Array (V.fromList (map (String . toText) props))) withAP
              withAP
        _ -> o'
   in Object fixed
normalizeSchemaOpenAI (Array a) = Array (fmap normalizeSchemaOpenAI a)
normalizeSchemaOpenAI v = v

stripJsonFences :: Text -> Text
stripJsonFences t =
  let t' = T.strip t
      t'' =
        if "```" `T.isPrefixOf` t'
          then T.strip . T.drop 1 . T.dropWhile (/= '\n') $ t'
          else t'
      t''' =
        if "```" `T.isSuffixOf` t''
          then T.strip $ T.dropEnd 3 t''
          else t''
   in t'''

-- | Recursively removes "minimum" and "maximum" keys from a JSON Value, also $comment fields
stripBounds :: Value -> Value
stripBounds (Object obj) =
  Object $
    KM.fromList
      [ (k, stripBounds v)
        | (k, v) <- KM.toList obj,
          k /= "minimum",
          k /= "maximum",
          k /= "$comment"
      ]
stripBounds (Array arr) = Array (fmap stripBounds arr)
stripBounds other = other
