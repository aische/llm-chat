module LLM.Core.ProviderUtils
  ( lenientConfig,
    readAll,
    handleStreamResponse,
  )
where

import Data.ByteString qualified as BS
import Data.Text.Encoding (decodeUtf8)
import LLM.Core.Types (LLMError (HttpError), LLMResult)
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
      pure $ Left $ HttpError status (decodeUtf8 (BS.concat chunks))
    else handler (HC.responseBody resp)
