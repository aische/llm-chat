module Tools.History (historyTool) where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    object,
    withObject,
    (.:),
  )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import LLM.Core.Types
  ( Conversation,
    Tool (..),
    ToolCall (tcName),
    ToolContext (tcConversation, tcWindowOffset),
    ToolDef (ToolDef, toolDescription, toolName, toolParameters),
    ToolResult (trContent, trName),
    Turn (..),
  )

historyTool :: Tool
historyTool =
  Tool
    { toolDef =
        ToolDef
          { toolName = "get_history",
            toolDescription =
              "Retrieve earlier conversation history that is not in your current context window. "
                <> "Pass chunk=0 for the most recent hidden history, chunk=1 for the one before that, etc. "
                <> "Returns an empty result when there is no more history.",
            toolParameters = historySchema
          },
      toolExecute = getHistory
    }

historySchema :: Value
historySchema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "chunk"
              .= object
                [ "type" .= ("integer" :: Text),
                  "description" .= ("0 = most recent hidden chunk, 1 = the one before that, etc." :: Text)
                ]
          ],
      "required" .= (["chunk"] :: [Text])
    ]

-- | Return a chunk of hidden conversation history.
-- The hidden prefix is everything before 'tcWindowOffset'. It is split
-- into window-sized pages whose boundaries align to 'UserTurn' starts,
-- working backward from the window offset.
-- Chunk 0 is the most recent hidden page, chunk 1 the one before, etc.
getHistory :: ToolContext -> Value -> IO Text
getHistory ctx args = do
  let chunkIdx = fromMaybe 0 $ parseMaybe parseChunk args
      hidden = take (tcWindowOffset ctx) (tcConversation ctx)
  if null hidden
    then pure "(no earlier history)"
    else do
      let -- Count user messages in the visible window to determine page size
          nUserMessages = countUserTurns (drop (tcWindowOffset ctx) (tcConversation ctx))
          -- Chunk the hidden prefix into pages of N user messages each
          chunks = chunkBackward nUserMessages hidden
      if chunkIdx < 0 || chunkIdx >= length chunks
        then pure "(no more history)"
        else pure $ formatChunk (chunks !! chunkIdx)

-- | Count the number of 'UserTurn's in a conversation.
countUserTurns :: Conversation -> Int
countUserTurns = length . filter isUserTurn

isUserTurn :: Turn -> Bool
isUserTurn (UserTurn _) = True
isUserTurn _ = False

-- | Split a conversation into pages of @n@ user messages each, working
-- backward from the end. Each page starts at a 'UserTurn'.
-- Chunk 0 is the most recent page, chunk 1 the one before, etc.
-- The oldest chunk (highest index) may contain fewer than @n@ user messages.
chunkBackward :: Int -> Conversation -> [Conversation]
chunkBackward _ [] = []
chunkBackward n conv = reverse (go (length conv) [])
  where
    go 0 acc = acc
    go end acc =
      let start = findNthUserBack n (take end conv)
          page = slice start end conv
       in go start (page : acc)

-- | Find the start index for a page containing @n@ user messages,
-- scanning backward from the end of the given prefix.
-- Returns 0 if fewer than @n@ user messages remain.
findNthUserBack :: Int -> Conversation -> Int
findNthUserBack n conv = go (length conv - 1) n
  where
    go idx remaining
      | idx < 0 = 0
      | remaining <= 0 = idx + 1
      | otherwise = case conv !! idx of
          UserTurn _ -> go (idx - 1) (remaining - 1)
          _ -> go (idx - 1) remaining

-- | Extract a slice [start, end) from a list.
slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start

-- | Format a chunk of conversation turns as readable text.
formatChunk :: Conversation -> Text
formatChunk = T.intercalate "\n" . map formatTurn

formatTurn :: Turn -> Text
formatTurn (UserTurn t) = "[User] " <> t
formatTurn (AssistantTurn t calls) =
  "[Assistant] "
    <> t
    <> if null calls
      then ""
      else " [called: " <> T.intercalate ", " (map tcName calls) <> "]"
formatTurn (ToolTurn results) =
  "[Tool results] "
    <> T.intercalate ", " [trName r <> ": " <> T.take 200 (trContent r) | r <- results]

parseChunk :: Value -> Parser Int
parseChunk = withObject "args" (.: "chunk")
