module LLM.Core.Usage
  ( Usage (..),
    PricingInfo (..),
    emptyUsage,
    addUsage,
    estimateCost,
  )
where

-- | Token usage from a single API call
data Usage = Usage
  { usageInputTokens :: !Int,
    usageOutputTokens :: !Int,
    usageTotalCost :: !Double
  }
  deriving (Show, Eq)

emptyUsage :: Usage
emptyUsage = Usage 0 0 0

addUsage :: Usage -> Usage -> Usage
addUsage a b =
  Usage
    { usageInputTokens = usageInputTokens a + usageInputTokens b,
      usageOutputTokens = usageOutputTokens a + usageOutputTokens b,
      usageTotalCost = usageTotalCost a + usageTotalCost b
    }

-- | Pricing in dollars per million tokens
data PricingInfo = PricingInfo
  { pricePerMillionInput :: Double,
    pricePerMillionOutput :: Double
  }
  deriving (Show)

estimateCost :: PricingInfo -> Usage -> Double
estimateCost p u =
  fromIntegral (usageInputTokens u) * pricePerMillionInput p / 1_000_000
    + fromIntegral (usageOutputTokens u) * pricePerMillionOutput p / 1_000_000
