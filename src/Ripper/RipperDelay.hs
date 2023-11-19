module Ripper.RipperDelay
  ( getRipperDelay
  ) where

import Data.Time.TZTime
import RSSGen.Duration

type RipEndTime = TZTime
type Now = TZTime

getRipperDelay :: Maybe RipEndTime -> Now -> RetryDelay
getRipperDelay (Just _) _ = shortAfterRipDelay
getRipperDelay Nothing _ = defaultDelay

-- A very short ripper delay that happens soon after a rip has ended because we
-- don't know whether the stream has ended or there was an error and it will be
-- back soon.
shortAfterRipDelay :: RetryDelay
shortAfterRipDelay = RetryDelay $ durationSeconds 1

-- | The default ripper delay if no other rule matches.
defaultDelay :: RetryDelay
defaultDelay = RetryDelay $ durationMinutes 10
