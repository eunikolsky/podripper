module Ripper.RipperDelay
  ( getRipperDelay
  ) where

import RSSGen.Duration

getRipperDelay :: RetryDelay
getRipperDelay = defaultDelay

-- | The default ripper delay if no other rule matches.
defaultDelay :: RetryDelay
defaultDelay = RetryDelay $ durationMinutes 10
