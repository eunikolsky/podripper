{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module MP3.AudioDuration
  ( AudioDuration(..)
  ) where

import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU

-- | Audio duration of an MP3 frame/file, in seconds.
newtype AudioDuration = AudioDuration { getAudioDuration :: Double }
  deriving newtype (Eq, Ord, Fractional, Num)

instance Show AudioDuration where
  show (AudioDuration d) = show d <> " s"

-- boilerplate from the `Data.Vector.Unboxed` docs to be able to store
-- `AudioDuration` in an unboxed vector
newtype instance VU.MVector s AudioDuration = MV_Double (VU.MVector s Double)
newtype instance VU.Vector AudioDuration = V_Double (VU.Vector Double)
deriving newtype instance VGM.MVector VU.MVector AudioDuration
deriving newtype instance VG.Vector VU.Vector AudioDuration
instance VU.Unbox AudioDuration
