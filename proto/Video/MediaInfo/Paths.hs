{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE UnicodeSyntax #-}

module Video.MediaInfo.Paths
  where

import FPath.AbsFile ( AbsFile, absfile )

mediaInfo :: AbsFile
mediaInfo = [absfile|__mediainfo__/bin/mediainfo|]
