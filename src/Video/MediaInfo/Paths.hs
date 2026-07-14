{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE UnicodeSyntax #-}

module Video.MediaInfo.Paths
  where

import FPath.AbsFile ( AbsFile, absfile )

mediaInfo :: AbsFile
mediaInfo = [absfile|/nix/store/m5ph6sbx13gjdh3wp230a3n1wnmghsql-mediainfo-25.04/bin/mediainfo|]
