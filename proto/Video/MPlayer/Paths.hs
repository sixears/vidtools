{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE UnicodeSyntax #-}

module Video.MPlayer.Paths
  where

import FPath.AbsFile ( AbsFile, absfile )

mplayer ∷ AbsFile
mplayer = [absfile|/nix/store/fyfvxckk7cqq3y3s797a7fvxzf48jpry-mplayer-unstable-2022-02-03/bin/mplayer|]
