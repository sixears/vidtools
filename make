#!/home/martyn/bin/bash

pkgs_mediainfo=$(dirname $( dirname $( realpath $(type -p mediainfo) )))
pkgs_mplayer=$(dirname $( dirname $( realpath $(type -p mplayer) )))

for f in $( find proto/ -type f -name \*.hs ); do
  t=src/"${f#proto/}"
  cat "$f" \
    | perl -plE "s{__mediainfo__}{$pkgs_mediainfo}g"  \
    | perl -plE "s{__mplayer__}{$pkgs_mplayer}g"      \
    > "$t"
done
