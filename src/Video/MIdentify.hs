{-# LANGUAGE UnicodeSyntax #-}

module Video.MIdentify
  ( MIdentify
  , length
  , main
  , midentify
  ) where

import Base1
import Prelude ( Float, floor, (/) )

-- base --------------------------------

import Data.List   ( concatMap, find, sortOn )
import Data.Maybe  ( isJust )
import Data.Type.Equality  ( type (~) )
import Text.Read   ( read, readEither )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- containers-plus ---------------------

import ContainersPlus.Map       ( mapRemove, mapRemoveF )
import ContainersPlus.MapUtils  ( fromListAndDups )

-- duration ----------------------------

import Duration  ( Duration( SECS ), asSeconds )

-- env-plus ----------------------------

import Env.Types ( ә, ӭ )

-- extra -------------------------------

import Data.List.Extra  ( split )

-- finite-list -------------------------

import FiniteList  ( (~), pattern LL1, pattern LL2, pattern LL3, pattern LL5 )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile, absfile )
import FPath.Parseable        ( parse )
import FPath.Error.FPathError ( AsFPathError, FPathError )

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog )

-- log-plus ----------------------------

import Log ( Log, debugT, warnT )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock(NoMock), HasDoMock )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process ( ꙩ )

-- monadio-plus ------------------------

import MonadIO                       ( say )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )

-- mono-traversable --------------------

import Data.Containers  ( ContainerKey, IsMap, MapValue )

-- more-unicode ------------------------

import Data.MoreUnicode.Default  ( ð )
import Data.MoreUnicode.Lens     ( (⊩) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, runReaderT )

-- parsers -----------------------------

import Text.Parser.Char ( digit, string )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( UsageFPProcIOTPError )

-- text --------------------------------

import Data.Text qualified as T

--- text-printer -----------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus                          ( TextualPlus(textual'), tparse )
import TextualPlus.Error.TextualParseError  ( AsTextualParseError,
                                              TextualParseError,
                                              throwAsTextualParseError
                                            )

------------------------------------------------------------
--                     local imports
------------------------------------------------------------

import Video.MPlayer.Paths qualified as Paths

import Video.MIdentify.Options  ( DefaultOptions, Options
                                , Mode( ModeDefault, ModeTabs )
                                , ShowChapters( NoShowChapters, ShowChapters )
                                , ShowUnparsedFields( ShowUnparsedFields )
                                , inputs, mode, parseOptions, showChapters
                                , showUnparsedFields
                                )

--------------------------------------------------------------------------------

{-| `mapRemove`, but drop the input key from the result -}

mapRemove_ ∷ (Ord κ) ⇒ κ → Map.Map κ ν → (𝕄 ν, Map.Map κ ν)
mapRemove_ k m = first snd $ mapRemove k m

----------------------------------------

{-| `mapRemoveF`, but drop the keys (thus rely on position to know which values
    relate to which keys) -}

mapRemoveF_ ∷ (Foldable φ,Functor φ,IsMap ψ,ContainerKey ψ ~ κ,MapValue ψ ~ ν)=>
              φ κ → ψ → (φ (𝕄 ν), ψ)
mapRemoveF_ ks = first (snd ⊳) ∘ mapRemoveF ks

----------------------------------------

{-| `mapRemoveF_`, but replace missing values with a default -}
mapRemoveFð ∷ (Foldable φ,Functor φ,IsMap ψ,ContainerKey ψ ~ κ,MapValue ψ ~ ν)=>
              ν → φ κ → ψ → (φ ν, ψ)
mapRemoveFð df ks = first ((df ⧐) ⊳) ∘ mapRemoveF_ ks

------------------------------------------------------------

data MIdentify = MIdentify { _length      ∷ 𝕄 Duration
                           , _filename    ∷ 𝕄 AbsFile
                           , _videoHeight ∷ 𝕄 ℕ
                           , _videoWidth  ∷ 𝕄 ℕ
                           }
  deriving Show

--------------------

instance Default MIdentify where
  def = MIdentify 𝓝 𝓝 𝓝 𝓝

--------------------

instance Printable MIdentify where
  print m = P.text $ [fmt|%T: %4dx%4d - %T|] (m ⊣ filename  ⧏ [absfile|/NONE|])
                                             (m ⊣ videoWidth  ⧏ 0)
                                             (m ⊣ videoHeight ⧏ 0)
                                             (m ⊣ length      ⧏ SECS 0)



--------------------

length ∷ Lens' MIdentify (𝕄 Duration)
length = lens _length (\ mid l → mid { _length = l })

--------------------

filename ∷ Lens' MIdentify (𝕄 AbsFile)
filename = lens _filename (\ mid f → mid { _filename = f })

--------------------

videoHeight ∷ Lens' MIdentify (𝕄 ℕ)
videoHeight = lens _videoHeight (\ mid h → mid { _videoHeight = h })

--------------------

videoWidth ∷ Lens' MIdentify (𝕄 ℕ)
videoWidth = lens _videoWidth (\ mid w → mid { _videoWidth = w })

----------------------------------------

parseLength ∷ ∀ ε η . (AsTextualParseError ε, MonadError ε η) =>
               Map.Map 𝕋 𝕋 → η (MIdentify → MIdentify)
parseLength m =
 case Map.lookup "LENGTH" m of
    𝓝    → return id
    𝓙 t  → (\ l → (& length ⊩ l)) ⊳ tparse (t ◇ "s")

----------------------------------------

parseFilename ∷ ∀ ε η . (AsFPathError ε, MonadError ε η) =>
                 Map.Map 𝕋 𝕋 → η (MIdentify → MIdentify)
parseFilename m =
 case Map.lookup "FILENAME" m of
    𝓝   → return id
    𝓙 t → (\ f → (\ mid → mid & filename ⊩ f)) ⊳ parse t

------------------------------------------------------------

{- | run `mplayer -identify`; return an `MIdentify` instance; but also all
     the fields -}
midentify ∷ ∀ ε δ μ .
            (MonadIO μ, MonadLog (Log MockIOClass) μ,
             HasDoMock δ, MonadReader δ μ,
             AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,
             AsIOError ε, AsTextualParseError ε,
             HasCallStack, Printable ε, MonadError ε μ) ⇒
            AbsFile → μ (MIdentify, Map.Map 𝕋 𝕋)
midentify input = do
  let opts = [ "-vo", "null"
             , "-ao", "null"
             , "-frames", "0"
             , "-identify"
             , "-lavfdopts", "analyzeduration=10"
             , "-lavfdopts", "probesize=10000000"
             ]
      args = opts ⊕ [ toText input ]
      parseLine l = if "ID_" `T.isPrefixOf` l
                    then case T.breakOn "=" l of
                           (_,"") → []
                           (k,v)  → [(T.drop 3 k,T.drop 1 v)]
                    else []

  -- this will error out in case of non-zero exit, so no need to
  -- bind the exit value to a var

  -- I'm not sure why it needs $HOME, possibly to find any user config
  (_,(stdout∷[𝕋],stderr∷[𝕋])) ← ꙩ (Paths.mplayer, args, [ӭ $ ә"HOME"])
  forM_ stderr debugT

  let groups = split (\ t → T.span (≡'=') t ≡ (t,"") ∧ ﬧ (T.null t)) stdout
      video_group_m = find(isJust ∘ find(T.isPrefixOf "ID_VIDEO_CODEC=")) groups
  video_group ← case video_group_m of
                  𝓙 vg → return vg
                  𝓝    → throwAsTextualParseError "No video group found"
                                                   (T.unpack ⊳ stdout)

  let (dups, m_identifiers) = fromListAndDups $ concatMap parseLine video_group

  forM_ (Map.toList dups)
        (\ (k,vs) → warnT $ [fmt|duplicate key %t ignored values: %L|] k vs)

  let (errs∷[𝕋],mid) =
        foldl (\ (es, mid') f → case f m_identifiers of
                                       𝓡 f' → (es,f' mid')
                                       𝓛 err → (err:es,mid')
              )
              ([],ð)
              [ (first toText) ⊳ (parseLength @TextualParseError)
              , (first toText) ⊳ (parseFilename @FPathError)
              ]

  forM_ errs (\ err → warnT $ [fmt|parse err %T|] err)

  -- WARN OF PARSE ERRORS
  -- CONVERT OTHER FUNCTIONS TO PARSE
  -- SPLIT PRINT INTO SEPARATE MODULE
  -- PARSE SUBTITLES (to show SDH / Eng)

  return (mid, m_identifiers)

------------------------------------------------------------

newtype ChapterNum = ChapterNum { unChapterNum :: ℕ }
  deriving (Eq, Ord, Show)

instance TextualPlus ChapterNum where
  textual' =
    ChapterNum ⊳ (string "CHAPTER_" ⋫ (read ⊳ some digit) ⋪ string "_NAME")
instance Printable ChapterNum where
  print = P.text ∘ [fmt|chapter %2d|] ∘ unChapterNum

------------------------------------------------------------

newtype SubtitleNum = SubtitleNum { unSubtitleNum :: ℕ }
  deriving (Eq, Ord, Show)

sid_lang ∷ Map.Map 𝕋 𝕋 → SubtitleNum → (𝕄 𝕋, Map.Map 𝕋 𝕋)
sid_lang m s = let name = [fmt|SID_%d_LANG|] (unSubtitleNum s)
               in  mapRemove_ name m

sid_name ∷ Map.Map 𝕋 𝕋 → SubtitleNum → (𝕄 𝕋, Map.Map 𝕋 𝕋)
sid_name m s = let name = [fmt|SID_%d_NAME|] (unSubtitleNum s)
               in  mapRemove_ name m

------------------------------------------------------------

info ∷ MonadIO μ ⇒ 𝕋 → 𝕋 → μ ()
info k v = say $ [fmtT|%-10t: %t|] k v

----------------------------------------

printChapterDetails ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → ChapterNum → μ (Map.Map 𝕋 𝕋)
printChapterDetails m c = do
  let c' = unChapterNum c
  let (LL3 c_name c_start c_end, m') =
        let keys = [fmt|CHAPTER_%d_%s|] c' ⊳ LL3 "NAME" "START" "END"
        in  mapRemoveF_ keys m
  let c_st = maybe 0 (either (const 0) id ∘ readEither @ℕ ∘ T.unpack) c_start
      c_ed = maybe 0 (either (const 0) id ∘ readEither @ℕ ∘ T.unpack) c_end
  info ([fmt|chapter %T|] c) $
    [fmtT|%t\t%8d → %8d|] (c_name ⧏ "UNKNOWN") (c_st) (c_ed)
  return m'

----------------------------------------

printFilename ∷ MonadIO μ ⇒ MIdentify → μ ()
printFilename mid = info "file" (maybe "UNKNOWN" toText (mid ⊣ filename))

----------------------------------------

fmtKhz ∷ 𝕄 𝕋 → 𝕋
fmtKhz b = case b of
             𝓝 → "UNKNOWN"
             𝓙 b' → case readEither @Float $ T.unpack b' of
                      𝓛 _ → b'
                      𝓡 i → [fmtT|%f|] $ i/1_000

----------------------------------------

printAudio ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printAudio m = do
  let 𝕦 = (⧏ "UNKNOWN")
      keys = LL5 "AID_0_LANG" "AUDIO_CODEC" "AUDIO_FORMAT"
                 "AUDIO_NCH" "AUDIO_RATE"
      (LL5 a_lang a_codec a_fmt a_chan a_rate, m') = mapRemoveF_ keys m

  info "audio" $ [fmtT|%t %t@%tkHz (%t/%t)|]
                 (𝕦 a_lang) (𝕦 a_chan)
                 (fmtKhz a_rate) (𝕦 a_codec) (𝕦 a_fmt)

  return m'

----------------------------------------

printVideo ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printVideo m = do
  let 𝕦 = (⧏ "UNKNOWN")
      keys = LL5 "VIDEO_HEIGHT" "VIDEO_WIDTH" "VIDEO_FPS"
                 "VIDEO_CODEC" "VIDEO_FORMAT"
      (LL5 v_height v_width v_fps v_codec v_fmt, m') = mapRemoveF_ keys m

  info "video" $ [fmtT|%tx%t@%3fFPS (%t/%t)|]
                 (𝕦 v_width) (𝕦 v_height)
                 (maybe 0 (read @Float ∘ T.unpack) v_fps)
                 (𝕦 v_codec) (𝕦 v_fmt)

  return m'

----------------------------------------

printLength ∷ MonadIO μ ⇒ MIdentify → μ ()
printLength mid = do
  let t = maybe "UNKNOWN" (\ x → [fmt|%m|] (floor @_ @ℕ $ x ⊣ asSeconds))
                          (mid ⊣ length)
  info "length" t

----------------------------------------

printClipInfo ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printClipInfo m = do
  let printN ṁ c = do
        let keys = ([fmt|CLIP_INFO_NAME%d|]c)~(LL1 ([fmt|CLIP_INFO_VALUE%d|]c))
        let (LL2 c_name c_value, ṁ') = mapRemoveFð "UNKNOWN" keys ṁ
        info c_name c_value
        return ṁ'

  let (c_count, m') = mapRemove_ "CLIP_INFO_N" m
  case c_count of
    𝓝          → return m'
    𝓙 c_count' → foldM printN m' [0..(read @ℤ ∘ T.unpack $ c_count')-1]

----------------------------------------

printSubtitleDetails ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → SubtitleNum → μ (Map.Map 𝕋 𝕋)
printSubtitleDetails m s = do
  let (s_lang,m') = sid_lang m s
  let (s_name,m'') = sid_name m' s
  info "subtitle" $ [fmtT|%d%t|] (unSubtitleNum s) $
                      case (s_lang,s_name) of
                        (𝓙 l, 𝓙 n) → ": " ⊕ l ⊕ "/" ⊕ n
                        (𝓙 l, 𝓝)   → ": " ⊕ l
                        (𝓝, 𝓙 n)   → ": /" ⊕ n
                        (𝓝, 𝓝)     → ""
  return m''

----------------------------------------

defaultPrintMap ∷ MonadIO μ ⇒
                  ShowChapters → (MIdentify, Map.Map 𝕋 𝕋) → μ (Map.Map 𝕋 𝕋)
defaultPrintMap show_chapters (mid,m) = do
  let (LL2 (_,s_cnt)(_,c_cnt),m') = mapRemoveF(LL2 "SUBTITLE_ID" "CHAPTER_ID") m

  let chapters  = maybe [] (\ c → ChapterNum  ⊳ [0..(read $ T.unpack c)]) c_cnt
  let subtitles = maybe [] (\ s → SubtitleNum ⊳ [0..(read $ T.unpack s)]) s_cnt

  let foldM' f xs i = foldM f i xs

  foldM (\ ṁ f → f ṁ) m'
          ([ (\ n → printFilename mid ⪼ return n)
           , (\ n → printLength mid ⪼ return n)
           , printVideo
           , printAudio
           , printClipInfo
           ] ⊕ (case show_chapters of
                  ShowChapters → [ foldM' printChapterDetails chapters ]
                  NoShowChapters → [])
             ⊕ [ \ ṁ → foldM printSubtitleDetails ṁ subtitles ])

----------------------------------------

defaultPrint ∷ MonadIO μ ⇒ DefaultOptions → (MIdentify, Map.Map 𝕋 𝕋) → μ ()
defaultPrint default_opts (mid,m) = do
  m' ← defaultPrintMap (default_opts ⊣ showChapters) (mid,m)

  when (ShowUnparsedFields ≡ default_opts ⊣ showUnparsedFields) $
    let printKV (k,v) = say $ [fmtT|%-20t\t%t|] k v
    in  forM_ (sortOn fst $ Map.toList m') printKV

----------------------------------------

tabPrint ∷ MonadIO μ ⇒ (α, Map.Map 𝕋 𝕋) → μ ()
tabPrint (_,m_identifiers) = do
  let printKV (k,v) = say $ [fmtT|%t\t%t|] k v
  forM_ (sortOn fst $ Map.toList m_identifiers) printKV

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          AsProcExitError ε, AsTextualParseError ε, Printable ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← inputs opts
  forM_ ins $ \ input → do
    let printer = case opts ⊣ mode of
                    ModeTabs                    → tabPrint
                    ModeDefault default_options → defaultPrint default_options
    midentify input ≫ printer

----------------------------------------

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "run mplayer -identify yada yada for each file"
      my_main = myMain @UsageFPProcIOTPError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)

-- that's all, folks! ----------------------------------------------------------
