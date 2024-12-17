{-# LANGUAGE UnicodeSyntax #-}
module Video.MIdentify
  ( main
  , midentify
  ) where

import Debug.Trace ( trace, traceShow )
-- import Data.Kind.Type  ( Type )

import Base1

import Prelude ( Float, divMod, floor, fromRational, (/) )

-- base --------------------------------

import Control.Monad.Identity ( Identity(Identity) )
import Data.Function          ( flip )
import Data.List              ( concatMap, filter, sort, sortOn )
import Data.Maybe             ( catMaybes, fromMaybe )
import Text.Read              ( Read, read, readEither )

-- containers --------------------------

import Data.Map.Strict qualified as Map

-- duration ----------------------------

import Duration ( Duration, asSeconds )

-- env-plus ----------------------------

import Env.Types ( ә, ӭ )

-- fpath -------------------------------

import FPath.AbsFile          ( AbsFile )
import FPath.Error.FPathError ( AsFPathError )

-- fstat -------------------------------

import FStat qualified

-- logging-effect ----------------------

import Control.Monad.Log ( LoggingT, MonadLog, Severity(Informational) )

-- log-plus ----------------------------

import Log ( Log, debugT )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock(NoMock), HasDoMock )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.FStat   ( stat )
import MockIO.Process ( ꙩ )

-- monadio-plus ------------------------

import MonadIO                       ( say, warn )
import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.FPath                 ( pResolve )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens ( (⫤) )

-- mtl ---------------------------------

import Control.Monad.Reader ( MonadReader, runReaderT )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( argument, flag, help, long, metavar, str )
import Options.Applicative.Types   ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

-- parsers -----------------------------

import Text.Parser.Char ( digit, string )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( UsageFPProcIOError )

-- text --------------------------------

import Data.Text qualified as T

-- import Data.Text ( breakOn, drop, intercalate, isPrefixOf, pack, unpack )

--- text-printer -----------------------

import Text.Printer qualified as P

-- textual-plus ------------------------

import TextualPlus ( TextualPlus(textual'), parseTextual )

-- trifecta ----------------------------

import Text.Trifecta.Result ( Result(Failure, Success) )

-- trifecta-plus -----------------------

import TrifectaPlus ( tParse )

------------------------------------------------------------
--                     local imports
------------------------------------------------------------

import Video.MPlayer.Paths qualified as Paths

--------------------------------------------------------------------------------

data Mode = ModeDefault | ModeTabs

data Options = Options { _mode   :: Mode
                       , _inputs :: NonEmpty 𝕋
                       }

----------------------------------------

inputs ∷ ∀ ε μ . (AsIOError ε, AsFPathError ε, MonadError ε μ,
                  HasCallStack, MonadIO μ) ⇒
         Options → μ (NonEmpty AbsFile)
inputs o = sequence $ pResolve @AbsFile ⊳ _inputs o

----------------------------------------

mode ∷ Lens' Options Mode
mode = lens _mode (\ o is → o { _mode = is })

----------------------------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ (flag ModeDefault ModeTabs
             (long "tabs" ⊕ help "output tab-delimited"))
          ⊵ parseNE (argument str (metavar "FILENAME"))

------------------------------------------------------------

data FileData = FileData { _len    :: Duration
                         , _width  :: ℕ
                         , _height :: ℕ
                         , _size   :: Word64
                         }

fd_len ∷ Lens' FileData Duration
fd_len = lens _len (\ o l → o { _len = l })

fd_size ∷ Lens' FileData Word64
fd_size = lens _size (\ o s → o { _size = s })

fd_width ∷ Lens' FileData ℕ
fd_width = lens _width (\ o w → o { _width = w })

fd_height ∷ Lens' FileData ℕ
fd_height = lens _height (\ o h → o { _height = h })

----------------------------------------

parseMIdentify ∷ 𝕄 FStat.FStat → Map.Map 𝕋 𝕋 → 𝔼 𝕋 FileData
parseMIdentify st identifiers = do
  let readEitherT ∷ Read α ⇒ 𝕊 → 𝕋 → 𝔼 𝕋 α
      readEitherT typ s =
        case readEither (T.unpack s) of
          𝕽 x → 𝕽 $ x
          𝕷 e → 𝕷 $ ([fmt|failed to parse %t as %s: %s|] s typ e)
      get ∷ 𝕋 → (𝕋 → 𝔼 𝕋 α) → 𝔼 𝕋 α
      get name f = maybe (𝕷 $ [fmt|no %t found|] name)
                         f (identifiers ⫤ name)

  l ← get "ID_LENGTH" (parseTextual ∘ (⊕"s"))
  w ← get "ID_VIDEO_WIDTH" (readEitherT "ℕ")
  h ← get "ID_VIDEO_HEIGHT" (readEitherT "ℕ")
  z ← maybe (𝕷 "empty stat") (𝕽 ∘ FStat.size) st
  return $ FileData l w h z

----------------------------------------

{- | return all the ID_ tags from mplayer -identify, as a map -}
midentify ∷ ∀ ε δ μ .
            (MonadIO μ, MonadLog (Log MockIOClass) μ,
             HasDoMock δ, MonadReader δ μ,
             AsProcExitError ε, AsCreateProcError ε, AsFPathError ε,AsIOError ε,
             HasCallStack, Printable ε, MonadError ε μ) ⇒
            AbsFile → μ (Map.Map 𝕋 𝕋)
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

  let m_identifiers = Map.fromList $ concatMap parseLine stdout
  return m_identifiers

----------------------------------------

mapRemove_ ∷ (Ord κ) ⇒ κ → Map.Map κ ν → (𝕄 ν, Map.Map κ ν)
mapRemove_ k m = Map.updateLookupWithKey (const $ const 𝕹) k m

------------------------------------------------------------

class UnMaybe φ where
  (~~) ∷ α → φ (𝕄 α) → φ α

--------------------

instance UnMaybe Identity where
  a ~~ Identity ȧ = Identity (fromMaybe a ȧ)

------------------------------------------------------------

class MapRemove φ where
  mapRemove ∷ Ord κ ⇒ φ κ → Map.Map κ ν → (φ (𝕄 ν), Map.Map κ ν)

instance MapRemove Identity where
  mapRemove ∷ Ord κ ⇒ Identity κ → Map.Map κ ν → (Identity (𝕄 ν), Map.Map κ ν)
  mapRemove (Identity (a)) m = let (v,ṁ) = mapRemove_ a m
                           in  ((Identity (v)), ṁ)

------------------------------------------------------------

newtype L2 α = L2 (α, α)
  deriving (Eq, Show)

--------------------

pattern LL2 ∷ α → α → L2 α
pattern LL2 a ȧ ← (L2(a,ȧ))
  where LL2 a ȧ = L2(a,ȧ)

--------------------

instance UnMaybe L2 where
  a ~~ (L2 (ȧ,ạ)) = L2 (fromMaybe a ȧ, fromMaybe a ạ)

--------------------

instance MapRemove L2 where
  mapRemove ∷ Ord κ ⇒ L2 κ → Map.Map κ ν → (L2 (𝕄 ν), Map.Map κ ν)
  mapRemove (L2 (a,ȧ)) m = let (v,ṁ) = mapRemove_ a m
                               (ṿ,ṃ) = mapRemove_ ȧ ṁ
                           in  ((L2 (v,ṿ)), ṃ)

------------------------------------------------------------

newtype L3 α = L3 (α, α, α)
  deriving (Eq, Show)

--------------------

pattern LL3 ∷ α → α → α → L3 α
pattern LL3 a ȧ ạ ← (L3(a,ȧ,ạ))
  where LL3 a ȧ ạ = L3(a,ȧ,ạ)

--------------------

instance UnMaybe L3 where
  a ~~ (L3 (ȧ,ä,ạ)) = L3 (fromMaybe a ȧ, fromMaybe a ä, fromMaybe a ạ)

--------------------

instance MapRemove L3 where
  mapRemove ∷ Ord κ ⇒ L3 κ → Map.Map κ ν → (L3 (𝕄 ν), Map.Map κ ν)
  mapRemove (L3 (a,ȧ,ạ)) m = let (v,ṁ) = mapRemove_ a m
                                 (v̇,ṃ) = mapRemove_ ȧ ṁ
                                 (ṿ,m̈) = mapRemove_ ạ ṃ
                           in  ((L3 (v,v̇,ṿ)), ṃ)


------------------------------------------------------------

newtype L4 α = L4 (α, α, α, α)
  deriving (Eq, Show)

--------------------

pattern LL4 ∷ α → α → α → α → L4 α
pattern LL4 a ȧ ä ạ ← (L4(a,ȧ,ä,ạ))
  where LL4 a ȧ ä ạ = L4(a,ȧ,ä,ạ)

--------------------

instance UnMaybe L4 where
  a ~~ (L4 (ȧ,ä,ạ,å)) = L4 (fromMaybe a ȧ, fromMaybe a ä, fromMaybe a ạ,
                            fromMaybe a å)

--------------------

instance MapRemove L4 where
  mapRemove ∷ Ord κ ⇒ L4 κ → Map.Map κ ν → (L4 (𝕄 ν), Map.Map κ ν)
  mapRemove (L4 (a,ȧ,ä,å)) m = let (v,ṃ) = mapRemove_ a m
                                     (v̇,ṁ) = mapRemove_ ȧ ṃ
                                     (v̈,m̈) = mapRemove_ ä ṁ
                                     (v̊,m̊) = mapRemove_ å m̈
                                in  ((L4 (v,v̇,v̈,v̊)), m̊)

------------------------------------------------------------

newtype L5 α = L5 (α, α, α, α, α)
  deriving (Eq, Show)

--------------------

pattern LL5 ∷ α → α → α → α → α → L5 α
pattern LL5 a ȧ ä ạ å ← (L5(a,ȧ,ä,ạ,å))
  where LL5 a ȧ ä ạ å = L5(a,ȧ,ä,ạ,å)

--------------------

instance MapRemove L5 where
  mapRemove ∷ Ord κ ⇒ L5 κ → Map.Map κ ν → (L5 (𝕄 ν), Map.Map κ ν)
  mapRemove (L5 (a,ȧ,ä,å,ã)) m = let (v,ṃ) = mapRemove_ a m
                                        (v̇,ṁ) = mapRemove_ ȧ ṃ
                                        (v̈,m̈) = mapRemove_ ä ṁ
                                        (v̊,m̊) = mapRemove_ å m̈
                                        (ṽ,m̃) = mapRemove_ ã m̊
                                  in  ((L5 (v,v̇,v̈,v̊,ṽ)), m̃)

--------------------

instance UnMaybe L5 where
  a ~~ (L5 (ȧ,ä,ạ,å,ã)) = L5 (fromMaybe a ȧ, fromMaybe a ä, fromMaybe a ạ,
                              fromMaybe a å, fromMaybe a ã)

------------------------------------------------------------

class LLN φ where
  type LLNext φ ∷ * → *
--  type LLNext φ ∷ Type → Type
  infixl 2 ~
  (~) ∷ α → φ α → (LLNext φ) α

instance LLN L2 where
  type LLNext L2 = L3
  (~) a (L2 (ȧ,ạ)) = L3 (a,ȧ,ạ)

instance LLN L3 where
  type LLNext L3 = L4
  (~) a (L3 (ȧ,ạ,å)) = L4 (a,ȧ,ạ,å)

------------------------------------------------------------

mapRemove2 ∷ 𝕋 → 𝕋 → Map.Map 𝕋 𝕋 → (𝕄 𝕋,𝕄 𝕋, Map.Map 𝕋 𝕋)
mapRemove2 k1 k2 m =
  let go k (acc,n) = let (v,n') = mapRemove_ k n in (v:acc,n')
      ([v1,v2], m') = foldr go ([],m) [k1,k2]
  in (v1,v2, m')

----------------------------------------

mapRemove2d ∷ 𝕋 → 𝕋 → 𝕋 → Map.Map 𝕋 𝕋 → (𝕋,𝕋, Map.Map 𝕋 𝕋)
mapRemove2d d k1 k2 m =
  let go k (acc,n) = let (v,n') = mapRemove_ k n in ((fromMaybe d v):acc,n')
      ([v1,v2], m') = foldr go ([],m) [k1,k2]
  in (v1,v2, m')

----------------------------------------

mapRemove3d ∷ 𝕋 → 𝕋 → 𝕋 → 𝕋 → Map.Map 𝕋 𝕋 → (𝕋,𝕋,𝕋,Map.Map 𝕋 𝕋)
mapRemove3d d k1 k2 k3 m =
  let go k (acc,n) = let (v,n') = mapRemove_ k n in ((fromMaybe d v):acc,n')
      ([v1,v2,v3], m') = foldr go ([],m) [k1,k2,k3]
  in (v1,v2,v3,m')

----------------------------------------

mUnk ∷ 𝕄 𝕋 → 𝕋
mUnk 𝕹     = "UNKNOWN"
mUnk (𝕵 t) = t

------------------------------------------------------------

newtype ChapterNum = ChapterNum { unChapterNum :: ℕ }
  deriving (Eq, Ord, Show)

instance TextualPlus ChapterNum where
  textual' =
    ChapterNum ⊳ (string "CHAPTER_" ⋫ (read ⊳ some digit) ⋪ string "_NAME")
instance Printable ChapterNum where
  print = P.text ∘ [fmt|CHAPTER_%d_NAME|] ∘ unChapterNum

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

printChapterDetails ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → ChapterNum → μ (Map.Map 𝕋 𝕋)
printChapterDetails m c = do
{-
  let (c_name,c_start,c_end,m') =
        let c' = unChapterNum c
        in  mapRemove3d "UNKNOWN" (toText c)
                                  ([fmt|CHAPTER_%d_START|] c')
                                  ([fmt|CHAPTER_%d_END|]   c') m
-}
  let c' = unChapterNum c
  let (L3(c_name,c_start,c_end),m') =
        let (cs ,ṁ) = mapRemove (L3(toText c,
                                    [fmt|CHAPTER_%d_START|] c',
                                    [fmt|CHAPTER_%d_END|] c')) m
        in ("UNKNOWN" ~~ cs,ṁ)
  say $ [fmtT|  chapter %d: %t\t%t → %t|] (unChapterNum c) c_name c_start c_end
  return m'

----------------------------------------

printSubtitleDetails ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → SubtitleNum → μ (Map.Map 𝕋 𝕋)
printSubtitleDetails m s = do
  let (s_lang,m') = sid_lang m s
  let (s_name,m'') = sid_name m' s
  say $ [fmtT|  subtitle %d%t|] (unSubtitleNum s) $
          case (s_lang,s_name) of
            (𝕵 l, 𝕵 n) → ": " ⊕ l ⊕ "/" ⊕ n
            (𝕵 l, 𝕹)   → ": " ⊕ l
            (𝕹, 𝕵 n)   → ": /" ⊕ n
            (𝕹, 𝕹)     → ""
  return m''

----------------------------------------

printFilename ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printFilename m = do
  let (filename∷𝕄 𝕋, m') = mapRemove_ "FILENAME" m
  case filename of
    𝕵 fn → say $ "file:\t" ⊕ fn
    𝕹    → return ()
  return m'

----------------------------------------

fmtKhz ∷ 𝕄 𝕋 → 𝕋
fmtKhz b = case b of
             𝕹 → "UNKNOWN"
             𝕵 b' → case readEither @Float $ T.unpack b' of
                      𝕷 _ → b'
                      𝕽 i → [fmtT|%f|] $ i/1_000

----------------------------------------

printAudio ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printAudio m = do
  let (L5(a_lang,a_codec,a_fmt,a_chan,a_rate), m') =
        mapRemove (L5 ("AID_0_LANG","AUDIO_CODEC","AUDIO_FORMAT","AUDIO_NCH","AUDIO_RATE")) m

  say $ [fmtT|audio: %t %t@%tkHz (%t/%t)|]
        (mUnk a_lang) (mUnk a_chan)
        (fmtKhz a_rate)
        (mUnk a_codec) (mUnk a_fmt)

  return m'

----------------------------------------

printVideo ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printVideo m = do
  let (L5(v_height,v_width,v_fps,v_codec,v_fmt), m') =
        mapRemove (L5("VIDEO_HEIGHT","VIDEO_WIDTH","VIDEO_FPS","VIDEO_CODEC","VIDEO_FORMAT")) m

  say $ [fmtT|video: %tx%t@%3fFPS (%t/%t)|]
        (mUnk v_width) (mUnk v_height)
        (maybe 0 (read @Float ∘ T.unpack) v_fps)
        (mUnk v_codec) (mUnk v_fmt)

  return m'

----------------------------------------

printLength ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printLength m = do
  let (l∷𝕄 𝕋, m') = mapRemove_ "LENGTH" m

  let t = case l of
            𝕹 → "UNKNOWN"
            𝕵 l' → let s ∷ ℕ = floor ∘ read @Float $ T.unpack l'
                       (mm,ss)  = s `divMod` 60
                       (hh,mm') = mm `divMod` 60
                   in  if hh < 1
                       then [fmtT|%d:%02d|] mm' ss
                       else [fmtT|%d:%02d:%02d|] hh mm ss
  say $ [fmtT|length: %t|] t
  return m'

----------------------------------------

printClipInfo ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ (Map.Map 𝕋 𝕋)
printClipInfo m = do
  let printN ṁ c = do
        let (c_name,c_value,ṁ') = mapRemove2d "UNKNOWN" ([fmt|CLIP_INFO_NAME%d|]  c) ([fmt|CLIP_INFO_VALUE%d|] c) ṁ
        say $ [fmtT|%t: %t|] c_name c_value
        return ṁ'

  let (c_count, m') = mapRemove_ "CLIP_INFO_N" m
  case c_count of
    𝕹          → return m'
    𝕵 c_count' → foldM printN m' [0..(read @ℤ ∘ T.unpack $ c_count')-1]

----------------------------------------

defaultPrint ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ ()
defaultPrint m = do
  let (s_cnt,c_cnt, m') = mapRemove2 "SUBTITLE_ID" "CHAPTER_ID" m

  let chapters  = maybe [] (\ c → ChapterNum  ⊳ [0..(read $ T.unpack c)]) c_cnt
  let subtitles = maybe [] (\ s → SubtitleNum ⊳ [0..(read $ T.unpack s)]) s_cnt

  m'' ← foldM (\ ṁ f → f ṁ) m'
                [ printFilename
                , printLength
                , printVideo
                , printAudio
                , printClipInfo
                , \ ṁ → foldM printChapterDetails ṁ  chapters
                , \ ṁ → foldM printSubtitleDetails ṁ subtitles
                ]

  let printKV (k,v) = say $ [fmtT|%-20t\t%t|] k v
  forM_ (sortOn fst $ Map.toList m'') printKV

----------------------------------------

tabPrint ∷ MonadIO μ ⇒ Map.Map 𝕋 𝕋 → μ ()
tabPrint m_identifiers = do
  let printKV (k,v) = say $ [fmtT|%t\t%t|] k v
  forM_ (sortOn fst $ Map.toList m_identifiers) printKV

----------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          AsProcExitError ε, Printable ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← inputs opts
  forM_ ins $ \ input → do
    let printer = case opts ⊣ mode of
                    ModeTabs    → tabPrint
                    ModeDefault → defaultPrint
    midentify input ≫ printer

----------------------------------------

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "run mplayer -identify yada yada for each file"
      my_main = myMain @UsageFPProcIOError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)

-- that's all, folks! ----------------------------------------------------------
