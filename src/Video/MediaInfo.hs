{-# LANGUAGE DataKinds, GADTs, RankNTypes, StandaloneDeriving #-}

{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- | Parse the output of the mediainfo command, summarize it -}

module Video.MediaInfo
  ( MediaInfo, main )
where

import Base1
import Prelude ( Bounded, Enum, Float, Integral, Num, Real,
                 error, fromRational, fail, round, toRational )

-- aeson -------------------------------

import Data.Aeson         ( FromJSON( parseJSON ),
                            Value( Bool, Number, Object, String ),
                            (.:), (.:?), (.!=),
                            eitherDecode, eitherDecodeStrict, withArray,
                            withObject, withText
                          )
import Data.Aeson.KeyMap  ( (!?) )

-- aeson-plus --------------------------

import Data.Aeson.Error  ( AsAesonError, throwAsAesonError )

-- base --------------------------------

import Data.Bool           ( bool )
import Data.List           ( isInfixOf, zip )
import Data.Monoid         ( Monoid( mempty ) )
import Data.Ratio          ( Ratio, (%), denominator, numerator )
import Text.Read           ( Read, readEither )
import Text.Show           ( showParen, showsPrec, showString )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( ℚ, (≠) )

-- duration ----------------------------

import Duration  ( Duration( SECS ) )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

-- fpath -------------------------------

import FPath.AbsDir            ( absdir )
import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.File              ( FileAs )
import FPath.Parseable         ( __parse__ )
import FPath.RelDir            ( reldir )
import FPath.RelFile           ( relfile )

-- lens --------------------------------

import Control.Lens.Iso     ( Iso', iso )
import Control.Lens.Getter  ( view )
import Control.Lens.Setter  ( (<>~) )

-- log-plus ----------------------------

import Log  ( Log, warnT )

-- logging-effect ----------------------

import Control.Monad.Log ( MonadLog, LoggingT )

-- mockio-log --------------------------

import MockIO.DoMock      ( DoMock( NoMock ), HasDoMock )
import MockIO.Log         ( logit )
import MockIO.MockIOClass ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process  ( ꙩ )

-- monadio-plus ------------------------

import MonadIO                        ( say )
import MonadIO.Base                   ( getArgs )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )
import MonadIO.FPath                  ( pResolve )

-- mtl -----------------------

import Control.Monad.Reader  ( MonadReader, runReaderT )

-- natural -----------------------------

import Natural.Length  ( ỻ )

-- optparse-applicative ----------------

import Options.Applicative.Builder ( argument, metavar, str )
import Options.Applicative.Types   ( Parser )

-- optparse-plus -----------------------

import OptParsePlus ( parseNE )

-- scientific --------------------------

import Data.Scientific  ( toBoundedInteger )

-- stdmain -----------------------------

import StdMain            ( stdMainNoDR )
import StdMain.UsageError ( UsageParseAesonFPPIOError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, assertEqual, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertLeft, assertRight, runTestsP, runTestsReplay,
                    runTestTree )

-- text --------------------------------

import Data.Text  qualified as  T

import Data.Text.Read  ( rational )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Video.MediaInfo.Paths qualified as Paths

import Video.MediaInfo.TestData  ( t0, t1, t2, t3, t4, t5, t6 )

--------------------------------------------------------------------------------

{-| things that are isomorphic to some bounded integral type -}
class (Bounded α, Enum α, Integral α, Num α) => IsBoundedI α β | α → β where
  {-| isomorphism with a bounded integral type -}
  boundedI ∷ Iso' α β

------------------------------------------------------------

{-| wrapper for bounded integral types, to define our own FromJSON instance -}
newtype (Bounded α, Enum α, Integral α, Num α) => BoundedI α = BoundedI α
  deriving  (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

----------

instance (Bounded α, Integral α, Num α) => IsBoundedI (BoundedI α) α where
  boundedI = iso (\ (BoundedI x) → x) BoundedI

----------

instance (Bounded α, Integral α, Read α) => FromJSON (BoundedI α) where
  {-| we define our own parseJSON to handle strings and numbers both -}
  parseJSON (Number n) =
    case toBoundedInteger n of
      𝓙 i → return $ BoundedI i
      𝓝   → fail $ [fmt|failed parsing BoundedI as Number: %w|] n
  parseJSON (String s) =
    case readEither (T.unpack s) of
      𝓡 i → return $ BoundedI i
      𝓛 e → fail $ [fmt|failed parsing BoundedI as Text: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for BoundedI: '%w'|] v

------------------------------------------------------------

{-| wrapper around `Duration` to allow for mediainfo-specific FromJSON
    instance -}
newtype MDuration = MDuration Duration  deriving  (Eq, Printable, Show)

instance FromJSON MDuration where
  parseJSON (Number n) = return ∘ MDuration ∘ SECS $ toRational n
  parseJSON (String s) =
    case (second (first SECS) ⊳ rational) s of
      𝓡 (d,_) → pure $ MDuration d
      𝓛  e    → fail $ [fmt|failed to parse Duration: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MDuration: '%w'|] v

----------------------------------------

{-| create an MDuratino from a ℚ seconds -}
msecs ∷ ℚ → MDuration
msecs = MDuration ∘ SECS

------------------------------------------------------------

{-| things that are isomorphic to some ratio type -}
class IsRatio α β | α → β where
  {-| isomorphism with `Ratio` -}
  ratio ∷ Iso' α (Ratio β)

------------------------------------------------------------

{-| wrapper around `Ratio` to allow for FromJSON instance -}
newtype Num α => MRatio α = MRatio (Ratio α)  deriving  (Eq, Num, Show)

----------

instance Num α => IsRatio (MRatio α) α where
  ratio = iso (\ (MRatio x) → x) MRatio

----------

instance (Integral α, Num α, Read α) => FromJSON (MRatio α) where
  parseJSON (Number n) =
    let r = toRational n
    in  pure ∘ MRatio $ (fromIntegral $ numerator r)
                      % (fromIntegral $ denominator r)
  parseJSON (String s) =
    case rational s of
      𝓡 (d,_) → pure $ MRatio d
      𝓛  e    → fail $ [fmt|failed to parse Ratio: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MRatio: '%w'|] v

------------------------------------------------------------

{-| wrapper for strictly-positive `Ratio` instances -}
newtype Num α => MRatioN α = MRatioN (Ratio α)  deriving  (Eq, Num, Show)

instance (Integral α, Num α, Read α) => FromJSON (MRatioN α) where
  parseJSON (Number s) = do
    let r = toRational s
        check t x = if x < 0
                    then fail $ [fmt|negative %t found in ratio %w|] t r
                    else return $ fromIntegral x
    n ← check "numerator"   $ numerator r
    d ← check "denominator" $ denominator r
    return $ MRatioN (n % d)
  parseJSON (String s) =
    case rational s of
      𝓡 (d,_) → pure $ MRatioN d
      𝓛  e    → fail $ [fmt|failed to parse Ratio: %s|] e
  parseJSON v          = fail$ [fmt|wrong type for MRatioN: '%w'|] v

------------------------------------------------------------

{-| Framerate -}
newtype FrameRate = FrameRate (MRatioN ℕ)  deriving  (Eq, FromJSON, Num, Show)

instance IsRatio FrameRate ℕ where
  ratio = iso (\ (FrameRate (MRatioN x)) → x) (FrameRate ∘ MRatioN)

------------------------------------------------------------

{-| Number of video streams -}
newtype VideoCount = VideoCount (BoundedI Word8)
  deriving (Eq,FromJSON,Num,Show)

------------------------------------------------------------

{-| Number of audio streams -}
newtype AudioCount = AudioCount (BoundedI Word8)
  deriving (Eq,FromJSON,Num,Show)

------------------------------------------------------------

{-| Number of text streams -}

newtype TextCount  = TextCount  (BoundedI Word8)
  deriving (Eq,Enum,FromJSON,Integral,Num,Ord,Real,Show)

------------------------------------------------------------

{-| number of channels in an audio stream -}
newtype AudioChannels = AudioChannels (BoundedI Word8)
  deriving (Eq,Enum,FromJSON,Integral,Num,Ord,Real,Show)

------------------------------------------------------------

{-| width of a video stream in pixels -}
newtype VideoWidth   = VideoWidth (BoundedI Word16)
  deriving (Enum,Eq,FromJSON,Integral,Num,Ord,Real,Show)

------------------------------------------------------------

{-| height of a video stream in pixels -}
newtype VideoHeight  = VideoHeight(BoundedI Word16)
  deriving (Enum,Eq,FromJSON,Integral,Num,Ord,Real,Show)

------------------------------------------------------------

{-| size of a file in bytes-}
newtype FileSize     = FileSize   (BoundedI Word64)
  deriving (Bounded, Enum,Eq,FromJSON,Integral,Num,Ord,Real,Show)

----------

instance IsBoundedI FileSize Word64 where
  boundedI = iso (\ (FileSize x) → x ⊣ boundedI) (FileSize ∘ BoundedI)

------------------------------------------------------------

{-| "general" track type -}
data TrackGeneral =
  TrackGeneral { _videoCount ∷ VideoCount
               , _audioCount ∷ AudioCount
               , _textCount  ∷ TextCount
               , _duration   ∷ MDuration
               , _fileSize   ∷ FileSize
               , _frameRate  ∷ FrameRate
               }
  deriving (Eq, Show)

----------

instance FromJSON TrackGeneral where
  parseJSON = withObject "TrackGeneral" $ \ v →
    TrackGeneral ⊳ v .:  "VideoCount"
                 ⊵ v .:  "AudioCount"
                 ⊵ v .:? "TextCount" .!= 0
                 ⊵ v .:  "Duration"
                 ⊵ v .:  "FileSize"
                 ⊵ v .:  "FrameRate"

--------------------

textCount ∷ Lens' TrackGeneral TextCount
textCount = lens _textCount (\ tg tc → tg { _textCount = tc })

--------------------

duration ∷ Lens' TrackGeneral MDuration
duration = lens _duration (\ tg d → tg { _duration = d })

--------------------

fileSize ∷ Lens' TrackGeneral FileSize
fileSize = lens _fileSize (\ tg fs → tg { _fileSize = fs })

--------------------

frameRate ∷ Lens' TrackGeneral FrameRate
frameRate = lens _frameRate (\ tg fr → tg { _frameRate = fr })

------------------------------------------------------------

data VideoFormat = VideoFormat_AVC | VideoFormat_HEVC
                 | VideoFormat_MPEG4_Visual
  deriving (Eq,Show)

----------

instance FromJSON VideoFormat where
  parseJSON = withText "VideoFormat" $ \ v →
    case v of
      "AVC"           → pure VideoFormat_AVC
      "HEVC"          → pure VideoFormat_HEVC
      "MPEG-4 Visual" → pure VideoFormat_MPEG4_Visual
      _               → fail $ [fmt|Unknown video format: %t|] v

----------

instance Printable VideoFormat where
  print VideoFormat_AVC          = P.text "AVC"
  print VideoFormat_HEVC         = P.text "HEVC"
  print VideoFormat_MPEG4_Visual = P.text "MPEG5"

------------------------------------------------------------

{-| "videoFormat" track type -}
data TrackVideo =
  TrackVideo { _videoFormat ∷ VideoFormat
             , _videoWidth  ∷ VideoWidth
             , _videoHeight ∷ VideoHeight
             }
  deriving (Eq, Show)

----------

instance FromJSON TrackVideo where
  parseJSON = withObject "TrackVideo" $ \ v →
    TrackVideo ⊳ v .: "Format"
               ⊵ v .: "Width"
               ⊵ v .: "Height"

--------------------

videoWidth ∷ Lens' TrackVideo VideoWidth
videoWidth = lens _videoWidth (\ tv vw → tv { _videoWidth = vw })

--------------------

videoHeight ∷ Lens' TrackVideo VideoHeight
videoHeight = lens _videoHeight (\ tv vh → tv { _videoHeight = vh })

--------------------

videoFormat ∷ Lens' TrackVideo VideoFormat
videoFormat = lens _videoFormat (\ tv vf → tv { _videoFormat = vf })

------------------------------------------------------------

data AudioFormat = AudioFormat_AAC | AudioFormat_AC3
                 | AudioFormat_EAC3 | AudioFormat_MPEG
  deriving (Eq,Show)

----------

instance FromJSON AudioFormat where
  parseJSON = withText "AudioFormat" $ \ v →
    case v of
      "AAC"           → pure AudioFormat_AAC
      "AC-3"          → pure AudioFormat_AC3
      "E-AC-3"        → pure AudioFormat_EAC3
      "MPEG Audio"    → pure AudioFormat_MPEG
      _               → fail $ [fmt|Unknown audio format: %t|] v

----------

instance Printable AudioFormat where
  print AudioFormat_AAC  = P.text "AAC"
  print AudioFormat_AC3  = P.text "AC3"
  print AudioFormat_EAC3 = P.text "EAC3"
  print AudioFormat_MPEG = P.text "MPEG"

------------------------------------------------------------

data Language = Language_EN | Language_Other 𝕋 deriving (Eq,Show)

instance FromJSON Language where
  parseJSON = withText "Language" $ \ t →
    pure $ case t of
      "en"            → Language_EN
      _               → Language_Other t

instance Printable Language where
  print Language_EN        = P.text "English"
  print (Language_Other l) = P.text l

------------------------------------------------------------

data IsDefault = IsDefault | IsntDefault
  deriving (Eq,Show)

instance FromJSON IsDefault where
  parseJSON (Bool b)   = pure $ bool IsntDefault IsDefault b
  parseJSON (String t) = case t of
                           "Yes" → pure IsDefault
                           "No"  → pure IsntDefault
                           _     → fail $ [fmt|Default: bad value '%t'|] t
  parseJSON v          = fail $ [fmt|Default: bad value type '%w'|] v

------------------------------------------------------------

data IsForced = IsForced | IsntForced
  deriving (Eq,Show)

instance FromJSON IsForced where
  parseJSON (Bool b)   = pure $ bool IsntForced IsForced b
  parseJSON (String t) = case t of
                           "Yes" → pure IsForced
                           "No"  → pure IsntForced
                           _     → fail $ [fmt|Forced: bad value '%t'|] t
  parseJSON v          = fail $ [fmt|Forced: bad value type '%w'|] v

------------------------------------------------------------

{-| "audio" track type -}
data TrackAudio =
  TrackAudio { _audioFormat   ∷ AudioFormat
             , _audioLang     ∷ Language
             , _audioDefault  ∷ IsDefault
             , _audioForced   ∷ IsForced
             , _audioChannels ∷ AudioChannels
             }
  deriving (Eq, Show)

----------

instance FromJSON TrackAudio where
  parseJSON = withObject "TrackAudio" $ \ v →
    TrackAudio ⊳ v .:  "Format"
               ⊵ v .:? "Language" .!= Language_Other "none"
               ⊵ v .:? "Default"  .!= IsntDefault
               ⊵ v .:? "Forced"   .!= IsntForced
               ⊵ v .:  "Channels"

--------------------

audioFormat ∷ Lens' TrackAudio AudioFormat
audioFormat = lens _audioFormat (\ at f → at { _audioFormat = f })

--------------------

audioLang ∷ Lens' TrackAudio Language
audioLang = lens _audioLang (\ at l → at { _audioLang = l })

--------------------

audioDefault ∷ Lens' TrackAudio IsDefault
audioDefault = lens _audioDefault (\ at d → at { _audioDefault = d })

--------------------

audioForced ∷ Lens' TrackAudio IsForced
audioForced = lens _audioForced (\ at f → at { _audioForced = f })

--------------------

audioChannels ∷ Lens' TrackAudio AudioChannels
audioChannels = lens _audioChannels (\ at c → at { _audioChannels = c })

----------

instance Printable TrackAudio where
  print a = P.text $ [fmt|%T(%d) %T%t%t|] (a ⊣ audioLang)
                                          (a ⊣ audioChannels)
                                          (a ⊣ audioFormat)
                                          (case a ⊣ audioDefault of
                                             IsDefault   → "+"
                                             IsntDefault → "")
                                          (case a ⊣ audioForced of
                                             IsForced   → "*"
                                             IsntForced → "")

------------------------------------------------------------

data TextFormat = TextFormat_ASS | TextFormat_PGS | TextFormat_UTF8
                | TextFormat_VobSub
  deriving (Eq,Show)

----------

instance FromJSON TextFormat where
  parseJSON = withText "TextFormat" $ \ v →
    case v of
      "ASS"       → pure TextFormat_ASS
      "PGS"       → pure TextFormat_PGS
      "UTF-8"     → pure TextFormat_UTF8
      "VobSub"    → pure TextFormat_VobSub
      _           → fail $ [fmt|Unknown text format: %t|] v

----------

instance Printable TextFormat where
  print TextFormat_ASS    = P.text "ASS"
  print TextFormat_PGS    = P.text "PGS"
  print TextFormat_UTF8   = P.text "UTF-8"
  print TextFormat_VobSub = P.text "VobSub"

------------------------------------------------------------

data TextTitle = TextTitle_EN_SDH | TextTitle_EN_US | TextTitle_EN_Forced
               | TextTitle_SDH | TextTitle_EN | TextTitle_Other 𝕋
  deriving (Eq,Show)

----------

instance FromJSON TextTitle where
  parseJSON = withText "TextTitle" $ \ t →
    case t of
      "English (SDH)"           → pure TextTitle_EN_SDH
      "ENG NON UDENTI - PGS"    → pure TextTitle_EN_SDH
      "ENG NON UDENTI - SRT"    → pure TextTitle_EN_SDH
      "SDH"                     → pure TextTitle_SDH
      "English (United States)" → pure TextTitle_EN_US
      "ENG REGULAR - PGS"       → pure TextTitle_EN
      "ENG REGULAR - SRT"       → pure TextTitle_EN
      "ENG FORCED - PGS"        → pure TextTitle_EN_Forced
      "ENG FORCED - SRT"        → pure TextTitle_EN_Forced
      _                         → pure $ TextTitle_Other t

------------------------------------------------------------

{-| "text" track type -}
data TrackText =
  TrackText { _textFormat   ∷ TextFormat
            , _textLang     ∷ Language
            , _textDefault  ∷ IsDefault
            , _textForced   ∷ IsForced
            , _textTitle    ∷ 𝕄 TextTitle
            , _textCodecID  ∷ 𝕋
            }
  deriving (Eq, Show)

----------

instance FromJSON TrackText where
  parseJSON = withObject "TrackText" $ \ v →
    TrackText ⊳ v .:  "Format"
              ⊵ v .:? "Language" .!= Language_Other "none"
              ⊵ v .:  "Default"
              ⊵ v .:  "Forced"
              ⊵ v .:? "Title"
              ⊵ v .:  "CodecID"

--------------------

textFormat ∷ Lens' TrackText TextFormat
textFormat = lens _textFormat (\ at f → at { _textFormat = f })

--------------------

textLang ∷ Lens' TrackText Language
textLang = lens _textLang (\ at l → at { _textLang = l })

--------------------

textDefault ∷ Lens' TrackText IsDefault
textDefault = lens _textDefault (\ at d → at { _textDefault = d })

--------------------

textForced ∷ Lens' TrackText IsForced
textForced = lens _textForced (\ at f → at { _textForced = f })

--------------------

instance Printable TrackText where
  print t = P.text $ [fmt|%T %T%t%t|] (t ⊣ textLang)
                                      (t ⊣ textFormat)
                                      (case t ⊣ textDefault of
                                         IsDefault   → "+"
                                         IsntDefault → "")
                                      (case t ⊣ textForced of
                                         IsForced   → "*"
                                         IsntForced → "")

------------------------------------------------------------

newtype TrackNull = TrackNull ()

------------------------------------------------------------

data TrackType = TrackTypeGeneral | TrackTypeVideo | TrackTypeAudio
               | TrackTypeText | TrackTypeNull
  deriving (Enum,Show)

data Track_ (t ∷ TrackType) where
  TrackTypeGeneral_ ∷ TrackGeneral → Track_ 'TrackTypeGeneral
  TrackTypeVideo_ ∷ TrackVideo → Track_ 'TrackTypeVideo
  TrackTypeAudio_ ∷ TrackAudio → Track_ 'TrackTypeAudio
  TrackTypeText_ ∷ TrackText → Track_ 'TrackTypeText
  -- we use TrackNull to help with parsing, for tracks we ignore
  -- (we parse them as TrackNull, and then drop them when forming a Tracks
  -- object)
  TrackTypeNull_ ∷ Track_ 'TrackTypeNull

deriving instance Eq (Track_ 'TrackTypeGeneral)
deriving instance Eq (Track_ 'TrackTypeVideo)
deriving instance Eq (Track_ 'TrackTypeAudio)
deriving instance Eq (Track_ 'TrackTypeText)

deriving instance Show (Track_ 'TrackTypeGeneral)
deriving instance Show (Track_ 'TrackTypeVideo)
deriving instance Show (Track_ 'TrackTypeAudio)
deriving instance Show (Track_ 'TrackTypeText)

------------------------------------------------------------

data Track = forall f. Track (Track_ f)

----------------------------------------

{-| things that can be converted to a `Track` -}
class MkTrack α where
  {-| convert α to a `Track` -}
  mkTrack ∷ α → Track

----------

instance MkTrack TrackNull where
  mkTrack _ = Track TrackTypeNull_

instance MkTrack TrackGeneral where
  mkTrack = Track ∘ TrackTypeGeneral_

----------

instance MkTrack TrackVideo where
  mkTrack = Track ∘ TrackTypeVideo_

----------

instance MkTrack TrackAudio where
  mkTrack = Track ∘ TrackTypeAudio_

----------

instance MkTrack TrackText where
  mkTrack = Track ∘ TrackTypeText_

----------------------------------------

instance Eq Track where
  Track (TrackTypeGeneral_ x) == Track (TrackTypeGeneral_ y) = x == y
  Track (TrackTypeVideo_ x)   == Track (TrackTypeVideo_ y)   = x == y
  Track (TrackTypeAudio_ x)   == Track (TrackTypeAudio_ y)   = x == y
  Track (TrackTypeText_ x)    == Track (TrackTypeText_ y)    = x == y
  _ == _ = 𝓕

----------

instance Show Track where
  showsPrec p (Track (TrackTypeGeneral_ x)) = showParen (p > 10) $
    showString "TrackTypeGeneral " ∘ showsPrec 11 x
  showsPrec p (Track (TrackTypeVideo_ x)) = showParen (p > 10) $
    showString "TrackTypeVideo " ∘ showsPrec 11 x
  showsPrec p (Track (TrackTypeAudio_ x)) = showParen (p > 10) $
    showString "TrackTypeAudio " ∘ showsPrec 11 x
  showsPrec p (Track (TrackTypeText_ x)) = showParen (p > 10) $
    showString "TrackTypeText " ∘ showsPrec 11 x
  showsPrec p (Track (TrackTypeNull_)) = showParen (p > 10) $
    showString "TrackTypeNull " ∘ showsPrec 11 ()

----------

instance FromJSON Track where
  parseJSON = withObject "Track" $ \ v →
    case v !? "@type" of
      𝓙 (String "General") → mkTrack @TrackGeneral ⊳ parseJSON (Object v)
      𝓙 (String "Video")   → mkTrack @TrackVideo   ⊳ parseJSON (Object v)
      𝓙 (String "Audio")   → mkTrack @TrackAudio   ⊳ parseJSON (Object v)
      𝓙 (String "Text")    → mkTrack @TrackText    ⊳ parseJSON (Object v)
      -- we ignore Menu (for now); it seems to be chapters
      𝓙 (String "Menu")    → pure $ mkTrack @TrackNull (TrackNull ())
      𝓙 t → error $ [fmt|Track type: %w|] t
      𝓝 → error "no @type"


------------------------------------------------------------

class HasGeneralTracks α where
  generalTracks ∷ Lens' α [TrackGeneral]
  generalTrack ∷ α → 𝔼 𝕋 TrackGeneral
  generalTrack a =
    case a ⊣ generalTracks of
      []   → 𝓛 "no general track found"
      [gt] → 𝓡 gt
      ts   → 𝓛 $ [fmt|too many (%d) general tracks found|] (ỻ ts)

instance HasGeneralTracks [TrackGeneral] where
  generalTracks = lens id (\ _ ts → ts)


------------------------------------------------------------

-- newtype Tracks = Tracks { _ttracks ∷ Vector Track }
data Tracks = Tracks { _generalTracks ∷ [TrackGeneral]
                     , _videoTracks ∷ [TrackVideo]
                     , _audioTracks ∷ [TrackAudio]
                     , _textTracks ∷ [TrackText]
                     }
  deriving (Eq,Show)

instance Semigroup Tracks where
  (Tracks g v a t) <> (Tracks g' v' a' t') =
    Tracks (g◇g') (v◇v') (a◇a') (t◇t')

instance Monoid Tracks where
  mempty = Tracks [] [] [] []

----------

instance FromJSON Tracks where
    parseJSON = withArray "Tracks" $ \ v → do
      trks ← mapM parseJSON v
      let ts ∷ Tracks = foldl addTrack ф trks
      return ts

--------------------

instance HasGeneralTracks Tracks where
-- generalTracks ∷ Lens' Tracks [TrackGeneral]
  generalTracks = lens _generalTracks (\ ts gs → ts { _generalTracks = gs })

--------------------

videoTracks ∷ Lens' Tracks [TrackVideo]
videoTracks = lens _videoTracks (\ ts vs → ts { _videoTracks = vs })

--------------------

audioTracks ∷ Lens' Tracks [TrackAudio]
audioTracks = lens _audioTracks (\ ts as → ts { _audioTracks = as })

--------------------

textTracks ∷ Lens' Tracks [TrackText]
textTracks = lens _textTracks (\ ts tt → ts { _textTracks = tt })

--------------------

addTrack ∷ Tracks → Track → Tracks
addTrack ts (Track (TrackTypeGeneral_ g)) = ts & generalTracks <>~ [g]
addTrack ts (Track (TrackTypeVideo_   v)) = ts & videoTracks   <>~ [v]
addTrack ts (Track (TrackTypeAudio_   a)) = ts & audioTracks   <>~ [a]
addTrack ts (Track (TrackTypeText_    x)) = ts & textTracks    <>~ [x]
addTrack ts (Track (TrackTypeNull_     )) = ts

--------------------
-- ttracks ∷ Lens' Tracks (Vector Track)
-- ttracks = lens _ttracks (\ t ts → t { _ttracks = ts })

------------------------------------------------------------

data Media = Media { _ref ∷ AbsFile, _tracks ∷ Tracks }  deriving  (Eq,Show)

----------

instance FromJSON Media where
    parseJSON = withObject "Media" $ \ v → do
      rf    ← __parse__ @_ @𝕋 ⊳ v .: "@ref"
      trks ← v .: "track"

      return $ Media rf trks

--------------------

class HasConsistencyCheck α where
  checkConsistency ∷ α → [𝕋]

instance HasConsistencyCheck Tracks where
  checkConsistency trks =

    case generalTrack trks of
      𝓛 e → [e]
      𝓡 gt →  let tcount    = ỻ (trks ⊣ textTracks)
                  gt_tc     = gt ⊣ textCount
              in  if tcount ≠ fromIntegral gt_tc
                  then [[fmt|expected %d text tracks, found %d|] gt_tc tcount]
                  else []

--------------------

ref ∷ Lens' Media AbsFile
ref = lens _ref (\ m f → m { _ref = f })

--------------------

tracks ∷ Lens' Media Tracks
tracks = lens _tracks (\ m ts → m { _tracks = ts })

--------------------

instance HasGeneralTracks Media where
  generalTracks = tracks ∘ generalTracks

--------------------

instance HasConsistencyCheck Media where
  checkConsistency = checkConsistency ∘ view tracks

--------------------

videoTrack ∷ Media → 𝔼 𝕋 TrackVideo
videoTrack m =
  case m ⊣ tracks ∘ videoTracks of
    []   → 𝓛 $ [fmt|no video track found in %T|] (m ⊣ ref)
    [vt] → 𝓡 vt
    ts   → 𝓛 $ [fmt|too many (%d) video tracks found in %T|] (ỻ ts) (m ⊣ ref)

------------------------------------------------------------

{-| representation of data parsed from mediainfo command -}
data MediaInfo = MediaInfo { _media ∷ Media }  deriving (Eq,Show)

----------

instance FromJSON MediaInfo where
    parseJSON = withObject "MediaInfo" $ \ v →
      MediaInfo ⊳ (v .: "media")

----------

instance HasConsistencyCheck MediaInfo where
  checkConsistency = checkConsistency ∘ view media

----------

miToText ∷ MediaInfo → 𝔼 𝕋 𝕋
miToText mi = do
  gt ← generalTrack (mi ⊣ media)
  vt ← videoTrack   (mi ⊣ media)
  let framerate = round @Float @Word64 ∘ fromRational ∘ toRational $
                    gt ⊣ frameRate ∘ ratio
  let topline = [fmt|%T %dx%d %-5T %T %y %dFPS|] (mi ⊣ media ∘ ref)
                                                 (vt ⊣ videoWidth)
                                                 (vt ⊣ videoHeight)
                                                 (vt ⊣ videoFormat)
                                                 (gt ⊣ duration)
                                                 (gt ⊣ fileSize ∘ boundedI)
                                                 framerate
  let aline (i∷ℕ,a) = [fmt|  A%02d: %T|] i a
      tline (i∷ℕ,t) = [fmt|  T%02d: %T|] i t
  return ∘ T.unlines $ [topline]
                     ◇ (aline ⊳ (zip [1..] $ mi ⊣ media ∘ tracks ∘ audioTracks))
                     ◇ (tline ⊳ (zip [1..] $ mi ⊣ media ∘ tracks ∘ textTracks))

instance Printable MediaInfo where
  print = P.text ∘ either ([fmt|ERROR: %T|]) ("OKAY: " ◇) ∘ miToText

--------------------

media ∷ Lens' MediaInfo Media
media = lens _media (\ mi m → mi { _media = m })

------------------------------------------------------------

mediainfo ∷  ∀ ε γ δ μ . (MonadIO μ, FileAs γ, MonadLog (Log MockIOClass) μ,
                          HasDoMock δ, MonadReader δ μ,
                          AsIOError ε, AsFPathError ε, AsCreateProcError ε,
                          AsProcExitError ε, AsAesonError ε, Printable ε,
                          MonadError ε μ) =>
             γ → μ MediaInfo

mediainfo fn = do
  (_, json) ← ꙩ (Paths.mediaInfo, ["--Output=JSON", toText fn])
  case eitherDecodeStrict @MediaInfo json of
    𝓡 info → return info
    𝓛 e    → throwAsAesonError e

----------

_test_mediainfo ∷ (MonadIO μ, MonadMask μ) => 𝕄 AbsFile → μ ()
_test_mediainfo fn = do
  let movies_dir = [absdir|/local/martyn/Movies/|]
      mickey_dir = [reldir|Mickey 17  [2025]  (15)  1080p  HEVC/|]
      def_fn = movies_dir ⫻ mickey_dir ⫻ [relfile|Mickey 17.mkv|]
      runNoMock = flip runReaderT NoMock
  e ← logit @UsageParseAesonFPPIOError $ runNoMock $ mediainfo (def_fn ⧐ fn)
  case e of
    𝓛 e' → say $ [fmtT|ERROR: %T|] e'
    𝓡 r → say r

----------------------------------------

tests ∷ TestTree
tests =
  let decode = eitherDecode @MediaInfo
  in  testGroup "MediaInfo" $
        let fn          = [absfile|/local/martyn/TV/Alien: Earth - 01x03.mkv|]
            check t e x = testCase t $ assertRight (assertEqual t e) (decode x)
            tgen        = TrackGeneral 1 1 23 (msecs 3259.552) 2116638261 24
            -- tgen        = mkTrack tgen
            tvid        = TrackVideo VideoFormat_AVC 1920 1080
            taud        = TrackAudio AudioFormat_EAC3 Language_EN
                                     IsDefault IsntForced 6
            txt_utf8    = "S_TEXT/UTF8"
            txt1        = TrackText TextFormat_UTF8 Language_EN IsDefault
                                    IsntForced (𝓙 TextTitle_EN_SDH) txt_utf8
            txt2        = TrackText TextFormat_UTF8 (Language_Other "cs")
                                    IsntDefault IsntForced
                                    (𝓙 $ TextTitle_Other "Čeština") txt_utf8
        in  [ let txt = "Unexpected end-of-input, expecting JSON value"
              in  testCase "t0" $
                  assertLeft (\ e → assertBool ("t0: " ◇ e) $ txt `isInfixOf` e)
                             (decode t0)
            , check "t1" (MediaInfo (Media [absfile|/file|] ф)) t1
            , let expect = MediaInfo (Media fn ф) in check "t2" expect t2
            , let expect = MediaInfo (Media fn (ф & generalTracks <>~ [tgen]))
              in  check "t3" expect t3
            , let expect = MediaInfo (Media fn (ф & generalTracks <>~ [tgen]
                                                  & videoTracks   <>~ [tvid]))
              in  check "t4" expect t4
            , let -- ts = fromList [tgen, tvid, taud]
                  expect = MediaInfo (Media fn (ф & generalTracks <>~ [tgen]
                                                  & videoTracks   <>~ [tvid]
                                                  & audioTracks   <>~ [taud]
                                               ))
              in  check "t5" expect t5
            , let -- ts = fromList [tgen, tvid, taud, txt1, txt2]
                  expect = MediaInfo (Media fn(ф & generalTracks <>~ [tgen]
                                                 & videoTracks   <>~ [tvid]
                                                 & audioTracks   <>~ [taud]
                                                 & textTracks    <>~ [txt1,txt2]
                                              ))
              in  check "t6" expect t6
            ]


-- main ------------------------------------------------------------------------

data Options = Options { _inputs ∷ NonEmpty 𝕋 }

----------

instance Printable Options where
  print (Options ins) = P.text $ [fmt|%L|] ([fmtT|"%t|] ⊳ ins)

--------------------

parseOptions ∷ Parser Options
parseOptions =
  Options ⊳ parseNE (argument str (metavar "FILENAME"))

----------------------------------------

inputs ∷ ∀ ε μ . (AsIOError ε, AsFPathError ε, MonadError ε μ,
                  HasCallStack, MonadIO μ) ⇒
         Options → μ (NonEmpty AbsFile)
inputs o = sequence $ pResolve @AbsFile ⊳ _inputs o

------------------------------------------------------------

myMain ∷ ∀ ε .
         (HasCallStack, AsIOError ε, AsFPathError ε, AsCreateProcError ε,
          AsProcExitError ε, AsAesonError ε, Printable ε) ⇒
         Options → LoggingT (Log MockIOClass) (ExceptT ε IO) ()
myMain opts = flip runReaderT NoMock $ do
  ins ∷ NonEmpty AbsFile ← inputs opts
  forM_ ins $ \ input → do
    md_info ← mediainfo input
    mapM_ warnT (checkConsistency md_info)
    liftIO $ say md_info -- printer

----------------------------------------

{-| parse output of mediainfo command for each named file, summarize it -}
main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "run mediainfo for each file; summarize the findings"
      my_main = myMain @UsageParseAesonFPPIOError
  getArgs ≫ (\ args → stdMainNoDR progDesc parseOptions my_main args)


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
