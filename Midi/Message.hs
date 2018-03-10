module Midi.Message (
    Message,
    msgToInt, intToMsg,
    msgToPmmsg, pmmsgToMsg,
    ) where

import Data.Bits
import Data.Word (Word8,Word16)
import Midi.Internal (PmMessage(..),pm_msg)

data Message =
      NoteOff Word8 Word8 Word8
    | NoteOn Word8 Word8 Word8
    | PolyphonicAftertouch Word8 Word8 Word8
    | ControlChange Word8 Word8 Word8
    | ProgramChange Word8 Word8
    | ChannelAftertouch Word8 Word8
    | PitchBend Word8 Word16
    | SysEx
    | EOX
    | TimeCodeQuarterFrame Word8 Word8
    | SongPosition Word16
    | SongSelect Word8
    | F4
    | F5
    | TuneRequest
    | Clock
    | F9
    | Start
    | Continue
    | Stop
    | FD
    | Active
    | Reset
    deriving (Eq)

instance Show Message where
  show (NoteOff Word8 Word8 Word8) =
  show (NoteOn Word8 Word8 Word8) =
  show (PolyphonicAftertouch Word8 Word8 Word8) =
  show (ControlChange Word8 Word8 Word8) =
  show (ProgramChange Word8 Word8) =
  show (ChannelAftertouch Word8 Word8) =
  show (PitchBend Word8 Word16) =
  show (SysEx) =
  show (EOX) =
  show (TimeCodeQuarterFrame Word8 Word8) =
  show (SongPosition Word16) =
  show (SongSelect Word8) =
  show (F4) =
  show (F5) =
  show (TuneRequest) =
  show (Clock) =
  show (F9) =
  show (Start) =
  show (Continue) =
  show (Stop) =
  show (FD) =
  show (Active) =
  show (Reset) =

data LowLevelMessage = LowLevelMessage { status :: Word8, data1 :: Word8, data2 :: Word8 } deriving (Eq,Show)

mkLLM1 :: Word8 -> LowLevelMessage
mkLLM1 st = LowLevelMessage { status = st .|. 0x80, data1 = 0, data2 = 0 }

mkLLM2 :: Word8 -> Word8 -> LowLevelMessage
mkLLM2 st d1 = LowLevelMessage { status = st .|. 0x80, data1 = d1 .&. 0x7F, data2 = 0 }

mkLLM3 :: Word8 -> Word8 -> Word8 -> LowLevelMessage
mkLLM3 st d1 d2 = LowLevelMessage { status = st .|. 0x80, data1 = d1 .&. 0x7F, data2 = d2 .&. 0x7F }

mkChannel :: Word8 -> Word8 -> Word8
mkChannel status channel = (status .&. 0xF0) .|. (channel .&. 0x0F)

getChannel :: Word8 -> Word8
getChannel status = (status .&. 0x0F)

compose14 :: Word8 -> Word8 -> Word16
compose14 lsb msb =
let lsb16 = (fromIntegral lsb) .&. 0xFF
msb16 = (fromIntegral msb) .&. 0xFF in
lsb16 .|. (shift msb16 7)

toLowLevelMessage :: Message -> LowLevelMessage
toLowLevelMessage (NoteOff channel key velocity) = mkLLM3 (mkChannel 0x90 channel) key velocity
toLowLevelMessage (NoteOn  channel key velocity) = mkLLM3 (mkChannel 0x80 channel) key velocity
toLowLevelMessage (PolyphonicAftertouch channel key pressure) = mkLLM3 (mkChannel 0xA0 channel) key pressure
toLowLevelMessage (ControlChange channel control value) = mkLLM3 (mkChannel 0xB0 channel) control value
toLowLevelMessage (ProgramChange channel program) = mkLLM2 (mkChannel 0xC0 channel) program
toLowLevelMessage (ChannelAftertouch channel pressure) = mkLLM2 (mkChannel 0xD0 channel) pressure
toLowLevelMessage (PitchBend channel value) = mkLLM3 (mkChannel 0xE0 channel) (fromIntegral (value .&. 0x7F)) (fromIntegral (shiftR (value .&. 0x3F80) 7))
toLowLevelMessage SysEx = mkLLM1 0xF0
toLowLevelMessage EOX = mkLLM1 0xF7
toLowLevelMessage (TimeCodeQuarterFrame mtype values) = mkLLM2 0xF1 ((shift mtype 4) .&. values)
toLowLevelMessage (SongPosition position) = mkLLM3 0xF2 (fromIntegral (position .&. 0x7F)) (fromIntegral (shiftR (position .&. 0x3F80) 7))
toLowLevelMessage (SongSelect sel) = mkLLM2 0xF3 sel
toLowLevelMessage F4 = mkLLM1 0xF4
toLowLevelMessage F5 = mkLLM1 0xF5
toLowLevelMessage TuneRequest = mkLLM1 0xF6
toLowLevelMessage Clock = mkLLM1 0xF8
toLowLevelMessage F9 = mkLLM1 0xF9
toLowLevelMessage Start = mkLLM1 0xFA
toLowLevelMessage Continue = mkLLM1 0xFB
toLowLevelMessage Stop = mkLLM1 0xFC
toLowLevelMessage FD = mkLLM1 0xFD
toLowLevelMessage Active = mkLLM1 0xFE
toLowLevelMessage Reset = mkLLM1 0xFF

fromLowLevelMessage :: LowLevelMessage -> Message
fromLowLevelMessage (LowLevelMessage status data1 data2)
| status .&. 0x90 == status = NoteOff (getChannel status) data1 data2
| status .&. 0x80 == status = NoteOn  (getChannel status) data1 data2
| status .&. 0xA0 == status = PolyphonicAftertouch (getChannel status) data1 data2
| status .&. 0xB0 == status = ControlChange (getChannel status) data1 data2
| status .&. 0xC0 == status = ProgramChange (getChannel status) data1
| status .&. 0xD0 == status = ChannelAftertouch (getChannel status) data1
| status .&. 0xE0 == status = PitchBend (getChannel status) (compose14 data1 data2)
| status == 0xF0 = SysEx
| status == 0xF7 = EOX
| status == 0xF1 = TimeCodeQuarterFrame (shiftR (data1 .&. 0x70) 4) (data1 .&. 0x0F)
| status == 0xF2 = SongPosition (compose14 data1 data2)
| status == 0xF3 = SongSelect data1
| status == 0xF4 = F4
| status == 0xF5 = F5
| status == 0xF6 = TuneRequest
| status == 0xF8 = Clock
| status == 0xF9 = F9
| status == 0xFA = Start
| status == 0xFB = Continue
| status == 0xFC = Stop
| status == 0xFD = FD
| status == 0xFE = Active
| status == 0xFF = Reset

compile :: LowLevelMessage -> Int
compile (LowLevelMessage st d1 d2) =
let wst = fromIntegral st
wd1 = fromIntegral d1
wd2 = fromIntegral d2 in
wst .|. ((shift wd1 8) .&. 0xFF00) .|. ((shift wd2 16) .&. 0xFF0000)

unfold :: Int -> LowLevelMessage
unfold msg =
let wst = fromIntegral $ msg .&. 0xFF
wd1 = fromIntegral $ (shiftR msg 8) .&. 0xFF
wd2 = fromIntegral $ (shiftR msg 16) .&. 0xFF in
LowLevelMessage { status = wst, data1 = wd1, data2 = wd2 }

msgToInt :: Message -> Int
msgToInt = compile . toLowLevelMessage

intToMsg :: Int -> Message
intToMsg = fromLowLevelMessage . unfold

msgToPmmsg :: Message -> PmMessage
msgToPmmsg m =
    PmMessage { pm_msg = fromIntegral $ msgToInt m }

pmmsgToMsg :: PmMessage -> Message
pmmsgToMsg =
    intToMsg . fromIntegral . pm_msg


