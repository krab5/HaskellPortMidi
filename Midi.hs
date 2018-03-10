module Midi (
    StreamError, errorMessage,
    MessageFilter(..),
    ChannelMask, allChannels, noChannels, maskChannel, unmaskChannel, combineMasks,
    DeviceInfo, structureVersion, apiName, deviceName, isInput, isOutput, isOpened,
    Event, message, timestamp, mkEvent, fromMessage,
    Stream,
    MIDIException(..), errorToException,
    Device(..),
    Timestamp,
    initialize, terminate,
    hasHostError, hostErrorText,
    countDevices, defaultInputDevice, defaultOutputDevice,
    getDeviceInfo,
    openInput, openOutput,
    setFilter, setChannelMask,
    abort, close, synchronize,
    readInput, pollInput,
    writeOutput, writeShort, writeSysEx,
    ) where

import Data.Bits
import Data.Word (Word8,Word16)
import qualified Midi.Internal as I
import Midi.Message
import Type.Reflection (Typeable,)
import Control.Exception

{- Devices -}
data Device = NoDevice | Device Int deriving (Eq)

instance Show Device where
  show NoDevice = "#ND"
  show (Device i) = "#" ++ show i

device_to_pmdevice :: Device -> I.PmDeviceID
device_to_pmdevice NoDevice = I.pmNoDevice
device_to_pmdevice (Device i) = I.PmDeviceID { I.pm_dev_id = fromIntegral i }

pmdevice_to_device :: I.PmDeviceID -> Device
pmdevice_to_device dev
    | dev == I.pmNoDevice = NoDevice
    | otherwise           = Device $ fromIntegral $ I.pm_dev_id dev

{- Timestamp -}
type Timestamp = Int

ts_to_pmts :: Timestamp -> I.PmTimestamp
ts_to_pmts ts = I.PmTimestamp { I.pm_ts = fromIntegral ts }

pmts_to_ts :: I.PmTimestamp -> Timestamp
pmts_to_ts = fromIntegral . I.pm_ts

{- Error -}
data StreamError =
          NoError
        | NoData
        | GotData
        | HostError
        | InvalidDeviceID
        | InsufficientMemory
        | BufferTooSmall
        | BufferOverflow
        | BadPtr
        | BadData
        | InternalError
        | BufferMaxSize
        deriving (Eq,Show)

err_to_pmerr :: StreamError -> I.PmError
err_to_pmerr err =
    case err of
      NoError -> I.pmNoError
      NoData -> I.pmNoData
      GotData -> I.pmGotData
      HostError -> I.pmHostError
      InvalidDeviceID -> I.pmInvalidDeviceId
      InsufficientMemory -> I.pmInsufficientMemory
      BufferTooSmall -> I.pmBufferTooSmall
      BufferOverflow -> I.pmBufferOverflow
      BadPtr -> I.pmBadPtr
      BadData -> I.pmBadData
      InternalError -> I.pmInternalError
      BufferMaxSize -> I.pmBufferMaxSize

pmerr_to_err :: I.PmError -> StreamError
pmerr_to_err err 
    | err == I.pmNoError = NoError
    | err == I.pmNoData = NoData
    | err == I.pmGotData = GotData
    | err == I.pmHostError = HostError
    | err == I.pmInvalidDeviceId = InvalidDeviceID
    | err == I.pmInsufficientMemory = InsufficientMemory
    | err == I.pmBufferTooSmall = BufferTooSmall
    | err == I.pmBufferOverflow = BufferOverflow
    | err == I.pmBadPtr = BadPtr
    | err == I.pmBadData = BadData
    | err == I.pmBufferMaxSize = BufferMaxSize
    | err == I.pmInternalError = InternalError
    | otherwise = InternalError

errorMessage :: StreamError -> IO String
errorMessage = I.errorText . err_to_pmerr

{- Filters -}
data MessageFilter =
          FUndefined
        | FActive
        | FSysEx
        | FClock
        | FPlay
        | FTick
        | FFD
        | FReset
        | FRealtime
        | FNote
        | FChannelAftertouch
        | FPolyphonicAftertouch
        | FAftertouch
        | FProgram
        | FControl
        | FPitchbend
        | FMTC
        | FSongPosition
        | FSongSelect
        | FTune
        | FSystemCommon
        deriving (Eq,Show)

filter_to_pmfilter :: MessageFilter -> I.PmFilter
filter_to_pmfilter filter =
    case filter of
      FUndefined -> I.pmFiltUndefined
      FActive -> I.pmFiltActive
      FSysEx -> I.pmFiltSysex
      FClock -> I.pmFiltClock
      FPlay -> I.pmFiltPlay
      FTick -> I.pmFiltTick
      FFD -> I.pmFiltFD
      FReset -> I.pmFiltReset
      FRealtime -> I.pmFiltRealtime
      FNote -> I.pmFiltNote
      FChannelAftertouch -> I.pmFiltChannelAftertouch
      FPolyphonicAftertouch -> I.pmFiltPolyAftertouch
      FAftertouch -> I.pmFiltAftertouch
      FProgram -> I.pmFiltProgram
      FControl -> I.pmFiltControl
      FPitchbend -> I.pmFiltPitchbend
      FMTC -> I.pmFiltMTC
      FSongPosition -> I.pmFiltSongPosition
      FSongSelect -> I.pmFiltSongSelect
      FTune -> I.pmFiltTune
      FSystemCommon -> I.pmFiltSystemCommon

filt_combine :: [MessageFilter] -> I.PmFilter
filt_combine = I.filt_combine . map filter_to_pmfilter

{- Channel mask -}
newtype ChannelMask = ChannelMask { wrd :: Word16 } deriving (Eq)

allChannels :: ChannelMask
allChannels = ChannelMask $ complement zeroBits

noChannels :: ChannelMask
noChannels = ChannelMask $ zeroBits

maskChannel :: ChannelMask -> Int -> ChannelMask
maskChannel (ChannelMask msk) i =
    ChannelMask $ clearBit msk i

unmaskChannel :: ChannelMask -> Int -> ChannelMask
unmaskChannel (ChannelMask msk) i =
    ChannelMask $ setBit msk i

combineMasks :: ChannelMask -> ChannelMask -> ChannelMask
combineMasks (ChannelMask msk1) (ChannelMask msk2) =
    ChannelMask $ msk1 .|. msk2

instance Show ChannelMask where
  show (ChannelMask wrd) =
      foldr (flip (++) . show) "" $ map (((.&.) 0x01) . (shiftR wrd)) [0..(finiteBitSize b)]

{- Device info -}
data DeviceInfo = DeviceInfo {
    structureVersion :: Int,
    apiName :: String,
    deviceName :: String,
    isInput :: Bool,
    isOutput :: Bool,
    isOpened :: Bool
} deriving (Eq)

instance Show DeviceInfo where
  show (DeviceInfo _ _ dn i o op) =
      "<" ++ dn ++ "-[" ++ (if i then "I" else "") ++ (if o then "O" else "") ++ "]" ++ (if op then "-Opened" else "") ++ ">"

pmdevinfo_to_devinfo :: I.PmDeviceInfo -> IO DeviceInfo
pmdevinfo_to_devinfo pmdevinfo = 
    do
        interf <- I.getInterf pmdevinfo
        name <- I.getName pmdevinfo
        return DeviceInfo {
            structureVersion = I.getStructVersion pmdevinfo,
            apiName = interf,
            deviceName = name,
            isInput  = I.getIsInput pmdevinfo,
            isOutput = I.getIsOutput pmdevinfo,
            isOpened = I.getIsOpened pmdevinfo
        }

{- Event -}
data Event = Event {
    message :: Message,
    timestamp :: Timestamp
} deriving (Eq)

instance Show Event where
  show (Event msg ts) =
      "<" ++ (if ts == 0 then "_" else show ts) ++ ">" ++ show msg

pmevent_to_event :: I.PmEvent -> Event
pmevent_to_event evt =
    Event {
        message   = pmmsgToMsg $ I.pm_message evt,
        timestamp = pmts_to_ts $ I.pm_timestamp evt
    }

event_to_pmevent :: Event -> I.PmEvent
event_to_pmevent evt =
    I.PmEvent {
        I.pm_message   = msgToPmmsg $ message evt,
        I.pm_timestamp = ts_to_pmts $ timestamp evt
    }

mkEvent :: Message -> Timestamp -> Event
mkEvent m t = Event { message = m, timestamp = t }

fromMessage :: Message -> Event
fromMessage = flip mkEvent $ 0

{- Stream -}
data Stream = Stream { ctx_ :: I.PmContext }

{- Exceptions -}
data MIDIException =
          NoDeviceException
        | NoDataException
        | HostErrorException
        | InvalidDeviceIDException
        | InsufficientMemoryException
        | BufferTooSmallException
        | BufferOverflowException
        | BadPtrException
        | BadDataException
        | InternalErrorException
        | BufferMaxSizeException
        deriving (Eq,Show,Typeable)

instance Exception MIDIException

errorToException :: StreamError -> MIDIException
errorToException NoData             = NoDataException
errorToException HostError          = HostErrorException
errorToException InvalidDeviceID    = InvalidDeviceIDException
errorToException InsufficientMemory = InsufficientMemoryException
errorToException BufferTooSmall     = BufferTooSmallException
errorToException BufferOverflow     = BufferOverflowException
errorToException BadPtr             = BadPtrException
errorToException BadData            = BadDataException
errorToException InternalError      = InternalErrorException
errorToException BufferMaxSize      = BufferMaxSizeException

throwIOMIDIException :: StreamError -> IO ()
throwIOMIDIException se
    | se == NoError || se == GotData = return ()
    | otherwise = throw $ errorToException se

throwEitherMIDIException :: Either StreamError a -> IO a
throwEitherMIDIException ei =
    case ei of
      Left err -> throw $ errorToException err
      Right val -> return val

throwMaybeMIDIException :: Maybe a -> IO a
throwMaybeMIDIException m =
    case m of
      Nothing -> throw InvalidDeviceIDException
      Just a -> return a

throwNoDeviceMIDIException ::  Device -> IO Device
throwNoDeviceMIDIException dev =
    case dev of
      NoDevice -> throw NoDeviceException
      Device _ -> return dev


{- Functions -}
initialize :: IO ()
initialize = 
    (fmap pmerr_to_err $ I.c_Pm_Initialize) >>= throwIOMIDIException

terminate :: IO ()
terminate = 
    (fmap pmerr_to_err $ I.c_Pm_Terminate) >>= throwIOMIDIException

hasHostError :: Stream -> IO Bool
hasHostError = I.hasHostError . ctx_

hostErrorText :: IO String
hostErrorText = I.hostErrorText

countDevices :: IO Int
countDevices = 
    fmap fromIntegral $ I.c_Pm_CountDevices

defaultInputDevice :: IO Device
defaultInputDevice = 
    (fmap pmdevice_to_device $ I.c_Pm_GetDefaultInputDeviceID) >>= throwNoDeviceMIDIException

defaultOutputDevice :: IO Device
defaultOutputDevice =
    (fmap pmdevice_to_device $ I.c_Pm_GetDefaultOutputDeviceID) >>= throwNoDeviceMIDIException

getDeviceInfo :: Device -> IO DeviceInfo
getDeviceInfo dev = do
    result <- I.getDeviceInfo $ device_to_pmdevice dev
    case result of
      Nothing -> throw InvalidDeviceIDException
      Just a -> pmdevinfo_to_devinfo a

openInput :: Device -> Int -> IO Stream
openInput dev buffsize = do
    result <- I.openInput (device_to_pmdevice dev) buffsize
    case result of
      Left err -> throw $ errorToException $ pmerr_to_err err
      Right ct -> return (Stream { ctx_ = ct })

openOutput :: Device -> Int -> Int -> IO Stream
openOutput dev buffsize latency = do
    result <- I.openOutput (device_to_pmdevice dev) buffsize latency
    case result of
      Left err -> throw $ errorToException $ pmerr_to_err err
      Right ct -> return (Stream { ctx_ = ct })

setFilter :: Stream -> [MessageFilter] -> IO ()
setFilter (Stream st) filts = 
    (fmap pmerr_to_err $ I.setFilter st (filt_combine filts)) >>= throwIOMIDIException

setChannelMask :: Stream -> ChannelMask -> IO ()
setChannelMask (Stream st) (ChannelMask ch) =
    (fmap pmerr_to_err $ I.setChannelMask st ch) >>= throwIOMIDIException

abort :: Stream -> IO ()
abort (Stream ctx) =
    (fmap pmerr_to_err $ I.abort $ ctx) >>= throwIOMIDIException

close :: Stream -> IO ()
close (Stream ctx) = 
    (fmap pmerr_to_err $ I.close $ ctx) >>= throwIOMIDIException

synchronize :: Stream -> IO ()
synchronize (Stream ctx) =
    (fmap pmerr_to_err $ I.synchronize $ ctx) >>= throwIOMIDIException

readInput :: Int -> Stream -> IO [Event]
readInput buffsize (Stream st) =
    do
        result <- I.readInput st buffsize
        case result of
          Left err -> throw $ errorToException $ pmerr_to_err err
          Right evts -> return $ map pmevent_to_event evts

pollInput :: Stream -> IO Bool
pollInput (Stream st) =
    do
        result <- I.pollInput st
        case result of
          Left err -> throw $ errorToException $ pmerr_to_err err
          Right b -> return b

writeOutput :: Stream -> [Event] -> IO ()
writeOutput (Stream st) evts =
    (fmap pmerr_to_err $ I.writeOutput st $ map event_to_pmevent evts) >>= throwIOMIDIException

writeShort :: Stream -> Event -> IO ()
writeShort (Stream st) evt =
    (fmap pmerr_to_err $ I.writeShort st $ event_to_pmevent evt) >>= throwIOMIDIException

writeSysEx :: Stream -> Timestamp -> [Word8] -> IO ()
writeSysEx (Stream st) ts wrds =
    (fmap pmerr_to_err $ I.writeSysEx st (ts_to_pmts ts) wrds) >>= throwIOMIDIException




