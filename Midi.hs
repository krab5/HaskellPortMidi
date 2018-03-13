{-
   Main module of the library.
   Allow for MIDI stream manipulation (opening, closing, writing and
   reading).

   Every function is wrapped in IO Monads with exception system and
   data is manipulated through high-level structures, allowing for
   easy MIDI "program" writing.
   
   @author krab5
   
   Changelog:
    2018/03/01  Creation
    2018/03/09  First version completion
    2018/03/10  Commenting and cleaning for release
-}
module Midi (
    StreamError, errorMessage,
    MessageFilter(..),
    ChannelMask, allChannels, noChannels, maskChannel, unmaskChannel, combineMasks,
    DeviceInfo, structureVersion, apiName, deviceName, isInput, isOutput, isOpened,
    Event, message, timestamp, mkEvent, fromMessage,
    Stream, noStream,
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

-- Import
import Data.Bits
import Data.Word (Word8,Word16)
import qualified Midi.Internal as I
import Midi.Message
import Type.Reflection (Typeable,)
import Control.Exception

{-
   Device data structure

   There are basically two types of devices : no device at all and
   a device with a number.

   Device numbers are managed by PortMidi API, care should be taken
   not to build Device with non-existing device number.
-}
data Device = NoDevice | Device Int deriving (Eq)

instance Show Device where
  show NoDevice = "#ND"
  show (Device i) = "#" ++ show i

-- Internal - switching back and forth between the high-level Device structure
-- and PortMidi PmDeviceID
device_to_pmdevice :: Device -> I.PmDeviceID
device_to_pmdevice NoDevice = I.pmNoDevice
device_to_pmdevice (Device i) = I.PmDeviceID { I.pm_dev_id = fromIntegral i }

pmdevice_to_device :: I.PmDeviceID -> Device
pmdevice_to_device dev
    | dev == I.pmNoDevice = NoDevice
    | otherwise           = Device $ fromIntegral $ I.pm_dev_id dev

{-
   Timestamp type

   Timestamp is used for dating messages.
   When reading input, events retrieved are assigned a timestamp indicating
   when the underlying API received them.
   When using output though, timestamp is not needed, unless the output
   stream has been opened with a non-zero latency (see PortMidi documentation
   for more details).
-}
type Timestamp = Int

ts_to_pmts :: Timestamp -> I.PmTimestamp
ts_to_pmts ts = I.PmTimestamp { I.pm_ts = fromIntegral ts }

pmts_to_ts :: I.PmTimestamp -> Timestamp
pmts_to_ts = fromIntegral . I.pm_ts

{-
    StreamError data type

    This high-level data type is used mostly as an intermediate for
    throwing exceptions.
-}
data StreamError =
          NoError
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
    | err == I.pmHostError = HostError
    | err == I.pmInvalidDeviceId = InvalidDeviceID
    | err == I.pmInsufficientMemory = InsufficientMemory
    | err == I.pmBufferTooSmall = BufferTooSmall
    | err == I.pmBufferOverflow = BufferOverflow
    | err == I.pmBadPtr = BadPtr
    | err == I.pmBadData = BadData
    | err == I.pmBufferMaxSize = BufferMaxSize
    | err == I.pmInternalError = InternalError
    | I.errorCode err >= 0 = NoError
    | otherwise = InternalError

{-
   Get the message corresponding to the error.
   The error text is given by PortMidi itself, hence the IO output.
-}
errorMessage :: StreamError -> IO String
errorMessage = I.errorText . err_to_pmerr

{-
    MessageFilter data type

    PortMidi supports message filtering, allowing for automatically
    discarding any unused message.
-}
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

{-
    ChannelMask data type

    PortMidi allows for masking messages from some channels, basically
    discarding messages from whichever channel is not in the mask
    automatically.
-}
newtype ChannelMask = ChannelMask { wrd :: Word16 } deriving (Eq)

-- Build a channel mask where every channel is being allowed
allChannels :: ChannelMask
allChannels = ChannelMask $ complement zeroBits

-- Build a channel mask where every channel is being denied
noChannels :: ChannelMask
noChannels = ChannelMask $ zeroBits

-- Mask a channel in the channel mask, making it disappear
maskChannel :: ChannelMask -> Int -> ChannelMask
maskChannel (ChannelMask msk) i =
    ChannelMask $ clearBit msk i

-- Unmask a channel in the channel mask, making it being listened to
unmaskChannel :: ChannelMask -> Int -> ChannelMask
unmaskChannel (ChannelMask msk) i =
    ChannelMask $ setBit msk i

-- Combine two channel masks
combineMasks :: ChannelMask -> ChannelMask -> ChannelMask
combineMasks (ChannelMask msk1) (ChannelMask msk2) =
    ChannelMask $ msk1 .|. msk2

instance Show ChannelMask where
  show (ChannelMask wrd) =
      foldr (flip (++) . show) "" $ map (((.&.) 0x01) . (shiftR wrd)) [0..(finiteBitSize wrd)]

{-
    DeviceInfo data type

    High level mapping of a PortMidi structure (PmDeviceInfo) that represent
    a device.
    The structure is made with 6 fields:
      - structureVersion: version of the struct returned by PortMidi
      - apiName: name of the underlying API managing the device
      - deviceName: name of the device
      - isInput,isOutput: true if the device can serve as input or output (respectively)
      - isOpened: true if the device is currently opened (by the user or by another
                  application)
-}
data DeviceInfo = DeviceInfo {
    structureVersion :: Int,
    apiName :: String,
    deviceName :: String,
    isInput :: Bool,
    isOutput :: Bool,
    isOpened :: Bool
} deriving (Eq)

-- Human readable form for the DeviceInfo structure.
-- Note that structure version and API name are not shown in this form
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

{-
    Event structure

    High level mapping of the PortMidi PmEvent structure, representing
    a MIDI event.
    Such event is made of a "message" field, holding the actual content of
    the event, as well as a "timestamp" field that indicates when the event
    occurred (when being read) or when it should occurre (when being written
    in a stream with a non-zero latency).
-}
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

-- Build an event with given message and timestamp.
mkEvent :: Message -> Timestamp -> Event
mkEvent m t = Event { message = m, timestamp = t }

-- Build an event with given message. Timestamp is set to 0.
fromMessage :: Message -> Event
fromMessage = flip mkEvent $ 0

{-
    Stream data type

    (kind of) Abstract type representing a MIDI stream.
-}
data Stream = Stream { ctx_ :: I.PmContext }

noStream :: IO Stream
noStream = fmap (Stream) $ I.noContext

{-
    MIDIException data type

    Enumeration of exception that can be raised when manipulating
    MIDI streams.
-}
data MIDIException =
          HostErrorException
        | InvalidDeviceIDException
        | InsufficientMemoryException
        | BufferTooSmallException
        | BufferOverflowException
        | BadPtrException
        | BadDataException
        | InternalErrorException
        | BufferMaxSizeException
        deriving (Eq,Typeable)

instance Show MIDIException where
  show HostErrorException = "Host error"
  show InvalidDeviceIDException = "Invalid device ID"
  show InsufficientMemoryException = "Insufficient memory"
  show BufferTooSmallException = "Buffer too small"
  show BufferOverflowException = "Buffer overflow"
  show BadPtrException = "Bad pointer"
  show BadDataException = "Invalid MIDI data"
  show InternalErrorException = "Internal PortMidi error"
  show BufferMaxSizeException = "Buffer reached maximum size"

instance Exception MIDIException

errorToException :: StreamError -> MIDIException
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
throwIOMIDIException NoError = (return ())
throwIOMIDIException se = throw $ errorToException se

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


{- Functions -}
-- Initialize the MIDI API
-- This function should be called before any other
initialize :: IO ()
initialize = 
    (fmap pmerr_to_err $ I.c_Pm_Initialize) >>= throwIOMIDIException

-- Shut down the MIDI API
-- This function should be applied whenever MIDI is not needed anymore
terminate :: IO ()
terminate = 
    (fmap pmerr_to_err $ I.c_Pm_Terminate) >>= throwIOMIDIException

-- Test whether or not the given stream as encountered errors
hasHostError :: Stream -> IO Bool
hasHostError = I.hasHostError . ctx_

-- Retrieve the error message associated with any error the host encountered
hostErrorText :: IO String
hostErrorText = I.hostErrorText

-- Retrieve the number of devices
countDevices :: IO Int
countDevices = 
    fmap fromIntegral $ I.c_Pm_CountDevices

-- Get the default input device
-- May raise an exception if there are no default input device
defaultInputDevice :: IO Device
defaultInputDevice = 
    (fmap pmdevice_to_device $ I.c_Pm_GetDefaultInputDeviceID) 

-- Get the default output device
-- -- May raise an exception if there are no default output device
defaultOutputDevice :: IO Device
defaultOutputDevice =
    (fmap pmdevice_to_device $ I.c_Pm_GetDefaultOutputDeviceID)

-- Get the informations related to the given device
-- Throw an exception if the device does not exist
getDeviceInfo :: Device -> IO DeviceInfo
getDeviceInfo dev = do
    result <- I.getDeviceInfo $ device_to_pmdevice dev
    case result of
      Nothing -> throw InvalidDeviceIDException
      Just a -> pmdevinfo_to_devinfo a

-- Open given device for input with given event buffer size
-- Returns a Stream abstract object
-- May raise an exception in case of problem (e.g.: invalid device)
openInput :: Device -> Int -> IO Stream
openInput dev buffsize = do
    result <- I.openInput (device_to_pmdevice dev) buffsize
    case result of
      Left err -> throw $ errorToException $ pmerr_to_err err
      Right ct -> return (Stream { ctx_ = ct })

-- Open given device for output with given event buffer size and latency
-- Returns a Stream abstract object
-- May raise an exception in case of problem
openOutput :: Device -> Int -> Int -> IO Stream
openOutput dev buffsize latency = do
    result <- I.openOutput (device_to_pmdevice dev) buffsize latency
    case result of
      Left err -> throw $ errorToException $ pmerr_to_err err
      Right ct -> return (Stream { ctx_ = ct })

-- Set the filter applied by PortMIDI when processing events into or from
-- the stream.
-- May raise an exception if a problem occured.
setFilter :: Stream -> [MessageFilter] -> IO ()
setFilter (Stream st) filts = 
    (fmap pmerr_to_err $ I.setFilter st (filt_combine filts)) >>= throwIOMIDIException

-- Set the channel mask applied by PortMIDI when processing events into or from
-- the stream.
-- May raise an exception if a problem occured.
setChannelMask :: Stream -> ChannelMask -> IO ()
setChannelMask (Stream st) (ChannelMask ch) =
    (fmap pmerr_to_err $ I.setChannelMask st ch) >>= throwIOMIDIException

-- Cancel any sending, discard any incoming message and close the stream.
-- May raise an exception if a problem occured.
abort :: Stream -> IO ()
abort (Stream ctx) =
    (fmap pmerr_to_err $ I.abort $ ctx) >>= throwIOMIDIException

-- Close the stream.
-- May raise an exception if a problem occured.
close :: Stream -> IO ()
close (Stream ctx) = 
    (fmap pmerr_to_err $ I.close $ ctx) >>= throwIOMIDIException

-- Force the device to resynchronize
-- May raise an exception if a problem occured.
synchronize :: Stream -> IO ()
synchronize (Stream ctx) =
    (fmap pmerr_to_err $ I.synchronize $ ctx) >>= throwIOMIDIException

-- Read the stream for incoming messages, retrieving up to "bufsize" events
-- from it.
-- Note that the resulting output will be of *at most* n elements, where n
-- is the minimum between bufsize and the buffer size passed on when opening
-- the stream.
-- An exception may be raised.
readInput :: Int -> Stream -> IO [Event]
readInput buffsize (Stream st) =
    do
        result <- I.readInput st buffsize
        case result of
          Left err -> throw $ errorToException $ pmerr_to_err err
          Right evts -> return $ map pmevent_to_event evts

-- Ask the device if there are anything to read on the given stream.
-- May raise an exception.
pollInput :: Stream -> IO Bool
pollInput (Stream st) =
    do
        result <- I.pollInput st
        case result of
          Left err -> throw $ errorToException $ pmerr_to_err err
          Right b -> return b

-- Write a list of event (in order) to the output stream.
-- May raise an exception.
writeOutput :: Stream -> [Event] -> IO ()
writeOutput (Stream st) evts =
    (fmap pmerr_to_err $ I.writeOutput st $ map event_to_pmevent evts) >>= throwIOMIDIException

-- Write one event to the output stream.
-- May raise an exception.
writeShort :: Stream -> Event -> IO ()
writeShort (Stream st) evt =
    (fmap pmerr_to_err $ I.writeShort st $ event_to_pmevent evt) >>= throwIOMIDIException

-- Write a bunch of Sysex messages to the output stream.
-- Sysex message list should end with "EOX", which stands for "End Of syseX"
-- May raise an exception.
writeSysEx :: Stream -> Timestamp -> [Word8] -> IO ()
writeSysEx (Stream st) ts wrds =
    (fmap pmerr_to_err $ I.writeSysEx st (ts_to_pmts ts) wrds) >>= throwIOMIDIException




