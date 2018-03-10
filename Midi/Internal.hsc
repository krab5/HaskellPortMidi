{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Midi.Internal where

import Data.Ix
import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
--import System.IO (unsafePerformIO,)

#include <portmidi.h>

{- Types -}
-- Typedefs and enums
newtype PmError = PmError { errorCode :: CInt } deriving (Eq,Show)
newtype PmBool = PmBool { boolcode :: CInt } deriving (Eq,Show)
newtype PmFilter = PmFilter { filtercode :: CInt } deriving (Eq,Show)
newtype PmDeviceID = PmDeviceID { pm_dev_id :: CInt } deriving (Eq,Show)
newtype PmTimestamp = PmTimestamp { pm_ts :: CInt } deriving (Eq,Show,Ord)
newtype PmMessage = PmMessage { pm_msg :: CInt } deriving (Eq,Show)

type PmTimeProcPtr = FunPtr (Ptr () -> PmTimestamp)

-- Structs
data PmDeviceInfo = PmDeviceInfo {
    pm_structVersion :: CInt,
    pm_interf :: CString,
    pm_name :: CString,
    pm_input :: PmBool,
    pm_output :: PmBool,
    pm_opened :: PmBool
} deriving (Eq,Show)

data PmEvent = PmEvent {
    pm_message :: PmMessage,
    pm_timestamp :: PmTimestamp
} deriving (Eq,Show)

-- Abstract types
data PmStream

{- Enumeration definition -}
#{enum PmBool, PmBool,
    pmTrue = TRUE,
    pmFalse = FALSE
}

#{enum PmError, PmError,
    pmNoError = pmNoError,
    pmNoData = pmNoData,
    pmGotData = pmGotData,
    pmHostError = pmHostError,
    pmInvalidDeviceId = pmInvalidDeviceId,
    pmInsufficientMemory = pmInsufficientMemory,
    pmBufferTooSmall = pmBufferTooSmall,
    pmBufferOverflow = pmBufferOverflow,
    pmBadPtr = pmBadPtr,
    pmBadData = pmBadData,
    pmInternalError = pmInternalError,
    pmBufferMaxSize = pmBufferMaxSize
}

#{enum PmFilter, PmFilter,
    pmFiltActive = PM_FILT_ACTIVE,
    pmFiltSysex = PM_FILT_SYSEX,
    pmFiltClock = PM_FILT_CLOCK,
    pmFiltPlay = PM_FILT_PLAY,
    pmFiltTick = PM_FILT_TICK,
    pmFiltFD = PM_FILT_FD,
    pmFiltUndefined = PM_FILT_UNDEFINED,
    pmFiltReset = PM_FILT_RESET,
    pmFiltRealtime = PM_FILT_REALTIME,
    pmFiltNote = PM_FILT_NOTE,
    pmFiltChannelAftertouch = PM_FILT_CHANNEL_AFTERTOUCH,
    pmFiltPolyAftertouch = PM_FILT_POLY_AFTERTOUCH,
    pmFiltAftertouch = PM_FILT_AFTERTOUCH,
    pmFiltProgram = PM_FILT_PROGRAM,
    pmFiltControl = PM_FILT_CONTROL,
    pmFiltPitchbend = PM_FILT_PITCHBEND,
    pmFiltMTC = PM_FILT_MTC,
    pmFiltSongPosition = PM_FILT_SONG_POSITION,
    pmFiltSongSelect = PM_FILT_SONG_SELECT,
    pmFiltTune = PM_FILT_TUNE,
    pmFiltSystemCommon = PM_FILT_SYSTEMCOMMON
}

{- Constants -}
pmNoDevice :: PmDeviceID
pmNoDevice = PmDeviceID (#const pmNoDevice)

pmDefaultSysexBufferSize :: CInt
pmDefaultSysexBufferSize = #const PM_DEFAULT_SYSEX_BUFFER_SIZE

__pm_HDRLength :: CInt
__pm_HDRLength = #const HDRLENGTH

pmHostErrorMsgLen :: CUInt
pmHostErrorMsgLen = #const PM_HOST_ERROR_MSG_LEN

midiEox :: Word8
midiEox = 0xf7

{- Storable instances -}
instance Storable PmBool where
  sizeOf    _ = (#size int)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = 
      let intptr = castPtr ptr :: Ptr CInt in
          do
              val <- peek intptr
              return PmBool { boolcode = val }
  poke ptr (PmBool code) =
      let inptr = castPtr ptr :: Ptr CInt in
          poke inptr code

instance Storable PmMessage where
  sizeOf    _ = (#size int)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = 
      let intptr = castPtr ptr :: Ptr CInt in
          do
              val <- peek intptr
              return PmMessage { pm_msg = val }
  poke ptr (PmMessage msg) =
      let inptr = castPtr ptr :: Ptr CInt in
          poke inptr msg

instance Storable PmTimestamp where
  sizeOf    _ = (#size int)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = 
      let intptr = castPtr ptr :: Ptr CInt in
          do
              val <- peek intptr
              return PmTimestamp { pm_ts = val }
  poke ptr (PmTimestamp ts) =
      let inptr = castPtr ptr :: Ptr CInt in
          poke inptr ts

instance Storable PmDeviceInfo where
  sizeOf    _ = (#size PmDeviceInfo)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
      svers <- (#peek PmDeviceInfo, structVersion) ptr
      interf <- (#peek PmDeviceInfo, interf) ptr
      name <- (#peek PmDeviceInfo, name) ptr
      input <- (#peek PmDeviceInfo, input) ptr
      output <- (#peek PmDeviceInfo, output) ptr
      opened <- (#peek PmDeviceInfo, opened) ptr
      return PmDeviceInfo {
          pm_structVersion = svers,
          pm_interf = interf,
          pm_name = name,
          pm_input = input,
          pm_output = output,
          pm_opened = opened
      }
  poke ptr (PmDeviceInfo svers interf name input output opened) = do
      (#poke PmDeviceInfo, structVersion) ptr svers
      (#poke PmDeviceInfo, interf) ptr interf
      (#poke PmDeviceInfo, name) ptr name
      (#poke PmDeviceInfo, input) ptr input
      (#poke PmDeviceInfo, output) ptr output
      (#poke PmDeviceInfo, opened) ptr opened

instance Storable PmEvent where
  sizeOf    _ = (#size PmEvent)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
      msg <- (#peek PmEvent, message) ptr
      ts <- (#peek PmEvent, timestamp) ptr
      return PmEvent { pm_message = PmMessage { pm_msg = msg }, pm_timestamp = PmTimestamp { pm_ts = ts }}
  poke ptr (PmEvent msg ts) = do
      (#poke PmEvent, message) ptr msg
      (#poke PmEvent, timestamp) ptr ts


{- Foreign import -}
foreign import ccall unsafe "portmidi.h Pm_Initialize"
    c_Pm_Initialize :: IO PmError

foreign import ccall unsafe "portmidi.h Pm_Terminate"
    c_Pm_Terminate :: IO PmError

foreign import ccall unsafe "portmidi.h Pm_HasHostError"
    c_Pm_HasHostError :: Ptr PmStream -> IO PmBool

foreign import ccall unsafe "portmidi.h Pm_GetErrorText"
    c_Pm_GetErrorText :: PmError -> IO CString

foreign import ccall unsafe "portmidi.h Pm_GetHostErrorText"
    c_Pm_GetHostErrorText :: CString -> CUInt -> IO ()

foreign import ccall unsafe "portmidi.h Pm_CountDevices"
    c_Pm_CountDevices :: IO CInt

foreign import ccall unsafe "portmidi.h Pm_GetDefaultInputDeviceID"
    c_Pm_GetDefaultInputDeviceID :: IO PmDeviceID

foreign import ccall unsafe "portmidi.h Pm_GetDefaultOutputDeviceID"
    c_Pm_GetDefaultOutputDeviceID :: IO PmDeviceID

foreign import ccall unsafe "portmidi.h Pm_GetDeviceInfo"
    c_Pm_GetDeviceInfo :: PmDeviceID -> IO (Ptr PmDeviceInfo)

foreign import ccall unsafe "portmidi.h Pm_OpenInput"
    c_Pm_OpenInput :: Ptr (Ptr PmStream)     -- (out) stream
                   -> PmDeviceID            -- inputDevice
                   -> Ptr ()                -- inputDriverInfo
                   -> CInt                  -- bufferSize
                   -> PmTimeProcPtr         -- time_proc
                   -> Ptr ()                -- time_info
                   -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_OpenOutput"
    c_Pm_OpenOutput :: Ptr (Ptr PmStream)    -- (out) stream
                    -> PmDeviceID           -- outputDevice
                    -> Ptr ()               -- outputDriverInfo
                    -> CInt                 -- bufferSize
                    -> PmTimeProcPtr        -- time_proc
                    -> Ptr ()               -- time_info
                    -> CInt                 -- latency
                    -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_SetFilter"
    c_Pm_SetFilter :: Ptr PmStream -> PmFilter -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_SetChannelMask"
    c_Pm_SetChannelMask :: Ptr PmStream -> CInt -> IO PmError

foreign import ccall unsafe "pormidi.h Pm_Abort"
    c_Pm_Abort :: Ptr PmStream -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_Close"
    c_Pm_Close :: Ptr PmStream -> IO PmError

foreign import ccall unsafe "portmidi.h &Pm_Close"
    c_ptr_Pm_Close :: FunPtr (Ptr PmStream -> IO ())

foreign import ccall unsafe "portmidi.h Pm_Synchronize"
    c_Pm_Synchronize :: Ptr PmStream -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_Read"
    c_Pm_Read :: Ptr PmStream -> Ptr PmEvent -> CInt -> IO CInt

foreign import ccall unsafe "portmidi.h Pm_Poll"
    c_Pm_Poll :: Ptr PmStream -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_Write"
    c_Pm_Write :: Ptr PmStream -> Ptr PmEvent -> CInt -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_WriteShort"
    c_Pm_WriteShort :: Ptr PmStream -> PmTimestamp -> PmMessage -> IO PmError

foreign import ccall unsafe "portmidi.h Pm_WriteSysEx"
    c_Pm_WriteSysEx :: Ptr PmStream -> PmTimestamp -> Ptr CUChar -> IO PmError



{- Utility functions -}
pmbool_to_bool :: PmBool -> Bool
pmbool_to_bool b
    | b == pmFalse = False
    | otherwise    = True

bool_to_pmbool :: Bool -> PmBool
bool_to_pmbool True = pmTrue
bool_to_pmbool False = pmFalse

filt_combine :: [PmFilter] -> PmFilter
filt_combine = PmFilter . foldr ((.|.) . filtercode) 0

mask_combine :: [CInt] -> CInt
mask_combine = foldr (.|.) 0

wordToCInt :: Word8 -> CInt
wordToCInt = fromIntegral

cIntToWord :: CInt -> Word8
cIntToWord = fromIntegral . ((.&.) 0xFF)

compile :: [Word8] -> CInt
compile = foldr (\x -> \acc -> (shift acc 8) .|. (wordToCInt x)) 0

unfold :: CInt -> [Word8]
unfold = (map cIntToWord) . (take 4) . (iterate $ flip shift $ -8)

{-
errorText :: PmError -> IO String
errorText err = do
    errstr <- c_Pm_GetErrorText err
    str <- peekCString errstr
    return (str)
-}

errorText :: PmError -> IO String
errorText err =
    (c_Pm_GetErrorText err) >>= peekCString

{- PmDeviceInfo mid-level bindings -}
getInterf :: PmDeviceInfo -> IO String
getInterf = peekCString . pm_interf

getName :: PmDeviceInfo -> IO String
getName = peekCString . pm_name

getStructVersion :: PmDeviceInfo -> Int
getStructVersion = fromIntegral . pm_structVersion

getIsInput :: PmDeviceInfo -> Bool
getIsInput = pmbool_to_bool . pm_input

getIsOutput :: PmDeviceInfo -> Bool
getIsOutput = pmbool_to_bool . pm_output

getIsOpened :: PmDeviceInfo -> Bool
getIsOpened = pmbool_to_bool . pm_opened

{- Context -}
data PmContext = PmContext !(ForeignPtr PmStream)

hasHostError :: PmContext -> IO Bool
hasHostError (PmContext stream) = do
    withForeignPtr stream $ \pmstream_ptr -> do
        pmb <- c_Pm_HasHostError pmstream_ptr
        return (pmbool_to_bool pmb)

hostErrorText :: IO String
hostErrorText = do
    withCString emptyStr $ \msg_ptr -> do
        c_Pm_GetHostErrorText msg_ptr pmHostErrorMsgLen
        errstr <- peekCString msg_ptr
        return (errstr)
    where emptyStr = take (fromIntegral pmHostErrorMsgLen) $ repeat '\0'

getDeviceInfo :: PmDeviceID -> IO (Maybe PmDeviceInfo)
getDeviceInfo dev = do
    devinfo <- c_Pm_GetDeviceInfo dev
    if devinfo == nullPtr
        then return Nothing
        else do
            di <- peek devinfo
            return $ Just di

openInput :: PmDeviceID -> Int -> IO (Either PmError PmContext)
openInput device bufferSize = do
    alloca $ \streamptrptr -> do
        err <- c_Pm_OpenInput streamptrptr device nullPtr (fromIntegral bufferSize) nullFunPtr nullPtr
        if err == pmNoError
            then do
                streamptr <- peek streamptrptr
                stream <- newForeignPtr c_ptr_Pm_Close streamptr
                return (Right $ PmContext stream)
            else do
                return (Left err)

openOutput :: PmDeviceID -> Int -> Int -> IO (Either PmError PmContext)
openOutput device bufferSize latency = do
    alloca $ \streamptrptr -> do
        err <- c_Pm_OpenOutput streamptrptr device nullPtr (fromIntegral bufferSize) nullFunPtr nullPtr (fromIntegral latency)
        if err == pmNoError
            then do
                streamptr <- peek streamptrptr
                stream <- newForeignPtr c_ptr_Pm_Close streamptr
                return (Right $ PmContext stream)
            else do
                return (Left err)

setFilter :: PmContext -> PmFilter -> IO PmError
setFilter (PmContext stream) filt =
    withForeignPtr stream $ (flip c_Pm_SetFilter) filt

setChannelMask :: PmContext -> Word16 -> IO PmError
setChannelMask (PmContext stream) mask =
    withForeignPtr stream $ (flip c_Pm_SetChannelMask) (fromIntegral mask)

abort :: PmContext -> IO PmError
abort (PmContext stream) =
    withForeignPtr stream c_Pm_Abort

close :: PmContext -> IO PmError
close (PmContext stream) =
    withForeignPtr stream c_Pm_Close

synchronize :: PmContext -> IO PmError
synchronize (PmContext stream) =
    withForeignPtr stream c_Pm_Synchronize

readInput :: PmContext -> Int -> IO (Either PmError [PmEvent])
readInput (PmContext stream) length =
    withForeignPtr stream $ \stream_ptr -> do
        alloca $ \evtBuffer -> do
            result <- c_Pm_Read stream_ptr evtBuffer (fromIntegral length)
            if result <= 0
                then return (Left $ PmError result)
                else do
                    evts <- foldr (\n -> \acc -> (:) <$> (peekElemOff evtBuffer n) <*> acc) (return []) (range (0,fromIntegral result))
                    return (Right evts)

pollInput :: PmContext -> IO (Either PmError Bool)
pollInput (PmContext stream) =
    withForeignPtr stream $ \stream_ptr -> do
        result <- c_Pm_Poll stream_ptr
        if (errorCode result) < 0
            then return (Left result)
            else return (Right $ pmbool_to_bool $ PmBool $ errorCode result)

writeOutput :: PmContext -> [PmEvent] -> IO PmError
writeOutput (PmContext stream) events =
    withForeignPtr stream $ \stream_ptr -> do
        allocaBytes evtsize $ \evts_ptr -> 
            foldr (\(offset,event) -> ((>>) (pokeElemOff evts_ptr offset event))) (return ()) (zip [0..] events)
            >> (c_Pm_Write stream_ptr evts_ptr (fromIntegral $ length events))
    where evtsize = (length events) * (sizeOf (undefined :: PmEvent))

writeShort :: PmContext -> PmEvent -> IO PmError
writeShort (PmContext stream) evt =
    withForeignPtr stream $ \stream_ptr ->
        c_Pm_WriteShort stream_ptr (pm_timestamp evt) (pm_message evt)

writeSysEx :: PmContext -> PmTimestamp -> [Word8] -> IO PmError
writeSysEx (PmContext stream) ts msgs =
    withForeignPtr stream $ \stream_ptr ->
        allocaBytes (length msgs) $ \msg_ptr ->
            foldr (\(offset,msg) -> ((>>) (pokeElemOff msg_ptr offset $ fromIntegral msg))) (return ()) (zip [0..] msgs)
            >> (c_Pm_WriteSysEx stream_ptr ts msg_ptr)





