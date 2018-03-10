module Exception (
    ) where

import Control.Exception
import Midi (StreamError)

data MIDIException =
          NoDataException
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

throwIOMIDIException :: StreamError -> IO ()
throwIOMIDIException se
    | se == NoData = throw NoDataException 
    | se == GotDate = throw GotDataException
    | se == HostError = throw HostErrorException
    | se == InvalidDeviceID = throw InvalidDeviceIDException
    | se == InsufficientMemory = throw InsufficientMemoryException
    | se == BufferTooSmall = throw BufferTooSmallException
    | se == BufferOverflow = throw BufferOverflowException
    | se == BadPtr = throw BadPtrException
    | se == BadData = throw BadDataException
    | se == InternalError = throw InternalErrorException
    | se == BufferMaxSize = throw BufferMaxSizeException
    | otherwise = return

throwEitherMIDIException :: Either StreamError a -> IO a
throwEitherMIDIException ei =
    case ei of
      Left err -> throwIOMidiException err
      Right val -> return val




