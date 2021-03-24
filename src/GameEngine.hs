{-# LANGUAGE ForeignFunctionInterface #-}

module GameEngine where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "makeEngine"
    c_makeEngine :: CInt -> CInt -> IO (FunPtr ())

-- foreign import ccall "acpp_capi.h applyStateA"
--     applyStateA :: FunPtr () -> FunPtr (CInt -> CInt) -> IO (FunPtr ())

-- foreign import ccall "acpp_capi.h getStateA"
--     getStateA :: FunPtr () -> IO CInt

-- foreign import ccall "acpp_capi.h setStateA"
--     setStateA :: FunPtr () -> CInt -> IO (FunPtr ())

-- foreign import ccall "acpp_capi.h freeA"
--     freeA :: FunPtr () -> IO ()