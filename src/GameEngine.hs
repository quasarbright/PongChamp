{-# LANGUAGE ForeignFunctionInterface #-}

module GameEngine where

import Foreign.C.Types
import Foreign.Ptr

foreign import ccall "makeEngine"
    c_makeEngine :: CInt -> CInt -> IO (FunPtr ())

foreign import ccall "clear"
    c_clear :: FunPtr () -> IO ()

foreign import ccall "flip"
    c_flip :: FunPtr () -> IO ()

foreign import ccall "delay"
    c_delay :: FunPtr () -> CInt -> IO ()

foreign import ccall "DrawRectangle"
    c_drawRectangle :: FunPtr () -> CInt -> CInt -> CInt -> CInt -> IO ()