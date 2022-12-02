module API where

import Data.IORef
import qualified Data.JSString as JSStr
import qualified JavaScript.Object as Obj
import GHCJS.Foreign.Callback
import GHCJS.Types
import GHCJS.Marshal

foreign import javascript unsafe
  "global[\"mkCode\"] = $1"
  _setupMkCode
  :: Callback (IO JSVal) -> IO ()

setupMkCode :: (JSString -> IO ()) -> IO ()
setupMkCode f = do
  toggle <- newIORef False
  str <- newIORef ""
  produce <-
    asyncCallback $ do
      putStrLn "in produce"
      (readIORef str >>= f . JSStr.pack)
      whenM
        (readIORef toggle)
        (readIORef str >>= f . JSStr.pack)

  consume <-
    asyncCallback1
      (\jsVal -> do
        putStrLn "in consume"
        s <- JSStr.unpack <$> fromJSValUnchecked jsVal
        modifyIORef str (<> s)
        readIORef str >>= print
        modifyIORef toggle not)

  mkCode <-
    syncCallback' $ do
      obj <- Obj.create
      Obj.unsafeSetProp (JSStr.pack "produce") (jsval produce) obj
      Obj.unsafeSetProp (JSStr.pack "consume") (jsval consume) obj
      pure (jsval obj)

  _setupMkCode mkCode


whenM :: Monad m => m Bool -> m () -> m ()
whenM pred action = do
  b <- pred
  if b then action else pure ()
