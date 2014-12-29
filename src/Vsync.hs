{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import Control.Monad (when, unless)
import Data.Time
    
withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False

renderFrame :: Window -> IO ()
renderFrame window = do
  swapBuffers window
                             
main :: IO ()
main = do
  withWindow 640 480 "vsync check" $ \win -> do
         start <- getCurrentTime
         loop win
         stop <- getCurrentTime
         print $ diffUTCTime stop start
         exitSuccess

loop :: Window -> IO ()         
loop window = do
  pollEvents
  renderFrame window
  k <- keyIsPressed window Key'Escape
  unless k $ loop window
