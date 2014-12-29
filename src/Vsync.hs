{-# LANGUAGE PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import System.Exit (exitSuccess)
import Control.Monad (when)
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

fpsMessage :: Int -> NominalDiffTime -> String
fpsMessage frames elapsed = show frames ++ " frames in " ++ show elapsed ++ " seconds: " ++ show ( fromIntegral frames / elapsed ) ++ " fps."

main :: IO ()
main = do
  withWindow 640 480 "vsync check" $ \win -> do
         start <- getCurrentTime
         frames <- loop win 0
         stop <- getCurrentTime
         putStrLn $ fpsMessage frames (diffUTCTime stop start)
         exitSuccess

loop :: Window -> Int -> IO Int
loop window counter = do
  pollEvents
  renderFrame window
  k <- keyIsPressed window Key'Escape
  if k
     then return counter
     else loop window (counter + 1)
