import System.IO 
import Control.Monad

main :: IO ()
main = do
    --hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    c <- getChar 
    when (c /= ' ') $ do
        putChar c
        hFlush stdout
        main
