import System.IO ( isEOF )

main :: IO ()
main = mainLoop 0

mainLoop :: Integer -> IO ()
mainLoop n = do done <- isEOF
                if done
                   then putStrLn $ show n
                   else do inp <- getLine
                           mainLoop (n + (read inp :: Integer))

