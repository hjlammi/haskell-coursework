import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args in do
    contents <- readFile fileName
    let linesOfFiles = lines contents in do
      mapM_ putStrLn linesOfFiles
