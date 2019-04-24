module Main where

import qualified Lib
import qualified Either
import Text.Printf (printf)

text :: String -> Text 
text = toText

main :: IO ()
main = do
  n <- (readEither <$> getLine)
  nCases <- n # Either.unless (text "each run must begin with a line with a single integer for the number of cases")
  cases <- sequence (const loadCase <$> [1 .. nCases])
  let angles = cases <#> Lib.degToRad
  let answers = angles <#> \d -> (Lib.sin d, Lib.cos d)
  writeSolution answers 

  where 
    loadCase :: IO Float
    loadCase = do
      input <- readEither <$> getLine
      input # Either.unless (text "each case must be a single floating-point number") 
    
    writeSolution :: [(Float, Float)] -> IO ()
    writeSolution answers = do
      putTextLn $ text ""
      forM_ answers $ \(sinAns, cosAns) -> 
        putTextLn (text $ printf "%0.3f %0.3f" sinAns cosAns)
    
    


