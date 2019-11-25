module Main

import Data.Vect
import Data.String


xs : IO (Maybe (k ** Vect k Nat))
xs = do
  Just n <- parsePositive <$> getLine | Nothing => pure Nothing 
  pure $ Just $ (_ ** finToNat <$> range {len=toNat n})

main : IO ()
main = do  
  Just (_ ** ys) <- xs | Nothing => putStrLn "Not a valid number"
  putStrLn ("Hello world: " ++ show ys)



