module Lib (degToRad, Lib.sin, Lib.cos) where

factorial :: Float -> Float
factorial 0.0 = 1.0
factorial n = n * factorial (n - 1.0)

degToRad :: Float -> Float
degToRad x = x * pi / 180.0

sin :: Float -> Float
sin x = 
  -- x - x^3/3! + x^5/5! + ...
  -- sigma [1..] ( (-1)^n-1 * (x^(2n - 1) / (2n - 1)!) )

  let 
    iteration n = (-1)**(n-1) * (x**(2*n - 1) / factorial (2*n - 1))
    series = [1..] <#> iteration # take 20 
  in sum series

cos :: Float -> Float
cos x = 
  -- 1 - x^2/2! + x^4/4! + ...
  -- sigma [0..] ( (-1)^n * (x^2n / 2n!) )

  let 
    iteration n = (-1)**n * (x**(2*n) / factorial (2*n))
    series = [0..] <#> iteration # take 21 
  in sum series
