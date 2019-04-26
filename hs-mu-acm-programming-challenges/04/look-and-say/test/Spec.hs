
import Lib (pairwise, splitPairwise, lookAndSay)

main :: IO ()
main = do
  putTextLn $ show $ pairwise ([]::[Int])
  putTextLn $ show $ pairwise [1]
  putTextLn $ show $ pairwise [1,2,3]

  putTextLn $ show $ fmap toList $ splitPairwise (==) ([]::[Int])
  putTextLn $ show $ fmap toList $ splitPairwise (==) [1]
  putTextLn $ show $ fmap toList $ splitPairwise (==) [1,1,2,2,2,3,4,5,6,6]

  putTextLn $ toText $ lookAndSay "21"
  putTextLn $ toText $ lookAndSay "11133122"
  