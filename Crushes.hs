module Crushes where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L



crushes :: M.Map String String 
crushes = M.fromList [ ("carbon"   ,  "aluminum")
                     , ("iron"     ,  "helium"  )
                     , ("helium"   ,  "iron"    )
                     , ("argon"    ,  "radium"  )
                     , ("radium"   ,  "carbon"  )
                     , ("hydrogen" ,  "carbon"  )
                     , ("lithium"  ,  "neon"    ) ]

crush :: String -> Maybe String
crush a = M.lookup a crushes

requited :: String -> Maybe Bool
requited a = crush a >>= crush >>= return . (== a)

isSame :: String -> String -> Bool
isSame a b = a == b
