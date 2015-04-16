module Crushes where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.List as L

type CrushDB = M.Map String String

sampleCrushDB :: CrushDB 
sampleCrushDB = M.fromList [ ("carbon"   ,  "aluminum")
                           , ("iron"     ,  "helium"  )
                           , ("helium"   ,  "iron"    )
                           , ("argon"    ,  "radium"  )
                           , ("radium"   ,  "carbon"  )
                           , ("hydrogen" ,  "carbon"  )
                           , ("lithium"  ,  "neon"    ) ]

crush :: CrushDB -> String -> Maybe String
crush db a = M.lookup a db

-- implemented with (>>=)
opponent :: CrushDB -> String -> Maybe String
opponent db a = crush db a >>= crush db

-- and implemented without (>>=)
opponent' :: CrushDB -> String -> Maybe String
opponent' db a = case crush db a of
                   Just b -> case crush db b of
                               Just c -> Just c
                               _ -> Nothing
                   _ -> Nothing

isRequited :: CrushDB -> String -> Maybe Bool
isRequited db a = fmap (== a) (opponent db a)

