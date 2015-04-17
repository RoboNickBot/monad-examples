-- | Module    : Education.Monad.Examples.Crushes
-- Copyright   : 2015 RoboNickBot
-- License     : GPL-3
-- Maintainer  : nicklewchenko92@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Using the Maybe Monad: Periodic High-school of the Elements

module Crushes where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M


-- | A 'CrushDB' is a map of all /known/ crushes in the school, where
-- the keys are the names of elements that are crushing, and the
-- values are the names of elements that are being crushed upon.
type CrushDB = M.Map String String

sampleCrushDB :: CrushDB 
sampleCrushDB = M.fromList [ ("Carbon"   ,  "Aluminum")
                           , ("Iron"     ,  "Helium"  )
                           , ("Helium"   ,  "Iron"    )
                           , ("Argon"    ,  "Radium"  )
                           , ("Radium"   ,  "Carbon"  )
                           , ("Hydrogen" ,  "Carbon"  )
                           , ("Lithium"  ,  "Neon"    ) ]

crush :: CrushDB -> String -> Maybe String
crush db a = M.lookup a db

-- implemented without (>>=)
opponent' :: CrushDB -> String -> Maybe String
opponent' db a = case crush db a of
                   Just b -> case crush db b of
                               Just c -> Just c
                               _ -> Nothing
                   _ -> Nothing

-- implemented with (>>=)
opponent :: CrushDB -> String -> Maybe String
opponent db a = crush db a >>= crush db

isRequited :: CrushDB -> String -> Maybe Bool
isRequited db a = fmap (== a) (opponent db a)

