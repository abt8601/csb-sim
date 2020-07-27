module CSB.Internal.JSON where

import           Data.Aeson
import           Data.Char

jsonOpts :: Options
jsonOpts = defaultOptions { fieldLabelModifier     = removeLeadingUnderscore
                          , constructorTagModifier = toCamelCase
                          }
 where
  removeLeadingUnderscore ('_' : xs) = xs
  removeLeadingUnderscore xs         = xs

  toCamelCase ""       = ""
  toCamelCase (x : xs) = toLower x : xs
