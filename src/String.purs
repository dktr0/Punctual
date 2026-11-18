module String where

import Prelude ((<>))

namedFunction :: String -> String -> String
namedFunction fName x = fName <> "(" <> x <> ")"
