{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RankNTypes, GADTs, ScopedTypeVariables #-}
module Web.Ivy.Routes where

import Prelude hiding (print)
import Text.Syntax
import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Data.Char (isLetter, isDigit, isAlphaNum)
import Text.Syntax.Printer.Naive
import Text.Syntax.Parser.Naive
import Web.Ivy.Types
import Data.Typeable

{-
class HasSyntax a where
    syntax :: Syntax s => a -> s a
-}

data Route = forall a. (Typeable a, Handler a) => Route a

routeIso :: forall a. (Typeable a, Handler a) => Iso a Route
routeIso = Iso f g where
    f = Just . Route
    g r = case r of Route r' -> cast r'

{-
instance Resource Route where
    get r = case r of Route r' -> get r'
    post r= case r of Route r' -> post r'
    put r = case r of Route r' -> put r'
    delete r = case r of Route r' -> delete r'

unwrap :: (Resource a => a -> Application) -> Route -> Application
unwrap f r = case r of Route r' -> f r'

print' :: HasSyntax a => a -> Maybe String
print' a = print (syntax a) a
-}

parseUrl = parse

url :: (Typeable a, Handler a) => Printer Route -> a -> Maybe String
url r h = print r (Route h)

dash = text "-"
a </> b = a <*> text "/" *> b
infixr 7 <->
a <-> b = a <*> text "-" *> b

int :: Syntax delta => delta Int
int = Iso read' show' <$> many digit where
  read' s  =  case [ x | (x, "") <- reads s ] of
                [] -> Nothing
                (x : _) -> Just x
              
  show' x  =  Just (show x)

digit   =  subset isDigit <$> token

char = subset isAlphaNum <$> token
string = many char
