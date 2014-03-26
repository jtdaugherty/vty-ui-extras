{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Widgets.Extras.Text
    ( highlight
    )
where

import Control.Applicative
import Graphics.Vty.Widgets.Text (Formatter(..))
import Graphics.Vty (Attr, def_attr)
import Text.Trans.Tokenize (TextStreamEntity(..), Token(..), TextStream(..))
import qualified Data.Text as T
import qualified Data.Text.ICU.Regex as R

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  The regular expression
-- is only applied to individual string tokens (individual words,
-- whitespace strings, etc.); it is NOT applied to whole lines,
-- paragraphs, or text spanning multiple lines.  If you have need of
-- that kind of functionality, apply your own attributes with your own
-- regular expression prior to calling 'setTextWithAttrs'.
highlight :: R.Regex -> Attr -> Formatter
highlight regex attr = Formatter $
    \_ (TS ts) -> TS <$> mapM highlightToken ts
        where
          highlightToken :: TextStreamEntity Attr -> IO (TextStreamEntity Attr)
          highlightToken NL = return NL
          highlightToken (T t) =
              if tokenAttr t /= def_attr
              then return $ T t
              else T <$> (highlightToken' t)

          highlightToken' :: Token Attr -> IO (Token Attr)
          highlightToken' t = do
              R.setText regex $ tokenStr t
              found <- R.find regex 0
              case found of
                  False -> return t
                  True -> do
                      theStart <- R.start regex 0
                      theEnd <- R.end regex 0
                      if theStart == Just 0 && theEnd == Just (toEnum $ T.length (tokenStr t)) then
                          return $ t { tokenAttr = attr } else
                          return t
