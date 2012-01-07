{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Widgets.Extras.Text
    ( highlight
    )
where

import Text.Regex.Base (RegexLike, matchAll)

import Graphics.Vty.Widgets.Text (Formatter(..))
import Graphics.Vty (Attr, def_attr)
import Text.Trans.Tokenize (TextStreamEntity(..), Token(..), TextStream(..))

-- |A highlight formatter takes a regular expression used to scan the
-- text and an attribute to assign to matches.  The regular expression
-- is only applied to individual string tokens (individual words,
-- whitespace strings, etc.); it is NOT applied to whole lines,
-- paragraphs, or text spanning multiple lines.  If you have need of
-- that kind of functionality, apply your own attributes with your own
-- regular expression prior to calling 'setTextWithAttrs'.
highlight :: (RegexLike r String) => r -> Attr -> Formatter
highlight regex attr = Formatter $
    \_ (TS ts) -> return $ TS $ map highlightToken ts
        where
          highlightToken :: TextStreamEntity Attr -> TextStreamEntity Attr
          highlightToken NL = NL
          highlightToken (T t) =
              if tokenAttr t /= def_attr
              then T t
              else T (highlightToken' t)

          highlightToken' :: Token Attr -> Token Attr
          highlightToken' t =
              if null $ matchAll regex $ tokenStr t
              then t
              else t { tokenAttr = attr }
