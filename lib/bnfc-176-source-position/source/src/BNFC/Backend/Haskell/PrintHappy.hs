{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module BNFC.Backend.Haskell.PrintHappy where

-- pretty-printer generated by the BNF converter

import BNFC.Backend.Haskell.AbsHappy
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))


instance Print Terminal where
  prt _ (Terminal i) = doc (showString ( i))



instance Print Production where
  prt i e = case e of
    P id type_ righthandsides -> prPrec i 0 (concatD [prt 0 id, doc (showString "::"), doc (showString "{"), prt 0 type_, doc (showString "}"), doc (showString ":"), prt 0 righthandsides])

instance Print RightHandSide where
  prt i e = case e of
    Rhs symbols expression -> prPrec i 0 (concatD [prt 0 symbols, doc (showString "{"), prt 0 expression, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "|"), prt 0 xs])
instance Print Symbol where
  prt i e = case e of
    NonTerm id -> prPrec i 0 (concatD [prt 0 id])
    QuotedTerm terminal -> prPrec i 0 (concatD [prt 0 terminal])
    IdentTerm id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Expression where
  prt i e = case e of
    EIdent id -> prPrec i 2 (concatD [prt 0 id])
    EPair expression1 expression2 -> prPrec i 2 (concatD [doc (showString "("), prt 0 expression1, doc (showString ","), prt 0 expression2, doc (showString ")")])
    EApp expression1 expression2 -> prPrec i 1 (concatD [prt 1 expression1, prt 2 expression2])

instance Print Type where
  prt i e = case e of
    TIdent id -> prPrec i 2 (concatD [prt 0 id])
    TPair type_1 type_2 -> prPrec i 2 (concatD [doc (showString "("), prt 0 type_1, doc (showString ","), prt 0 type_2, doc (showString ")")])
    TList type_ -> prPrec i 2 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])
    TApp type_1 type_2 -> prPrec i 1 (concatD [prt 1 type_1, prt 2 type_2])

