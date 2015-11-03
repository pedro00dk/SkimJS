module Value (Value (..)) where

-- Importing Syntax to Value Module
import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | Break
    | Continue
    | Throw Value
    | Function Id [Id] [Statement]
    | Gvar
    | Nil

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show Break = "break"
  show Continue = "continue"
  show (Throw value) = "throw " ++ show value
  show (Function (Id name) args stmts) = "function " ++ name ++ "(" ++ showArgs args ++")"
  show Nil = "undefined"

showArgs [] = ""
showArgs ((Id arg):xs) = show arg ++ showCommaArgs xs

showCommaArgs [] = ""
showCommaArgs ((Id arg):xs) = ", " ++ show arg ++ showCommaArgs xs

  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)