module Value (Value (..), Attribute (..)) where
-- ---------------------- Added Attribute in export list

-- Importing Syntax to Value Module
import Language.ECMAScript3.Syntax

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
---------------------------------------------------------------------------------------------------
    | Object [Attribute]
    | Break
    | Continue
    | Throw Value
    | Function Id [Id] [Statement]
    | Return Value
    | NReturn
    | Gvar
---------------------------------------------------------------------------------------------------
    | Nil

---------------------------------------------------------------------------------------------------
data Attribute = IDType String Value
    | STRType String Value
    | INTType Integer Value
---------------------------------------------------------------------------------------------------

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
---------------------------------------------------------------------------------------------------
  show (Object atts) = "object" ++ "(" ++ showAttributes atts ++ ")"
  show Break = "break"
  show Continue = "continue"
  show (Throw value) = "throw " ++ show value
  show (Function (Id name) args stmts) = "function " ++ name ++ "(" ++ showArgs args ++")"
---------------------------------------------------------------------------------------------------
  show Nil = "undefined"

---------------------------------------------------------------------------------------------------
-- Show function arrgs
showArgs [] = ""
showArgs ((Id arg):xs) = show arg ++ showCommaArgs xs
showCommaArgs [] = ""
showCommaArgs ((Id arg):xs) = ", " ++ show arg ++ showCommaArgs xs

-- Show object attributes
showAttributes [] = ""
showAttributes (att:xs) = case att of
    IDType id val -> show id ++ ": " ++ show val ++ showCommaAttributes xs
    STRType str val -> show str ++ ": " ++ show val ++ showCommaAttributes xs
    INTType int val -> show int ++ ": " ++ show val ++ showCommaAttributes xs

showCommaAttributes [] = ""
showCommaAttributes (att:xs) = case att of
    IDType id val -> ", " ++ show id ++ ": " ++ show val ++ showCommaAttributes xs
    STRType str val -> ", " ++ show str ++ ": " ++ show val ++ showCommaAttributes xs
    INTType int val -> ", " ++ show int ++ ": " ++ show val ++ showCommaAttributes xs

---------------------------------------------------------------------------------------------------

  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ ", " ++ (showListContents as)