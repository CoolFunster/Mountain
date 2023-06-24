I'd like you to help me write the parser for this language.
Here is the data format of the language defined in Haskell

```haskell
type Id = String
data Exp =
    EVar Id
  | EFun Pattern Exp -- fn x -> x # is a function which takes in an x and returns it
  | EApp Exp Exp -- f(x) # is an application of the function f on argument x
  | ELet Pattern Exp Exp -- var x = 5; x + 1 # is a binding of 5 to x which is then used in the second expr x + 1
  | EAnnot Type Exp -- Int :: x # is an annotation that the variable x has type Integer. 
  deriving (Eq, Show)

data Lit
  =
    LUnit -- () # The unit is the empty tuple
  | LInt Integer -- 
  | LBool Bool
  | LThing String
  | LChar Char
  | LString String
  | LFloat Float
  deriving (Eq, Ord, Show)

data Pattern =
    PLit Lit -- 
  | PVar Id
  | PWildcard
  | PAnnot Type Pattern -- Int :: x  this specifies that the binding term needs to be of a type type 

data Type
  =
  -- Builtins
    TInt -- Int
  | TBool -- Bool
  | TChar -- Char
  | TString -- String
  | TFloat -- Float
  | TUnit -- ()
  | TFun Type Type -- fn <Type> -> <Type>
```

I'd like the types of the language to be able to parse Bools, Ints, Floats, Strings and Chars as before