module Sirius.Syntax (
    Name(..),
    Program (..),
    Declaration (..),
    Command (..),
    GenericArgument (..),
    Entity (..),
) where

import Relude

data Name
    = NamespacedName
        { namespace :: Text
        , name :: Text
        }
    | RawName Text

data Program = MkProgram
    { namespace :: Text
    , declarations :: Seq Declaration
    }

data Declaration
    = DefineFunction Name (Seq Command)

data Command
    = GenericCommand Name (Seq GenericArgument)
    | Function Name
    | TagAdd Entity Name
    | TagRemove Entity Name

data Entity

data GenericArgument
    = Literal Text
    | Named Name
    | Lambda (Seq Command)
    | Int Integer
