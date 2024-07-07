{
{-# LANGUAGE NoStrictData #-}
module Sirius.Parser where

import Prelude
import Sirius.Lexer (Token(..))
import Sirius.Syntax

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
}

%name parse main
%tokentype { Token }
%error { parseErrorFrom }

%monad { ParseM }

%token
    ident           { Ident $$ }
    quoted          { Quoted $$ }
    int             { Integer $$ }
    '{'             { LBrace }
    '}'             { RBrace }
    ';'             { Semi }
    eof             { EOF }

%%

manyList(p)
    :               { [] }
    | p manyList(p) { $1 : $2 }

many(p) : manyList(p) { Seq.fromList $1 }

many1(p) : p manyList(p) { Seq.fromList ($1 : $2) }

sepTrailingList(sep, p)
    :                               { [] }
    | p                             { [$1] }
    | p sep sepTrailingList(sep, p) { $1 : $3 }

sepTrailing(sep, p) : sepTrailingList(sep, p) { Seq.fromList $1 }

main :: { Program }
main : many(declaration) { MkProgram $1 }

declaration :: { Declaration }
declaration : { undefined }


declaration :: { Declaration }
declaration
    : ident '{' sepTrailing(';', command) '}' { DefineFunction $1 $3 }

command :: { Command }
command
    : ident many(argument) { GenericCommand $1 $2 }

argument :: { Argument }
argument
    : quoted { Literal $1 }
    | ident { Named $1 }
    | int { Int $1 }
    | '{' sepTrailing(';', command) '}' { Lambda $2 }

{

data ParseError
    = ParseError

newtype ParseM a = MkParseM (Either ParseError a)
    deriving newtype (Functor, Applicative, Monad)

runParseM :: ParseM a -> Either ParseError a
runParseM (MkParseM either) = either

parseError :: ParseError -> ParseM a
parseError error = MkParseM (Left error)

parseErrorFrom :: [Token] -> ParseM a
parseErrorFrom [] = undefined
parseErrorFrom (token : _) = parseError ParseError
}