from dataclasses import dataclass
from typing import Dict

from compiler.ir import IRVar


@dataclass(frozen=True)
class Type:
    """Base class for types."""


@dataclass(frozen=True)
class BasicType(Type):
    name: str


# @dataclass(frozen=True)
# class StrType(BasicType):
#     name: str = "Str"


@dataclass(frozen=True)
class ConsType(BasicType):
    """Super class for all other types"""


@dataclass(frozen=True)
class FuncType(Type):
    """This class is used for building function types"""
    args: list[BasicType] | None
    return_type: Type


Int = BasicType('Int')
Bool = BasicType('Bool')
Unit = BasicType('Unit')
# Str = StrType()
Cons = ConsType('Int | Bool')
Arithmetic = FuncType([Int, Int], Int)
Comparison = FuncType([Int, Int], Bool)
Equality = FuncType([Cons, Cons], Bool)
Logical = FuncType([Bool, Bool], Bool)
PrintInt = FuncType([Int], Unit)
PrintBool = FuncType([Bool], Unit)
ReadInt = FuncType(None, Int)

#global_basic_types = [Int.name, Bool.name, Unit.name]
root_types: Dict[IRVar, Type] = {
    IRVar('Unit'): Unit,
    IRVar('print_int'): PrintInt,
    IRVar('print_bool'): PrintBool,
    IRVar('read_int'): ReadInt,
    IRVar('or'): Logical,
    IRVar('and'): Logical,
    IRVar('=='): Equality,
    IRVar('!='): Equality,
    IRVar('>'): Comparison,
    IRVar('>='): Comparison,
    IRVar('<'): Comparison,
    IRVar('<='): Comparison,
    IRVar('+'): Arithmetic,
    IRVar('-'): Arithmetic,
    IRVar('*'): Arithmetic,
    IRVar('/'): Arithmetic,
    IRVar('%'): Arithmetic,
    IRVar('unary_not'): Bool,
    IRVar('unary_-'): Int,
}
