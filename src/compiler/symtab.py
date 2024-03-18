from dataclasses import dataclass
import operator
from typing import Callable, Dict, Generic, Optional, TypeVar

from compiler.ir import IRVar
from compiler.types import Arithmetic, Bool, Comparison, Equality, Int, Logical, PrintBool, PrintInt, ReadInt, Type, Unit

T = TypeVar('T')


@dataclass
class SymTab(Generic[T]):
    """A dictionary of variable names to values: contains variables defined in the current scope"""
    locals: dict[str, T]
    """An optional reference to the outer scope's symbol table"""
    parent: Optional['SymTab[T]']

    def __init__(self, locals: dict[str, T], parent: Optional['SymTab[T]']):
        self.locals = locals
        self.parent = parent

    def require(self, key: str) -> T:
        if key in self.locals:
            return self.locals[key]
        elif self.parent is not None:
            return self.parent.require(key)
        else:
            #return None
            raise ValueError(
                f'"{key}" was not found in the context and all parent contexts'
            )

    def add_local(self, key: str, value: T) -> None:
        self.locals[key] = value


def look_up_context(symtab: SymTab, key: str) -> SymTab | None:
    if key in symtab.locals:
        return symtab
    elif symtab.parent is not None:
        return look_up_context(symtab.parent, key)
    else:
        return None


"""The built-in functions"""


# Prints an integer and a newline to standard output.
def print_int(value: int) -> None:
    print(value)


# Prints either true or false and a newline to standard output.
def print_bool(value: bool) -> None:
    if value:
        print(f'true')
    else:
        print(f'false')


# Reads a single line, including the newline, from standard input, and interprets it as an integer.
# If the input before the newline contains characters other than digits and a prefix minus,
# then read_int is allowed to fail or have arbitrary behavior.
def read_int() -> int:
    value = input()
    return int(value)


global_interpreter_locals: dict[str, Callable] = {
    'print_int': print_int,
    'print_bool': print_bool,
    'read_int': read_int,
    'or': operator.or_,
    'and': operator.and_,
    '==': operator.eq,
    '!=': operator.ne,
    '>': operator.gt,
    '>=': operator.ge,
    '<': operator.lt,
    '<=': operator.le,
    '+': operator.add,
    '-': operator.sub,
    '*': operator.mul,
    '/': operator.floordiv,
    '%': operator.mod,
    'unary_not': operator.not_,
    'unary_-': operator.neg,
}

top_level_type_locals: dict[str, Type] = {
    'Int': Int,
    'Bool': Bool,
    'Unit': Unit,
    'print_int': PrintInt,
    'print_bool': PrintBool,
    'read_int': ReadInt,
    'or': Logical,
    'and': Logical,
    '==': Equality,
    '!=': Equality,
    '>': Comparison,
    '>=': Comparison,
    '<': Comparison,
    '<=': Comparison,
    '+': Arithmetic,
    '-': Arithmetic,
    '*': Arithmetic,
    '/': Arithmetic,
    '%': Arithmetic,
    'unary_not': Bool,
    'unary_-': Int,
}
