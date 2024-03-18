from typing import Any, Dict
from compiler.tokenizer import Location
from dataclasses import dataclass, field

from compiler.types import BasicType, FuncType, Type, Unit


@dataclass
class Expression:
    """Base class for AST nodes representing expressions."""
    location: Location
    type: Type = field(kw_only=True, default_factory=lambda: Unit)


@dataclass
class TypeExpr(Expression):
    """Base class for AST nodes representing type expressions."""
    # def __init__(self, location: Location):
    #     super.__init__(location)


@dataclass
class BasicTypeExpr(TypeExpr):
    name: str

    def __eq__(self, __value: object) -> bool:
        if isinstance(__value, self.__class__):
            return self.name == __value.name
        elif isinstance(__value, BasicType):
            return self.name == __value.name
        else:
            return super().__eq__(__value)


@dataclass
class FuncTypeExpr(TypeExpr):
    params: list[BasicTypeExpr] | None
    result: BasicTypeExpr

    def __eq__(self, __value: object) -> bool:
        if isinstance(__value, self.__class__):
            return self.params == __value.params and self.result == __value.result
        elif isinstance(__value, FuncType):
            return self.params == __value.args and self.result == __value.return_type
        else:
            return super().__eq__(__value)


@dataclass
class Literal(Expression):
    # value=None is used when parsing the keyword `unit`
    value: int | bool | None

    def __setattr__(self, __name: str, __value: Any) -> None:
        if __name == 'value' and isinstance(__value, int):
            if not (-2**64 <= __value <= 2**63 - 1):
                raise Exception(
                    f'{self.location}: Integer literal value must be between -2**64 and 2**63 - 1'
                )

        return super().__setattr__(__name, __value)

    # def __init__(self, location: Location, value: int | bool | None):
    #     """Integer literal: a whole number between -2^64 and 2^63 - 1"""
    #     if isinstance(value, int):
    #         if not (-2**64 <= value <= 2**63 - 1):
    #             raise ValueError(
    #                 "Integer literal value must be between -2^64 and 2^63 - 1")
    #     self.location = location
    #     self.type = type
    #     self.value = value


@dataclass
class Identifier(Expression):
    name: str

    def __hash__(self) -> int:
        return hash(self.name)


@dataclass
class UnaryOp(Expression):
    """AST node for a binary operation like `-A' or  'not B` or 'not not B'"""
    op: str
    unary_clause: Expression


@dataclass
class BinaryOp(Expression):
    """AST node for a binary operation like `A + B`"""
    left: Expression
    op: str
    right: Expression


@dataclass
class IfExpression(Expression):
    op: str
    cond: Expression
    then_clause: Expression
    else_clause: Expression | None


@dataclass
class FunctionCall(Expression):
    op: str
    name: Identifier
    args: list[Expression] | None


@dataclass
class Block(Expression):
    op: str
    statements: list[Expression] | None


@dataclass
class WhileLoop(Expression):
    op: str
    cond: Expression
    body: Expression


@dataclass
class VarDeclaration(Expression):
    op: str
    name: Identifier
    declared_type_exp: TypeExpr | None
    initializer: Expression


@dataclass
class Return(Expression):
    op: str
    value: Expression | None


@dataclass
class FuncDefinition():
    """Base class for AST nodes representing function definitions."""
    location: Location

    name: Identifier
    args: Dict[Identifier, BasicTypeExpr] | None
    return_type: BasicTypeExpr
    body: Block


@dataclass
class Module():
    """Base class for AST nodes representing modules."""
    funcs: list[FuncDefinition] | None
    expr: Expression | None


@dataclass
class ControlFlow(Expression):
    """
    name = 'break': exit the innermost active loop
    name = 'continue': go back to the beginning of active loop
    """
    name: str
