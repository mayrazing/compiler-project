from compiler.parser import parse
from compiler.symtab import SymTab, top_level_type_locals
from compiler.tokenizer import tokenize
import pytest

from compiler.type_checker import typecheck
from compiler.types import Bool, Int, Unit

symtab = SymTab(locals=top_level_type_locals, parent=None)


def test_typecheck_var_declaration() -> None:
    assert typecheck(parse(tokenize('var x = 1')), symtab) == Unit
    assert typecheck(parse(tokenize('var b : Bool = true')), symtab) == Unit


def test_typecheck_type_expr() -> None:
    assert typecheck(parse(tokenize('var z: Int = 1')), symtab) == Unit
    assert typecheck(parse(tokenize('var s: (Int) => Unit = print_int')),
                     symtab) == Unit
    assert typecheck(parse(tokenize('var t: (Bool) => Unit = print_bool')),
                     symtab) == Unit


def test_typecheck_binary_op() -> None:
    assert typecheck(parse(tokenize('1 + 2 - 3')), symtab) == Int
    assert typecheck(parse(tokenize('1 + 2 * 3')), symtab) == Int
    assert typecheck(parse(tokenize('4 / 2')), symtab) == Int
    assert typecheck(parse(tokenize('4 / 2')), symtab) == Int
    assert typecheck(parse(tokenize('x + 3')), symtab) == Int
    assert typecheck(parse(tokenize('x < 3')), symtab) == Bool
    assert typecheck(parse(tokenize('x < 3 or x > 8')), symtab) == Bool
    assert typecheck(parse(tokenize('x > 3 and x < 8')), symtab) == Bool


def test_typecheck_block() -> None:
    assert typecheck(parse(tokenize('{}')), symtab) == Unit
    assert typecheck(parse(tokenize('{x = x + 2; x }')), symtab) == Int
    assert typecheck(parse(tokenize('{x = x + 1; x > 3}')), symtab) == Bool
    assert typecheck(parse(tokenize('{x = x + 1; x > 3; }')), symtab) == Unit


def test_typecheck_unary_op() -> None:
    assert typecheck(parse(tokenize('-x')), symtab) == Int
    assert typecheck(parse(tokenize('not (x > 3)')), symtab) == Bool
    assert typecheck(parse(tokenize('b = not not(x > 3)')), symtab) == Bool


def test_typecheck_if_expression() -> None:
    assert typecheck(parse(tokenize('if 1 > 2 then 3')), symtab) == Unit
    assert typecheck(parse(tokenize('if 1 < 2 then 3 else 4')), symtab) == Int
    assert typecheck(parse(tokenize('if 1 > 2 then true else false')),
                     symtab) == Bool
    assert typecheck(parse(tokenize('x = 10 + if 1 > 2 then 3 else 4')),
                     symtab) == Int


def test_typecheck_while_loop() -> None:
    assert typecheck(parse(tokenize('var y = 10; while y > 3 do y = y -1')),
                     symtab) == Unit


def test_typecheck_function_call() -> None:
    assert typecheck(parse(tokenize('print_int(x)')), symtab) == Unit


def test_typecheck_func_def() -> None:
    assert typecheck(
        parse(tokenize('fun cal_square(x: Int): Int {\nreturn x * x;\n}')),
        symtab) == Unit


"""
Here are some test cases for inputs that should fail to parse
Matching exception messages: not very care about location info
"""


def test_typecheck_binary_op_exception() -> None:
    match_exception = r'.*"mk" was not found in the context and all parent contexts'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('mk = 7')), symtab)

    match_exception = r'.*expected "(.*?)" type, got "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('var a = 0; not a = 7')), symtab)


def test_typecheck_var_declaration_exception() -> None:
    match_exception = r'.*variable "x" expected type "(.*?)", got type "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('var x: Int = true')), symtab)


def test_typecheck_if_expression_exception() -> None:
    match_exception = r'.*if condition expression expected type "(.*?)", got type "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('if x then 1')), symtab)


def test_typecheck_while_loop_exception() -> None:
    match_exception = r'.*while-loop expression expected type "(.*?)", got type "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('while - x do x = x + 1')), symtab)


def test_typecheck_function_call_exception() -> None:
    match_exception = r'.*"ss" was not found in the context and all parent contexts'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('x = ss + 1; print_int(x)')), symtab)

    match_exception = r'.*type of function "x" expected "(.*?)", got "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('x = x + 1; x(x)')), symtab)

    match_exception = r'.*function expression expected the type of 1-th parameter is "(.*?)", got "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('print_int(true)')), symtab)

    match_exception = r'.*number of parameters of the function "print_int" cannot exceed 6, got "7"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(parse(tokenize('print_int(1,2,3,4,5,6,7)')), symtab)


def test_typecheck_func_def_exception() -> None:
    match_exception = r'.*function definition "cal_square" was defined in the context and all parent contexts'
    with pytest.raises(Exception, match=match_exception):
        typecheck(
            parse(tokenize('fun cal_square(x: Int): Int {\nreturn x * x;\n}')),
            symtab)

    match_exception = r'.*function definition "cal_square2" expected a return type "(.*?)", got "(.*?)"'
    with pytest.raises(Exception, match=match_exception):
        typecheck(
            parse(
                tokenize('fun cal_square2(x: Int): Bool {\nreturn x * x;\n}')),
            symtab)
