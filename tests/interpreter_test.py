from compiler.interpreter import SymTab, interpret
from compiler.parser import parse
from compiler.symtab import global_interpreter_locals
from compiler.tokenizer import tokenize
from typing import Any
import pytest

symtab: SymTab = SymTab(locals=global_interpreter_locals, parent=None)


def test_interpret_var_declaration() -> None:
    assert interpret(parse(tokenize('var x = 1')), symtab) == None
    assert interpret(parse(tokenize('var b : Int = 1')), symtab) == None


def test_interpret_binary_op() -> None:
    assert interpret(parse(tokenize('1 + 2 - 3')), symtab) == 0
    assert interpret(parse(tokenize('1 + 2 * 3')), symtab) == 7
    assert interpret(parse(tokenize('4 / 2')), symtab) == 2
    assert interpret(parse(tokenize('4 % 3')), symtab) == 1
    assert interpret(parse(tokenize('x + 3')), symtab) == 4
    assert interpret(parse(tokenize('x < 3')), symtab) == True
    assert interpret(parse(tokenize('x < 3 or x > 8')), symtab) == True
    assert interpret(parse(tokenize('x > 3 and x < 8')), symtab) == False


def test_interpret_block() -> None:
    assert interpret(parse(tokenize('{}')), symtab) == None
    assert interpret(parse(tokenize('{x = x + 2; x }')), symtab) == 3
    assert interpret(parse(tokenize('{x = x + 1; x > 3}')), symtab) == True


def test_interpret_unary_op() -> None:
    assert interpret(parse(tokenize('-x')), symtab) == -4
    assert interpret(parse(tokenize('not (x > 3)')), symtab) == False
    assert interpret(parse(tokenize('not not(x > 3)')), symtab) == True


def test_interpret_if_expression() -> None:
    assert interpret(parse(tokenize('if 1 > 2 then 3')), symtab) == None
    assert interpret(parse(tokenize('if 1 < 2 then 3 else 4')), symtab) == 3
    assert interpret(parse(tokenize('if 1 > 2 then 3 else 4')), symtab) == 4
    assert interpret(parse(tokenize('10 + if 1 > 2 then 3 else 4')),
                     symtab) == 14


def test_interpret_while_loop() -> None:
    assert interpret(
        parse(tokenize('{var x = 10; while x > 3 do x = x -1; x}')),
        symtab) == 3


def test_interpret_function_call(capsys: Any) -> None:
    interpret(parse(tokenize('x = x + 1; print_int(x)')), symtab)
    captured = capsys.readouterr()
    assert captured.out == '5\n'


"""
Here are some test cases for inputs that should fail to parse
Matching exception messages: not very care about location info
"""


def test_interpret_binary_op_exception() -> None:
    match_exception = r'.*"mk" was not found in the context and all parent contexts'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('mk = 7')), symtab)

    match_exception = r'.*left expression must be an identifier if the operator is "="'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('var a = 0; not a = 7')), symtab)


# def test_interpret_var_declaration_exception() -> None:
#     match_exception = r'.*the context already has a local identifier "x"'
#     with pytest.raises(Exception, match=match_exception):
#         interpret(parse(tokenize('var x = 7')), symtab)


def test_interpret_if_expression_exception() -> None:
    match_exception = r'.*only support the bool value of condition expression, got'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('if x then 1')), symtab)


def test_interpret_while_loop_exception() -> None:
    match_exception = r'.*only support the bool value of condition expression, got'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('while - x do x = x + 1')), symtab)


def test_interpret_function_call_exception() -> None:
    match_exception = r'.*"y" was not found in the context and all parent contexts'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('x = y + 1; print_int(x)')), symtab)

    match_exception = r'.*the type of function name must be a function, got "6"'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('x = x + 1; x(x)')), symtab)

    match_exception = r'.*the implementation is allowed to limit the number of allowed arguments to 6, got "7"'
    with pytest.raises(Exception, match=match_exception):
        interpret(parse(tokenize('print_int(1,2,3,4,5,6,7)')), symtab)
