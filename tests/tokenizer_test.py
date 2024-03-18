from compiler.tokenizer import Location, Token, tokenize
import pytest

L = Location(line=-1, column=-1)


def test_tokenizer() -> None:
    assert tokenize("if  3\nwhile") == [
        Token(type='IDENTIFIER', text='if', location=L),
        Token(type='INTEGER', text='3', location=L),
        Token(type='IDENTIFIER', text='while', location=L),
    ]

    assert tokenize("if  -3\nwhile") == [
        Token(type='IDENTIFIER', text='if', location=L),
        Token(type='OPERATOR', text='-', location=L),
        Token(type='INTEGER', text='3', location=L),
        Token(type='IDENTIFIER', text='while', location=L),
    ]

    assert tokenize("print_int(123);\n # for test\n _endpats = {X_9HG}") == [
        Token(type='IDENTIFIER', text='print_int', location=L),
        Token(type='PUNCTUATION', text='(', location=L),
        Token(type='INTEGER', text='123', location=L),
        Token(type='PUNCTUATION', text=')', location=L),
        Token(type='PUNCTUATION', text=';', location=L),
        Token(type='IDENTIFIER', text='_endpats', location=L),
        Token(type='OPERATOR', text='=', location=L),
        Token(type='PUNCTUATION', text='{', location=L),
        Token(type='IDENTIFIER', text='X_9HG', location=L),
        Token(type='PUNCTUATION', text='}', location=L),
    ]

    assert tokenize("if a != 7\n // for test \n c = -3") == [
        Token(type='IDENTIFIER', text='if', location=L),
        Token(type='IDENTIFIER', text='a', location=L),
        Token(type='OPERATOR', text='!=', location=L),
        Token(type='INTEGER', text='7', location=L),
        Token(type='IDENTIFIER', text='c', location=L),
        Token(type='OPERATOR', text='=', location=L),
        Token(type='OPERATOR', text='-', location=L),
        Token(type='INTEGER', text='3', location=L),
    ]

    assert tokenize(
        "if a != 7\n /* for \ntest  */\n  c = -3/* for\tanother\ntest \n */"
    ) == [
        Token(type='IDENTIFIER', text='if', location=L),
        Token(type='IDENTIFIER', text='a', location=L),
        Token(type='OPERATOR', text='!=', location=L),
        Token(type='INTEGER', text='7', location=L),
        Token(type='IDENTIFIER', text='c', location=L),
        Token(type='OPERATOR', text='=', location=L),
        Token(type='OPERATOR', text='-', location=L),
        Token(type='INTEGER', text='3', location=L),
    ]

    assert tokenize("1 + 2 % 3") == [
        Token(type='INTEGER', text='1', location=L),
        Token(type='OPERATOR', text='+', location=L),
        Token(type='INTEGER', text='2', location=L),
        Token(type='OPERATOR', text='%', location=L),
        Token(type='INTEGER', text='3', location=L),
    ]

    assert tokenize("var x: (a, b) => Int = 2") == [
        Token(type='IDENTIFIER', text='var', location=L),
        Token(type='IDENTIFIER', text='x', location=L),
        Token(type='PUNCTUATION', text=':', location=L),
        Token(type='PUNCTUATION', text='(', location=L),
        Token(type='IDENTIFIER', text='a', location=L),
        Token(type='PUNCTUATION', text=',', location=L),
        Token(type='IDENTIFIER', text='b', location=L),
        Token(type='PUNCTUATION', text=')', location=L),
        Token(type='OPERATOR', text='=>', location=L),
        Token(type='IDENTIFIER', text='Int', location=L),
        Token(type='OPERATOR', text='=', location=L),
        Token(type='INTEGER', text='2', location=L)
    ]

    assert tokenize("fun square(x: Int): Int {\nreturn x * x;\n}") == [
        Token(type='IDENTIFIER', text='fun', location=L),
        Token(type='IDENTIFIER', text='square', location=L),
        Token(type='PUNCTUATION', text='(', location=L),
        Token(type='IDENTIFIER', text='x', location=L),
        Token(type='PUNCTUATION', text=':', location=L),
        Token(type='IDENTIFIER', text='Int', location=L),
        Token(type='PUNCTUATION', text=')', location=L),
        Token(type='PUNCTUATION', text=':', location=L),
        Token(type='IDENTIFIER', text='Int', location=L),
        Token(type='PUNCTUATION', text='{', location=L),
        Token(type='IDENTIFIER', text='return', location=L),
        Token(type='IDENTIFIER', text='x', location=L),
        Token(type='OPERATOR', text='*', location=L),
        Token(type='IDENTIFIER', text='x', location=L),
        Token(type='PUNCTUATION', text=';', location=L),
        Token(type='PUNCTUATION', text='}', location=L)
    ]


"""
Here are some test cases for inputs that should fail to parse
Matching exception messages: not very care about location info
"""


def test_tokenize_exceptions() -> None:
    match_exception = r'.*Tokenizing failed at: "\$"'
    with pytest.raises(Exception, match=match_exception):
        tokenize("$")
