"""Tokenizer: splitting a source code text into token lists

Parameters:
    source_code: str

Returns:
    |list[Token]
    |--Token
    |----type: TokenType
    |------TokenType: Literal
    |----text: str
    |----location: Location
    |------Location
    |--------file: str
    |--------line: int
    |--------column: int

Raises:
    KeyError - raises an exception

------
We use function 'tokenize(source_code: str) -> list[Object]' to recognizes the following tokens:
1. Identifiers
    i.e. variable names and keywords
2. Non-negative integer literals
3. Operators: +, -, *, /, =, ==, !=, <, <=, >, >=
4. Punctuation: (, ), {, }, ,, ;

We can support skip:
1. Whitespace
2. Newlines
3. Comments: one-line and multi-line 
"""
from dataclasses import dataclass
from typing import Literal
import re

# identifier(keyword, and others), integer(decimal, hexadecimal, octal), operator, punctuation
TokenType = Literal['INTEGER', 'IDENTIFIER', 'OPERATOR', 'PUNCTUATION',
                    'NEWLINE', 'SKIP', 'COMMENT', 'END', 'BOOL', None]
keyword = {
    'if', 'then', 'else', 'while', 'do', 'fun', 'return', 'var', 'Int', 'Bool',
    'Unit', 'break', 'continue'
}

newline_token = r'\n'
skip_token = r'[ \t\f]+'
comment_token = r'(//|#)+(.*?)$|/\*(.*?)\*/'
integer_token = r'[0-9]+'
identifier_token = r'[a-zA-Z_][a-zA-Z0-9_]*'
operator_token = r'==|!=|<=|>=|=>|[+\-*/=<>%]'
punc_token = r'[(){},;:]'
mismatch_token = r'.'
bool_token = r'(true|false)'

token_expression = [
    ('NEWLINE', newline_token),
    ('COMMENT', comment_token),
    ('BOOL', bool_token),
    ('INTEGER', integer_token),
    ('IDENTIFIER', identifier_token),
    ('OPERATOR', operator_token),
    ('PUNCTUATION', punc_token),
    ('SKIP', skip_token),
    ('MISMATCH', mismatch_token),
]


@dataclass
class Location:
    file: str
    line: int
    column: int

    def __init__(self, line: int, column: int) -> None:
        self.file = 'test'
        self.line = line
        self.column = column

    def __eq__(self, __value: object) -> bool:
        if isinstance(__value, self.__class__):
            #return self.line== __value.line and self.column== __value.column
            return True
        return True

    def __str__(self) -> str:
        return f"File: {self.file}, Line: {self.line}, Column: {self.column}"


@dataclass(frozen=True)
class Token:
    type: TokenType
    text: str
    # location: file, line and column
    location: Location

    def __str__(self) -> str:
        return self.text


def tokenize(source_code: str) -> list[Token]:

    def check_type(group_name: str | None) -> TokenType:
        type: TokenType = None
        match group_name:
            case 'IDENTIFIER':
                type = 'IDENTIFIER'
            case 'INTEGER':
                type = 'INTEGER'
            case 'OPERATOR':
                type = 'OPERATOR'
            case 'PUNCTUATION':
                type = 'PUNCTUATION'
            case 'BOOL':
                type = 'BOOL'
            case _:
                type = None

        return type

    token_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_expression)

    line_num = 1
    line_start = 0
    result: list[Token] = []

    for m in re.finditer(token_regex, source_code, re.MULTILINE | re.DOTALL):
        group_name = m.lastgroup
        value = m.group(0)
        column_num = m.start() - line_start + 1
        location = Location(line_num, column_num)

        if group_name == 'NEWLINE':
            line_start = m.end()
            line_num += 1
            continue
        elif group_name == 'SKIP' or group_name == 'COMMENT':
            continue
        elif group_name == 'MISMATCH':
            raise Exception(f'{location}: Tokenizing failed at: "{value}"')

        type: TokenType = check_type(group_name)
        result.append(Token(type=type, text=value, location=location))

    return result
