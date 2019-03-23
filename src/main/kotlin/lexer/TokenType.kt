package lexer

import java.lang.Error

enum class TokenType {
    NOTHING,

    IDENTIFIER,

    INT_CONSTANT,
    DOUBLE_CONSTANT,
    STRING_CONSTANT, // TODO: we do not support strings
    CHAR_CONSTANT, // TODO: support char constant in lexer

    COMMA,
    COLON,
    SEMICOLON,
    DOT,
    DOT_DOT,

    ASSIGN_OPERATOR,

    EQUAL_OPERATOR,
    NOT_EQUAL_OPERATOR,
    LESS_OPERATOR,
    LESS_OR_EQUAL_OPERATOR,
    GREATER_OPERATOR,
    GREATER_OR_EQUAL_OPERATOR,

    NOT,
    OR,
    AND,

    PLUS,
    MINUS,

    DIV,
    MOD,

    STAR,
    SLASH,
    CARET,

    PROGRAM,
    TYPE,
    VAR,
    BEGIN,
    END,
    IF,
    THEN,
    ELSE,
    DO,
    FOR,
    TO,
    DOWN_TO,
    IN,
    WHILE,
    REPEAT,
    UNTIL,

    START_COMMENT_STAR,
    END_COMMENT_STAR,

    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,

    THIS_IS_THE_END,

    PROCEDURE,

    FUNCTION
}