package lexer

enum class TokenType {
    NOTHING,

    IDENTIFIER,

    INT_CONSTANT,
    DOUBLE_CONSTANT,
    STRING_CONSTANT,

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

    PLUS,
    MINUS,

    STAR,
    SLASH,
    CARET,

    PROGRAM,
    BEGIN,
    END,
    IF,
    THEN,
    ELSE,
    DO,
    FOR,
    IN,
    WHILE,

    START_COMMENT_STAR,
    END_COMMENT_STAR,

    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,

    THIS_IS_THE_END
}