package lexer

import io.IOProvider
import io.TextPosition
import common.ErrorList
import common.Error
import common.ErrorCode

class Lexer(private val io: IOProvider, val errors: ErrorList) {

    private lateinit var tokenPosition: TextPosition

    private fun scanSymbol(): Token {
        return when(io.nextChar()) {
            in 'a'..'z', in 'A'..'Z', '_'  -> {
                var identifier = io.takeNextChar().toString()
                while (io.nextChar().isLetterOrDigit() || io.nextChar() == '_')
                    identifier += io.takeNextChar()

                Token.KEYWORDS.getOrDefault(identifier.toLowerCase(), TokenType.IDENTIFIER).let {
                    if (it == TokenType.IDENTIFIER)
                        IdentifierToken(TokenType.IDENTIFIER, tokenPosition, identifier)
                    else
                        KeywordToken(it, tokenPosition)
                }
            }
            in '0'..'9' -> {
                var number = io.takeNextChar().toString()
                while (io.nextChar().isDigit())
                    number += (io.takeNextChar())
                ConstantToken(TokenType.INT_CONSTANT, tokenPosition, number)
            }
            '\'' -> {
                var string = io.takeNextChar().toString()
                while (io.nextChar() != '\'')
                    string += (io.takeNextChar())
                ConstantToken(TokenType.STRING_CONSTANT, tokenPosition, string)
            }
            ':' -> {
                io.takeNextChar()
                return if (io.nextChar() == '=')
                    Token(TokenType.ASSIGN_OPERATOR, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.COLON, tokenPosition)
            }
            ';' -> {
                io.takeNextChar()
                return Token(TokenType.SEMICOLON, tokenPosition)
            }
            ',' -> {
                io.takeNextChar()
                return Token(TokenType.COMMA, tokenPosition)
            }
            '^' -> {
                io.takeNextChar()
                return Token(TokenType.CARET, tokenPosition)
            }
            '<' -> {
                io.takeNextChar()
                return when {
                    io.nextChar() == '=' -> Token(TokenType.LESS_OR_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
                    io.nextChar() == '>' -> Token(TokenType.NOT_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
                    else -> Token(TokenType.LESS_OPERATOR, tokenPosition)
                }
            }
            '>' -> {
                io.takeNextChar()
                return if (io.nextChar() == '=')
                    Token(TokenType.GREATER_OR_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.GREATER_OPERATOR, tokenPosition)
            }
            '.' -> {
                io.takeNextChar()
                return if (io.nextChar() == '.')
                    Token(TokenType.DOT_DOT, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.DOT, tokenPosition)
            }
            '+' -> {
                io.takeNextChar()
                return Token(TokenType.PLUS, tokenPosition)
            }
            '-' -> {
                io.takeNextChar()
                return Token(TokenType.MINUS, tokenPosition)
            }
            '*' -> {
                io.takeNextChar()
                return if (io.nextChar() == ')')
                    Token(TokenType.START_COMMENT_STAR, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.STAR, tokenPosition)
            }
            '/' -> {
                io.takeNextChar()
                return Token(TokenType.SLASH, tokenPosition)
            }
            '=' -> {
                io.takeNextChar()
                return Token(TokenType.EQUAL_OPERATOR, tokenPosition)
            }
            '(' -> {
                io.takeNextChar()
                return if (io.nextChar() == '*')
                    Token(TokenType.END_COMMENT_STAR, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.LEFT_BRACKET, tokenPosition)
            }
            ')' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_BRACKET, tokenPosition)
            }
            '{' -> {
                io.takeNextChar()
                return Token(TokenType.LEFT_CURLY_BRACKET, tokenPosition)
            }
            '}' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_CURLY_BRACKET, tokenPosition)
            }
            '[' -> {
                io.takeNextChar()
                return Token(TokenType.LEFT_SQUARE_BRACKET, tokenPosition)
            }
            ']' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_SQUARE_BRACKET, tokenPosition)
            }
            '\u0000' -> {
                io.takeNextChar()
                return Token(TokenType.THIS_IS_THE_END, tokenPosition)
            }
            else -> {
                io.takeNextChar()
                errors.pushError(Error(tokenPosition, ErrorCode.NO_NAME_ERROR))
                return Token(TokenType.NOTHING, tokenPosition)
            }
        }
    }

    fun nextSymbol(): Token {
        while (io.nextChar() == ' ')
            io.takeNextChar()

        tokenPosition = io.currentPosition.copy()
        return scanSymbol()
    }

}