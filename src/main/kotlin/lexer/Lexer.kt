package lexer

import io.IOProvider
import io.TextPosition

class Lexer(private val io: IOProvider) {

    lateinit var tokenPosition: TextPosition

    private fun scanSymbol(): Token {
        return when(io.nextChar()) {
            in 'a'..'z', in 'A'..'Z', '_'  -> {
                var identifier = io.takeNextChar().toString()
                while (io.nextChar().isLetterOrDigit() || io.nextChar() == '_')
                    identifier += io.takeNextChar()

                Token.KEYWORDS.getOrDefault(identifier.toLowerCase(), TokenType.IDENTIFIER).let {
                    if (it == TokenType.IDENTIFIER)
                        IdentifierToken(TokenType.IDENTIFIER, identifier)
                    else
                        KeywordToken(it)
                }
            }
            in '0'..'9' -> {
                var number = io.takeNextChar().toString()
                while (io.nextChar().isDigit())
                    number += (io.takeNextChar())
                ConstantToken(TokenType.INT_CONSTANT, number)
            }
            '\'' -> {
                var string = io.takeNextChar().toString()
                while (io.nextChar() != '\'')
                    string += (io.takeNextChar())
                ConstantToken(TokenType.STRING_CONSTANT, string)
            }
            ':' -> {
                io.takeNextChar()
                return if (io.nextChar() == '=')
                    Token(TokenType.ASSIGN_OPERATOR).also { io.takeNextChar() }
                else
                    Token(TokenType.COLON)
            }
            ';' -> {
                io.takeNextChar()
                return Token(TokenType.SEMICOLON)
            }
            ',' -> {
                io.takeNextChar()
                return Token(TokenType.COMMA)
            }
            '^' -> {
                io.takeNextChar()
                return Token(TokenType.CARET)
            }
            '<' -> {
                io.takeNextChar()
                return when {
                    io.nextChar() == '=' -> Token(TokenType.LESS_OR_EQUAL_OPERATOR).also { io.takeNextChar() }
                    io.nextChar() == '>' -> Token(TokenType.NOT_EQUAL_OPERATOR).also { io.takeNextChar() }
                    else -> Token(TokenType.LESS_OPERATOR)
                }
            }
            '>' -> {
                io.takeNextChar()
                return if (io.nextChar() == '=')
                    Token(TokenType.GREATER_OR_EQUAL_OPERATOR).also { io.takeNextChar() }
                else
                    Token(TokenType.GREATER_OPERATOR)
            }
            '.' -> {
                io.takeNextChar()
                return if (io.nextChar() == '.')
                    Token(TokenType.DOT_DOT).also { io.takeNextChar() }
                else
                    Token(TokenType.DOT)
            }
            '+' -> {
                io.takeNextChar()
                return Token(TokenType.PLUS)
            }
            '-' -> {
                io.takeNextChar()
                return Token(TokenType.MINUS)
            }
            '*' -> {
                io.takeNextChar()
                return if (io.nextChar() == ')')
                    Token(TokenType.START_COMMENT_STAR).also { io.takeNextChar() }
                else
                    Token(TokenType.STAR)
            }
            '/' -> {
                io.takeNextChar()
                return Token(TokenType.SLASH)
            }
            '=' -> {
                io.takeNextChar()
                return Token(TokenType.EQUAL_OPERATOR)
            }
            '(' -> {
                io.takeNextChar()
                return if (io.nextChar() == '*')
                    Token(TokenType.END_COMMENT_STAR).also { io.takeNextChar() }
                else
                    Token(TokenType.LEFT_BRACKET)
            }
            ')' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_BRACKET)
            }
            '{' -> {
                io.takeNextChar()
                return Token(TokenType.LEFT_CURLY_BRACKET)
            }
            '}' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_CURLY_BRACKET)
            }
            '[' -> {
                io.takeNextChar()
                return Token(TokenType.LEFT_SQUARE_BRACKET)
            }
            ']' -> {
                io.takeNextChar()
                return Token(TokenType.RIGHT_SQUARE_BRACKET)
            }
            '\u0000' -> {
                io.takeNextChar()
                return Token(TokenType.THIS_IS_THE_END)
            }
            else -> {
                io.takeNextChar()
                return Token(TokenType.NOTHING)
            }
        }
    }

    fun nextSymbol(): Token {
        while (io.nextChar() == ' ')
            io.takeNextChar()

        tokenPosition = io.currentPosition
        return scanSymbol()
    }

}