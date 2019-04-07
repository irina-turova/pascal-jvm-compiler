package lexer

import io.IOProvider
import io.TextPosition
import common.ErrorList
import common.Error
import common.ErrorCode
import kotlin.math.pow

class Lexer(private val io: IOProvider, private val errors: ErrorList) {

    private val maxInt = 32767
    private val maxReal = 1.7 * 10.0.pow(38)

    private lateinit var tokenPosition: TextPosition

    private fun scanSymbol(): Token {
        return when(io.nextChar()) {
            in 'a'..'z', in 'A'..'Z'  -> {
                var identifier = io.takeNextChar().toString()
                while (io.nextChar().isLetterOrDigit())
                    identifier += io.takeNextChar()

                Token.KEYWORDS[identifier.toLowerCase()].let {
                    if (it == null)
                        IdentifierToken(TokenType.IDENTIFIER, tokenPosition, identifier)
                    else
                        KeywordToken(it, tokenPosition)
                }
            }
            in '0'..'9' -> {
                var metDot = false
                var metE = false
                var metSign = false

                var number = io.takeNextChar().toString()
                while (io.nextChar().isDigit() || io.nextChar() in setOf('.', 'e', 'E', '+', '-')) {
                    if (io.nextChar() == '.') {
                        if (metDot || metE)
                            break
                        metDot = true
                    }
                    if (io.nextChar().toLowerCase() == 'e') {
                        if (metE)
                            break
                        metE = true
                    }
                    if (io.nextChar() == '+' || io.nextChar() == '-') {
                        if (metSign || number.last().toLowerCase() != 'e')
                            break
                        metSign = true
                    }
                    number += (io.takeNextChar())
                }

                ConstantToken(
                    if (metDot || metE) {
                        if (number.toDouble() > maxReal)
                            errors.pushError(Error(tokenPosition, ErrorCode.CONSTANT_OUT_OF_RANGE))
                        TokenType.DOUBLE_CONSTANT
                    } else {
                        if (number.toInt() > maxInt)
                            errors.pushError(Error(tokenPosition, ErrorCode.CONSTANT_OUT_OF_RANGE))
                        TokenType.INT_CONSTANT
                    }, tokenPosition, number.toIntOrNull() ?: number.toFloat())
            }
            '\'' -> {
                io.takeNextChar()
                val char = io.takeNextChar()
                if (io.nextChar() != '\'') {
                    errors.pushError(Error(tokenPosition, ErrorCode.CHARACTER_EXPRESSION_EXPECTED))
                    Token(TokenType.NOTHING, tokenPosition)
                } else {
                    io.takeNextChar()
                    ConstantToken(TokenType.CHAR_CONSTANT, tokenPosition, char)
                }
            }
            ':' -> {
                io.takeNextChar()
                if (io.nextChar() == '=')
                    Token(TokenType.ASSIGN_OPERATOR, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.COLON, tokenPosition)
            }
            ';' -> {
                io.takeNextChar()
                Token(TokenType.SEMICOLON, tokenPosition)
            }
            ',' -> {
                io.takeNextChar()
                Token(TokenType.COMMA, tokenPosition)
            }
//            '^' -> {
//                io.takeNextChar()
//                Token(TokenType.CARET, tokenPosition)
//            }
//            '<' -> {
//                io.takeNextChar()
//                when {
//                    io.nextChar() == '=' -> Token(TokenType.LESS_OR_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
//                    io.nextChar() == '>' -> Token(TokenType.NOT_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
//                    else -> Token(TokenType.LESS_OPERATOR, tokenPosition)
//                }
//            }
//            '>' -> {
//                io.takeNextChar()
//                if (io.nextChar() == '=')
//                    Token(TokenType.GREATER_OR_EQUAL_OPERATOR, tokenPosition).also { io.takeNextChar() }
//                else
//                    Token(TokenType.GREATER_OPERATOR, tokenPosition)
//            }
            '.' -> {
                io.takeNextChar()
                if (io.nextChar() == '.')
                    Token(TokenType.DOT_DOT, tokenPosition).also { io.takeNextChar() }
                else
                    Token(TokenType.DOT, tokenPosition)
            }
            '+' -> {
                io.takeNextChar()
                Token(TokenType.PLUS, tokenPosition)
            }
            '-' -> {
                io.takeNextChar()
                Token(TokenType.MINUS, tokenPosition)
            }
            '*' -> {
                io.takeNextChar()
                Token(TokenType.STAR, tokenPosition)
            }
            '/' -> {
                io.takeNextChar()
                if (io.nextChar() == '/') {
                    while (io.nextChar() != '\n')
                        io.takeNextChar()
                    while (io.nextChar().isWhitespace())
                        io.takeNextChar()

                    scanSymbol()
                } else
                    Token(TokenType.SLASH, tokenPosition)
            }
            '=' -> {
                io.takeNextChar()
                Token(TokenType.EQUAL_OPERATOR, tokenPosition)
            }
            '(' -> {
                io.takeNextChar()
                if (io.nextChar() == '*') {
                    io.takeNextChar()
                    var prevChar = io.takeNextChar()
                    while (!(prevChar == '*'  && io.nextChar() == ')') && io.nextChar() != '\u0000')
                        prevChar = io.takeNextChar()
                    if (io.nextChar() != '\u0000')
                        io.takeNextChar()
                    while (io.nextChar().isWhitespace())
                        io.takeNextChar()
                    scanSymbol()
                } else
                    Token(TokenType.LEFT_BRACKET, tokenPosition)
            }
            ')' -> {
                io.takeNextChar()
                Token(TokenType.RIGHT_BRACKET, tokenPosition)
            }
            '{' -> {
                io.takeNextChar()
                while (io.nextChar() != '}' && io.nextChar() != '\u0000')
                    io.takeNextChar()
                if (io.nextChar() != '\u0000')
                    io.takeNextChar()
                while (io.nextChar().isWhitespace())
                    io.takeNextChar()

                scanSymbol()
            }
            '}' -> {
                io.takeNextChar()
                Token(TokenType.RIGHT_CURLY_BRACKET, tokenPosition)
            }
            '[' -> {
                io.takeNextChar()
                Token(TokenType.LEFT_SQUARE_BRACKET, tokenPosition)
            }
            ']' -> {
                io.takeNextChar()
                Token(TokenType.RIGHT_SQUARE_BRACKET, tokenPosition)
            }
            '\u0000' -> {
                io.takeNextChar()
                Token(TokenType.THIS_IS_THE_END, tokenPosition)
            }
            else -> {
                io.takeNextChar()
                errors.pushError(Error(tokenPosition, ErrorCode.NO_NAME_ERROR))
                Token(TokenType.NOTHING, tokenPosition)
            }
        }
    }

    fun nextSymbol(): Token {
        while (io.nextChar().isWhitespace())
            io.takeNextChar()

        tokenPosition = io.currentPosition.copy()
        return scanSymbol()
    }

    fun flush() {
        io.flush()
    }

}