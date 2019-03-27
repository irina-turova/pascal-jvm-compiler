package lexer

import io.TextPosition

open class Token(val type: TokenType, val position: TextPosition) {
    companion object {
        val KEYWORDS = mapOf(
            "and" to TokenType.AND,
            "begin" to TokenType.BEGIN,
            "div" to TokenType.DIV,
            "do" to TokenType.DO,
            "downto" to TokenType.DOWN_TO,
            "else" to TokenType.ELSE,
            "end" to TokenType.END,
            "for" to TokenType.FOR,
            "function" to TokenType.FUNCTION,
            "if" to TokenType.IF,
            "in" to TokenType.IN,
            "mod" to TokenType.MOD,
            "not" to TokenType.NOT,
            "or" to TokenType.OR,
            "procedure" to TokenType.PROCEDURE,
            "program" to TokenType.PROGRAM,
            "repeat" to TokenType.REPEAT,
            "then" to TokenType.THEN,
            "to" to TokenType.TO,
            "type" to TokenType.TYPE,
            "until" to TokenType.UNTIL,
            "var" to TokenType.VAR,
            "while" to TokenType.WHILE
        )
    }

    override fun toString(): String {
        return type.toString()
    }
}

class KeywordToken(type: TokenType, position: TextPosition): Token(type, position)

class IdentifierToken(type: TokenType, position: TextPosition, val identifier: String): Token(type, position) {
    override fun toString(): String {
        return super.toString() + " " + identifier
    }
}

class ConstantToken(type: TokenType, position: TextPosition, val value: Any): Token(type, position)