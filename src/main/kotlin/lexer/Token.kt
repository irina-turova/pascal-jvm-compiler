package lexer

import io.TextPosition

open class Token(val type: TokenType, val position: TextPosition) {
    companion object {
        val KEYWORDS = mapOf(
            "program" to TokenType.PROGRAM,
            "type" to TokenType.TYPE,
            "var" to TokenType.VAR,
            "begin" to TokenType.BEGIN,
            "end" to TokenType.END,
            "if" to TokenType.IF,
            "then" to TokenType.THEN,
            "else" to TokenType.ELSE,
            "do" to TokenType.DO,
            "for" to TokenType.FOR,
            "in" to TokenType.IN,
            "while" to TokenType.WHILE,
            "not" to TokenType.NOT,
            "or" to TokenType.OR,
            "and" to TokenType.AND
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