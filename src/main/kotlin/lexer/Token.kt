package lexer

open class Token(val type: TokenType) {
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
            "while" to TokenType.WHILE
        )
    }

    override fun toString(): String {
        return type.toString()
    }
}

class KeywordToken(type: TokenType): Token(type)

class IdentifierToken(type: TokenType, val identifier: String): Token(type) {
    override fun toString(): String {
        return super.toString() + " " + identifier
    }
}

class ConstantToken(type: TokenType, val value: Any): Token(type)