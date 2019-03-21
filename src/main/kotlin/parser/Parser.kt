package parser

import lexer.Lexer
import lexer.Token
import lexer.TokenType
import java.lang.Exception
import kotlin.reflect.KFunction1

class Parser(val lexer: Lexer) {

    private var currentToken: Token

    init {
        currentToken = getNextSymbol()
    }

    fun getNextSymbol(): Token {
        return lexer.nextSymbol()
    }

    fun parse() {
        program()
    }

    fun accept(expectedToken: TokenType) {
        if (expectedToken == currentToken.type)
            currentToken = getNextSymbol()
        else
            throw Exception("Expected $expectedToken got ${currentToken.type}")
    }

    /**
     * <program> ::= program <identifier> ; <block> .
     */
    private fun program() {
        accept(TokenType.PROGRAM)
        accept(TokenType.IDENTIFIER)
        accept(TokenType.SEMICOLON)
        block()
        accept(TokenType.DOT)
    }

    /**
     * <block> ::= <label declaration part> <constant definition part> <type definition part>
     *     <variable declaration part> <procedure and function declaration part> <statement part>
     */
    private fun block() {
        type_definition_part()
        variable_declaration_part()
        statement_part()
    }

    /**
     * <type definition part> ::= <empty> | type <type definition> {;<type definition>};
     */
    private fun type_definition_part() {
        if (currentToken.type == TokenType.TYPE) {
            type_definition()
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                type_definition()
                accept(TokenType.SEMICOLON)
            }
        }
    }

    /**
     * <type definition> ::= <identifier> = <type>
     */
    private fun type_definition() {
        accept(TokenType.IDENTIFIER)
        accept(TokenType.EQUAL_OPERATOR)
        type()
    }

    /**
     * <variable declaration part> ::= <empty> | var <variable declaration> {; <variable declaration>} ;
     */
    private fun variable_declaration_part() {
        if (currentToken.type == TokenType.VAR) {
            accept(TokenType.VAR)
            variable_declaration()
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                variable_declaration()
                accept(TokenType.SEMICOLON)
            }
        }
    }

    /**
     * <variable declaration> ::= <identifier> {,<identifier>} : <type>
     */
    private fun variable_declaration() {
        accept(TokenType.IDENTIFIER)
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            accept(TokenType.IDENTIFIER)
        }
        accept(TokenType.COLON)
        type()
    }

    /**
     * <type> ::= <simple type> | <structured type> | <pointer type>
     */
    private fun type() {
        simple_type()
    }

    /**
     * <simple type> ::= <scalar type> | <subrange type> | <type identifier>
     */
    private fun simple_type() {
        when {
            currentToken.type == TokenType.LEFT_BRACKET -> scalar_type()
            currentToken.type == TokenType.IDENTIFIER -> type_identifier()
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <scalar type> ::= (<identifier> {,<identifier>})
     */
    private fun scalar_type() {
        accept(TokenType.LEFT_BRACKET)
        accept(TokenType.IDENTIFIER)

        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            accept(TokenType.IDENTIFIER)
        }

        accept(TokenType.RIGHT_BRACKET)
    }

    /**
     * <type identifier> ::= <identifier>
     */
    private fun type_identifier() {
        accept(TokenType.IDENTIFIER)
    }

    /**
     * <statement part> ::= <compound statement>
     */
    private fun statement_part() {
        compound_statement()
    }

    /**
     * <compound statement> ::= begin <statement> {; <statement> } end;
     */
    private fun compound_statement() {
        accept(TokenType.BEGIN)

        statement()
        while (currentToken.type == TokenType.SEMICOLON) {
            accept(TokenType.SEMICOLON)
            statement()
        }

        accept(TokenType.END)
    }

    /**
     * <statement> ::= <unlabelled statement> | <label> : <unlabelled statement>
     */
    private fun statement() {
        unlabelled_statement()
    }

    /**
     * <unlabelled statement> ::= <simple statement> | <structured statement>
     */
    private fun unlabelled_statement() {
        when {
            currentToken.type == TokenType.IDENTIFIER -> simple_statement()
            currentToken.type in setOf(TokenType.BEGIN, TokenType.IF, TokenType.WHILE) -> structured_statement()
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <simple statement> ::= <assignment statement> | <procedure statement> | <go to statement> | <empty statement>
     */
    private fun simple_statement() {
        if (currentToken.type == TokenType.IDENTIFIER)
            assignment_statement()
    }

    /**
     * <assignment statement> ::= <variable> := <expression> | <function identifier> := <expression>
     */
    private fun assignment_statement() {
        accept(TokenType.IDENTIFIER)
        accept(TokenType.ASSIGN_OPERATOR)
        expression()
    }

    /**
     * <structured statement> ::= <compound statement> | <conditional statement> | <repetitive statement> | <with statement>
     */
    private fun structured_statement() {
        when {
            currentToken.type == TokenType.BEGIN -> compound_statement()
            currentToken.type == TokenType.IF -> conditional_statement()
            currentToken.type == TokenType.WHILE -> repetitive_statement()
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <conditional statement> ::= <if statement> | <case statement>
     */
    private fun conditional_statement() {
        if_statement()
    }

    /**
     * <if statement> ::= if <expression> then <statement> | if <expression> then <statement> else <statement>
     */
    private fun if_statement() {
        accept(TokenType.IF)
        expression()
        accept(TokenType.THEN)
        statement()
        if (currentToken.type == TokenType.ELSE) {
            accept(TokenType.ELSE)
            statement()
        }
    }

    /**
     * <expression> ::= <simple expression> | <simple expression> <relational operator> <simple expression>
     */
    private fun expression() {
        simple_expression()
        if (currentToken.type in relationalOperators) {
            relational_operator()
            simple_expression()
        }
    }

    /**
     * <simple expression> ::= <term> | <sign> <term>| <simple expression> <adding operator> <term>
     */
    private fun simple_expression() {
        if (currentToken.type in setOf(TokenType.PLUS, TokenType.MINUS)) {
            sign()
            term()
        } else if (currentToken.type in setOf(TokenType.IDENTIFIER, TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT,
                TokenType.LEFT_BRACKET, TokenType.NOT))
            term()
        while (currentToken.type in setOf(TokenType.PLUS, TokenType.MINUS, TokenType.OR)) {
            adding_operator()
            simple_expression()
        }
    }

    /**
     * <adding operator> ::= + | - | or
     */
    val addingOperators = setOf(TokenType.PLUS, TokenType.MINUS, TokenType.OR)
    private fun adding_operator() {
        for (opToken in addingOperators)
            if (currentToken.type == opToken) {
                accept(opToken)
                return
            }
        throw Exception("ERROR")
    }

    /**
     * <sign> ::= + | -
     */
    private fun sign() {
        when {
            currentToken.type == TokenType.PLUS -> accept(TokenType.PLUS)
            currentToken.type == TokenType.MINUS -> accept(TokenType.MINUS)
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <term> ::= <factor> | <term> <multiplying operator> <factor>
     */
    private fun term() {
        factor()
        while (currentToken.type in multiplyingOperators) {
            multiplying_operator()
            factor()
        }
    }

    /**
     * <factor> ::= <variable> | <unsigned constant> | ( <expression> ) | <function designator> | <set> | not <factor>
     */
    private fun factor() {
        when {
            currentToken.type == TokenType.IDENTIFIER -> variable()
            currentToken.type in setOf(TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.STRING_CONSTANT) -> unsingned_constant()
            currentToken.type == TokenType.LEFT_BRACKET -> {
                accept(TokenType.LEFT_BRACKET)
                expression()
                accept(TokenType.RIGHT_BRACKET)
            }
            currentToken.type == TokenType.NOT -> {
                accept(TokenType.NOT)
                factor()
            }
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <variable> ::= <entire variable> | <component variable> | <referenced variable>
     */
    private fun variable() {
        accept(TokenType.IDENTIFIER)
    }

    /**
     * <unsigned constant> ::= <unsigned number> | <string> | < constant identifier> < nil>
     */
    private fun unsingned_constant() {
        when {
            currentToken.type == TokenType.INT_CONSTANT -> accept(TokenType.INT_CONSTANT)
            currentToken.type == TokenType.DOUBLE_CONSTANT -> accept(TokenType.DOUBLE_CONSTANT)
            currentToken.type == TokenType.STRING_CONSTANT -> accept(TokenType.STRING_CONSTANT)
            else -> throw Exception("ERROR")
        }
    }

    /**
     * <multiplying operator> ::= * | / | div | mod | and
     */
    val multiplyingOperators = setOf(TokenType.STAR, TokenType.SLASH, TokenType.DIV,
        TokenType.MOD, TokenType.AND)
    private fun multiplying_operator() {
        for (opToken in relationalOperators)
            if (currentToken.type == opToken) {
                accept(opToken)
                return
            }
        throw Exception("ERROR")
    }

    /**
     * <relational operator> ::= = | <> | < | <= | >= | > | in
     */
    val relationalOperators = setOf(TokenType.EQUAL_OPERATOR, TokenType.NOT_EQUAL_OPERATOR, TokenType.LESS_OPERATOR,
        TokenType.LESS_OR_EQUAL_OPERATOR, TokenType.GREATER_OR_EQUAL_OPERATOR, TokenType.GREATER_OPERATOR)
    private fun relational_operator() {
        for (opToken in relationalOperators)
            if (currentToken.type == opToken) {
                accept(opToken)
                return
            }
        throw Exception("ERROR")
    }

    /**
     * <repetitive statement> ::= <while statement> | <repeat statemant> | <for statement>
     */
    private fun repetitive_statement() {
        while_statement()
    }

    /**
     * <while statement> ::= while <expression> do <statement>
     */
    private fun while_statement() {
        accept(TokenType.WHILE)
        expression()
        accept(TokenType.DO)
        statement()
    }
}