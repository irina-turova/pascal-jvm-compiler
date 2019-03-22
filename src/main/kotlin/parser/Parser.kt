package parser

import lexer.*
import java.lang.Exception
import common.ErrorList
import common.ErrorCode
import common.Error

class Parser(val lexer: Lexer, val errors: ErrorList) {

    private var currentToken: Token

    init {
        currentToken = getNextSymbol()
    }

    private fun getNextSymbol(): Token {
        return lexer.nextSymbol()
    }
    
    private fun pushError(code: ErrorCode) {
        errors.pushError(Error(lexer.tokenPosition, code))
    }

    fun parse() {
        program()
    }

    private fun accept(expectedToken: TokenType) {
        if (expectedToken == currentToken.type)
            currentToken = getNextSymbol()
        else
            pushError(ErrorCode.fromExpectedToken(expectedToken))
    }

    /**
     * <program> ::= program <identifier> ; <block> .
     */
    private fun program() {
        program_heading()
        accept(TokenType.SEMICOLON)
        block()
        accept(TokenType.DOT)
    }

    private fun program_heading() {
        accept(TokenType.PROGRAM)
        accept(TokenType.IDENTIFIER)
        if (currentToken.type == TokenType.LEFT_BRACKET) {
            identifier_list()
        }
    }

    private fun identifier_list() {
        accept(TokenType.IDENTIFIER)
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            accept(TokenType.IDENTIFIER)
        }
    }

    /**
     * <block> ::= <label declaration part> <constant definition part> <type definition part>
     *     <variable declaration part> <procedure and function declaration part> <statement part>
     */
    private fun block() {
        type_definition_part()
        variable_declaration_part()
        procedure_and_function_declaration_part()
        statement_part()
    }

    /**
     * <procedure and function declaration part> ::= {<procedure or function declaration > ;}
     */
    private fun procedure_and_function_declaration_part() {
        while (currentToken.type == TokenType.PROCEDURE || currentToken.type == TokenType.FUNCTION) {
            procedure_or_function_declaration()
            accept(TokenType.SEMICOLON)
        }
    }

    /**
     * <procedure or function declaration > ::= <procedure declaration> | <function declaration >
     */
    private fun procedure_or_function_declaration() {
        if (currentToken.type == TokenType.PROCEDURE) procedure_declaration()
        else if (currentToken.type == TokenType.FUNCTION) function_declaration()
    }

    /**
     * <procedure declaration> ::= <procedure heading> <block>
     *     // в описании стандарта более сложно
     */
    private fun procedure_declaration() {
        procedure_heading()
        block()
    }

    /**
     * <procedure heading> ::= procedure <identifier> ; |
     */
    private fun procedure_heading() {
        accept(TokenType.PROCEDURE)
        accept(TokenType.IDENTIFIER)

        if (currentToken.type == TokenType.LEFT_BRACKET)
            function_parameter_list()
    }

    /**
     * formal-parameter-list = `(' formal-parameter-section { ` ;' formal-parameter-section ~ `)' .
     */
    private fun function_parameter_list() {
        accept(TokenType.LEFT_BRACKET)
        formal_parameters_section()
        while (currentToken.type == TokenType.SEMICOLON) {
            accept(TokenType.SEMICOLON)
            formal_parameters_section()
        }
    }

    private fun function_declaration() {
        function_heading()
        block()
    }

    private fun function_heading() {
        accept(TokenType.FUNCTION)
        accept(TokenType.IDENTIFIER)

        if (currentToken.type == TokenType.LEFT_BRACKET)
            function_parameter_list()
    }

    /**
     * <formal parameter section> ::= <parameter group> | var <parameter group>
     *     | function <parameter group> | procedure <identifier> { , <identifier>}
     */
    private fun formal_parameters_section() {
        when(currentToken.type) {
            TokenType.IDENTIFIER -> value_parameter_section()
            TokenType.VAR -> variable_parameter_section()
            TokenType.FUNCTION -> function_heading()
            TokenType.PROCEDURE -> procedure_heading()
            else -> pushError(ErrorCode.UNEXPECTED_SYMBOL)
        }
    }

    /**
     * variable-parameter-section = var identifier-list ":" parameter-type
     */
    private fun variable_parameter_section() {
        accept(TokenType.VAR)
        parameter_group()
    }

    /**
     * value-parameter-section = identifier-list ":" parameter-type
     */
    private fun value_parameter_section() {
        parameter_group()
    }

    /**
     * <parameter group> ::= <identifier> {, <identifier>} : <type identifier>
     */
    private fun parameter_group() {
        accept(TokenType.IDENTIFIER)
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            accept(TokenType.IDENTIFIER)
        }
        accept(TokenType.COLON)
        type_identifier()
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
        if (currentToken.type == TokenType.IDENTIFIER)
            type_identifier()
        else
            pushError(ErrorCode.TYPE_IDENTIFIER_EXPECTED)
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
            else -> pushError(ErrorCode.IDENTIFIER_EXPECTED) // ?
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
            else -> throw Exception("Unexpected error - here MUST be one of BEGIN, IF, WHILE")
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
        throw Exception("Unexpected error - here MUST be one of +, -, or")
    }

    /**
     * <sign> ::= + | -
     */
    private fun sign() {
        when {
            currentToken.type == TokenType.PLUS -> accept(TokenType.PLUS)
            currentToken.type == TokenType.MINUS -> accept(TokenType.MINUS)
            else -> throw Exception("Unexpected error - here MUST be one of +, -")
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
            else -> pushError(ErrorCode.VARIABLE_IDENTIFIER_EXPECTED) // ?
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
            else -> pushError(ErrorCode.CONSTANT_EXPECTED)
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
        throw Exception("Unexpected error - here MUST be one of *, /, div, mod, and")
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
        throw Exception("Unexpected error - here MUST be one of =, <>, <, <=, >=, >, in")
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