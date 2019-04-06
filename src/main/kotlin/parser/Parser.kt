package parser

import common.Error
import common.ErrorCode
import common.ErrorList
import lexer.IdentifierToken
import lexer.Lexer
import lexer.Token
import lexer.TokenType
import semantic.ScopeManager
import semantic.identifiers.ConstantIdentifier
import semantic.identifiers.ProgramIdentifier
import semantic.identifiers.TypeIdentifier
import semantic.identifiers.VariableIdentifier
import semantic.types.Type

class Parser(private val lexer: Lexer, private val errors: ErrorList, private val scopeManager: ScopeManager) {

    private var currentToken: Token

    init {
        currentToken = getNextSymbol()
    }

    private fun getNextSymbol(): Token {
        return lexer.nextSymbol()
    }
    
    private fun pushError(code: ErrorCode) {
        errors.pushError(Error(currentToken.position, code))
    }

    private fun skipTo(vararg tokens: Set<TokenType>) {
        print("Skipping: ")
        val united = tokens.fold(setOf(TokenType.THIS_IS_THE_END)) { a, b -> a union b}
        while (currentToken.type !in united) {
            println("\t$currentToken")
            currentToken = getNextSymbol()
        }
    }

    private fun checkBeg(starters: Set<TokenType>, followers: Set<TokenType>) {
        if (currentToken.type !in starters) {
            pushError(ErrorCode.UNEXPECTED_SYMBOL)
            println("Now we will skip because of:")
            Exception().printStackTrace(System.out)
            skipTo(starters, followers)
        }
    }

    private fun checkEnd(followers: Set<TokenType>, error: ErrorCode? = null) {
        if (currentToken.type !in followers) {
            pushError(ErrorCode.UNEXPECTED_SYMBOL)
            println("Now we will skip because of:")
            Exception().printStackTrace(System.out)
            skipTo(followers)
        }
    }

    fun parse() {
        program(setOf(TokenType.THIS_IS_THE_END))
        lexer.flush()
    }

    private fun accept(expectedToken: TokenType) {
        if (expectedToken == currentToken.type)
            currentToken = getNextSymbol()
        else
            pushError(ErrorCode.fromExpectedToken(expectedToken))
    }

    private val blockStarters = setOf(/* TokenType.TYPE, */ TokenType.VAR /*, TokenType.FUNCTION */, TokenType.BEGIN)
    private val structuredStatementStarters = setOf(TokenType.BEGIN /*, TokenType.IF, TokenType.WHILE, TokenType.REPEAT, TokenType.FOR */)
    private val simpleStatementStarters = setOf(TokenType.IDENTIFIER)
    private val unsignedConstantStarters = setOf(TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT)
    private val factorStarters = setOf(TokenType.IDENTIFIER, TokenType.LEFT_BRACKET /*, TokenType.NOT*/) + unsignedConstantStarters
    private val termStarters = factorStarters
    private val simpleExpressionStarters = setOf(TokenType.PLUS, TokenType.MINUS) + termStarters

    /**
     * <program> ::= program <identifier> ; <block> .
     */
    private fun program(followers: Set<TokenType>) {
        val starters = setOf(TokenType.PROGRAM)
        checkBeg(starters, followers)

        // The main scope for the program
        scopeManager.openScope()

        if (currentToken.type in starters) {
            program_heading(followers union blockStarters )
            block(followers union setOf(TokenType.DOT))
            accept(TokenType.DOT)

            checkEnd(followers)
        }
        scopeManager.closeScope()
    }

    /**
     * program-heading = `program' identifier [ `(' program-parameter-list `)' ]
     */
    private fun program_heading(followers: Set<TokenType>) {
        val starters = setOf(TokenType.PROGRAM)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            accept(TokenType.PROGRAM)
            val token = currentToken
            if (token is IdentifierToken)
                scopeManager.addIdentifier(ProgramIdentifier(token.identifier, ScopeManager.programType))
            accept(TokenType.IDENTIFIER)
            if (currentToken.type == TokenType.LEFT_BRACKET) {
                accept(TokenType.LEFT_BRACKET)
                identifier_list(followers + TokenType.RIGHT_BRACKET)
                scopeManager.flushVariableBuffer(ScopeManager.programParameterType)
                accept(TokenType.RIGHT_BRACKET)
            }
            accept(TokenType.SEMICOLON)
            checkEnd(followers)
        }
    }

    /**
     * identifier-list = identifier { `,' identifier }
     */
    private fun identifier_list(followers: Set<TokenType>) {
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            if (scopeManager.findLocalIdentifier(currentToken) != null)
                pushError(ErrorCode.DUPLICATE_IDENTIFIER)
            else
                scopeManager.addVariableToBuffer(currentToken)

            accept(TokenType.IDENTIFIER)
            while (currentToken.type == TokenType.COMMA) {
                accept(TokenType.COMMA)

                if (scopeManager.findLocalIdentifier(currentToken) != null)
                    pushError(ErrorCode.DUPLICATE_IDENTIFIER)
                else
                    scopeManager.addVariableToBuffer(currentToken)

                accept(TokenType.IDENTIFIER)
            }
            checkEnd(followers)
        }
    }

    /**
     * <block> ::= <label declaration part> <constant definition part> <type definition part>
     *     <variable declaration part> <procedure and function declaration part> <statement part>
     */
    private fun block(followers: Set<TokenType>) {
        checkBeg(blockStarters, followers)

        if (currentToken.type in blockStarters) {
            // type_definition_part( followers union setOf(TokenType.VAR, TokenType.FUNCTION, TokenType.BEGIN))
            variable_declaration_part(followers union setOf(TokenType.BEGIN) )
            statement_part(followers)

            checkEnd(followers)
        }
    }

    /**
     * <type definition part> ::= <empty> | type <type definition> {;<type definition>};
     */
//    private fun type_definition_part(followers: Set<TokenType>) {
//        checkBeg(setOf(TokenType.TYPE, TokenType.VAR, TokenType.FUNCTION, TokenType.BEGIN), followers)
//
//        if (currentToken.type == TokenType.TYPE) {
//            accept(TokenType.TYPE)
//            type_definition(followers union setOf(TokenType.SEMICOLON))
//            accept(TokenType.SEMICOLON)
//            while (currentToken.type == TokenType.IDENTIFIER) {
//                type_definition(followers union setOf(TokenType.SEMICOLON))
//                accept(TokenType.SEMICOLON)
//            }
//            checkEnd(followers)
//        }
//    }

    /**
     * <type definition> ::= <identifier> = <type>
     */
//    private fun type_definition(followers: Set<TokenType>) {
//        val starters = setOf(TokenType.IDENTIFIER)
//        checkBeg(starters, followers)
//
//        if (currentToken.type in starters) {
//            val token = currentToken
//            val identifier = if (token is IdentifierToken) TypeIdentifier(token.identifier)
//            else null
//            if (identifier != null && scopeManager.addIdentifier(identifier) != null)
//                pushError(ErrorCode.DUPLICATE_IDENTIFIER)
//
//            accept(TokenType.IDENTIFIER)
//            accept(TokenType.EQUAL_OPERATOR)
//            val typedef = type(followers)
//            identifier?.type = typedef
//
//            checkEnd(followers)
//        }
//    }

    /**
     * <variable declaration part> ::= <empty> | var <variable declaration> {; <variable declaration>} ;
     */
    private fun variable_declaration_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.VAR, TokenType.BEGIN), followers)

        if (currentToken.type == TokenType.VAR) {
            accept(TokenType.VAR)
            variable_declaration(followers union setOf(TokenType.SEMICOLON))
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                variable_declaration(followers union setOf(TokenType.SEMICOLON))
                accept(TokenType.SEMICOLON)
            }

            checkEnd(followers)
        }
    }

    /**
     * <variable declaration> ::= <identifier> {,<identifier>} : <type>
     */
    private fun variable_declaration(followers: Set<TokenType>) {
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            identifier_list(followers + TokenType.COLON)
            accept(TokenType.COLON)
            val variableType = type(followers)
            scopeManager.flushVariableBuffer(variableType)
            checkEnd(followers)
        }
    }

    /**
     * <type> ::= <simple type> | <structured type> | <pointer type>
     */
    private fun type(followers: Set<TokenType>): Type? {
        return simple_type(followers)
    }

    /**
     * <simple type> ::= <scalar type> | <subrange type> | <type identifier>
     */
    private fun simple_type(followers: Set<TokenType>): Type? {
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        var resultType: Type? = null

        if (currentToken.type in starters) {
            resultType =  type_identifier()
            checkEnd(followers, ErrorCode.TYPE_IDENTIFIER_EXPECTED)
        }

        return resultType
    }

    /**
     * no neutralization needed
     * <type identifier> ::= <identifier>
     */
    private fun type_identifier(): Type? {
        val identifier = scopeManager.findIdentifier(currentToken)
        accept(TokenType.IDENTIFIER)
        return if (identifier == null || identifier !is TypeIdentifier) {
            pushError(ErrorCode.TYPE_IDENTIFIER_EXPECTED)
            null
        } else
            identifier.type
    }

    /**
     * <statement part> ::= <compound statement>
     */
    private fun statement_part(followers: Set<TokenType>) {
        compound_statement(followers)
    }

    /**
     * <compound statement> ::= begin <statement-sequence> end;
     */
    private fun compound_statement(followers: Set<TokenType>) {
        val starters = setOf(TokenType.BEGIN)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            accept(TokenType.BEGIN)
            statement_sequence(followers + TokenType.END)
            accept(TokenType.END)

            checkEnd(followers)
        }
    }

    /**
     * statement-sequence = statement { ` ;' statement } .
     */
    private fun statement_sequence(followers: Set<TokenType>) {
        val starters = simpleStatementStarters + structuredStatementStarters
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            statement(followers + TokenType.SEMICOLON)
            while (currentToken.type == TokenType.SEMICOLON) {
                accept(TokenType.SEMICOLON)
                statement(followers + TokenType.SEMICOLON)
            }
            checkEnd(followers)
        }
    }

    /**
     * <statement> ::= <unlabelled statement> | <label> : <unlabelled statement>
     */
    private fun statement(followers: Set<TokenType>) {
        unlabelled_statement(followers)
    }

    /**
     * <unlabelled statement> ::= <simple statement> | <structured statement>
     */
    private fun unlabelled_statement(followers: Set<TokenType>) {
        val starters = simpleStatementStarters + structuredStatementStarters
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            when {
                currentToken.type == TokenType.IDENTIFIER -> simple_statement(followers)
                currentToken.type in setOf(TokenType.BEGIN /*, TokenType.IF, TokenType.WHILE */) -> structured_statement(followers)
                else -> pushError(ErrorCode.IDENTIFIER_EXPECTED) // TODO: check if it's ok
            }
            checkEnd(followers)
        }
    }

    /**
     * <simple statement> ::= <assignment statement> | <procedure statement> | <go to statement> | <empty statement>
     */
    private fun simple_statement(followers: Set<TokenType>) {
        checkBeg(simpleStatementStarters, followers)

        if (currentToken.type in simpleStatementStarters) {
            val identifierName = (currentToken as IdentifierToken).identifier
            accept(TokenType.IDENTIFIER)
            when (currentToken.type) {
                TokenType.ASSIGN_OPERATOR -> assignment_statement(followers, identifierName)
                else -> pushError(ErrorCode.UNEXPECTED_SYMBOL)
            }

            checkEnd(followers)
        }
    }

    /**
     * <assignment statement> ::= <variable> := <expression> | <function identifier> := <expression>
     */
    private fun assignment_statement(followers: Set<TokenType>, identifierName: String) {
        val starters = setOf(TokenType.ASSIGN_OPERATOR)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {

            val identifier = scopeManager.findIdentifier(identifierName)

            if (identifier == null)
                pushError(ErrorCode.UNKNOWN_IDENTIFIER)
            else if (identifier !is VariableIdentifier)
                pushError(ErrorCode.VARIABLE_IDENTIFIER_EXPECTED)

            // accept(TokenType.IDENTIFIER)
            accept(TokenType.ASSIGN_OPERATOR)
            val expressionType = expression(followers)

            val identifierType = identifier?.type
            if (identifierType != null && expressionType != null && !identifierType.isCompatibleTo(expressionType))
                pushError(ErrorCode.ILLEGAL_ASSIGNMENT)

            if (identifier == null) {
                scopeManager.addIdentifier(VariableIdentifier(identifierName, expressionType))
            }

            checkEnd(followers)
        }
    }

    /**
     * <structured statement> ::= <compound statement> | <conditional statement> | <repetitive statement> | <with statement>
     */
    private fun structured_statement(followers: Set<TokenType>) {
        checkBeg(structuredStatementStarters, followers)

        if (currentToken.type in structuredStatementStarters) {
            when (currentToken.type) {
                TokenType.BEGIN -> compound_statement(followers)
                // TokenType.IF -> conditional_statement(followers)
                // TokenType.WHILE -> repetitive_statement(followers)
                else -> throw Exception("Unexpected error - here MUST be one of BEGIN, IF, WHILE, REPEAT, FOR")
            }
            checkEnd(followers)
        }
    }

    /**
     * <conditional statement> ::= <if statement> | <case statement>
     */
//    private fun conditional_statement(followers: Set<TokenType>) {
//        if_statement(followers)
//    }

    /** no before neutralization needed
     * <if statement> ::= if <expression> then <statement> | if <expression> then <statement> else <statement>
     */
//    private fun if_statement(followers: Set<TokenType>) {
//        accept(TokenType.IF)
//
//        val expressionType = expression(followers + TokenType.THEN)
//        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
//            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
//        accept(TokenType.THEN)
//        statement(followers + TokenType.ELSE)
//        if (currentToken.type == TokenType.ELSE) {
//            accept(TokenType.ELSE)
//            statement(followers)
//        }
//        checkEnd(followers)
//    }

    /**
     * <expression> ::= <simple expression> | <simple expression> <relational operator> <simple expression>
     */
    private fun expression(followers: Set<TokenType>): Type? {
        val starters = simpleExpressionStarters
        checkBeg(starters, followers)

        var resultType: Type? = null

        if (currentToken.type in starters) {
            resultType = simple_expression(followers /* + relationalOperators */)
//            if (currentToken.type in relationalOperators) {
//                val operator = currentToken.type
//                relational_operator()
//                val expType = simple_expression(followers)
//                resultType = if (resultType != null && expType != null)
//                    resultType.comparingType(expType, operator)
//                        .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
//                else null
//            }

            checkEnd(followers)
        }
        return resultType
    }

    /**
     * <simple expression> ::= <term> | <sign> <term>| <simple expression> <adding operator> <term>
     */
    private fun simple_expression(followers: Set<TokenType>): Type? {
        checkBeg(simpleExpressionStarters, followers)

        var resultType: Type? = null

        if (currentToken.type in simpleExpressionStarters) {

            if (currentToken.type in setOf(TokenType.PLUS, TokenType.MINUS)) {
                sign()
                resultType = term(followers + addingOperators)

                if (resultType != null && !resultType.isSignable())
                    pushError(ErrorCode.INTEGER_OR_REAL_EXPRESSION_EXPECTED)
            } else if (currentToken.type in setOf(
                    TokenType.IDENTIFIER, TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT,
                    TokenType.LEFT_BRACKET /*, TokenType.NOT */
                )
            )
                resultType = term(followers + addingOperators)

            while (currentToken.type in addingOperators) {
                val operator = currentToken.type
                adding_operator()
                val termType = term(followers + addingOperators)
                resultType = if (resultType != null && termType != null)
                    resultType.addingType(termType, operator)
                        .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
                else null
            }

            checkEnd(followers)
        }

        return resultType
    }

    /**
     * <adding operator> ::= + | - | or
     */
    private val addingOperators = setOf(TokenType.PLUS, TokenType.MINUS /*, TokenType.OR */)
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
    private fun term(followers: Set<TokenType>): Type? {
        checkBeg(termStarters, followers)

        var resultType: Type? = null

        if (currentToken.type in termStarters) {
            resultType = factor(followers + multiplyingOperators)
            while (currentToken.type in multiplyingOperators) {
                val operator = currentToken.type
                multiplying_operator()
                val factorType = factor(followers + multiplyingOperators)

                resultType = if (resultType != null && factorType != null)
                    resultType.multiplyingType(factorType, operator)
                        .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
                else null
            }
            checkEnd(followers)
        }
        return resultType
    }

    /**
     * <factor> ::= <variable> | <unsigned constant> | ( <expression> ) | <function designator> | <set> | not <factor>
     */
    private fun factor(followers: Set<TokenType>): Type? {
        checkBeg(factorStarters, followers)

        var resultType: Type? = null

        if (currentToken.type in factorStarters) {

            when {
                currentToken.type == TokenType.IDENTIFIER -> {
                    val identifierName = (currentToken as IdentifierToken).identifier
                    val identifier = scopeManager.findIdentifier(currentToken)
                    when (identifier) {
                        is VariableIdentifier -> resultType = variable()
                        is ConstantIdentifier -> {
                            resultType = identifier.type; currentToken = getNextSymbol()
                        }
                        else -> {
                            accept(TokenType.IDENTIFIER)

                            pushError(ErrorCode.UNKNOWN_IDENTIFIER)
                            scopeManager.addIdentifier(VariableIdentifier(identifierName))

                            resultType = null
                        }
                    }
                }
                currentToken.type in setOf(
                    TokenType.INT_CONSTANT,
                    TokenType.DOUBLE_CONSTANT,
                    TokenType.CHAR_CONSTANT
                ) -> resultType = unsigned_constant()
                currentToken.type == TokenType.LEFT_BRACKET -> {
                    accept(TokenType.LEFT_BRACKET)
                    resultType = expression(followers + TokenType.RIGHT_BRACKET)
                    accept(TokenType.RIGHT_BRACKET)
                }
//                currentToken.type == TokenType.NOT -> {
//                    accept(TokenType.NOT)
//                    resultType = factor(followers)
//                    if (resultType != null && !resultType.isLogical())
//                        pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
//                }
                else -> {
                    pushError(ErrorCode.IDENTIFIER_EXPECTED) // ?
                    resultType = null
                }
            }
            checkEnd(followers)
        }

        return resultType
    }

    /**
     * no neutralization needed
     * <variable> ::= <entire variable> | <component variable> | <referenced variable>
     */
    private fun variable(): Type? {
        val identifier = scopeManager.findIdentifier(currentToken)
        accept(TokenType.IDENTIFIER)
        return identifier?.type
    }

    /**
     * no neutralization needed
     * <unsigned constant> ::= <unsigned number> | <string> | < constant identifier> < nil>
     */
    private fun unsigned_constant(): Type? {

        val resultType: Type?
        when {
            currentToken.type == TokenType.INT_CONSTANT -> {
                accept(TokenType.INT_CONSTANT)
                resultType = ScopeManager.integerType
            }
            currentToken.type == TokenType.DOUBLE_CONSTANT -> {
                accept(TokenType.DOUBLE_CONSTANT)
                resultType = ScopeManager.realType
            }
            currentToken.type == TokenType.CHAR_CONSTANT -> {
                accept(TokenType.CHAR_CONSTANT)
                resultType = ScopeManager.charType
            }
            else -> {
                pushError(ErrorCode.CONSTANT_EXPECTED)
                resultType = null
            }
        }
        return resultType
    }

    /**
     * <multiplying operator> ::= * | / | div | mod | and
     */
    private val multiplyingOperators = setOf(TokenType.STAR, TokenType.SLASH, TokenType.DIV,
        TokenType.MOD /* , TokenType.AND */)
    private fun multiplying_operator() {
        for (opToken in multiplyingOperators)
            if (currentToken.type == opToken) {
                accept(opToken)
                return
            }
        throw Exception("Unexpected error - here MUST be one of *, /, div, mod, and")
    }

    /**
     * <relational operator> ::= = | <> | < | <= | >= | > | in
     */
//    private val relationalOperators = setOf(TokenType.EQUAL_OPERATOR, TokenType.NOT_EQUAL_OPERATOR, TokenType.LESS_OPERATOR,
//        TokenType.LESS_OR_EQUAL_OPERATOR, TokenType.GREATER_OR_EQUAL_OPERATOR, TokenType.GREATER_OPERATOR)
//    private fun relational_operator() {
//        for (opToken in relationalOperators)
//            if (currentToken.type == opToken) {
//                accept(opToken)
//                return
//            }
//        throw Exception("Unexpected error - here MUST be one of =, <>, <, <=, >=, >, in")
//    }

    /**
     * no before neutralization needed
     * <repetitive statement> ::= <while statement> | <repeat statement> | <for statement>
     */
//    private fun repetitive_statement(followers: Set<TokenType>) {
//        while_statement(followers)
//    }

    /**
     * no before neutralization needed
     * <while statement> ::= while <expression> do <statement>
     */
//    private fun while_statement(followers: Set<TokenType>) {
//        accept(TokenType.WHILE)
//        val expressionType = expression(followers + TokenType.DO)
//        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
//            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
//        accept(TokenType.DO)
//        statement(followers)
//    }

}