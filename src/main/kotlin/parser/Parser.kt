package parser

import lexer.TokenType
import lexer.Token
import lexer.Lexer
import java.lang.Exception
import common.ErrorList
import common.ErrorCode
import common.Error
import lexer.IdentifierToken
import semantic.Parameter
import semantic.ScopeManager
import semantic.SimpleParameter
import semantic.TransmissionMode
import semantic.types.Type
import semantic.identifiers.*

class Parser(val lexer: Lexer, val errors: ErrorList, val scopeManager: ScopeManager) {

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
        val united = tokens.fold(setOf(TokenType.THIS_IS_THE_END)) { a, b -> a union b}
        while (currentToken.type !in united) {
            println("Skipping $currentToken")
            currentToken = getNextSymbol()
        }
    }

    private fun checkBeg(starters: Set<TokenType>, followers: Set<TokenType>) {
        if (currentToken.type !in (starters union followers)) {
            pushError(ErrorCode.UNEXPECTED_SYMBOL)
            skipTo(starters, followers)
        }
    }

    private fun checkEnd(followers: Set<TokenType>, error: ErrorCode? = null) {
        if (currentToken.type !in followers) {
            pushError(ErrorCode.UNEXPECTED_SYMBOL)
            skipTo(followers)
        }
    }

    fun parse() {
        program(setOf(TokenType.THIS_IS_THE_END))
    }

    private fun accept(expectedToken: TokenType) {
        if (expectedToken == currentToken.type)
            currentToken = getNextSymbol()
        else
            pushError(ErrorCode.fromExpectedToken(expectedToken))
    }

    private val block_starters = setOf(TokenType.TYPE, TokenType.VAR, TokenType.FUNCTION, TokenType.PROCEDURE, TokenType.BEGIN)
    private val structuredStatementStarters = setOf(TokenType.BEGIN, TokenType.IF, TokenType.WHILE, TokenType.REPEAT, TokenType.FOR)
    private val simpleStatementStarters = setOf(TokenType.IDENTIFIER)
    private val unsignedConstantStarters = setOf(TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT)
    private val factorStarters = setOf(TokenType.IDENTIFIER, TokenType.LEFT_BRACKET, TokenType.NOT) + unsignedConstantStarters
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
            program_heading(followers union block_starters )
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
                accept(TokenType.RIGHT_BRACKET)
            }
            accept(TokenType.SEMICOLON)
            checkEnd(followers)
        }
    }

    /**
     * identifier-list = identifier { `,' identifier }
     */
    private fun identifier_list(followers: Set<TokenType>) { // TODO: identifier list for procedures and other usages
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            scopeManager.addVariableToBuffer(currentToken)
            accept(TokenType.IDENTIFIER)
            while (currentToken.type == TokenType.COMMA) {
                accept(TokenType.COMMA)
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
        checkBeg(block_starters, followers)

        if (currentToken.type in block_starters) {
            type_definition_part( followers union setOf(TokenType.VAR, TokenType.PROCEDURE, TokenType.FUNCTION, TokenType.BEGIN))
            variable_declaration_part(followers union setOf(TokenType.PROCEDURE, TokenType.FUNCTION, TokenType.BEGIN) )
            procedure_and_function_declaration_part( followers union setOf(TokenType.BEGIN))
            statement_part(followers)

            checkEnd(followers)
        }
    }

    /**
     * <procedure and function declaration part> ::= {<procedure or function declaration > ;}
     */
    private fun procedure_and_function_declaration_part(followers: Set<TokenType>) {
        val starters = setOf(TokenType.PROCEDURE, TokenType.FUNCTION)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            while (currentToken.type == TokenType.PROCEDURE || currentToken.type == TokenType.FUNCTION) {
                procedure_or_function_declaration(followers + TokenType.SEMICOLON)
                accept(TokenType.SEMICOLON)
            }

            scopeManager.findLocalForwards().forEach {
                // TODO: Add explanation with func or proc name
                pushError(ErrorCode.UNDEFINED_FORWARD)
            }

            checkEnd(followers)
        }
    }

    /**
     * <procedure or function declaration > ::= <procedure declaration> | <function declaration >
     */
    private fun procedure_or_function_declaration(followers: Set<TokenType>) {
        if (currentToken.type == TokenType.PROCEDURE) procedure_declaration(followers)
        else if (currentToken.type == TokenType.FUNCTION) function_declaration(followers)

        checkEnd(followers)
    }

    /**
     * procedure-declaration = procedure-heading ` ;' directive
     * | procedure-identification ` ;' procedure-block
     * | procedure-heading ` ;' procedure-block .
     *
     * we change to:
     * procedure-declaration = `procedure` ( procedure-heading ` ;' directive
     *      | procedure-identification ` ;' procedure-block
     *      | procedure-heading ` ;' procedure-block . )
     */
    private fun procedure_declaration(followers: Set<TokenType>) {
        val starters = setOf(TokenType.PROCEDURE)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {

            accept(TokenType.PROCEDURE)

            val identifier = scopeManager.findLocalIdentifier(currentToken)

            if (identifier == null) {
                val newIdentifier = procedure_heading(followers + TokenType.SEMICOLON)
                    .also { scopeManager.addIdentifier(it) }
                accept(TokenType.SEMICOLON)
                currentToken.let { token ->
                    if (token is IdentifierToken && token.identifier.toLowerCase() == "forward") {
                        accept(TokenType.IDENTIFIER)
                        newIdentifier.isForward = true
                    } else {
                        scopeManager.openScope()
                        // TODO: add procedure parameters to local scope
                        block(followers)
                        scopeManager.closeScope()
                    }
                }
            } else if (identifier is ProcedureIdentifier && identifier.isForward) {
                accept(TokenType.IDENTIFIER)
                identifier.isForward = false
                accept(TokenType.SEMICOLON)
                scopeManager.openScope()
                // TODO: add procedure parameters to local scope
                block(followers)
                scopeManager.closeScope()
            } else {
                pushError(ErrorCode.DUPLICATE_IDENTIFIER)
                checkEnd(followers)
            }

            checkEnd(followers)
        }
    }

    /**
     * <procedure heading> ::= procedure <identifier> [ formal-parameter-list ]
     *
     *  we change to
     *  <procedure heading> ::= <identifier> [ formal-parameter-list ]
     */
    private fun procedure_heading(followers: Set<TokenType>): ProcedureIdentifier {
        val identifier = ProcedureIdentifier((currentToken as IdentifierToken).identifier)
        scopeManager.addIdentifier(identifier)

        accept(TokenType.IDENTIFIER)

        if (currentToken.type == TokenType.LEFT_BRACKET)
            identifier.parameters = formal_parameter_list(followers)

        checkEnd(followers)

        return identifier
    }

    /**
     * formal-parameter-list = `(' formal-parameter-section { ` ;' formal-parameter-section } `)' .
     */
    private fun formal_parameter_list(followers: Set<TokenType>): List<Parameter> {

        val parameters = mutableListOf<Parameter>()

        accept(TokenType.LEFT_BRACKET)
        formal_parameters_section(followers)
        while (currentToken.type == TokenType.SEMICOLON) {
            accept(TokenType.SEMICOLON)
            parameters.add(formal_parameters_section(followers))
        }
        accept(TokenType.RIGHT_BRACKET)
    }

    /**
     * function-declaration = function-heading ` ;' directive | function-identification ` ;' function-block
     *      | function-heading ` ;' function-block .
     *      
     * we change to:
     * function-declaration = `function` (function-heading ` ;' directive | function-identification ` ;' function-block
     *      | function-heading ` ;' function-block )
     */
    private fun function_declaration(followers: Set<TokenType>) {

        accept(TokenType.FUNCTION)

        val identifier = scopeManager.findLocalIdentifier(currentToken)

        if (identifier == null) {
            val newIdentifier = function_heading(followers union block_starters)
                .also { scopeManager.addIdentifier(it) }
            accept(TokenType.SEMICOLON)
            currentToken.let {token ->
                if (token is IdentifierToken &&  token.identifier.toLowerCase() == "forward") {
                    accept(TokenType.IDENTIFIER)
                    newIdentifier.isForward = true
                } else {
                    scopeManager.openScope()
                    // TODO: add procedure parameters to local scope
                    block(followers)
                    scopeManager.closeScope()
                }
            }
        } else if (identifier is FunctionIdentifier && identifier.isForward) {
            accept(TokenType.IDENTIFIER)
            identifier.isForward = false
            accept(TokenType.SEMICOLON)
            scopeManager.openScope()
            // TODO: add procedure parameters to local scope
            block(followers)
            scopeManager.closeScope()
        } else {
            pushError(ErrorCode.DUPLICATE_IDENTIFIER)
            checkEnd(followers)
        }

        checkEnd(followers)
    }

    /**
     * function-heading = `function' identifier [ formal-parameter-list ] ':' result-type .
     * 
     * we change to:
     * function-heading = identifier [ formal-parameter-list ] ':' result-type .
     */
    private fun function_heading(followers: Set<TokenType>): FunctionIdentifier {
        val identifier = FunctionIdentifier((currentToken as IdentifierToken).identifier)
        scopeManager.addIdentifier(identifier)

        accept(TokenType.IDENTIFIER)

        if (currentToken.type == TokenType.LEFT_BRACKET)
            formal_parameter_list(followers)

        accept(TokenType.COLON)
        identifier.resultType = type_identifier()

        checkEnd(followers)

        return identifier
    }

    /**
     * formal-parameter-section > value-parameter-specification
     * | variable-parameter-specification
     * | procedural-parameter-specification
     * | functional-parameter-specification .
     */
    private fun formal_parameters_section(followers: Set<TokenType>): List<Parameter?> {
        return when(currentToken.type) {
            TokenType.IDENTIFIER -> value_parameter_section()
            TokenType.VAR -> variable_parameter_section()
            TokenType.FUNCTION -> listOf(functional_parameter_specification(followers))
            TokenType.PROCEDURE -> listOf(procedural_parameter_specification(followers))
            else -> {pushError(ErrorCode.UNEXPECTED_SYMBOL); listOf()}
        }
        // checkEnd(followers)
    }

    /**
     * no before neutralization needed
     * procedural-parameter-specification = procedure-heading .
     *
     * procedure-heading = `procedure' identifier [ formal-parameter-list ]
     */
    private fun procedural_parameter_specification(followers: Set<TokenType>): Parameter? {

        accept(TokenType.PROCEDURE)

        val identifier = (currentToken as? IdentifierToken)?.let {
            ProcedureIdentifier(it.identifier).also {
                scopeManager.addIdentifier(it)
            }
        }

        if (currentToken.type == TokenType.LEFT_BRACKET)
        identifier?.parameters = formal_parameter_list(followers)

        accept(TokenType.IDENTIFIER)
        checkEnd(followers)
    }

    private fun functional_parameter_specification(followers: Set<TokenType>): Parameter? {

        accept(TokenType.FUNCTION)

        val identifier = (currentToken as? IdentifierToken)?.let {
            FunctionIdentifier(it.identifier).also {
                scopeManager.addIdentifier(it)
            }
        }

        accept(TokenType.IDENTIFIER)

        if (currentToken.type == TokenType.LEFT_BRACKET)
            identifier?.parameters = formal_parameter_list(followers)

        accept(TokenType.COLON)
        identifier?.resultType = type_identifier()

        checkEnd(followers)
    }

    /**
     * variable-parameter-section = var identifier-list ":" parameter-type
     */
    private fun variable_parameter_section(): List<SimpleParameter> {
        accept(TokenType.VAR)
        return parameter_group().apply { forEach { it.mode = TransmissionMode.VARIABLE } }
    }

    /**
     * value-parameter-section = identifier-list ":" parameter-type
     */
    private fun value_parameter_section(): List<SimpleParameter> {
        return parameter_group()
    }

    /**
     * <parameter group> ::= <identifier> {, <identifier>} : <type identifier>
     */
    private fun parameter_group(): List<SimpleParameter> {

        val parameters = mutableListOf<SimpleParameter>()

        parameters.add(SimpleParameter((currentToken as? IdentifierToken)?.identifier ?: "", null, TransmissionMode.VALUE))
        if (currentToken is IdentifierToken)
        accept(TokenType.IDENTIFIER)
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            accept(TokenType.IDENTIFIER)
        }
        accept(TokenType.COLON)
        val parametersType = type_identifier()
        parameters.forEach {
            it.type = parametersType
        }
        return parameters
    }

    /**
     * <type definition part> ::= <empty> | type <type definition> {;<type definition>};
     */
    private fun type_definition_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.TYPE), followers)

        if (currentToken.type == TokenType.TYPE) {
            accept(TokenType.TYPE)
            type_definition(followers union setOf(TokenType.SEMICOLON))
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                type_definition(followers union setOf(TokenType.SEMICOLON))
                accept(TokenType.SEMICOLON)
            }
            checkEnd(followers)
        }
    }

    /**
     * <type definition> ::= <identifier> = <type>
     */
    private fun type_definition(followers: Set<TokenType>) {
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {
            val token = currentToken
            val identifier = if (token is IdentifierToken) TypeIdentifier(token.identifier)
            else null
            if (identifier != null && scopeManager.addIdentifier(identifier) != null)
                pushError(ErrorCode.DUPLICATE_IDENTIFIER)

            accept(TokenType.IDENTIFIER)
            accept(TokenType.EQUAL_OPERATOR)
            val typedef = type(followers)
            identifier?.type = typedef

            checkEnd(followers)
        }
    }

    /**
     * <variable declaration part> ::= <empty> | var <variable declaration> {; <variable declaration>} ;
     */
    private fun variable_declaration_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.VAR), followers)

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
        if (identifier == null || identifier !is TypeIdentifier) {
            pushError(ErrorCode.TYPE_IDENTIFIER_EXPECTED)
            return null
        } else
            return identifier.type
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
                currentToken.type in setOf(
                    TokenType.BEGIN,
                    TokenType.IF,
                    TokenType.WHILE,
                    TokenType.REPEAT,
                    TokenType.FOR
                ) -> structured_statement(followers)
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
            val identifier = scopeManager.findIdentifier(currentToken)
            if (identifier is VariableIdentifier)
                assignment_statement(followers)
            else if (identifier is ProcedureIdentifier)
                procedure_statement(followers)
            else if (identifier is FunctionIdentifier) {
                function_designator()
            } else
                pushError(ErrorCode.IDENTIFIER_EXPECTED)

            checkEnd(followers)
        }
    }

    /**
     * procedure-statement = procedure-identifier ( [ actual-parameter-list ]
     * | read-parameter-list | readln-parameter-list
     * | write-parameter-list | writeln-parameter-list ) .
     */
    private fun procedure_statement(followers: Set<TokenType>) {

        val identifier = scopeManager.findIdentifier(currentToken) as ProcedureIdentifier

        accept(TokenType.IDENTIFIER)

        actual_parameter_list(identifier.parameters)

        checkEnd(followers)
    }

    /**
     * <assignment statement> ::= <variable> := <expression> | <function identifier> := <expression>
     */
    private fun assignment_statement(followers: Set<TokenType>) {
        val starters = setOf(TokenType.IDENTIFIER)
        checkBeg(starters, followers)

        if (currentToken.type in starters) {

            val identifier = scopeManager.findIdentifier(currentToken)

            if (identifier == null)
                pushError(ErrorCode.UNKNOWN_IDENTIFIER)
            else if (identifier !is VariableIdentifier)
                pushError(ErrorCode.VARIABLE_IDENTIFIER_EXPECTED)

            accept(TokenType.IDENTIFIER)
            accept(TokenType.ASSIGN_OPERATOR)
            val expressionType = expression(followers)

            val identifierType = identifier?.type
            if (identifierType != null && expressionType != null && !identifierType.isCompatibleTo(expressionType))
                pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR)

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
                TokenType.IF -> conditional_statement(followers)
                TokenType.WHILE, TokenType.REPEAT, TokenType.FOR -> repetitive_statement(followers)
                else -> throw Exception("Unexpected error - here MUST be one of BEGIN, IF, WHILE, REPEAT, FOR")
            }
            checkEnd(followers)
        }
    }

    /**
     * <conditional statement> ::= <if statement> | <case statement>
     */
    private fun conditional_statement(followers: Set<TokenType>) {
        if_statement(followers)
    }

    /** no before neutralization needed
     * <if statement> ::= if <expression> then <statement> | if <expression> then <statement> else <statement>
     */
    private fun if_statement(followers: Set<TokenType>) {
        accept(TokenType.IF)

        val expressionType = expression(followers + TokenType.THEN)
        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
        accept(TokenType.THEN)
        statement(followers + TokenType.ELSE)
        if (currentToken.type == TokenType.ELSE) {
            accept(TokenType.ELSE)
            statement(followers)
        }
        checkEnd(followers)
    }

    /**
     * <expression> ::= <simple expression> | <simple expression> <relational operator> <simple expression>
     */
    private fun expression(followers: Set<TokenType>): Type? {
        val starters = simpleExpressionStarters
        checkBeg(starters, followers)

        var resultType: Type? = null

        if (currentToken.type in starters) {
            resultType = simple_expression(followers + relationalOperators)
            if (currentToken.type in relationalOperators) {
                val operator = currentToken.type
                relational_operator()
                val expType = simple_expression(followers)
                resultType = if (resultType != null && expType != null)
                    resultType.comparingType(expType, operator)
                        .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
                else null
            }

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
                    pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR)
            } else if (currentToken.type in setOf(
                    TokenType.IDENTIFIER, TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT,
                    TokenType.LEFT_BRACKET, TokenType.NOT
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
                    val identifier = scopeManager.findIdentifier(currentToken)
                    when (identifier) {
                        is VariableIdentifier -> resultType = variable()
                        is ConstantIdentifier -> {
                            resultType = identifier.type; currentToken = getNextSymbol()
                        }
                        is FunctionIdentifier -> resultType = function_designator()
                        null -> resultType = null // TODO: Standard functions or error
                        else -> resultType = null // TODO: тоже какая-то ошибка, например, если это процедура или тип...
                    }
                }
                currentToken.type in setOf(
                    TokenType.INT_CONSTANT,
                    TokenType.DOUBLE_CONSTANT,
                    TokenType.CHAR_CONSTANT
                ) -> resultType = unsingned_constant()
                currentToken.type == TokenType.LEFT_BRACKET -> {
                    accept(TokenType.LEFT_BRACKET)
                    resultType = expression(followers + TokenType.RIGHT_BRACKET)
                    accept(TokenType.RIGHT_BRACKET)
                }
                currentToken.type == TokenType.NOT -> {
                    accept(TokenType.NOT)
                    resultType = factor(followers)
                    if (resultType != null && !resultType.isLogical())
                        pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR)
                }
                else -> {
                    pushError(ErrorCode.VARIABLE_IDENTIFIER_EXPECTED) // ?
                    resultType = null
                }
            }
            checkEnd(followers)
        }

        return resultType
    }

    /**
     * no neutralization needed
     * function-designator = function-identifier [ actual-parameter-list ] .
     */
    private fun function_designator(): Type? {
        val resultType = scopeManager.findIdentifier(currentToken)?.let { if (it is FunctionIdentifier) it.resultType else null }
        accept(TokenType.IDENTIFIER)
        if (currentToken.type == TokenType.LEFT_BRACKET)
            actual_parameter_list() // TODO: check parameters list types
        return resultType
    }

    /**
     * no neutralization needed
     * actual-parameter-list = `(' actual-parameter { `,' actual-parameter } `)' .
     */
    private fun actual_parameter_list(requiredParameters: List<Parameter>) {

        val neededParameters = requiredParameters.toMutableList()

        accept(TokenType.LEFT_BRACKET)
        actual_parameter(neededParameters.firstOrNull())
        neededParameters.removeAt(0)

        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            actual_parameter(neededParameters.firstOrNull())
            neededParameters.removeAt(0)
        }
        accept(TokenType.RIGHT_BRACKET)
    }

    /**
     * no neutralization needed
     * actual-parameter = expression | variable-access | procedure-identifier | function-identifier .
     */
    private fun actual_parameter(neededParameter: Parameter?) { // TODO: need parameter info passing..
        when (neededParameter) {

        }
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
    private fun unsingned_constant(): Type? {

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
     * no before neutralization needed
     * <repetitive statement> ::= <while statement> | <repeat statemant> | <for statement>
     */
    private fun repetitive_statement(followers: Set<TokenType>) {
        when (currentToken.type) {
            TokenType.WHILE -> while_statement(followers)
            TokenType.REPEAT -> repeat_statement(followers)
            TokenType.FOR -> for_statement(followers)
            else -> throw Exception("Unexpected error - here MUST be one of WHILE, REPEAT, FOR")
        }
        checkEnd(followers)
    }

    /**
     * no before neutralization needed
     * <while statement> ::= while <expression> do <statement>
     */
    private fun while_statement(followers: Set<TokenType>) {
        accept(TokenType.WHILE)
        val expressionType = expression(followers + TokenType.DO)
        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
        accept(TokenType.DO)
        statement(followers)
    }

    /**
     * no before neutralization needed
     * repeat-statement = `repeat' statement-sequence `until' Boolean-expression
     */
    private fun repeat_statement(followers: Set<TokenType>) {
        accept(TokenType.REPEAT)
        statement_sequence(followers + TokenType.UNTIL)
        accept(TokenType.UNTIL)
        val expressionType = expression(followers)

        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
    }

    /**
     * no before neutralization needed
     * for-statement = `for' control-variable ` :=' initial-value ( `to' | `downto' ) final-value `do' statement .
     * control-variable = entire-variable .
     * initial-value = expression .
     * final-value = expression .
     */
    private fun for_statement(followers: Set<TokenType>) {
        accept(TokenType.FOR)
        val forVariableType = variable()
        accept(TokenType.ASSIGN_OPERATOR)
        val initialValueType = expression(followers + TokenType.TO + TokenType.DOWN_TO)

        if (forVariableType != null && initialValueType != null && !forVariableType.isCompatibleTo(initialValueType))
            pushError(ErrorCode.INVALID_FOR_CONTROL_VARIABLE)

        when (currentToken.type) {
            TokenType.TO -> accept(TokenType.TO)
            TokenType.DOWN_TO -> accept(TokenType.DOWN_TO)
            else -> pushError(ErrorCode.TO_OR_DOWNTO_EXPECTED)
        }
        val finalValueType = expression(followers + TokenType.DO)

        if (forVariableType != null && finalValueType != null && !forVariableType.isCompatibleTo(finalValueType))
            pushError(ErrorCode.INVALID_FOR_CONTROL_VARIABLE)

        accept(TokenType.DO)
        statement(followers)
    }
}