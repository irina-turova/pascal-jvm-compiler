package parser

import lexer.TokenType
import lexer.Token
import lexer.Lexer
import java.lang.Exception
import common.ErrorList
import common.ErrorCode
import common.Error
import lexer.IdentifierToken
import semantic.ScopeManager
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

    private fun checkEnd(followers: Set<TokenType>) {
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
                identifier_list()
                accept(TokenType.RIGHT_BRACKET)
            }
            accept(TokenType.SEMICOLON)
            checkEnd(followers)
        }
    }

    /**
     * identifier-list = identifier { `,' identifier }
     */
    private fun identifier_list() { // TODO: identifier list for procedures and other usages
        scopeManager.addVariableToBuffer(currentToken)
        accept(TokenType.IDENTIFIER)
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            scopeManager.addVariableToBuffer(currentToken)
            accept(TokenType.IDENTIFIER)
        }
    }

    /**
     * <block> ::= <label declaration part> <constant definition part> <type definition part>
     *     <variable declaration part> <procedure and function declaration part> <statement part>
     */
    val block_starters = setOf(TokenType.TYPE, TokenType.VAR, TokenType.FUNCTION, TokenType.PROCEDURE, TokenType.BEGIN)
    private fun block(followers: Set<TokenType>) {
        checkBeg(block_starters, followers)

        if (currentToken.type in block_starters) {
            type_definition_part( followers union setOf(TokenType.VAR, TokenType.PROCEDURE, TokenType.FUNCTION, TokenType.BEGIN))
            variable_declaration_part(followers union setOf(TokenType.PROCEDURE, TokenType.FUNCTION, TokenType.BEGIN) )
            procedure_and_function_declaration_part( followers union setOf(TokenType.BEGIN))
            statement_part()

            checkEnd(followers)
        }
    }

    /**
     * <procedure and function declaration part> ::= {<procedure or function declaration > ;}
     */
    private fun procedure_and_function_declaration_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.PROCEDURE, TokenType.FUNCTION), followers)

        while (currentToken.type == TokenType.PROCEDURE || currentToken.type == TokenType.FUNCTION) {
            procedure_or_function_declaration( followers union setOf(TokenType.FUNCTION, TokenType.PROCEDURE))
            accept(TokenType.SEMICOLON)
        }

        scopeManager.findLocalForwards().forEach { // TODO: Add explanation with func or proc name
            pushError(ErrorCode.UNDEFINED_FORWARD)
        }

        checkEnd(followers)
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

        scopeManager.openScope()

        accept(TokenType.PROCEDURE)

        val identifier = scopeManager.findLocalIdentifier(currentToken)

        if (identifier == null) {
            val newIdentifier = procedure_heading(followers union block_starters)
            accept(TokenType.SEMICOLON)
            currentToken.let {token ->
                if (token is IdentifierToken &&  token.identifier.toLowerCase() == "forward") {
                    accept(TokenType.IDENTIFIER)
                    newIdentifier.isForward = true
                } else
                    block(followers)
            }
        } else if (identifier is ProcedureIdentifier && identifier.isForward) {
            accept(TokenType.IDENTIFIER)
            identifier.isForward = false
            accept(TokenType.SEMICOLON)
            block(followers)
        } else {
            pushError(ErrorCode.DUPLICATE_IDENTIFIER)
            checkEnd(followers)
        }

        scopeManager.closeScope()

        checkEnd(followers)
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
            formal_parameter_list(followers)

        checkEnd(followers)

        return identifier
    }

    /**
     * formal-parameter-list = `(' formal-parameter-section { ` ;' formal-parameter-section } `)' .
     */
    private fun formal_parameter_list(followers: Set<TokenType>) {
        accept(TokenType.LEFT_BRACKET)
        formal_parameters_section(followers)
        while (currentToken.type == TokenType.SEMICOLON) {
            accept(TokenType.SEMICOLON)
            formal_parameters_section(followers)
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

        scopeManager.openScope()

        accept(TokenType.FUNCTION)

        val identifier = scopeManager.findLocalIdentifier(currentToken)

        if (identifier == null) {
            val newIdentifier = function_heading(followers union block_starters)
            accept(TokenType.SEMICOLON)
            currentToken.let {token ->
                if (token is IdentifierToken &&  token.identifier.toLowerCase() == "forward") {
                    accept(TokenType.IDENTIFIER)
                    newIdentifier.isForward = true
                } else
                    block(followers)
            }
        } else if (identifier is FunctionIdentifier && identifier.isForward) {
            accept(TokenType.IDENTIFIER)
            identifier.isForward = false
            accept(TokenType.SEMICOLON)
            block(followers)
        } else {
            pushError(ErrorCode.DUPLICATE_IDENTIFIER)
            checkEnd(followers)
        }

        scopeManager.closeScope()

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
     * <formal parameter section> ::= <parameter group> | var <parameter group>
     *     | function <parameter group> | procedure <identifier> { , <identifier>}
     */
    private fun formal_parameters_section(followers: Set<TokenType>) {
        when(currentToken.type) {
            TokenType.IDENTIFIER -> value_parameter_section()
            TokenType.VAR -> variable_parameter_section()
            TokenType.FUNCTION -> function_heading(followers)
            TokenType.PROCEDURE -> procedure_heading(followers)
            else -> pushError(ErrorCode.UNEXPECTED_SYMBOL)
        }
        // checkEnd(followers)
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
    private fun type_definition_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.TYPE), followers)

        if (currentToken.type == TokenType.TYPE) {
            accept(TokenType.TYPE)
            type_definition()
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                type_definition()
                accept(TokenType.SEMICOLON)
            }
            checkEnd(followers)
        }
    }

    /**
     * <type definition> ::= <identifier> = <type>
     */
    private fun type_definition() {
        val token = currentToken
        val identifier = if (token is IdentifierToken) TypeIdentifier(token.identifier)
            else null
        if (identifier != null && scopeManager.addIdentifier(identifier) != null)
            pushError(ErrorCode.DUPLICATE_IDENTIFIER)

        accept(TokenType.IDENTIFIER)
        accept(TokenType.EQUAL_OPERATOR)
        val typedef = type()
        identifier?.type = typedef
    }

    /**
     * <variable declaration part> ::= <empty> | var <variable declaration> {; <variable declaration>} ;
     */
    private fun variable_declaration_part(followers: Set<TokenType>) {
        checkBeg(setOf(TokenType.VAR), followers)

        if (currentToken.type == TokenType.VAR) {
            accept(TokenType.VAR)
            variable_declaration()
            accept(TokenType.SEMICOLON)
            while (currentToken.type == TokenType.IDENTIFIER) {
                variable_declaration()
                accept(TokenType.SEMICOLON)
            }

            checkEnd(followers)
        }
    }

    /**
     * <variable declaration> ::= <identifier> {,<identifier>} : <type>
     */
    private fun variable_declaration() {
        identifier_list()
        accept(TokenType.COLON)
        val variableType = type()
        scopeManager.flushVariableBuffer(variableType)
    }

    /**
     * <type> ::= <simple type> | <structured type> | <pointer type>
     */
    private fun type(): Type? {
        return simple_type()
    }

    /**
     * <simple type> ::= <scalar type> | <subrange type> | <type identifier>
     */
    private fun simple_type(): Type? {
        if (currentToken.type == TokenType.IDENTIFIER)
            return type_identifier()
        else {
            pushError(ErrorCode.TYPE_IDENTIFIER_EXPECTED)
            return null
        }
    }

    /**
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
    private fun statement_part() {
        compound_statement()
    }

    /**
     * <compound statement> ::= begin <statement-sequence> end;
     */
    private fun compound_statement() {
        accept(TokenType.BEGIN)

        statement_sequence()

        accept(TokenType.END)
    }

    /**
     * statement-sequence = statement { ` ;' statement } .
     */
    private fun statement_sequence() {
        statement()
        while (currentToken.type == TokenType.SEMICOLON) {
            accept(TokenType.SEMICOLON)
            statement()
        }
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
            currentToken.type in setOf(TokenType.BEGIN, TokenType.IF, TokenType.WHILE, TokenType.REPEAT, TokenType.FOR) -> structured_statement()
            else -> pushError(ErrorCode.IDENTIFIER_EXPECTED) // TODO: check if it's ok
        }
    }

    /**
     * <simple statement> ::= <assignment statement> | <procedure statement> | <go to statement> | <empty statement>
     */
    private fun simple_statement() {
        if (currentToken.type == TokenType.IDENTIFIER) {
            val identifier = scopeManager.findIdentifier(currentToken)
            if (identifier is VariableIdentifier)
                assignment_statement()
            else if (identifier is ProcedureIdentifier)
                procedure_statement()
            // TODO: do we need also function-designator here?
        }
    }

    /**
     * procedure-statement = procedure-identifier ( [ actual-parameter-list ]
     * | read-parameter-list | readln-parameter-list
     * | write-parameter-list | writeln-parameter-list ) .
     */
    private fun procedure_statement() {
        accept(TokenType.IDENTIFIER)
        actual_parameter_list()
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
        when(currentToken.type) {
            TokenType.BEGIN -> compound_statement()
            TokenType.IF -> conditional_statement()
            TokenType.WHILE, TokenType.REPEAT, TokenType.FOR -> repetitive_statement()
            else -> throw Exception("Unexpected error - here MUST be one of BEGIN, IF, WHILE, REPEAT, FOR")
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
        val expressionType = expression()
        if (expressionType != null && !expressionType.isCompatibleTo(ScopeManager.booleanType))
            pushError(ErrorCode.BOOLEAN_EXPRESSION_EXPECTED)
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
    private fun expression(): Type? {
        var resultType = simple_expression()
        if (currentToken.type in relationalOperators) {
            val operator = currentToken.type
            relational_operator()
            val expType = simple_expression()
            resultType = if (resultType != null && expType != null)
                resultType.comparingType(expType, operator)
                    .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
            else null
        }
        return resultType
    }

    /**
     * <simple expression> ::= <term> | <sign> <term>| <simple expression> <adding operator> <term>
     */
    private fun simple_expression(): Type? {

        var resultType: Type?

        if (currentToken.type in setOf(TokenType.PLUS, TokenType.MINUS)) {
            sign()
            resultType = term()
            if (resultType != null && !resultType.isSignable())
                pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR)
        } else if (currentToken.type in setOf(TokenType.IDENTIFIER, TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT,
                TokenType.LEFT_BRACKET, TokenType.NOT))
            resultType = term()
        else
            resultType = null
        while (currentToken.type in setOf(TokenType.PLUS, TokenType.MINUS, TokenType.OR)) {
            val operator = currentToken.type
            adding_operator()
            val termType = term()
            resultType = if (resultType != null && termType != null)
                resultType.addingType(termType, operator)
                    .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
            else null
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
    private fun term(): Type? {
        var resultType = factor()
        while (currentToken.type in multiplyingOperators) {
            val operator = currentToken.type
            multiplying_operator()
            val factorType = factor()
            resultType = if (resultType != null && factorType != null)
                resultType.multiplyingType(factorType, operator)
                    .also { if (it == null) pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR) }
            else null
        }
        return resultType
    }

    /**
     * <factor> ::= <variable> | <unsigned constant> | ( <expression> ) | <function designator> | <set> | not <factor>
     */
    private fun factor(): Type? {
        val resultType: Type?

        when {
            currentToken.type == TokenType.IDENTIFIER -> {
                val identifier = scopeManager.findIdentifier(currentToken)
                when (identifier) {
                    is VariableIdentifier -> resultType = variable()
                    is ConstantIdentifier -> {resultType = identifier.type; currentToken = getNextSymbol()}
                    is FunctionIdentifier -> resultType = function_designator()
                    null -> resultType = null // TODO: Standard functions or error
                    else -> resultType = null // TODO: тоже какая-то ошибка, например, если это процедура или тип...
                }
            }
            currentToken.type in setOf(TokenType.INT_CONSTANT, TokenType.DOUBLE_CONSTANT, TokenType.CHAR_CONSTANT) -> resultType = unsingned_constant()
            currentToken.type == TokenType.LEFT_BRACKET -> {
                accept(TokenType.LEFT_BRACKET)
                resultType = expression()
                accept(TokenType.RIGHT_BRACKET)
            }
            currentToken.type == TokenType.NOT -> {
                accept(TokenType.NOT)
                resultType = factor()
                if (resultType != null && !resultType.isLogical())
                    pushError(ErrorCode.OPERAND_TYPES_DO_NOT_MATCH_OPERATOR)
            }
            else -> {
                pushError(ErrorCode.VARIABLE_IDENTIFIER_EXPECTED) // ?
                resultType = null
            }
        }

        return resultType
    }

    /**
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
     * actual-parameter-list = `(' actual-parameter { `,' actual-parameter } `)' .
     */
    private fun actual_parameter_list() {
        accept(TokenType.LEFT_BRACKET)
        actual_parameter()
        while (currentToken.type == TokenType.COMMA) {
            accept(TokenType.COMMA)
            actual_parameter()
        }
        accept(TokenType.RIGHT_BRACKET)
    }

    /**
     * actual-parameter = expression | variable-access | procedure-identifier | function-identifier .
     */
    private fun actual_parameter() { // TODO: need parameter info passing..

    }

    /**
     * <variable> ::= <entire variable> | <component variable> | <referenced variable>
     */
    private fun variable(): Type? {
        val identifier = scopeManager.findIdentifier(currentToken)
        accept(TokenType.IDENTIFIER)
        return identifier?.type
    }

    /**
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
     * <repetitive statement> ::= <while statement> | <repeat statemant> | <for statement>
     */
    private fun repetitive_statement() {
        when (currentToken.type) {
            TokenType.WHILE -> while_statement()
            TokenType.REPEAT -> repeat_statement()
            TokenType.FOR -> for_statement()
            else -> throw Exception("Unexpected error - here MUST be one of WHILE, REPEAT, FOR")
        }
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

    /**
     * repeat-statement = `repeat' statement-sequence `until' Boolean-expression
     */
    private fun repeat_statement() {
        accept(TokenType.REPEAT)
        statement_sequence()
        accept(TokenType.UNTIL)
        expression()
    }

    /**
     * for-statement = `for' control-variable ` :=' initial-value ( `to' | `downto' ) final-value `do' statement .
     * control-variable = entire-variable .
     * initial-value = expression .
     * final-value = expression .
     */
    private fun for_statement() {
        accept(TokenType.FOR)
        variable()
        accept(TokenType.ASSIGN_OPERATOR)
        expression()
        when (currentToken.type) {
            TokenType.TO -> accept(TokenType.TO)
            TokenType.DOWN_TO -> accept(TokenType.DOWN_TO)
            else -> pushError(ErrorCode.TO_OR_DOWNTO_EXPECTED)
        }
        expression()
        accept(TokenType.DO)
        statement()
    }
}