package semantic

import lexer.IdentifierToken
import lexer.Token
import semantic.identifiers.*
import semantic.types.*
import java.util.*

class ScopeManager {

    private val scopes = Stack<Scope>()


    init {
        // add fictive scope
        scopes.push(Scope())

        addType(programType)
        addType(programParameterType)

        addType(integerType)
        addIdentifier(TypeIdentifier("integer", integerType))

        addType(booleanType)
        addIdentifier(TypeIdentifier("boolean", booleanType))
        addIdentifier(ConstantIdentifier("false", booleanType))
        addIdentifier(ConstantIdentifier("true", booleanType))

        addType(realType)
        addIdentifier(TypeIdentifier("real", realType))

        addType(charType)
        addIdentifier(TypeIdentifier("char", charType))
    }

    fun openScope(function: FunctionIdentifier? = null) {

        scopes.push(Scope())

        function?.parameters?.filterNotNull()?.forEach { param ->
            addIdentifier(VariableIdentifier(param.name, param.type))
        }

        function?.let {
            addIdentifier(VariableIdentifier(it.name, it.resultType))
        }

    }


    private val variablesBuffer = mutableListOf<Identifier>()

    fun addVariableToBuffer(token: Token) {
        if (token is IdentifierToken)
            VariableIdentifier(token.identifier).let {
                variablesBuffer.add(it)
                addIdentifier(it)
            }
    }

    fun flushVariableBuffer(type: Type?) {
        variablesBuffer.forEach { it.type = type }
        variablesBuffer.clear()
    }

    fun getBufferedVariables(): List<Identifier> {
        return variablesBuffer.toList()
    }

    fun findLocalIdentifier(token: Token): Identifier? {
        if (token !is IdentifierToken)
            return null
        scopes.peek().findIdentifier(token.identifier)?.let {
            return it
        }

        return null
    }

    fun findLocalForwards(): List<FunctionIdentifier> {
        return scopes.peek().findForwards()
    }

    fun findIdentifier(token: Token): Identifier? {
        if (token !is IdentifierToken)
            return null
        for (scope in scopes.elements().toList().reversed()) {
            scope.findIdentifier(token.identifier)?.let {
                return it
            }
        }

        return null
    }

    fun findIdentifier(identifierName: String): Identifier? {
        for (scope in scopes.elements().toList().reversed()) {
            scope.findIdentifier(identifierName)?.let {
                return it
            }
        }
        return null
    }

    fun addIdentifier(identifier: Identifier): Identifier? {
        scopes.peek().findIdentifier(identifier.name).let {
            if (it == null)
                scopes.peek().addIdentifier(identifier)
            return it
        }
    }

    private fun addType(type: Type) {
        scopes.peek().addType(type)
    }

    fun closeScope() {
        scopes.pop()
    }

    companion object {
        val programType = ProgramType()
        val programParameterType = ProgramParameterType()
        val integerType = ScalarType("I")
        val booleanType = EnumType(listOf("false", "true"), "I")
        val realType = ScalarType("F")
        val charType = ScalarType("C") // ?
    }
}