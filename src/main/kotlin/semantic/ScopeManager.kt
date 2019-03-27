package semantic

import lexer.IdentifierToken
import lexer.Token
import semantic.identifiers.ExecutableIdentifier
import semantic.identifiers.Identifier
import semantic.identifiers.TypeIdentifier
import semantic.identifiers.VariableIdentifier
import semantic.types.ProgramType
import semantic.types.ScalarType
import semantic.types.Type
import java.util.*

class ScopeManager {

    private val scopes = Stack<Scope>()


    init {
        // add fictive scope
        scopes.push(Scope())

        addType(programType)
        addType(integerType)
        addIdentifier(TypeIdentifier("integer", integerType))
        addType(booleanType)
        addIdentifier(TypeIdentifier("boolean", booleanType))
        addType(realType)
        addIdentifier(TypeIdentifier("real", realType))
        addType(charType)
        addIdentifier(TypeIdentifier("char", charType))
    }

    fun openScope() {
        scopes.push(Scope())
    }


    private val variablesBuffer = mutableListOf<Identifier>()

    fun addVariableToBuffer(token: Token) {
        if (token is IdentifierToken)
            VariableIdentifier(token.identifier).let {
                variablesBuffer.add(it)
                scopes.peek().addIdentifier(it)
            }
    }

    fun flushVariableBuffer(type: Type?) {
        variablesBuffer.forEach { it.type = type }
        variablesBuffer.clear()
    }

    fun findLocalIdentifier(token: Token): Identifier? {
        if (token !is IdentifierToken)
            return null
        scopes.peek().findIdentifier(token.identifier)?.let {
            return it
        }

        return null
    }

    fun findLocalForwards(): List<ExecutableIdentifier> {
        return scopes.peek().findForwards()
    }

    fun findIdentifier(token: Token): Identifier? {
        if (token !is IdentifierToken)
            return null
        for (scope in scopes.elements()) { // !! check elements order
            scope.findIdentifier(token.identifier)?.let {
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
        val integerType = ScalarType()
        val booleanType = ScalarType()
        val realType = ScalarType()
        val charType = ScalarType()
    }
}