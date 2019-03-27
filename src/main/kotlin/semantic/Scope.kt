package semantic

import semantic.identifiers.ExecutableIdentifier
import semantic.identifiers.Identifier
import semantic.identifiers.IdentifierTable
import semantic.types.Type
import semantic.types.TypeTable

class Scope {

    private val identifiers = IdentifierTable()
    private val types = TypeTable()

    fun addIdentifier(identifier: Identifier) {
        identifiers.add(identifier)
    }

    fun findIdentifier(identifier: String): Identifier? {
        return identifiers.find(identifier)
    }

    fun addType(type: Type) {
        types.add(type)
    }

    fun findForwards(): List<ExecutableIdentifier> {
        return identifiers.findForwards()
    }
}