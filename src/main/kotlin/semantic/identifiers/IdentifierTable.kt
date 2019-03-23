package semantic.identifiers

class IdentifierTable {

    private val identifiers = mutableMapOf<String, Identifier>()

    fun add(identifier: Identifier) {
        identifiers[identifier.name.toLowerCase()] = identifier
    }

    fun find(identifier: String): Identifier? {
        return identifiers[identifier.toLowerCase()]
    }
}