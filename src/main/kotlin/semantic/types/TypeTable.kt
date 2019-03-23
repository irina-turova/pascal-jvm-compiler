package semantic.types

class TypeTable {

    private val types = mutableSetOf<Type>()

    fun add(type: Type) {
        types.add(type)
    }
}
