package semantic.identifiers

import semantic.parameters.Parameter
import semantic.types.Type

abstract class Identifier(open val name: String, var type: Type?)

class ProgramIdentifier(name: String, type: Type? = null): Identifier(name, type)

class VariableIdentifier(name: String, type: Type? = null): Identifier(name, type)

class ConstantIdentifier(name: String, type: Type? = null): Identifier(name, type)

class TypeIdentifier(name: String, type: Type? = null): Identifier(name, type)

class FunctionIdentifier(name: String, type: Type? = null): Identifier(name, type) {
    var parameters = mutableListOf<Parameter?>()
    var isForward = false
    var resultType: Type? = null
}