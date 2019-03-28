package semantic.identifiers

import semantic.Parameter
import semantic.types.Type

abstract class Identifier(open val name: String, var type: Type?)

class ProgramIdentifier(name: String, type: Type? = null): Identifier(name, type)

class VariableIdentifier(name: String, type: Type? = null): Identifier(name, type)

class ConstantIdentifier(name: String, type: Type? = null): Identifier(name, type)

class TypeIdentifier(name: String, type: Type? = null): Identifier(name, type)

abstract class ExecutableIdentifier(name: String, type: Type? = null): Identifier(name, type) {
    var parameters = listOf<Parameter>()
    var isForward = false
}

class FunctionIdentifier(name: String, type: Type? = null): ExecutableIdentifier(name, type) {
    var resultType: Type? = null
}
class ProcedureIdentifier(name: String, type: Type? = null): ExecutableIdentifier(name, type)