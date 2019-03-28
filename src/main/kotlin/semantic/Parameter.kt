package semantic

import semantic.types.Type

open class Parameter {
}

class SimpleParameter(val name: String, var type: Type?, var mode: TransmissionMode): Parameter()

class ExecutableParameter: Parameter() {
    val parameterTypes = mutableListOf<Type>()
    val isForward: Boolean = false
}