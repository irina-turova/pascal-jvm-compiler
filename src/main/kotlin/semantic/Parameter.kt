package semantic

import semantic.types.Type

open class Parameter {
}

class SimpleParameter(val type: Type, val mode: TransmissionMode): Parameter()

class ExecutableParameter: Parameter() {
    val parameterTypes = mutableListOf<Type>()
    val isForward: Boolean = false
}