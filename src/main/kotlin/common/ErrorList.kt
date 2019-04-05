package common

import java.util.*

class ErrorList {
    private val errors = ArrayDeque<Error>()
    private val errorsLineLimit = 5
    private val errorsLimit = 999

    private var totalErrorCount = 0

    fun peekError(): Error? {
        return if (errors.isNotEmpty())
            errors.peekFirst().also { errors.pollFirst() }
        else
            null
    }

    fun pushError(error: Error) {
        if (errors.size < errorsLineLimit && totalErrorCount++ < errorsLimit)
            errors.addLast(error)
    }
}