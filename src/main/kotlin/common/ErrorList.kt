package common

import java.util.*

class ErrorList {
    private val errors = ArrayDeque<Error>()
    private val ERRORS_LIMIT = 5

    fun peekError(): Error? {
        return if (errors.isNotEmpty())
            errors.peekFirst().also { errors.pollFirst() }
        else
            null
    }

    fun pushError(error: Error) {
        if (errors.size < ERRORS_LIMIT)
            errors.addLast(error)
    }
}