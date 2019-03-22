package common

import io.TextPosition

data class Error(val textPosition: TextPosition, val errorCode: ErrorCode)