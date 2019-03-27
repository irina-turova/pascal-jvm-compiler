package io

import java.io.File
import java.lang.Exception
import common.ErrorList

class IOProvider(fileName: String, val errors: ErrorList) {

    private val reader = File(fileName).also {
        if (!it.exists())
            throw Exception("File with specified name does not exist.")
    }.bufferedReader()

    var currentLine: String = reader.readLine()
    var currentPosition = TextPosition(1, 1)

    private fun processNextChar(): Boolean {
        while (currentPosition.charNumber > currentLine.length) {
            if (currentLine != "\u0000")
                listCurrentLine()
            if (reader.ready())
                currentLine = reader.readLine().replace("\t", " ".repeat(4)) + " "
            else {
                currentLine = "\u0000"
                return false
            }
            currentPosition.lineNumber++
            currentPosition.charNumber = 1
        }
        return true
    }

    fun nextChar(): Char {
        return if (processNextChar())
            currentLine[currentPosition.charNumber - 1]
        else '\u0000'
    }

    fun takeNextChar(): Char {
        return if (processNextChar())
            currentLine[currentPosition.charNumber++ - 1]
        else '\u0000'
    }

    private fun listCurrentLine() {
        val columnWidth = 6
        val lineNumberString = currentPosition.lineNumber.toString()
        val lineNumPadEnd = (columnWidth - lineNumberString.length) / 2
        println("${lineNumberString.padStart(columnWidth - lineNumPadEnd).padEnd(columnWidth)} $currentLine")

        var error = errors.peekError()
        while (error != null) {
            val errorNumberString = errors.totalErrorsCnt.toString()
            val errorNumPadEnd = (columnWidth - errorNumberString.length) / 2
            println(errorNumberString.padStart(columnWidth - errorNumPadEnd, '*').padEnd(columnWidth, '*') +
                    " ".repeat(error.textPosition.charNumber - 1) + "^ ошибка: ${error.errorCode.errorText()}")
            error = errors.peekError()
        }
    }

}