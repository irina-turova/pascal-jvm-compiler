package io

import java.io.File
import java.lang.Exception
import common.ErrorList

class IOProvider(fileName: String, private val errors: ErrorList) {

    private val reader = File(fileName).also {
        if (!it.exists())
            throw Exception("File with specified name does not exist.")
    }.bufferedReader()

    private val writer = File("$fileName.listing").bufferedWriter()

    private var currentLine: String = reader.readLine().replace("\t", " ".repeat(4)).plus("\n")
    var currentPosition = TextPosition(1, 1)

    private fun processNextChar(): Boolean {
        while (currentPosition.charNumber > currentLine.length) {
            if (currentLine != "\u0000")
                listCurrentLine()
            if (reader.ready())
                currentLine = reader.readLine().replace("\t", " ".repeat(4)).plus("\n")
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

    private val columnWidth = 8

    private fun listCurrentLine() {
        val lineNumberString = currentPosition.lineNumber.toString()
        val lineNumPadEnd = (columnWidth - lineNumberString.length) / 2
        ("${lineNumberString.padStart(columnWidth - lineNumPadEnd).padEnd(columnWidth)} $currentLine").also {
            print(it)
            writer.write(it)
        }

        listErrors()
    }

    private var errorNumber = 1

    private fun listErrors() {
        var error = errors.peekError()
        while (error != null) {
            val errorNumberString = (errorNumber++).toString()
            val errorNumPadEnd = (columnWidth - errorNumberString.length) / 2
            (errorNumberString.padStart(columnWidth - errorNumPadEnd, '*').padEnd(columnWidth, '*') +
                    " ".repeat(error.textPosition.charNumber) + "^ error: ${error.errorCode.errorText()}").also {
                println(it)
                writer.write(it)
                writer.newLine()
            }
            error = errors.peekError()
        }
    }

    fun flush() {
        listErrors()
        writer.flush()
    }

}