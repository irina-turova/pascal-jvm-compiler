package io

import java.io.File
import java.lang.Exception

class IOProvider(fileName: String) {

    private val reader = File(fileName).also {
        if (!it.exists())
            throw Exception("File with specified name does not exist.")
    }.bufferedReader()

    var currentLine: String = reader.readLine()
    var currentPosition = TextPosition(1, 1)

    private fun processNextChar(): Boolean {
        while (currentPosition.charNumber > currentLine.length) {
            listCurrentLine()
            if (reader.ready())
                currentLine = reader.readLine()
            else return false
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
        println("Listing: $currentLine")
    }

}