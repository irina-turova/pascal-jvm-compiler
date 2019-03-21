package compiler

import io.IOProvider
import lexer.Lexer
import parser.Parser

object Compiler {

    @JvmStatic
    fun main(args: Array<String>) {
        if (args.getOrNull(0) == null) {
            println("Specify program file name as the first parameter.")
            return
        }
        val io = IOProvider(args[0])
        val lexer = Lexer(io)
        Parser(lexer).parse()
    }
}