package compiler

import io.IOProvider
import lexer.Lexer

object Compiler {

    @JvmStatic
    fun main(args: Array<String>) {
        if (args.getOrNull(0) == null) {
            println("Specify program file name as the first parameter.")
            return
        }
        val io = IOProvider(args[0])
        val lexer = Lexer(io)
        for (i in 1..20)
            println(lexer.nextSymbol())
    }
}