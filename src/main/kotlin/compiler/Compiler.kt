package compiler

import io.IOProvider
import lexer.Lexer
import parser.Parser
import common.ErrorList
import semantic.ScopeManager

object Compiler {

    @JvmStatic
    fun main(args: Array<String>) {
        if (args.getOrNull(0) == null) {
            println("Specify program file name as the first parameter.")
            return
        }
        val errorList = ErrorList()
        val io = IOProvider(args[0], errorList)
        val lexer = Lexer(io, errorList)
        Parser(lexer, errorList, ScopeManager()).parse()
    }
}