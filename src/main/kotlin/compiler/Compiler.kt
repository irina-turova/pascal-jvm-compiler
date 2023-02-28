package compiler

import common.ErrorList
import io.IOProvider
import lexer.Lexer
import parser.Parser
import semantic.ScopeManager
import java.io.File
import java.io.FileOutputStream
import java.util.jar.Attributes
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import java.util.jar.Manifest


object Compiler {

    @JvmStatic
    fun main(args: Array<String>) {
        if (args.getOrNull(0) == null) {
            println("Specify program file name as the first parameter.")
            return
        }

        File(args[0]).also {
            if (!it.exists()) {
                println("File with specified name does not exist")
                return
            }
        }

        val errorList = ErrorList()
        val io = IOProvider(args[0], errorList)
        val lexer = Lexer(io, errorList)
        val classBytes = Parser(lexer, errorList, ScopeManager()).parse()


        val manifest = Manifest()
        manifest.mainAttributes[Attributes.Name.MANIFEST_VERSION] = "1.0"
        manifest.mainAttributes[Attributes.Name.MAIN_CLASS] = "PascalProgram"

        val jarOutputStream = JarOutputStream(FileOutputStream("program.jar"), manifest)

        jarOutputStream.putNextEntry(JarEntry("PascalProgram.class"))
        jarOutputStream.write(classBytes)
        jarOutputStream.closeEntry()

        jarOutputStream.close()
    }
}