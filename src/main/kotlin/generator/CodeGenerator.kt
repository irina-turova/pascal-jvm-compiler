package generator

import jdk.internal.org.objectweb.asm.Label
import jdk.internal.org.objectweb.asm.MethodVisitor
import jdk.internal.org.objectweb.asm.Opcodes
import jdk.internal.org.objectweb.asm.Opcodes.*
import lexer.TokenType
import semantic.ScopeManager
import semantic.identifiers.VariableIdentifier
import semantic.types.Type
import java.lang.Exception
import java.util.*

typealias CG = CodeGenerator

object CodeGenerator {

    lateinit var mainMethodWriter: MethodVisitor
    lateinit var methodStart: Label
    lateinit var methodEnd: Label

    private val operandTypes = Stack<Type>()
    private val operators = Stack<TokenType>()

    fun flush() {
        while (operators.isNotEmpty())
            calculateOperator()
    }

    fun addOperand(operand: OperandType) {
        operandTypes.add(operand.type)
        loadOperand(operand)
    }

    fun addOperator(operator: TokenType) {
        if (operator == TokenType.RIGHT_BRACKET) {
            while (operators.isNotEmpty() && operators.peek() != TokenType.LEFT_BRACKET) {
                calculateOperator()
            }
            operators.pop()
        } else {
            while (operators.isNotEmpty() && operators.peek() != TokenType.LEFT_BRACKET && getOperatorPriority(operators.peek()) > getOperatorPriority(operator)) {
                calculateOperator()
            }
            operators.push(operator)
        }
    }

    fun assign(variable: VariableIdentifier, expressionType: Type) {
        flush()
        if (variable.type == ScopeManager.realType && expressionType == ScopeManager.integerType)
            mainMethodWriter.visitInsn(I2F)
        mainMethodWriter.visitVarInsn(
            when (variable.type?.jvmName) {
                "I" -> ISTORE
                "F" -> FSTORE
                else -> ISTORE
            }, variable.id)
    }

    private fun calculateOperator() {

        //val operandTypes = loadOperands()
        val desiredType = operandTypes.pop()
        val firstOperandType = operandTypes.pop()

        when (operators.pop()) {
            TokenType.PLUS -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.IADD)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FADD)
                    else -> mainMethodWriter.visitInsn(Opcodes.IADD)
                }
            }
            TokenType.MINUS -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.ISUB)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FSUB)
                    else -> mainMethodWriter.visitInsn(Opcodes.ISUB)
                }
            }
            TokenType.STAR -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.IMUL)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FMUL)
                    else -> mainMethodWriter.visitInsn(Opcodes.IMUL)
                }
            }
            TokenType.SLASH -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.IDIV)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FDIV)
                    else -> mainMethodWriter.visitInsn(Opcodes.IDIV)
                }
            }
            TokenType.DIV -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.IDIV)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FDIV)
                    else -> mainMethodWriter.visitInsn(Opcodes.IDIV)
                }
            }
            TokenType.MOD -> {
                when(desiredType.jvmName) {
                    "I" -> mainMethodWriter.visitInsn(Opcodes.IREM)
                    "F" -> mainMethodWriter.visitInsn(Opcodes.FREM)
                    else -> mainMethodWriter.visitInsn(Opcodes.IREM)
                }
            }
        }
        operandTypes.push(desiredType)
    }

    private fun loadOperand(operand: OperandType) {
        when (operand) {
            is VariableType -> {
                if (operand.id < 1)
                    throw Exception("Generation: operand id $operand is not supported")
                mainMethodWriter.visitVarInsn(
                    when (operand.type.jvmName) {
                        "I" -> ILOAD
                        "F" -> FLOAD
                        else -> ILOAD
                    }
                    , operand.id)
            }
            is ConstantOperand -> {
                mainMethodWriter.visitLdcInsn(operand.value)
            }
            else -> throw Exception("Generation: operand $operand is not supported")
        }
    }

    private fun getOperatorPriority(operator: TokenType): Int {
        return when (operator) {
            TokenType.PLUS, TokenType.MINUS -> 1
            TokenType.STAR, TokenType.SLASH, TokenType.DIV, TokenType.MOD -> 2
            TokenType.LEFT_BRACKET -> 3
            else ->  throw Exception("Generation: operator $operator is not supported")
        }
    }
}


abstract class OperandType(val type:Type)

class VariableType(val id: Int, type: Type): OperandType(type)

class ConstantOperand(val value: Any, type: Type): OperandType(type)
