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

        val operator = operators.pop()

        when (operator) {
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
            TokenType.EQUAL_OPERATOR, TokenType.NOT_EQUAL_OPERATOR, TokenType.LESS_OPERATOR,
            TokenType.LESS_OR_EQUAL_OPERATOR, TokenType.GREATER_OR_EQUAL_OPERATOR, TokenType.GREATER_OPERATOR -> {

                val (ifCmpOp, fCmpOp) = when (operator) {
                    TokenType.EQUAL_OPERATOR -> Pair(IF_ICMPEQ, IFEQ)
                    TokenType.NOT_EQUAL_OPERATOR -> Pair(IF_ICMPNE, IFNE)
                    TokenType.LESS_OPERATOR -> Pair(IF_ICMPLT, IFLT)
                    TokenType.LESS_OR_EQUAL_OPERATOR -> Pair(IF_ICMPLE, IFLE)
                    TokenType.GREATER_OR_EQUAL_OPERATOR -> Pair(IF_ICMPGE, IFGE)
                    TokenType.GREATER_OPERATOR -> Pair(IF_ICMPGT, IFGT)
                    else -> Pair(IF_ICMPLT, IFLT)
                }

                val put1 = Label()
                val after = Label()

                if (desiredType.jvmName == "I")
                    mainMethodWriter.visitJumpInsn(ifCmpOp, put1)
                else {
                    mainMethodWriter.visitInsn(FCMPL)
                    mainMethodWriter.visitJumpInsn(fCmpOp, put1)
                }

                mainMethodWriter.visitLdcInsn(0)
                mainMethodWriter.visitJumpInsn(GOTO, after)
                mainMethodWriter.visitLabel(put1)
                mainMethodWriter.visitLdcInsn(1)
                mainMethodWriter.visitLabel(after)
            }
            TokenType.AND -> mainMethodWriter.visitInsn(IAND)
            TokenType.OR -> mainMethodWriter.visitInsn(IOR)
            TokenType.NOT -> {
                mainMethodWriter.visitLdcInsn(1)
                mainMethodWriter.visitInsn(IXOR)
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
            TokenType.EQUAL_OPERATOR, TokenType.NOT_EQUAL_OPERATOR, TokenType.LESS_OPERATOR,
            TokenType.LESS_OR_EQUAL_OPERATOR, TokenType.GREATER_OR_EQUAL_OPERATOR, TokenType.GREATER_OPERATOR -> 2
            TokenType.PLUS, TokenType.MINUS, TokenType.OR -> 3
            TokenType.STAR, TokenType.SLASH, TokenType.DIV, TokenType.MOD, TokenType.AND -> 4
            TokenType.NOT -> 5
            TokenType.LEFT_BRACKET -> 6
            else ->  throw Exception("Generation: operator $operator is not supported")
        }
    }
}


abstract class OperandType(val type:Type)

class VariableType(val id: Int, type: Type): OperandType(type)

class ConstantOperand(val value: Any, type: Type): OperandType(type)
