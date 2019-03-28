package semantic.types

import lexer.TokenType
import semantic.ScopeManager

abstract class Type {
    fun isSignable(): Boolean { // TODO: Проверить для своих типов...
        return this == ScopeManager.integerType || this == ScopeManager.realType
    }

    fun isLogical(): Boolean {
        return this == ScopeManager.booleanType
    }

    fun isCompatibleTo(other: Type): Boolean { // TODO: Проверить для своих типов...
        return when(this) {
             ScopeManager.realType ->
                other in setOf(ScopeManager.integerType, ScopeManager.realType)
            else -> this == other
        }
    }

    fun comparingType(other: Type, operator: TokenType): Type? {
        return when(this) {
            ScopeManager.integerType, ScopeManager.realType ->
                if (other in setOf(
                        ScopeManager.integerType,
                        ScopeManager.realType
                    )) ScopeManager.booleanType else null
            ScopeManager.booleanType -> if (other == ScopeManager.booleanType) ScopeManager.booleanType else null
            ScopeManager.charType -> if (other == ScopeManager.charType) ScopeManager.booleanType else null
            else -> null
        }
    }

    fun addingType(other: Type, operator: TokenType): Type? {
        return when {
            operator == TokenType.OR -> if (this == ScopeManager.booleanType && other == ScopeManager.booleanType) ScopeManager.booleanType else null
            this == other && this != ScopeManager.booleanType -> this
            else -> null
        }
    }

    /*
    val multiplyingOperators = setOf(TokenType.STAR, TokenType.SLASH, TokenType.DIV,
        TokenType.MOD, TokenType.AND)
     */
    fun multiplyingType(other: Type, operator: TokenType): Type? {
        return when {
            operator == TokenType.AND -> if (this == ScopeManager.booleanType && other == ScopeManager.booleanType) ScopeManager.booleanType else null
            operator == TokenType.DIV || operator == TokenType.MOD -> if (this == other && this == ScopeManager.integerType) this else null
            setOf<Type>(
                ScopeManager.integerType,
                ScopeManager.realType
            ).containsAll(setOf(this, other)) ->
                if (this == other && operator != TokenType.SLASH) this else ScopeManager.realType
            else -> null
        }
    }
}

class ProgramType: Type()

class ScalarType: Type()

class EnumType: Type()

