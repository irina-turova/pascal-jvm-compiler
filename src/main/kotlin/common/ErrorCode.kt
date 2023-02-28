package common

import lexer.TokenType

enum class ErrorCode(private val code: Int) {
    ERROR(34),

    IDENTIFIER_EXPECTED(2),
    UNKNOWN_IDENTIFIER(3),
    DUPLICATE_IDENTIFIER(4),
    ERROR_IN_REAL_CONSTANT(6),
    TYPE_IDENTIFIER_EXPECTED(12),
    VARIABLE_IDENTIFIER_EXPECTED(20),
    TYPE_MISMATCH(26),
    CONSTANT_EXPECTED(31),
    BEGIN_EXPECTED(36),
    END_EXPECTED(37),
    BOOLEAN_EXPRESSION_EXPECTED(40),
    OPERAND_TYPES_DO_NOT_MATCH_OPERATOR(41),
    ILLEGAL_ASSIGNMENT(43),
    DO_EXPECTED(50),
    THEN_EXPECTED(57),
    TO_OR_DOWNTO_EXPECTED(58),
    UNDEFINED_FORWARD(59),
    CONSTANT_OUT_OF_RANGE(76),
    INTEGER_OR_REAL_EXPRESSION_EXPECTED(79),
    SEMICOLON_EXPECTED(85),
    COLON_EXPECTED(86),
    TO_EXPECTED(87),
    LEFT_BRACKET_EXPECTED(88),
    RIGHT_BRACKET_EXPECTED(89),
    EQUAL_OPERATOR_EXPECTED(90),
    ASSIGN_OPERATOR_EXPECTED(91),
    DOT_EXPECTED(94),
    DOT_DOT_EXPECTED(95),
    INVALID_FOR_CONTROL_VARIABLE(97),
    CHARACTER_EXPRESSION_EXPECTED(106),
    PROCEDURE_OR_FUNCTION_VARIABLE_EXPECTED(142),

    NO_NAME_ERROR(152),
    UNEXPECTED_SYMBOL(-1),
    WRONG_NUMBER_OF_PARAMETERS(-2),

    ;

    fun errorText(): String {
        return codesToMessage[this.code] ?: "no message"
    }

    private val codesToMessage = mapOf(
        -2 to "Wrong number of parameters",
        -1 to "Unexpected symbol",
        1 to "Out of memory",
        2 to "Identifier expected",
        3 to "Unknown identifier",
        4 to "Duplicate identifier",
        5 to "Syntax error",
        6 to "Error in real constant",
        7 to "Error in integer constant",
        8 to "String constant exceeds line",
        9 to "Too many nested files",
        10 to "Unexpected end of file",
        11 to "Line too long",
        12 to "Type identifier expected",
        13 to "Too many open files",
        14 to "Invalid file name",
        15 to "File not found",
        16 to "Disk full",
        17 to "Invalid compiler directive",
        18 to "Too many files",
        19 to "Undefined type in pointer definition",
        20 to "Variable identifier expected",
        21 to "Error in type",
        22 to "Structure too large",
        23 to "Set base type of range",
        24 to "File components may not be files",
        25 to "Invalid string length",
        26 to "Type mismatch",
        27 to "Invalid subrange base type",
        28 to "Lower bound greater than upper bound",
        29 to "Ordinal type expected",
        30 to "Integer constant expected",
        31 to "Constant expected",
        32 to "Integer or real constant expected",
        33 to "Type identifier expected",
        34 to "Invalid function result type",
        35 to "Label identifier expected",
        36 to "BEGIN expected",
        37 to "END expected",
        38 to "Integer expression expected",
        39 to "Ordinal expression expected",
        40 to "Boolean expression expected",
        41 to "Operand types do not match operator",
        42 to "Error in expression",
        43 to "Illegal assignment",
        44 to "Field identifier expected",
        45 to "Object file too large",
        46 to "Undefined external",
        47 to "Invalid object file record",
        48 to "Code segment too large",
        49 to "Data segment too large",
        50 to "DO expected",
        51 to "Invalid PUBLIC definition",
        52 to "Invalid EXTRN definition",
        53 to "Too many EXTRN definition",
        54 to "OF expected",
        55 to "INTERFACE expected",
        56 to "Invalid relocatable reference",
        57 to "THEN expected",
        58 to "TO or DOWNTO expected",
        59 to "Undefined forward",
        60 to "Too many procedures",
        61 to "Invalid typecast",
        62 to "Division by zero",
        63 to "Invalid file type",
        64 to "Cannot Read or Write variables of this type",
        65 to "Pointer variable expected",
        66 to "String variable expected",
        67 to "String expression expected",
        68 to "Circular unit reference",
        69 to "Unit name mismatch",
        70 to "Unit version mismatch",
        71 to "Duplicate unit name",
        72 to "Unit file format error",
        73 to "IMPLEMENTATION expected",
        74 to "Constant and case types do not match",
        75 to "Record variable expected",
        76 to "Constant out of range",
        77 to "File variable expected",
        78 to "Pointer expression expected",
        79 to "Integer or real expression expected",
        80 to "Label not within current block",
        81 to "Label already defined",
        82 to "Undefined label in processing statement part",
        83 to "Invalid @ argument",
        84 to "Unit expected",
        85 to "';' expected",
        86 to "':' expected",
        87 to "'to' expected",
        88 to "'(' expected",
        89 to "')' expected",
        90 to "'=' expected",
        91 to "':=' expected",
        92 to "'[' or '(' expected",
        93 to "']' or ')' expected",
        94 to "'.' expected",
        95 to "'..' expected",
        96 to "Too many variables",
        97 to "Invalid FOR control variable",
        98 to "Integer variable expected",
        99 to "File and procedure types are not allowed here",
        100 to "String length mismatch",
        101 to "Invalid ordering of fields",
        102 to "String constant expected",
        103 to "Integer or real variable expected",
        104 to "Ordinal variable expected",
        105 to "INLINE error",
        106 to "Character expression expected",
        107 to "Too many relocation items",
        108 to "Overflow in arithmetic operator",
        109 to "No enclosing FOR to WHILE or REPEAT statement",
        110 to "Debug information table overflow",
        111 to "- - -",
        112 to "CASE constant out of range",
        113 to "Error in statement",
        114 to "Cannot call an interrupt procedure",
        115 to "- - -",
        116 to "Must be in 8087 mode to compile this",
        117 to "Target address not found",
        118 to "Include files are not allowed here",
        119 to "No inherited methods are accessible here",
        120 to "- - -",
        121 to "Invalid qualifier",
        122 to "Invalid variable reference",
        123 to "Too many symbols",
        124 to "Statement part too large",
        125 to "- - -",
        126 to "Files must be var parameters",
        127 to "Too many conditional symbols",
        128 to "Misplaced conditional directive",
        129 to "ENDIF directive missing",
        130 to "Error in initial conditional defines",
        131 to "Header does not match previous definition",
        132 to "Critical disk error",
        133 to "Cannot evaluate this expression",
        134 to "Expression incorrectly terminated",
        135 to "Invalid format specifier",
        136 to "Invalid indirect reference",
        137 to "Structured variable are not allowed here",
        138 to "Cannot evaluate without System unit",
        139 to "Cannot access this symbol",
        140 to "Invalid floating-point operation",
        141 to "Cannot compile overlay to memory",
        142 to "Procedure or function variable expected",
        143 to "Invalid procedure or function reference",
        144 to "Cannot overlay this unit",
        145 to "Too many nested scopes",
        146 to "File access denied",
        147 to "Object type expected",
        148 to "Local object types are not allowed",
        149 to "VIRTUAL expected",
        150 to "Method identifier expected",
        151 to "Virtual constructor are not allowed",
        152 to "noname error",
        153 to "Destructor identifier expected",
        154 to "Fail only allowed within constructor",
        155 to "Invalid combination of opcode and operands",
        156 to "Memory reference expected",
        157 to "Cannot add or subtract relocatable symbols",
        158 to "Invalid register combination",
        159 to "286/287 instructions are not enabled",
        160 to "Invalid symbol reference",
        161 to "Code generation error",
        162 to "ASM expected"
    )

    companion object {
        fun fromExpectedToken(tokenType: TokenType): ErrorCode {
            return tokenToExpectedError[tokenType] ?: NO_NAME_ERROR
        }

        private val tokenToExpectedError = mutableMapOf(
            TokenType.IDENTIFIER to IDENTIFIER_EXPECTED,
            TokenType.BEGIN to BEGIN_EXPECTED,
            TokenType.END to END_EXPECTED,
            TokenType.DO to DO_EXPECTED,
            TokenType.THEN to THEN_EXPECTED,
            TokenType.SEMICOLON to SEMICOLON_EXPECTED,
            TokenType.COLON to COLON_EXPECTED,
            TokenType.TO to TO_EXPECTED, // ?
            TokenType.LEFT_BRACKET to LEFT_BRACKET_EXPECTED,
            TokenType.RIGHT_BRACKET to RIGHT_BRACKET_EXPECTED,
            TokenType.EQUAL_OPERATOR to EQUAL_OPERATOR_EXPECTED,
            TokenType.ASSIGN_OPERATOR to ASSIGN_OPERATOR_EXPECTED,
            TokenType.DOT to DOT_EXPECTED,
            TokenType.DOT_DOT to DOT_DOT_EXPECTED
        )
    }


}
