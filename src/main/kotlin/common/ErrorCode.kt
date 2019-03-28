package common

import lexer.TokenType

enum class ErrorCode(val code: Int) {
    ERROR(34),

    IDENTIFIER_EXPECTED(2),
    UNKNOWN_IDENTIFIER(3),
    DUPLICATE_IDENTIFIER(4),
    TYPE_IDENTIFIER_EXPECTED(12),
    VARIABLE_IDENTIFIER_EXPECTED(20),
    TYPE_MISMATCH(26),
    CONSTANT_EXPECTED(31),
    BEGIN_EXPECTED(36),
    END_EXPECTED(37),
    BOOLEAN_EXPRESSION_EXPECTED(40),
    OPERAND_TYPES_DO_NOT_MATCH_OPERATOR(41),
    DO_EXPECTED(50),
    THEN_EXPECTED(57),
    TO_OR_DOWNTO_EXPECTED(58),
    UNDEFINED_FORWARD(59),
    CONSTANT_OUT_OF_RANGE(76),
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

    NO_NAME_ERROR(152),
    UNEXPECTED_SYMBOL(-1),
    WRONG_NUMBER_OF_PARAMETERS(-2),

    ;

    fun errorText(): String {
        return codesToMessage[this.code] ?: "no message"
    }

    private val codesToMessage = mapOf(
        -2 to "Wrong number of parameters - Неверное количество параметров",
        -1 to "Unexpected symbol - Неожиданный символ",
        1 to "Out of memory - Выход за границы памяти",
        2 to "Identifier expected - He указан идентификатор",
        3 to "Unknown identifier - Неизвестный идентификатор",
        4 to "Duplicate identifier - Двойной идентификатор",
        5 to "Syntax error - Синтаксическая ошибка",
        6 to "Error in real constant - Ошибка в вещественной константе",
        7 to "Error in integer constant - Ошибка в целой константе",
        8 to "String constant exceeds line - Строковая константа превышает допустимые размеры",
        9 to "Too many nested files - Слишком много вложенных файлов",
        10 to "Unexpected end of file - Неожиданный конец файла",
        11 to "Line too long - Слишком длинная строка",
        12 to "Type identifier expected - Здесь нужен идентификатор типа",
        13 to "Too many open files - Слишком много открытых файлов",
        14 to "Invalid file name - Неверное имя файла",
        15 to "File not found - Файл не найден",
        16 to "Disk full - Диск заполнен",
        17 to "Invalid compiler directive - Неправильная директива компилятора",
        18 to "Too many files - Слишком много файлов",
        19 to "Undefined type in pointer definition - Неопределенный тип в объявлении указателя",
        20 to "Variable identifier expected - Отсутствует идентификатор переменной",
        21 to "Error in type - Ошибка в объявлении типа",
        22 to "Structure too large - Слишком большая структура",
        23 to "Set base type of range - Базовый тип множества нарушает границы",
        24 to "File components may not be files - Компонентами файла не могут быть файлы",
        25 to "Invalid string length - Неверная длина строки",
        26 to "Type mismatch - Несоответствие типов",
        27 to "Invalid subrange base type - Неправильный базовый тип для типа-диапазона",
        28 to "Lower bound greater than upper bound - Нижняя граница больше верхней",
        29 to "Ordinal type expected - Нужен порядковый тип",
        30 to "Integer constant expected - Нужна целая константа",
        31 to "Constant expected - Нужна константа",
        32 to "Integer or real constant expected - Нужна целая или вещественная константа",
        33 to "Type identifier expected - Нужен идентификатор типа",
        34 to "Invalid function result type - Неправильный тип результата функции",
        35 to "Label identifier expected - Нужен идентификатор метки",
        36 to "BEGIN expected - Нужен BEGIN",
        37 to "END expected - Нужен END",
        38 to "Integer expression expected - Нужно выражение типа INTEGER",
        39 to "Ordinal expression expected - Нужно выражение перечисляемого типа",
        40 to "Boolean expression expected - Нужно выражение типа BOOLEAN",
        41 to "Operand types do not match operator - Типы операндов не соответствуют операции",
        42 to "Error in expression - Ошибка в выражении",
        43 to "Illegal assignment - Неверное присваивание",
        44 to "Field identifier expected - Нужен идентификатор поля",
        45 to "Object file too large - Объектный файл слишком большой",
        46 to "Undefined external - Неопределенная внешняя процедура",
        47 to "Invalid object file record - Неправильная запись объектного файла",
        48 to "Code segment too large - Сегмент кода слишком большой",
        49 to "Data segment too large - Сегмент данных слишком велик",
        50 to "DO expected - Нужен оператор DO",
        51 to "Invalid PUBLIC definition - Неверное PUBLIC-определение",
        52 to "Invalid EXTRN definition - Неправильное EXTRN-определение",
        53 to "Too many EXTRN definition - Слишком много EXTRN-определений",
        54 to "OF expected - Требуется OF",
        55 to "INTERFACE expected - Требуется интерфейсная секция",
        56 to "Invalid relocatable reference - Неправильная перемещаемая ссылка",
        57 to "THEN expected - Требуется THEN",
        58 to "TO or DOWNTO expected - Требуется ТО или DOWNTO",
        59 to "Undefined forward - Неопределенное опережающее описание",
        60 to "Too many procedures - Слишком иного процедур",
        61 to "Invalid typecast - Неверное преобразование типа",
        62 to "Division by zero - Деление на ноль",
        63 to "Invalid file type - Неверный файловый тип",
        64 to "Cannot Read or Write variables of this type - Нет возможности считать или записать переменные данного типа",
        65 to "Pointer variable expected - Нужно использовать переменную-указатель",
        66 to "String variable expected - Нужна строковая переменная",
        67 to "String expression expected - Нужно выражение строкового типа",
        68 to "Circular unit reference - Перекрестная ссылка модулей",
        69 to "Unit name mismatch - Несоответствие имен программных модулей",
        70 to "Unit version mismatch - Несоответствие версий модулей",
        71 to "Duplicate unit name - Повторное имя программного модуля",
        72 to "Unit file format error - Ошибка формата файла модуля",
        73 to "IMPLEMENTATION expected - Отсутствует исполняемая часть модуля",
        74 to "Constant and case types do not match - Типы констант и тип выражения оператора CASE не соответствуют друг другу",
        75 to "Record variable expected - Нужна переменная типа запись",
        76 to "Constant out of range - Константа нарушает границы",
        77 to "File variable expected - Нужна файловая переменная",
        78 to "Pointer expression expected - Нужно выражение типа указатель",
        79 to "Integer or real expression expected - Нужно выражение вещественного или целого типа",
        80 to "Label not within current block - Метка не находится внутри текущего блока",
        81 to "Label already defined - Метка уже определена",
        82 to "Undefined label in processing statement part - Неопределенная метка в предшествующем разделе операторов",
        83 to "Invalid @ argument - Неправильный аргумент операции @",
        84 to "Unit expected - Нужно кодовое слово UNIT",
        85 to "';' expected - Нужно указать \";\"",
        86 to "':' expected - Нужно указать \":\"",
        87 to "' to\" expected - Нужно указать \" to\"",
        88 to "'(\" expected - Нужно указать \"(\"",
        89 to "'')\" expected - Нужно указать \")\"",
        90 to "'=\" expected - Нужно указать \"=\"",
        91 to "':=\" expected - Нужно указать \":=\"",
        92 to "'[\" or \"(.\" expected - Нужно указать \"[\" или \"(.\"",
        93 to "']\" or \".)\" expected - Нужно указать \"]\" или \".)\"",
        94 to "'.\" expected - Нужно указать \".\"",
        95 to ".. expected - Нужно указать \"..\"",
        96 to "Too many variables - Слишком много переменных",
        97 to "Invalid FOR control variable - Неправильный параметр цикла оператора FOR",
        98 to "Integer variable expected - Нужна переменная целого типа",
        99 to "File and procedure types are not allowed here - Здесь не могут использоваться файлы или процедурные типы",
        100 to "String length mismatch - Несоответствие длины строки",
        101 to "Invalid ordering of fields - Неверный порядок полей",
        102 to "String constant expected - Нужна константа строкового типа",
        103 to "Integer or real variable expected - Нужна переменная типа INTEGER или REAL",
        104 to "Ordinal variable expected - Нужна переменная порядкового типа",
        105 to "INLINE error - Ошибка в операторе INLINE",
        106 to "Character expression expected - Предшествующее выражение должно иметь символьный тип",
        107 to "Too many relocation items - Слишком много перемещаемых Элементов",
        108 to "Overflow in arithmetic operator - Переполнение при выполнении арифметического оператора",
        109 to "No enclosing FOR to WHILE or REPEAT statment - Нет операторов to заканчивающих операторы FOR to WHILE или REPEAT",
        110 to "Debug information table overflow - Переполнение информационной таблицы отладки",
        111 to "- - -",
        112 to "CASE constant out of range - Константа CASE нарушает допустимые границы",
        113 to "Error in statement - Ошибка в операторе",
        114 to "Cannot call an interrupt procedure - Невозможно вызвать процедуру прерывания",
        115 to "- - -",
        116 to "Must be in 8087 mode to compile this - Для компиляции необходим режим 8087",
        117 to "Target address not found - Указанный адрес не найден",
        118 to "Include files are not allowed here - Здесь не допускаются включаемые файлы",
        119 to "No inherited methods are accessible here - В этом месте программы нет унаследованных методов",
        120 to "- - -",
        121 to "Invalid qualifier - Неверный квалификатор",
        122 to "Invalid variable reference - Недействительная ссылка на переменную",
        123 to "Too many symbols - Слишком много символов",
        124 to "Statement part too large - Слишком большой раздел операторов",
        125 to "- - -",
        126 to "Files must be var parameters - Файлы должны передаваться как параметры-переменные",
        127 to "Too many conditional symbols - Слишком много условных символов",
        128 to "Misplaced conditional directive - Пропущена условная директива",
        129 to "ENDIF directive missing - Пропущена директива ENDIF",
        130 to "common.Error in initial conditional defines - Ошибка в условных определениях",
        131 to "Header does not match previous definition - Заголовок не соответствует предыдущему определению",
        132 to "Critical disk error - Критическая ошибка диска",
        133 to "Cannot evaluate this expression - Нельзя вычислить данное выражение",
        134 to "Expression incorrectly terminated - Некорректное завершение выражения",
        135 to "Invalid format specifier - Неверный спецификатор формата",
        136 to "Invalid indirect reference - Недопустимая косвенная ссылка",
        137 to "Structured variable are not allowed here - Здесь нельзя использовать переменную структурного типа",
        138 to "Cannot evaluate without System unit - Нельзя вычислить выражение без модуля SYSTEM",
        139 to "Cannot access this symbol - Нет доступа к данному символу",
        140 to "Invalid floating-point operation - Недопустимая операция с плавающей запятой",
        141 to "Cannot compile overlay to memory - Нельзя выполнить компиляцию оверлейных модулей в память",
        142 to "Procedure or function variable expected - Должна использоваться переменная процедурного типа",
        143 to "Invalid procedure or function reference - Недопустимая ссылка на процедуру или функцию",
        144 to "Cannot overlay this unit - Этот модуль не может использоваться в качестве оверлейного",
        145 to "Too many nested scopes - Слишком много вложений",
        146 to "File access denied - Отказано в доступе к файлу",
        147 to "Object type expected - Здесь должен быть тип OBJECT",
        148 to "Local object types are not allowed - Нельзя объявлять локальные объекты",
        149 to "VIRTUAL expected - Пропущено слово VIRTUAL",
        150 to "Method identifier expected - Пропущен идентификатор инкапсулированного правила",
        151 to "Virtual constructor are not allowed - Конструктор не может быть виртуальным",
        152 to "noname - неизвестная ошибка",
        153 to "Destructor identifier expected - Пропущен идентификатор деструктора",
        154 to "Fail only allowed within constructor - Обращение к стандартной процедуре FAIL может содержаться только в конструкторе",
        155 to "Invalid combination of opcode and operands - Недопустимая комбинация кода команды и операндов",
        156 to "Memory reference expected - Отсутствует адрес",
        157 to "Cannot add or subtract relocatable symbols - Нельзя складывать или вычитать перемещаемые символы",
        158 to "Invalid register combination - Недопустимая комбинация регистров",
        159 to "286/287 instructions are not enabled - Недоступен набор команд микропроцессоров 286/287",
        160 to "Invalid symbol reference - Недопустимая ссылка на символ",
        161 to "Code generation error - Ошибка генерации кода",
        162 to "ASM expected - Отсутствует зарезервированное слово ASM"
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
