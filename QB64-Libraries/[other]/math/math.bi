'******************************************************************************
'
'Math Evaluator Library by SMcNeill
'v2.1
'*modified by Gorlock
'
'******************************************************************************
'
'Version 1.0  - 2013-10-01
'Version 2.1  - 2013-10-01
'  - Luke: Added sigle letter variables, ANS and SET
'
'******************************************************************************
'
'Include this code at the beginning of your program:
'https://db.tt/O7ou9bpW
'
'******************************************************************************
'
'Credits:
'              Steve McNeill - Primary programmer
'              Luke Ceddia   - Secondary Programmer, Testing and Advice with math functions
'Use: FREEWARE.  Available for all projects, both commercial and private with no credit needed nor expected.
'Liability: NONE.  Use it, turn into a zombie, it's YOUR responsibility.
'
'http://www.qb64.net/forum/index.php?topic=11476.0
'
'******************************************************************************
'
'Variables:
'All variables are a single leter (case insensitive). When using it in calculations,
'put a "#" after it, to designate it as a variable. So a# + 2 * c# will be evaluated with
'whatever a and c contain.
'By default, they are set to 0.
'
'EQL:
'   a EQL 3 * 2         Stores 6 in variable a
'   b# EQL 2 + 1        Stores 3 in b
'   a EQL a# * 2        Double a, then store in back in a
'
'ANS:
'Contains the result of the previous calculation. Use it as you would a number: 2 * ans
'At program start, it is 0.
'
'******************************************************************************

FUNCTION Evaluate_Expression$ (e$)
t$ = e$
b = INSTR(UCASE$(e$), "EQL")
IF b THEN
    t$ = MID$(e$, b + 3)
    var$ = UCASE$(LTRIM$(RTRIM$(MID$(e$, 1, b - 1))))
END IF
QuickReturn = 0
PreParse t$
IF QuickReturn THEN Evaluate_Expression$ = t$: EXIT FUNCTION
IF LEFT$(t$, 5) = "ERROR" THEN Evaluate_Expression$ = t$: EXIT FUNCTION
exp$ = "(" + t$ + ")"
DO
    pl = INSTR(exp$, ")")
    IF pl > 0 THEN
        c = 0
        DO UNTIL pl - c <= 0
            c = c + 1
            IF pl THEN
                IF MID$(exp$, pl - c, 1) = "(" THEN EXIT DO
            END IF
        LOOP
        s = pl - c + 1
        IF s < 1 THEN Evaluate_Expression$ = "ERROR -- BAD () Count": EXIT FUNCTION
        eval$ = " " + MID$(exp$, s, pl - s) + " "
        ParseExpression eval$
        eval$ = LTRIM$(RTRIM$(eval$))
        IF LEFT$(eval$, 5) = "ERROR" THEN Evaluate_Expression$ = eval$: EXIT FUNCTION
        exp$ = DWD(LEFT$(exp$, s - 2) + eval$ + MID$(exp$, pl + 1))
    END IF
LOOP UNTIL pl = 0
c = 0
DO
    c = c + 1
    SELECT CASE MID$(exp$, c, 1)
        CASE "0" TO "9", ".", "-"
        CASE ELSE
            exp$ = "ERROR - Unknown Diagnosis: (" + exp$ + ") "
    END SELECT
LOOP UNTIL c >= LEN(exp$)
IF var$ <> "" THEN
    SELECT CASE LEN(var$)
        CASE 1
            v$ = var$
        CASE 2
            IF RIGHT$(var$, 1) = "#" THEN
                v$ = LEFT$(var$, 1)
            ELSE
                Evaluate_Expression$ = "ERROR - Bad User Variable Value.  (" + var$ + ")"
                EXIT SUB
            END IF
        CASE ELSE
            Evaluate_Expression$ = "ERROR - Bad User Variable Value.  (" + var$ + ")"
            EXIT SUB
    END SELECT
    index = ASC(v$) - 64
    IF index < 1 OR index > 26 THEN Evaluate_Expression$ = "ERROR - Letter required for variable name": EXIT SUB
    math_vars(index) = exp$
END IF
f = FREEFILE
OPEN math_Filename FOR BINARY AS #f
counter = 0
FOR c = 0 TO 26
    length& = LEN(math_vars(c))
    PUT #f, , length&
    PUT #f, , math_vars(c)
NEXT c
CLOSE #f
math_vars(0) = exp$
Evaluate_Expression$ = exp$
END FUNCTION

SUB ParseExpression (exp$)
DIM math_num(0 TO 10) AS STRING
FOR J = 1 TO 250
    lowest = 0
    DO UNTIL lowest = LEN(exp$)
        lowest = LEN(exp$)
        OpOn = 0
        FOR P = 1 TO UBOUND(OName)
            IF J = math_PL(P) THEN
                IF LEFT$(exp$, 1) = "-" THEN
                    op = INSTR(2, exp$, math_OName(P))
                ELSE
                    op = INSTR(exp$, math_OName(P))
                END IF
                IF op > 0 AND op < lowest THEN
                    lowest = op
                    OpOn = P
                END IF
            END IF
        NEXT
        IF OpOn = 0 THEN EXIT DO
        IF LEFT$(exp$, 1) = "-" THEN
            op = INSTR(2, exp$, math_OName(OpOn))
        ELSE
            op = INSTR(exp$, math_OName(OpOn))
        END IF
        numset = 0
        IF math_OName(OpOn) = "-" THEN
            SELECT CASE MID$(exp$, op - 3, 3)
                CASE "NOT", "XOR", "AND", "EQV", "IMP"
                    EXIT DO
            END SELECT
            IF MID$(exp$, op - 3, 2) = "OR" THEN EXIT DO
        END IF
        IF op THEN
            c = LEN(math_OName(OpOn)) - 1
            DO
                SELECT CASE MID$(exp$, op + c + 1, 1)
                    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
                        numset = -1
                    CASE "-"
                        IF math_OName(OpOn) = "PI" OR numset THEN EXIT DO
                    CASE ELSE
                        EXIT DO
                END SELECT
                c = c + 1
            LOOP UNTIL op + c >= LEN(exp$)
            pl = op + c
            c = 0
            DO
                c = c + 1
                SELECT CASE MID$(exp$, op - c, 1)
                    CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
                    CASE "-"
                        c1 = c
                        bad = 0
                        DO
                            c1 = c1 + 1
                            SELECT CASE MID$(exp$, op - c1, 1)
                                CASE "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "."
                                    bad = -1
                                    EXIT DO
                            END SELECT
                        LOOP UNTIL op - c1 <= 0
                        IF bad THEN EXIT DO
                    CASE ELSE
                        EXIT DO
                END SELECT
            LOOP UNTIL op - c <= 0
            s = op - c
            math_num(1) = MID$(exp$, s + 1, op - s - 1)
            math_num(2) = MID$(exp$, op + LEN(math_OName(OpOn)), pl - op - LEN(math_OName(OpOn)) + 1)
            math_num(3) = EvaluateNumbers(OpOn, math_num())
            IF LEFT$(math_num(3), 5) = "ERROR" THEN exp$ = math_num(3): EXIT SUB
            exp$ = LTRIM$(N2S(DWD(LEFT$(exp$, s) + RTRIM$(LTRIM$(math_num(3))) + MID$(exp$, pl + 1))))
        END IF
        op = 0
    LOOP
NEXT
END SUB

SUB Set_OrderOfOperations
IF NOT _DIREXISTS("internal") THEN MKDIR "internal"
IF NOT _DIREXISTS("internal/MathEval") THEN MKDIR "internal/MathEval"
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ANS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "PI"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "%"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 5
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ARCCOS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ARCSIN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ARCSEC"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ARCCSC"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ARCCOT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "SECH"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "CSCH"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "COTH"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "COS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "SIN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "TAN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "LOG"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "EXP"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ATN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "D2R"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "D2G"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "R2D"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "R2G"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "G2D"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "G2R"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ABS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "SGN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "INT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "_ROUND"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "FIX"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "SEC"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "CSC"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "COT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 10
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "^"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 20
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "SQR"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 20
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "ROOT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 20
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "*"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 30
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "/"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 30
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "BTM"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 30
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "\"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 40
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "MOD"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 50
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "+"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 60
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "-"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 60
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "BTA"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 60
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "BTS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 60
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "<>"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "><"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "<="
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = ">="
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "=<"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "=>"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = ">"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "<"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "="
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 70
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "NOT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 80
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "AND"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 90
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "OR"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 100
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "XOR"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 110
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "EQV"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 120
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "IMP"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 130
f = FREEFILE
FOR c = ASC("A") TO ASC("Z")
    i = i + 1: REDIM _PRESERVE math_OName(0 TO i)
    math_OName(i) = CHR$(c) + "#"
    REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1
NEXT c
FOR c = ASC("A") TO ASC("Z")
    i = i + 1
    REDIM _PRESERVE math_OName(0 TO i)
    math_OName(i) = CHR$(c) + "!"
    REDIM _PRESERVE math_PL(0 TO i)
    math_PL(i) = 1000
NEXT c
OPEN math_Filename FOR BINARY AS #f
counter = 0
FOR c = 0 TO 26
    GET #f, , length&
    t$ = SPC(length&)
    GET #f, , t$
    math_vars(c) = t$
NEXT c
CLOSE #f
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "DATE$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "TIME$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "COMMAND$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "WIKI"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "QB64"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "FORUMS"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "WEBCHAT"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "D2R$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "D2G$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "R2D$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "R2G$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "G2R$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "G2D$"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "RUN:"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
i = i + 1: REDIM _PRESERVE math_OName(0 TO i): math_OName(i) = "RETURN"
REDIM _PRESERVE math_PL(0 TO i): math_PL(i) = 1000
END SUB

FUNCTION EvaluateNumbers$ (p, math_num() AS STRING)
DIM n1 AS _FLOAT, n2 AS _FLOAT, n3 AS _FLOAT
SELECT CASE math_OName(p)
    CASE "PI"
        n1 = 3.14159265358979323846264338327950288##
    CASE "%"
        IF math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get percent of NULL string": EXIT FUNCTION
        n1 = (VAL(math_num(1))) / 100
    CASE "ARCCOS"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS from value >1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOS from value <-1, which is Invalid": EXIT FUNCTION
        IF n1 = 1 THEN EvaluateNumbers$ = "0": EXIT FUNCTION
        n1 = (2 * ATN(1)) - ATN(n1 / SQR(1 - n1 * n1))
    CASE "ARCSIN"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSIN of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSIN from value >1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSIN from value <-1, which is Invalid": EXIT FUNCTION
        n1 = ATN(n1 / SQR(1 - (n1 * n1)))
    CASE "ARCSEC"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC from value > 1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCSEC from value < -1, which is Invalid": EXIT FUNCTION
        n1 = ATN(n1 / SQR(1 - n1 * n1)) + (SGN(n1) - 1) * (2 * ATN(1))
    CASE "ARCCSC"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC from value >=1, which is Invalid": EXIT FUNCTION
        IF n1 < -1 THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCSC from value <-1, which is Invalid": EXIT FUNCTION
        n1 = ATN(1 / SQR(1 - n1 * n1)) + (SGN(n1) - 1) * (2 * ATN(1))
    CASE "ARCCOT"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ARCCOT of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        n1 = (2 * ATN(1)) - ATN(n1)
    CASE "SECH"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SECH of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 88.02969 OR (EXP(n1) + EXP(-n1)) = 0 THEN EvaluateNumbers$ = "ERROR - Bad SECH command": EXIT FUNCTION
        n1 = 2 / (EXP(n1) + EXP(-n1))
    CASE "CSCH"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get CSCH of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF n1 > 88.02969 OR (EXP(n1) - EXP(-n1)) = 0 THEN EvaluateNumbers$ = "ERROR - Bad CSCH command": EXIT FUNCTION
        n1 = 2 / (EXP(n1) - EXP(-n1))
    CASE "COTH"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COTH of NULL string": EXIT FUNCTION
        n1 = VAL(math_num(2))
        IF 2 * n1 > 88.02969 OR EXP(2 * n1) - 1 = 0 THEN EvaluateNumbers$ = "ERROR - Bad COTH command": EXIT FUNCTION
        n1 = (EXP(2 * n1) + 1) / (EXP(2 * n1) - 1)
    CASE "COS"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COS of NULL string": EXIT FUNCTION
        n1 = COS(VAL(math_num(2)))
    CASE "SIN"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SIN of NULL string": EXIT FUNCTION
        n1 = SIN(VAL(math_num(2)))
    CASE "TAN"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get TAN of NULL string": EXIT FUNCTION
        n1 = TAN(VAL(math_num(2)))
    CASE "LOG"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get LOG of NULL string": EXIT FUNCTION
        n1 = LOG(VAL(math_num(2)))
    CASE "EXP"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get EXP of NULL string": EXIT FUNCTION
        n1 = EXP(VAL(math_num(2)))
    CASE "ATN"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ATN of NULL string": EXIT FUNCTION
        n1 = ATN(VAL(math_num(2)))
    CASE "D2R"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Radian of NULL Degree value": EXIT FUNCTION
        n1 = 0.0174532925 * (VAL(math_num(2)))
    CASE "D2G"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Grad of NULL Degree string": EXIT FUNCTION
        n1 = 1.1111111111 * (VAL(math_num(2)))
    CASE "R2D"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Degree of NULL Radian string": EXIT FUNCTION
        n1 = 57.2957795 * (VAL(math_num(2)))
    CASE "R2G"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Grad of NULL Radian string": EXIT FUNCTION
        n1 = 0.015707963 * (VAL(math_num(2)))
    CASE "G2D"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Degree of NULL Gradian string": EXIT FUNCTION
        n1 = 0.9 * (VAL(math_num(2)))
    CASE "G2R"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get Radian of NULL Grad string": EXIT FUNCTION
        n1 = 63.661977237 * (VAL(math_num(2)))
    CASE "ABS"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ABS of NULL string": EXIT FUNCTION
        n1 = ABS(VAL(math_num(2)))
    CASE "SGN"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SGN of NULL string": EXIT FUNCTION
        n1 = SGN(VAL(math_num(2)))
    CASE "INT"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get INT of NULL string": EXIT FUNCTION
        n1 = INT(VAL(math_num(2)))
    CASE "_ROUND"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to _ROUND a NULL string": EXIT FUNCTION
        n1 = _ROUND(VAL(math_num(2)))
    CASE "FIX"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to FIX a NULL string": EXIT FUNCTION
        n1 = FIX(VAL(math_num(2)))
    CASE "SEC"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SEC of NULL string": EXIT FUNCTION
        n1 = COS(VAL(math_num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - COS value is 0, thus SEC is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "CSC"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get CSC of NULL string": EXIT FUNCTION
        n1 = SIN(VAL(math_num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - SIN value is 0, thus CSC is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "COT"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get COT of NULL string": EXIT FUNCTION
        n1 = COS(VAL(math_num(2)))
        IF n1 = 0 THEN EvaluateNumbers$ = "ERROR - TAN value is 0, thus COT is 1/0 which is Invalid": EXIT FUNCTION
        n1 = 1 / n1
    CASE "BTA"
        IF math_num(2) = "" OR math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - BTA": EXIT FUNCTION
        EvaluateNumbers$ = BTen$(math_num(1), "+", math_num(2)): EXIT FUNCTION
    CASE "BTS"
        IF math_num(2) = "" OR math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - BTS": EXIT FUNCTION
        EvaluateNumbers$ = BTen$(math_num(1), "-", math_num(2)): EXIT FUNCTION
    CASE "BTM"
        IF math_num(2) = "" OR math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - BTM": EXIT FUNCTION
        EvaluateNumbers$ = BTen$(math_num(1), "*", math_num(2)): EXIT FUNCTION
    CASE "^"
        IF math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to raise NULL string to exponent": EXIT FUNCTION
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to raise number to NULL exponent": EXIT FUNCTION
        n1 = VAL(math_num(1)) ^ VAL(math_num(2))
    CASE "SQR"
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get SQR of NULL string": EXIT FUNCTION
        IF VAL(math_num(2)) < 0 THEN EvaluateNumbers$ = "ERROR - Cannot take take SQR of numbers < 0.  I'm a computer, I have a poor imagination.": EXIT FUNCTION
        n1 = SQR(VAL(math_num(2)))
    CASE "ROOT"
        IF math_num(1) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get ROOT of a NULL string": EXIT FUNCTION
        IF math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to get NULL ROOT of a string": EXIT FUNCTION
        n1 = VAL(math_num(1)): n2 = VAL(math_num(2))
        IF n2 = 1 THEN EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1))): EXIT FUNCTION
        IF n2 = 0 THEN EvaluateNumbers$ = "ERROR - There is no such thing as a 0 ROOT of a number": EXIT FUNCTION
        IF n1 < 0 AND n2 MOD 2 = 0 AND n2 > 1 THEN EvaluateNumbers$ = "ERROR - Cannot take take an EVEN ROOT of numbers < 0.  I'm a computer, I have a poor imagination.": EXIT FUNCTION
        IF n1 < 0 AND n2 >= 1 THEN sign = -1: n1 = -n1 ELSE sign = 1
        n3 = 1## / n2
        IF n3 <> INT(n3) AND n2 < 1 THEN sign = SGN(n1): n1 = ABS(n1)
        n1 = sign * (n1 ^ n3)
    CASE "*"
        IF math_num(1) = "" OR math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to multiply NULL string ": EXIT FUNCTION
        n1 = VAL(math_num(1)) * VAL(math_num(2))
    CASE "/":
        IF math_num(1) = "" OR math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to divide NULL string ": EXIT FUNCTION
        IF VAL(math_num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(math_num(1)) / VAL(math_num(2))
    CASE "\"
        IF math_num(1) = "" OR math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to divide NULL string ": EXIT FUNCTION
        IF VAL(math_num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(math_num(1)) \ VAL(math_num(2))
    CASE "MOD"
        IF math_num(1) = "" OR math_num(2) = "" THEN EvaluateNumbers$ = "ERROR - Attemping to MOD with NULL string ": EXIT FUNCTION
        IF VAL(math_num(2)) = 0 THEN EvaluateNumbers$ = "ERROR - Division by 0": EXIT FUNCTION
        n1 = VAL(math_num(1)) MOD VAL(math_num(2))
    CASE "+"
        n1 = VAL(math_num(1)) + VAL(math_num(2))
    CASE "-"
        n1 = VAL(math_num(1)) - VAL(math_num(2))
    CASE "="
        n1 = VAL(math_num(1)) = VAL(math_num(2))
    CASE ">"
        n1 = VAL(math_num(1)) > VAL(math_num(2))
    CASE "<"
        n1 = VAL(math_num(1)) < VAL(math_num(2))
    CASE "<>", "><"
        n1 = VAL(math_num(1)) <> VAL(math_num(2))
    CASE "<=", "=<"
        n1 = VAL(math_num(1)) <= VAL(math_num(2))
    CASE ">=", "=>"
        n1 = VAL(math_num(1)) >= VAL(math_num(2))
    CASE "NOT"
        n1 = NOT VAL(math_num(2))
    CASE "AND"
        n1 = VAL(math_num(1)) AND VAL(math_num(2))
    CASE "OR"
        n1 = VAL(math_num(1)) OR VAL(math_num(2))
    CASE "XOR"
        n1 = VAL(math_num(1)) XOR VAL(math_num(2))
    CASE "EQV"
        n1 = VAL(math_num(1)) EQV VAL(math_num(2))
    CASE "IMP"
        n1 = VAL(math_num(1)) IMP VAL(math_num(2))
    CASE "ANS"
        n1 = VAL(math_vars(0))
    CASE ELSE
        EvaluateNumbers$ = "ERROR - Bad operation"
        FOR c = ASC("A") TO ASC("Z")
            IF math_OName(p) = CHR$(c) + "#" THEN EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(VAL(math_vars(c - 64))))): EXIT FUNCTION
        NEXT
END SELECT
EvaluateNumbers$ = RTRIM$(LTRIM$(STR$(n1)))
END FUNCTION

FUNCTION DWD$ (exp$)
t$ = exp$
DO
    bad = 0
    DO
        l = INSTR(t$, "++")
        IF l THEN
            t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2)
            bad = -1
        END IF
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "+-")
        IF l THEN
            t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2)
            bad = -1
        END IF
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "-+")
        IF l THEN
            t$ = LEFT$(t$, l - 1) + "-" + MID$(t$, l + 2):
            bad = -1
        END IF
    LOOP UNTIL l = 0
    DO
        l = INSTR(t$, "--")
        IF l THEN
            t$ = LEFT$(t$, l - 1) + "+" + MID$(t$, l + 2)
            bad = -1
        END IF
    LOOP UNTIL l = 0
LOOP UNTIL NOT bad
DWD$ = t$
VerifyString t$
END FUNCTION

SUB PreParse (e$)
DIM f AS _FLOAT
t$ = e$
j = 1
highflag = 0
lowflag = 0
DO
    comp$ = UCASE$(MID$(t$, j, 1))
    SELECT CASE comp$
        CASE "0" TO "9", ".", "(", ")"
            j = j + 1
        CASE ELSE
            good = 0
            FOR i = 1 TO UBOUND(OName)
                IF UCASE$(MID$(t$, j, LEN(math_OName(i)))) = math_OName(i) AND math_PL(i) > 250 THEN highflag = -1
            NEXT
            IF i <= UBOUND(Oname) THEN j = j + LEN(math_OName(i)) ELSE j = j + 1
    END SELECT
LOOP UNTIL j > LEN(t$)
IF highflag THEN ParseString t$
IF QuickReturn THEN e$ = t$: EXIT SUB
t$ = ""
FOR i = 1 TO LEN(e$)
    IF MID$(e$, i, 1) <> " " THEN t$ = t$ + MID$(e$, i, 1)
NEXT
t$ = UCASE$(t$)
IF t$ = "" THEN e$ = "0"
l = 0
DO
    l = INSTR(l + 1, t$, "(")
    IF l THEN c = c + 1
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(l + 1, t$, ")")
    IF l THEN c1 = c1 + 1
LOOP UNTIL l = 0
IF c <> c1 THEN e$ = "ERROR -- Bad Parenthesis:" + STR$(c) + "( vs" + STR$(c1) + ")": EXIT SUB
l = 0
DO
    l = INSTR(l + 1, t$, "NOT")
    IF l THEN
        l1 = INSTR(l + 1, t$, "AND")
        IF l1 = 0 OR (INSTR(l + 1, t$, "OR") > 0 AND INSTR(l + 1, t$, "OR") < l1) THEN l1 = INSTR(l + 1, t$, "OR")
        IF l1 = 0 OR (INSTR(l + 1, t$, "XOR") > 0 AND INSTR(l + 1, t$, "XOR") < l1) THEN l1 = INSTR(l + 1, t$, "XOR")
        IF l1 = 0 OR (INSTR(l + 1, t$, "EQV") > 0 AND INSTR(l + 1, t$, "EQV") < l1) THEN l1 = INSTR(l + 1, t$, "EQV")
        IF l1 = 0 OR (INSTR(l + 1, t$, "IMP") > 0 AND INSTR(l + 1, t$, "IMP") < l1) THEN l1 = INSTR(l + 1, t$, "IMP")
        IF l1 = 0 THEN l1 = LEN(t$) + 1
        t$ = LEFT$(t$, l - 1) + "(" + MID$(t$, l, l1 - l) + ")" + MID$(t$, l + l1 - l)
        l = l + 3
    END IF
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(l + 1, t$, "(")
    IF l AND l > 2 THEN
        good = 0
        FOR i = 1 TO UBOUND(OName)
            IF MID$(t$, l - LEN(math_OName(i)), LEN(math_OName(i))) = math_OName(i) AND math_PL(i) > 1 AND math_PL(i) <= 250 THEN good = -1: EXIT FOR
        NEXT
        IF NOT good THEN e$ = "ERROR - Improper operations before (.": EXIT SUB
        l = l + 1
    END IF
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(l + 1, t$, ")")
    IF l AND l < LEN(t$) THEN
        good = 0
        FOR i = 1 TO UBOUND(OName)
            IF MID$(t$, l + 1, LEN(math_OName(i))) = math_OName(i) AND math_PL(i) > 1 AND math_PL(i) <= 250 THEN good = -1: EXIT FOR
        NEXT
        IF NOT good THEN e$ = "ERROR - Improper operations after ).": EXIT SUB
        l = l + 1
    END IF
LOOP UNTIL l = 0 OR l = LEN(t$)
l = 0
DO
    l = INSTR(t$, "&H")
    IF l THEN
        pl = l + 1
        finished = 0
        DO
            pl = pl + 1
            comp$ = MID$(t$, e, 1)
            SELECT CASE comp$
                CASE "0" TO "9", "A" TO "F"
                CASE ELSE
                    good = 0
                    FOR i = 1 TO UBOUND(OName)
                        IF MID$(t$, e, LEN(math_OName(i))) = math_OName(i) AND math_PL(i) > 1 AND math_PL(i) <= 250 THEN good = -1: EXIT FOR
                    NEXT
                    IF NOT good THEN e$ = "ERROR - Improper &H value. (" + comp$ + ")": EXIT SUB
                    pl = pl - 1
                    finished = -1
            END SELECT
        LOOP UNTIL finished OR pl = LEN(t$)
        t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(VAL(MID$(t$, l, pl - l + 1))))) + MID$(t$, pl + 1)
    END IF
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(t$, "&B")
    IF l THEN
        pl = l + 1
        finished = 0
        DO
            pl = pl + 1
            comp$ = MID$(t$, e, 1)
            SELECT CASE comp$
                CASE "0", "1"
                CASE ELSE
                    good = 0
                    FOR i = 1 TO UBOUND(OName)
                        IF MID$(t$, e, LEN(math_OName(i))) = math_OName(i) AND math_PL(i) > 1 AND math_PL(i) <= 250 THEN good = -1: EXIT FOR
                    NEXT
                    IF NOT good THEN e$ = "ERROR - Improper &B value. (" + comp$ + ")": EXIT SUB
                    pl = pl - 1
                    finished = -1
            END SELECT
        LOOP UNTIL finished OR pl = LEN(t$)
        bin$ = MID$(t$, l + 2, pl - l - 1)
        FOR i = 1 TO LEN(bin$)
            IF MID$(bin$, i, 1) = "1" THEN f = f + 2 ^ (LEN(bin$) - i)
        NEXT
        t$ = LEFT$(t$, l - 1) + LTRIM$(RTRIM$(STR$(f))) + MID$(t$, pl + 1)
    END IF
LOOP UNTIL l = 0
t$ = N2S(t$)
VerifyString t$
e$ = t$
END SUB

SUB ParseString (e$)
t$ = e$
IF UCASE$(LEFT$(t$, 4)) = "RUN:" THEN
    l = INSTR(UCASE$(t$), "RETURN")
    IF l = 0 THEN e$ = "ERROR -- No RETURN after RUN": QuickReturn = -1: EXIT SUB
    tempfile$ = "MathProcess" + DATE$
    f = FREEFILE
    OPEN tempfile$ + ".txt" FOR OUTPUT AS #f
    PRINT #f, MID$(t$, 5, l - 5)
    PRINT #f, "OPEN " + CHR$(34) + tempfile$ + ".txt" + CHR$(34) + " FOR OUTPUT AS #1"
    PRINT #f, "PRINT #1, " + MID$(t$, l + 7)
    PRINT #f, "CLOSE"
    PRINT #f, "SYSTEM "
    CLOSE #f
    SHELL _HIDE "QB64.exe -c " + tempfile$ + ".txt"
    SHELL _HIDE tempfile$ + ".exe"
    OPEN tempfile$ + ".txt" FOR INPUT AS #f
    LINE INPUT #f, e$
    CLOSE #f
    IF _FILEEXISTS(tempfile$ + ".txt") THEN KILL tempfile$ + ".txt"
    IF _FILEEXISTS(tempfile$ + ".exe") THEN KILL tempfile$ + ".exe"
    QuickReturn = -1
    EXIT SUB
END IF
QUI = 0
FOR c = ASC("A") TO ASC("Z")
    IF INSTR(UCASE$(t$), CHR$(c) + "!") THEN
        f = FREEFILE
        IF _FILEEXISTS(math_Dirname + CHR$(c) + "!.txt") THEN
            QUI = -1
            OPEN math_Dirname + CHR$(c) + "!.txt" FOR INPUT AS #f
            t$ = ""
            DO UNTIL EOF(f)
                LINE INPUT #f, t1$
                t2$ = t2$ + t1$ + CHR$(13)
            LOOP
            CLOSE #f
        ELSE
            e$ = "ERROR --" + math_Dirname + CHR$(c) + "!.txt is not a valid quickload file.": math_QuickReturn = -1: EXIT SUB
        END IF
    END IF
NEXT
IF QUI THEN e$ = t2$: math_QuickReturn = -1: EXIT SUB
l = 0
DO
    l = INSTR(UCASE$(t$), "DATE$")
    IF l THEN
        t$ = LEFT$(t$, l - 1) + DATE$ + MID$(t$, l + 5)
    END IF
LOOP UNTIL l = 0
l = 0
DO
    l = INSTR(UCASE$(t$), "TIME$")
    IF l THEN
        t$ = LEFT$(t$, l - 1) + TIME$ + MID$(t$, l + 5)
    END IF
LOOP UNTIL l = 0
IF INSTR(UCASE$(t$), "WIKI") THEN SHELL _HIDE "http://qb64.net/wiki/index.php?title=Main_Page"
IF INSTR(UCASE$(t$), "QB64") THEN SHELL _HIDE "http://www.qb64.net/"
IF INSTR(UCASE$(t$), "FORUMS") THEN SHELL _HIDE "http://www.qb64.net/forum/index.php"
IF INSTR(UCASE$(t$), "WEBCHAT") THEN SHELL _HIDE "http://webchat.freenode.net/"
IF INSTR(UCASE$(t$), "COMMAND$") THEN t$ = LEFT$(t$, l - 1) + COMMAND$ + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "D2R$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION D2R## (x AS _FLOAT): D2R = 0.0174532925 * x: END FUNCTION" + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "D2G$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION D2G## (x AS _FLOAT): D2G = 1.1111111111 * x: END FUNCTION" + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "R2G$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION R2D## (x AS _FLOAT): R2D = 57.2957795 * x: END FUNCTION" + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "R2G$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION R2G## (x AS _FLOAT): R2G = 0.015707963 * x: END FUNCTION" + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "G2D$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION G2D## (x AS _FLOAT): G2D = 0.9 * x: END FUNCTION" + MID$(t$, l + 5)
IF INSTR(UCASE$(t$), "G2R$") THEN t$ = LEFT$(t$, l - 1) + "FUNCTION G2R## (x AS _FLOAT): G2R = 63.661977237 * x: END FUNCTION" + MID$(t$, l + 5)
l = 0
DO
    l = INSTR(UCASE$(t$), "D2R$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "D2G$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "D2G$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "R2G$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "R2D$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "G2D$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "G2R$")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "WIKI")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "QB64")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "FORUMS")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "WEBCHAT")
    IF l = 0 THEN l = INSTR(UCASE$(t$), "COMMAND$")
    IF l THEN
        t$ = LEFT$(t$, l - 1) + MID$(t$, l + 5)
    END IF
LOOP UNTIL l = 0
e$ = t$
math_QuickReturn = -1
END SUB

SUB VerifyString (t$)
j = 1
DO
    comp$ = MID$(t$, j, 1)
    SELECT CASE comp$
        CASE "0" TO "9", ".", "(", ")"
            j = j + 1
        CASE ELSE
            good = 0
            FOR i = 1 TO UBOUND(OName)
                IF MID$(t$, j, LEN(math_OName(i))) = math_OName(i) THEN good = -1: EXIT FOR
            NEXT
            IF NOT good THEN t$ = "ERROR - Bad Operational value. (" + comp$ + ")": EXIT SUB
            j = j + LEN(math_OName(i))
    END SELECT
LOOP UNTIL j > LEN(t$)
END SUB


FUNCTION BTen$ (InTop AS STRING, Op AS STRING, InBot AS STRING)
InTop = LTRIM$(RTRIM$(InTop))
InBot = LTRIM$(RTRIM$(InBot))
l = INSTR(InTop, "-")
IF l = 0 THEN l = INSTR(InTop, "+")
IF l = 0 THEN InTop = "+" + InTop
l = INSTR(InBot, "-")
IF l = 0 THEN l = INSTR(InBot, "+")
IF l = 0 THEN InBot = "+" + InBot
l = INSTR(InTop, ".")
IF l = 0 THEN InTop = InTop + "."
l = INSTR(InBot, ".")
IF l = 0 THEN InBot = InBot + "."
IF Op$ = "-" THEN
    Op$ = "+"
    IF MID$(InBot, 1, 1) = "-" THEN
        MID$(InBot, 1, 1) = "+"
    ELSE
        MID$(InBot, 1, 1) = "-"
    END IF
END IF
TDP& = Check&(10, InTop$)
BDP& = Check&(10, InBot$)
IF TDP& < 0 OR BDP& < 0 THEN EXIT FUNCTION
TSign% = Check&(11, InTop$)
BSign% = Check&(11, InBot$)
IF Op$ = CHR$(43) OR Op$ = CHR$(45) THEN
    Temp& = 9
ELSEIF Op$ = CHR$(42) OR Op$ = CHR$(50) THEN
    Temp& = 7
ELSE
    EXIT FUNCTION
END IF
LSA& = TDP& - 2
TLS& = LSA& \ Temp&
IF LSA& MOD Temp& > 0 THEN
    TLS& = TLS& + 1
    DO WHILE (TLPad& + LSA&) MOD Temp& > 0
        TLPad& = TLPad& + 1
    LOOP
END IF
LSA& = BDP& - 2
BLS& = LSA& \ Temp&
IF LSA& MOD Temp& > 0 THEN
    BLS& = BLS& + 1
    DO WHILE (BLPad& + LSA&) MOD Temp& > 0
        BLPad& = BLPad& + 1
    LOOP
END IF
IF TLS& >= BLS& THEN LSA& = TLS& ELSE LSA& = BLS&
RSA& = LEN(InTop$) - TDP&
TRS& = RSA& \ Temp&
IF RSA& MOD Temp& > 0 THEN
    TRS& = TRS& + 1
    DO WHILE (TRPad& + RSA&) MOD Temp& > 0
        TRPad& = TRPad& + 1
    LOOP
END IF
RSA& = LEN(InBot$) - BDP&
BRS& = RSA& \ Temp&
IF RSA& MOD Temp& > 0 THEN
    BRS& = BRS& + 1
    DO WHILE (BRPad& + RSA&) MOD Temp& > 0
        BRPad& = BRPad& + 1
    LOOP
END IF
IF TRS& >= BRS& THEN RSA& = TRS& ELSE RSA& = BRS&
IF Op$ = CHR$(43) OR Op$ = CHR$(45) THEN
    REDIM Result(1 TO (LSA& + RSA&)) AS LONG
    IF (Op$ = CHR$(43) AND TSign% = BSign%) OR (Op$ = CHR$(45) AND TSign% <> BSign%) THEN
        FOR I& = 1 TO LSA&
            IF I& <= (LSA& - TLS&) THEN
            ELSEIF I& = (1 + LSA& - TLS&) THEN
                Result(I&) = VAL(MID$(InTop$, 2, (9 - TLPad&)))
                TDP& = 11 - TLPad&
            ELSE
                Result(I&) = VAL(MID$(InTop$, TDP&, 9))
                TDP& = TDP& + 9
            END IF
            IF I& <= (LSA& - BLS&) THEN
            ELSEIF I& = (1 + LSA& - BLS&) THEN
                Result(I&) = Result(I&) + VAL(MID$(InBot$, 2, (9 - BLPad&)))
                BDP& = 11 - BLPad&
            ELSE
                Result(I&) = Result(I&) + VAL(MID$(InBot$, BDP&, 9))
                BDP& = BDP& + 9
            END IF
        NEXT I&
        TDP& = TDP& + 1: BDP& = BDP& + 1
        FOR I& = (LSA& + 1) TO (LSA& + RSA&)
            IF I& > (LSA& + TRS&) THEN
            ELSEIF I& = (LSA& + TRS&) THEN
                Result(I&) = (10 ^ TRPad&) * VAL(RIGHT$(InTop$, (9 - TRPad&)))
            ELSE
                Result(I&) = VAL(MID$(InTop$, TDP&, 9))
                TDP& = TDP& + 9
            END IF
            IF I& > (LSA& + BRS&) THEN
            ELSEIF I& = (LSA& + BRS&) THEN
                Result(I&) = Result(I&) + (10 ^ BRPad&) * VAL(RIGHT$(InBot$, (9 - BRPad&)))
            ELSE
                Result(I&) = Result(I&) + VAL(MID$(InBot$, BDP&, 9))
                BDP& = BDP& + 9
            END IF
        NEXT I&
        FOR I& = (LSA& + RSA&) TO 2 STEP -1
            IF Result(I&) >= 1000000000 THEN
                Result(I& - 1) = Result(I& - 1) + 1
                Result(I&) = Result(I&) - 1000000000
            END IF
        NEXT I&
        IF TSign% = 1 THEN RetStr$ = CHR$(43) ELSE RetStr$ = CHR$(45)
    ELSE
        IF TDP& > BDP& THEN
            Compare& = 1
        ELSEIF TDP& < BDP& THEN
            Compare& = -1
        ELSE
            IF LEN(InTop$) > LEN(InBot$) THEN Compare& = LEN(InBot$) ELSE Compare& = LEN(InTop$)
            FOR I& = 2 TO Compare&
                IF VAL(MID$(InTop$, I&, 1)) > VAL(MID$(InBot$, I&, 1)) THEN
                    Compare& = 1
                    EXIT FOR
                ELSEIF VAL(MID$(InTop$, I&, 1)) < VAL(MID$(InBot$, I&, 1)) THEN
                    Compare& = -1
                    EXIT FOR
                END IF
            NEXT I&
            IF Compare& > 1 THEN
                IF LEN(InTop$) > LEN(InBot$) THEN
                    Compare& = 1
                ELSEIF LEN(InTop$) < LEN(InBot$) THEN
                    Compare& = -1
                ELSE
                    Compare& = 0
                END IF
            END IF
        END IF
        IF Compare& = 1 THEN
            Result(1) = VAL(MID$(InTop$, 2, (9 - TLPad&)))
            TDP& = 11 - TLPad&
            FOR I& = 2 TO LSA&
                Result(I&) = VAL(MID$(InTop$, TDP&, 9))
                TDP& = TDP& + 9
            NEXT I&
            TDP& = TDP& + 1
            FOR I& = (LSA& + 1) TO (LSA& + TRS& - 1)
                Result(I&) = VAL(MID$(InTop$, TDP&, 9))
                TDP& = TDP& + 9
            NEXT I&
            Result(LSA& + TRS&) = 10& ^ TRPad& * VAL(RIGHT$(InTop$, (9 - TRPad&)))
            BDP& = (LEN(InBot$) - 17) + BRPad&
            FOR I& = (LSA& + BRS&) TO (1 + LSA& - BLS&) STEP -1
                IF I& = LSA& THEN BDP& = BDP& - 1
                IF I& = (LSA& + BRS&) THEN
                    Temp& = (10& ^ BRPad&) * VAL(RIGHT$(InBot$, (9 - BRPad&)))
                ELSEIF I& = (1 + LSA& - BLS&) THEN
                    Temp& = VAL(MID$(InBot$, 2, (9 - BLPad&)))
                ELSE
                    Temp& = VAL(MID$(InBot$, BDP&, 9))
                    BDP& = BDP& - 9
                END IF
                IF Result(I&) < Temp& THEN
                    FOR J& = (I& - 1) TO 1 STEP -1
                        IF Result(J&) = 0 THEN
                            Result(J&) = 999999999
                        ELSE
                            Result(J&) = Result(J&) - 1
                            EXIT FOR
                        END IF
                    NEXT J&
                    Result(I&) = Result(I&) + 1000000000
                END IF
                Result(I&) = Result(I&) - Temp&
            NEXT I&
            IF TSign% = 1 THEN RetStr$ = CHR$(43) ELSE RetStr$ = CHR$(45)
        ELSEIF Compare& = -1 THEN
            Result(1) = VAL(MID$(InBot$, 2, (9 - BLPad&)))
            BDP& = 11 - BLPad&
            FOR I& = 2 TO LSA&
                Result(I&) = VAL(MID$(InBot$, BDP&, 9))
                BDP& = BDP& + 9
            NEXT I&
            BDP& = BDP& + 1
            FOR I& = (LSA& + 1) TO (LSA& + BRS& - 1)
                Result(I&) = VAL(MID$(InBot$, BDP&, 9))
                BDP& = BDP& + 9
            NEXT I&
            Result(LSA& + BRS&) = 10& ^ BRPad& * VAL(RIGHT$(InBot$, (9 - BRPad&)))
            TDP& = (LEN(InTop$) - 17) + TRPad&
            FOR I& = (LSA& + TRS&) TO (1 + LSA& - TLS&) STEP -1
                IF I& = LSA& THEN TDP& = TDP& - 1
                IF I& = (LSA& + TRS&) THEN
                    Temp& = (10& ^ TRPad&) * VAL(RIGHT$(InTop$, (9 - TRPad&)))
                ELSEIF I& = (1 + LSA& - TLS&) THEN
                    Temp& = VAL(MID$(InTop$, 2, (9 - TLPad&)))
                ELSE
                    Temp& = VAL(MID$(InTop$, TDP&, 9))
                    TDP& = TDP& - 9
                END IF
                IF Result(I&) < Temp& THEN
                    FOR J& = (I& - 1) TO 1 STEP -1
                        IF Result(J&) = 0 THEN
                            Result(J&) = 999999999
                        ELSE
                            Result(J&) = Result(J&) - 1
                            EXIT FOR
                        END IF
                    NEXT J&
                    Result(I&) = Result(I&) + 1000000000
                END IF
                Result(I&) = Result(I&) - Temp&
            NEXT I&
            IF BSign% = 1 THEN RetStr$ = CHR$(43) ELSE RetStr$ = CHR$(45)
        ELSE
            LSA& = 1
            RSA& = 1
            RetStr$ = CHR$(43)
        END IF
    END IF
    RetStr$ = RetStr$ + LTRIM$(STR$(Result(1)))
    FOR I& = 2 TO LSA&
        RetStr$ = RetStr$ + RIGHT$(STRING$(8, 48) + LTRIM$(STR$(Result(I&))), 9)
    NEXT I&
    RetStr$ = RetStr$ + CHR$(46)
    FOR I& = (LSA& + 1) TO (LSA& + RSA&)
        RetStr$ = RetStr$ + RIGHT$(STRING$(8, 48) + LTRIM$(STR$(Result(I&))), 9)
    NEXT I&
    ERASE Result
ELSEIF Op$ = CHR$(42) THEN
    REDIM TArray(1 TO (LSA& + RSA&)) AS LONG
    REDIM BArray(1 TO (LSA& + RSA&)) AS LONG
    REDIM ResDBL(0 TO (LSA& + RSA&)) AS DOUBLE
    FOR I& = 1 TO LSA&
        IF I& <= (LSA& - TLS&) THEN
        ELSEIF I& = (1 + LSA& - TLS&) THEN
            TArray(I&) = VAL(MID$(InTop$, 2, (7 - TLPad&)))
            TDP& = 9 - TLPad&
        ELSE
            TArray(I&) = VAL(MID$(InTop$, TDP&, 7))
            TDP& = TDP& + 7
        END IF
        IF I& <= (LSA& - BLS&) THEN
        ELSEIF I& = (1 + LSA& - BLS&) THEN
            BArray(I&) = VAL(MID$(InBot$, 2, (7 - BLPad&)))
            BDP& = 9 - BLPad&
        ELSE
            BArray(I&) = VAL(MID$(InBot$, BDP&, 7))
            BDP& = BDP& + 7
        END IF
    NEXT I&
    TDP& = TDP& + 1
    BDP& = BDP& + 1
    FOR I& = (LSA& + 1) TO (LSA& + RSA&)
        IF I& > (LSA& + TRS&) THEN
        ELSEIF I& = (LSA& + TRS&) THEN
            TArray(I&) = 10 ^ TRPad& * VAL(RIGHT$(InTop$, (7 - TRPad&)))
        ELSE
            TArray(I&) = VAL(MID$(InTop$, TDP&, 7))
            TDP& = TDP& + 7
        END IF
        IF I& > (LSA& + BRS&) THEN
        ELSEIF I& = (LSA& + BRS&) THEN
            BArray(I&) = 10 ^ BRPad& * VAL(RIGHT$(InBot$, (7 - BRPad&)))
        ELSE
            BArray(I&) = VAL(MID$(InBot$, BDP&, 7))
            BDP& = BDP& + 7
        END IF
    NEXT I&
    FOR I& = (LSA& + TRS&) TO (1 + LSA& - TLS&) STEP -1
        FOR J& = (LSA& + BRS&) TO (1 + LSA& - BLS&) STEP -1
            Temp# = 1# * TArray(I&) * BArray(J&)
            IF (I& + J&) MOD 2 = 0 THEN
                TL& = INT(Temp# / 10000000)
                TR& = Temp# - 10000000# * TL&
                ResDBL(((I& + J&) \ 2) - 1) = ResDBL(((I& + J&) \ 2) - 1) + TL&
                ResDBL((I& + J&) \ 2) = ResDBL((I& + J&) \ 2) + 10000000# * TR&
            ELSE
                ResDBL((I& + J&) \ 2) = ResDBL((I& + J&) \ 2) + Temp#
            END IF
            IF ResDBL((I& + J&) \ 2) >= 100000000000000# THEN
                Temp# = ResDBL((I& + J&) \ 2)
                TL& = INT(Temp# / 100000000000000#)
                ResDBL(((I& + J&) \ 2) - 1) = ResDBL(((I& + J&) \ 2) - 1) + TL&
                ResDBL((I& + J&) \ 2) = Temp# - 100000000000000# * TL&
            END IF
        NEXT J&
    NEXT I&
    ERASE TArray, BArray
    IF (TSign% * BSign%) = 1 THEN
        RetStr$ = CHR$(43)
    ELSE
        RetStr$ = CHR$(45)
    END IF
    RetStr$ = RetStr$ + LTRIM$(STR$(ResDBL(0)))
    FOR I& = 1 TO (LSA&)
        RetStr$ = RetStr$ + RIGHT$(STRING$(13, 48) + LTRIM$(STR$(ResDBL(I&))), 14)
    NEXT I&
    RetStr$ = LEFT$(RetStr$, LEN(RetStr$) - 7) + CHR$(46) + RIGHT$(RetStr$, 7)
    FOR I& = (LSA& + 1) TO (LSA& + RSA&)
        RetStr$ = RetStr$ + RIGHT$(STRING$(13, 48) + LTRIM$(STR$(ResDBL(I&))), 14)
    NEXT I&
    ERASE ResDBL
ELSEIF Op$ = CHR$(50) THEN
    REDIM IArray(1 TO (LSA& + RSA&)) AS LONG
    REDIM ResDBL(0 TO (LSA& + RSA&)) AS DOUBLE
    FOR I& = 1 TO LSA&
        IF I& <= (LSA& - TLS&) THEN
        ELSEIF I& = (1 + LSA& - TLS&) THEN
            IArray(I&) = VAL(MID$(InTop$, 2, (7 - TLPad&)))
            TDP& = 9 - TLPad&
        ELSE
            IArray(I&) = VAL(MID$(InTop$, TDP&, 7))
            TDP& = TDP& + 7
        END IF
    NEXT I&
    TDP& = TDP& + 1
    FOR I& = (LSA& + 1) TO (LSA& + RSA&)
        IF I& > (LSA& + TRS&) THEN
        ELSEIF I& = (LSA& + TRS&) THEN
            IArray(I&) = 10 ^ TRPad& * VAL(RIGHT$(InTop$, (7 - TRPad&)))
        ELSE
            IArray(I&) = VAL(MID$(InTop$, TDP&, 7))
            TDP& = TDP& + 7
        END IF
    NEXT I&
    FOR I& = (LSA& + TRS&) TO 1 STEP -1
        FOR J& = I& TO 1 STEP -1
            Temp# = 1# * IArray(I&) * IArray(J&)
            IF I& <> J& THEN Temp# = Temp# * 2
            IF (I& + J&) MOD 2 = 0 THEN
                TL& = INT(Temp# / 10000000)
                TR& = Temp# - 10000000# * TL&
                ResDBL(((I& + J&) \ 2) - 1) = ResDBL(((I& + J&) \ 2) - 1) + TL&
                ResDBL((I& + J&) \ 2) = ResDBL((I& + J&) \ 2) + 10000000# * TR&
            ELSE
                ResDBL((I& + J&) \ 2) = ResDBL((I& + J&) \ 2) + Temp#
            END IF
            IF ResDBL((I& + J&) \ 2) >= 100000000000000# THEN
                Temp# = ResDBL((I& + J&) \ 2)
                TL& = INT(Temp# / 100000000000000#)
                ResDBL(((I& + J&) \ 2) - 1) = ResDBL(((I& + J&) \ 2) - 1) + TL&
                ResDBL((I& + J&) \ 2) = Temp# - 100000000000000# * TL&
            END IF
        NEXT J&
    NEXT I&
    ERASE IArray
    IF (TSign% * BSign%) = 1 THEN RetStr$ = CHR$(43) ELSE RetStr$ = CHR$(45)
    RetStr$ = RetStr$ + LTRIM$(STR$(ResDBL(0)))
    FOR I& = 1 TO (LSA&)
        RetStr$ = RetStr$ + RIGHT$(STRING$(13, 48) + LTRIM$(STR$(ResDBL(I&))), 14)
    NEXT I&
    RetStr$ = LEFT$(RetStr$, LEN(RetStr$) - 7) + CHR$(46) + RIGHT$(RetStr$, 7)
    ERASE ResDBL
END IF
DO WHILE MID$(RetStr$, 2, 1) = CHR$(48) AND MID$(RetStr$, 3, 1) <> CHR$(46)
    RetStr$ = LEFT$(RetStr$, 1) + RIGHT$(RetStr$, LEN(RetStr$) - 2)
LOOP
DO WHILE RIGHT$(RetStr$, 1) = CHR$(48) AND RIGHT$(RetStr$, 2) <> CHR$(46) + CHR$(48)
    RetStr$ = LEFT$(RetStr$, LEN(RetStr$) - 1)
LOOP
IF MID$(RetStr$, 1, 1) = "+" THEN MID$(RetStr$, 1, 1) = " "
DO
    r$ = RIGHT$(RetStr$, 1)
    IF r$ = "0" THEN RetStr$ = LEFT$(RetStr$, LEN(RetStr$) - 1)
LOOP UNTIL r$ <> "0"
r$ = RIGHT$(RetStr$, 1)
IF r$ = "." THEN RetStr$ = LEFT$(RetStr$, LEN(RetStr$) - 1)
BTen$ = RetStr$
END FUNCTION

FUNCTION Check& (Op AS LONG, InString AS STRING)
RetVal& = LEN(InString$)
SELECT CASE Op&
    CASE 10
        IF RetVal& = 0 THEN
            RetVal& = -1
        ELSE
            SELECT CASE ASC(LEFT$(InString$, 1))
                CASE 43, 45
                    FOR I& = 2 TO RetVal&
                        SELECT CASE ASC(MID$(InString$, I&, 1))
                            CASE 46
                                IF DPC% > 0 THEN
                                    RetVal& = 0 - I&
                                    EXIT FOR
                                ELSE
                                    DPC% = DPC% + 1
                                    RetVal& = I&
                                END IF
                            CASE 48 TO 57
                            CASE ELSE
                                RetVal& = 0 - I&
                                EXIT FOR
                        END SELECT
                    NEXT I&
                CASE ELSE
                    RetVal& = -1
            END SELECT
            IF DPC% = 0 AND RetVal& > 0 THEN
                RetVal& = 0 - RetVal&
            ELSEIF RetVal& = 2 THEN
                InString$ = LEFT$(InString$, 1) + CHR$(48) + RIGHT$(InString$, LEN(InString$) - 1)
                RetVal& = RetVal& + 1
            END IF
            IF RetVal& = LEN(InString$) THEN InString$ = InString$ + CHR$(48)
            DO WHILE ASC(RIGHT$(InString$, 1)) = 48 AND RetVal& < (LEN(InString$) - 1)
                InString$ = LEFT$(InString$, LEN(InString$) - 1)
            LOOP
            DO WHILE ASC(MID$(InString$, 2, 1)) = 48 AND RetVal& > 3
                InString$ = LEFT$(InString$, 1) + RIGHT$(InString$, LEN(InString$) - 2)
                RetVal& = RetVal& - 1
            LOOP
        END IF
    CASE 11
        IF RetVal& = 0 THEN RetVal& = -64
        FOR I& = 1 TO RetVal&
            SELECT CASE ASC(MID$(InString$, I&, 1))
                CASE 32
                    RetVal& = 64
                CASE 43
                    RetVal& = 1
                    EXIT FOR
                CASE 45
                    RetVal& = -1
                    EXIT FOR
                CASE 241
                    RetVal& = 0
                    EXIT FOR
                CASE ELSE
                    RetVal& = 64
                    EXIT FOR
            END SELECT
        NEXT I&
    CASE ELSE
        RetVal& = 0 - Op&
END SELECT
Check& = RetVal&
END FUNCTION

FUNCTION N2S$ (exp$)
t$ = LTRIM$(RTRIM$(exp$))
IF LEFT$(t$, 1) = "-" THEN
    sign$ = "-"
    t$ = MID$(t$, 2)
END IF
dp = INSTR(t$, "D+"): dm = INSTR(t$, "D-")
ep = INSTR(t$, "E+"): em = INSTR(t$, "E-")
check1 = SGN(dp) + SGN(dm) + SGN(ep) + SGN(em)
IF check1 < 1 OR check1 > 1 THEN N2S = exp$: EXIT SUB
SELECT CASE l
    CASE IS < dp
        l = dp
    CASE IS < dm
        l = dm
    CASE IS < ep
        l = ep
    CASE IS < em
        l = em
END SELECT
l$ = LEFT$(t$, l - 1)
r$ = MID$(t$, l + 1)
r&& = VAL(r$)
IF INSTR(l$, ".") THEN
    IF r&& > 0 THEN
        r&& = r&& - LEN(l$) + 2
    ELSE
        r&& = r&& + 1
    END IF
    l$ = LEFT$(l$, 1) + MID$(l$, 3)
END IF
SELECT CASE r&&
    CASE 0
    CASE IS < 0
        FOR i = 1 TO -r&&
            l$ = "0" + l$
        NEXT
        l$ = "0." + l$
    CASE ELSE
        FOR i = 1 TO r&&
            l$ = l$ + "0"
        NEXT
END SELECT
N2S$ = sign$ + l$
END SUB