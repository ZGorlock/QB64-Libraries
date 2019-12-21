'dimension shared variables
DIM SHARED math_QuickReturn AS INTEGER
DIM SHARED math_DirName AS STRING
DIM SHARED math_FileName AS STRING

'dimension shared arrays
REDIM SHARED math_OName(0 TO 0) AS STRING
REDIM SHARED math_PL(0 TO 0) AS INTEGER
REDIM SHARED math_vars(1 TO 26) AS STRING

'set shared strings
math_DirName = "internal/MathEval/"
math_FileName = "internal/MathEval/Math Evaluator User Variables.bin"

'set order of operations
Set_OrderOfOperations