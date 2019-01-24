' size in bytes (STRING) the data type needs
CONST IntegerLength = 2

' how many of each type will we store
CONST ARRAYSIZE = 100

' determine memory needed by multiplying arraysize by datatypesize
CONST IntegerArrayLength = IntegerLength * ARRAYSIZE

DEFSTR S ' all variables whose name starts with s is a string
TYPE IntList
  'dim for 10 integers
  IntegerArray AS STRING * INTEGERARRAYLENGTH
END TYPE
DECLARE FUNCTION Array$ (Dim1, Dim2, Dim3, Value$)
REDIM SHARED intList(1 TO 1000) AS IntList
' The Mstuff array will be treated as a
'   3-dimensional array containing mixed data types.

' Create various values...
MyInteger% = 32000

' Set various array elements with the values above.
' The Value$ parameter accepts the values.
sRet = Array$(IntegerArray, 2, 3, STR$(MyInteger%))

' Get the values back.
' Array$ returns the values if the Value$ parameter = ""
NewInteger% = VAL(Array$(IntegerArray, 2, 3, ""))
NewLong& = VAL(Array$(LongArray, 9, 1, ""))
NewSingle! = VAL(Array$(SingleArray, 2, 7, ""))
NewDouble# = VAL(Array$(DoubleArray, 1, 1, ""))
CLS

' Print them
PRINT "   Stored:"; MyInteger%, MyLong&, MySingle!, MyDouble#
PRINT "Retrieved:"; NewInteger%, NewLong&, NewSingle!, NewDouble#

FUNCTION Array$ (Dim1, Dim2, Dim3, Value$)
  SELECT CASE Value$
    CASE ""
      SELECT CASE Dim1
        CASE IntegerArray
          Array$ = LTRIM$(STR$(CVI(MID$(Mstuff(Dim2).IntegerArray, Dim3 * 2 - 1, 2))))
        CASE LongArray
          Array$ = LTRIM$(STR$(CVL(MID$(Mstuff(Dim2).LongArray, Dim3 * 4 - 1, 4))))
        CASE SingleArray
          Array$ = LTRIM$(STR$(CVS(MID$(Mstuff(Dim2).SingleArray, Dim3 * 4 - 1, 4))))
        CASE DoubleArray
          Array$ = LTRIM$(STR$(CVD(MID$(Mstuff(Dim2).DoubleArray, Dim3 * 8 - 1, 8))))
      END SELECT
    CASE ELSE
      SELECT CASE Dim1
        CASE IntegerArray
          MID$(Mstuff(Dim2).IntegerArray, Dim3 * 2 - 1, 2) = MKI$(VAL(Value$))
        CASE LongArray
          MID$(Mstuff(Dim2).LongArray, Dim3 * 4 - 1, 4) = MKL$(VAL(Value$))
        CASE SingleArray
          MID$(Mstuff(Dim2).SingleArray, Dim3 * 4 - 1, 4) = MKS$(VAL(Value$))
        CASE DoubleArray
          MID$(Mstuff(Dim2).DoubleArray, Dim3 * 8 - 1, 8) = MKD$(VAL(Value$))
      END SELECT
  END SELECT

END FUNCTION

