' how many of each type will we store
CONST ARRAYSIZE = 100

' The following constants will be used to refer to
'   elements of the user defined type (Dim1).
CONST IntegerArray = 1
CONST LongArray = 2
CONST SingleArray = 3
CONST DoubleArray = 4
CONST StringArray = 5

' size in bytes (STRING) each data type needs
CONST IntegerLength = 2
CONST LongLength = 4
CONST SingleLength = 4
CONST DoubleLength = 8
CONST StringLength = 8

' determine memory needed by multiplying arraysize by datatypesize
CONST INTEGERARRAYLENGTH = IntegerLength * ARRAYSIZE
CONST LONGARRAYLENGTH = LongLength * ARRAYSIZE
CONST SINGLEARRAYLENGTH = SingleLength * ARRAYSIZE
CONST DOUBLEARRAYLENGTH = DoubleLength * ARRAYSIZE
CONST STRINGARRAYLENGTH = StringLength * ARRAYSIZE

DEFSTR Z 'variables starting with z are strings
TYPE MyStuff
  'dim for 10 integers
  IntegerArray AS STRING * INTEGERARRAYLENGTH
  'dim for 10 long integers
  LongArray AS STRING * LONGARRAYLENGTH
  'dim for 10 single precision values
  SingleArray AS STRING * SINGLEARRAYLENGTH
  'dim for 10 double precision values
  DoubleArray AS STRING * DOUBLEARRAYLENGTH
  'dim for 10 double precision values
  StringArray AS STRING * STRINGARRAYLENGTH
END TYPE
DECLARE FUNCTION Array$ (Dim1, Dim2, Dim3, Value$)
REDIM SHARED Mstuff(1 TO 1000) AS MyStuff
' The Mstuff array will be treated as a
'   3-dimensional array containing mixed data types.

' Create various values...
MyInteger% = 32000
MyLong& = 71234
MySingle! = 123456.123#
MyDouble# = 987656789.9876543#
MyString$ = "EightCHR"

' Set various array elements with the values above.
' The Value$ parameter accepts the values.
zRet = Array$(IntegerArray, 2, 3, STR$(MyInteger%))
zRet = Array$(LongArray, 9, 1, STR$(MyLong&))
zRet = Array$(SingleArray, 2, 7, STR$(MySingle!))
zRet = Array$(DoubleArray, 1, 1, STR$(MyDouble#))
zRet = Array$(StringArray, 1, 1, MyString$)

' Get the values back.
' Array$ returns the values if the Value$ parameter = ""
NewInteger% = VAL(Array$(IntegerArray, 2, 3, ""))
NewLong& = VAL(Array$(LongArray, 9, 1, ""))
NewSingle! = VAL(Array$(SingleArray, 2, 7, ""))
NewDouble# = VAL(Array$(DoubleArray, 1, 1, ""))
NewString$ = Array$(StringArray, 1, 1, "")
CLS

' Print them
PRINT "   Stored:"; MyInteger%; MyLong&; MySingle!; MyDouble#, MyString$
PRINT "Retrieved:"; NewInteger%; NewLong&; NewSingle!; NewDouble#, NewString$

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
        CASE StringArray
          Array$ = LTRIM$(MID$(Mstuff(Dim2).StringArray, Dim3 * 8 - 1, 8))
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
        CASE StringArray
          MID$(Mstuff(Dim2).StringArray, Dim3 * 8 - 1, 8) = Value$
      END SELECT
  END SELECT

END FUNCTION


