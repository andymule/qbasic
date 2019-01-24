' $DYNAMIC
' how many of each type will we store
CONST BUCKETSIZE = 127 'how many items per bucket , prime number
CONST TABLESIZE = 1031 'how many buckets in the total hash list
'total hashable size would be BUCKETSIZE * TABLESIZE

CONST IntegerLength = 2
CONST StringLength = 8

' determine memory needed by multiplying BUCKETSIZE by datatypesize
CONST INTEGERARRAYLENGTH = IntegerLength * BUCKETSIZE
CONST STRINGARRAYLENGTH = StringLength * BUCKETSIZE

DEFSTR S 'variables starting with z are strings
TYPE StringIntHashTable
  'dim for BUCKETSIZE amount of integers
  Values AS STRING * INTEGERARRAYLENGTH
  'string as key, max 8 char (bytes) in key
  Keys AS STRING * STRINGARRAYLENGTH
  'how many items in this dict
  Size AS INTEGER
END TYPE
'DECLARE FUNCTION Array$ (Dim2, Dim3, Value$)
REDIM SHARED table(1 TO TABLESIZE) AS StringIntHashTable
'asd% = 123
'REDIM SHARED table(asd%) AS StringIntHashTable     dynamic? kind of

test1% = 88
test2% = 89

temp% = HashPutInt%("34", test1%)
temp% = HashPutInt%("34", test2%)
'NewInteger% = HashGetInt%("test1")
'PRINT "   Stored:"; test1%
'PRINT "Retrieved:"; NewInteger%

FUNCTION HashPutInt% (sKey, Value%)
  tempkey$ = "        "
  FOR i = 1 TO 8
    MID$(tempkey$, i, 1) = MID$(sKey, i, 1) 'SPACE$ 'rset
  NEXT i
  FOR i = 1 TO LEN(tempkey$)
    hash = hash + ASC(tempkey$, i)
  NEXT i
  FOR i = 1 TO LEN(tempkey$) - 1
    hash = hash + ASC(tempkey$, i) * ASC(tempkey$, i + 1)
  NEXT i
  hash = hash MOD TABLESIZE
  FOR index = 1 TO table(hash).Size
    Value$ = MID$(table(hash).Keys, table(hash).Size * StringLength + 1, StringLength)
    PRINT tempkey$, LEN(tempkey$)
    PRINT Value$, LEN(Value$)
    IF MID$(Value$, 1, 8) = MID$(tempkey$, 1, 8) THEN
      PRINT "FOUND DOUBLE VALUE AT:"; tempkey$; Value%
      SLEEP 3
      END
    END IF
    'if table(hash).
  NEXT index
  ' not already found, so add to list
  table(hash).Size = table(hash).Size + 1
  MID$(table(hash).Values, table(hash).Size * 2 + 1, 2) = MKI$(Value%)
  MID$(table(hash).Keys, table(hash).Size * StringLength + 1, StringLength) = MID$(tempkey$, 1, StringLength) 'trim to fit and save
  PRINT MID$(tempkey$, 1, 8)
END FUNCTION

FUNCTION HashGetInt% (sKey)
  'CASE IntegerArray
  HashGetInt% = VAL(LTRIM$(STR$(CVI(MID$(table(1).Values, table(1).Size * 2 + 1, 2)))))
  'CASE StringArray
  Value$ = LTRIM$(MID$(table(1).Keys, table(1).Size * StringLength + 1, StringLength))
END FUNCTION
