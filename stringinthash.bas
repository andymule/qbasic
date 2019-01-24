' $DYNAMIC
''''''''''''''''''' HEADER ''''''''''''
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
REDIM SHARED table(1 TO TABLESIZE) AS StringIntHashTable
'REDIM SHARED table(asd%) AS StringIntHashTable     dynamic? kind of
''''''''''''''''''' END HEADER'''''''''''''''''''''

'''''''''''''' EXAMPLE CODE'''''''''''''
test1% = 33000 ' int overflow
test2% = 32000 'big int

temp% = HashPutInt%("34", test1%)
temp% = HashPutInt%("35", test2%)
temp% = HashPutInt%("36", 123) ' const int
returned1% = HashGetInt%("34")
returned2% = HashGetInt%("35")
returned3% = HashGetInt%("36")
'PRINT "   Stored:"; test1%
PRINT "Int overflow:"; returned1%
PRINT "large int:"; returned2%
PRINT "normal but then...:"; returned3%
returned4% = HashPutInt%("36", 6969) ' overwrite old value
returned4% = HashGetInt%("36")
PRINT "overwritten value:"; returned4%
''''''''''''''''''' END EXAMPLE '''''''''''''''''''''


''''''''''''' SUBS AND FUNCTIONS ''''''''''''''''''''
FUNCTION HashPutInt% (sKey, Value%)
  IF LEN(sKey) > 8 THEN
    PRINT "ERROR! Can't key more than 8 chars"
    RETURN
  END IF
  hash = GetHash(sKey)
  FOR index = 1 TO table(hash).Size
    Value$ = MID$(table(hash).Keys, (index - 1) * StringLength + 1, LEN(sKey)) ' TODO bug on similar strings of diff size, eg. "asd3" and "asd" might return same
    IF MID$(Value$, 1, 8) = MID$(sKey, 1, 8) THEN
      ' TODO think about what this should do
      'PRINT "FOUND DOUBLE VALUE AT:"; sKey; Value% ' found so something?
      'SLEEP 3
      'END
      MID$(table(hash).Values, (index - 1) * 2 + 1, 8) = MKI$(Value%)
      EXIT FUNCTION
    END IF
  NEXT index
  ' not already found, so add to list
  ' TODO dynamic array duh cmon
  IF table(hash).Size = BUCKETSIZE THEN
    PRINT "ERROR! Bucket in hashtable full. Increase BUSKETSIZE or TABLESIZE, or decrease data."
    END
  END IF
  MID$(table(hash).Values, table(hash).Size * 2 + 1, 8) = MKI$(Value%)
  MID$(table(hash).Keys, table(hash).Size * StringLength + 1, StringLength) = MID$(sKey, 1, LEN(sKey))
  table(hash).Size = table(hash).Size + 1
END FUNCTION

FUNCTION HashGetInt% (sKey)
  HashGetInt% = -666
  hash = GetHash(sKey)
  FOR index = 1 TO table(hash).Size
    Value$ = MID$(table(hash).Keys, (index - 1) * StringLength + 1, LEN(sKey))
    IF MID$(Value$, 1, 8) = MID$(sKey, 1, 8) THEN
      HashGetInt% = VAL(STR$(CVI(MID$(table(hash).Values, (index - 1) * IntegerLength + 1, IntegerLength))))
    END IF
  NEXT index
END FUNCTION


FUNCTION GetHash% (key$)
  FOR i = 1 TO LEN(key$)
    hash = hash + ASC(key$, i)
  NEXT i
  FOR i = 1 TO LEN(key$) - 1
    hash = hash + ASC(key$, i) * ASC(key$, i + 1)
  NEXT i
  GetHash% = hash MOD TABLESIZE
END FUNCTION
'''''''''''''''' END SUB AND FUNCTIONS '''''''''''''''
