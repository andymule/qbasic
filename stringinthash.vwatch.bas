'--------------------------------------------------------------------------------
'vWATCH64 initialization code - version 1.105:
'--------------------------------------------------------------------------------
DECLARE LIBRARY
    FUNCTION vwatch64_GETPID& ALIAS getpid ()
END DECLARE

DECLARE LIBRARY "timers"
    SUB VWATCH64_STOPTIMERS ALIAS stop_timers
    SUB VWATCH64_STARTTIMERS ALIAS start_timers
END DECLARE

CONST vwatch64_ID = "vWATCH64"
CONST vwatch64_VERSION = "1.105"
CONST vwatch64_CHECKSUM = "45C9F0C8"
CONST vwatch64_FILENAME = "C:\Users\Arckex\qb64\vwatch64.dat"

'Breakpoint control:
CONST vwatch64_CONTINUE = 1
CONST vwatch64_NEXTSTEP = 2
CONST vwatch64_READY = 3
CONST vwatch64_SETVAR = 4
CONST vwatch64_SKIPSUB = 5
CONST vwatch64_SETNEXT = 7

TYPE vwatch64_HEADERTYPE
    CLIENT_ID AS STRING * 8
    VERSION AS STRING * 5
    CONNECTED AS _BYTE
    RESPONSE AS _BYTE
    PID AS LONG
END TYPE

TYPE vwatch64_CLIENTTYPE
    NAME AS STRING * 256
    CHECKSUM AS STRING * 8
    TOTALSOURCELINES AS LONG
    EXENAME AS STRING * 256
    LINENUMBER AS LONG
    TOTALVARIABLES AS LONG
    PID AS LONG
END TYPE

TYPE vwatch64_BREAKPOINTTYPE
    ACTION AS _BYTE
    LINENUMBER AS LONG
END TYPE


TYPE vwatch64_VARIABLESTYPE
    NAME AS STRING * 256
    SCOPE AS STRING * 50
    UDT AS STRING * 40
    DATATYPE AS STRING * 20
END TYPE

TYPE vwatch64_VARIABLEVALUETYPE
    VALUE AS STRING * 256
END TYPE

DIM SHARED vwatch64_BREAKPOINT AS vwatch64_BREAKPOINTTYPE
DIM SHARED vwatch64_WATCHPOINTCOMMAND AS vwatch64_BREAKPOINTTYPE
DIM SHARED vwatch64_WATCHPOINTCOMMANDBLOCK AS LONG
DIM SHARED vwatch64_BREAKPOINTBLOCK AS LONG
DIM SHARED vwatch64_BREAKPOINTLISTBLOCK AS LONG
DIM SHARED vwatch64_BREAKPOINTLIST AS STRING * 85
DIM SHARED vwatch64_CLIENT AS vwatch64_CLIENTTYPE
DIM SHARED vwatch64_CLIENTBLOCK AS LONG
DIM SHARED vwatch64_CLIENTFILE AS INTEGER
DIM SHARED vwatch64_DATAINFOBLOCK AS LONG
DIM SHARED vwatch64_DATABLOCK AS LONG
DIM SHARED vwatch64_EXCHANGEBLOCK AS LONG
DIM SHARED vwatch64_WATCHPOINTLISTBLOCK AS LONG
DIM SHARED vwatch64_WATCHPOINTEXPBLOCK AS LONG
DIM SHARED vwatch64_HEADER AS vwatch64_HEADERTYPE
DIM SHARED vwatch64_HEADERBLOCK AS LONG
DIM SHARED vwatch64_USERQUIT AS _BYTE
DIM SHARED vwatch64_NEXTLINE AS LONG
DIM SHARED vwatch64_SUBLEVEL AS INTEGER
DIM SHARED vwatch64_TARGETVARINDEX AS LONG
DIM SHARED vwatch64_TIMER AS INTEGER
DIM SHARED vwatch64_EXCHANGEDATASIZE$4
DIM SHARED vwatch64_EXCHANGEDATA AS STRING
DIM SHARED vWATCH64_DUMMY%%

DIM SHARED vwatch64_VARIABLES(1 TO 12) AS vwatch64_VARIABLESTYPE
DIM SHARED vwatch64_VARIABLEDATA(1 TO 12) AS vwatch64_VARIABLEVALUETYPE
DIM SHARED vwatch64_WATCHPOINTLIST AS STRING * 12
DIM SHARED vwatch64_WATCHPOINT(1 TO 12) AS vwatch64_VARIABLEVALUETYPE
vwatch64_VARIABLES(1).NAME = "sKey"
vwatch64_VARIABLES(1).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(1).DATATYPE = "STRING"
vwatch64_VARIABLES(2).NAME = "Value%"
vwatch64_VARIABLES(2).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(2).DATATYPE = "INTEGER"
vwatch64_VARIABLES(3).NAME = "sKey"
vwatch64_VARIABLES(3).SCOPE = "FUNCTION HashGetInt%"
vwatch64_VARIABLES(3).DATATYPE = "STRING"
vwatch64_VARIABLES(4).NAME = "test1%"
vwatch64_VARIABLES(4).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(4).DATATYPE = "INTEGER"
vwatch64_VARIABLES(5).NAME = "test2%"
vwatch64_VARIABLES(5).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(5).DATATYPE = "INTEGER"
vwatch64_VARIABLES(6).NAME = "temp%"
vwatch64_VARIABLES(6).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(6).DATATYPE = "INTEGER"
vwatch64_VARIABLES(7).NAME = "tempkey$"
vwatch64_VARIABLES(7).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(7).DATATYPE = "STRING"
vwatch64_VARIABLES(8).NAME = "i"
vwatch64_VARIABLES(8).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(8).DATATYPE = "SINGLE"
vwatch64_VARIABLES(9).NAME = "hash"
vwatch64_VARIABLES(9).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(9).DATATYPE = "SINGLE"
vwatch64_VARIABLES(10).NAME = "index"
vwatch64_VARIABLES(10).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(10).DATATYPE = "SINGLE"
vwatch64_VARIABLES(11).NAME = "Value$"
vwatch64_VARIABLES(11).SCOPE = "FUNCTION HashPutInt%"
vwatch64_VARIABLES(11).DATATYPE = "STRING"
vwatch64_VARIABLES(12).NAME = "Value$"
vwatch64_VARIABLES(12).SCOPE = "FUNCTION HashGetInt%"
vwatch64_VARIABLES(12).DATATYPE = "STRING"

vwatch64_HEADERBLOCK = 1
vwatch64_CLIENTBLOCK = LEN(vwatch64_HEADER) + 1
vwatch64_BREAKPOINTBLOCK = vwatch64_CLIENTBLOCK + LEN(vwatch64_CLIENT) + 1
vwatch64_BREAKPOINTLISTBLOCK = vwatch64_BREAKPOINTBLOCK + LEN(vwatch64_BREAKPOINT) + 1
vwatch64_DATAINFOBLOCK = vwatch64_BREAKPOINTLISTBLOCK + LEN(vwatch64_BREAKPOINTLIST) + 1
vwatch64_DATABLOCK = vwatch64_DATAINFOBLOCK + LEN(vwatch64_VARIABLES()) + 1
vwatch64_WATCHPOINTLISTBLOCK = vwatch64_DATABLOCK + LEN(vwatch64_VARIABLEDATA()) + 1
vwatch64_WATCHPOINTEXPBLOCK = vwatch64_WATCHPOINTLISTBLOCK + LEN(vwatch64_WATCHPOINTLIST) + 1
vwatch64_WATCHPOINTCOMMANDBLOCK = vwatch64_WATCHPOINTEXPBLOCK + LEN(vwatch64_WATCHPOINT()) + 1
vwatch64_EXCHANGEBLOCK = vwatch64_WATCHPOINTCOMMANDBLOCK + LEN(vwatch64_WATCHPOINTCOMMAND) + 1

vwatch64_CONNECTTOHOST

'Initialize the data export timer:
vwatch64_TIMER = _FREETIMER
ON TIMER(vwatch64_TIMER, .1) vwatch64_VARIABLEWATCH
TIMER(vwatch64_TIMER) ON

'--------------------------------------------------------------------------------
'End of vWATCH64 initialization code.
'--------------------------------------------------------------------------------

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

vwatch64_LABEL_28:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(28): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_28 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_28
test1% = 88
vwatch64_SKIP_28:::: 
vwatch64_LABEL_29:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(29): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_29 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_29
test2% = 89
vwatch64_SKIP_29:::: 

vwatch64_LABEL_31:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(31): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_31 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_31
temp% = HashPutInt%("34", test1%)
vwatch64_SKIP_31:::: 
vwatch64_LABEL_32:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(32): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_32 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_32
temp% = HashPutInt%("34", test2%)
vwatch64_SKIP_32:::: 
'NewInteger% = HashGetInt%("test1")
'PRINT "   Stored:"; test1%
'PRINT "Retrieved:"; NewInteger%

IF vwatch64_HEADER.CONNECTED THEN
    vwatch64_HEADER.CONNECTED = 0
    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER
END IF
CLOSE #vwatch64_CLIENTFILE
ON ERROR GOTO vwatch64_FILEERROR
KILL vwatch64_FILENAME

END
vwatch64_FILEERROR:
RESUME NEXT

vwatch64_CLIENTFILEERROR:
IF vwatch64_HEADER.CONNECTED THEN OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE
RESUME

vwatch64_SETNEXTLINE:
SELECT CASE vwatch64_NEXTLINE
    CASE 28: GOTO vwatch64_LABEL_28
    CASE 29: GOTO vwatch64_LABEL_29
    CASE 31: GOTO vwatch64_LABEL_31
    CASE 32: GOTO vwatch64_LABEL_32
END SELECT

vwatch64_SETVARIABLE:
ON ERROR GOTO vwatch64_CLIENTFILEERROR
GET #vwatch64_CLIENTFILE, vwatch64_EXCHANGEBLOCK, vwatch64_EXCHANGEDATASIZE$4
vwatch64_TARGETVARINDEX = CVL(vwatch64_EXCHANGEDATASIZE$4)
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATASIZE$4
vwatch64_EXCHANGEDATA = SPACE$(CVL(vwatch64_EXCHANGEDATASIZE$4))
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATA
vwatch64_BREAKPOINT.ACTION = vwatch64_READY
PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
ON ERROR GOTO vwatch64_FILEERROR

SELECT CASE vwatch64_TARGETVARINDEX
    CASE 4: test1% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 5: test2% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 6: temp% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
END SELECT
ON ERROR GOTO 0
RETURN

FUNCTION HashPutInt% (sKey, Value%)
    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1
vwatch64_LABEL_38:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(38): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_38 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_38
  tempkey$ = "        "
vwatch64_SKIP_38:::: 
vwatch64_LABEL_39:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(39): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_39 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_39
  IF LEN(sKey) < 8 THEN
vwatch64_SKIP_39:::: 
vwatch64_LABEL_40:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(40): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_40 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_40
    RSET tempkey$ = sKey
vwatch64_SKIP_40:::: 
vwatch64_LABEL_41:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(41): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_41 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_41
  ELSE
vwatch64_SKIP_41:::: 
vwatch64_LABEL_42:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(42): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_42 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_42
    FOR i = 1 TO 8
vwatch64_SKIP_42:::: 
vwatch64_LABEL_43:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(43): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_43 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_43
      MID$(tempkey$, i, 1) = MID$(sKey, i, 1) 'SPACE$
vwatch64_SKIP_43:::: 
vwatch64_LABEL_44:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(44): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_44 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_44
    NEXT i
vwatch64_SKIP_44:::: 
vwatch64_LABEL_45:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(45): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_45 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_45
  END IF
vwatch64_SKIP_45:::: 

  'IF LEN(sKey) < 8 THEN
  '  FOR i = LEN(sKey) + 1 TO 8
  '    MID$(sKey, i, 1) = " "
  '  NEXT i
  'END IF
vwatch64_LABEL_52:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(52): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_52 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_52
  FOR i = 1 TO LEN(tempkey$)
vwatch64_SKIP_52:::: 
vwatch64_LABEL_53:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(53): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_53 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_53
    hash = hash + ASC(tempkey$, i)
vwatch64_SKIP_53:::: 
    'PRINT hash
vwatch64_LABEL_55:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(55): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_55 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_55
  NEXT i
vwatch64_SKIP_55:::: 
vwatch64_LABEL_56:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(56): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_56 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_56
  FOR i = 1 TO LEN(tempkey$) - 1
vwatch64_SKIP_56:::: 
vwatch64_LABEL_57:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(57): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_57 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_57
    hash = hash + ASC(tempkey$, i) * ASC(tempkey$, i + 1)
vwatch64_SKIP_57:::: 
    'PRINT hash
vwatch64_LABEL_59:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(59): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_59 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_59
  NEXT i
vwatch64_SKIP_59:::: 
vwatch64_LABEL_60:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(60): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_60 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_60
  hash = hash MOD TABLESIZE
vwatch64_SKIP_60:::: 
  'PRINT "HASH", hash
vwatch64_LABEL_62:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(62): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_62 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_62
  FOR index = 1 TO table(hash).Size
vwatch64_SKIP_62:::: 
vwatch64_LABEL_63:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(63): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_63 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_63
    Value$ = MID$(table(hash).Keys, table(hash).Size * StringLength + 1, StringLength)
vwatch64_SKIP_63:::: 
vwatch64_LABEL_64:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(64): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_64 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_64
    PRINT tempkey$, LEN(tempkey$)
vwatch64_SKIP_64:::: 
vwatch64_LABEL_65:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(65): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_65 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_65
    PRINT Value$, LEN(Value$)
vwatch64_SKIP_65:::: 
vwatch64_LABEL_66:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(66): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_66 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_66
    IF MID$(Value$, 1, 8) = MID$(tempkey$, 1, 8) THEN
vwatch64_SKIP_66:::: 
vwatch64_LABEL_67:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(67): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_67 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_67
      PRINT "FOUND DOUBLE VALUE AT:"; tempkey$; Value%
vwatch64_SKIP_67:::: 
vwatch64_LABEL_68:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(68): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_68 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_68
      SLEEP 3
vwatch64_SKIP_68:::: 
vwatch64_LABEL_69:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(69): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_69 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_69
ON ERROR GOTO vwatch64_FILEERROR
IF vwatch64_HEADER.CONNECTED THEN
    vwatch64_HEADER.CONNECTED = 0
    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER
END IF
CLOSE #vwatch64_CLIENTFILE
KILL "C:\Users\Arckex\qb64\vwatch64.dat"
ON ERROR GOTO 0
      END
vwatch64_SKIP_69:::: 
vwatch64_LABEL_70:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(70): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_70 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_70
    END IF
vwatch64_SKIP_70:::: 
    'if table(hash).
vwatch64_LABEL_72:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(72): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_72 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_72
  NEXT index
vwatch64_SKIP_72:::: 
  ' not already found, so add to list
vwatch64_LABEL_74:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(74): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_74 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_74
  table(hash).Size = table(hash).Size + 1
vwatch64_SKIP_74:::: 
vwatch64_LABEL_75:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(75): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_75 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_75
  MID$(table(hash).Values, table(hash).Size * 2 + 1, 2) = MKI$(Value%)
vwatch64_SKIP_75:::: 
vwatch64_LABEL_76:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(76): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_76 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_76
  MID$(table(hash).Keys, table(hash).Size * StringLength + 1, StringLength) = MID$(sKey, 1, StringLength) 'trim to fit and save
vwatch64_SKIP_76:::: 
vwatch64_LABEL_77:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(77): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_77 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_77
  PRINT MID$(sKey, 1, 8)
vwatch64_SKIP_77:::: 
vwatch64_LABEL_78:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(78): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN vWATCH64_DUMMY%% = 0 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_78
vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1
EXIT FUNCTION
vwatch64_SETNEXTLINE:
SELECT CASE vwatch64_NEXTLINE
    CASE 38: GOTO vwatch64_LABEL_38
    CASE 39: GOTO vwatch64_LABEL_39
    CASE 40: GOTO vwatch64_LABEL_40
    CASE 41: GOTO vwatch64_LABEL_41
    CASE 42: GOTO vwatch64_LABEL_42
    CASE 43: GOTO vwatch64_LABEL_43
    CASE 44: GOTO vwatch64_LABEL_44
    CASE 45: GOTO vwatch64_LABEL_45
    CASE 52: GOTO vwatch64_LABEL_52
    CASE 53: GOTO vwatch64_LABEL_53
    CASE 55: GOTO vwatch64_LABEL_55
    CASE 56: GOTO vwatch64_LABEL_56
    CASE 57: GOTO vwatch64_LABEL_57
    CASE 59: GOTO vwatch64_LABEL_59
    CASE 60: GOTO vwatch64_LABEL_60
    CASE 62: GOTO vwatch64_LABEL_62
    CASE 63: GOTO vwatch64_LABEL_63
    CASE 64: GOTO vwatch64_LABEL_64
    CASE 65: GOTO vwatch64_LABEL_65
    CASE 66: GOTO vwatch64_LABEL_66
    CASE 67: GOTO vwatch64_LABEL_67
    CASE 68: GOTO vwatch64_LABEL_68
    CASE 69: GOTO vwatch64_LABEL_69
    CASE 70: GOTO vwatch64_LABEL_70
    CASE 72: GOTO vwatch64_LABEL_72
    CASE 74: GOTO vwatch64_LABEL_74
    CASE 75: GOTO vwatch64_LABEL_75
    CASE 76: GOTO vwatch64_LABEL_76
    CASE 77: GOTO vwatch64_LABEL_77
    CASE 78: GOTO vwatch64_LABEL_78
END SELECT

vwatch64_VARIABLEWATCH:
IF vwatch64_HEADER.CONNECTED = 0 THEN RETURN
ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(1).VALUE = sKey
    vwatch64_VARIABLEDATA(2).VALUE = STR$(Value%)
    vwatch64_VARIABLEDATA(7).VALUE = tempkey$
    vwatch64_VARIABLEDATA(8).VALUE = STR$(i)
    vwatch64_VARIABLEDATA(9).VALUE = STR$(hash)
    vwatch64_VARIABLEDATA(10).VALUE = STR$(index)
    vwatch64_VARIABLEDATA(11).VALUE = Value$
ON ERROR GOTO 0
RETURN


vwatch64_SETVARIABLE:
ON ERROR GOTO vwatch64_CLIENTFILEERROR
GET #vwatch64_CLIENTFILE, vwatch64_EXCHANGEBLOCK, vwatch64_EXCHANGEDATASIZE$4
vwatch64_TARGETVARINDEX = CVL(vwatch64_EXCHANGEDATASIZE$4)
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATASIZE$4
vwatch64_EXCHANGEDATA = SPACE$(CVL(vwatch64_EXCHANGEDATASIZE$4))
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATA
vwatch64_BREAKPOINT.ACTION = vwatch64_READY
PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
ON ERROR GOTO vwatch64_FILEERROR

SELECT CASE vwatch64_TARGETVARINDEX
    CASE 1: sKey = vwatch64_EXCHANGEDATA
    CASE 2: Value% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 7: tempkey$ = vwatch64_EXCHANGEDATA
    CASE 8: i = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 9: hash = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 10: index = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 11: Value$ = vwatch64_EXCHANGEDATA
END SELECT
GOSUB vwatch64_VARIABLEWATCH
ON ERROR GOTO 0
RETURN
END FUNCTION

FUNCTION HashGetInt% (sKey)
    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1
  'CASE IntegerArray
vwatch64_LABEL_82:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(82): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_82 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_82
  HashGetInt% = VAL(LTRIM$(STR$(CVI(MID$(table(1).Values, table(1).Size * 2 + 1, 2)))))
vwatch64_SKIP_82:::: 
  'CASE StringArray
vwatch64_LABEL_84:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(84): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_84 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_84
  Value$ = LTRIM$(MID$(table(1).Keys, table(1).Size * StringLength + 1, StringLength))
vwatch64_SKIP_84:::: 
vwatch64_LABEL_85:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(85): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN vWATCH64_DUMMY%% = 0 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_85
vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1
EXIT FUNCTION
vwatch64_SETNEXTLINE:
SELECT CASE vwatch64_NEXTLINE
    CASE 82: GOTO vwatch64_LABEL_82
    CASE 84: GOTO vwatch64_LABEL_84
    CASE 85: GOTO vwatch64_LABEL_85
END SELECT

vwatch64_VARIABLEWATCH:
IF vwatch64_HEADER.CONNECTED = 0 THEN RETURN
ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(3).VALUE = sKey
    vwatch64_VARIABLEDATA(12).VALUE = Value$
ON ERROR GOTO 0
RETURN


vwatch64_SETVARIABLE:
ON ERROR GOTO vwatch64_CLIENTFILEERROR
GET #vwatch64_CLIENTFILE, vwatch64_EXCHANGEBLOCK, vwatch64_EXCHANGEDATASIZE$4
vwatch64_TARGETVARINDEX = CVL(vwatch64_EXCHANGEDATASIZE$4)
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATASIZE$4
vwatch64_EXCHANGEDATA = SPACE$(CVL(vwatch64_EXCHANGEDATASIZE$4))
GET #vwatch64_CLIENTFILE, , vwatch64_EXCHANGEDATA
vwatch64_BREAKPOINT.ACTION = vwatch64_READY
PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
ON ERROR GOTO vwatch64_FILEERROR

SELECT CASE vwatch64_TARGETVARINDEX
    CASE 3: sKey = vwatch64_EXCHANGEDATA
    CASE 12: Value$ = vwatch64_EXCHANGEDATA
END SELECT
GOSUB vwatch64_VARIABLEWATCH
ON ERROR GOTO 0
RETURN
END FUNCTION


'--------------------------------------------------------------------------------
'vWATCH64 procedures:
'--------------------------------------------------------------------------------
SUB vwatch64_CONNECTTOHOST
    DIM k AS LONG
    DIM Message1$, Message2$, NoGo%
    DIM FileIsOpen%, FileExists%

    vwatch64_CHECKFILE:
    IF _FILEEXISTS(vwatch64_FILENAME) = 0 THEN
        Message1$ = "vWATCH64 doesn't seem to be running."
        Message2$ = "(Checking for 'vwatch64.dat'; ESC to cancel...)"
        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
            _TITLE "Connecting to vWATCH64..."
            _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$
            _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$
        ELSE
            _CONSOLETITLE "Connecting to vWATCH64..."
            PRINT Message1$: PRINT Message1$
        END IF
        DO: _LIMIT 30
            k = _KEYHIT
            IF k = -27 THEN SYSTEM
            IF _FILEEXISTS(vwatch64_FILENAME) THEN _KEYCLEAR: EXIT DO
        LOOP
    END IF

    vwatch64_CLIENTFILE = 25522
    OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE

    'Check if a connection is already active
    IF LOF(vwatch64_CLIENTFILE) > 0 THEN
        'Check if the file can be deleted; if so, vWATCH64 is not running.
        CLOSE #vwatch64_CLIENTFILE
        NoGo% = 0
        ON ERROR GOTO vwatch64_FILEERROR
        KILL vwatch64_FILENAME
        ON ERROR GOTO 0
        NoGo% = _FILEEXISTS(vwatch64_FILENAME)

        IF NoGo% THEN
            CLS
            Message1$ = "ERROR: vWATCH64 is already connected to another"
            Message2$ = "client/debuggee."
            IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
                _TITLE "FAILED!"
                _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$
                _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$
            ELSE
                _CONSOLETITLE "FAILED!"
                PRINT Message1$: PRINT Message1$
            END IF
            END
        END IF
        GOTO vwatch64_CHECKFILE
    ELSEIF LOF(vwatch64_CLIENTFILE) = 0 THEN
        'Check if the file can be deleted; if so, vWATCH64 is not running.
        CLOSE #vwatch64_CLIENTFILE
        ON ERROR GOTO vwatch64_FILEERROR
        KILL vwatch64_FILENAME
        ON ERROR GOTO 0
        IF _FILEEXISTS(vwatch64_FILENAME) = 0 THEN GOTO vwatch64_CHECKFILE
    END IF

    OPEN vwatch64_FILENAME FOR BINARY AS vwatch64_CLIENTFILE
    vwatch64_CLIENT.NAME = "C:\Users\Arckex\qb64\programs\mine\stringinthash.bas"
    vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM
    vwatch64_CLIENT.TOTALSOURCELINES = 85
    vwatch64_CLIENT.TOTALVARIABLES = 12
    vwatch64_CLIENT.PID = vwatch64_GETPID&
    vwatch64_CLIENT.EXENAME = COMMAND$(0)

    'Send this client's version and connection request
    vwatch64_HEADER.CLIENT_ID = vwatch64_ID
    vwatch64_HEADER.VERSION = vwatch64_VERSION
    vwatch64_HEADER.CONNECTED = -1
    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER
    PUT #vwatch64_CLIENTFILE, vwatch64_DATAINFOBLOCK, vwatch64_VARIABLES()

    'Wait for authorization:
    CLS
    Message1$ = "Waiting for authorization; ESC to cancel..."
    IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
        _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$
    ELSE
        PRINT Message1$
    END IF
    DO: _LIMIT 30
        GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER
        k = _KEYHIT
        IF k = -27 THEN SYSTEM
     LOOP UNTIL vwatch64_HEADER.RESPONSE = -1 OR vwatch64_HEADER.CONNECTED = 0

    IF vwatch64_HEADER.CONNECTED = 0 THEN
        SYSTEM
    END IF

    CLS
    IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
        _TITLE "Untitled"
    ELSE
        _CONSOLETITLE "Untitled"
    END IF
    PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT
END SUB

SUB vwatch64_VARIABLEWATCH
    SHARED test1%
    SHARED test2%
    SHARED temp%

    IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT SUB
    ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(4).VALUE = STR$(test1%)
    vwatch64_VARIABLEDATA(5).VALUE = STR$(test2%)
    vwatch64_VARIABLEDATA(6).VALUE = STR$(temp%)
    ON ERROR GOTO vwatch64_CLIENTFILEERROR
    PUT #vwatch64_CLIENTFILE, vwatch64_DATABLOCK, vwatch64_VARIABLEDATA().VALUE
    ON ERROR GOTO 0
END SUB

FUNCTION vwatch64_CHECKBREAKPOINT&(LineNumber AS LONG)
    STATIC FirstRunDone AS _BYTE
    STATIC StepMode AS _BYTE
    STATIC StepAround AS _BYTE
    STATIC StartLevel AS INTEGER
    DIM k AS LONG
    DIM Message1$, Message2$

    IF FirstRunDone = 0 THEN
        IF vwatch64_HEADER.CONNECTED = 0 THEN
            _DELAY .5
            IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
                _TITLE "Untitled"
            ELSE
                _CONSOLETITLE "Untitled"
            END IF
            FirstRunDone = -1
            EXIT FUNCTION
        END IF
    ELSE
        IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT FUNCTION
    END IF

    vwatch64_CLIENT.LINENUMBER = LineNumber
    ON ERROR GOTO vwatch64_CLIENTFILEERROR
    PUT #vwatch64_CLIENTFILE, vwatch64_CLIENTBLOCK, vwatch64_CLIENT

    'Check if step mode was initiated by the host:
    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
    IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1
    IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1

    GOSUB vwatch64_PING

    'Get the breakpoint list:
    vwatch64_BREAKPOINT.ACTION = vwatch64_READY
    PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
    GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTLISTBLOCK, vwatch64_BREAKPOINTLIST

    IF StepAround = -1 AND vwatch64_SUBLEVEL > StartLevel AND (ASC(vwatch64_BREAKPOINTLIST, LineNumber) <> 1) THEN EXIT FUNCTION
    IF StepAround = -1 AND vwatch64_SUBLEVEL = StartLevel THEN StepAround = 0

    vwatch64_VARIABLEWATCH
    IF vwatch64_CHECKWATCHPOINT = -1 THEN StepMode = -1

    'On the first time this procedure is called, execution is halted,
    'until the user presses F5 or F8 in vWATCH64
    IF FirstRunDone = 0 THEN
        Message1$ = "Hit F8 to run line by line or switch to vWATCH64 and hit F5 to run;"
        Message2$ = "(ESC to quit)"
        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
            _TITLE Message1$
            _PRINTSTRING(_WIDTH \ 2 - LEN(Message1$) \ 2, _HEIGHT \ 2), Message1$
            _PRINTSTRING(_WIDTH \ 2 - LEN(Message2$) \ 2, _HEIGHT \ 2 + 1), Message2$
        ELSE
            _CONSOLETITLE "Switch to vWATCH64 and hit F5 to run or F8 to run line by line;"
        END IF
        VWATCH64_STOPTIMERS
        DO: _LIMIT 500
            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
            IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETNEXT THEN
                vwatch64_CHECKBREAKPOINT& = vwatch64_BREAKPOINT.LINENUMBER
                vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP
                vwatch64_BREAKPOINT.LINENUMBER = 0
                PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
                IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
                    _TITLE "Untitled": CLS
                ELSE
                    _CONSOLETITLE "Untitled": CLS
                END IF
                FirstRunDone = -1
                ON ERROR GOTO 0
                EXIT FUNCTION
            END IF
            k = _KEYHIT
            IF k = 16896 THEN vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP 'F8
            IF k = -27 THEN 'ESC
                CLOSE #vwatch64_CLIENTFILE
                SYSTEM
            END IF
            _KEYCLEAR
            GOSUB vwatch64_PING
        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP OR vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR OR vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepMode = -1: StepAround = 0
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1: StepMode = -1
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR THEN
            vwatch64_CHECKBREAKPOINT& = -1
            StepMode = -1
        END IF
        IF NOT _SCREENHIDE AND _DEST <> _CONSOLE THEN
            _TITLE "Untitled": CLS
        ELSE
            _CONSOLETITLE "Untitled": CLS
        END IF
        FirstRunDone = -1
        ON ERROR GOTO 0
        VWATCH64_STARTTIMERS
        EXIT FUNCTION
    END IF

    IF (ASC(vwatch64_BREAKPOINTLIST, LineNumber) = 2) THEN
            vwatch64_CHECKBREAKPOINT& = -2
            EXIT FUNCTION
    END IF

    IF (ASC(vwatch64_BREAKPOINTLIST, LineNumber) = 1) OR (StepMode = -1) THEN
        VWATCH64_STOPTIMERS
        StepMode = -1
        DO: _LIMIT 500
            GET #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
            IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETNEXT THEN
                vwatch64_CHECKBREAKPOINT& = vwatch64_BREAKPOINT.LINENUMBER
                vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP
                vwatch64_BREAKPOINT.LINENUMBER = 0
                StepMode = -1
                PUT #vwatch64_CLIENTFILE, vwatch64_BREAKPOINTBLOCK, vwatch64_BREAKPOINT
                ON ERROR GOTO 0
                EXIT FUNCTION
            END IF
            k = _KEYHIT
            IF k = 16896 THEN vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP 'F8
            _KEYCLEAR
            GOSUB vwatch64_PING
        LOOP UNTIL vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE OR vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP OR vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR OR vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_CONTINUE THEN StepMode = 0: StepAround = 0
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_NEXTSTEP THEN StepAround = 0: StepMode = -1
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SKIPSUB THEN StartLevel = vwatch64_SUBLEVEL - 1: StepAround = -1: StepMode = -1
        IF vwatch64_BREAKPOINT.ACTION = vwatch64_SETVAR THEN
            vwatch64_CHECKBREAKPOINT& = -1
            StepMode = -1
        END IF
        VWATCH64_STARTTIMERS
    END IF

    ON ERROR GOTO 0
    EXIT FUNCTION
    vwatch64_PING:
    'Check if connection is still alive on host's end
    GET #vwatch64_CLIENTFILE, vwatch64_HEADERBLOCK, vwatch64_HEADER
    IF vwatch64_HEADER.CONNECTED = 0 THEN
        CLOSE vwatch64_CLIENTFILE
        IF FirstRunDone = 0 THEN FirstRunDone = -1: CLS: _TITLE "Untitled"
        VWATCH64_STARTTIMERS
        EXIT FUNCTION
    END IF
    RETURN
END SUB


FUNCTION vwatch64_CHECKWATCHPOINT
    DIM i AS LONG, DataType$
    GET #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTLISTBLOCK, vwatch64_WATCHPOINTLIST
    FOR i = 1 TO 12
        IF ASC(vwatch64_WATCHPOINTLIST, i) = 1 THEN
            GET #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTEXPBLOCK, vwatch64_WATCHPOINT()
            DataType$ = UCASE$(RTRIM$(vwatch64_VARIABLES(i).DATATYPE))
            IF INSTR(DataType$, "STRING") THEN DataType$ = "STRING"
            IF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = "=" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                       IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) = RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                       IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) = VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN
                           GOTO WatchpointStop
                        END IF
                END SELECT
            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = "<=" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) <= RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) <= VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN
                            GOTO WatchpointStop
                        END IF
                END SELECT
            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = ">=" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) >= RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) >= VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN
                            GOTO WatchpointStop
                        END IF
                END SELECT
            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 2) = "<>" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) <> RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) <> VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 3))) THEN
                            GOTO WatchpointStop
                        END IF
                END SELECT
            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = "<" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) < RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) < VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN
                            GOTO WatchpointStop
                        END IF
                END SELECT
            ELSEIF LEFT$(vwatch64_WATCHPOINT(i).VALUE, 1) = ">" THEN
                SELECT CASE DataType$
                    CASE "STRING"
                        IF RTRIM$(vwatch64_VARIABLEDATA(i).VALUE) > RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2)) THEN
                            GOTO WatchpointStop
                        END IF
                    CASE ELSE
                        IF VAL(RTRIM$(vwatch64_VARIABLEDATA(i).VALUE)) > VAL(RTRIM$(MID$(vwatch64_WATCHPOINT(i).VALUE, 2))) THEN
                            GOTO WatchpointStop
                        END IF
                END SELECT
            END IF
        END IF
    NEXT i

    EXIT FUNCTION

   WatchpointStop:
   vwatch64_WATCHPOINTCOMMAND.ACTION = vwatch64_NEXTSTEP
   vwatch64_WATCHPOINTCOMMAND.LINENUMBER = i
   PUT #vwatch64_CLIENTFILE, vwatch64_WATCHPOINTCOMMANDBLOCK, vwatch64_WATCHPOINTCOMMAND
   vwatch64_CHECKWATCHPOINT = -1
END FUNCTION
'--------------------------------------------------------------------------------
'End of vWATCH64 procedures.
'--------------------------------------------------------------------------------
