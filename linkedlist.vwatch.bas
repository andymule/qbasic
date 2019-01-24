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
CONST vwatch64_CHECKSUM = "B65F0D6E"
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
DIM SHARED vwatch64_BREAKPOINTLIST AS STRING * 103
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

DIM SHARED vwatch64_VARIABLES(1 TO 15) AS vwatch64_VARIABLESTYPE
DIM SHARED vwatch64_VARIABLEDATA(1 TO 15) AS vwatch64_VARIABLEVALUETYPE
DIM SHARED vwatch64_WATCHPOINTLIST AS STRING * 15
DIM SHARED vwatch64_WATCHPOINT(1 TO 15) AS vwatch64_VARIABLEVALUETYPE
vwatch64_VARIABLES(1).NAME = "Dim1"
vwatch64_VARIABLES(1).SCOPE = "FUNCTION Array$"
vwatch64_VARIABLES(1).DATATYPE = "SINGLE"
vwatch64_VARIABLES(2).NAME = "Dim2"
vwatch64_VARIABLES(2).SCOPE = "FUNCTION Array$"
vwatch64_VARIABLES(2).DATATYPE = "SINGLE"
vwatch64_VARIABLES(3).NAME = "Dim3"
vwatch64_VARIABLES(3).SCOPE = "FUNCTION Array$"
vwatch64_VARIABLES(3).DATATYPE = "SINGLE"
vwatch64_VARIABLES(4).NAME = "Value$"
vwatch64_VARIABLES(4).SCOPE = "FUNCTION Array$"
vwatch64_VARIABLES(4).DATATYPE = "STRING"
vwatch64_VARIABLES(5).NAME = "MyInteger%"
vwatch64_VARIABLES(5).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(5).DATATYPE = "INTEGER"
vwatch64_VARIABLES(6).NAME = "MyLong&"
vwatch64_VARIABLES(6).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(6).DATATYPE = "LONG"
vwatch64_VARIABLES(7).NAME = "MySingle!"
vwatch64_VARIABLES(7).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(7).DATATYPE = "SINGLE"
vwatch64_VARIABLES(8).NAME = "MyDouble#"
vwatch64_VARIABLES(8).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(8).DATATYPE = "DOUBLE"
vwatch64_VARIABLES(9).NAME = "MyString$"
vwatch64_VARIABLES(9).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(9).DATATYPE = "STRING"
vwatch64_VARIABLES(10).NAME = "zRet"
vwatch64_VARIABLES(10).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(10).DATATYPE = "STRING"
vwatch64_VARIABLES(11).NAME = "NewInteger%"
vwatch64_VARIABLES(11).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(11).DATATYPE = "INTEGER"
vwatch64_VARIABLES(12).NAME = "NewLong&"
vwatch64_VARIABLES(12).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(12).DATATYPE = "LONG"
vwatch64_VARIABLES(13).NAME = "NewSingle!"
vwatch64_VARIABLES(13).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(13).DATATYPE = "SINGLE"
vwatch64_VARIABLES(14).NAME = "NewDouble#"
vwatch64_VARIABLES(14).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(14).DATATYPE = "DOUBLE"
vwatch64_VARIABLES(15).NAME = "NewString$"
vwatch64_VARIABLES(15).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(15).DATATYPE = "STRING"

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
vwatch64_LABEL_45:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(45): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_45 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_45
MyInteger% = 32000
vwatch64_SKIP_45:::: 
vwatch64_LABEL_46:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(46): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_46 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_46
MyLong& = 71234
vwatch64_SKIP_46:::: 
vwatch64_LABEL_47:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(47): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_47 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_47
MySingle! = 123456.123#
vwatch64_SKIP_47:::: 
vwatch64_LABEL_48:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(48): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_48 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_48
MyDouble# = 987656789.9876543#
vwatch64_SKIP_48:::: 
vwatch64_LABEL_49:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(49): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_49 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_49
MyString$ = "EightCHR"
vwatch64_SKIP_49:::: 

' Set various array elements with the values above.
' The Value$ parameter accepts the values.
vwatch64_LABEL_53:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(53): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_53 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_53
zRet = Array$(IntegerArray, 2, 3, STR$(MyInteger%))
vwatch64_SKIP_53:::: 
vwatch64_LABEL_54:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(54): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_54 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_54
zRet = Array$(LongArray, 9, 1, STR$(MyLong&))
vwatch64_SKIP_54:::: 
vwatch64_LABEL_55:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(55): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_55 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_55
zRet = Array$(SingleArray, 2, 7, STR$(MySingle!))
vwatch64_SKIP_55:::: 
vwatch64_LABEL_56:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(56): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_56 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_56
zRet = Array$(DoubleArray, 1, 1, STR$(MyDouble#))
vwatch64_SKIP_56:::: 
vwatch64_LABEL_57:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(57): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_57 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_57
zRet = Array$(StringArray, 1, 1, MyString$)
vwatch64_SKIP_57:::: 

' Get the values back.
' Array$ returns the values if the Value$ parameter = ""
vwatch64_LABEL_61:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(61): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_61 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_61
NewInteger% = VAL(Array$(IntegerArray, 2, 3, ""))
vwatch64_SKIP_61:::: 
vwatch64_LABEL_62:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(62): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_62 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_62
NewLong& = VAL(Array$(LongArray, 9, 1, ""))
vwatch64_SKIP_62:::: 
vwatch64_LABEL_63:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(63): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_63 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_63
NewSingle! = VAL(Array$(SingleArray, 2, 7, ""))
vwatch64_SKIP_63:::: 
vwatch64_LABEL_64:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(64): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_64 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_64
NewDouble# = VAL(Array$(DoubleArray, 1, 1, ""))
vwatch64_SKIP_64:::: 
vwatch64_LABEL_65:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(65): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_65 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_65
NewString$ = Array$(StringArray, 1, 1, "")
vwatch64_SKIP_65:::: 
vwatch64_LABEL_66:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(66): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_66 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_66
CLS
vwatch64_SKIP_66:::: 

' Print them
vwatch64_LABEL_69:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(69): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_69 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_69
PRINT "   Stored:"; MyInteger%; MyLong&; MySingle!; MyDouble#, MyString$
vwatch64_SKIP_69:::: 
vwatch64_LABEL_70:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(70): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_70 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_70
PRINT "Retrieved:"; NewInteger%; NewLong&; NewSingle!; NewDouble#, NewString$
vwatch64_SKIP_70:::: 

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
    CASE 45: GOTO vwatch64_LABEL_45
    CASE 46: GOTO vwatch64_LABEL_46
    CASE 47: GOTO vwatch64_LABEL_47
    CASE 48: GOTO vwatch64_LABEL_48
    CASE 49: GOTO vwatch64_LABEL_49
    CASE 53: GOTO vwatch64_LABEL_53
    CASE 54: GOTO vwatch64_LABEL_54
    CASE 55: GOTO vwatch64_LABEL_55
    CASE 56: GOTO vwatch64_LABEL_56
    CASE 57: GOTO vwatch64_LABEL_57
    CASE 61: GOTO vwatch64_LABEL_61
    CASE 62: GOTO vwatch64_LABEL_62
    CASE 63: GOTO vwatch64_LABEL_63
    CASE 64: GOTO vwatch64_LABEL_64
    CASE 65: GOTO vwatch64_LABEL_65
    CASE 66: GOTO vwatch64_LABEL_66
    CASE 69: GOTO vwatch64_LABEL_69
    CASE 70: GOTO vwatch64_LABEL_70
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
    CASE 5: MyInteger% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 6: MyLong& = _CV(LONG, vwatch64_EXCHANGEDATA)
    CASE 7: MySingle! = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 8: MyDouble# = _CV(DOUBLE, vwatch64_EXCHANGEDATA)
    CASE 9: MyString$ = vwatch64_EXCHANGEDATA
    CASE 10: zRet = vwatch64_EXCHANGEDATA
    CASE 11: NewInteger% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 12: NewLong& = _CV(LONG, vwatch64_EXCHANGEDATA)
    CASE 13: NewSingle! = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 14: NewDouble# = _CV(DOUBLE, vwatch64_EXCHANGEDATA)
    CASE 15: NewString$ = vwatch64_EXCHANGEDATA
END SELECT
ON ERROR GOTO 0
RETURN

FUNCTION Array$ (Dim1, Dim2, Dim3, Value$)
    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1
vwatch64_LABEL_73:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(73): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_73 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_73
  SELECT CASE Value$
vwatch64_SKIP_73:::: 
    CASE ""
vwatch64_LABEL_75:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(75): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_75 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_75
      SELECT CASE Dim1
vwatch64_SKIP_75:::: 
        CASE IntegerArray
vwatch64_LABEL_77:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(77): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_77 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_77
          Array$ = LTRIM$(STR$(CVI(MID$(Mstuff(Dim2).IntegerArray, Dim3 * 2 - 1, 2))))
vwatch64_SKIP_77:::: 
        CASE LongArray
vwatch64_LABEL_79:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(79): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_79 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_79
          Array$ = LTRIM$(STR$(CVL(MID$(Mstuff(Dim2).LongArray, Dim3 * 4 - 1, 4))))
vwatch64_SKIP_79:::: 
        CASE SingleArray
vwatch64_LABEL_81:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(81): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_81 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_81
          Array$ = LTRIM$(STR$(CVS(MID$(Mstuff(Dim2).SingleArray, Dim3 * 4 - 1, 4))))
vwatch64_SKIP_81:::: 
        CASE DoubleArray
vwatch64_LABEL_83:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(83): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_83 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_83
          Array$ = LTRIM$(STR$(CVD(MID$(Mstuff(Dim2).DoubleArray, Dim3 * 8 - 1, 8))))
vwatch64_SKIP_83:::: 
        CASE StringArray
vwatch64_LABEL_85:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(85): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_85 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_85
          Array$ = LTRIM$(MID$(Mstuff(Dim2).StringArray, Dim3 * 8 - 1, 8))
vwatch64_SKIP_85:::: 
      END SELECT
    CASE ELSE
vwatch64_LABEL_88:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(88): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_88 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_88
      SELECT CASE Dim1
vwatch64_SKIP_88:::: 
        CASE IntegerArray
vwatch64_LABEL_90:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(90): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_90 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_90
          MID$(Mstuff(Dim2).IntegerArray, Dim3 * 2 - 1, 2) = MKI$(VAL(Value$))
vwatch64_SKIP_90:::: 
        CASE LongArray
vwatch64_LABEL_92:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(92): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_92 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_92
          MID$(Mstuff(Dim2).LongArray, Dim3 * 4 - 1, 4) = MKL$(VAL(Value$))
vwatch64_SKIP_92:::: 
        CASE SingleArray
vwatch64_LABEL_94:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(94): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_94 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_94
          MID$(Mstuff(Dim2).SingleArray, Dim3 * 4 - 1, 4) = MKS$(VAL(Value$))
vwatch64_SKIP_94:::: 
        CASE DoubleArray
vwatch64_LABEL_96:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(96): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_96 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_96
          MID$(Mstuff(Dim2).DoubleArray, Dim3 * 8 - 1, 8) = MKD$(VAL(Value$))
vwatch64_SKIP_96:::: 
        CASE StringArray
vwatch64_LABEL_98:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(98): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_98 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_98
          MID$(Mstuff(Dim2).StringArray, Dim3 * 8 - 1, 8) = Value$
vwatch64_SKIP_98:::: 
      END SELECT
  END SELECT

vwatch64_LABEL_102:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(102): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN vWATCH64_DUMMY%% = 0 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_102
vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1
EXIT FUNCTION
vwatch64_SETNEXTLINE:
SELECT CASE vwatch64_NEXTLINE
    CASE 73: GOTO vwatch64_LABEL_73
    CASE 75: GOTO vwatch64_LABEL_75
    CASE 77: GOTO vwatch64_LABEL_77
    CASE 79: GOTO vwatch64_LABEL_79
    CASE 81: GOTO vwatch64_LABEL_81
    CASE 83: GOTO vwatch64_LABEL_83
    CASE 85: GOTO vwatch64_LABEL_85
    CASE 88: GOTO vwatch64_LABEL_88
    CASE 90: GOTO vwatch64_LABEL_90
    CASE 92: GOTO vwatch64_LABEL_92
    CASE 94: GOTO vwatch64_LABEL_94
    CASE 96: GOTO vwatch64_LABEL_96
    CASE 98: GOTO vwatch64_LABEL_98
    CASE 102: GOTO vwatch64_LABEL_102
END SELECT

vwatch64_VARIABLEWATCH:
IF vwatch64_HEADER.CONNECTED = 0 THEN RETURN
ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(1).VALUE = STR$(Dim1)
    vwatch64_VARIABLEDATA(2).VALUE = STR$(Dim2)
    vwatch64_VARIABLEDATA(3).VALUE = STR$(Dim3)
    vwatch64_VARIABLEDATA(4).VALUE = Value$
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
    CASE 1: Dim1 = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 2: Dim2 = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 3: Dim3 = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 4: Value$ = vwatch64_EXCHANGEDATA
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

    vwatch64_CLIENTFILE = 11079
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
    vwatch64_CLIENT.NAME = "C:\Users\Arckex\qb64\programs\mine\linkedlist.bas"
    vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM
    vwatch64_CLIENT.TOTALSOURCELINES = 103
    vwatch64_CLIENT.TOTALVARIABLES = 15
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
    SHARED MyInteger%
    SHARED MyLong&
    SHARED MySingle!
    SHARED MyDouble#
    SHARED MyString$
    SHARED zRet
    SHARED NewInteger%
    SHARED NewLong&
    SHARED NewSingle!
    SHARED NewDouble#
    SHARED NewString$

    IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT SUB
    ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(5).VALUE = STR$(MyInteger%)
    vwatch64_VARIABLEDATA(6).VALUE = STR$(MyLong&)
    vwatch64_VARIABLEDATA(7).VALUE = STR$(MySingle!)
    vwatch64_VARIABLEDATA(8).VALUE = STR$(MyDouble#)
    vwatch64_VARIABLEDATA(9).VALUE = MyString$
    vwatch64_VARIABLEDATA(10).VALUE = zRet
    vwatch64_VARIABLEDATA(11).VALUE = STR$(NewInteger%)
    vwatch64_VARIABLEDATA(12).VALUE = STR$(NewLong&)
    vwatch64_VARIABLEDATA(13).VALUE = STR$(NewSingle!)
    vwatch64_VARIABLEDATA(14).VALUE = STR$(NewDouble#)
    vwatch64_VARIABLEDATA(15).VALUE = NewString$
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
    FOR i = 1 TO 15
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
