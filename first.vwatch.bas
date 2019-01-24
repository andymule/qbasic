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
CONST vwatch64_CHECKSUM = "24D90408"
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
DIM SHARED vwatch64_BREAKPOINTLIST AS STRING * 101
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

DIM SHARED vwatch64_VARIABLES(1 TO 10) AS vwatch64_VARIABLESTYPE
DIM SHARED vwatch64_VARIABLEDATA(1 TO 10) AS vwatch64_VARIABLEVALUETYPE
DIM SHARED vwatch64_WATCHPOINTLIST AS STRING * 10
DIM SHARED vwatch64_WATCHPOINT(1 TO 10) AS vwatch64_VARIABLEVALUETYPE
vwatch64_VARIABLES(1).NAME = "numbers(0)"
vwatch64_VARIABLES(1).SCOPE = "SHARED"
vwatch64_VARIABLES(1).DATATYPE = "INTEGER"
vwatch64_VARIABLES(2).NAME = "runningtotal"
vwatch64_VARIABLES(2).SCOPE = "SHARED"
vwatch64_VARIABLES(2).DATATYPE = "INTEGER"
vwatch64_VARIABLES(3).NAME = "in"
vwatch64_VARIABLES(3).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(3).DATATYPE = "DOUBLE"
vwatch64_VARIABLES(4).NAME = "total"
vwatch64_VARIABLES(4).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(4).DATATYPE = "DOUBLE"
vwatch64_VARIABLES(5).NAME = "count"
vwatch64_VARIABLES(5).SCOPE = "FUNCTION CheckRepeat%"
vwatch64_VARIABLES(5).DATATYPE = "SINGLE"
vwatch64_VARIABLES(6).NAME = "count"
vwatch64_VARIABLES(6).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(6).DATATYPE = "SINGLE"
vwatch64_VARIABLES(7).NAME = "found%"
vwatch64_VARIABLES(7).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(7).DATATYPE = "INTEGER"
vwatch64_VARIABLES(8).NAME = "i"
vwatch64_VARIABLES(8).SCOPE = "MAIN MODULE"
vwatch64_VARIABLES(8).DATATYPE = "SINGLE"
vwatch64_VARIABLES(9).NAME = "hash%"
vwatch64_VARIABLES(9).SCOPE = "FUNCTION CheckRepeat%"
vwatch64_VARIABLES(9).DATATYPE = "INTEGER"
vwatch64_VARIABLES(10).NAME = "i%"
vwatch64_VARIABLES(10).SCOPE = "FUNCTION CheckRepeat%"
vwatch64_VARIABLES(10).DATATYPE = "INTEGER"

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

'look at dad python class

'SCREEN 13
'_FULLSCREEN

TYPE Node
  nodeValue AS INTEGER ' In this example its a linked list of INTEGERS
  nodeCounter AS INTEGER ' 2 bytes
END TYPE

CONST numnodes = 953
DIM SHARED numbers(numnodes) AS INTEGER
DIM SHARED nodes(numnodes, numnodes) AS Node
DIM SHARED runningtotal AS INTEGER

vwatch64_LABEL_16:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(16): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_16 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_16
CLS
vwatch64_SKIP_16:::: 
DIM in AS DOUBLE
DIM total AS DOUBLE
vwatch64_LABEL_19:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(19): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_19 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_19
count = 0
vwatch64_SKIP_19:::: 

vwatch64_LABEL_21:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(21): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_21 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_21
OPEN "day1.txt" FOR INPUT AS #1
vwatch64_SKIP_21:::: 
vwatch64_LABEL_22:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(22): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_22 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_22
WHILE NOT EOF(1)
vwatch64_SKIP_22:::: 
vwatch64_LABEL_23:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(23): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_23 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_23
  count = count + 1
vwatch64_SKIP_23:::: 
vwatch64_LABEL_24:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(24): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_24 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_24
  INPUT #1, in
vwatch64_SKIP_24:::: 
  'PRINT in
vwatch64_LABEL_26:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(26): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_26 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_26
  numbers(count) = in
vwatch64_SKIP_26:::: 
vwatch64_LABEL_27:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(27): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_27 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_27
  runningtotal = runningtotal + in
vwatch64_SKIP_27:::: 
vwatch64_LABEL_28:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(28): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_28 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_28
WEND
vwatch64_SKIP_28:::: 
vwatch64_LABEL_29:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(29): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_29 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_29
CLOSE #1
vwatch64_SKIP_29:::: 

vwatch64_LABEL_31:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(31): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_31 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_31
PRINT runningtotal
vwatch64_SKIP_31:::: 
vwatch64_LABEL_32:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(32): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_32 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_32
PRINT count
vwatch64_SKIP_32:::: 

vwatch64_LABEL_34:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(34): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_34 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_34
runningtotal = 0
vwatch64_SKIP_34:::: 
vwatch64_LABEL_35:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(35): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_35 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_35
found% = 0
vwatch64_SKIP_35:::: 
vwatch64_LABEL_36:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(36): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_36 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_36
WHILE found% = 0
vwatch64_SKIP_36:::: 
vwatch64_LABEL_37:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(37): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_37 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_37
  FOR i = 1 TO numnodes
vwatch64_SKIP_37:::: 
vwatch64_LABEL_38:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(38): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_38 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_38
    runningtotal = runningtotal + numbers(i)
vwatch64_SKIP_38:::: 
vwatch64_LABEL_39:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(39): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_39 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_39
    found% = CheckRepeat(runningtotal)
vwatch64_SKIP_39:::: 
vwatch64_LABEL_40:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(40): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_40 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_40
    IF found% = 1 THEN EXIT FOR
vwatch64_SKIP_40:::: 
vwatch64_LABEL_41:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(41): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_41 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_41
  NEXT i
vwatch64_SKIP_41:::: 
vwatch64_LABEL_42:::: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(42): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_42 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_42
WEND
vwatch64_SKIP_42:::: 

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
    CASE 16: GOTO vwatch64_LABEL_16
    CASE 19: GOTO vwatch64_LABEL_19
    CASE 21: GOTO vwatch64_LABEL_21
    CASE 22: GOTO vwatch64_LABEL_22
    CASE 23: GOTO vwatch64_LABEL_23
    CASE 24: GOTO vwatch64_LABEL_24
    CASE 26: GOTO vwatch64_LABEL_26
    CASE 27: GOTO vwatch64_LABEL_27
    CASE 28: GOTO vwatch64_LABEL_28
    CASE 29: GOTO vwatch64_LABEL_29
    CASE 31: GOTO vwatch64_LABEL_31
    CASE 32: GOTO vwatch64_LABEL_32
    CASE 34: GOTO vwatch64_LABEL_34
    CASE 35: GOTO vwatch64_LABEL_35
    CASE 36: GOTO vwatch64_LABEL_36
    CASE 37: GOTO vwatch64_LABEL_37
    CASE 38: GOTO vwatch64_LABEL_38
    CASE 39: GOTO vwatch64_LABEL_39
    CASE 40: GOTO vwatch64_LABEL_40
    CASE 41: GOTO vwatch64_LABEL_41
    CASE 42: GOTO vwatch64_LABEL_42
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
    CASE 1: numbers(0) = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 2: runningtotal = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 3: in = _CV(DOUBLE, vwatch64_EXCHANGEDATA)
    CASE 4: total = _CV(DOUBLE, vwatch64_EXCHANGEDATA)
    CASE 6: count = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 7: found% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 8: i = _CV(SINGLE, vwatch64_EXCHANGEDATA)
END SELECT
ON ERROR GOTO 0
RETURN

FUNCTION CheckRepeat% (count):
    vwatch64_SUBLEVEL = vwatch64_SUBLEVEL + 1
vwatch64_LABEL_45:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(45): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_45 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_45
  PRINT count;
vwatch64_SKIP_45:::: 
vwatch64_LABEL_46:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(46): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_46 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_46
  hash% = ABS(count MOD numnodes)
vwatch64_SKIP_46:::: 
vwatch64_LABEL_47:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(47): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_47 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_47
  i% = 0
vwatch64_SKIP_47:::: 
vwatch64_LABEL_48:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(48): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_48 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_48
  DO
vwatch64_SKIP_48:::: 
vwatch64_LABEL_49:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(49): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_49 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_49
    i% = i% + 1
vwatch64_SKIP_49:::: 
vwatch64_LABEL_50:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(50): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_50 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_50
    IF nodes(hash%, i%).nodeValue = count THEN
vwatch64_SKIP_50:::: 
vwatch64_LABEL_51:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(51): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_51 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_51
      PRINT
vwatch64_SKIP_51:::: 
vwatch64_LABEL_52:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(52): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_52 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_52
      PRINT count
vwatch64_SKIP_52:::: 
vwatch64_LABEL_53:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(53): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_53 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_53
      PRINT "DONE!"
vwatch64_SKIP_53:::: 
vwatch64_LABEL_54:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(54): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_54 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_54
      CheckRepeat = 1
vwatch64_SKIP_54:::: 
vwatch64_LABEL_55:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(55): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_55 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_55
ON ERROR GOTO vwatch64_FILEERROR
IF vwatch64_HEADER.CONNECTED THEN
    vwatch64_HEADER.CONNECTED = 0
    PUT #vwatch64_CLIENTFILE, 1, vwatch64_HEADER
END IF
CLOSE #vwatch64_CLIENTFILE
KILL "C:\Users\Arckex\qb64\vwatch64.dat"
ON ERROR GOTO 0
      END
vwatch64_SKIP_55:::: 
vwatch64_LABEL_56:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(56): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_56 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_56
    END IF
vwatch64_SKIP_56:::: 
vwatch64_LABEL_57:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(57): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_57 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_57
  LOOP WHILE nodes(hash%, i%).nodeCounter = 1
vwatch64_SKIP_57:::: 
vwatch64_LABEL_58:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(58): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_58 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_58
  nodes(hash%, i%).nodeValue = count
vwatch64_SKIP_58:::: 
vwatch64_LABEL_59:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(59): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_59 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_59
  nodes(hash%, i%).nodeCounter = nodes(hash%, i%).nodeCounter + 1
vwatch64_SKIP_59:::: 
vwatch64_LABEL_60:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(60): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN GOTO vwatch64_SKIP_60 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_60
  CheckRepeat = 0
vwatch64_SKIP_60:::: 
vwatch64_LABEL_61:::: GOSUB vwatch64_VARIABLEWATCH: vwatch64_NEXTLINE = vwatch64_CHECKBREAKPOINT(61): IF vwatch64_NEXTLINE > 0 THEN GOTO vwatch64_SETNEXTLINE ELSE IF vwatch64_NEXTLINE = -2 THEN vWATCH64_DUMMY%% = 0 ELSE IF vwatch64_NEXTLINE = -1 THEN GOSUB vwatch64_SETVARIABLE: GOTO vwatch64_LABEL_61
vwatch64_SUBLEVEL = vwatch64_SUBLEVEL - 1
EXIT FUNCTION
vwatch64_SETNEXTLINE:
SELECT CASE vwatch64_NEXTLINE
    CASE 45: GOTO vwatch64_LABEL_45
    CASE 46: GOTO vwatch64_LABEL_46
    CASE 47: GOTO vwatch64_LABEL_47
    CASE 48: GOTO vwatch64_LABEL_48
    CASE 49: GOTO vwatch64_LABEL_49
    CASE 50: GOTO vwatch64_LABEL_50
    CASE 51: GOTO vwatch64_LABEL_51
    CASE 52: GOTO vwatch64_LABEL_52
    CASE 53: GOTO vwatch64_LABEL_53
    CASE 54: GOTO vwatch64_LABEL_54
    CASE 55: GOTO vwatch64_LABEL_55
    CASE 56: GOTO vwatch64_LABEL_56
    CASE 57: GOTO vwatch64_LABEL_57
    CASE 58: GOTO vwatch64_LABEL_58
    CASE 59: GOTO vwatch64_LABEL_59
    CASE 60: GOTO vwatch64_LABEL_60
    CASE 61: GOTO vwatch64_LABEL_61
END SELECT

vwatch64_VARIABLEWATCH:
IF vwatch64_HEADER.CONNECTED = 0 THEN RETURN
ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(1).VALUE = STR$(numbers(0))
    vwatch64_VARIABLEDATA(2).VALUE = STR$(runningtotal)
    vwatch64_VARIABLEDATA(5).VALUE = STR$(count)
    vwatch64_VARIABLEDATA(9).VALUE = STR$(hash%)
    vwatch64_VARIABLEDATA(10).VALUE = STR$(i%)
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
    CASE 1: numbers(0) = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 2: runningtotal = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 5: count = _CV(SINGLE, vwatch64_EXCHANGEDATA)
    CASE 9: hash% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
    CASE 10: i% = _CV(INTEGER, vwatch64_EXCHANGEDATA)
END SELECT
GOSUB vwatch64_VARIABLEWATCH
ON ERROR GOTO 0
RETURN
END FUNCTION

'DIM myPtr AS LONG
'DIM myMemory(10000) AS INTEGER
'myPtr = VARPTR(myMemory(0))
'POKE myPtr + 30000, 100 'because we dimed the array as integer, it actually holds 40'004 bytes,
'' remember it start at 0
'PRINT PEEK(myPtr + 30000)

'SUB Hash:
'  'hash the prefix & suffix(there are also many ways to do this...)
'  index = ((Prefix * 256&) XOR Suffix) MOD Table.Size
'  'Calculate an offset just in case we don't find what we want on the
'  'first try...
'  IF index = 0 THEN 'can't have Table.Size-0 !
'    Offset = 1
'  ELSE
'    Offset = Table.Size - index
'  END IF
'  DO 'until we (1) find an empty entry or (2) find what we're lookin for
'    IF code(index) = -1 THEN 'is this entry
'      Found = FALSE 'yup- we didn't find the string
'      RETURN
'      'is this entry the one we're looking for?
'    ELSEIF Prefix(index) = Prefix AND Suffix(index) = Suffix THEN
'      'yup, congrats you now understand hashing!!!
'      Found = TRUE
'      RETURN
'    ELSE
'      'shoot! we didn't find anything interesting, so we must
'      'retry- this is what slows hashing down. I could of used
'      'a bigger table, that would of speeded things up a little
'      'because this retrying would not happen as often...
'      index = index - Offset
'      IF index < 0 THEN 'too far down the table?
'        'wrap back the index to the end of the table
'        index = index + Table.Size
'      END IF
'    END IF
'  LOOP
'END SUB


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

    vwatch64_CLIENTFILE = 28447
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
    vwatch64_CLIENT.NAME = "C:\Users\Arckex\qb64\programs\mine\first.bas"
    vwatch64_CLIENT.CHECKSUM = vwatch64_CHECKSUM
    vwatch64_CLIENT.TOTALSOURCELINES = 101
    vwatch64_CLIENT.TOTALVARIABLES = 10
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
    SHARED in AS DOUBLE
    SHARED total AS DOUBLE
    SHARED count
    SHARED found%
    SHARED i

    IF vwatch64_HEADER.CONNECTED = 0 THEN EXIT SUB
    ON ERROR GOTO vwatch64_FILEERROR
    vwatch64_VARIABLEDATA(1).VALUE = STR$(numbers(0))
    vwatch64_VARIABLEDATA(2).VALUE = STR$(runningtotal)
    vwatch64_VARIABLEDATA(3).VALUE = STR$(in)
    vwatch64_VARIABLEDATA(4).VALUE = STR$(total)
    vwatch64_VARIABLEDATA(6).VALUE = STR$(count)
    vwatch64_VARIABLEDATA(7).VALUE = STR$(found%)
    vwatch64_VARIABLEDATA(8).VALUE = STR$(i)
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
    FOR i = 1 TO 10
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
