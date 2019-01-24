'look at dad python class

TYPE Node
  nodeValues AS STRING * 150
  nodeValue AS INTEGER
  nodeCounter AS INTEGER
END TYPE

CONST numnodes = 953
DIM SHARED numbers(numnodes) AS INTEGER
DIM SHARED nodes(numnodes, numnodes) AS Node
DIM SHARED runningtotal AS INTEGER

CLS
DIM in AS DOUBLE
DIM total AS DOUBLE
count = 0

OPEN "day1.txt" FOR INPUT AS #1
WHILE NOT EOF(1)
  count = count + 1
  INPUT #1, in
  numbers(count) = in
  runningtotal = runningtotal + in
WEND
CLOSE #1

PRINT runningtotal
PRINT count

runningtotal = 0
found% = 0
WHILE found% = 0
  FOR i = 1 TO numnodes
    runningtotal = runningtotal + numbers(i)
    found% = CheckRepeat(runningtotal)
    IF found% = 1 THEN EXIT FOR
  NEXT i
WEND

FUNCTION CheckRepeat% (count):
  PRINT count;
  hash% = ABS(count MOD numnodes)
  i% = 0
  DO
    i% = i% + 1
    IF nodes(hash%, i%).nodeValue = count THEN
      PRINT
      PRINT count
      PRINT "DONE!"
      CheckRepeat = 1
      END
    END IF
  LOOP WHILE nodes(hash%, i%).nodeCounter = 1
  nodes(hash%, i%).nodeValue = count
  nodes(hash%, i%).nodeCounter = nodes(hash%, i%).nodeCounter + 1
  CheckRepeat = 0
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
