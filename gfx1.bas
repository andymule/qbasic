SCREEN 13

PSET (0, 0), ASC("H") 'top left corner of screen
PSET (1, 0), ASC("E")
PSET (2, 0), ASC("L")
PSET (3, 0), ASC("L")
PSET (4, 0), ASC("O")

DIM m AS _MEM

m = _MEMIMAGE(0) 'copy the screen memory to m

x1$ = _MEMGET(m, m.OFFSET, STRING * 5) 'get at block start position

LOCATE 2, 1: PRINT LEN(x1$) 'prints 5 bytes as size is STRING * 5

PRINT x1$ 'prints HELLO as ASCII character values

PRINT m.OFFSET; m.SIZE; m.ELEMENTSIZE

_MEMFREE m

