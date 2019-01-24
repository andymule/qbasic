DIM SW AS INTEGER
DIM SH AS INTEGER
DIM Colors AS INTEGER
DIM x AS INTEGER

SW = 1920
SH = 1080

SCREEN _NEWIMAGE(SW, SH, 256)

_FULLSCREEN

_MOUSEHIDE

Colors = 32

FOR x = 1 TO SW / 2 STEP 10

  _LIMIT 15

  CIRCLE ((SW / 2 - x - 1), (SH / 2 - 1)), x, Colors
  CIRCLE ((SW / 2 + x - 1), (SH / 2 - 1)), x, Colors
  CIRCLE ((SW / 2 - 1), (SH / 2 - x - 1)), x, Colors
  CIRCLE ((SW / 2 - 1), (SH / 2 + x - 1)), x, Colors

  Colors = Colors + 1

  IF Colors = 62 THEN
    Colors = 32
  END IF

  _DISPLAY
NEXT

_MOUSESHOW


