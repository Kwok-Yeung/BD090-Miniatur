*&---------------------------------------------------------------------*
*& Report zbd090_report
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbd090_report_0001.

BREAK-POINT.

"Einfaches Selektionsfeld:
PARAMETERS p_bukrs TYPE bukrs.

"Eine Leerzeile
SELECTION-SCREEN SKIP 1.

"Ankreuzfeld:
PARAMETERS p_test AS CHECKBOX.

"Auswahlknöpfe:
PARAMETERS p_rb_a RADIOBUTTON GROUP gr1.
PARAMETERS p_rb_b RADIOBUTTON GROUP gr1.
PARAMETERS p_rb_c RADIOBUTTON GROUP gr1.

"Mehrfachselektionsfeld
DATA gf_help_land1 TYPE land1.
SELECT-OPTIONS s_land1 FOR gf_help_land1.


BREAK-POINT.

"Liste: Texte an bestimmter Position mit bestimmter Farbe ausgeben
FORMAT COLOR 3.
WRITE: /13 'A', /12 'BBB', /11 'CCCCC'.
FORMAT COLOR OFF.
WRITE 30 'Vier' COLOR COL_POSITIVE.
WRITE / 'Fünf' UNDER 'Vier'.

IF 2 = 3.
  WRITE: / 'Bedingung ''IF 2 = 3'' ist wahr.'.
ELSEIF 2 < 3.
  WRITE: / 'Bedingung ''IF 2 < 3'' ist wahr.'.
ELSEIF 2 > 3.
  WRITE: / 'Bedingung ''IF 2 > 3'' ist wahr.'.
ELSE.
  WRITE: 'Keine Bedingung ist erfüllt.'.
ENDIF.

DATA lf_boss TYPE string.
lf_boss = 'Hugo'.
IF lf_boss = 'HUGO' OR lf_boss = 'hugo'.
  WRITE: / 'IF-Anweisungen ignorieren die Groß-/Kleinschreibung.'.
ELSEIF 2 < 7 AND 7 < 10.
  WRITE: / '7 liegt ist größer als 2 und kleiner als 10.'.
ENDIF.
*}Sitzung 1
