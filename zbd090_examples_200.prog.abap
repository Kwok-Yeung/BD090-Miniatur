*&---------------------------------------------------------------------*
*& Report zbd090_examples_200
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbd090_examples_200.

TYPES tt_makt TYPE STANDARD TABLE OF makt.

PARAMETERS p_0003 RADIOBUTTON GROUP ex. "0003:Types, variables
PARAMETERS p_0004 RADIOBUTTON GROUP ex. "0004:itab define
PARAMETERS p_0005_1 RADIOBUTTON GROUP ex. "0005:itab SELECT
PARAMETERS p_0005_2 RADIOBUTTON GROUP ex. "0005:itab LOOP
PARAMETERS p_0006_1 RADIOBUTTON GROUP ex. "0006:itab headline
PARAMETERS p_0006_2 RADIOBUTTON GROUP ex. "0006:itab COLLECT
PARAMETERS p_0007_1 RADIOBUTTON GROUP ex. "0007:substring
PARAMETERS p_0007_2 RADIOBUTTON GROUP ex. "0007:CONCATENATE
PARAMETERS p_0008_1 RADIOBUTTON GROUP ex. "0008:form
PARAMETERS p_0008_2 RADIOBUTTON GROUP ex. "0008:macro
PARAMETERS p_0009 RADIOBUTTON GROUP ex. "0009:local class
PARAMETERS p_0010_1 RADIOBUTTON GROUP ex. "0010:field symbol performance
PARAMETERS p_0010_2 RADIOBUTTON GROUP ex. "0010:field symbol static/dynamic
PARAMETERS p_0010_3 RADIOBUTTON GROUP ex. "0010:field symbol structure fields
PARAMETERS p_0011 RADIOBUTTON GROUP ex. "0011:alv reuse
*{Sitzung 11
PARAMETERS p_liste1 RADIOBUTTON GROUP lis.
PARAMETERS p_liste2 RADIOBUTTON GROUP lis.
*}Sitzung 11


CLASS lcl_main DEFINITION DEFERRED.
DATA go_main TYPE REF TO lcl_main.


CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS ev_start_of_selection.


  PRIVATE SECTION.

    METHODS ex_0003_types_variables.
    METHODS ex_0004_itab_define.
    METHODS ex_0005_itab_select.
    METHODS ex_0005_itab_loop.
    METHODS ex_0006_itab_headline.
    METHODS ex_0006_itab_collect.
    METHODS ex_0007_string_substring.
    METHODS ex_0007_string_concatenate.
    METHODS ex_0008_form.
    METHODS ex_0008_macro.
    METHODS ex_0009_local_class.
    METHODS ex_0010_field_sym_performance.
    METHODS ex_0010_field_sym_dynamic.
    METHODS ex_0010_field_sym_all_fields.
    METHODS ex_0011_alv_reuse.
ENDCLASS.

*{Sitzung 9
"Definition der Kasse.
*----------------------------------------------------------------------*
*       CLASS lcl_fahrrad DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fahrrad DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS constructor
      IMPORTING
        if_farbe TYPE string
        if_gang  TYPE i.
    METHODS hochschalten.
    METHODS runterschalten.
    METHODS daten_holen
      EXPORTING
        ef_farbe TYPE string
        ef_gang  TYPE i.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mf_farbe TYPE string.
    DATA mf_gang  TYPE i.
    CLASS-DATA gf_egal TYPE text10.
ENDCLASS.                    "lcl_fahrrad DEFINITION
*}Sitzung 9

CLASS lcl_main IMPLEMENTATION.
  METHOD ev_start_of_selection.
    CASE abap_true.
      WHEN p_0003.
        go_main->ex_0003_types_variables(  ).
      WHEN p_0004.
        go_main->ex_0004_itab_define( ).
      WHEN p_0005_1.
        go_main->ex_0005_itab_select(  ).
      WHEN p_0005_2.
        go_main->ex_0005_itab_loop(  ).
      WHEN p_0006_1.
        go_main->ex_0006_itab_headline( ).
      WHEN p_0006_2.
        go_main->ex_0006_itab_collect( ).
      WHEN p_0007_1.
        go_main->ex_0007_string_substring( ).
      WHEN p_0007_2.
        go_main->ex_0007_string_concatenate( ).
      WHEN p_0008_1.
        go_main->ex_0008_form( ).
      WHEN p_0008_2.
        go_main->ex_0008_macro( ).
      WHEN p_0009.
        go_main->ex_0009_local_class( ).
      WHEN p_0010_1.
        go_main->ex_0010_field_sym_performance( ).
      WHEN p_0010_2.
        go_main->ex_0010_field_sym_dynamic( ).
      WHEN p_0010_3.
        go_main->ex_0010_field_sym_all_fields( ).
      WHEN p_0011.
        go_main->ex_0011_alv_reuse( ).
    ENDCASE.
  ENDMETHOD.

  METHOD ex_0003_types_variables.
*{Sitzung 3
    "--- Feld, Typ für ein Feld, Konstante
    TYPES tf_vorname TYPE vorna.
    DATA lf_vorname TYPE vorna. "VALUE 'Hugo'.
    CONSTANTS gc_vorname TYPE vorna VALUE 'Hugo'.

    BREAK-POINT. "Der Debugger hält hier an, bei jedem Benutzer, (wenn Berechtigung vorhanden)!

    "--- Struktur, Type für eine Struktur
    TYPES ts_makt TYPE makt.
    TYPES: BEGIN OF ts_mensch,
             name       TYPE c LENGTH 35,
             hausnummer TYPE c LENGTH 5,
             alter      TYPE i,
             datum      TYPE sy-datum,
             posnr      TYPE n LENGTH 3,
           END OF ts_mensch.
    DATA ls_makt TYPE ts_makt.
    DATA ls_mensch TYPE ts_mensch.

    BREAK csu. "Der Debugger hält hier an, bei Benutzer CSU.

    "Zugriff auf ein Feld einer Struktur
    ls_makt-matnr = 'C3PO'. "Alphanumerische Festwerte in ' '
    ls_mensch-alter = 33.   "Ganze Zahlen als Festwerte ohne ' '
    ls_mensch-posnr = `133`."Variablen vo Typ n als Festwerte in ` `

    WRITE / ls_makt-matnr.

    "---Interne Tabelle, Typ für interne Tabellen

    "Deklaration
    TYPES tt_mensch TYPE STANDARD TABLE OF ts_mensch.
    DATA lt_mensch TYPE tt_mensch.

    "Interne Tabelle mit Testdaten füllen
    ls_mensch-name       = 'Albert'.
    ls_mensch-hausnummer = '35A'.
    ls_mensch-alter      = 17.
    APPEND ls_mensch TO lt_mensch.
    ls_mensch-name       = 'Susi'.
    ls_mensch-alter      = 18.
    APPEND ls_mensch TO lt_mensch.

    BREAK-POINT.
*}Sitzung 3
  ENDMETHOD.

  METHOD ex_0004_itab_define.
*{Sitzung 4
    TYPES: BEGIN OF ts_name,
             vorna TYPE vorna,
             nachn TYPE nachn,
           END OF ts_name.

    "Struktur ls_NAME enthält 2 Felder: VORNA, NACHN
    DATA ls_name TYPE ts_name.

    BREAK-POINT.

*--- INCLUDE erweitert eine Struktur um zusätzliche Felder
    TYPES: BEGIN OF ts_adresse1.
             INCLUDE TYPE ts_name. "Alternative: INCLUDE STRUCTURE ls_name.
    TYPES:   stras TYPE kna1-stras,
             land1 TYPE kna1-land1,
           END OF ts_adresse1.

    "Struktur ls_ADRESSE1 enthält 4 Felder: VORNA, NACHN, STRAS, LAND1
    DATA ls_adresse1 TYPE ts_adresse1.

    BREAK-POINT.

*--- Geschachtelte Struktur (Struktur in einer Struktur)
    TYPES: BEGIN OF ts_adresse2,
             s_name TYPE ts_name,
             stras  TYPE kna1-stras,
             land1  TYPE kna1-land1,
           END OF ts_adresse2.

    "Struktur ls_ADRESSE2 enthält 3 Komponenten: Struktur S_NAME und die Felder STRAS, LAND1
    DATA ls_adresse2 TYPE ts_adresse2.

    DATA lt_name TYPE STANDARD TABLE OF ts_name.

    BREAK-POINT.
*}Sitzung 4
  ENDMETHOD.

  METHOD ex_0005_itab_loop.
*{Sitzung 5
    DATA lt_makt TYPE STANDARD TABLE OF makt.

    SELECT * FROM makt INTO TABLE lt_makt UP TO 13 ROWS.


    BREAK-POINT.

    "--- Alle Datensätze einer internen Tabelle ausgeben

    DATA ls_makt TYPE makt.

    LOOP AT lt_makt INTO ls_makt.
      "ls_MAKT wird bei jedem Durchlauf überschrieben.
      IF sy-tabix = 3.
        CONTINUE.
      ELSEIF sy-tabix = 6.
        EXIT.
      ENDIF.
      WRITE: / ls_makt-matnr, ls_makt-spras, ls_makt-maktx.
    ENDLOOP.
*}Sitzung 5
  ENDMETHOD.

  METHOD ex_0005_itab_select.
*{Sitzung 5
    "--- SELECT-Anweisung: Bestimmte Felder eines Datensatzes lesen

    DATA lf_landk TYPE t005-landk.
    DATA lf_xegld TYPE t005-xegld.
    DATA ls_t005 TYPE t005.
    DATA lt_makt TYPE STANDARD TABLE OF makt.

    BREAK-POINT.

    SELECT SINGLE landk FROM t005 INTO lf_landk
    WHERE land1 = '+-'.
    "War die Anweisung erfolgreich?
    IF sy-subrc = 0.
      WRITE / 'Land +- existiert.'.
    ELSE.
      WRITE / 'Land +- existiert nicht.'.
    ENDIF.

    SELECT SINGLE landk xegld FROM t005 INTO (lf_landk, lf_xegld)
    WHERE land1 = 'DE'.
    "War die Anweisung erfolgreich?
    IF sy-subrc = 0.
      WRITE / 'Land DE existiert.'.
    ELSE.
      WRITE / 'Land DE existiert nicht.'.
    ENDIF.

    "--- SELECT-Anweisung: Alle Felder mehrerer Datensätze lesen

    SELECT * FROM makt INTO TABLE lt_makt UP TO 13 ROWS.

*}Sitzung 5
  ENDMETHOD.

  METHOD ex_0006_itab_headline.
*{Sitzung 6
    PERFORM ex_0006_itab_headline.
*}Sitzung 6
  ENDMETHOD.

  METHOD ex_0006_itab_collect.
*{Sitzung 6
    PERFORM ex_0006_itab_collect.
*}Sitzung 6
  ENDMETHOD.

  METHOD ex_0007_string_substring.
*{Sitzung 7
    DATA lf_atbez TYPE cabnt-atbez VALUE 'Merkmalsbezeichnung'.
    DATA lf_i TYPE i.
    DATA lf_kunnr TYPE kunnr VALUE '1234567890'.

    BREAK-POINT.

*--- Zugritt auf einen Teil einer Zeichenkette
    lf_kunnr+2(4) = lf_atbez+3(4).

*--- Länge einer Zeichenkette, Länge eines Feldes
    lf_kunnr = 'Hugo'.
    lf_i = strlen( lf_kunnr ).
    DESCRIBE FIELD lf_kunnr LENGTH lf_i IN CHARACTER MODE.

*}Sitzung 7
  ENDMETHOD.

  METHOD ex_0007_string_concatenate.
*{Sitzung 7

*--- CONCATENATE
    DATA lf_text TYPE c LENGTH 53.
    DATA lf_text1 TYPE c LENGTH 10 VALUE 'Hugo'.
    DATA lf_text2 TYPE n LENGTH 5 VALUE '00013'.
    DATA lf_text3 TYPE d VALUE '20001225'.

    BREAK-POINT.

    CONCATENATE lf_text1 lf_text2 'ABC' lf_text3 INTO lf_text.
    "=> lf_text = 'Hugo00013ABC20001225'

    CONCATENATE lf_text1 lf_text2 'ABC' lf_text3 INTO lf_text
      SEPARATED BY ', '.
    "=> lf_text = 'Hugo, 00013, ABC, 20001225'

    CONCATENATE lf_text1 lf_text2 'ABC' lf_text3 INTO lf_text
      RESPECTING BLANKS.
    "=> lf_text = 'Hugo      00013ABC20001225'

    "Alternative ab 7.?:
    lf_text = lf_text1 && lf_text2 && 'ABC' && lf_text3.
    "=> lf_text = 'Hugo00013ABC20001225'

    lf_text = |Fester Text 1 { lf_text1 } Fester Text 2{ lf_text2 }|.
    "=> lf_text = 'Fester Text 1 Hugo Fester Text 200013'


*---CONDENSE
    DATA lf_text4 TYPE c LENGTH 30 VALUE 'Hugo     das  Auto   .'.
    CONDENSE lf_text4.    "=> lf_text1 = 'Hugo das Auto .'
    CONDENSE lf_text4 NO-GAPS.  "=> lf_text1 = 'HugodasAuto.'

    BREAK-POINT.
*}Sitzung 7
  ENDMETHOD.

  METHOD ex_0008_form.
*{Sitzung 8
    TYPES tt_makt TYPE STANDARD TABLE OF makt.
    DATA ls_makt TYPE makt.
    DATA lt_makt TYPE tt_makt.

    "Testdaten zusammenstellen
    ls_makt-matnr = '4711'.
    APPEND ls_makt TO lt_makt.

    BREAK-POINT.

    "Form-Routine mit typisierten Parametern aufrufen
    "Lokale Daten
    PERFORM daten_ausgeben1
    USING ls_makt-matnr
          ls_makt
          lt_makt.

    "Form-Routine ohne typisierte Parameter aufrufen
    PERFORM daten_ausgeben2
    USING ls_makt-matnr
          ls_makt
          lt_makt.

    BREAK-POINT.

*}Sitzung 8
  ENDMETHOD.

  METHOD ex_0008_macro.
*{Sitzung 8

    BREAK-POINT.

    DATA lf_i1 TYPE i.
    DATA lf_i2 TYPE i.

    DEFINE mein_macro.
      "Beliebiger Quelltext.
      "Zur Verfügung stehende Parameter: &1, &2, &3, ... &9
      lf_i1 = &1 + &2.
      lf_i2 = &1 * &2.
      WRITE: / 'Zahl 1 =', &1, 'Zahl 2 =', &2, 'Summe =', lf_i1, 'Produkt =', lf_i2.
    END-OF-DEFINITION.

    "Macros können nicht debugged werden.
    "Dort aufgerufene andere Modularisierungseinheiten (z.B. Form-Routinen)
    "können jedoch debugged werden, wenn man dort einen Break-Point setzt.
    mein_macro:
    5       2, "&1 = 5  , &2 = 2.
    7       3, "&1 = 7  , &2 = 3.
    127    44. "&1 = 127, &2 = 44.

    BREAK-POINT.
*}Sitzung 8
  ENDMETHOD.

  METHOD ex_0009_local_class.
*{Sitzung 9
    "Verwendung der lokalen Klasse LCL_FAHRRAD.
    DATA lf_farbe TYPE string.
    DATA lf_gang TYPE i.
    DATA lo_fahrrad1 TYPE REF TO lcl_fahrrad.
    DATA lo_fahrrad2 TYPE REF TO lcl_fahrrad.

    BREAK-POINT.

    "Fahrräder erzeugen.
    CREATE OBJECT lo_fahrrad1
      EXPORTING
        if_farbe = 'Grün'
        if_gang  = 1.

    CREATE OBJECT lo_fahrrad2
      EXPORTING
        if_farbe = 'Rot'
        if_gang  = 1.


    "Gangschaltung verwenden
    lo_fahrrad1->hochschalten( ).        "mf_gang: 1 -> 2
    lo_fahrrad1->hochschalten( ).        "mf_gang: 2 -> 3
    lo_fahrrad2->hochschalten( ).        "mf_gang: 1 -> 2
    lo_fahrrad2->runterschalten( ).      "mf_gang: 2 -> 1


    "Daten der Fahrräder holen und ausgeben.
    CALL METHOD lo_fahrrad1->daten_holen
      IMPORTING
        ef_farbe = lf_farbe          "'Grün'
        ef_gang  = lf_gang.        "3

    WRITE: / 'Fahrrad 1:', lf_farbe, lf_gang.

    IF lo_fahrrad2 IS BOUND.
      CALL METHOD lo_fahrrad2->daten_holen
        IMPORTING
          ef_farbe = lf_farbe          "'Rot'
          ef_gang  = lf_gang.        "1

      WRITE: / 'Fahrrad 2:', lf_farbe, lf_gang.
    ELSE.
      WRITE: / 'Instanzvariable lo_fahrrad2 zeigt auf keine Instanz.'.
    ENDIF.

    "Fehler: Instanzmethode über eine Instanzvariable aufrufen,
    "        die auf keine Instanz zeigt.

    CLEAR lo_fahrrad1.

    DATA lo_x_root TYPE REF TO cx_root.

    TRY.

        CALL METHOD lo_fahrrad1->daten_holen
          IMPORTING
            ef_farbe = lf_farbe
            ef_gang  = lf_gang.

      CATCH cx_root INTO lo_x_root.
        BREAK-POINT.
        WRITE: / 'Instanzvariable lo_fahrrad1 zeigt auf keine Instanz.'.
        RETURN.
    ENDTRY.

    WRITE: / 'Fahrrad 1:', lf_farbe, lf_gang.
*}Sitzung 9
  ENDMETHOD.

  METHOD ex_0010_field_sym_performance.
*{Sitzung 10

    "--- Feldsymbole deklarieren (bitte fertige Typen angeben!)
    BREAK-POINT.

    FIELD-SYMBOLS <lf_kunnr> TYPE kna1-kunnr.
    FIELD-SYMBOLS <lf_alter> TYPE i.

    "FIELD-SYMBOLS <lf_betrag> TYPE p LENGTH 8 DECIMALS 2. "Syntaxfehler!
    TYPES tf_betrag TYPE p LENGTH 8 DECIMALS 2.
    FIELD-SYMBOLS <lf_betrag> TYPE tf_betrag.

    FIELD-SYMBOLS <ls_makt> TYPE makt.
    "FIELD-SYMBOLS <lt_makt> type standard table of makt. "Syntaxfehler!
    TYPES tt_makt TYPE STANDARD TABLE OF makt.
    FIELD-SYMBOLS <lt_makt> TYPE tt_makt.


    "--- ab 7.40 zusätzlich möglich:
    BREAK-POINT.

*LOOP AT lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt1>).
*  "...
*ENDLOOP.

*READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt2>) WITH KEY ... .


    "--- Performancegewinn durch Verzicht auf Kopien und MODIFY-Befehl
    DATA ls_makt TYPE makt.
    DATA lt_makt TYPE STANDARD TABLE OF makt.
    ls_makt-spras = sy-langu.
    ls_makt-matnr = 'M1'. ls_makt-maktx = 'eeeins'. APPEND ls_makt TO lt_makt.
    ls_makt-matnr = 'M2'. ls_makt-maktx = 'zzzwei'. APPEND ls_makt TO lt_makt.
    ls_makt-matnr = 'M3'. ls_makt-maktx = 'dddrei'. APPEND ls_makt TO lt_makt.

    BREAK-POINT.

    LOOP AT lt_makt INTO ls_makt. "Kopiervorgang!
      IF ls_makt-matnr = 'M1'.
        ls_makt-maktx = 'eins'.  "Wirkt sich nur in der Struktur aus!
      ENDIF.
    ENDLOOP.

    LOOP AT lt_makt ASSIGNING <ls_makt>. "Kein Kopiervorgang!
      IF <ls_makt>-matnr = 'M2'.
        <ls_makt>-maktx = 'zwei'.  "Wirkt sich sofort in der internen Tabelle aus!
      ENDIF.
    ENDLOOP.

    READ TABLE lt_makt ASSIGNING <ls_makt> INDEX 3. "Kein Kopiervorgang!
    <ls_makt>-maktx = 'drei'."Wirkt sich sofort in der internen Tabelle aus!

*}Sitzung 10
  ENDMETHOD.

  METHOD ex_0010_field_sym_dynamic.
*{Sitzung 10


    "--- Statischer ASSIGN
    DATA ls_t006 TYPE t006.
    FIELD-SYMBOLS <lf_dimid> TYPE t006-dimid.
    SELECT SINGLE * FROM t006 INTO ls_t006.

    BREAK-POINT.

    ASSIGN ls_t006-dimid TO <lf_dimid>.
    WRITE <lf_dimid>.


    "--- Dynamischer ASSIGN (sy-subrc abfragen oder mit IS ASSIGNED prüfen!)

    DATA lf_fieldname TYPE string VALUE 'LS_T006-DIMID'.
    FIELD-SYMBOLS <lf_any> TYPE any.
    FIELD-SYMBOLS <lf_any2> TYPE any.

    BREAK-POINT.

    ASSIGN ('LS_T006-DIMID') TO <lf_any>.
    IF sy-subrc = 0.
      WRITE / <lf_any>.
    ENDIF.

    ASSIGN (lf_fieldname) TO <lf_any2>.
    IF <lf_any2> IS ASSIGNED.
      WRITE / <lf_any2>.
    ENDIF.


    "--- Dirty ASSIGN (Möglichst vermeiden!)

    DATA: lf_feldname2 TYPE string VALUE '(SAPLKOSM)CODIA-KOKRS'.

    BREAK-POINT.

    CALL FUNCTION 'K_ORDER_SELECTION_SET_KOKRS'
      EXPORTING
        i_kokrs = '1234'.
    "Hinweis: SAPLKOSM ist der Programmname der zugehörigen Funktionsgruppe KOSM.

    ASSIGN ('(SAPLKOSM)CODIA-KOKRS') TO <lf_any>.
    "   oder: ASSIGN (lf_feldname) TO <lf_any>.
    IF sy-subrc = 0.
      WRITE: / <lf_any>.
    ENDIF.
*}Sitzung 10
  ENDMETHOD.

  METHOD ex_0010_field_sym_all_fields.
*{Sitzung 10
    "--- Alle Felder einer Struktur abarbeiten.
    DATA ls_t006 TYPE t006.
    FIELD-SYMBOLS <lf_any> TYPE any.

    SELECT SINGLE * FROM t006 INTO ls_t006 WHERE msehi = 'ST'.

    BREAK-POINT.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_t006 TO <lf_any>.
      IF sy-subrc NE 0.
        "Es gibt kein weiteres Feld. DO-Schleife verlassen.
        EXIT.
      ENDIF.
      "Feld auswerten.
      WRITE: / 'Feld Nummer', sy-index, 'hat Inhalt', <lf_any>.
    ENDDO.
*}Sitzung 10
  ENDMETHOD.
  METHOD ex_0011_alv_reuse.
*{Sitzung 11
    IF p_liste1 = abap_true.
      DATA lt_makt TYPE STANDARD TABLE OF makt.

      SELECT * FROM makt INTO TABLE lt_makt UP TO 13 ROWS.

      BREAK-POINT.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'MAKT'
          i_grid_title     = 'Super wichtige ALV-Liste'
        TABLES
          t_outtab         = lt_makt
        EXCEPTIONS
          OTHERS           = 2.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF p_liste2 = abap_true.
      TYPES: BEGIN OF ts_vbap,
               icon     TYPE icon_d,
               vbeln    TYPE vbap-vbeln,
               posnr    TYPE vbap-posnr,
               matnr    TYPE vbap-matnr,
               done     TYPE xfeld,
               t_colors TYPE slis_t_specialcol_alv,
             END OF ts_vbap.
      DATA ls_vbap TYPE ts_vbap.
      DATA lt_vbap TYPE STANDARD TABLE OF ts_vbap.
      DATA ls_colors   TYPE slis_specialcol_alv.
      DATA ls_layout TYPE slis_layout_alv.
      DATA ls_f TYPE slis_fieldcat_alv.
      DATA lt_fieldcat TYPE slis_t_fieldcat_alv.

      BREAK-POINT.

      "--- Testdaten bereitstellen

      "-- 1. Datensatz (mit Icon)
      CLEAR ls_vbap.
      ls_vbap-vbeln = '0000000013'.
      ls_vbap-posnr = '000100'.
      ls_vbap-matnr = 'A1'.
      ls_vbap-done  = 'X'.
      ls_vbap-icon  = icon_graphics.
      APPEND ls_vbap TO lt_vbap.

      "-- 2. Datensatz (mit Farbangaben je Zellen)
      CLEAR ls_vbap.
      ls_vbap-vbeln = '0000000013'.
      ls_vbap-posnr = '000200'.
      ls_vbap-matnr = 'A2'.
      ls_vbap-done  = ' '.
      ls_vbap-icon  = icon_create_text.
*
      CLEAR ls_colors.
      ls_colors-fieldname = 'POSNR'.
      ls_colors-color-col = 1.
      ls_colors-color-int = 1.
      ls_colors-color-inv = 1.
      APPEND ls_colors TO ls_vbap-t_colors.
      ls_colors-fieldname = 'MATNR'.
      ls_colors-color-col = 3.
      APPEND ls_colors TO ls_vbap-t_colors.
      ls_colors-fieldname = 'VBELN'.
      ls_colors-color-col = 5.
      APPEND ls_colors TO ls_vbap-t_colors.
      APPEND ls_vbap TO lt_vbap.

      "-- 3. Datensatz
      CLEAR ls_vbap.
      ls_vbap-vbeln = '0000000013'.
      ls_vbap-posnr = '000300'.
      ls_vbap-matnr = 'A3'.
      ls_vbap-done  = ' '.
      CLEAR ls_colors.
      ls_colors-fieldname = 'POSNR'.
      ls_colors-color-col = 6.
      ls_colors-color-int = 1.
      ls_colors-color-inv = 1.
      APPEND ls_colors TO ls_vbap-t_colors.
      ls_colors-fieldname = 'MATNR'.
      ls_colors-color-col = 4.
      APPEND ls_colors TO ls_vbap-t_colors.
      ls_colors-fieldname = 'VBELN'.
      ls_colors-color-col = 2.
      APPEND ls_colors TO ls_vbap-t_colors.
      APPEND ls_vbap TO lt_vbap.


      "--- Feldkatalog

      BREAK-POINT.

      CLEAR ls_f.
      ls_f-fieldname     = 'VBELN'.
      ls_f-ref_tabname   = 'VBAP'.
      ls_f-ref_fieldname = ls_f-fieldname.
      APPEND ls_f TO lt_fieldcat.
      CLEAR ls_f.
      ls_f-fieldname     = 'POSNR'.
      ls_f-ref_tabname   = 'VBAP'.
      ls_f-ref_fieldname = ls_f-fieldname.
      APPEND ls_f TO lt_fieldcat.
      CLEAR ls_f.
      ls_f-fieldname     = 'MATNR'.
      ls_f-ref_tabname   = 'VBAP'.
      ls_f-ref_fieldname = ls_f-fieldname.
      ls_f-hotspot       = 'X'.
      APPEND ls_f TO lt_fieldcat.
      CLEAR ls_f.
      ls_f-fieldname     = 'DONE'.
      ls_f-datatype      = 'CHAR'.
      ls_f-outputlen     = 10.
      ls_f-checkbox      = 'X'.
      ls_f-seltext_s     = 'Erledigt'.
      APPEND ls_f TO lt_fieldcat.
      CLEAR ls_f.
      ls_f-fieldname     = 'ICON'.
      ls_f-datatype      = 'CHAR'.
      ls_f-outputlen     = 4.
      ls_f-icon          = 'X'.
      ls_f-seltext_s     = 'Bild'.
      APPEND ls_f TO lt_fieldcat.

      ls_layout-coltab_fieldname = 'T_COLORS'.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          is_layout   = ls_layout
          it_fieldcat = lt_fieldcat[]
        TABLES
          t_outtab    = lt_vbap
        EXCEPTIONS
          OTHERS      = 2.
    ENDIF.
*}Sitzung 11
  ENDMETHOD.

ENDCLASS.

*{Sitzung 9
"Implementierung der Klasse.
*----------------------------------------------------------------------*
*       CLASS lcl_fahrrad IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_fahrrad IMPLEMENTATION.
  METHOD class_constructor.
    gf_egal = sy-uname+0(2) && '_' && sy-datum+6(2).
  ENDMETHOD.
  METHOD constructor.
    me->mf_farbe = if_farbe.
    me->mf_gang = if_gang.
  ENDMETHOD.                    "constructor
  METHOD hochschalten.
    ADD 1 TO me->mf_gang.
  ENDMETHOD.                    "hochschalten
  METHOD runterschalten.
    SUBTRACT 1 FROM me->mf_gang.
  ENDMETHOD.                    "runterschalten
  METHOD daten_holen.
    ef_farbe = me->mf_farbe.
    ef_gang  = me->mf_gang.
  ENDMETHOD.                    "daten_holen
ENDCLASS.                    "lcl_fahrrad IMPLEMENTATION
*}Sitzung 9

START-OF-SELECTION.
  go_main = NEW #(  ).
  go_main->ev_start_of_selection( ).

*{Sitzung 6
FORM ex_0006_itab_headline.
  DATA lt_itab1_mk TYPE STANDARD TABLE OF makt WITH HEADER LINE. "mit Kopfzeile
  DATA lt_itab1_ok TYPE STANDARD TABLE OF makt. "ohne Kopfzeile
  DATA lt_itab2_mk TYPE STANDARD TABLE OF makt WITH HEADER LINE. "mit Kopfzeile
  DATA lt_itab2_ok TYPE STANDARD TABLE OF makt. "ohne Kopfzeile

  "Testdaten bereitstellen
  DATA ls_makt TYPE makt.
  ls_makt-spras = 'D'.
  ls_makt-matnr = 'M1'. ls_makt-maktx = 'Hammer 1'. APPEND ls_makt TO lt_itab2_ok.
  ls_makt-matnr = 'M2'. ls_makt-maktx = 'Hammer 2'. APPEND ls_makt TO lt_itab2_ok.
  ls_makt-matnr = 'M3'. ls_makt-maktx = 'Hammer 3'. APPEND ls_makt TO lt_itab2_ok.
  lt_itab2_mk[] = lt_itab2_ok[].
  lt_itab2_mk = ls_makt.

  BREAK-POINT.


  "--- Kopieren mittels ITAB1 = ITAB2.

  lt_itab1_ok   = lt_itab2_ok. "Tabelleninhalt wird kopiert.
  REFRESH lt_itab1_ok.

  lt_itab1_ok[] = lt_itab2_ok[].  "Tabelleninhalt wird kopiert.

  lt_itab1_mk   = lt_itab2_mk. "Nur der Inhalt der Kopfzeile wird kopiert.
  REFRESH lt_itab1_mk.

  lt_itab1_mk[] = lt_itab2_mk[].  "Tabelleninhalt wird kopiert.


  "---CLEAR

  CLEAR lt_itab1_ok. "Tabelleninhalt wird gelöscht.
  CLEAR lt_itab1_mk. "Nur der Inhalt der Kopfzeile wird gelöscht.
  CLEAR lt_itab2_ok[]. "Tabelleninhalt wird gelöscht.
  CLEAR lt_itab2_mk[]. "Tabelleninhalt wird gelöscht.

  BREAK-POINT.

ENDFORM.
*}Sitzung 6

*{Sitzung 6
FORM ex_0006_itab_collect.
  "---COLLECT

  TYPES: BEGIN OF ts_daten,
           kunnr  TYPE c LENGTH 10,
           bukrs  TYPE c LENGTH 4,
           belege TYPE i,
           umsatz TYPE p LENGTH 8 DECIMALS 0,
         END OF ts_daten.
  DATA ls_d TYPE ts_daten.
  DATA lt_d TYPE STANDARD TABLE OF ts_daten.

  BREAK-POINT.

  ls_d-kunnr = 'K1'. ls_d-bukrs = 'B1'. ls_d-belege = 1. ls_d-umsatz = 10.
  COLLECT ls_d INTO lt_d.
  ls_d-kunnr = 'K1'. ls_d-bukrs = 'B2'. ls_d-belege = 1. ls_d-umsatz = 10.
  COLLECT ls_d INTO lt_d.
  ls_d-kunnr = 'K1'. ls_d-bukrs = 'B1'. ls_d-belege = 1. ls_d-umsatz = 10.
  COLLECT ls_d INTO lt_d.
  ls_d-kunnr = 'K2'. ls_d-bukrs = 'B1'. ls_d-belege = 1. ls_d-umsatz = 10.
  COLLECT ls_d INTO lt_d.

  BREAK-POINT.
ENDFORM.
*}Sitzung 6


*{Sitzung 8
*&---------------------------------------------------------------------*
*&      Form  daten_ausgeben1
*&---------------------------------------------------------------------*
*       Typisierte Parameter
*----------------------------------------------------------------------*
*      -->UF_MATNR   text
*      -->US_MAKT    text
*      -->UT_MAKT    text
*----------------------------------------------------------------------*
FORM daten_ausgeben1
USING uf_matnr TYPE makt-matnr
      us_makt  TYPE makt
      ut_makt  TYPE tt_makt.
  DATA ls_makt TYPE makt.

  WRITE / 'Form daten_ausgeben1:'.
  WRITE / uf_matnr.

  WRITE / us_makt-matnr.

  LOOP AT ut_makt INTO ls_makt.
    WRITE: / ls_makt-matnr.
  ENDLOOP.

  PERFORM daten_ausgeben3
  USING us_makt.
ENDFORM.                    "daten_ausgeben1

*&---------------------------------------------------------------------*
*&      Form  daten_ausgeben2
*&---------------------------------------------------------------------*
*       Untypisierte Parameter
*----------------------------------------------------------------------*
*      -->UF_MATNR   text
*      -->US_MAKT    text
*      -->UT_MAKT    text
*----------------------------------------------------------------------*
FORM daten_ausgeben2
USING uf_matnr
      us_makt
      ut_makt.
  DATA ls_makt TYPE makt.

  WRITE / 'Form daten_ausgeben2:'.
  WRITE / uf_matnr. "Kein Problem, aber fehleranfällig

*  WRITE / us_makt-matnr.  "Syntaxfehler

*  LOOP AT ut_makt INTO ls_makt.  "Syntaxfehler
*    WRITE: / ls_makt-matnr.
*  ENDLOOP.
ENDFORM.                    "daten_ausgeben
*&---------------------------------------------------------------------*
*&      Form  DATEN_AUSGEBEN3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM daten_ausgeben3
USING us_maaakt TYPE makt.
  WRITE / 'Form daten_ausgeben3:'.

  "WRITE / uf_matnr. "Feld aus Form daten_ausgeben1 hier nicht bekannt.
  "WRITE / us_makt-matnr. "Feld aus Form daten_ausgeben1 hier nicht bekannt.

  WRITE / us_maaakt-matnr. "Feld aus Form daten_ausgeben1 über Parameter US_MAAAKT bekannt!

ENDFORM.                    " DATEN_AUSGEBEN3
*}Sitzung 8
