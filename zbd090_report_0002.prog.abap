REPORT zcsu.

*{Sitzung 2
PARAMETERS p_bukrs TYPE bukrs.
PARAMETERS p_budat TYPE budat.
DATA gf_help_i TYPE i.
SELECT-OPTIONS s_i FOR gf_help_i. "Number
SELECTION-SCREEN SKIP 1.
PARAMETERS p_rb_si  RADIOBUTTON GROUP tas."Send info messages
PARAMETERS p_rb_se1 RADIOBUTTON GROUP tas."Send error:START-OF-SELECTION
PARAMETERS p_rb_se2 RADIOBUTTON GROUP tas."Send error:AT SELECTION-SCREEN
PARAMETERS p_rb_als RADIOBUTTON GROUP tas."Verzweigungsliste
SELECTION-SCREEN COMMENT /1(30) c01.
SELECTION-SCREEN COMMENT /1(30) c02.

*Selektionstexte:
*P_BUDAT  Buchungsdatum
*P_BUKRS  Buchungskreis
*P_RB_ALS	Verzweigungsliste ausgeben
*P_RB_SE1	Fehler bei START-OF-SELECTION
*P_RB_SE2	Fehler bei AT SELECTION-SCREEN
*P_RB_SI  Informationsmeldungen ausgeben
*S_I  Kennzahlen

INITIALIZATION.
  c01 = 'P_BUDAT: <F1>'.
  c02 = 'P_BUDAT: <F4>'.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'INITIALIZATION'.
  ENDIF.

  p_bukrs = '1000'.
  p_budat = sy-datum + 2.

  CLEAR: s_i, s_i[].

  s_i-sign   = 'E'.
  s_i-option = 'EQ'.
  s_i-low    = 15.
  APPEND s_i TO s_i.
  s_i-sign   = 'I'.
  s_i-option = 'BT'.
  s_i-low    = 13.
  s_i-high   = 40.
  APPEND s_i TO s_i.


START-OF-SELECTION.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'START-OF-SELECTION'.
  ENDIF.
  IF p_rb_se1 = abap_true.
    MESSAGE e208(00) WITH 'START-OF-SELECTION'.
  ENDIF.

  IF p_rb_als = abap_true.

    DATA ls_t006 TYPE t006.
    DATA lt_t006 TYPE STANDARD TABLE OF t006.

    BREAK-POINT.

    SELECT msehi dimid isocode FROM t006
      INTO CORRESPONDING FIELDS OF TABLE lt_t006 UP TO 13 ROWS.

    LOOP AT lt_t006 INTO ls_t006.
      WRITE: / ls_t006-msehi, ls_t006-dimid, ls_t006-isocode.
      HIDE: ls_t006-msehi, ls_t006-dimid.
*      HIDE ls_t006-isocode.
    ENDLOOP.

  ELSE.

    WRITE: / 'Ereignis START-OF-SELECTION.'.
    WRITE: / 'Buchungskreis', p_bukrs.
    WRITE: / 'Buchungsdatum', p_budat.

  ENDIF.


END-OF-SELECTION.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'END-OF-SELECTION'.
  ENDIF.

  WRITE: / 'Ereignis END-OF-SELECTION.'.


AT SELECTION-SCREEN OUTPUT.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'AT SELECTION-SCREEN OUTPUT'.
  ENDIF.


AT SELECTION-SCREEN.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'AT SELECTION-SCREEN'.
  ENDIF.
  IF p_rb_se2 = abap_true.
    MESSAGE e208(00) WITH 'AT SELECTION-SCREEN'.
  ENDIF.


AT LINE-SELECTION.

  IF p_rb_si = abap_true.
    MESSAGE i208(00) WITH 'AT LINE-SELECTION'.
  ENDIF.

  IF p_rb_als = abap_true.
    BREAK-POINT.

    WRITE: /3 ls_t006-msehi COLOR COL_POSITIVE, "Wert der gewählten Zeile
              ls_t006-dimid COLOR COL_POSITIVE, "Wert der gewählten Zeile
              ls_t006-isocode COLOR COL_NEGATIVE. "Aktueller Wert der Variable

    DATA lf_lisel TYPE string.
    lf_lisel = sy-lisel.

    WRITE: /5 'SY-LSIND =', sy-lsind,
           /5 'SY-LISEL =', lf_lisel,
           /5 'SY-CUCOL =', sy-cucol,
           /5 'SY-CUROW =', sy-curow.
  ENDIF.


AT SELECTION-SCREEN ON HELP-REQUEST FOR p_budat.

  MESSAGE i208(00) WITH 'I thing, the posting date is realy important.'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_budat.

  MESSAGE i208(00) WITH 'My own F4 help'.
*}Sitzung 2
