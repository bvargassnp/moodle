*----------------------------------------------------------------------*
***INCLUDE LZDEDU_CERTIFICADOSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_QUITAR_DECIMALES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_NOTECRED  text
*----------------------------------------------------------------------*
FORM f_quitar_decimales  USING p_entero
                         CHANGING ch_corto.

  DATA: lv_char(16).

  lv_char = p_entero.

  CALL FUNCTION 'STRING_REVERSE'
    EXPORTING
      string    = lv_char
      lang      = sy-langu
    IMPORTING
      rstring   = lv_char
    EXCEPTIONS
      too_small = 1
      OTHERS    = 2.


  SHIFT lv_char BY 3 PLACES .

  CALL FUNCTION 'STRING_REVERSE'
    EXPORTING
      string    = lv_char
      lang      = sy-langu
    IMPORTING
      rstring   = lv_char
    EXCEPTIONS
      too_small = 1
      OTHERS    = 2.

  ch_corto = lv_char.

ENDFORM.
