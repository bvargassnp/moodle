*----------------------------------------------------------------------*
***INCLUDE LZDEDU_ACTUALIZA_IT_PAGOSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SET_DATA_9114
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_data_9114 TABLES fetch_tb_tab
                      USING ft_set
                            ft_tabix.

  FIELD-SYMBOLS <tab>  TYPE c.
  FIELD-SYMBOLS <text>  TYPE c.

  APPEND INITIAL LINE TO fetch_tb_tab ASSIGNING <tab> CASTING.
  ASSIGN gs_9114 TO <text> CASTING.
  <tab> = <text>.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SET_DATA_9119
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_data_9119 TABLES fetch_tb_tab
                      USING ft_set
                            ft_tabix.

  FIELD-SYMBOLS <tab>  TYPE c.
  FIELD-SYMBOLS <text>  TYPE c.

  APPEND INITIAL LINE TO fetch_tb_tab ASSIGNING <tab> CASTING.
  ASSIGN gs_9119 TO <text> CASTING.
  <tab> = <text>.

ENDFORM.
FORM f_set_data_xxxx TABLES fetch_tb_tab
                      USING ft_set
                            ft_tabix.

  FIELD-SYMBOLS <tab>  TYPE c.

  APPEND INITIAL LINE TO fetch_tb_tab ASSIGNING <tab> CASTING.
  <tab> = <text>.

ENDFORM.
