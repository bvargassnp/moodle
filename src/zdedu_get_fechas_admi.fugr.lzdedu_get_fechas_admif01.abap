*----------------------------------------------------------------------*
***INCLUDE LZDEDU_GET_FECHAS_ADMIF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETAR_AUXILIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIOS  text
*      <--P_P_LT_ESTUDIOS_AUX  text
*----------------------------------------------------------------------*
FORM f_completar_auxiliar  USING    p_lt_original TYPE tyt_objid
                           CHANGING p_lt_auxiliar TYPE tyt_objid_aux.

  DATA: ls_original TYPE t_objid,
        ls_auxiliar TYPE t_objid_aux.

  REFRESH p_lt_auxiliar.

  LOOP AT p_lt_original INTO ls_original.
    ls_auxiliar-objid = ls_original-objid.
    ls_auxiliar-sobid = ls_original-sobid.

    APPEND ls_auxiliar TO
    p_lt_auxiliar.
    CLEAR ls_auxiliar.
  ENDLOOP.

ENDFORM.
