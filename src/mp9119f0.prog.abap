*----------------------------------------------------------------------*
***INCLUDE MP9119F0.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GRABAR_INFOTIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P9119  text
*      -->P_PT9119  text
*----------------------------------------------------------------------*
FORM f_grabar_infotipo  USING ls_p9119 TYPE p9119
                              ls_pt9119 TYPE pt9119.
  DATA: lc_9119 TYPE i VALUE 9119,
        ls_key  TYPE HRIPKEY.

  MOVE-CORRESPONDING p9119 to ls_key.

  CALL FUNCTION 'ZMF_ACTUALIZAR_ITXXXX'
    EXPORTING
      is_xxxx    = pt9119
      i_objid    = ls_p9119-objid
      i_infotype = lc_9119
      is_key     = ls_key
      i_otype    = ls_p9119-otype
    EXCEPTIONS
      error      = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.
