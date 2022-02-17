* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9119                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9119 OUTPUT.
* replace with infotype specific coding
ENDMODULE.                    "INIT_9117 OUTPUT

* INPUT modules
*&---------------------------------------------------------------------*
*&      Module  FILL_PT9119  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fill_pt9119 OUTPUT.

  " Si se cambia de registro, se consulta nuevamente
  IF p9119-tabnr NE gs_hrt9119-tabnr.
    SELECT SINGLE *
      FROM hrt9119
      INTO gs_hrt9119
      WHERE tabnr EQ p9119-tabnr.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING gs_hrt9119 TO pt9119.
    ENDIF.

  ELSE.
    " Si no se cambia de registro, no se consulta nuevamente para no pisar los valores
    " modificados por el usuario
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_DROPDOWN_ENTIDAD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_dropdown_entidad OUTPUT.
  DATA: lv_param  TYPE vrm_id,
        lt_values TYPE vrm_values.

  lv_param = 'PT9119-ENTIDAD'.

  SELECT cod_ent     AS key
         descripcion as text
    FROM zedu_entidadt
    INTO TABLE lt_values
  WHERE spras eq sy-langu.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_param
      values = lt_values.


ENDMODULE.
