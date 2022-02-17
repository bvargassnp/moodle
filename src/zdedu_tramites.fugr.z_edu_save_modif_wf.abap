FUNCTION z_edu_save_modif_wf.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_BP) TYPE  BU_PARTNER
*"     REFERENCE(IV_ESTADO) TYPE  CHAR1
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------

  DATA: ls_return TYPE bapiret2.

  CASE iv_estado.
    WHEN 'A'. " si se aprueba

      SELECT SINGLE *
        FROM zedu_datos_pers
        INTO CORRESPONDING FIELDS OF gs_datos_personales
        WHERE partner = iv_bp.

      SELECT SINGLE *
        FROM zedu_datos_dir
        INTO CORRESPONDING FIELDS OF gs_datos_direccion
        WHERE partner = iv_bp.

      SELECT *
        FROM zedu_datos_banco
        INTO CORRESPONDING FIELDS OF TABLE gt_datos_bancarios
        WHERE partner = iv_bp.

      CALL FUNCTION 'Z_EDU_SAVE_MODIF'
        EXPORTING
          iv_no_wf  = 'X' " Sin llamar al Workflow
        TABLES
          et_return = et_return.

*      READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
*      IF sy-subrc IS NOT INITIAL.
"Sin importar si la actualizacion fue exitosa o no se borran los datos temporales, ya que si es erroneo
"se debe solucionar el error y volver a empezar el proceso
        DELETE FROM zedu_datos_pers  WHERE partner = iv_bp.
        DELETE FROM zedu_datos_dir   WHERE partner = iv_bp.
        DELETE FROM zedu_datos_banco WHERE partner = iv_bp.
*      ENDIF.

    WHEN 'R'. " si se rechaza.

      DELETE FROM zedu_datos_pers  WHERE partner = iv_bp.
      DELETE FROM zedu_datos_dir   WHERE partner = iv_bp.
      DELETE FROM zedu_datos_banco WHERE partner = iv_bp.

  ENDCASE.

ENDFUNCTION.
