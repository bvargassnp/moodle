FUNCTION z_edu_modif_rol .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_TIPO_ROL) TYPE  CHAR1
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
*  FLCU00 Deudor
*  FLCU01 Cliente
*  FLVN00 Acreedor
*  FLVN01 Proveedor

  DATA: ls_return  TYPE bapiret2,
        lv_message TYPE string,
        lv_error.

  CASE i_tipo_rol.
    WHEN 'C'. " Cliente / Deudor

      PERFORM f_modif_cliente USING i_partner
                              CHANGING lv_error.

      CALL FUNCTION 'Z_EDU_REFRESH_DEUDOR'.

    WHEN 'P'. " Proveedor / Acreedor

      PERFORM f_modif_proveedor USING i_partner
                                      i_bukrs
                                CHANGING lv_error.

      CALL FUNCTION 'Z_EDU_REFRESH_ACREEDOR'.

  ENDCASE.

  IF lv_error IS INITIAL.
*   El BP ha sido modificado.
    MESSAGE s009(zedu_wd_message) INTO lv_message.
  ELSE.
*   Error al modificar el BP.
    MESSAGE e024(zedu_wd_message) INTO lv_message.
  ENDIF.

  MOVE sy-msgid TO ls_return-id.
  MOVE sy-msgno TO ls_return-number.
  MOVE sy-msgty TO ls_return-type.

  APPEND ls_return TO et_return.

ENDFUNCTION.
