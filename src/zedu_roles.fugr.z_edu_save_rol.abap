FUNCTION z_edu_save_rol .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(I_BUKRS) TYPE  BUKRS OPTIONAL
*"     REFERENCE(I_TIPO_ROL) TYPE  CHAR1
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
  DATA:
    ls_acreedor_tmp TYPE zedu_s_acreedor_cvi,
    ls_deudor_tmp   TYPE zedu_s_deudor_cvi.

  IF gs_deudor-tipo_tramite = 'C'
  OR gs_acreedor-tipo_tramite = 'C'. " crear

    "Crea una copia de los datos
    ls_acreedor_tmp = gs_acreedor.
    ls_deudor_tmp   = gs_deudor.

    CALL FUNCTION 'Z_EDU_CREATE_ROL'
      EXPORTING
        i_partner  = i_partner
        i_tipo_rol = i_tipo_rol
      TABLES
        et_return  = et_return.

    "Seteo nuevamente los datos en memoria.
    CALL FUNCTION 'Z_EDU_SET_ACREEDOR'
      EXPORTING
        is_acreedor = ls_acreedor_tmp.
    CALL FUNCTION 'Z_EDU_SET_DEUDOR'
      EXPORTING
        is_deudor = ls_deudor_tmp.

    "Ingresa los datos adicionales por medio de modificacion
    CALL FUNCTION 'Z_EDU_MODIF_ROL'
      EXPORTING
        i_partner  = i_partner
        i_bukrs    = i_bukrs
        i_tipo_rol = i_tipo_rol
      TABLES
        et_return  = et_return.

    "Inicializa nuevamente las estructuras
    CALL FUNCTION 'Z_EDU_REFRESH_ACREEDOR'.
    CALL FUNCTION 'Z_EDU_REFRESH_DEUDOR'.

  ELSEIF gs_deudor-tipo_tramite = 'M'
      OR gs_acreedor-tipo_tramite = 'M'. " modificar

    CALL FUNCTION 'Z_EDU_MODIF_ROL'
      EXPORTING
        i_partner  = i_partner
        i_bukrs    = i_bukrs
        i_tipo_rol = i_tipo_rol
      TABLES
        et_return  = et_return.

  ENDIF.


ENDFUNCTION.
