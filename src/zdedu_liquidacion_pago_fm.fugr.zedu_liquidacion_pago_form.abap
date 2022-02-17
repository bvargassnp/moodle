FUNCTION ZEDU_LIQUIDACION_PAGO_FORM.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_OPBEL) TYPE  OPBEL_KK OPTIONAL
*"     REFERENCE(I_NR_FORMULARIO) TYPE  ZPRE_ADMI_1-NR_FORMULARIO
*"       OPTIONAL
*"     REFERENCE(I_ID_LIQUIDACION) TYPE  ZID_LIQUIDACION OPTIONAL
*"     REFERENCE(I_FECHAS) TYPE  ZEDU_S_LIQUIDACION_PAGO_FECHAS
*"       OPTIONAL
*"     REFERENCE(I_EMAIL) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_OTF) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(I_DOCS_PAGADOS) TYPE  CHECK OPTIONAL
*"     REFERENCE(I_NO_COMMIT) TYPE  CHECK OPTIONAL
*"     REFERENCE(I_TIMELIMIT) TYPE  PIQTIMELIMIT OPTIONAL
*"     REFERENCE(I_POS_PLAN_PAGOS) TYPE  OPUPW_KK OPTIONAL
*"     REFERENCE(I_MOSTRAR_INTER) TYPE  CHECK OPTIONAL
*"  EXPORTING
*"     REFERENCE(T_RETURN) TYPE  WRF_BAPIRETURN_TTY
*"     REFERENCE(T_OTF) TYPE  TSFOTF
*"  CHANGING
*"     REFERENCE(CT_DOCUMENTOS) TYPE  FKKINV_OPBEL_TAB OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_valor_cero TYPE check, "Indicador de No Pago
        lv_type       TYPE bapireturn-type,
        lv_message    TYPE bapireturn-message,
        lv_msgv1      TYPE msgv1,
        lv_msgv2      TYPE msgv2,
        lv_plan_pagos TYPE check.


  PERFORM f_validar_entrada
  USING i_opbel
        i_nr_formulario
        i_id_liquidacion
        i_fechas
        i_mostrar_inter
  CHANGING t_return.

  CHECK t_return IS INITIAL.

  PERFORM f_obtener_datos
  USING i_opbel
        i_nr_formulario
        i_id_liquidacion
        i_fechas
        i_docs_pagados
        i_no_commit
        i_timelimit
        i_pos_plan_pagos
        i_mostrar_inter
  CHANGING t_return
           ct_documentos
           lv_valor_cero
           lv_plan_pagos. "-->  MgM DCEK901536

  CHECK t_return IS INITIAL.


* Inicio agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015
  PERFORM f_agrupar_conceptos USING i_id_liquidacion.
  PERFORM f_mapear_nombres_conceptos USING i_id_liquidacion.
  PERFORM f_mostrar_conceptos_pagados USING i_id_liquidacion ct_documentos.
* Fin agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015


  "No imprimir formulario si no hay valor por pagar y lv_valor_cero es inicial (centro de idiomas)
  IF ( gs_header-ord_value <> 0 OR lv_valor_cero = 'X' ) OR ( gs_header-ord_value = 0 AND i_docs_pagados = 'X' ).
    PERFORM f_imprimir_formulario
      USING i_email
            i_otf
            i_nr_formulario  "-->  DGAGLIARDI DCEK901536
            i_id_liquidacion
            lv_valor_cero    "-->  DGAGLIARDI DCEK901748
            i_pos_plan_pagos
            lv_plan_pagos
    CHANGING t_otf
             t_return. "-->  DGAGLIARDI DCEK901536
  ELSE.
    "Log mensaje de valor a pagar en cero
    IF gs_header-ord_value <= 0.
      lv_type = 'W'.
      lv_msgv1 = gs_header-xblnr.
      lv_msgv2 = gs_header-objid_st.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZDEDU_FACTURACION'
          msgnr               = '105'
          msgv1               = lv_msgv1
          msgv2               = lv_msgv2
        IMPORTING
          message_text_output = lv_message.

      PERFORM f_cargar_error USING lv_type
                                   lv_message
                          CHANGING t_return.
      IF 1 = 2. MESSAGE w105(zdedu_facturacion). ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
