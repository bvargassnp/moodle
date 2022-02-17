FUNCTION Z_EDU_FICA_CTA_X_COBRAR.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_NRO_FORMULARIO) TYPE  CHAR10
*"  EXPORTING
*"     REFERENCE(EV_OPBEL) TYPE  OPBEL_KK
*"     REFERENCE(ES_RETURN) TYPE  BAPIRET2
*"  EXCEPTIONS
*"      DOCUMENTO_YA_CONTABILIZADO
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_dfkkop,
           opbel TYPE dfkkop-opbel,
           xblnr TYPE dfkkop-xblnr,
         END OF ty_dfkkop.

  DATA: lt_dfkkop         TYPE TABLE OF ty_dfkkop,
        ls_dfkkop         TYPE ty_dfkkop,
        lt_param          TYPE TABLE OF zedu_c_param,
        ls_param          TYPE zedu_c_param,
        ls_fkkko          TYPE bapidfkkko,
        lt_fkkop          TYPE TABLE OF bapidfkkop,
        ls_fkkop          TYPE bapidfkkop,
        lv_fech_registr   TYPE zpre_admi_1-fech_registr,
        lv_valor_pagado   TYPE zpre_admi_2-valor_pagado,
        lv_programa_1     TYPE zpre_admi_2-programa_1,
        lv_bukrs          TYPE dfkkop-bukrs,
        lv_gpart          TYPE dfkkop-gpart,
        lv_tabnr          TYPE hrp1759-tabnr,
        lv_profit_ctr     TYPE hrt1759-profit_ctr,
        lv_segmento       TYPE hrt1759-segment,
        lt_fbstab         TYPE STANDARD TABLE OF tfkfbc,
        ls_fbstab         TYPE tfkfbc,
        ls_fkkop_1101     TYPE fkkop,
        lv_hkont          TYPE fkkop-hkont,
        lv_xblnr          TYPE xblnr_kk,
        lv_crear_doc_fica TYPE check,
        ls_pre_admi_2     TYPE zpre_admi_2,
*-- Inicio Modificación Adepcon 27.03.2017 -------------------------------------------
* Contabilización de documentos de preinscripción en BP genérico diferenciando a nivel
* de contabilización los conceptos de descuentos/becas
        ls_fkkop_desc     TYPE bapidfkkop,
        lv_descuento_beca TYPE zpre_admi_1-descuento_beca,
        lv_condicion_beca TYPE zpre_admi_1-condicion_beca,
        lv_valor_apagar   TYPE zpre_admi_1-valor_apagar,
        ls_c_desc_oper    TYPE zedu_c_desc_oper,
        lt_objects        TYPE STANDARD TABLE OF hrobject,
        ls_objects        TYPE hrobject,
        lt_infty_1222     TYPE STANDARD TABLE OF p1222,
        lv_low            TYPE hrt1222-low,
        lv_tipo_estudio   TYPE zedu_c_tipo_estu.

  STATICS: st_c_desc_oper TYPE STANDARD TABLE OF zedu_c_desc_oper.
*-- Fin Modificación Adepcon 27.03.2017 ----------------------------------------------

  CLEAR: lv_gpart, lv_bukrs.
*Validar que no haya un documento FICA ya contabilizado con este número de formulario:
*Se requiere, con el número de formulario realizar la búsqueda en la tabla DFKKOP, cuando:

  SELECT *
    FROM zedu_c_param
    INTO TABLE lt_param
    WHERE repid = sy-repid.

  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'BUSPARTNER' BINARY SEARCH.
  lv_gpart = ls_param-valor.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'COMP_CODE' BINARY SEARCH.
  lv_bukrs = ls_param-valor.

  "M001 inicio pjv 16.09.2016
  IF lv_gpart IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_gpart
      IMPORTING
        output = lv_gpart.
  ENDIF.
  "M001 fin pjv 16.09.2016

  SELECT opbel xblnr
    FROM dfkkop
    INTO TABLE lt_dfkkop
    WHERE gpart = lv_gpart
      AND augst = space
      AND bukrs = lv_bukrs
      AND xmanl = space.

  lv_xblnr = iv_nro_formulario.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_xblnr
    IMPORTING
      output = lv_xblnr.

  DELETE lt_dfkkop WHERE xblnr NE lv_xblnr.
  READ TABLE lt_dfkkop INTO ls_dfkkop INDEX 1.
  ev_opbel = ls_dfkkop-opbel.

  IF ev_opbel IS NOT INITIAL.
    RAISE documento_ya_contabilizado.
  ENDIF.

*-- Inicio Modificación Adepcon 27.03.2017 -------------------------------------------
* Contabilización de documentos de preinscripción en BP genérico diferenciando a nivel
* de contabilización los conceptos de descuentos/becas
*   SELECT SINGLE fech_registr
*   FROM zpre_admi_1
*   INTO lv_fech_registr
  SELECT SINGLE fech_registr descuento_beca condicion_beca valor_apagar
    INTO (lv_fech_registr, lv_descuento_beca, lv_condicion_beca, lv_valor_apagar)
*-- Fin Modificación Adepcon 27.03.2017 ----------------------------------------------
    FROM zpre_admi_1
    WHERE nr_formulario = iv_nro_formulario.

*  SELECT SINGLE valor_pagado programa_1
*    FROM zpre_admi_2
*    INTO (lv_valor_pagado, lv_programa_1)
*    WHERE nr_formulario = iv_nro_formulario.

  SELECT SINGLE *
    FROM zpre_admi_2
    INTO ls_pre_admi_2
    WHERE nr_formulario = iv_nro_formulario.

  lv_valor_pagado = ls_pre_admi_2-valor_pagado.
  lv_programa_1 = ls_pre_admi_2-programa_1.


  SELECT SINGLE tabnr
    FROM hrp1759
    INTO lv_tabnr
    WHERE plvar = '01'
      AND otype = 'SC'
      AND objid = lv_programa_1.

  SELECT SINGLE profit_ctr segment
    FROM hrt1759
    INTO (lv_profit_ctr,lv_segmento)
    WHERE tabnr = lv_tabnr.

*** Cabecera:
*  CONCATENATE 'INS' sy-datum+2(6) sy-uzeit INTO ls_fkkko-fikey.
  CONCATENATE sy-datum+2(6) sy-uzeit INTO ls_fkkko-fikey. " AAMMDDHHMMSS
  CLEAR ls_param.
  ls_fkkko-doc_source_key = '01'.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'APPL_AREA' BINARY SEARCH.
  ls_fkkko-appl_area    = ls_param-valor.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'DOC_TYPE' BINARY SEARCH.
  ls_fkkko-doc_type     = ls_param-valor.
  ls_fkkko-doc_date     = lv_fech_registr.
  ls_fkkko-post_date    = sy-datum.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'CURRENCY' BINARY SEARCH.
  ls_fkkko-currency     = ls_param-valor.
  ls_fkkko-currency_iso = ls_fkkko-currency.


  ls_fkkko-ref_doc_no   = iv_nro_formulario.

***  Posiciones de bp
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'REP_ITEM' BINARY SEARCH.
  "ls_fkkop-rep_item     = ls_param-valor.
  ls_fkkop-item      = '0001'.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'COMP_CODE' BINARY SEARCH.
  ls_fkkop-comp_code    = ls_param-valor.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_profit_ctr
    IMPORTING
      output = ls_fkkop-profit_ctr.

  ls_fkkop-actdeterid = 'A8'.
  ls_fkkop-segment      = lv_segmento.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'BUSPARTNER' BINARY SEARCH.
  ls_fkkop-buspartner   = ls_param-valor.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ls_fkkop-buspartner
    IMPORTING
      output = ls_fkkop-buspartner.


  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'CONT_ACCT' BINARY SEARCH.
  ls_fkkop-cont_acct    = ls_param-valor.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ls_fkkop-cont_acct
    IMPORTING
      output = ls_fkkop-cont_acct.

  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'CONTRACT' BINARY SEARCH.
  ls_fkkop-contract     = ls_param-valor.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ls_fkkop-contract
    IMPORTING
      output = ls_fkkop-contract.


  ls_fkkop-appl_area    = ls_fkkko-appl_area.
*-- Inicio Modificación Adepcon 28.03.2017 -------------------------------------------
* Las operaciones principales y parciales de inscripció son diferentes en Centro de Idioma
* que en formal. En base al campo prueba_i se determina, si es X es CI sino formal.
* En la tabla de parametros generales se configuran dos combinaciones diferentes de operaciones,
* para centro de idiomas se usa MAIN_TRACI y SUB_TRACI y para los demas MAIN_TRANS y SUB_TRANS.
*  CLEAR ls_param.
*  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'MAIN_TRANS' BINARY SEARCH.
*  ls_fkkop-main_trans   = ls_param-valor.
*  CLEAR ls_param.
*  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'SUB_TRANS' BINARY SEARCH.
*  ls_fkkop-sub_trans    = ls_param-valor.

  CASE ls_pre_admi_2-prueba_i.
    WHEN abap_true.
      CLEAR ls_param.
      READ TABLE lt_param INTO ls_param WITH KEY idparam = 'MAIN_TRACI' BINARY SEARCH.
      ls_fkkop-main_trans   = ls_param-valor.
      CLEAR ls_param.
      READ TABLE lt_param INTO ls_param WITH KEY idparam = 'SUB_TRACI' BINARY SEARCH.
      ls_fkkop-sub_trans    = ls_param-valor.
    WHEN OTHERS.
*-- Inicio Modificación Adepcon 04.05.2017 -------------------------------------------
      "Determinación de Ooperación Principal y Parcial a partir del Programa
      ls_objects-plvar = '01'.
      ls_objects-otype = cl_hrpiq00const=>c_otype_o.
      ls_objects-objid =  ls_pre_admi_2-t_posgrado.
      APPEND ls_objects TO lt_objects.

      CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
        EXPORTING
          infty                 = '1222'
        TABLES
          innnn                 = lt_infty_1222
          objects               = lt_objects
        EXCEPTIONS
          nothing_found         = 1
          wrong_condition       = 2
          infotyp_not_supported = 3
          OTHERS                = 4.

      READ TABLE lt_infty_1222 INTO DATA(ls_infty_1222) INDEX 1.
      SELECT SINGLE low
        INTO lv_low
        FROM hrt1222
        WHERE tabnr = ls_infty_1222-tabnr.

      lv_tipo_estudio = lv_low.
      SELECT SINGLE hvorg tvorg
        FROM zedu_c_preins_op
        INTO (ls_fkkop-main_trans,ls_fkkop-sub_trans)
        WHERE tipo_estudio = lv_tipo_estudio.

      IF ls_fkkop-main_trans IS INITIAL OR ls_fkkop-sub_trans IS INITIAL.
        CLEAR ls_param.
        READ TABLE lt_param INTO ls_param WITH KEY idparam = 'MAIN_TRANS' BINARY SEARCH.
        ls_fkkop-main_trans   = ls_param-valor.
        CLEAR ls_param.
        READ TABLE lt_param INTO ls_param WITH KEY idparam = 'SUB_TRANS' BINARY SEARCH.
        ls_fkkop-sub_trans    = ls_param-valor.
      ENDIF.
*-- Fin Modificación Adepcon 04.05.2017 -------------------------------------------
  ENDCASE.
*-- Fin Modificación Adepcon 28.03.2017 ----------------------------------------------

  ls_fkkop-doc_type     = ls_fkkko-doc_type.
  ls_fkkop-post_date    = ls_fkkko-post_date.
  ls_fkkop-doc_date     = lv_fech_registr.
  ls_fkkop-net_date     = lv_fech_registr.
  ls_fkkop-currency     = ls_fkkko-currency.
  ls_fkkop-currency_iso = ls_fkkko-currency_iso.
  CLEAR ls_param.
  READ TABLE lt_param INTO ls_param WITH KEY idparam = 'TEXT' BINARY SEARCH.
  ls_fkkop-text         = ls_param-valor.  "
  "ls_fkkop-period_key   = sy-datum+2(4).  "  tomar dato de period actual.
  ls_fkkop-amount       = lv_valor_pagado.
  ls_fkkop-stat_key = 'G'.
  "Si valor a pagar es cero, no crear documento contable y actualizar estado a pagado
  lv_crear_doc_fica = 'X'.

*-- Inicio Modificación Adepcon 28.03.2017 -------------------------------------------
** Cuando el descuento es del 100% se debe contabilizar igualmente el documento y
** generar un recibo. Esto aplica en Centro de Idiomas (CI) para el B019
*  IF ls_fkkop-amount = 0.
  IF ls_fkkop-amount = 0 AND lv_condicion_beca NE 'B019'.
*-- Fin Modificación Adepcon 28.03.2017 ----------------------------------------------

    CLEAR lv_crear_doc_fica.
    ls_pre_admi_2-fecha_pago = sy-datum.
    ls_pre_admi_2-hora_pago = sy-uzeit.
    ls_pre_admi_2-sta2_solpago = 'P'.
    MODIFY zpre_admi_2 FROM ls_pre_admi_2.
    es_return-id = 'ZDEDU_FACTURACION'.
    es_return-number = '061'.
    es_return-type = 'S'.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = es_return-id
        msgnr               = '061'
      IMPORTING
        message_text_output = es_return-message.
    IF 1 = 2. MESSAGE s061(zdedu_facturacion). ENDIF.
  ENDIF.


  "Determinar Cuenta de la posición de Partner
  IF lv_crear_doc_fica = 'X'.
    CALL FUNCTION 'FKK_FUNC_MODULE_DETERMINE'
      EXPORTING
        i_fbeve  = '1101'
      TABLES
        t_fbstab = lt_fbstab.


    ls_fkkop_1101-bukrs = ls_fkkop-comp_code.
    ls_fkkop_1101-hvorg = ls_fkkop-main_trans.
    ls_fkkop_1101-tvorg = ls_fkkop-sub_trans.
    ls_fkkop_1101-applk = ls_fkkko-appl_area.
    ls_fkkop_1101-kofiz = 'A8'.

    LOOP AT lt_fbstab INTO ls_fbstab.
      CALL FUNCTION ls_fbstab-funcc
        EXPORTING
          i_fkkop             = ls_fkkop_1101
        IMPORTING
          e_hkont             = lv_hkont
        EXCEPTIONS
          error_in_input_data = 1
          OTHERS              = 2.

      ls_fkkop-g_l_acct = lv_hkont.
    ENDLOOP.

*-- Inicio Modificación Adepcon 27.03.2017 -------------------------------------------
* Contabilización de documentos de preinscripción en BP genérico diferenciando a nivel
* de contabilización los conceptos de descuentos/becas
    IF lv_descuento_beca IS NOT INITIAL.

      IF st_c_desc_oper[] IS INITIAL.
        SELECT * INTO TABLE st_c_desc_oper FROM zedu_c_desc_oper.
      ENDIF.

      READ TABLE st_c_desc_oper WITH KEY kschl = lv_condicion_beca
                                         kofiz = ls_fkkop_1101-kofiz
            INTO ls_c_desc_oper.

      IF sy-subrc = 0.
**    La línea es por el total, sin considerar el descuento/beca.
        ls_fkkop-amount = lv_valor_apagar.

**    El monto de descuento/beca se contabiliza en una posición diferente a una cuenta contable diferente
        ls_fkkop_desc = ls_fkkop.
        ls_fkkop_desc-amount = lv_descuento_beca * -1.

        ls_fkkop_1101-bukrs = ls_fkkop_desc-comp_code.
        ls_fkkop_1101-applk = ls_fkkop_desc-appl_area.
        ls_fkkop_1101-kofiz = 'A8'.
        ls_fkkop_1101-hvorg = ls_fkkop_desc-main_trans = ls_c_desc_oper-hvorg.
        ls_fkkop_1101-tvorg = ls_fkkop_desc-sub_trans = ls_c_desc_oper-tvorg.

        LOOP AT lt_fbstab INTO ls_fbstab.
          CALL FUNCTION ls_fbstab-funcc
            EXPORTING
              i_fkkop             = ls_fkkop_1101
            IMPORTING
              e_hkont             = lv_hkont
            EXCEPTIONS
              error_in_input_data = 1
              OTHERS              = 2.

          ls_fkkop_desc-g_l_acct = lv_hkont.
          ls_fkkop-item      = '0002'.
          APPEND ls_fkkop_desc TO lt_fkkop.
        ENDLOOP.
      ENDIF.

** En el caso de las becas SPP que el descuento es del 100% y los documentos
** se contabilizan con un neto de 0; al momento de contabilizar el formulario
** ya se registra como pagado, ya que nadie abonará un montó y la compensación
** posteriormente la hará en algun momento la transacción FPMA.
      IF lv_condicion_beca = 'B019'.
        ls_pre_admi_2-fecha_pago = sy-datum.
        ls_pre_admi_2-hora_pago = sy-uzeit.
        ls_pre_admi_2-sta2_solpago = 'P'.
        MODIFY zpre_admi_2 FROM ls_pre_admi_2.
      ENDIF.
    ENDIF.
*-- Fin Modificación Adepcon 27.03.2017 ----------------------------------------------

    APPEND ls_fkkop TO lt_fkkop.

    CALL FUNCTION 'BAPI_CTRACDOCUMENT_CREATE'
      EXPORTING
        testrun          = ' '
        documentheader   = ls_fkkko
      IMPORTING
        documentnumber   = ev_opbel
        return           = es_return
      TABLES
        partnerpositions = lt_fkkop.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFUNCTION.
