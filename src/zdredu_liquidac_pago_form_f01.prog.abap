*4. implementación de los métodos de la interface
CLASS  lcl_selappif IMPLEMENTATION.
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~get_param.
    ex_selmeth = p_selmet.
    ex_selvari = p_selvar.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~get_param
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~set_param.
    p_selmet = im_selmeth.
    p_selvar = im_selvari.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~set_param
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~get_okcode.
    ex_okcode = sscrfields-ucomm.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~get_okcode
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~set_okcode.
    sscrfields-ucomm = im_okcode.
    sy-ucomm         = im_okcode.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~set_okcode
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~get_application_info.
    DATA: l_repid.
    l_repid = sy-repid.
    ex_repid = l_repid.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~get_application_info
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~set_subscreen_data.
    rep_subscr = im_seltabinfo.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~set_subscreen_data
**---------------------------------------------------------------------*
  METHOD if_hrpiq00selmethod_appif~get_plvar.
    CALL FUNCTION 'HRIQ_GET_ACTIVE_WF_PLVAR'
      EXPORTING
        ask_plvar_dialog = ' '
      IMPORTING
        act_plvar        = ex_plvar
      EXCEPTIONS
        no_active_plvar  = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'A' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                    "if_hrpiq00selmethod_appif~get_plvar
ENDCLASS.                    "

*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMIR_FORMULARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprimir_formulario CHANGING pt_mensajes TYPE bapiret2_t.

  TYPES: BEGIN OF lty_xblnr_gpart,
           xblnr TYPE xblnr_kk,
           gpart TYPE gpart_kk,
         END OF lty_xblnr_gpart.

  DATA: lt_return      TYPE wrf_bapireturn_tty,
        ls_return      TYPE bapireturn,
        ls_fechas      TYPE zedu_s_liquidacion_pago_fechas,
        lv_opbel       TYPE opbel_kk,
        lv_gpart       TYPE gpart_kk,
        ls_mensajes    TYPE bapiret2,
        lv_otf         TYPE char1,
        lt_xblnr_gpart TYPE SORTED TABLE OF lty_xblnr_gpart WITH NON-UNIQUE KEY primary_key COMPONENTS xblnr,
        ls_xblnr_gpart TYPE lty_xblnr_gpart,
        lv_xblnr       TYPE xblnr_kk.

  DATA lt_documentos_form TYPE fkkinv_opbel_tab. "-->  MgM DCEK901536

  ls_fechas-calendario   = p_calen.
  ls_fechas-manual       = p_manual.
  ls_fechas-ord_date     = p_ordina.
  ls_fechas-extra1_date  = p_extra1.
  ls_fechas-extra2_date  = p_extra2.


  lv_otf = p_nodial.
  IF p_selliq = 5.  "Liquidación de pago inscripción admisiones
    CALL FUNCTION 'ZEDU_LIQUIDACION_PAGO_FORM'
      EXPORTING
        i_nr_formulario  = p_nrform
        i_id_liquidacion = p_selliq
        i_fechas         = ls_fechas
        i_email          = p_email
        i_otf            = lv_otf
      IMPORTING
        t_return         = lt_return.

    LOOP AT lt_return INTO ls_return.
      ls_mensajes-type = ls_return-type.
      ls_mensajes-message = ls_return-message.
      ls_mensajes-message_v1 = ls_return-message_v1.
      ls_mensajes-message_v2 = ls_return-message_v2.
      ls_mensajes-message_v3 = ls_return-message_v3.
      ls_mensajes-message_v4 = ls_return-message_v4.
      APPEND ls_mensajes TO pt_mensajes.
    ENDLOOP.
  ELSE.
    LOOP AT gt_documentos_acad INTO DATA(ls_documentos_acad).
      CHECK ls_documentos_acad-xblnr IS NOT INITIAL.
      ls_xblnr_gpart-xblnr = ls_documentos_acad-xblnr.
      ls_xblnr_gpart-gpart = ls_documentos_acad-gpart.
      INSERT ls_xblnr_gpart INTO TABLE lt_xblnr_gpart.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM lt_xblnr_gpart.

    LOOP AT lt_xblnr_gpart INTO ls_xblnr_gpart.
      lv_xblnr = ls_xblnr_gpart-xblnr.
      lv_gpart = ls_xblnr_gpart-gpart.

      CHECK lv_xblnr CO '0123456789'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_xblnr
        IMPORTING
          output = lv_opbel.

      "Verificar si se debe generar liquidación al estudiante
      READ TABLE gt_estudiantes INTO DATA(ls_estudiantes) WITH KEY partner = lv_gpart.

      "Adicionar Mensaje Cabecera para mejor lectura del Log en Generación Masiva
      CLEAR ls_mensajes.
      ls_mensajes-type = 'S'.
      ls_mensajes-id = 'ZDEDU_FACTURACION'.
      ls_mensajes-number = '106'.
      ls_mensajes-message_v1 = ls_estudiantes-otype.
      ls_mensajes-message_v2 = ls_estudiantes-objid.
      ls_mensajes-message_v3 = ls_estudiantes-partner.
      ls_mensajes-message_v4 = lv_xblnr.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZDEDU_FACTURACION'
          msgnr               = '106'
          msgv1               = ls_mensajes-message_v1
          msgv2               = ls_mensajes-message_v2
          msgv3               = ls_mensajes-message_v3
          msgv4               = ls_mensajes-message_v4
        IMPORTING
          message_text_output = ls_mensajes-message.
      APPEND ls_mensajes TO pt_mensajes.

      IF ls_estudiantes-no_generar_liq IS INITIAL.
        CALL FUNCTION 'ZEDU_LIQUIDACION_PAGO_FORM'
          EXPORTING
            i_opbel          = lv_opbel
            i_id_liquidacion = p_selliq
            i_fechas         = ls_fechas
            i_email          = p_email
            i_otf            = lv_otf
            i_docs_pagados   = p_cero
            i_mostrar_inter  = p_inter
          IMPORTING
            t_return         = lt_return
          CHANGING
            ct_documentos    = lt_documentos_form.

        LOOP AT lt_return INTO ls_return.
          ls_mensajes-type = ls_return-type.
          ls_mensajes-message = ls_return-message.
          ls_mensajes-message_v1 = ls_return-message_v1.
          ls_mensajes-message_v2 = ls_return-message_v2.
          ls_mensajes-message_v3 = ls_return-message_v3.
          ls_mensajes-message_v4 = ls_return-message_v4.
          APPEND ls_mensajes TO pt_mensajes.
        ENDLOOP.
        CLEAR:  lt_documentos_form, lt_return.
      ELSE.
        "Estudiante '&1' tiene deudas vencidas. No se genera Liq. de Pago.
        ls_mensajes-type = 'E'.
        ls_mensajes-id = 'ZDEDU_FACTURACION'.
        ls_mensajes-number = '102'.
        ls_mensajes-message_v1 = ls_estudiantes-otype && ls_estudiantes-objid.
        ls_mensajes-message_v2 = lv_opbel.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = 'ZDEDU_FACTURACION'
            msgnr               = '102'
            msgv1               = ls_mensajes-message_v1
            msgv2               = ls_mensajes-message_v2
          IMPORTING
            message_text_output = ls_mensajes-message.

        APPEND ls_mensajes TO pt_mensajes.
        IF 1 = 2. MESSAGE s102(zdedu_facturacion). ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LLENAR_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_listbox .

  DATA: lt_vrm_values  TYPE vrm_values,
        ls_vrm_values  TYPE vrm_value,
        ls_metodos_liq TYPE zedu_metodos_liq,
        lv_id          TYPE vrm_id.

  SELECT mandt
         id_liquidacion
         fecha_calendario
         fecha_manual
         fecha_ord
         fecha_extra1
         fecha_extra2
         descripcion_liq
    FROM zedu_metodos_liq
      INTO TABLE gt_metodos_liq.

  IF sy-subrc NE 0.
    REFRESH gt_metodos_liq.
  ENDIF.

  LOOP AT gt_metodos_liq INTO ls_metodos_liq.
    ls_vrm_values-key  = ls_metodos_liq-id_liquidacion.
    ls_vrm_values-text = ls_metodos_liq-descripcion_liq.
    APPEND ls_vrm_values TO lt_vrm_values.
    CLEAR ls_vrm_values.
  ENDLOOP.

  lv_id = co_p_selliq.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lv_id
      values          = lt_vrm_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_datos .
  CLEAR gt_estudiantes.
  IF p_selliq = 5.  "Liquidación de pago inscripción admisiones
    PERFORM f_validar_formulario.
  ELSE.
    PERFORM f_obtener_estudiantes CHANGING gt_estudiantes.
    IF p_deuda IS INITIAL.
      PERFORM f_validar_partidas_abiertas USING gt_estudiantes.
    ENDIF.
    PERFORM f_obtener_pagos USING gt_estudiantes.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_ESTUDIANTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_estudiantes CHANGING p_lt_estudiantes TYPE tyt_estudiantes.

  DATA: lt_cmacbpst    TYPE tyt_cmacbpst,
        lt_object_tab  TYPE hrobject_tab,
        ls_cmacbpst    TYPE tys_cmacbpst,
        ls_object_tab  TYPE hrobject,
        ls_estudiantes TYPE tys_estudiantes,
        lv_base        TYPE piqflag,
        lv_stru        TYPE piqflag,
        lv_fcode       TYPE t77fc-fcode,
        lv_count       TYPE i.

  CHECK p_selliq NE 5.

  lv_base  = co_x.
  lv_stru  = co_x.
  lv_fcode = co_disp.

  CALL METHOD g_selmethods_ref->get_objects
    EXPORTING
      im_base_authority_check   = lv_base
      im_stru_authority_check   = lv_stru
      im_fcode                  = lv_fcode
      im_period                 = gs_selperiod
    IMPORTING
      ex_object_tab             = lt_object_tab
      ex_stru_auth_failed_count = lv_count
    EXCEPTIONS
      selmethscen_not_found     = 1
      otype_not_supported       = 2
      selmeth_not_found         = 3
      invalid_otype             = 4
      selscen_not_found         = 5
      selmeth_not_active        = 6
      selvari_not_found         = 7
      objects_not_found         = 8
      no_stru_authority         = 9
      no_base_authority         = 10
      OTHERS                    = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lt_object_tab IS NOT INITIAL.

    SELECT partner
           stobjid
      FROM cmacbpst
      INTO TABLE lt_cmacbpst
      FOR ALL ENTRIES IN lt_object_tab
      WHERE stobjid = lt_object_tab-objid.

    IF sy-subrc = 0.
      SORT lt_cmacbpst BY stobjid.

      LOOP AT lt_object_tab INTO ls_object_tab.
        ls_estudiantes-plvar = ls_object_tab-plvar.
        ls_estudiantes-otype = ls_object_tab-otype.
        ls_estudiantes-objid = ls_object_tab-objid.
        READ TABLE lt_cmacbpst INTO ls_cmacbpst
        WITH KEY stobjid = ls_object_tab-objid
        BINARY SEARCH.
        IF sy-subrc = 0.
          ls_estudiantes-partner = ls_cmacbpst-partner.
        ENDIF.
        APPEND ls_estudiantes TO p_lt_estudiantes.
        CLEAR ls_estudiantes.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OCULTAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ocultar_campos .

  IF p_selmet IS INITIAL
    OR p_selvar IS INITIAL.
*  7. pai de la imagen de selección para el evento at selection-screen:
*   Ocultar campos fechas
    LOOP AT SCREEN.
      IF screen-group1 = co_fo
      OR screen-group1 = co_fov
      OR screen-group1 = co_fe1
      OR screen-group1 = co_fe2
      OR screen-group1 = co_ml
      OR screen-group1 = co_fc
      OR screen-group1 = co_fm
      OR screen-group1 = co_nf.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.

  IF p_selliq IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = co_fo
      OR screen-group1 = co_fov
      OR screen-group1 = co_fe1
      OR screen-group1 = co_fe2
      OR screen-group1 = co_fm
      OR screen-group1 = co_fc
      OR screen-group1 = co_nf.
        screen-active = 0.
      ELSEIF screen-group1 = co_ml.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
    EXIT.
  ENDIF.

  READ TABLE gt_metodos_liq
    INTO gs_metodo_liq
      WITH KEY id_liquidacion = p_selliq.

  IF sy-subrc = 0.

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN co_fc.
          IF gs_metodo_liq-fecha_calendario IS INITIAL.
            screen-active = 0.
            CLEAR p_calen.
            p_manual = co_x.
          ELSE.
            screen-active = 1.
          ENDIF.
        WHEN co_fm.
          IF gs_metodo_liq-fecha_manual IS INITIAL.
            screen-active = 0.
            CLEAR p_manual.
            p_calen = co_x.
          ELSE.
            screen-active = 1.
          ENDIF.
        WHEN co_nf.
          IF p_selliq = 5.
            screen-active = 1.
          ELSE.
            screen-active = 0.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF p_manual IS NOT INITIAL.

    CLEAR p_calen.

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN co_fo.
          IF gs_metodo_liq-fecha_ord IS INITIAL.
            screen-active = 0.
          ELSE.
            screen-active = 1.
          ENDIF.
        WHEN co_fov.
          IF gs_metodo_liq-fecha_ord IS INITIAL.
            screen-active = 0.
          ELSEIF gs_metodo_liq-fecha_ord IS NOT INITIAL.
            IF p_selliq = 4..
              screen-active = 1.
            ELSE.
              screen-active = 0.
            ENDIF.
          ENDIF.
        WHEN co_fe1.
          IF gs_metodo_liq-fecha_extra1 IS INITIAL.
            screen-active = 0.
          ELSE.
            screen-active = 1.
          ENDIF.
        WHEN co_fe2.
          IF gs_metodo_liq-fecha_extra2 IS INITIAL.
            screen-active = 0.
          ELSE.
            screen-active = 1.
          ENDIF.
      ENDCASE.

      MODIFY SCREEN.
    ENDLOOP.

  ELSE.

    LOOP AT SCREEN.
      IF screen-group1 = co_fo
      OR screen-group1 = co_fov
      OR screen-group1 = co_fe1
      OR screen-group1 = co_fe2.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PAGOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_pagos USING p_lt_estudiantes TYPE tyt_estudiantes.

  DATA: lt_contr_liqui TYPE tyt_contr_liqui,
        ls_contr_liqui TYPE tys_contr_liqui,
        r_psobtyp      TYPE RANGE OF dfkkop-psobtyp,
        ls_psobtyp     TYPE tys_range_psobtyp,
        lt_form_blar   TYPE SORTED TABLE OF zedu_c_form_blar WITH UNIQUE KEY primary_key COMPONENTS id_liquidacion blart.

  CHECK p_lt_estudiantes IS NOT INITIAL.

  IF p_selliq = 4.
    PERFORM f_crear_documento USING p_lt_estudiantes.
  ENDIF.

  SELECT psobtyp
    FROM zedu_contr_liqui
    INTO TABLE lt_contr_liqui
    WHERE id_liquidacion = p_selliq.
  IF sy-subrc = 0.
    REFRESH r_psobtyp.
    LOOP AT lt_contr_liqui INTO ls_contr_liqui.
      ls_psobtyp-sign   = co_i.
      ls_psobtyp-option = co_eq.
      ls_psobtyp-low    = ls_contr_liqui-psobtyp.
      APPEND ls_psobtyp TO r_psobtyp.
      CLEAR ls_psobtyp.
    ENDLOOP.

    SELECT  xblnr "-->  MgM DCEK901536
            opbel
            gpart
            blart "-->  MgM DCEK901536
      FROM dfkkop
      INTO TABLE gt_documentos
      FOR ALL ENTRIES IN p_lt_estudiantes
      WHERE gpart   = p_lt_estudiantes-partner
        AND psobtyp IN r_psobtyp.
    "AND augst EQ space. " sin compensar -->  MgM DCEK901536

* Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 16/11/2017
    "Obtener clases de documentos de Pago
    SELECT mandt id_liquidacion blart
    INTO TABLE lt_form_blar
    FROM zedu_c_form_blar
    WHERE id_liquidacion = p_selliq.
* Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 16/11/2017

*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 09/11/2016
    IF sy-subrc EQ 0.
      LOOP AT gt_documentos INTO DATA(ls_documentos).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_documentos-xblnr
          IMPORTING
            output = ls_documentos-xblnr. " Internal display of INPUT, any category

        IF ls_documentos-blart EQ gc_fac_academ OR
           ls_documentos-blart EQ gc_reser_cupo.
          INSERT ls_documentos INTO TABLE gt_documentos_acad.
        ELSE. "otros
* Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 16/11/2017
          "Obtener clases de documentos de Pago
          READ TABLE lt_form_blar INTO DATA(ls_form_blar) WITH KEY primary_key COMPONENTS id_liquidacion = p_selliq blart = ls_documentos-blart.
          IF sy-subrc <> 0.
            DELETE gt_documentos.
            CONTINUE.
          ELSE.
            IF ls_documentos-xblnr IS NOT INITIAL.
              DELETE gt_documentos.
              CONTINUE.
            ENDIF.
          ENDIF.
* Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 16/11/2017
          INSERT ls_documentos INTO TABLE gt_documentos_otros.
        ENDIF.
      ENDLOOP.
*      if sy-subrc ne 0.
    ELSE.
*	End	  -->	MgM DCEK901536

      MESSAGE e000(fica_cd)
      WITH text-018.

      REFRESH gt_documentos.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validar_entrada CHANGING p_message TYPE c.

  IF p_selliq IS INITIAL.
    gv_cursor = co_p_selliq.
    p_message = text-004.
  ELSE.

    IF p_manual IS NOT INITIAL.
      IF gs_metodo_liq-fecha_ord IS NOT INITIAL
      AND p_ordina IS INITIAL.
        gv_cursor = co_p_ordina.
        p_message = text-005.
      ELSEIF gs_metodo_liq-fecha_extra1 IS NOT INITIAL
      AND p_extra1 IS INITIAL AND p_inter IS INITIAL.
        gv_cursor = co_p_extra1.
        p_message = text-006.
      ELSEIF gs_metodo_liq-fecha_extra2 IS NOT INITIAL
      AND p_extra2 IS INITIAL AND p_inter IS INITIAL.
        gv_cursor = co_p_extra2.
        p_message = text-007.
      ENDIF.
    ENDIF.

    IF  p_selliq = 5
    AND p_nrform IS INITIAL.
      gv_cursor = co_p_nrform.
      p_message = text-013.
    ENDIF.

    IF p_selliq = 4
      AND p_ordval IS INITIAL.
      gv_cursor = co_p_ordval.
      p_message = text-017.
    ENDIF.

    IF p_inter IS NOT INITIAL.
      IF p_calen IS NOT INITIAL OR
      p_ordina IS INITIAL OR
      p_extra1 IS NOT INITIAL OR
      p_extra2 IS NOT INITIAL.
        p_message = text-020.
      ENDIF.
    ENDIF.
  ENDIF.

  EXPORT gv_cursor
  TO MEMORY ID co_cursor.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_PARTIDAS_ABIERTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SELOBJECT_TAB  text
*----------------------------------------------------------------------*
FORM f_validar_partidas_abiertas USING p_lt_estudiantes TYPE tyt_estudiantes.
  TYPES: BEGIN OF lty_dfkkop,
           opbel TYPE opbel_kk,
           opupw TYPE opupw_kk,
           opupk TYPE opupk_kk,
           opupz TYPE opupz_kk,
           augst TYPE augst_kk,
           gpart TYPE gpart_kk,
           abwbl TYPE abwbl_kk,
           abwtp TYPE abwtp_kk,
           vkont TYPE vkont_kk,
           faedn TYPE faedn_kk,
           betrw TYPE betrw_kk,
         END OF lty_dfkkop.

  DATA: lt_dfkkop_deudas TYPE SORTED TABLE OF lty_dfkkop WITH UNIQUE KEY primary_key COMPONENTS opbel opupw opupk opupz
                                                         WITH NON-UNIQUE SORTED KEY ix1 COMPONENTS gpart,
        ls_dfkkop_deudas TYPE lty_dfkkop,
        lv_message       TYPE char50,
        lt_abwbl         TYPE SORTED TABLE OF opbel_kk WITH NON-UNIQUE KEY primary_key COMPONENTS table_line,
        lv_abwbl         TYPE opbel_kk,
        lt_bapifkkop     TYPE STANDARD TABLE OF bapifkkop,
        lt_bapifkkop_all TYPE SORTED TABLE OF bapifkkop WITH UNIQUE KEY primary_key COMPONENTS doc_no rep_item item sub_item
                                                        WITH NON-UNIQUE SORTED KEY ix1 COMPONENTS partner.

  CHECK p_lt_estudiantes IS NOT INITIAL.
  "PArtida vencida
  "Valor mayor que 0
  "-->Cuenta Contrato configurada
  "-->Partida abierta
  "si es plan de pago verificar la fecha de vencimiento de las partidas del plan de pago no considerar las originales

  "Obtener partidas abiertas, vencidas y con valor mayor que 0 para cada estudiante
  SELECT opbel
         opupw
         opupk
         opupz
         augst
         gpart
         vkont
         abwbl
         abwtp
         faedn
         betrw
   FROM dfkkop
    INTO CORRESPONDING FIELDS OF TABLE lt_dfkkop_deudas
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE augst = space AND
          gpart = p_lt_estudiantes-partner AND
          faedn < sy-datum AND
          betrw > 0.

  "Obtener información de Plan de Pagos si los hay
  LOOP AT lt_dfkkop_deudas INTO ls_dfkkop_deudas.
    CHECK ls_dfkkop_deudas-abwtp = 'R'.
    INSERT ls_dfkkop_deudas-abwbl INTO TABLE lt_abwbl.
    DELETE lt_dfkkop_deudas.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM lt_abwbl.

  LOOP AT lt_abwbl INTO lv_abwbl.
    CLEAR lt_bapifkkop.
    CALL FUNCTION 'BAPI_INSTMNTPLN_GETDETAIL'
      EXPORTING
        doc_no            = lv_abwbl
      TABLES
        installment_fkkop = lt_bapifkkop.

    "Eliminar cuotas del plan de pagos compensadas o no vencidas
    LOOP AT lt_bapifkkop INTO DATA(ls_bapifkkop).
      CHECK ls_bapifkkop-status = '9'.
      CHECK ls_bapifkkop-disc_due >= sy-datum.
      DELETE lt_bapifkkop.
    ENDLOOP.
    INSERT LINES OF lt_bapifkkop INTO TABLE lt_bapifkkop_all.
  ENDLOOP.

  LOOP AT p_lt_estudiantes ASSIGNING FIELD-SYMBOL(<fs_estudiantes>).
    "Si el estudiante tiene un registro en lt_dfkkop_deudas o lt_bapifkkop es porque tiene deuda vencida, no generar liquidación
    READ TABLE lt_dfkkop_deudas TRANSPORTING NO FIELDS WITH KEY ix1 COMPONENTS gpart = <fs_estudiantes>-partner.
    IF sy-subrc = 0.
      <fs_estudiantes>-no_generar_liq = 'X'.
      CONTINUE.
    ENDIF.
    READ TABLE lt_bapifkkop_all TRANSPORTING NO FIELDS WITH KEY ix1 COMPONENTS partner = <fs_estudiantes>-partner.
    IF sy-subrc = 0.
      <fs_estudiantes>-no_generar_liq = 'X'.
    ENDIF.
  ENDLOOP.

*  IF sy-subrc = 0.
*    READ TABLE lt_dfkkop_deudas INTO ls_dfkkop_deudas
*    INDEX 1.
*    IF sy-subrc = 0.
*      lv_message = text-010.
*      REPLACE co_andpersand
*      INTO lv_message
*      WITH ls_dfkkop_deudas-gpart.
*
*      MESSAGE e000(fica_cd)
*      WITH lv_message
*           text-011.
*    ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_crear_documento USING p_lt_estudiantes TYPE tyt_estudiantes.

  DATA: lco_intentos TYPE i VALUE 3,
        lco_segundos TYPE i VALUE 5.

  DATA: lt_positions      TYPE bapidfkkop_tab,
        lt_dpsob_bp_acc   TYPE tyt_dpsob_bp_acc,
        lt_param          TYPE tyt_param,
        ls_param          TYPE tys_param,
        ls_header         TYPE bapidfkkko,
        ls_return         TYPE bapiret2,
        ls_pagos          TYPE tys_dfkkop,
        ls_estudiantes    TYPE tys_estudiantes,
        lv_documentnumber TYPE bapidfkkko-doc_no,
        lv_doc_type       TYPE bapidfkkop-doc_type,
        lv_main_trans     TYPE bapidfkkop-main_trans,
        lv_sub_trans      TYPE bapidfkkop-sub_trans,
        lv_appl_area      TYPE bapidfkkop-appl_area,
        lv_comp_code      TYPE bapidfkkop-comp_code,
        lv_currency       TYPE bapidfkkop-currency,
        lv_segment        TYPE bapidfkkop-segment,
        lv_prctr          TYPE bapidfkkop-profit_ctr.



  SELECT idparam
         valor
    FROM zedu_c_param
    INTO TABLE lt_param
    WHERE repid = sy-repid.
  IF sy-subrc = 0.
    LOOP AT lt_param INTO ls_param.
      CASE ls_param-idparam.
        WHEN co_doc_type.
          lv_doc_type = ls_param-valor.
        WHEN co_main_trans.
          lv_main_trans = ls_param-valor.
        WHEN co_sub_trans.
          lv_sub_trans = ls_param-valor.
        WHEN co_appl_area.
          lv_appl_area = ls_param-valor.
        WHEN co_comp_code.
          lv_comp_code = ls_param-valor.
        WHEN co_currency.
          lv_currency = ls_param-valor.
        WHEN co_segment.
          lv_segment = ls_param-valor.
        WHEN co_prctr.
          lv_prctr = ls_param-valor.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  SELECT a~psobkey
         a~partner
         a~partneracctyp
         a~partneracc
    FROM dpsob_bp_acc AS a
    INNER JOIN dpsob AS b
    ON a~psobkey = b~psobkey
    INTO TABLE lt_dpsob_bp_acc
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE a~partner       = p_lt_estudiantes-partner
      AND a~partneracctyp = co_st
      AND b~psobtyp       = co_o033.

  IF sy-subrc NE 0.
    CLEAR lt_dpsob_bp_acc[].
  ENDIF.

  LOOP AT p_lt_estudiantes INTO ls_estudiantes.
    CLEAR lt_positions.

    PERFORM f_armar_cabecera
    USING lv_doc_type
          lv_appl_area
          lv_currency
    CHANGING ls_header.

    PERFORM f_armar_posiciones
    USING ls_estudiantes
          lt_dpsob_bp_acc
          lv_comp_code
          lv_segment
          lv_appl_area
          lv_main_trans
          lv_sub_trans
          lv_doc_type
          lv_currency
          lv_prctr
    CHANGING lt_positions.

    CALL FUNCTION 'BAPI_CTRACDOCUMENT_CREATE'
      EXPORTING
        testrun          = space
        documentheader   = ls_header
      IMPORTING
        documentnumber   = lv_documentnumber
        return           = ls_return
      TABLES
        partnerpositions = lt_positions.
    IF ls_return-type = co_e.

      MESSAGE
      ID     ls_return-id
      TYPE   ls_return-type
      NUMBER ls_return-number
      WITH ls_return-message_v1
           ls_return-message_v2
           ls_return-message_v3
           ls_return-message_v4.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      "Esperar n intentos de x segundos hasta que el documento tenga la referencia, de lo contrario
      "no imprime el recibo.
      DO lco_intentos TIMES.
        SELECT SINGLE xblnr
          INTO @DATA(lv_xblnr)
          FROM dfkkko
          WHERE opbel = @lv_documentnumber.

        IF lv_xblnr IS INITIAL.
          WAIT UP TO lco_segundos SECONDS.
        ENDIF.
      ENDDO.

      ls_pagos-opbel = lv_documentnumber.
      ls_pagos-gpart = ls_estudiantes-partner.
      APPEND ls_pagos TO
             gt_documentos.
      CLEAR ls_pagos.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PROXIMO_NUMERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_RANGE  text
*      -->P_LV_NR_RANGE  text
*----------------------------------------------------------------------*
FORM f_obtener_proximo_numero  USING p_object   TYPE inri-object
                                     p_nr_range TYPE inri-nrrangenr
                               CHANGING p_number.

  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
    EXPORTING
      object           = p_object
    EXCEPTIONS
      foreign_lock     = 1
      object_not_found = 2
      system_failure   = 3
      OTHERS           = 4.
  IF sy-subrc = 0.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = p_nr_range
        object                  = p_object
      IMPORTING
        number                  = p_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
      EXPORTING
        object           = p_object
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_CABECERA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_HEADER  text
*----------------------------------------------------------------------*
FORM f_armar_cabecera USING p_doc_type
                            p_appl_area
                            p_currency
                      CHANGING p_header TYPE bapidfkkko.

  DATA: lv_object   TYPE inri-object,
        lv_applk    TYPE fkkko-applk,
        lv_fikey    TYPE fkkko-fikey,
        lv_nr_range TYPE inri-nrrangenr.

  p_header-doc_type   = p_doc_type.
  p_header-appl_area  = p_appl_area.

  CALL FUNCTION 'FKK_GET_APPLICATION'
    EXPORTING
      i_no_dialog      = 'X'
    IMPORTING
      e_applk          = lv_applk
    EXCEPTIONS
      no_appl_selected = 1
      OTHERS           = 2.

  IF sy-subrc = 0.

    CALL FUNCTION 'FKK_CALL_EVENT_1113'
      EXPORTING
        i_herkf = '01'
        i_applk = lv_applk
      IMPORTING
        e_fikey = lv_fikey.

    IF lv_fikey IS NOT INITIAL.
      CALL FUNCTION 'FKK_FIKEY_CHECK'
        EXPORTING
          i_fikey                = lv_fikey
          i_open_on_request      = 'X'
          i_open_without_dialog  = 'X'
          i_non_existing_allowed = 'X'
        EXCEPTIONS
          non_existing           = 1
          OTHERS                 = 2.
      IF sy-subrc = 0.
        p_header-fikey = lv_fikey.
      ENDIF.
    ENDIF.
  ENDIF.

  p_header-doc_date       = sy-datum.   "Fecha del dia.
  p_header-post_date      = sy-datum.   "Fecha del dia.
  p_header-currency       = p_currency. "Fijo COP
  p_header-doc_source_key = co_01.

  lv_nr_range = co_01.
  lv_object   = co_zedu_rcupo.

*  PERFORM f_obtener_proximo_numero
*  USING lv_object
*        lv_nr_range
*  CHANGING p_header-ref_doc_no.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_POSICIONES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_POSITIONS  text
*----------------------------------------------------------------------*
FORM f_armar_posiciones USING p_estudiantes     TYPE tys_estudiantes
                              p_lt_dpsob_bp_acc TYPE tyt_dpsob_bp_acc
                              p_comp_code
                              p_segment
                              p_appl_area
                              p_main_trans
                              p_sub_trans
                              p_doc_type
                              p_currency
                              p_prctr
                        CHANGING p_lt_positions TYPE bapidfkkop_tab.

  DATA: ls_positions    TYPE bapidfkkop,
        ls_param        TYPE tys_param,
        ls_dpsob_bp_acc TYPE tys_dpsob_bp_acc.

*  ls_positions-rep_item     = 1.
  ls_positions-item         = 1.
  ls_positions-segment      = p_segment.
  ls_positions-buspartner   = p_estudiantes-partner..
  READ TABLE p_lt_dpsob_bp_acc INTO ls_dpsob_bp_acc
  WITH KEY partner = p_estudiantes-partner.
  IF sy-subrc = 0.
    ls_positions-cont_acct  = ls_dpsob_bp_acc-partneracc.
    ls_positions-contract   = ls_dpsob_bp_acc-psobkey.
  ENDIF.

  ls_positions-comp_code    = p_comp_code.
  ls_positions-doc_type     = p_doc_type.
  ls_positions-actdeterid   = p_doc_type.
  ls_positions-appl_area    = p_appl_area.
  ls_positions-main_trans   = p_main_trans.
  ls_positions-sub_trans    = p_sub_trans.
  ls_positions-post_date    = sy-datum.
  ls_positions-doc_date     = sy-datum.
  ls_positions-net_date     = p_ordina.
  ls_positions-currency     = p_currency.
  ls_positions-currency_iso = p_currency.
  ls_positions-stat_key     = co_a.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_prctr
    IMPORTING
      output = ls_positions-profit_ctr.

  PERFORM f_determinar_cuenta
  CHANGING ls_positions.

*  SELECT SINGLE b~persl
*    FROM t7piqyearprd AS a
*    INNER JOIN t7piqpkeyi AS b
*    ON a~persl = b~persl
*    INTO ls_positions-period_key
*    WHERE a~peryr = sy-datum(4)
*      AND b~openfrom <= sy-datum
*      AND b~opento   >= sy-datum.
*
*  IF sy-subrc NE 0.
*    CLEAR ls_positions-period_key.
*  ENDIF.

  ls_positions-period_key = p_persl.
  ls_positions-amount = p_ordval.

  APPEND ls_positions
  TO p_lt_positions.
  CLEAR ls_positions.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_FORMULARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validar_formulario .

  DATA: lv_nr_formulario TYPE zpre_admi_1-nr_formulario,
        lv_message       TYPE char50.

  SELECT SINGLE nr_formulario
    FROM zpre_admi_1
    INTO lv_nr_formulario
    WHERE nr_formulario = p_nrform.
  IF sy-subrc NE 0.

    lv_message = text-014.
    REPLACE co_andpersand
    INTO lv_message
    WITH p_nrform.

    MESSAGE e000(fica_cd)
    WITH lv_message.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DETERMINAR_CUENTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_POSITIONS  text
*----------------------------------------------------------------------*
FORM f_determinar_cuenta  CHANGING p_positions TYPE bapidfkkop.

  DATA: lt_fbstab     TYPE STANDARD TABLE OF tfkfbc,
        ls_fbstab     TYPE tfkfbc,
        ls_fkkop_1101 TYPE fkkop,
        lv_hkont      TYPE fkkop-hkont.

  "Determinar Cuenta de la posición de Partner
  CALL FUNCTION 'FKK_FUNC_MODULE_DETERMINE'
    EXPORTING
      i_fbeve  = '1101'
    TABLES
      t_fbstab = lt_fbstab.

  ls_fkkop_1101-bukrs = p_positions-comp_code.
  ls_fkkop_1101-hvorg = p_positions-main_trans.
  ls_fkkop_1101-tvorg = p_positions-sub_trans.
  ls_fkkop_1101-applk = p_positions-appl_area.
  ls_fkkop_1101-kofiz = 'RC'.

  LOOP AT lt_fbstab INTO ls_fbstab.
    CALL FUNCTION ls_fbstab-funcc
      EXPORTING
        i_fkkop             = ls_fkkop_1101
      IMPORTING
        e_hkont             = lv_hkont
      EXCEPTIONS
        error_in_input_data = 1
        OTHERS              = 2.

    p_positions-g_l_acct = lv_hkont.
  ENDLOOP.
ENDFORM.

FORM f_mostrar_alv USING pt_return TYPE bapiret2_t.
  TYPES: BEGIN OF lty_alv,
           icon    TYPE  icon_l4,
           mensaje TYPE  bapi_msg,
         END OF lty_alv.

  DATA: lcl_table     TYPE REF TO cl_salv_table,
        lcl_functions TYPE REF TO cl_salv_functions,
        lcl_display   TYPE REF TO cl_salv_display_settings,
        lt_msgs       TYPE STANDARD TABLE OF lty_alv,
        ls_msgs       TYPE lty_alv,
        ls_return     TYPE bapiret2.

  LOOP AT pt_return INTO ls_return.
    CASE ls_return-type.
      WHEN 'S'.
        ls_msgs-icon = '@5B@'.
      WHEN 'E'.
        ls_msgs-icon = '@5C@'.
      WHEN 'W'.
        ls_msgs-icon = '@5D@'.
    ENDCASE.

    ls_msgs-mensaje = ls_return-message.
    APPEND ls_msgs TO lt_msgs.
  ENDLOOP.

  cl_salv_table=>factory( IMPORTING r_salv_table = lcl_table
                           CHANGING t_table      = lt_msgs ).

  lcl_functions = lcl_table->get_functions( ).
  lcl_functions->set_all( abap_true ).
  lcl_display = lcl_table->get_display_settings( ).
  lcl_display->set_striped_pattern( cl_salv_display_settings=>true ).
  lcl_display->set_list_header( 'Procesamiento de Liquidación de Pago' ).
  lcl_table->display( ).
ENDFORM.
