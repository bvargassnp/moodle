FUNCTION z_edu_facturacion.
*"----------------------------------------------------------------------
*"*"Interfase local.
*"  IMPORTING
*"     REFERENCE(IV_ANIO) TYPE  PIQPERYR
*"     REFERENCE(IV_PERIODO) TYPE  PIQPERID
*"     REFERENCE(IV_ESTUDIANTE) TYPE  HROBJID
*"     REFERENCE(IV_SIMULADO) TYPE  XFELD OPTIONAL
*"     REFERENCE(IV_VISUALIZAR) TYPE  BOOLE_D OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_TARIFA) TYPE  CHAR20
*"     REFERENCE(EV_OPBEL) TYPE  OPBEL_KK
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"      ET_ERROR TYPE  HRIQ_ERROR_STRUCTURE OPTIONAL
*"  CHANGING
*"     REFERENCE(CV_XPOST) TYPE  CMAC_XPOST OPTIONAL
*"----------------------------------------------------------------------


  DATA : lt_program_of_study TYPE TABLE OF piq_reg_program_of_study,
         lt_fikey            TYPE TABLE OF cmac_fikey,
         lt_error            TYPE bapiret2_t,
         lt_open_pkey        TYPE t7piqpkey,
         lt_period_key       TYPE piq_period_key_tab,
         lt_keydate          TYPE piq_objtype_keydate_cate_tab,
         lt_feedoc_nr        TYPE cmac_fee_docnr_t,
         lt_student          TYPE piqst_objid_t,
         lt_student_         TYPE hriq_student_list_tab,
         lt_persl            TYPE cmac_persl_t,
         lt_ficadoc          TYPE cmac_feefica_t,
         lt_trig_info        TYPE piq_trigger_info_tab,
         lt_feehd            TYPE cmac_feehd_t,
         lt_feesc            TYPE cmac_feesc_t,
         lt_feesm            TYPE cmac_feesm_t,
         lt_itemres          TYPE cmac_itemres_t,
         ls_ficadoc          TYPE cmacdb_feefica,
         ls_program_of_study TYPE piq_reg_program_of_study,
         ls_itemres          TYPE cmacdb_itemres,
         ls_feectrl          TYPE piq_feectrl,
         ls_knumh            TYPE konp-knumh,
         ls_period_header    TYPE t7piqpkey,
         ls_feectrle         TYPE t7piqfeectrle,
         lv_period_key       TYPE piq_period_key-persl,
         lv_post             TYPE piqflag,
         ls_error            TYPE piq_error_structure,
         lv_out              TYPE c,
         lv_fact_key_sc      TYPE zedu_fact_key_sc.               "Reg zedu_fac_key_sc

  FIELD-SYMBOLS: <fs_trig_info> TYPE piq_trigger_info.

  CONSTANTS: lc_error_e   TYPE bapi_mtype   VALUE 'E',           "Mensaje E
             lc_error_i   TYPE bapi_mtype   VALUE 'I',           "Mensaje I
             lc_error_a   TYPE bapi_mtype   VALUE 'A',           "Mensaje A
             lc_persl_oc  TYPE persl_kk     VALUE 'FOTR',        "Formal otros conceptos.
             lc_tipofac_o TYPE zedu_tipofac VALUE 'O',           "Tipo de factura otros conceptos
             lc_symsgid   TYPE symsgid VALUE 'ZEDU_FACTURACION', "Clase de mensaje
             lc_anoper    TYPE progname VALUE 'T7PIQYEARPRD'.    "Periodos académicos vs financieros

  SELECT SINGLE persl
    FROM t7piqyearprd
    INTO lv_period_key
    WHERE peryr = iv_anio
      AND perid = iv_periodo.

  IF sy-subrc <> 0.
    ls_error-msgid = lc_symsgid.
    ls_error-msgty = lc_error_e.
    ls_error-msgno = '013'.
    ls_error-msgv1 = lc_anoper.
    ls_error-msgv2 = iv_anio.
    ls_error-msgv3 = iv_periodo.
    EXIT.
  ENDIF.

  CALL FUNCTION 'HRIQ_READ_PERIOD_KEY_INFO'
    EXPORTING
      iv_period_key = lv_period_key
    IMPORTING
      et_period_key = lt_period_key
    TABLES
      et_error      = et_error.

  CALL FUNCTION 'HRIQ_READ_KEYDATE_CATEGORY'
    EXPORTING
      it_period_key = lt_period_key
    IMPORTING
      et_keydate    = lt_keydate
    EXCEPTIONS
      nothing_found = 1
      only_sm       = 2
      no_sc         = 3
      no_st         = 4
      OTHERS        = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  APPEND iv_estudiante TO lt_student_.

  SELECT SINGLE * INTO  ls_feectrle
                  FROM  t7piqfeectrle
                  WHERE feecalcmode = '1'.

  MOVE-CORRESPONDING ls_feectrle TO ls_feectrl.

  IF NOT ls_feectrle-trigger1 IS INITIAL.
    SELECT * APPENDING CORRESPONDING FIELDS OF
             TABLE lt_trig_info
             FROM  t7piqfeetrig
             WHERE int_trig = ls_feectrle-trigger1.
    LOOP AT lt_trig_info ASSIGNING <fs_trig_info>.
      <fs_trig_info>-pmode    = ls_feectrle-pmode.
      <fs_trig_info>-ext_trig = '1'.
    ENDLOOP.
  ENDIF.
  IF NOT ls_feectrle-trigger2 IS INITIAL.
    SELECT * APPENDING CORRESPONDING FIELDS OF
             TABLE lt_trig_info
             FROM  t7piqfeetrig
             WHERE int_trig = ls_feectrle-trigger2.
    LOOP AT lt_trig_info ASSIGNING <fs_trig_info>.
      <fs_trig_info>-pmode    = ls_feectrle-pmode.
      <fs_trig_info>-ext_trig = '1'.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'HRIQ_READ_REGISTERED_PROGRAM'
    EXPORTING
      iv_istat            = '1'
      iv_feetrigger       = '1'
      iv_open_pkey        = lv_period_key
      it_student          = lt_student_
      it_period_key       = lt_period_key
      it_trig_info        = lt_trig_info
    IMPORTING
      et_reg_program_info = lt_program_of_study
    TABLES
      et_error            = et_error.

  DELETE ADJACENT DUPLICATES FROM lt_program_of_study COMPARING scobjid.

  ls_period_header-persl = lv_period_key.
  ls_period_header-excht = '100*'.

  IF iv_simulado IS INITIAL. " Si se requiere simulado.
    lv_post = 'X'.
  ENDIF.

  CALL FUNCTION 'HRIQCA_FEE_CALCULATION'
    EXPORTING
      iv_post               = lv_post
      iv_date               = sy-datum
      iv_display_result     = iv_visualizar
      is_period_header      = ls_period_header
      iv_fee_grp            = '2'
      iv_feemode            = '1'
      is_feectrl            = ls_feectrl
    IMPORTING
      et_feedoc_nr          = lt_feedoc_nr
    TABLES
      it_program_of_study   = lt_program_of_study
      it_keydate            = lt_keydate
      et_error              = et_error
      it_period_key         = lt_period_key
      et_fikey              = lt_fikey
    EXCEPTIONS
      system_failure        = 1
      communication_failure = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*&LAT 20-12-2016 0001 INICIO: Pruebas de facturación cuando los parámetros fueron establecidos con Ignacio como OK
*  DESCRIBE TABLE et_error.
*  IF sy-tfill > 0.
*    LOOP AT et_error INTO ls_error.
*      IF ls_error-msgty = lc_error_e OR ls_error-msgty = lc_error_i OR ls_error-msgty = lc_error_a.
*        ev_tarifa = 'NO'.
*        lv_out = 'X'.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*    IF lv_out = 'X'.
*      EXIT. "No continuar
*    ENDIF.
*  ENDIF.
*&LAT 20-12-2016 0002 FINAL: Pruebas de facturación cuando los parámetros fueron establecidos con Ignacio como OK
  CALL FUNCTION 'Z_EDU_GET_FACT_KEY_SC'
    IMPORTING
      e_fact_key_sc = lv_fact_key_sc.

  APPEND iv_estudiante TO lt_student.
  APPEND lv_period_key TO lt_persl.

  IF lv_fact_key_sc-tifa = lc_tipofac_o.
    REFRESH lt_persl.
    APPEND lc_persl_oc TO lt_persl.
  ENDIF.

  IF lv_fact_key_sc-persl IS NOT INITIAL.
    REFRESH lt_persl.
    APPEND lv_fact_key_sc-persl TO lt_persl.
  ENDIF.

  CALL FUNCTION 'CMAC_RFC_FEEDOC_READ'
    EXPORTING
      it_docnr         = lt_feedoc_nr
      it_student       = lt_student
      it_persl         = lt_persl
      iv_feecalcmode   = '1'
    IMPORTING
      et_feehd         = lt_feehd
      et_feesc         = lt_feesc
      et_feesm         = lt_feesm
      et_ficadoc       = lt_ficadoc
      et_itemres       = lt_itemres
      et_return        = lt_error
    EXCEPTIONS
      no_authorization = 1
      feedoc_not_found = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*-- Contabilizado luego de mostar en display
  IF lv_fact_key_sc-xpost = cl_hrpiq00const=>c_checked.
    cv_xpost = cl_hrpiq00const=>c_checked.
  ENDIF.

  IF lt_itemres[] IS INITIAL.
    ev_tarifa = 'NO'.
  ELSE.
    READ TABLE lt_itemres INTO ls_itemres INDEX 1.
*--&LAT se lee el dato contabilizado o de lo contrario la primera posición.
    READ TABLE lt_ficadoc INTO ls_ficadoc WITH KEY xnew = cl_hrpiq00const=>c_checked.
    IF sy-subrc <> 0.
      READ TABLE lt_ficadoc INTO ls_ficadoc WITH KEY itemnr = ls_itemres-itemnr.
    ENDIF.
*-.&LAT fin mejora para encontrar documento contabilizado.
    ev_opbel = ls_ficadoc-opbel.

    WRITE ls_itemres-docamt TO ev_tarifa CURRENCY ls_itemres-doccuky LEFT-JUSTIFIED.
  ENDIF.

ENDFUNCTION.
