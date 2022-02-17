"Name: \FU:CMAC_FEE_DOC_CREATE\SE:BEGIN\EI
ENHANCEMENT 0 ZEDU_TRATAR_CONT_DOCS.
  CONSTANTS: lco_separador        TYPE c LENGTH 1 VALUE '|',
             lco_pregrado         TYPE c LENGTH 3 VALUE 'PRE',
             lco_postgrado        TYPE c LENGTH 3 VALUE 'POS',
             lco_perio_plan_s     TYPE c LENGTH 1 VALUE 'S',
             lco_perio_plan_r     TYPE c LENGTH 1 VALUE 'R',
             lco_perio_plan_a     TYPE c LENGTH 1 VALUE 'A',
             lco_perio_plan_c     TYPE c LENGTH 1 VALUE 'C',
             lco_odontologico     TYPE c LENGTH 1 VALUE 'O',
             lco_continua         TYPE ZEDU_LINEA_E VALUE 'C'.


  DATA: lv_index_aux              TYPE i,
        lv_aufnr                  TYPE aufnr,
        ls_sc_data                TYPE cmac_sc,
        lv_xblnr                  TYPE xblnr_kk,
        ls_tfk033d                TYPE tfk033d,
        lv_tabnr                  TYPE HRTABNR,
        lv_prctr                  TYPE PRCTR,
        ls_fact_key_sc            TYPE zedu_fact_key_sc,
        lv_clase_plan             TYPE c LENGTH 3,
        lv_nivel_plan             TYPE c LENGTH 1,
        lv_perio_plan             TYPE c LENGTH 1,
        lv_key                    TYPE zedu_tipo_plan,
        lt_operacion              TYPE SORTED TABLE OF zedu_c_operacion WITH UNIQUE KEY primary_key COMPONENTS tipo_plan,
        ls_fkkop_1101             TYPE fkkop,
        lv_hkont                  TYPE fkkop-hkont,
        lt_fbstab                 TYPE STANDARD TABLE OF tfkfbc,
        ls_fbstab                 TYPE tfkfbc.

  FIELD-SYMBOLS: <fs_header_aux>  TYPE cmac_fkkko,
                 <fs_gl_item_aux> TYPE cmac_fkkopk,
                 <fs_bp_items>    TYPE cmac_fkkop.

*  LOOP AT it_header ASSIGNING <fs_header_aux>.
*
*    READ TABLE it_gl_items TRANSPORTING NO FIELDS
*                 WITH KEY docidx = <fs_header_aux>-docidx BINARY SEARCH.
*    lv_index_aux = sy-tabix.
*
*    LOOP AT it_gl_items ASSIGNING <fs_gl_item_aux> FROM lv_index_aux.
*      CHECK <fs_gl_item_aux>-docidx EQ <fs_header_aux>-docidx.
*      IF sy-tcode eq 'PQ_FEE_CALC'.
*        lv_aufnr = <fs_gl_item_aux>-aufnr.
*        CLEAR <fs_gl_item_aux>-aufnr.
*      ENDIF.
*    ENDLOOP.
*
*    READ TABLE it_sc_data INTO ls_sc_data INDEX 1.
*    IF sy-subrc IS NOT INITIAL.
*      CLEAR ls_sc_data.
*    ENDIF.
**    IF sy-tcode eq 'PQ_GRANT_CALC'.
**      <fs_header_aux>-xblnr = ls_sc_data-cmscid.
**    ELSEIF sy-tcode eq 'PQ_FEE_CALC'.
**      CONCATENATE ls_sc_data-cmscid lv_aufnr INTO <fs_header_aux>-xblnr.
**    ENDIF.
*    lv_xblnr = ls_sc_data-cmscid && lv_aufnr.
*  ENDLOOP.

  READ TABLE it_sc_data INTO ls_sc_data INDEX 1.
  LOOP AT it_bp_items ASSIGNING <fs_bp_items>.
    "Asignar Centro de Beneficio
    CLEAR ls_tfk033d.
    ls_tfk033d-applk = 'P'.
    ls_tfk033d-buber = '0310'.
    ls_tfk033d-ktopl = 'PUC'.
    ls_tfk033d-key01 = <fs_bp_items>-bukrs.
    ls_tfk033d-key03 = <fs_bp_items>-kofiz.
    ls_tfk033d-key04 = <fs_bp_items>-hvorg.
    ls_tfk033d-key05 = <fs_bp_items>-tvorg.

    CALL FUNCTION 'FKK_ACCOUNT_DETERMINE'
      EXPORTING
        i_tfk033d                = ls_tfk033d
     IMPORTING
       E_TFK033D                 = ls_tfk033d
     EXCEPTIONS
       ERROR_IN_INPUT_DATA       = 1
       NOTHING_FOUND             = 2
       OTHERS                    = 3.

    IF ls_tfk033d-fun02 IS INITIAL. "Cuando el centro de Beneficio no es establecido por la determinación de cuentas, buscarlo en el infotipo 1759
      "Buscar Centro de Beneficio del Plan de Estudio
      SELECT SINGLE tabnr
        INTO lv_tabnr
        FROM hrp1759
        WHERE plvar = '01' AND
              otype = 'SC' AND
              objid = ls_sc_data-cmscid AND
              begda <= sy-datum AND
              endda >= sy-datum.

       SELECT SINGLE profit_ctr
         INTO lv_prctr
         FROM hrt1759
         WHERE tabnr = lv_tabnr.

       IF sy-subrc = 0.
         <fs_bp_items>-prctr = lv_prctr.
       ENDIF.
    ENDIF.

  ENDLOOP.

  "Establecer Datos SLCM en memoria, se recuperan datos en Evento FICA 10 función Z_EDU_FKK_ASIG_DOC_ORIG_0010
  CALL FUNCTION 'Z_EDU_SET_FEE_DOC_DATA'
    EXPORTING
      is_st_data = is_st_data
      it_ficadoc = it_ficadoc.

  "-->Lpavia Completar referencia Adicional VTRE2 en todas las posiciones de partner
  "Obtener Datos de Memoria
  CALL FUNCTION 'Z_EDU_GET_FACT_KEY_SC'
   IMPORTING
     e_fact_key_sc = ls_fact_key_sc.


  IF ls_fact_key_sc-pregrado IS NOT INITIAL.
    lv_clase_plan = lco_pregrado.
    lv_nivel_plan = ls_fact_key_sc-subdiv_edu.
  ELSEIF ls_fact_key_sc-posgrado IS NOT INITIAL.
    lv_clase_plan = lco_postgrado.
    lv_nivel_plan = ls_fact_key_sc-subdiv_edu.
  ENDIF.
  IF ls_fact_key_sc-anualiza IS NOT INITIAL.
    IF ls_fact_key_sc-valor_fijo IS NOT INITIAL.
      lv_perio_plan = lco_perio_plan_a. "Anualizado Valor Fijo
    ELSE.
      lv_perio_plan = lco_perio_plan_c. "Anualizado Valor Credito
    ENDIF.
  ELSE.
    IF ls_fact_key_sc-valor_fijo IS NOT INITIAL.
      lv_perio_plan = lco_perio_plan_s. "Semestral Valor Fijo
    ELSE.
      lv_perio_plan = lco_perio_plan_r. "Semestral Valor Credito
    ENDIF.
  ENDIF.

  LOOP AT it_bp_items ASSIGNING <fs_bp_items>.
    <fs_bp_items>-vtre2 = ls_sc_data-cmscid && lco_separador &&
                          ls_fact_key_sc-linea_edu && lco_separador &&
                          lv_clase_plan && lco_separador &&
                          lv_nivel_plan && lco_separador &&
                          lv_perio_plan && lco_separador.


    IF ls_fact_key_sc-linea_edu = lco_continua. "Continua
      lv_key = ls_fact_key_sc-linea_edu.
    ELSE.
      lv_key = lv_nivel_plan.
    ENDIF.

    "Establecer Operación Principal y Parcial
    SELECT mandt tipo_plan hvorg tvorg
      INTO TABLE lt_operacion
      FROM zedu_c_operacion.

    READ TABLE lt_operacion INTO DATA(ls_operacion) WITH KEY primary_key COMPONENTS tipo_plan = lv_key.
    CHECK sy-subrc = 0.
    IF ls_operacion-hvorg IS NOT INITIAL AND ls_operacion-tvorg IS NOT INITIAL.
      <fs_bp_items>-hvorg = ls_operacion-hvorg.
      <fs_bp_items>-tvorg = ls_operacion-tvorg.
    ENDIF.

    "Establecer Cuenta
    CALL FUNCTION 'FKK_FUNC_MODULE_DETERMINE'
      EXPORTING
        i_fbeve  = '1101'
      TABLES
        t_fbstab = lt_fbstab.

    ls_fkkop_1101-bukrs = <fs_bp_items>-bukrs.
    ls_fkkop_1101-hvorg = <fs_bp_items>-hvorg.
    ls_fkkop_1101-tvorg = <fs_bp_items>-tvorg.
    ls_fkkop_1101-applk = <fs_bp_items>-applk.
    ls_fkkop_1101-kofiz = <fs_bp_items>-kofiz.

    LOOP AT lt_fbstab INTO ls_fbstab.
      CALL FUNCTION ls_fbstab-funcc
        EXPORTING
          i_fkkop             = ls_fkkop_1101
        IMPORTING
          e_hkont             = lv_hkont
        EXCEPTIONS
          error_in_input_data = 1
          OTHERS              = 2.

      <fs_bp_items>-hkont = lv_hkont.
    ENDLOOP.
  ENDLOOP.
  "<---LPavia Completar referencia Adicional VTRE2 en todas las posiciones de partner

*--&LAT 18-11-2016 001 INICIO: Odontológicos (por solicitud expresa de Leslie Bonilla.
  data:  lt_1732            TYPE TABLE OF p1732,           "Tabla Inf. para Categ. por SC
         lr_1732            TYPE p1732,                    "Rec. Inf. para Categ. por SC
         lt_object          TYPE TABLE OF hrobject,        "Tabla hrobject
         lr_object          TYPE hrobject,                 "Rec. hrobject
         lt_piqstudysegment TYPE piqstudysegment_t,        "Tabla studysegment
         lr_piqstudysegment TYPE piqstudysegment,          "Rec. studysegment
         lt_return          TYPE bapiret2_t,               "Tabla mensajes
         lt_1771            TYPE TABLE OF p1771,           "Tabla Inf. para créditos por SM
         lr_1771            TYPE p1771,                    "Rec. Inf. para créditos por SM
         lt_timelimits      type PIQ_ACADEMIC_CALENDAR_TAB,"Tabla timelimits
         lr_timelimits      type PIQ_ACADEMIC_CALENDAR.    "Red. timelimits


  CONSTANTS: lc_SCFEECAT_odon type PIQSCFEECAT value '155',  "Categ. Clínicos odont. SC
             lc_tl_odon       type PIQTIMELIMIT value 'ODON'."Timelimits Odontológicos

  lr_object-plvar = cl_hrpiq00const=>c_plvar_active.
  lr_object-otype = cl_hrpiq00const=>c_otype_sc.
  lr_object-objid = ls_sc_data-cmscid.
  append lr_object to lt_object.

  CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
       EXPORTING
          infty                 = cl_hrpiq00const=>c_infty_1732
          istat                 = '1'
          begda                 = IS_FEE_CONTROL-CALCDATE
          endda                 = IS_FEE_CONTROL-CALCDATE
       TABLES
          innnn                 = lt_1732
          objects               = lt_object
       EXCEPTIONS
          nothing_found         = 1
          wrong_condition       = 2
          infotyp_not_supported = 3
          OTHERS                = 4.

  read table lt_1732 into lr_1732 index 1.
  if lr_1732-SCFEECAT = lc_SCFEECAT_odon. "Categ. SC odontológico

    CALL FUNCTION 'HRIQ_STUDENT_STUDYSEGM_READ'
        EXPORTING
          iv_plvar          = cl_hrpiq00const=>c_plvar_active
          iv_st_objid       = IS_ST_DATA-CMSTID
          iv_sc_objid       = ls_sc_data-cmscid
        IMPORTING
          et_segments       = lt_piqstudysegment
          et_message_return = lt_return.

    read table lt_piqstudysegment into lr_piqstudysegment index 1.

    lr_object-plvar = cl_hrpiq00const=>c_plvar_active.
    lr_object-otype = cl_hrpiq00const=>c_otype_cs.
    lr_object-objid = lr_piqstudysegment-CS_OBJID.
    append lr_object to lt_object.

    CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
       EXPORTING
         infty                 = cl_hrpiq00const=>c_infty_1771
         istat                 = cl_hrpiq00const=>C_ISTAT_ACTIVE
       TABLES
         innnn                 = lt_1771
         objects               = lt_object
       EXCEPTIONS
         nothing_found         = 1
         wrong_condition       = 2
         infotyp_not_supported = 3
         OTHERS                = 4.

    DESCRIBE TABLE lt_1771.
    if sy-tfill = 1. "Primer nivel odontológico.
      "-->Lpavia Completar Flag de Odontologica en referencia Adicional VTRE2 en todas las posiciones de partner
      LOOP AT it_bp_items ASSIGNING <fs_bp_items>.
        <fs_bp_items>-vtre2 = <fs_bp_items>-vtre2 && lco_odontologico.
      ENDLOOP.
      "<---LPavia Completar referencia Adicional VTRE2 en todas las posiciones de partner


      DATA: lv_error1     TYPE boole_d,
            lv_index1     TYPE sytabix,
            lv_resob1     TYPE resob_kk,
            lv_resky1     TYPE resky_kk,
            lv_dummy1     TYPE cmac_msg_handler.

      DATA: ls_fkkko1     TYPE fkkko,
            ls_ficadoc1   TYPE cmacdb_feefica,
            lt_fkkop1     TYPE TABLE OF fkkop  WITH HEADER LINE,
            lt_fkkcl1     TYPE TABLE OF fkkcl  WITH HEADER LINE,
            lt_fkkopk1    TYPE TABLE OF fkkopk WITH HEADER LINE,
            lt_ienqtab1   TYPE TABLE OF ienqtab     WITH HEADER LINE,
            lt_fkkop_clr1 TYPE TABLE OF cmac_fkkop  WITH HEADER LINE,
            lt_doc_index1 TYPE TABLE OF s_doc_index WITH HEADER LINE.

      DATA: lv_ficadocnr1  TYPE opbel_kk,
            lv_cmacdocnr1  TYPE cmac_fee_docnr,
            lt_ficadoc1    TYPE cmac_feefica_t,
            ls_fica_odont1 TYPE zedu_cmacdb_feef,
            ls_fica_odont2 TYPE zedu_cmacdb_feef,
            lv_secuencia_odont TYPE char1,
            lt_log_tab1    TYPE hriq_error_structure.

*-- &LAT 21-11-2016 Tabla con valores.
      TYPES: BEGIN OF ty_fkkop,
               BETRH type BETRH_KK,
               BETRW type BETRW_KK,
               BETR2 type BETR2_KK,
               BETR3 type BETR3_KK,
               SKFBT type SKFBT_KK,
               SBETH type SBETH_KK,
               SBETW type SBETW_KK,
               SBET2 type SBET2_KK,
               SBET3 type SBET3_KK,
               AUGBT Type  AUGBT_KK,
               AUGBS Type  AUGBS_KK,
               AUGSK Type  AUGSK_KK,
               FDWBT Type  FDWBT,
               QSSHB Type  QSSHB_KK,
               QBSHB Type  QBSHB_KK,
               SCTAX Type  SCTAX_KK,
               STTAX Type  STTAX_KK,
               PSWBT Type  PSWBT_KK,
               PSWTX Type  PSWTX_KK,
             end of ty_fkkop,

             begin of ty_fkkopk,
               BETRH  Type  BETRH_KK,
               BETRW  Type  BETRW_KK,
               BETR2  Type  BETR2_KK,
               BETR3  Type  BETR3_KK,
               SBASH  Type  SBASH_KK,
               SBASW  Type  SBASW_KK,
               SCTAX  Type  SCTAX_KK,
             end of ty_fkkopk.

      data: lt_fkkop_v  type table of ty_fkkop,  "Tabla con valores de fkkop para divisiones del 50%
            lt_fkkopk_v type table of ty_fkkopk, "Tabla con valores de fkkopk para divisiones del 50%
            lr_fkkop_v  type ty_fkkop,           "Rec. con valores de fkkop para divisiones del 50%
            lr_fkkopk_v type ty_fkkopk,          "Rec. con valores de fkkopk para divisiones del 50%
            lv_conta    type i,                  "Contador de contabilizaciones
            lv_fecha2_v type FAEDN_KK.           "Fecha de vencimiento odont. sgdo dcto.

      FIELD-SYMBOLS: <fs_header1>  TYPE cmac_fkkko,
                     <fs_bp_item1> TYPE cmac_fkkop,
                     <fs_cl_item1> TYPE cmac_fkkcl,
                     <fs_gl_item1> TYPE cmac_fkkopk,
                     <fs_ficadoc1> TYPE cmacdb_feefica.

      do 2 TIMES.
        lv_conta = lv_conta + 1.

        "Inicio agregado : Leonardo de Jesus Pavia ( LPAVIA ), del 13/03/2019
        "Log de documento adicional en Odontologicos para calculo de Doc Ref. en Deltas
        lv_secuencia_odont = lv_conta.
        CALL FUNCTION 'Z_EDU_SET_FEE_DOC_DATA'
         EXPORTING
           i_secuencia_odont = lv_secuencia_odont
           it_ficadoc        = it_ficadoc.
        "Fin agregado : Leonardo de Jesus Pavia ( LPAVIA ), del 13/03/2019

        clear: ls_fkkko1,ls_ficadoc1.
        refresh: lt_fkkop1, lt_fkkcl1,lt_fkkopk1,
                 lt_ienqtab1, lt_fkkop_clr1, lt_doc_index1.
        clear: lv_ficadocnr1, lv_cmacdocnr1, lt_ficadoc1, lt_log_tab1.

* Get next fee calculation document number
        CALL FUNCTION 'CMAC_FEEDOC_NEXTNR_GET'
          EXPORTING
           iv_calc_base           = is_fee_control-cmfeegroup
           iv_feecalcmode         = is_fee_control-cmprocmode
          IMPORTING
            ev_docnr               = lv_cmacdocnr1
          EXCEPTIONS
            number_range_not_found = 1
            number_not_found       = 2
            OTHERS                 = 3.
        IF sy-subrc NE 0.
         CALL FUNCTION 'CMAC_ERROR_HANDLE'
           EXPORTING
             iv_otype     = c_student
             iv_objabbr   = is_st_data-cmstnum
             iv_subobject = c_log_sub_feepost
           TABLES
             et_error     = ct_log_tab.
         EXIT.
        ENDIF.

* Update table for FI-CA documents with fee calculation document number
        lt_ficadoc1[] = it_ficadoc[].
        LOOP AT lt_ficadoc1 ASSIGNING <fs_ficadoc1>.
          <fs_ficadoc1>-docnr = lv_cmacdocnr1.
        ENDLOOP.

*       Keep the tables sorted by the index number
        SORT it_header   BY docidx.
        SORT it_bp_items BY docidx.
        SORT it_gl_items BY docidx.
        SORT it_cl_items BY docidx.

*       Build the header, BP items and G/L items for each document
        LOOP AT it_header ASSIGNING <fs_header1>.

          CLEAR: lv_ficadocnr1, ls_fkkko1, lt_fkkop1[],lt_fkkopk1[], lt_fkkcl1[],
                 lv_resob1, lv_resky1.

*         Docuemnt header
          MOVE-CORRESPONDING <fs_header1> TO ls_fkkko1.
          ls_fkkko1-awtyp = 'FEEC'.
          ls_fkkko1-awkey = lv_cmacdocnr1.

*         Document items
          IF <fs_header1>-clrdoc IS INITIAL.

*           BP items
            READ TABLE it_bp_items TRANSPORTING NO FIELDS
                       WITH KEY docidx = <fs_header1>-docidx BINARY SEARCH.
            lv_index1 = sy-tabix.
            LOOP AT it_bp_items ASSIGNING <fs_bp_item1> FROM lv_index1.
              CHECK <fs_bp_item1>-docidx EQ <fs_header1>-docidx.
              MOVE-CORRESPONDING <fs_bp_item1> TO lt_fkkop1.
              APPEND lt_fkkop1.
            ENDLOOP.

*           G/L items
            READ TABLE it_gl_items TRANSPORTING NO FIELDS
                       WITH KEY docidx = <fs_header1>-docidx BINARY SEARCH.
            lv_index1 = sy-tabix.
            LOOP AT it_gl_items ASSIGNING <fs_gl_item1> FROM lv_index1.
              CHECK <fs_gl_item1>-docidx EQ <fs_header1>-docidx.
              MOVE-CORRESPONDING <fs_gl_item1> TO lt_fkkopk1.
              APPEND lt_fkkopk1.
            ENDLOOP.

            IF NOT iv_massrun IS INITIAL.
*      -- LAT 21-11-2016 Inicio Dcto/2
               if lv_conta = 1.
                  loop at lt_fkkop1.
                    lt_fkkop1-BETRH = lt_fkkop1-BETRH / 2.
                    lt_fkkop1-BETRW = lt_fkkop1-BETRW / 2.
                    lt_fkkop1-BETR2 = lt_fkkop1-BETR2 / 2.
                    lt_fkkop1-BETR3 = lt_fkkop1-BETR3 / 2.
                    lt_fkkop1-SKFBT = lt_fkkop1-SKFBT / 2.
                    lt_fkkop1-SBETH = lt_fkkop1-SBETH / 2.
                    lt_fkkop1-SBETW = lt_fkkop1-SBETW / 2.
                    lt_fkkop1-SBET2 = lt_fkkop1-SBET2 / 2.
                    lt_fkkop1-SBET3 = lt_fkkop1-SBET3 / 2.
                    lt_fkkop1-AUGBT = lt_fkkop1-AUGBT / 2.
                    lt_fkkop1-AUGBS = lt_fkkop1-AUGBS / 2.
                    lt_fkkop1-AUGSK = lt_fkkop1-AUGSK / 2.
                    lt_fkkop1-FDWBT = lt_fkkop1-FDWBT / 2.
                    lt_fkkop1-QSSHB = lt_fkkop1-QSSHB / 2.
                    lt_fkkop1-QBSHB = lt_fkkop1-QBSHB / 2.
                    lt_fkkop1-SCTAX = lt_fkkop1-SCTAX / 2.
                    lt_fkkop1-STTAX = lt_fkkop1-STTAX / 2.
                    lt_fkkop1-PSWBT = lt_fkkop1-PSWBT / 2.
                    lt_fkkop1-PSWTX = lt_fkkop1-PSWTX / 2.
                    MOVE-CORRESPONDING lt_fkkop1 to lr_fkkop_v.
                    append lr_fkkop_v to lt_fkkop_v. "total del documento inicial (posición por posición)
                    modify lt_fkkop1.
                  endloop.

                  loop at lt_fkkopk1.
                     lt_fkkopk1-BETRH   = lt_fkkopk1-BETRH / 2.
                     lt_fkkopk1-BETRW   = lt_fkkopk1-BETRW / 2.
                     lt_fkkopk1-BETR2   = lt_fkkopk1-BETR2 / 2.
                     lt_fkkopk1-BETR3   = lt_fkkopk1-BETR3 / 2.
                     lt_fkkopk1-SBASH   = lt_fkkopk1-SBASH / 2.
                     lt_fkkopk1-SBASW   = lt_fkkopk1-SBASW / 2.
                     lt_fkkopk1-SCTAX   = lt_fkkopk1-SCTAX / 2.
                     MOVE-CORRESPONDING lt_fkkopk1 to lr_fkkopk_v.
                     append lr_fkkopk_v to lt_fkkopk_v.
                     modify lt_fkkopk1.
                  endloop.
               else.
                "  Date odontológico.
                  CALL FUNCTION 'HRIQ_READ_TIMELIMITS_CA'
                    EXPORTING
                      IV_PERYR                  = ls_sc_data-cmperyr
                      IV_PERID                  = ls_sc_data-cmperid
                      IV_KEYDATE                = SY-DATUM
                      IV_OTYPE                  = cl_hrpiq00const=>c_otype_sc
                      IV_OBJID                  = ls_sc_data-cmscid
                    IMPORTING
                      ET_TIMELIMITS             = lt_timelimits
                      ET_LOG_TAB                = lt_log_tab1. "Dato ya declarado!!!!!

                  read table lt_timelimits into lr_timelimits
                     with key TIMELIMIT = lc_tl_odon.
                  if sy-subrc = 0.
                     lv_fecha2_v = lr_timelimits-begda.
                  endif.

                  loop at lt_fkkop1.
                     read table lt_fkkop_v into lr_fkkop_v index sy-tabix.
                     if sy-subrc = 0.
                        lt_fkkop1-BETRH = lt_fkkop1-BETRH - lr_fkkop_v-BETRH.
                        lt_fkkop1-BETRW = lt_fkkop1-BETRW - lr_fkkop_v-BETRW.
                        lt_fkkop1-BETR2 = lt_fkkop1-BETR2 - lr_fkkop_v-BETR2.
                        lt_fkkop1-BETR3 = lt_fkkop1-BETR3 - lr_fkkop_v-BETR3.
                        lt_fkkop1-SKFBT = lt_fkkop1-SKFBT - lr_fkkop_v-SKFBT.
                        lt_fkkop1-SBETH = lt_fkkop1-SBETH - lr_fkkop_v-SBETH.
                        lt_fkkop1-SBETW = lt_fkkop1-SBETW - lr_fkkop_v-SBETW.
                        lt_fkkop1-SBET2 = lt_fkkop1-SBET2 - lr_fkkop_v-SBET2.
                        lt_fkkop1-SBET3 = lt_fkkop1-SBET3 - lr_fkkop_v-SBET3.
                        lt_fkkop1-AUGBT = lt_fkkop1-AUGBT - lr_fkkop_v-AUGBT.
                        lt_fkkop1-AUGBS = lt_fkkop1-AUGBS - lr_fkkop_v-AUGBS.
                        lt_fkkop1-AUGSK = lt_fkkop1-AUGSK - lr_fkkop_v-AUGSK.
                        lt_fkkop1-FDWBT = lt_fkkop1-FDWBT - lr_fkkop_v-FDWBT.
                        lt_fkkop1-QSSHB = lt_fkkop1-QSSHB - lr_fkkop_v-QSSHB.
                        lt_fkkop1-QBSHB = lt_fkkop1-QBSHB - lr_fkkop_v-QBSHB.
                        lt_fkkop1-SCTAX = lt_fkkop1-SCTAX - lr_fkkop_v-SCTAX.
                        lt_fkkop1-STTAX = lt_fkkop1-STTAX - lr_fkkop_v-STTAX.
                        lt_fkkop1-PSWBT = lt_fkkop1-PSWBT - lr_fkkop_v-PSWBT.
                        lt_fkkop1-PSWTX = lt_fkkop1-PSWTX - lr_fkkop_v-PSWTX.
                        if lv_fecha2_v is not initial.
                           lt_fkkop1-FAEDN = lv_fecha2_v.
                           lt_fkkop1-FAEDS = lv_fecha2_v.
                        endif.
                        modify lt_fkkop1.
                     endif.
                  endloop.

                  loop at lt_fkkopk1.
                     read table lt_fkkopk_v into lr_fkkopk_v index sy-tabix.
                     if sy-subrc = 0.
                        lt_fkkopk1-BETRH    = lt_fkkopk1-BETRH - lr_fkkopk_v-BETRH.
                        lt_fkkopk1-BETRW    = lt_fkkopk1-BETRW - lr_fkkopk_v-BETRW.
                        lt_fkkopk1-BETR2    = lt_fkkopk1-BETR2 - lr_fkkopk_v-BETR2.
                        lt_fkkopk1-BETR3    = lt_fkkopk1-BETR3 - lr_fkkopk_v-BETR3.
                        lt_fkkopk1-SBASH    = lt_fkkopk1-SBASH - lr_fkkopk_v-SBASH.
                        lt_fkkopk1-SBASW    = lt_fkkopk1-SBASW - lr_fkkopk_v-SBASW.
                        lt_fkkopk1-SCTAX    = lt_fkkopk1-SCTAX - lr_fkkopk_v-SCTAX.
                        modify lt_fkkopk1.
                     endif.
                  endloop.
              endif.
*      -- LAT 21-11-2016 Fin Dcto/2

*             Create the FI-CA document
              CALL FUNCTION 'FKK_CREATE_DOC_MASS'
                EXPORTING
                  i_fkkko       = ls_fkkko1
                IMPORTING
                  e_opbel       = lv_ficadocnr1
                TABLES
                  t_fkkop       = lt_fkkop1
                  t_fkkopk      = lt_fkkopk1
                EXCEPTIONS
                  error_message = 0.
            ELSE.
*             Read the RESOB and RESKY for the recon. key, they are needed
*             in creating the document
              CALL FUNCTION 'FKK_FIKEY_GET_STATUS'
                EXPORTING
                  i_fikey = ls_fkkko1-fikey
                IMPORTING
                  e_resob = lv_resob1
                  e_resky = lv_resky1.
*      -- LAT 21-11-2016 Inicio Dcto/2
               if lv_conta = 1.
                  loop at lt_fkkop1.
                    lt_fkkop1-BETRH = lt_fkkop1-BETRH / 2.
                    lt_fkkop1-BETRW = lt_fkkop1-BETRW / 2.
                    lt_fkkop1-BETR2 = lt_fkkop1-BETR2 / 2.
                    lt_fkkop1-BETR3 = lt_fkkop1-BETR3 / 2.
                    lt_fkkop1-SKFBT = lt_fkkop1-SKFBT / 2.
                    lt_fkkop1-SBETH = lt_fkkop1-SBETH / 2.
                    lt_fkkop1-SBETW = lt_fkkop1-SBETW / 2.
                    lt_fkkop1-SBET2 = lt_fkkop1-SBET2 / 2.
                    lt_fkkop1-SBET3 = lt_fkkop1-SBET3 / 2.
                    lt_fkkop1-AUGBT = lt_fkkop1-AUGBT / 2.
                    lt_fkkop1-AUGBS = lt_fkkop1-AUGBS / 2.
                    lt_fkkop1-AUGSK = lt_fkkop1-AUGSK / 2.
                    lt_fkkop1-FDWBT = lt_fkkop1-FDWBT / 2.
                    lt_fkkop1-QSSHB = lt_fkkop1-QSSHB / 2.
                    lt_fkkop1-QBSHB = lt_fkkop1-QBSHB / 2.
                    lt_fkkop1-SCTAX = lt_fkkop1-SCTAX / 2.
                    lt_fkkop1-STTAX = lt_fkkop1-STTAX / 2.
                    lt_fkkop1-PSWBT = lt_fkkop1-PSWBT / 2.
                    lt_fkkop1-PSWTX = lt_fkkop1-PSWTX / 2.
                    MOVE-CORRESPONDING lt_fkkop1 to lr_fkkop_v.
                    append lr_fkkop_v to lt_fkkop_v. "total del documento inicial (posición por posición)
                    modify lt_fkkop1.
                  endloop.

                  loop at lt_fkkopk1.
                     lt_fkkopk1-BETRH   = lt_fkkopk1-BETRH / 2.
                     lt_fkkopk1-BETRW   = lt_fkkopk1-BETRW / 2.
                     lt_fkkopk1-BETR2   = lt_fkkopk1-BETR2 / 2.
                     lt_fkkopk1-BETR3   = lt_fkkopk1-BETR3 / 2.
                     lt_fkkopk1-SBASH   = lt_fkkopk1-SBASH / 2.
                     lt_fkkopk1-SBASW   = lt_fkkopk1-SBASW / 2.
                     lt_fkkopk1-SCTAX   = lt_fkkopk1-SCTAX / 2.
                     MOVE-CORRESPONDING lt_fkkopk1 to lr_fkkopk_v.
                     append lr_fkkopk_v to lt_fkkopk_v.
                     modify lt_fkkopk1.
                  endloop.

               else.
                  "  Date odontológico.
                  CALL FUNCTION 'HRIQ_READ_TIMELIMITS_CA'
                    EXPORTING
                      IV_PERYR                  = ls_sc_data-cmperyr
                      IV_PERID                  = ls_sc_data-cmperid
                      IV_KEYDATE                = SY-DATUM
                      IV_OTYPE                  = cl_hrpiq00const=>c_otype_sc
                      IV_OBJID                  = ls_sc_data-cmscid
                    IMPORTING
                      ET_TIMELIMITS             = lt_timelimits
                      ET_LOG_TAB                = lt_log_tab1. "Dato ya declarado!!!!!

                  read table lt_timelimits into lr_timelimits
                     with key TIMELIMIT = lc_tl_odon.
                  if sy-subrc = 0.
                     lv_fecha2_v = lr_timelimits-begda.
                  endif.

                  loop at lt_fkkop1.
                     read table lt_fkkop_v into lr_fkkop_v index sy-tabix.
                     if sy-subrc = 0.
                        lt_fkkop1-BETRH = lt_fkkop1-BETRH - lr_fkkop_v-BETRH.
                        lt_fkkop1-BETRW = lt_fkkop1-BETRW - lr_fkkop_v-BETRW.
                        lt_fkkop1-BETR2 = lt_fkkop1-BETR2 - lr_fkkop_v-BETR2.
                        lt_fkkop1-BETR3 = lt_fkkop1-BETR3 - lr_fkkop_v-BETR3.
                        lt_fkkop1-SKFBT = lt_fkkop1-SKFBT - lr_fkkop_v-SKFBT.
                        lt_fkkop1-SBETH = lt_fkkop1-SBETH - lr_fkkop_v-SBETH.
                        lt_fkkop1-SBETW = lt_fkkop1-SBETW - lr_fkkop_v-SBETW.
                        lt_fkkop1-SBET2 = lt_fkkop1-SBET2 - lr_fkkop_v-SBET2.
                        lt_fkkop1-SBET3 = lt_fkkop1-SBET3 - lr_fkkop_v-SBET3.
                        lt_fkkop1-AUGBT = lt_fkkop1-AUGBT - lr_fkkop_v-AUGBT.
                        lt_fkkop1-AUGBS = lt_fkkop1-AUGBS - lr_fkkop_v-AUGBS.
                        lt_fkkop1-AUGSK = lt_fkkop1-AUGSK - lr_fkkop_v-AUGSK.
                        lt_fkkop1-FDWBT = lt_fkkop1-FDWBT - lr_fkkop_v-FDWBT.
                        lt_fkkop1-QSSHB = lt_fkkop1-QSSHB - lr_fkkop_v-QSSHB.
                        lt_fkkop1-QBSHB = lt_fkkop1-QBSHB - lr_fkkop_v-QBSHB.
                        lt_fkkop1-SCTAX = lt_fkkop1-SCTAX - lr_fkkop_v-SCTAX.
                        lt_fkkop1-STTAX = lt_fkkop1-STTAX - lr_fkkop_v-STTAX.
                        lt_fkkop1-PSWBT = lt_fkkop1-PSWBT - lr_fkkop_v-PSWBT.
                        lt_fkkop1-PSWTX = lt_fkkop1-PSWTX - lr_fkkop_v-PSWTX.
                        if lv_fecha2_v is not initial.
                           lt_fkkop1-FAEDN = lv_fecha2_v.
                           lt_fkkop1-FAEDS = lv_fecha2_v.
                        endif.
                        modify lt_fkkop1.
                     endif.
                  endloop.

                  loop at lt_fkkopk1.
                     read table lt_fkkopk_v into lr_fkkopk_v index sy-tabix.
                     if sy-subrc = 0.
                        lt_fkkopk1-BETRH    = lt_fkkopk1-BETRH - lr_fkkopk_v-BETRH.
                        lt_fkkopk1-BETRW    = lt_fkkopk1-BETRW - lr_fkkopk_v-BETRW.
                        lt_fkkopk1-BETR2    = lt_fkkopk1-BETR2 - lr_fkkopk_v-BETR2.
                        lt_fkkopk1-BETR3    = lt_fkkopk1-BETR3 - lr_fkkopk_v-BETR3.
                        lt_fkkopk1-SBASH    = lt_fkkopk1-SBASH - lr_fkkopk_v-SBASH.
                        lt_fkkopk1-SBASW    = lt_fkkopk1-SBASW - lr_fkkopk_v-SBASW.
                        lt_fkkopk1-SCTAX    = lt_fkkopk1-SCTAX - lr_fkkopk_v-SCTAX.
                        modify lt_fkkopk1.
                     endif.
                  endloop.
               endif.
*      -- LAT 21-11-2016 Fin Dcto/2
*             Create the FI-CA document
              CALL FUNCTION 'FKK_CREATE_DOC'
                EXPORTING
                  i_fkkko       = ls_fkkko1
                  i_update_task = iv_update_task
                  i_resob       = lv_resob1
                  i_resky       = lv_resky1
                IMPORTING
                  e_opbel       = lv_ficadocnr1
                TABLES
                  t_fkkop       = lt_fkkop1
                  t_fkkopk      = lt_fkkopk1
                EXCEPTIONS
                  error_message = 0.
            ENDIF.

            IF lv_ficadocnr1 IS INITIAL.
*             Get the message raised
              CALL FUNCTION 'CMAC_ERROR_HANDLE'
                EXPORTING
                  iv_otype     = c_student
                  iv_objabbr   = <fs_header1>-cmstnum
                  iv_subobject = c_log_sub_feepost
                TABLES
                  et_error     = ct_log_tab.
              lv_error1 = 'X'. EXIT.
            ELSE.
              MESSAGE s323 WITH lv_ficadocnr1 <fs_header1>-cmstnum
                           INTO lv_dummy1.
              CALL FUNCTION 'CMAC_ERROR_HANDLE'
                EXPORTING
                  iv_otype     = c_student
                  iv_objabbr   = <fs_header1>-cmstnum
                  iv_subobject = c_log_sub_feepost
                TABLES
                  et_error     = lt_log_tab1.
*             Add it to the mapping table
              lt_doc_index1-opbel  = lv_ficadocnr1.
              lt_doc_index1-docidx = <fs_header1>-docidx.
              APPEND lt_doc_index1.
*             Update data for fee calculation document
              ls_ficadoc1-docnr = lv_cmacdocnr1.
              ls_ficadoc1-opbel = lv_ficadocnr1.
              ls_ficadoc1-xnew  = 'X'.
              APPEND ls_ficadoc1 TO lt_ficadoc1.

              "Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
              "Log de documento adicional en Odontologicos para calculo de Doc Ref. en Deltas
              CASE lv_conta.
                WHEN 1.
                  ls_fica_odont1-docnr = ls_ficadoc1-docnr.
                  ls_fica_odont1-opbel = ls_ficadoc1-opbel.
                  ls_fica_odont1-xnew = ls_ficadoc1-xnew.
                  ls_fica_odont1-secuencia_odont = lv_secuencia_odont = lv_conta.
                WHEN 2.
                  ls_fica_odont2-docnr = ls_ficadoc1-docnr.
                  ls_fica_odont2-opbel = ls_ficadoc1-opbel.
                  ls_fica_odont2-xnew = ls_ficadoc1-xnew.
                  ls_fica_odont2-secuencia_odont = lv_secuencia_odont = lv_conta.
              ENDCASE.
              "Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
            ENDIF.

          ELSE.

*           BP items for clearing
            READ TABLE it_cl_items TRANSPORTING NO FIELDS
                        WITH KEY docidx = <fs_header1>-docidx BINARY SEARCH.
            lv_index1 = sy-tabix.
            LOOP AT it_cl_items ASSIGNING <fs_cl_item1> FROM lv_index1.
              CHECK <fs_cl_item1>-docidx EQ <fs_header1>-docidx.
              READ TABLE lt_doc_index1 WITH KEY
                                        docidx = <fs_cl_item1>-docidx_clr.
              MOVE-CORRESPONDING <fs_cl_item1> TO lt_fkkcl1.
              lt_fkkcl1-opbel   = lt_doc_index1-opbel.
              APPEND lt_fkkcl1.
              MOVE-CORRESPONDING <fs_cl_item1> TO lt_ienqtab1.
              lt_ienqtab1-uname = sy-uname.
              CLEAR: lt_ienqtab1-vkont.
              APPEND lt_ienqtab1.
            ENDLOOP.

*           Lock the items by BP and company code for clearing
            CALL FUNCTION 'FKK_OPEN_ITEM_ENQUEUE'
              EXPORTING
                i_shared = 'X'
                i_scope  = '3'
              TABLES
                t_enqtab = lt_ienqtab1.

            IF NOT iv_massrun IS INITIAL.
*             Create the document and clear off immediately
              CALL FUNCTION 'FKK_CREATE_DOC_MASS_AND_CLEAR'
                EXPORTING
                  i_fkkko       = ls_fkkko1
                IMPORTING
                  e_opbel       = lv_ficadocnr1
                TABLES
                  t_fkkcl       = lt_fkkcl1
                EXCEPTIONS
                  error_message = 0.
            ELSE.
*             Read the RESOB and RESKY for the recon. key, they are needed
*             in creating the document
              CALL FUNCTION 'FKK_FIKEY_GET_STATUS'
                EXPORTING
                  i_fikey = ls_fkkko1-fikey
                IMPORTING
                  e_resob = lv_resob1
                  e_resky = lv_resky1.
*             Create the document and clear off immediately
              CALL FUNCTION 'FKK_CREATE_DOC_AND_CLEAR'
                EXPORTING
                  i_fkkko       = ls_fkkko1
                  i_update_task = iv_update_task
                  i_resob       = lv_resob1
                  i_resky       = lv_resky1
                IMPORTING
                  e_opbel       = lv_ficadocnr1
                TABLES
                  t_fkkcl       = lt_fkkcl1
                EXCEPTIONS
                  error_message = 0.
            ENDIF.

            IF lv_ficadocnr1 IS INITIAL.
*             Get the message raised
              CALL FUNCTION 'CMAC_ERROR_HANDLE'
                EXPORTING
                  iv_otype     = c_student
                  iv_objabbr   = <fs_header1>-cmstnum
                  iv_subobject = c_log_sub_feepost
                TABLES
                  et_error     = ct_log_tab.
              lv_error1 = 'X'. EXIT.
            ELSE.
              MESSAGE s323 WITH lv_ficadocnr1 <fs_header1>-cmstnum
                           INTO lv_dummy1.
              CALL FUNCTION 'CMAC_ERROR_HANDLE'
                EXPORTING
                  iv_otype     = c_student
                  iv_objabbr   = <fs_header1>-cmstnum
                  iv_subobject = c_log_sub_feepost
                TABLES
                  et_error     = lt_log_tab1.
*             Add it to the mapping table
              lt_doc_index1-opbel  = lv_ficadocnr1.
              lt_doc_index1-docidx = it_header-docidx.
              APPEND lt_doc_index1.
*             Update data for fee calculation document
              ls_ficadoc1-docnr = lv_cmacdocnr1.
              ls_ficadoc1-opbel = lv_ficadocnr1.
              ls_ficadoc1-xnew  = 'X'.
              APPEND ls_ficadoc1 TO lt_ficadoc1.
              "Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
              "Log de documento adicional en Odontologicos para calculo de Doc Ref. en Deltas
              CASE lv_conta.
                WHEN 1.
                  ls_fica_odont1-docnr = ls_ficadoc1-docnr.
                  ls_fica_odont1-opbel = ls_ficadoc1-opbel.
                  ls_fica_odont1-xnew = ls_ficadoc1-xnew.
                  ls_fica_odont1-secuencia_odont = lv_conta.
                WHEN 2.
                  ls_fica_odont2-docnr = ls_ficadoc1-docnr.
                  ls_fica_odont2-opbel = ls_ficadoc1-opbel.
                  ls_fica_odont2-xnew = ls_ficadoc1-xnew.
                  ls_fica_odont2-secuencia_odont = lv_conta.
              ENDCASE.
              "Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
            ENDIF.

*           Release the lock
            CALL FUNCTION 'FKK_OPEN_ITEM_DEQUEUE'.

          ENDIF.

        ENDLOOP.

* Rollback when anything wrong
        IF NOT lv_error1 IS INITIAL.
           ROLLBACK WORK.
           EXIT.
        ENDIF.

* Create fee calculation document
        CALL FUNCTION 'CMAC_RFC_FEEDOC_CREATE'
          EXPORTING
            is_st_data        = is_st_data
            is_fee_control    = is_fee_control
            it_fee_tree       = it_fee_tree[]
            it_sc_data        = it_sc_data[]
            it_sm_data        = it_sm_data[]
            it_ficadoc        = lt_ficadoc1[]
            it_itemres        = it_feedoc_ad[]
            it_manl_fee       = it_manl_fee[]
            iv_update_task    = iv_update_task
            iv_check_auth     = 'X'
          CHANGING
            cv_docnr          = lv_cmacdocnr1
          EXCEPTIONS
            no_authorization  = 1
            customizing_error = 2
            number_not_found  = 3
            db_update_error   = 5
            OTHERS            = 6.
        IF sy-subrc <> 0.
           CALL FUNCTION 'CMAC_ERROR_HANDLE'
             EXPORTING
               iv_otype     = c_student
               iv_objabbr   = is_st_data-cmstnum
               iv_subobject = c_log_sub_feepost
             TABLES
               et_error     = ct_log_tab.
               lv_error1 = 'X'.
        ENDIF.


* Rollback when anything wrong
        IF NOT lv_error1 IS INITIAL.
           ROLLBACK WORK.
           EXIT.
        ENDIF.

*--Trigger BRF event
        PERFORM trigger_brf_event USING  it_calc_fees
                                         is_st_data
                                         lv_cmacdocnr1
                                         is_fee_control-pmode
                               CHANGING  ct_log_tab[].


* Everything is ok, commit and return
        "Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
        "Log de documento adicional en Odontologicos para calculo de Doc Ref. en Deltas
        CALL FUNCTION 'Z_EDU_ACT_CMACDB_FEEF' IN UPDATE TASK
          EXPORTING
            i_cmacdb_feefica       = ls_fica_odont1.

        CALL FUNCTION 'Z_EDU_ACT_CMACDB_FEEF' IN UPDATE TASK
          EXPORTING
            i_cmacdb_feefica       = ls_fica_odont2.
        "Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 21/02/2017
        COMMIT WORK.
        ev_feedoc_nr = lv_cmacdocnr1.
        APPEND LINES OF lt_log_tab1 TO ct_log_tab.

      enddo.
      exit.
    endif. "ST Primer nivel de odontológico
 endif."Fin categoría odontológico.
*--&LAT 18-11-2016 001 FIN: Odontológicos
ENDENHANCEMENT.
