class ZWDC_SOL_CERTIF_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  types:
    gty_estado_cs TYPE n LENGTH 1 .
  types:
    BEGIN OF gty_mc_peryrperid,
        peryrperid  TYPE piqperyrperid,
        peryrperidt TYPE zedu_peryrperidt,
      END OF gty_mc_peryrperid .
  types:
    gtyt_mc_peryrperid TYPE TABLE OF gty_mc_peryrperid .
  types:
    BEGIN OF gty_opbel_fac,
      opbel TYPE opbel_kk,
    END OF gty_opbel_fac .
  types:
    BEGIN OF gty_opbel_xblnr,
      opbel TYPE opbel_kk,
      xblnr TYPE xblnr_kk,
    END OF gty_opbel_xblnr .
  types:
    gtt_opbel_fac TYPE TABLE OF gty_opbel_fac .
  types:
    gtt_opbel_xblnr TYPE TABLE OF gty_opbel_xblnr .

  data GV_TIPO_ESTUDIANTE type PIQPDISCT2 .
  data GV_OPBEL type OPBEL_KK .
  data GV_ESTADO_CS type GTY_ESTADO_CS .
  constants GC_ESTADO_CS_ADMITIDO type GTY_ESTADO_CS value 1 ##NO_TEXT.
  constants GC_ESTADO_CS_ACTIVO type GTY_ESTADO_CS value 2 ##NO_TEXT.
  constants GC_ESTADO_CS_GRADUADO type GTY_ESTADO_CS value 3 ##NO_TEXT.
  constants GC_ESTADO_CS_RETIRADO type GTY_ESTADO_CS value 4 ##NO_TEXT.
  data GT_OPBEL_FAC type GTT_OPBEL_FAC .
  data GT_OPBEL_XBLNR type GTT_OPBEL_XBLNR .

  methods GET_PROGRAMAS_ESTUDIANTE
    importing
      !IV_STOBJID type HROBJID
    returning
      value(RT_PROGRAMAS) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_TIPO_CERTIFICADOS
    importing
      !IV_PROGRAMA type HROBJID
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_RECIBO_PAGO
    importing
      value(IT_PERYRPERID) type ZEDU_R_PERYRPERID_TAB
      !IV_ESTUDIANTE type HROBJID
      !IV_PROGRAMA type HROBJID
      !IV_SIMULADO type XFELD optional
    exporting
      !ET_ERROR type HRIQ_ERROR_STRUCTURE
    returning
      value(RT_TARIFA) type CHAR20 .
  methods GET_CERTIFICADO
    importing
      !IV_PROGRAMA type HROBJID
      !IV_ESTUDIANTE type HROBJID
      !IV_TIPO_CERT type STRING
    returning
      value(RT_CERTIFICADO) type CHAR20 .
  methods PAGO_ONLINE
    returning
      value(RT_URL) type STRING .
  methods GET_PERYRPERID_PROGRAMA
    importing
      !IV_OBJID type HROBJID
    exporting
      value(ET_MC_PERYRPERID) type GTYT_MC_PERYRPERID .
protected section.
private section.

  types:
    BEGIN OF gty_hrp1001,
      otype   TYPE hrp1001-otype,
      objid   TYPE hrp1001-objid,
      plvar   TYPE hrp1001-plvar,
      rsign   TYPE hrp1001-rsign,
      relat   TYPE hrp1001-relat,
      istat   TYPE hrp1001-istat,
      priox   TYPE hrp1001-priox,
      begda   TYPE hrp1001-begda,
      endda   TYPE hrp1001-endda,
      varyf   TYPE hrp1001-varyf,
      seqnr   TYPE hrp1001-seqnr,
      subty   TYPE hrp1001-subty,
      sclas   TYPE hrp1001-sclas,
      sobid   TYPE hrp1001-sobid,
      adatanr TYPE hrp1001-adatanr,
    END OF gty_hrp1001 .
  types:
    BEGIN OF gty_hrpad530,
      adatanr      TYPE hrpad530-adatanr,
      adm_ayear    TYPE hrpad530-adm_ayear,
      adm_perid    TYPE hrpad530-adm_perid,
      adm_enrcateg TYPE hrpad530-adm_enrcateg,
    END OF gty_hrpad530 .
  types:
    BEGIN OF gty_hrp1769,
      plvar       TYPE plvar,
      otype       TYPE otype,
      objid       TYPE hrobjid,
      subty       TYPE subtyp,
      istat       TYPE istat_d,
      begda       TYPE begdatum,
      endda       TYPE enddatum,
      varyf	      TYPE varyf,
      seqnr       TYPE seqnr,
      beg_process TYPE piqprocess_beg,
      end_process TYPE piqprocess_end,
      end_reason  TYPE piqderreason,
    END OF gty_hrp1769 .
  types:
    BEGIN OF gty_hrp1771,
      plvar    TYPE plvar,
      otype    TYPE otype,
      objid    TYPE hrobjid,
      subty    TYPE subtyp,
      istat    TYPE istat_d,
      begda    TYPE begdatum,
      endda    TYPE enddatum,
      varyf    TYPE varyf,
      seqnr    TYPE seqnr,
      ayear    TYPE hrp1771-ayear,
      perid    TYPE hrp1771-perid,
      regdate  TYPE piqregperdate,
      cancdate TYPE piqregcancdate,
    END OF gty_hrp1771 .
  types:
    BEGIN OF gty_tipo_cert.
          INCLUDE TYPE wdr_context_attr_value.
  TYPES:
    admitido TYPE zedu_admitido,
    activo   TYPE zedu_activo,
    graduado TYPE zedu_graduado,
    retirado TYPE zedu_retirado,
    END OF gty_tipo_cert .
  types:
    gtt_hrp1001  TYPE TABLE OF gty_hrp1001 .
  types:
    gtt_hrpad530 TYPE TABLE OF gty_hrpad530 .
  types:
    gtt_hrp1769 TYPE TABLE OF gty_hrp1769 .
  types:
    gtt_hrp1771 TYPE TABLE OF gty_hrp1771 .
  types:
    gtt_zedu_certif TYPE TABLE OF zedu_certif .
  types:
    gtt_tipo_cert TYPE TABLE OF gty_tipo_cert .

  data GT_HRP1001 type GTT_HRP1001 .
  data GT_HRPAD530 type GTT_HRPAD530 .
  data GT_HRP1769 type GTT_HRP1769 .
  data GT_HRP1771 type GTT_HRP1771 .
  data GT_TIPO_CERT type GTT_TIPO_CERT .

  methods SET_ESTADO_ESTUDIANTE
    importing
      !IV_PROGRAMA type HROBJID .
ENDCLASS.



CLASS ZWDC_SOL_CERTIF_ASS IMPLEMENTATION.


  METHOD get_certificado.


    DATA :
      ls_hrp1706    TYPE hrp1706.

    FIELD-SYMBOLS:
      <fs_hrp1001>  TYPE gty_hrp1001,
      <fs_hrpad530> TYPE gty_hrpad530.

    READ TABLE gt_hrp1001 ASSIGNING <fs_hrp1001>
      WITH KEY sobid = iv_programa
      BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.
    READ TABLE gt_hrpad530 ASSIGNING <fs_hrpad530>
      WITH KEY adatanr = <fs_hrp1001>-adatanr
      BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    CASE <fs_hrpad530>-adm_enrcateg.
      WHEN '01' OR '02' OR '03' OR '04' OR '05' OR '06' OR '07' OR '08' OR '09' OR '10'.
        gv_tipo_estudiante = 'O010'. " General
      WHEN '11' OR '12' OR '13' OR '14' OR '15' OR '16' OR '17' .
        gv_tipo_estudiante = 'O011'. " Becado
    ENDCASE.


*    SELECT SINGLE *
*      FROM hrp1706
*      INTO ls_hrp1706
*      WHERE plvar = '01'
*        AND otype = 'CS'
*        AND objid = iv_estudiante.
*
*    IF sy-subrc IS INITIAL.
*      IF ls_hrp1706-pdisct2 IS INITIAL.
*        ls_hrp1706-pdisct2 = gv_tipo_estudiante.
*      ELSEIF ls_hrp1706-pdisct3 IS INITIAL.
*        ls_hrp1706-pdisct3 = gv_tipo_estudiante.
*      ELSEIF ls_hrp1706-pdisct4 IS INITIAL.
*        ls_hrp1706-pdisct4 = gv_tipo_estudiante.
*      ENDIF.
*
*      MODIFY hrp1706 FROM ls_hrp1706.
*    ENDIF.


***    SELECT SINGLE knumh
***      FROM a501
***      INTO ls_knumh
***      WHERE kappl  = 'CM'
***        AND kschl  = gv_tipo_estudiante
***        AND cmscid = iv_programa
***        AND kfrst  = space
***        AND datbi >= sy-datum
***        AND datab <= sy-datum.
***
***    SELECT SINGLE kbetr
***      FROM konp
***      INTO rt_tarifa
***      WHERE knumh = ls_knumh.


  ENDMETHOD.


  METHOD get_peryrperid_programa.

    "Declaraciones
    DATA:
      ls_attr_value	   TYPE wdr_context_attr_value,
      ls_mc_peryrperid TYPE gty_mc_peryrperid,
      lt_hrp1771       TYPE gtt_hrp1771.

    "Field-Symbols
    FIELD-SYMBOLS:
      <fs_hrp1001>  TYPE gty_hrp1001,
      <fs_hrpad530> TYPE gty_hrpad530,
      <fs_hrp1771>  TYPE gty_hrp1771.


    "Obtiene el estado del estudio
    me->set_estado_estudiante( iv_objid ).

    "Determina la accion segun el estado del estudio
    CASE gv_estado_cs.
      WHEN gc_estado_cs_admitido. "ADMITIDO
        "Obtiene el registro de relacion ST - CS
        READ TABLE gt_hrp1001 ASSIGNING <fs_hrp1001>
          WITH KEY sobid = iv_objid
          BINARY SEARCH.
        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Obtiene el registro de admision
          READ TABLE gt_hrpad530 ASSIGNING <fs_hrpad530>
            WITH KEY adatanr = <fs_hrp1001>-adatanr
            BINARY SEARCH.
          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Asigna y crea el registro con el año y periodo
            CONCATENATE <fs_hrpad530>-adm_ayear
                        <fs_hrpad530>-adm_perid
              INTO ls_mc_peryrperid-peryrperid
              SEPARATED BY '-'.
            APPEND ls_mc_peryrperid TO et_mc_peryrperid.
          ENDIF.
        ENDIF.

      WHEN gc_estado_cs_activo.   "ACTIVO
        "Crea una copia de las matriculas
        lt_hrp1771 = gt_hrp1771.
        "Elimina los registros que no pertenecen al estudio
        DELETE lt_hrp1771
          WHERE objid NE iv_objid.
        "Recorre los registros de matricula del estudio
        LOOP AT lt_hrp1771 ASSIGNING <fs_hrp1771>.
          "Asigna y crea el registro con el año y periodo
          CONCATENATE <fs_hrp1771>-ayear
                      <fs_hrp1771>-perid
            INTO ls_mc_peryrperid-peryrperid
            SEPARATED BY '-'.
          APPEND ls_mc_peryrperid TO et_mc_peryrperid.
        ENDLOOP.

      WHEN gc_estado_cs_graduado OR "GRADUADO
           gc_estado_cs_retirado.   "RETIRADO
        "Crea una copia de las matriculas
        lt_hrp1771 = gt_hrp1771.
        "Elimina los registros que no pertenecen al estudio
        DELETE lt_hrp1771
          WHERE objid NE iv_objid.
        "Ordena los registros dejando de primero la matricula mas reciente
        SORT lt_hrp1771 BY endda DESCENDING begda DESCENDING regdate DESCENDING.
        "Si se tienen datos de matricula
        IF NOT lt_hrp1771 IS INITIAL.
          "Obtiene el primer registro que equivale a la ultima matricula
          READ TABLE lt_hrp1771 ASSIGNING <fs_hrp1771> INDEX 1.
          "Asigna y crea el registro con el año y periodo
          CONCATENATE <fs_hrp1771>-ayear
                      <fs_hrp1771>-perid
            INTO ls_mc_peryrperid-peryrperid
            SEPARATED BY '-'.
          APPEND ls_mc_peryrperid TO et_mc_peryrperid.

          "Si no se tienen datos de matricula
        ELSE.
          "Obtiene el registro de relacion ST - CS
          READ TABLE gt_hrp1001 ASSIGNING <fs_hrp1001>
            WITH KEY sobid = iv_objid
            BINARY SEARCH.
          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Obtiene el registro de admision
            READ TABLE gt_hrpad530 ASSIGNING <fs_hrpad530>
              WITH KEY adatanr = <fs_hrp1001>-adatanr
              BINARY SEARCH.
            "Si encuentra el registro
            IF sy-subrc EQ 0.
              "Asigna y crea el registro con el año y periodo
              CONCATENATE <fs_hrpad530>-adm_ayear
                          <fs_hrpad530>-adm_perid
                INTO ls_mc_peryrperid-peryrperid
                SEPARATED BY '-'.
              APPEND ls_mc_peryrperid TO et_mc_peryrperid.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD get_programas_estudiante.

    "Declaraciones
    DATA:
      lr_otjid  TYPE RANGE OF otjid,
      lrs_otjid LIKE LINE OF lr_otjid,
      lv_otjid  TYPE otjid,
      lv_tabix  TYPE sy-tabix.

    "Field-Symbols
    FIELD-SYMBOLS:
      <fs_hrp1001> TYPE gty_hrp1001.


    "Inicializa el retorno y declaraciones
    CLEAR:
      rt_programas,
      gt_hrp1001,
      gt_hrpad530,
      gt_hrp1771,
      gt_hrp1769.

    "Asigna el estudiante
    lv_otjid+0(2) = 'ST'.
    lv_otjid+2(8) = iv_stobjid.

    "Obtiene las relaciones ST - CS
    SELECT otype objid plvar rsign relat istat priox begda
           endda varyf seqnr subty sclas sobid adatanr
      FROM hrp1001
      INTO TABLE gt_hrp1001
      WHERE otjid EQ lv_otjid
        AND subty EQ 'A530'
        AND sclas EQ 'CS'
        AND plvar EQ '01'
        AND istat EQ '1'.

    "Ordena los registros por programa
    SORT gt_hrp1001 BY sobid.

    "Valida que se tengan datos
    CHECK NOT gt_hrp1001 IS INITIAL.

    "Obtiene las clases de oyentes de los estudios encontrados
    SELECT adatanr adm_ayear adm_perid adm_enrcateg
      FROM hrpad530
      INTO TABLE gt_hrpad530
      FOR ALL ENTRIES IN gt_hrp1001
      WHERE adatanr = gt_hrp1001-adatanr.

    "Ordena las clases de oyentes de los estudios
    SORT gt_hrpad530 BY adatanr.

    "Indica que los estudios son incluyentes
    lrs_otjid = 'IEQ'.

    "Recorre los estudios encontrados
    LOOP AT gt_hrp1001 ASSIGNING <fs_hrp1001>.
      "Almacena el indice
      lv_tabix = sy-tabix.

      "Valida si el estudio es valido
      READ TABLE gt_hrpad530 TRANSPORTING NO FIELDS
        WITH KEY adatanr = <fs_hrp1001>-adatanr
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el estudio
        lrs_otjid-low = <fs_hrp1001>-varyf.
        APPEND lrs_otjid TO lr_otjid.

        "Si no encuentra el registro
      ELSE.
        "Elimina la relacion ST - CS
        DELETE gt_hrp1001 INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

    "Continua solo si se tienen datos
    CHECK NOT lr_otjid[] IS INITIAL.

    "Obtiene las descripciones de los programas y arma tabla de salida
    SELECT objid AS value
           stext AS text
      FROM hrp1000
      INTO TABLE rt_programas
      WHERE otjid IN lr_otjid
        AND plvar EQ '01'.

    "Obtiene los periodos matriculados
    SELECT plvar otype objid subty istat begda endda
           varyf seqnr ayear perid regdate cancdate
      FROM hrp1771
      INTO TABLE gt_hrp1771
      WHERE otjid IN lr_otjid
        AND plvar EQ '01'.

    "Elimina los periodos cancelados
    DELETE gt_hrp1771
      WHERE NOT cancdate IS INITIAL.

    "Ordena los registros
    SORT gt_hrp1771 BY objid ASCENDING endda DESCENDING begda DESCENDING regdate DESCENDING.

    "Obtiene los segmentos de matricula
    SELECT plvar otype objid subty istat begda endda
           varyf seqnr beg_process end_process end_reason
      FROM hrp1769
      INTO TABLE gt_hrp1769
      WHERE otjid IN lr_otjid
        AND plvar EQ '01'.

    "Ordena los registros
    SORT gt_hrp1769 BY objid ASCENDING endda DESCENDING begda DESCENDING.

  ENDMETHOD.


  METHOD get_recibo_pago.

    "Declaraciones
    DATA:
      lv_otjid       TYPE otjid,
      lv_fact_key_sc TYPE zedu_fact_key_sc,
      lv_anio	       TYPE piqperyr,
      lv_periodo     TYPE piqperid,
      lv_first       TYPE abap_bool,
      lv_opbel       TYPE opbel_kk,
      lv_xblnr       TYPE xblnr_kk,
      lv_tarifa      TYPE char20,
      lv_tarifa_val  TYPE cmac_docamt,
      lv_tarifa_tval TYPE cmac_docamt,
      ls_peryrperid  TYPE zedu_r_peryrperid,
      ls_9118        TYPE p9118,
      ls_opbel_fac   TYPE gty_opbel_fac,
      lt_9118        TYPE TABLE OF p9118,
      lt_object      TYPE TABLE OF hrobject,              "Tabla hrobject
      lt_error       TYPE hriq_error_structure,
      lr_object      TYPE hrobject.                       "Rec. hrobject


    "Inicializa declaraciones
    CLEAR:
      gv_opbel,
      gt_opbel_fac.

    "Obtiene el codigo SC del programa
    "haciendo uso parcial del indice 3 - Índice SOBID/SCLAS/SUBTY/PLVAR/ENDDA
    SELECT SINGLE objid otjid
      INTO (lv_fact_key_sc-objid_sc, lv_otjid)
      FROM hrp1001
      WHERE sobid EQ iv_programa
        AND sclas EQ 'CS'
        AND subty EQ 'B514'
        AND plvar EQ '01'.

    "Continua solo si se tienen datos
    CHECK sy-subrc EQ 0.

    "Obtiene la linea educativa del programa
    SELECT SINGLE linea_edu
      INTO lv_fact_key_sc-linea_edu
      FROM hrp1730
      WHERE plvar EQ '01'
        AND otjid EQ lv_otjid.

    "Continua solo si se tienen datos
    CHECK sy-subrc EQ 0.

    "Recorre los periodos seleccionados
    LOOP AT it_peryrperid INTO ls_peryrperid.
      "Cuando es el primer registro
      AT FIRST.
        "indica que es el primer registro
        lv_first = abap_true.
      ENDAT.

      "Inicializa declaraciones
      CLEAR:
        lv_anio,
        lv_periodo,
        lv_opbel,
        lv_tarifa,
        lv_tarifa_val,
        lt_error.

      "Obtiene el año y el periodo
      SPLIT ls_peryrperid-low AT '-'
        INTO lv_anio
             lv_periodo.

      "Llamado a variables de memoria
      lv_fact_key_sc-tifa = 'O'.
      lv_fact_key_sc-ayear = lv_anio.
      lv_fact_key_sc-perid = lv_periodo.

      CALL FUNCTION 'Z_EDU_SET_FACT_KEY_SC'
        EXPORTING
          i_fact_key_sc = lv_fact_key_sc.

      CLEAR: ls_9118.
      REFRESH: lt_9118.

      ls_9118-otype = cl_hrpiq00const=>c_otype_st.
      ls_9118-begda = sy-datum.
      ls_9118-endda = cl_hrpiq00const=>c_date_highdate.
      ls_9118-plvar = cl_hrpiq00const=>c_plvar_active.
      ls_9118-objid = iv_estudiante.
      ls_9118-infty =  '9118'.
      ls_9118-begda = sy-datum.
      ls_9118-istat = '1'.
      ls_9118-aedtm = sy-datum.
      ls_9118-uname = sy-uname.
      ls_9118-descuento_recargo_3 = gv_tipo_estudiante. "'O010'. "Certificado

      APPEND ls_9118 TO lt_9118.

      CALL FUNCTION 'RH_INSERT_INFTY'
        EXPORTING
*         FCODE               = 'INSE'
          vtask               = 'D'
*         ORDER_FLG           = 'X'
*         COMMIT_FLG          = 'X'
*         AUTHY               = 'X'
*         PPPAR_IMP           =
*         OLD_TABNR           = ' '
          repid               = sy-repid
          form                = 'FETCH_9118'
*         KEEP_LUPD           =
*         WORKF_ACTV          = 'X'
        TABLES
          innnn               = lt_9118
*         ILFCODE             =
        EXCEPTIONS
          no_authorization    = 1
          error_during_insert = 2
          repid_form_initial  = 3
          corr_exit           = 4
          begda_greater_endda = 5
          OTHERS              = 6.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CLEAR et_error.

      "Si no es el primer periodo
      IF lv_first IS INITIAL.
        "Asigna el numero del primer documento
        lv_xblnr = gv_opbel.
        "Asigna los ceros a la izquierda
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_xblnr
          IMPORTING
            output = lv_xblnr.
        "Asigna el valor de referencia
        CALL FUNCTION 'Z_EDU_SET_DOC_ORIGINAL_CERT'
          EXPORTING
            i_xblnr = lv_xblnr.
      ENDIF.

      CALL FUNCTION 'Z_EDU_FACTURACION'
        EXPORTING
          iv_anio       = lv_anio
          iv_periodo    = lv_periodo
          iv_estudiante = iv_estudiante
          iv_simulado   = iv_simulado
        IMPORTING
          ev_tarifa     = lv_tarifa
          ev_opbel      = lv_opbel
        TABLES
          et_error      = lt_error.

      "Si se obtuvo un documento
      IF NOT lv_opbel IS INITIAL.
        REFRESH lt_object.
        lr_object-plvar = cl_hrpiq00const=>c_plvar_active.
        lr_object-otype = cl_hrpiq00const=>c_otype_st.
        lr_object-objid = iv_estudiante.
        APPEND lr_object TO lt_object.

        CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
          EXPORTING
            infty                 = '9118'
            istat                 = cl_hrpiq00const=>c_istat_active
          TABLES
            innnn                 = lt_9118
            objects               = lt_object
          EXCEPTIONS
            nothing_found         = 1
            wrong_condition       = 2
            infotyp_not_supported = 3
            OTHERS                = 4.

        IF sy-subrc EQ 0.
          CALL FUNCTION 'HRIQ_DELETE_INFTY'
            EXPORTING
              vtask               = 'D'
              commit_flg          = cl_hrpiq00const=>c_bool_no
            TABLES
              innnn               = lt_9118
            EXCEPTIONS
              error_during_delete = 1
              no_authorisation    = 2
              delete_first_record = 3
              corr_exit           = 4
              OTHERS              = 5.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

        "Crea el registro con el documento generado
        ls_opbel_fac-opbel = lv_opbel.
        APPEND ls_opbel_fac TO gt_opbel_fac.
      ENDIF.

      "Adiciona los mensajes de retorno
      APPEND LINES OF lt_error TO et_error.

      "Elimina separadores decimales, de miles y espacios del valor
      REPLACE ALL OCCURRENCES OF '.' IN lv_tarifa WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN lv_tarifa WITH ''.
      CONDENSE lv_tarifa NO-GAPS.
      "Si se tiene tarifa
      IF lv_tarifa NE 'NO'.
        lv_tarifa_val = lv_tarifa.
        "Aumenta el valor total a pagar
        ADD lv_tarifa_val TO lv_tarifa_tval.
      ENDIF.

      "Si es el primer documento generado
      IF lv_first EQ abap_true.
        "Retorna el numero del primer documento
        gv_opbel = lv_opbel.
      ENDIF.

      "Indica que ya no es el primer registro
      CLEAR lv_first.
    ENDLOOP.

    "Si no se tiene valor a pagar
    IF lv_tarifa_tval IS INITIAL OR lv_tarifa_tval EQ 0.
      "Asigna el retorno no
      rt_tarifa = 'NO'.

      "Si se tiene valor a pagar
    ELSE.
      "Adigna el valor total a pagar
      WRITE lv_tarifa_tval TO rt_tarifa LEFT-JUSTIFIED.
      CALL FUNCTION 'PSSV_TEXT_INTO_FIELD_CURRENCY'
        EXPORTING
          i_amount       = rt_tarifa
          i_currency     = 'COP'
        IMPORTING
          e_amount       = rt_tarifa
        EXCEPTIONS
          wrong_amount   = 1
          wrong_currency = 2
          wrong_decimal  = 3
          OTHERS         = 4.
    ENDIF.

  ENDMETHOD.


  METHOD get_tipo_certificados.

    DATA:
      ls_tipo_cert  TYPE gty_tipo_cert,
      ls_attr_value TYPE wdr_context_attr_value.


    me->set_estado_estudiante( iv_programa ).

    IF gt_tipo_cert[] IS INITIAL.
      SELECT tipo_cert AS value
             descr     AS text
             admitido
             activo
             graduado
             retirado
        FROM zedu_certif
        INTO TABLE gt_tipo_cert.
    ENDIF.

    CLEAR rt_attr_value.
    REFRESH rt_attr_value.

    "Recorre los registros
    LOOP AT gt_tipo_cert INTO ls_tipo_cert.
      "Determina los tipos de certificados segun el estado del estudio
      CASE gv_estado_cs.
        WHEN gc_estado_cs_admitido. "ADMITIDO
          CHECK ls_tipo_cert-admitido EQ abap_true.
          MOVE-CORRESPONDING ls_tipo_cert TO ls_attr_value.
          APPEND ls_attr_value TO rt_attr_value.

        WHEN gc_estado_cs_activo.   "ACTIVO
          CHECK ls_tipo_cert-activo EQ abap_true.
          MOVE-CORRESPONDING ls_tipo_cert TO ls_attr_value.
          APPEND ls_attr_value TO rt_attr_value.

        WHEN gc_estado_cs_graduado. "GRADUADO
          CHECK ls_tipo_cert-graduado EQ abap_true.
          MOVE-CORRESPONDING ls_tipo_cert TO ls_attr_value.
          APPEND ls_attr_value TO rt_attr_value.

        WHEN gc_estado_cs_retirado. "RETIRADO
          CHECK ls_tipo_cert-retirado EQ abap_true.
          MOVE-CORRESPONDING ls_tipo_cert TO ls_attr_value.
          APPEND ls_attr_value TO rt_attr_value.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD pago_online.

    DATA: lt_out                 TYPE zlfica_out_ws,
          ls_out                 TYPE zsfica_out_ws,
          lv_bp                  TYPE gpart_kk.

**    me->get_id_estudiante_uname( IMPORTING ev_partner = lv_bp ).
**
**    CALL FUNCTION 'Z_FI_RFC_CONSULTA_DEUDAS'
**      EXPORTING
**        i_gpart     = lv_bp
**      IMPORTING
**        e_t_out     = lt_out
**      EXCEPTIONS
**        error_envio = 1
**        error_act   = 2
**        no_data     = 3
**        OTHERS      = 4.
**    IF sy-subrc <> 0.
*** Implement suitable error handling here
**    ENDIF.
****
******    READ TABLE lt_out INTO ls_out WITH KEY xblnr = me->gv_opbel.
*****    rt_url = ls_out-url.

  ENDMETHOD.


  METHOD set_estado_estudiante.

    "Declaracionese
    DATA:
      lt_hrp1771     TYPE gtt_hrp1771.

    "Field-Symbols
    FIELD-SYMBOLS:
      <fs_hrp1001> TYPE gty_hrp1001,
      <fs_hrp1769> TYPE gty_hrp1769.


    "Inicializa declaraciones
    CLEAR:
      gv_estado_cs.

    "Obtiene el registro del estudio consultado
    READ TABLE gt_hrp1001 ASSIGNING <fs_hrp1001>
      WITH KEY sobid = iv_programa
      BINARY SEARCH.

    "Continua solo si encuentra el registro
    CHECK sy-subrc IS INITIAL.

    "Obtiene el registro del segmento de matricula al estudio
    READ TABLE gt_hrp1769 ASSIGNING <fs_hrp1769>
      WITH KEY objid = iv_programa
      BINARY SEARCH.

    "Continua solo si encuentra el registro
    CHECK sy-subrc IS INITIAL.

    "Crea una copia de los registros de matricula
    lt_hrp1771 = gt_hrp1771.

    "Elimina los registros que no pertenecen al estudio
    DELETE lt_hrp1771
      WHERE objid NE iv_programa.

    "Si se cumplen las condiciones de un estudiante Admitido
    IF <fs_hrp1769>-endda       EQ '99991231' AND
       <fs_hrp1769>-beg_process EQ 'RA01'     AND
       lt_hrp1771               IS INITIAL.
      "Asigna el estado de admitido al estado del estudio
      gv_estado_cs = gc_estado_cs_admitido.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Activo
    IF <fs_hrp1769>-endda       EQ '99991231' AND
       <fs_hrp1769>-beg_process EQ 'RA01'     AND
       NOT lt_hrp1771           IS INITIAL.
      "Asigna el estado de Activo al estado del estudio
      gv_estado_cs = gc_estado_cs_activo.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Graduado
    IF ( <fs_hrp1769>-end_process EQ 'RV01' OR
         <fs_hrp1769>-end_process EQ 'RW01' ) AND
        <fs_hrp1769>-end_reason EQ '1018' .
      "Asigna el estado de graduado al estado del estudio
      gv_estado_cs = gc_estado_cs_graduado.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Retirado
    IF ( <fs_hrp1769>-end_process EQ 'RV01' OR
         <fs_hrp1769>-end_process EQ 'RW01' ) AND
        <fs_hrp1769>-end_reason NE '1018' .
      "Asigna el estado de retirado al estados del estudio
      gv_estado_cs = gc_estado_cs_retirado.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
