class ZCL_WD_GENERAL_ASS definition
  public
  inheriting from CL_WD_COMPONENT_ASSISTANCE
  create public .

public section.

  class-data GA_ID_MESSAGE type SYMSGID value 'ZEDU_WD_MESSAGE' ##NO_TEXT.
  constants C_FIELD_PLAN_EST type STRING value `PLANES_ESTUDIO` ##NO_TEXT.
  constants C_FIELD_FACULTADES type STRING value `FACULTADES` ##NO_TEXT.
  constants C_FIELD_PROGRAMAS type STRING value `PROGRAMAS` ##NO_TEXT.
  constants C_TIPO_PROGRAMA type STRING value 'TIPO_PROGRAMA' ##NO_TEXT.
  constants C_FIELD_TIPO_PARTNER type STRING value 'TIP_PARTNER' ##NO_TEXT.
  constants C_PROGRAMA type STRING value 'PROGRAMA' ##NO_TEXT.
  constants C_PREPOSGRADO type STRING value 'PREPOSGRADO' ##NO_TEXT.
  constants C_PERIODO type STRING value 'PERIODO' ##NO_TEXT.
  constants C_ESTADO_CIVIL type STRING value 'ESTADO_CIVIL' ##NO_TEXT.
  constants C_ANIO type STRING value 'PERID' ##NO_TEXT.
  constants C_PERID type STRING value 'PERID' ##NO_TEXT.
  constants C_CONCEPTO type STRING value 'CONCEPTO' ##NO_TEXT.
  constants C_PROCESO type STRING value 'PROCESO' ##NO_TEXT.
  constants C_PRUEBAS type STRING value 'PRUEBAS' ##NO_TEXT.
  constants C_SUBPRUEBAS type STRING value 'SUBPRUEBAS' ##NO_TEXT.
  constants C_PAIS type STRING value 'PAIS' ##NO_TEXT.
  constants C_DEPARTAMENTO type STRING value 'DEPARTAMENTO' ##NO_TEXT.
  constants C_MUNICIPIO type STRING value 'MUNICIPIO' ##NO_TEXT.
  constants C_TIPO_DOC type STRING value 'TIPO_DOC' ##NO_TEXT.
  constants C_BANKL type STRING value 'BANKL' ##NO_TEXT.
  constants C_TITULO type STRING value 'TITULO' ##NO_TEXT.
  constants C_PROFESION type STRING value 'PROFESION' ##NO_TEXT.
  constants C_SOCIEDAD type STRING value 'SOCIEDAD' ##NO_TEXT.
  constants C_RESPUESTA type STRING value 'RESPUESTA' ##NO_TEXT.
  constants C_VKORG type STRING value 'VKORG' ##NO_TEXT.
  constants C_VTWEG type STRING value 'VTWEG' ##NO_TEXT.
  constants C_SPART type STRING value 'SPART' ##NO_TEXT.
  constants C_VERSG type STRING value 'VERSG' ##NO_TEXT.
  constants C_KDGRP type STRING value 'KDGRP' ##NO_TEXT.
  constants C_BZIRK type STRING value 'BZIRK' ##NO_TEXT.
  constants C_KONDA type STRING value 'KONDA' ##NO_TEXT.
  constants C_PLTYP type STRING value 'PLTYP' ##NO_TEXT.
  constants C_KTGRD type STRING value 'KTGRD' ##NO_TEXT.
  constants C_VKGRP type STRING value 'VKGRP' ##NO_TEXT.
  constants C_VKBUR type STRING value 'VKBUR' ##NO_TEXT.
  constants C_GRUPP type STRING value 'GRUPP' ##NO_TEXT.
  constants C_TOGRU type STRING value 'TOGRU' ##NO_TEXT.
  constants C_VWERK type STRING value 'VWERK' ##NO_TEXT.
  constants C_ZTERM type STRING value 'ZTERM' ##NO_TEXT.
  constants C_ZCODTID type STRING value 'ZCODTID' ##NO_TEXT.
  constants C_ZCODEAD type STRING value 'ZCODEAD' ##NO_TEXT.
  constants C_ZCODTIP type STRING value 'ZCODTIP' ##NO_TEXT.
  constants C_ZCODZON type STRING value 'ZCODZON' ##NO_TEXT.
  constants C_WAERS type STRING value 'WAERS' ##NO_TEXT.
  constants C_WITHT type STRING value 'WITHT' ##NO_TEXT.
  constants C_WT_WITHCD type STRING value 'WT_WITHCD' ##NO_TEXT.
  constants C_JOBGR type STRING value 'JOBGR' ##NO_TEXT.
  constants C_EKORG type STRING value 'EKORG' ##NO_TEXT.
  constants C_KALSK type STRING value 'KALSK' ##NO_TEXT.
  constants C_ZUAWA type STRING value 'ZUAWA' ##NO_TEXT.
  constants C_AKONT type STRING value 'AKONT' ##NO_TEXT.
  constants C_ZAHLS type STRING value 'ZAHLS' ##NO_TEXT.
  constants C_ZLSCH type STRING value 'ZLSCH' ##NO_TEXT.
  constants C_QSREC type STRING value 'QSREC' ##NO_TEXT.
  constants C_ASIGNATURA type STRING value 'ASIGNATURA' ##NO_TEXT.
  constants C_TIPO_TRABAJO type STRING value 'TIPO_TRABAJO' ##NO_TEXT.
  constants C_DURACION_TRABAJO type STRING value 'DURACION_TRABAJO' ##NO_TEXT.
  constants C_ENTIDAD type STRING value 'ENTIDAD' ##NO_TEXT.
  constants C_RANGO_INGRESO type STRING value 'RANGO_INGRESO' ##NO_TEXT.
  constants C_ESTRATO type STRING value 'ESTRATO' ##NO_TEXT.
  constants C_TRATAMIENTO type STRING value 'TRATAMIENTO' ##NO_TEXT.
  constants C_TEXT type TDOBJECT value 'TEXT' ##NO_TEXT.
  constants C_KALKS type STRING value 'KALKS' ##NO_TEXT.
  constants C_VSBED type STRING value 'VSBED' ##NO_TEXT.
  constants C_LPRIO type STRING value 'LPRIO' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_DROPDOWN_KEY_FICA
    importing
      !IM_FIELD type STRING
      !IM_VALUE type STRING optional
      !IM_VALUE_2 type STRING optional
      value(IM_NULL_VALUE) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DROPDOWN_KEY
    importing
      !IM_FIELD type STRING
      !IM_VALUE type STRING optional
      !IM_VALUE_2 type STRING optional
      value(IM_NULL_VALUE) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods VALIDA_TEL_FIJO_FAX
    importing
      !IV_TEL type AD_TLNMBR1 optional
      !IV_EXT type AD_TLXTNS1 optional
      !IV_FIJO type XFELD optional
      !IV_FAX type XFELD optional
      !IV_MOVIL type XFELD optional
      !IV_CAMPO type STRING optional
    returning
      value(RT_ERROR) type BAPIRETURN_T .
  methods FORMATEO_NOMBRES
    changing
      !CH_NOMBRE type CHAR40 .
  methods OBTENER_OBJID_ST
    importing
      !IV_TYPE type BU_ID_TYPE optional
      !IV_IDNUMBER type BU_ID_NUMBER
    returning
      value(RT_OBJID) type HROBJID .
  methods GET_ID_ESTUDIANTE_UNAME
    exporting
      value(EV_STOBJID) type PIQSTUDENT
      value(EV_PARTNER) type BU_PARTNER .
  methods GET_PARAM
    importing
      !IV_REPID type REPID
      !IV_IDPARAM type ZEDU_IDPARAM optional
      !IV_IDPARAMPOS type ZEDU_IDPARAM_POS optional
    returning
      value(RT_VALOR) type ZEDU_VALOR .
  methods GET_STANDARD_TEXT
    importing
      !IV_ID type TDID
      !IV_OBJECT type TDOBJECT
      !IV_NAME type TDOBNAME
    exporting
      !ET_LINES type TLINE_T
      !EV_STRING type STRING .
protected section.
private section.

  types:
    begin of ty_t7piqtrsubtest,
      testtype        type piqtrtesttype,
      subtestscale_id type piqtrextsubresscale,
    end of ty_t7piqtrsubtest .
  types:
    begin of ty_t7piqscalegrade,
      gradeid   type piqscale_gradeid,
      gradetext type piqscale_gradetext,
    end of ty_t7piqscalegrade .
  types:
    begin of ty_t7piqscalet,
      scaleid type t7piqscalet-scaleid,
      text    type t7piqscalet-text,
    end of ty_t7piqscalet .
  types:
    tt_t7piqtrsubtest type table of ty_t7piqtrsubtest .
  types:
    tt_t7piqscalegrade type table of ty_t7piqscalegrade .
  types:
    tt_t7piqscalet     type table of ty_t7piqscalet .
  types:
    begin of ty_objid,
      objid type hrobjid,
    end of ty_objid .
  types:
    ty_t_authority type standard table of hrms_bw_is_authority with default key .    "DCEK903458

  class-data GT_T7PIQSCALEGRADE type TT_T7PIQSCALEGRADE .
  class-data GT_T7PIQTRSUBTEST type TT_T7PIQTRSUBTEST .
  class-data GT_T7PIQSCALET type TT_T7PIQSCALET .
  class-data PLAN_VARIANT type PLVAR .
  class-data GT_DEPARTAMENTO type PIQ_T005U_T .
  class-data GT_MUNICIPIO type ZEDU_T_ADRCITY .
  class-data GT_DESC_MUNI type ZEDU_T_ADRCITYT .
  class-data GT_PARAM type ZEDU_C_PARAM_T .
  class-data GV_REPID type REPID .
  class-data GT_AUTHORITY type TY_T_AUTHORITY .             "DCEK903458
  class-data GR_AUTHORITY_O type /BCV/T_AUT_HROBJID_RANGE .

  class-methods ARMA_RANGOS_AUTORIZADOS .
ENDCLASS.



CLASS ZCL_WD_GENERAL_ASS IMPLEMENTATION.


  method arma_rangos_autorizados.

    loop at gt_authority
      into data(ls_autority)
        where otype eq `O`.

      if ls_autority-objid is initial.
        "tiene autorización a todas las O
        "se deja en blanco para que el rango tome todo
      else.
        append initial line to gr_authority_o assigning field-symbol(<fs_auth>).
        <fs_auth>-sign    = `I`.
        <fs_auth>-option  = `EQ`.
        <fs_auth>-low     = ls_autority-objid.
      endif.

    endloop.

    if sy-subrc ne 0 and
       gr_authority_o[] is initial.

      "Si no hay O se agrega una línea en rango para que no recupere ninguno
      append initial line to gr_authority_o assigning <fs_auth>.
      <fs_auth>-sign    = `I`.
      <fs_auth>-option  = `EQ`.
    endif.

  endmethod.


  method class_constructor.

    plan_variant  = `01`. "-->ver como recuperar

*	Begin	-->	MgM DCEK903458 Perfiles Estructurales 13/02/2017
  "recupero objetos autorizados
  call function 'HR_BW_IS_AUTHORITY'
    exporting
      uname  =  sy-uname
    tables
      e_t_authority = gt_authority.
*     I_T_SELECT          =

  call method arma_rangos_autorizados.
*	End	  -->	MgM DCEK903458

  endmethod.


  METHOD formateo_nombres.

    DATA: lv_nombre(40).

    lv_nombre = ch_nombre.

    TRANSLATE lv_nombre TO LOWER CASE.
    TRANSLATE lv_nombre(1) TO UPPER CASE.

    ch_nombre = lv_nombre.

  ENDMETHOD.


  METHOD get_dropdown_key.

    DATA: lt_t7piqyearprd TYPE TABLE OF t7piqyearprd,
          lt_result_tab   TYPE tswhactor,
          ls_return_tab   TYPE swhactor,
          lr_objid        TYPE RANGE OF hrobjid,
          lrs_objid       LIKE LINE OF lr_objid,
          ls_attr_value   TYPE wdr_context_attr_value,
*          ls_t7piqscalegrade TYPE ty_t7piqscalegrade,
          ls_desc_muni    TYPE adrcityt,
          lv_testtype     TYPE piqtrtesttype,
          lv_act_otype    TYPE objec-otype,
          lv_act_wegid    TYPE gdstr-wegid,
          lv_tipo_prog    TYPE hrobjid,
          lt_objid        TYPE STANDARD TABLE OF ty_objid,
          lv_objid        TYPE hrobjid.

    DATA lt_tp13t TYPE STANDARD TABLE OF tp13t.
    DATA lwa_tp13t LIKE LINE OF lt_tp13t.
    DATA lt_zfit_actecono TYPE STANDARD TABLE OF zfit_actecono.
    DATA lwa_zfit_actecono LIKE LINE OF lt_zfit_actecono.
    DATA lt_zfit_resfisc TYPE STANDARD TABLE OF zfit_resfisc.
    DATA lwa_zfit_resfisc LIKE LINE OF lt_zfit_resfisc.

    FIELD-SYMBOLS: <fs_t7piqscalegrade> TYPE ty_t7piqscalegrade,
                   <fs_departamento>    TYPE t005u,
                   <fs_municipio>       TYPE adrcity.

    CASE im_field.

*      WHEN `FACULTADES`.
      WHEN c_field_facultades.

        SELECT DISTINCT hrp1222~objid AS value
                        hrp1000~stext AS text
          INTO TABLE rt_attr_value
            FROM hrp1222
              INNER JOIN hrt1222
                ON hrp1222~tabnr EQ hrt1222~tabnr
              LEFT OUTER JOIN hrp1000
                ON  hrp1222~objid EQ hrp1000~objid AND
                    hrp1222~otype EQ hrp1000~otype          "DCEK901883
            WHERE hrp1222~otype EQ cl_hr_in_pe_agents=>c_org_unit
              AND hrp1222~subty EQ `9000`
*	Begin	-->	MgM DCEK902983 Cambio forma de búsqueda 21/01/2017
*              and hrt1222~low   eq `001`.
              AND hrp1222~endda = '99991231'
              AND hrp1222~otype EQ `O`
              AND hrt1222~attrib = 'ZSLCM_O'
              AND hrt1222~low   IN ( '001' , '009' , '010' )
*	End	  -->	MgM DCEK902983
              AND hrp1222~objid IN me->gr_authority_o.

        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_field_programas.

        DATA lt_padres TYPE STANDARD TABLE OF hrp1222-objid.

        "recuperamos los programas padres 002
        SELECT  objid
          FROM hrp1222
            INTO TABLE lt_padres
              WHERE objid IN (  SELECT sobid
                                  FROM hrp1001
                                    WHERE otype EQ cl_hr_in_pe_agents=>c_org_unit
                                      AND objid EQ im_value
                                      AND subty EQ `B002` )
                AND tabnr IN ("recupera marcados
                                SELECT tabnr
                                 FROM hrt1222
                                   WHERE attrib  = `ZSLCM_O`
                                     AND low EQ `002` ).  "con 002 se marcan los tipos padres
        IF sy-subrc IS NOT INITIAL.
          CLEAR lt_padres[].
        ENDIF.

        APPEND im_value TO lt_padres.

        SELECT objid AS value
               stext AS text
          FROM hrp1000
            INTO TABLE rt_attr_value
          FOR ALL ENTRIES IN lt_padres
              WHERE objid IN ( SELECT objid
                                  FROM hrp1222
                                    WHERE objid IN (  SELECT sobid
                                                        FROM hrp1001
                                                          WHERE otype EQ cl_hr_in_pe_agents=>c_org_unit
                                                            AND objid EQ lt_padres-table_line
                                                            AND subty EQ `B002` )
                                      AND tabnr IN ("recupera marcados
                                                    SELECT tabnr
                                                     FROM hrt1222
                                                       WHERE attrib  = `ZSLCM_O`
                                                         AND low NE space
                                                         AND low NE `002` ) ).
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_programa.

        SELECT  hrp1222~objid AS value
                hrp1000~stext AS text
          INTO TABLE rt_attr_value
            FROM hrp1222
              INNER JOIN hrt1222
                ON hrp1222~tabnr EQ hrt1222~tabnr
              LEFT OUTER JOIN hrp1000
                ON hrp1222~objid EQ hrp1000~objid
            WHERE hrp1222~otype EQ cl_hr_in_pe_agents=>c_org_unit
              AND hrp1222~subty EQ `9000`
              AND hrt1222~low   IN ('002','005').
*              AND hrt1222~attrib EQ 'ZLSCM_O'.
***              AND hrt1222~low   IN ('003','004','006','007','008').

        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_tipo_programa. " Tipo programa


        lv_act_otype = cl_hriq_course_constants=>c_otype_o.
        lv_act_wegid = 'O-O_DOWN'.
        lv_tipo_prog = im_value.

        CALL FUNCTION 'HRIQ_STRUC_GET'
          EXPORTING
            act_otype      = lv_act_otype
            act_objid      = lv_tipo_prog
            act_wegid      = lv_act_wegid
          TABLES
            result_tab     = lt_result_tab
          EXCEPTIONS
            no_plvar_found = 1
            no_entry_found = 2
            internal_error = 3
            OTHERS         = 4.
        IF sy-subrc IS INITIAL.

          lrs_objid = 'IEQ'.
          LOOP AT lt_result_tab INTO ls_return_tab.
            lrs_objid-low = ls_return_tab-objid.
            APPEND lrs_objid TO lr_objid.
          ENDLOOP.

          SELECT a~objid AS value
                 c~stext AS text
            FROM hrp1222 AS a
            INNER JOIN hrt1222 AS b
            ON b~tabnr = a~tabnr
            INNER JOIN  hrp1000 AS c
            ON c~objid = a~objid
            INTO TABLE rt_attr_value
            WHERE a~objid  IN lr_objid
              AND a~otype  = cl_hriq_course_constants=>c_otype_o
              AND b~attrib = 'ZSLCM_O'
              AND ( b~low EQ '002' OR b~low EQ '005' )
              AND c~langu  = sy-langu.
          IF sy-subrc = 0.
            SORT rt_attr_value
            BY value.
          ENDIF.


        ENDIF.

      WHEN c_field_plan_est.

        lv_tipo_prog = im_value.
        IF lv_tipo_prog IS NOT INITIAL. " Tipo programa

*	Begin	-->	MgM DCEK903247 cambia acceso en programa Lista Clases (CDJ) 02/02/2017
*          lv_act_otype = cl_hriq_course_constants=>c_otype_o.
*          lv_act_wegid = 'O-SC'.
*
*
*          call function 'HRIQ_STRUC_GET'
*            exporting
*              act_otype      = lv_act_otype
*              act_objid      = lv_tipo_prog
*              act_wegid      = lv_act_wegid
*            tables
*              result_tab     = lt_result_tab
*            exceptions
*              no_plvar_found = 1
*              no_entry_found = 2
*              internal_error = 3
*              others         = 4.
*          if sy-subrc is initial.
*
*            lrs_objid = 'IEQ'.
*            loop at lt_result_tab into ls_return_tab.
*              lrs_objid-low = ls_return_tab-objid.
*              append lrs_objid to lr_objid.
*            endloop.
*
*            select objid as value
*                   stext as text
*             from hrp1000
*               into table rt_attr_value
*                 where plvar eq cl_eain_constants=>c_active_plvar_01
*                   and otype eq cl_hriq_course_constants=>c_otype_sc
*                   and objid in lr_objid
*                   and langu eq sy-langu.
*            if sy-subrc is not initial.
*              clear rt_attr_value[].
*            endif.
*
*          endif.

          SELECT DISTINCT b~objid AS value
                          b~stext AS text
            FROM  hrp1001 AS a
              INNER JOIN hrp1000 AS b
                ON a~sobid  = b~objid
              INTO TABLE rt_attr_value
                WHERE a~plvar = cl_eain_constants=>c_active_plvar_01
                  AND a~otype = cl_hriq_course_constants=>c_otype_o
                  AND a~objid = lv_tipo_prog
                  AND a~endda = '99991231'
                  AND a~sclas = cl_hriq_course_constants=>c_otype_sc
                  AND b~plvar = cl_eain_constants=>c_active_plvar_01
                  AND b~otype = cl_hriq_course_constants=>c_otype_sc
                  AND b~endda = '99991231'
                  AND b~langu = sy-langu.
*	End	  -->	MgM DCEK903247

*	Begin	-->	MgM DCEK902983 agregado humanidades 23/01/2017
        ELSEIF im_value_2 IS NOT INITIAL.
          "im_value_2 => facultad
          LOOP AT me->get_dropdown_key(
                    EXPORTING im_field = me->c_tipo_programa
                              im_value = im_value_2 )  INTO DATA(ls_programa).

            "por cada tipo de programa que se corresponda con la facultad
            " agregamos al matchcode de plan de estudio
            APPEND LINES OF me->get_dropdown_key(
                              EXPORTING im_field = c_field_plan_est
                                        im_value = ls_programa-value )
              TO rt_attr_value.

          ENDLOOP.

*	End	  -->	MgM DCEK902983

        ELSE.

          SELECT  objid AS value
                  stext AS text
            FROM hrp1000
              INTO TABLE rt_attr_value
                WHERE plvar EQ cl_eain_constants=>c_active_plvar_01
                  AND otype EQ cl_hriq_course_constants=>c_otype_sc
                  AND langu EQ sy-langu.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ENDIF.

      WHEN c_preposgrado.

        lv_tipo_prog = im_value.
        IF lv_tipo_prog IS NOT INITIAL. " Tipo programa

          lv_act_otype = cl_hriq_course_constants=>c_otype_o.
          lv_act_wegid = 'O-O_DOWN'.

          CALL FUNCTION 'HRIQ_STRUC_GET'
            EXPORTING
              act_otype      = lv_act_otype
              act_objid      = lv_tipo_prog
              act_wegid      = lv_act_wegid
            TABLES
              result_tab     = lt_result_tab
            EXCEPTIONS
              no_plvar_found = 1
              no_entry_found = 2
              internal_error = 3
              OTHERS         = 4.
          IF sy-subrc IS INITIAL.

            lrs_objid = 'IEQ'.
            LOOP AT lt_result_tab INTO ls_return_tab.
              lrs_objid-low = ls_return_tab-objid.
              APPEND lrs_objid TO lr_objid.
            ENDLOOP.

            SELECT a~objid AS value
                   c~stext AS text
              FROM hrp1222 AS a
              INNER JOIN hrt1222 AS b
              ON b~tabnr = a~tabnr
              INNER JOIN  hrp1000 AS c
              ON c~objid = a~objid
              INTO TABLE rt_attr_value
              WHERE a~objid  IN lr_objid
                AND a~otype  = cl_hriq_course_constants=>c_otype_o
                AND b~attrib = 'ZSLCM_O'
                AND ( b~low EQ '003' OR
                      b~low EQ '004' OR
                      b~low EQ '006' OR
                      b~low EQ '007' OR
                      b~low EQ '008' )
                AND c~langu  = sy-langu.
            IF sy-subrc = 0.
              SORT rt_attr_value
              BY value.
            ENDIF.
          ENDIF.

        ELSE.
          SELECT a~objid AS value
                         c~stext AS text
                    FROM hrp1222 AS a
                    INNER JOIN hrt1222 AS b
                    ON b~tabnr = a~tabnr
                    INNER JOIN  hrp1000 AS c
                    ON c~objid = a~objid
                    INTO TABLE rt_attr_value
                    WHERE a~otype  = cl_hriq_course_constants=>c_otype_o
                      AND b~attrib = 'ZSLCM_O'
                      AND ( b~low EQ '003' OR
                            b~low EQ '004' OR
                            b~low EQ '006' OR
                            b~low EQ '007' OR
                            b~low EQ '008' )
                      AND c~langu  = sy-langu.
          IF sy-subrc = 0.
            SORT rt_attr_value
            BY value.
          ENDIF.
        ENDIF.
      WHEN c_periodo. "(académico)

        IF im_value IS NOT INITIAL.
          SELECT *
           FROM t7piqyearprd
           INTO TABLE lt_t7piqyearprd
           WHERE peryr = im_value.
          IF sy-subrc IS NOT INITIAL.
            CLEAR lt_t7piqyearprd[].
          ENDIF.

          CHECK lt_t7piqyearprd IS NOT INITIAL.

          SELECT perid AS value
                 perit AS text
            FROM t7piqperiodt
            INTO TABLE rt_attr_value
            FOR ALL ENTRIES IN lt_t7piqyearprd
            WHERE spras EQ sy-langu
              AND perid EQ lt_t7piqyearprd-perid.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ELSE.

          SELECT perid AS value
                 perit AS text
            FROM t7piqperiodt
            INTO TABLE rt_attr_value
             WHERE spras EQ sy-langu.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ENDIF.

      WHEN c_anio. "(académico)

        SELECT peryr AS value
               peryt AS text
         FROM t7piqyeart
           INTO TABLE rt_attr_value
             WHERE spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.
      WHEN c_estado_civil.

        SELECT marst AS value
               bez20 AS text
         FROM tb027t
           INTO TABLE rt_attr_value
             WHERE spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN `PERID`. "Período Académico

        SELECT  perid
                perit
          FROM t7piqperiodt
            INTO TABLE rt_attr_value
              WHERE spras EQ sy-langu.
        "and PERTYPE eq --> a definir
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_concepto.

        IF gt_t7piqtrsubtest IS INITIAL.
          SELECT testtype subtestscale_id
            FROM t7piqtrsubtest
            INTO TABLE gt_t7piqtrsubtest
            WHERE testtype = 'PRUEBAS_ADMI'.
          IF sy-subrc IS NOT INITIAL.
            CLEAR gt_t7piqtrsubtest[].
          ENDIF.

        ENDIF.

        IF gt_t7piqscalegrade IS INITIAL AND gt_t7piqtrsubtest IS NOT INITIAL.

          SELECT gradeid gradetext
            FROM t7piqscalegrade
            INTO TABLE gt_t7piqscalegrade
            FOR ALL ENTRIES IN gt_t7piqtrsubtest
            WHERE scaleid = gt_t7piqtrsubtest-subtestscale_id.
          IF sy-subrc IS NOT INITIAL.
            CLEAR gt_t7piqscalegrade[].
          ENDIF.
        ENDIF.

        LOOP AT gt_t7piqscalegrade ASSIGNING <fs_t7piqscalegrade>.
          ls_attr_value-value = <fs_t7piqscalegrade>-gradeid.
          ls_attr_value-text  = <fs_t7piqscalegrade>-gradetext.
          APPEND ls_attr_value TO rt_attr_value.
        ENDLOOP.

      WHEN c_pruebas.

        SELECT subtestid   AS value
               subtesttext AS text
         FROM t7piqtrsubtestt
           INTO TABLE rt_attr_value
             WHERE spras = sy-langu
               AND testtype = 'PRUEBAS_ADMI'.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.
      WHEN c_subpruebas.
        " VER VALOR ENTREVISTA!!!
        SELECT subtestid   AS value
               subtesttext AS text
         FROM t7piqtrsubtestt
           INTO TABLE rt_attr_value
             WHERE spras    = sy-langu
               AND testtype = 'ENTREVISTA'.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.
      WHEN c_pais.

        SELECT land1   AS value
               landx50 AS text
         FROM t005t
           INTO TABLE rt_attr_value
             WHERE spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_departamento.

        IF gt_departamento IS INITIAL.
          SELECT *
           FROM t005u
             INTO TABLE gt_departamento
               WHERE spras EQ sy-langu.
        ENDIF.

        LOOP AT gt_departamento ASSIGNING <fs_departamento> WHERE land1 = im_value.
          ls_attr_value-value = <fs_departamento>-bland.
          ls_attr_value-text  = <fs_departamento>-bezei.
          APPEND ls_attr_value TO rt_attr_value.
        ENDLOOP.

      WHEN c_municipio.

        IF gt_municipio IS INITIAL.
          SELECT *
           FROM adrcity
             INTO TABLE gt_municipio.
          IF sy-subrc IS NOT INITIAL.
            CLEAR gt_municipio[].
          ENDIF.

          SELECT *
           FROM adrcityt
             INTO TABLE gt_desc_muni
             WHERE langu = sy-langu.
          IF sy-subrc IS NOT INITIAL.
            CLEAR gt_desc_muni[].
          ENDIF.

        ENDIF.

        LOOP AT gt_municipio ASSIGNING <fs_municipio> WHERE country = im_value
                                                        AND region  = im_value_2.

          CLEAR ls_desc_muni.
          READ TABLE gt_desc_muni INTO ls_desc_muni WITH KEY country   = im_value
                                                             city_code = <fs_municipio>-city_code.

          ls_attr_value-value = ls_desc_muni-city_name. "city_code.
          ls_attr_value-text  = ls_desc_muni-city_name.
          APPEND ls_attr_value TO rt_attr_value.

        ENDLOOP.

      WHEN c_tipo_doc.

        IF im_value = 'EMPRESA'.

          SELECT a~category AS value
                 b~text     AS text
           FROM  tb039a AS a
            INNER JOIN tb039t AS b ON a~category = b~category
             INTO TABLE rt_attr_value
               WHERE langu EQ sy-langu
                 AND a~category LIKE 'FS%'
                 AND a~xorganisation = abap_true.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ELSEIF im_value = 'PERSONA'.

          SELECT a~category AS value
                 b~text     AS text
           FROM  tb039a AS a
            INNER JOIN tb039t AS b ON a~category = b~category
             INTO TABLE rt_attr_value
               WHERE langu EQ sy-langu
                 AND a~category LIKE 'FS%'
                 AND a~xperson = abap_true.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ELSE.

          SELECT category AS value
                 text     AS text
           FROM tb039t
             INTO TABLE rt_attr_value
               WHERE langu EQ sy-langu
                 AND category LIKE 'FS%'.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.

        ENDIF.


      WHEN c_bankl.

        SELECT bankl AS value
               banka AS text
          FROM bnka
          INTO TABLE rt_attr_value
          WHERE banks = 'CO'.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_titulo.

        SELECT title      AS value
               title_medi AS text
         FROM tsad3t
           INTO TABLE rt_attr_value
             WHERE langu EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_tratamiento.

        SELECT atext AS value
               anrlt AS text
         FROM t522t
           INTO TABLE rt_attr_value
             WHERE sprsl EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_profesion.

        SELECT jobgr AS value
               bez30 AS text
         FROM tb028t
           INTO TABLE rt_attr_value
             WHERE spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_sociedad.

        SELECT bukrs AS value
               butxt AS text
         FROM t001
           INTO TABLE rt_attr_value
             WHERE land1 EQ 'CO'
               AND ktopl EQ 'PUC'.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_field_tipo_partner.

        ls_attr_value-value = '01'.
        ls_attr_value-text  = 'Deudor'.
        APPEND ls_attr_value TO rt_attr_value.
        ls_attr_value-value = '02'.
        ls_attr_value-text  = 'Acreedor'.
        APPEND ls_attr_value TO rt_attr_value.

      WHEN c_respuesta.

        ls_attr_value-value = ' '.
        ls_attr_value-text  = 'No Informa'.
        APPEND ls_attr_value TO rt_attr_value.

        ls_attr_value-value = 'N'.
        ls_attr_value-text  = 'No'.
        APPEND ls_attr_value TO rt_attr_value.


        ls_attr_value-value = 'S'.
        ls_attr_value-text  = 'Si'.
        APPEND ls_attr_value TO rt_attr_value.


      WHEN c_asignatura.
        "       'ASIGNATURA'.
        lv_objid = im_value.

        SELECT objid
          INTO TABLE lt_objid
          FROM hrp1001
          WHERE sobid EQ lv_objid
          AND sclas EQ 'SC'
          AND otype EQ 'SM'.
        IF sy-subrc IS INITIAL.

          SELECT objid AS value
                 stext AS text
            INTO TABLE rt_attr_value
          FROM hrp1000
            FOR ALL ENTRIES IN lt_objid
          WHERE objid EQ lt_objid-objid
            AND plvar EQ '01'
            AND otype EQ 'SM'.
          IF sy-subrc IS NOT INITIAL.
            CLEAR rt_attr_value[].
          ENDIF.
        ENDIF.

        CLEAR lv_objid.
        ls_attr_value-value = lv_objid.
        ls_attr_value-text  = 'Todas las asignaturas'.
        APPEND ls_attr_value TO rt_attr_value.

      WHEN c_tipo_trabajo.

        " Selecciona tipos de trabajo
        SELECT cod_tiptra AS value
               descripcion AS text
          INTO TABLE rt_attr_value
        FROM zedu_tipotra.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_duracion_trabajo.

        " Selecciona rango de duraciones de trabajo
        SELECT cod_durtrab AS value
               descripcion AS text
          INTO TABLE rt_attr_value
        FROM zedu_durtrab.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_entidad.

        " Selecciona entidades
        SELECT cod_ent AS value
               descripcion AS text
          INTO TABLE rt_attr_value
        FROM zedu_entidadt
        WHERE spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_rango_ingreso.
        " Selecciona rango de ingresos de trabajo
        SELECT cod_ingreso AS value
               descripcion AS text
          INTO TABLE rt_attr_value
        FROM zedu_ingresos.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.

      WHEN c_estrato.
        " Selecciona estrato social
        SELECT social AS value
               socialt AS text
          INTO TABLE rt_attr_value
        FROM t7piqsocialt WHERE
        spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          CLEAR rt_attr_value[].
        ENDIF.
*{AD_2.0.0_01
      WHEN 'CLASE_ACTECO'. "Actividad económica
        SELECT * FROM zfit_actecono
        INTO TABLE lt_zfit_actecono.


        LOOP AT lt_zfit_actecono INTO lwa_zfit_actecono.
          ls_attr_value-value = lwa_zfit_actecono-clase.
          ls_attr_value-text  = lwa_zfit_actecono-descrip.
          APPEND ls_attr_value TO rt_attr_value.
        ENDLOOP.

      WHEN 'COD_RESFISCAL'. "Responsabilidad fiscal
        SELECT * FROM zfit_resfisc
        INTO TABLE lt_zfit_resfisc.


        LOOP AT lt_zfit_resfisc INTO lwa_zfit_resfisc.
          ls_attr_value-value = lwa_zfit_resfisc-codigo.
          ls_attr_value-text  = lwa_zfit_resfisc-descrip.
          APPEND ls_attr_value TO rt_attr_value.
        ENDLOOP.

      WHEN 'GRUPO_DEST'. "Grupo destino
        SELECT * FROM tp13t
        INTO TABLE lt_tp13t
          WHERE langu = sy-langu.

        LOOP AT lt_tp13t INTO lwa_tp13t.
          ls_attr_value-value = lwa_tp13t-group_d_t. "lwa_tp13t-group_d.
          ls_attr_value-text  = lwa_tp13t-group_d_t.
          APPEND ls_attr_value TO rt_attr_value.
        ENDLOOP.
*}AD_2.0.0_01
      WHEN OTHERS.

    ENDCASE.

    IF im_null_value EQ cl_bp_const=>true.

      "Agrego línea en blanco para poder seleccionar blanco
      APPEND INITIAL LINE TO rt_attr_value.

    ENDIF.

*	Begin	-->	MgM DCEK901229 para binary search 31/10/2016
    SORT rt_attr_value
      BY value.
*	End	  -->	MgM DCEK901229

* Inicio HIRS 28/08/2017 --> DCEK906190
    IF im_field EQ c_anio.
      SORT rt_attr_value[] BY value DESCENDING.
    ENDIF.
* FIN HIRS 28/08/2017 --> DCEK906190

  ENDMETHOD.


  method get_dropdown_key_fica.

    data: ls_attr_value type wdr_context_attr_value,
          lv_tipo.


    case im_field.

      when c_vkorg. " Unidades org.: Organizaciones de ventas

        select vkorg as value
               vtext as text
         from tvkot
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_vtweg. " Unidad organizativa: Canales de distribución

        select vtweg as value
               vtext as text
         from tvtwt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_spart. " Unidad organizativa: Sectores comerciales

        select spart as value
               vtext as text
         from tspat
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_kdgrp. " Deudores: Grupos de clientes

        select kdgrp as value
               ktext as text
         from t151t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_bzirk. " Deudores: Zonas de ventas

        select bzirk as value
               bztxt as text
         from t171t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_konda. " Condiciones: Grupos para clases cliente

        select konda as value
               vtext as text
         from t188t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_pltyp. " Condiciones: Tipos lista precios

        select pltyp as value
               ptext as text
         from t189t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_ktgrd. " Deudores: Grupos de imputación

        select ktgrd as value
               vtext as text
         from tvktt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_vkgrp. " Unidad organización: Grupos vendedores

        select vkgrp as value
               bezei as text
         from tvgrt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_vkbur. " Unidad de organización: Oficinas de ventas

        select vkbur as value
               bezei as text
         from tvkbt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_grupp. " Txt.grupo tesorería

        select grupp as value
               textl as text
         from t035t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

        if im_value is not initial.
          loop at rt_attr_value into ls_attr_value.
            check ls_attr_value-value(1) ne im_value.
            delete table rt_attr_value from ls_attr_value.
          endloop.
        endif.

when c_togru. " Denominación grupos de tolerancia de interlocutor comercial

        select togru as value
               txt30 as text
         from t043gt
           into table rt_attr_value
             where spras eq sy-langu
               and bukrs eq im_value.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_waers. " Denominación de los códigos de moneda

        select waers as value
               ltext as text
         from tcurt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_vwerk. " Centros/Sucursales

        select werks as value
               name1 as text
         from t001w
           into table rt_attr_value.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zterm. " Aclaraciones propias para condiciones de pago

        select zterm as value
               text1 as text
         from t052u
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

        if im_value is not initial.
          loop at rt_attr_value into ls_attr_value.
            check ls_attr_value-value(1) ne im_value.
            delete table rt_attr_value from ls_attr_value.
          endloop.
        endif.

*	Begin	-->	MgM DCEK906161 Datos adic. nuevos campos 21/09/2017
      when c_zcodtid.

        select zcodtid as value
               descri  as text
         from ztsd004
           into table rt_attr_value.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zcodead.

        select zcodead as value
               descri  as text
         from ztsd002
           into table rt_attr_value.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zcodtip.

        select zcodtip as value
               descri  as text
         from ztsd003
           into table rt_attr_value.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zcodzon.

        select zcodzon as value
               descri  as text
         from ztsd005
           into table rt_attr_value.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.
*	End	  -->	MgM DCEK906161

      when c_witht. " Tabla texto tipo retenciones

*	Begin	-->	MgM DCEK902947 filtra datos join 19/01/2017
*        select witht  as value
*               text40 as text
*         from t059u
*           into table rt_attr_value
*             where spras eq sy-langu
*               and land1 eq 'CO'.

        select u~witht  as value
               u~text40 as text
         from t059u as u
          inner join t059p as p
            on  u~land1 eq p~land1 and
                u~witht eq p~witht
           into table rt_attr_value
             where u~spras eq sy-langu
               and u~land1 eq 'CO'.
*	End	  -->	MgM DCEK902947

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.


  when c_wt_withcd. " Tabla de texto indicador de retención de impuestos

        if im_value is not initial.

          select wt_withcd as value
                 text40 as text
           from t059zt
             into table rt_attr_value
               where spras eq sy-langu
                 and land1 eq 'CO'
                 and witht eq im_value.
          if sy-subrc is not initial.
            clear rt_attr_value[].
          endif.

        else.

          select wt_withcd as value
                 text40    as text
            from t059zt
            into table rt_attr_value
            where spras eq sy-langu
              and land1 eq 'CO'.
          if sy-subrc is not initial.
            clear rt_attr_value[].
          endif.

        endif.


when c_qsrec. " Categorías de la retención: acreedores por tipo de retención

        if im_value is not initial.

*	Begin	-->	MgM DCEK902947 filtra datos join 19/01/2017
*          select qsrec as value
*               rctxt as text
*         from t059d
*           into table rt_attr_value
*             where spras eq sy-langu
*               and land1 eq 'CO'
*               and witht eq im_value.
*          if sy-subrc is not initial.
*            clear rt_attr_value[].
*          endif.
*
*        else.
*
*          select qsrec as value
*             rctxt as text
*       from t059d
*         into table rt_attr_value
*           where spras eq sy-langu
*             and land1 eq 'CO'.
*          if sy-subrc is not initial.
*            clear rt_attr_value[].
*          endif.

          select d~qsrec as value
                 d~rctxt as text
            from t059c as c
              inner join t059d as d
                 on  c~land1 eq d~land1 and
                     c~witht eq d~witht and
                     c~qsrec eq d~qsrec
                into table rt_attr_value
                 where c~land1 eq 'CO'
                   and c~witht eq im_value
                   and d~spras eq sy-langu.

          if sy-subrc is not initial.
            clear rt_attr_value[].
          endif.

        else.

          select d~qsrec as value
                 d~rctxt as text
            from t059c as c
              inner join t059d as d
                 on  c~land1 eq d~land1 and
                     c~witht eq d~witht and
                     c~qsrec eq d~qsrec
                into table rt_attr_value
                 where c~land1 eq 'CO'
                   and d~spras eq sy-langu.

          if sy-subrc is not initial.
            clear rt_attr_value[].
          endif.

*	End	  -->	MgM DCEK902947
        endif.

      when c_ekorg. " Organizaciones de compras

        select ekorg as value
               ekotx as text
         from t024e
           into table rt_attr_value
             where bukrs eq im_value.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_jobgr. " Profesiones/grupos IC: Textos

        select jobgr as value
               bez30 as text
         from tb028t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.


when c_kalsk. " Denominación del grupo para esquema de cálculo (Proveedor)

        select kalsk as value
               kalsb as text
         from tmkkt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.


      when c_zuawa. " Normas de asignación

        select zuawa as value
               ttext as text
         from tzunt
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

 when c_akont. " Maestro de ctas. de mayor (plan de ctas.: denominación)

        select saknr as value
               txt50 as text
         from skat
           into table rt_attr_value
             where spras eq sy-langu
               and ktopl eq 'PUC'.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zahls. " Razones de bloqueo en pagos automáticos

        select zahls as value
               textl as text
         from t008t
           into table rt_attr_value
             where spras eq sy-langu.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_zlsch. " Vías de pago para pagos automáticos

        select zlsch as value
               text1 as text
          from t042z
          into table rt_attr_value
          where land1 = 'CO'.
        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

***      WHEN c_respuesta.
***
***        ls_attr_value-value = ' '.
***        ls_attr_value-text  = 'No Informa'.
***        APPEND ls_attr_value TO rt_attr_value.
***
***        ls_attr_value-value = 'N'.
***        ls_attr_value-text  = 'No'.
***        APPEND ls_attr_value TO rt_attr_value.
***
***
***        ls_attr_value-value = 'S'.
***        ls_attr_value-text  = 'Si'.
***        APPEND ls_attr_value TO rt_attr_value.

*	Begin	-->	MgM DCEK902866 amplia campos Org.Vtas 18/01/2017
      when c_versg.

        select  stgku   as value
                bezei20 as text
         from tvsdt
           into table rt_attr_value
             where spras eq sy-langu.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_kalks. "Esquema de cálculo: Deudores: Textos

        select  kalks as value
                vtext as text
         from tvkdt
           into table rt_attr_value
             where spras eq sy-langu.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_lprio. "Deudores: Prioridades de entrega: Textos

        select  lprio as value
                bezei as text
         from tprit
           into table rt_attr_value
             where spras eq sy-langu.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.

      when c_vsbed. "Condiciones de expedición: Textos

        select  vsbed as value
                vtext as text
         from tvsbt
           into table rt_attr_value
             where spras eq sy-langu.

        if sy-subrc is not initial.
          clear rt_attr_value[].
        endif.
*	End	  -->	MgM DCEK902866

      when others.

    endcase.

    if im_null_value eq cl_bp_const=>true.

      "Agrego línea en blanco para poder seleccionar blanco
      append initial line to rt_attr_value.

    endif.

  endmethod.


  METHOD get_id_estudiante_uname.

    SELECT SINGLE objkey
      FROM usapplref
      INTO ev_partner
      WHERE bname = sy-uname.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ev_partner.
    ENDIF.

    CHECK ev_partner IS NOT INITIAL.

    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO ev_stobjid
      WHERE partner = ev_partner.
    IF sy-subrc IS NOT INITIAL.
      CLEAR ev_stobjid.
    ENDIF.



  ENDMETHOD.


  method get_param.
    data: ls_param        type zedu_c_param.

    if  gt_param is initial or
        gv_repid ne iv_repid.

      gv_repid = iv_repid.

      select *
        from zedu_c_param
          into table gt_param
            where repid = gv_repid.

      check sy-subrc eq 0.

      sort gt_param by idparam
                       idparampos.

    endif.

    clear ls_param.

    read table gt_param
      into ls_param
        with key idparam    = iv_idparam
                 idparampos = iv_idparampos
          binary search.

    if sy-subrc eq 0.
      rt_valor = ls_param-valor.
    endif.

  endmethod.


  METHOD get_standard_text.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = iv_id
        language                = sy-langu
        name                    = iv_name
        object                  = iv_object
      TABLES
        lines                   = et_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'IDMX_DI_TLINE_INTO_STRING'
      EXPORTING
        it_tline       = et_lines
      IMPORTING
        ev_text_string = ev_string.


  ENDMETHOD.


  METHOD obtener_objid_st.

    DATA: lv_partner TYPE bu_partner.

    IF iv_type IS INITIAL.
      SELECT SINGLE partner
        FROM but0id
        INTO lv_partner
        WHERE idnumber = iv_idnumber.
      IF sy-subrc IS NOT INITIAL.
        CLEAR lv_partner.
      ENDIF.
    ELSE.
      SELECT SINGLE partner
        FROM but0id
        INTO lv_partner
        WHERE type     = iv_type
          AND idnumber = iv_idnumber.
      IF sy-subrc IS NOT INITIAL.
        CLEAR lv_partner.
      ENDIF.
    ENDIF.


    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO rt_objid
      WHERE partner = lv_partner.
    IF sy-subrc IS NOT INITIAL.
      CLEAR rt_objid.
    ENDIF.


***      SELECT SINGLE objid
***      FROM hrp1702
***      INTO rt_objid
***      WHERE prdni = iv_identificacion.
  ENDMETHOD.


  METHOD valida_tel_fijo_fax.

    DATA: ls_error  TYPE bapiret2,
          lv_length TYPE i.


*** valido el número de telefono
    IF iv_tel IS NOT INITIAL.

      lv_length = strlen( iv_tel ).

      IF lv_length < 7.

*   El & tiene menos de 7 caracteres.
        IF 1 = 2. MESSAGE e000(zedu_wd_message) WITH '&'. ENDIF.

        ls_error-type   = 'E'.
        ls_error-id     = ga_id_message.
        ls_error-number = 000.
        IF iv_fijo IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono fijo'.
        ELSEIF iv_fax IS NOT INITIAL.
          ls_error-message_v1 = 'fax'.
        ELSEIF iv_movil IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono móvil'.
        ELSE.
          ls_error-message_v1 = 'teléfono'.
        ENDIF.
        ls_error-field = iv_campo.

        APPEND ls_error TO rt_error.

      ELSEIF lv_length > 10.

*   El & tiene más de 10 caracteres.
        IF 1 = 2. MESSAGE e001(zedu_wd_message) WITH '&'. ENDIF.

        ls_error-type   = 'E'.
        ls_error-id     = ga_id_message.
        ls_error-number = 001.
        IF iv_fijo IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono fijo'.
        ELSEIF iv_fax IS NOT INITIAL.
          ls_error-message_v1 = 'fax'.
        ELSEIF iv_movil IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono móvil'.
        ELSE.
          ls_error-message_v1 = 'teléfono'.
        ENDIF.
        ls_error-field = iv_campo.

        APPEND ls_error TO rt_error.

      ENDIF.

      IF iv_tel CN '1234567890 '.

*   El campo & tiene caracteres no válidos.
        IF 1 = 2. MESSAGE e010(zedu_wd_message) WITH '&'. ENDIF.

        ls_error-type   = 'E'.
        ls_error-id     = ga_id_message.
        ls_error-number = 010.
        IF iv_fijo IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono fijo'.
        ELSEIF iv_fax IS NOT INITIAL.
          ls_error-message_v1 = 'fax'.
        ELSEIF iv_movil IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono móvil'.
        ELSE.
          ls_error-message_v1 = 'teléfono'.
        ENDIF.
        ls_error-field = iv_campo.

        APPEND ls_error TO rt_error.

      ENDIF.

    ENDIF.

*** valido la extensión
    IF iv_ext IS NOT INITIAL.

      CLEAR lv_length.
      lv_length = strlen( iv_ext ).

      IF lv_length > 10.

*   La extensión del & tiene más de 10 caracteres.
        IF 1 = 2. MESSAGE e002(zedu_wd_message) WITH '&'. ENDIF.

        ls_error-type   = 'E'.
        ls_error-id     = ga_id_message.
        ls_error-number = 002.
        IF iv_fijo IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono fijo'.
        ELSEIF iv_fax IS NOT INITIAL.
          ls_error-message_v1 = 'fax'.
        ELSE.
          ls_error-message_v1 = 'teléfono'.
        ENDIF.
        ls_error-field = iv_campo.

        APPEND ls_error TO rt_error.

      ENDIF.

      IF iv_ext CN '1234567890 '.
*   El campo & tiene caracteres no válidos.
        IF 1 = 2. MESSAGE e010(zedu_wd_message) WITH '&'. ENDIF.


        ls_error-type   = 'E'.
        ls_error-id     = ga_id_message.
        ls_error-number = 010.
        IF iv_fijo IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono fijo'.
        ELSEIF iv_fax IS NOT INITIAL.
          ls_error-message_v1 = 'fax'.
        ELSEIF iv_movil IS NOT INITIAL.
          ls_error-message_v1 = 'teléfono móvil'.
        ELSE.
          ls_error-message_v1 = 'teléfono'.
        ENDIF.
        ls_error-field = iv_campo.

        APPEND ls_error TO rt_error.

      ENDIF.

    ENDIF.

***IF ls_datos_bancarios-bankn IS NOT INITIAL AND ls_datos_bancarios-bankn CN '1234567890 '.
***    rt_error = 'X'.
***    CLEAR ls_errores.
***    ls_errores-id = wd_assist->ga_id_message.
***    ls_errores-number = '010'.
***    ls_errores-message_v1 = wd_assist->if_wd_component_assistance~get_text( key = '003' ).
***    ls_errores-field = 'BANKN'.
***    APPEND ls_errores TO lt_errores_aux.
***  ENDIF.

  ENDMETHOD.
ENDCLASS.
