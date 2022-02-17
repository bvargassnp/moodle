class ZWDC_PRU_ADMI_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  methods GET_PRUEBAS_ADMISION
    importing
      !IV_SELECCION type ZEDU_S_SEL_PRU_ADMI
    returning
      value(RT_RESULTADO) type ZEDU_T_RESULT_PRUE_ADM .
  methods GET_CANDIDATOS
    importing
      !IV_SELECCION type ZEDU_S_SEL_PRU_ADMI
    returning
      value(RT_RESULTADO) type ZEDU_T_TAB_PRU_ADMI .
  methods GRABAR_PRUEBAS
    importing
      !IS_PRADM type ZEDU_PRADM
      !IS_PRADT type ZEDU_PRADT
    returning
      value(RT_ERROR) type BAPIRETURN_T .
  methods GET_PRUEBAS .
  methods GET_RESULTADOS
    importing
      !I_PRDNI type BPTAXNUM
      !I_TEST type PIQTRTESTTYPE optional
    returning
      value(RT_RESULTADOS) type ZEDU_T_PRADM .
  methods GET_DDBK_PRUEBAS
    importing
      !IT_PRUEBAS type ZEDU_T_PRADM
    returning
      value(RT_DDBK_PRUEBAS) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_TEXTOS_RESULTADOS
    importing
      !I_KEY type ZEDU_ED_KEYOBS optional
    returning
      value(R_TEXTO) type STRING .
  methods GET_FECHA_EXAM_ENTR
    importing
      !IV_PLAN_ESTUDIO type HROBJID
      !IV_ANIO type PIQPERYR
      !IV_PERIODO type PIQPERID
      !IV_TIPO_FECHA type PIQEVOBTYPE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DDBK_ANIO_PERIODO
    importing
      !IV_PROGRAMA type HROBJID
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_OBSERVACION
    importing
      !IV_TESTTYPE type PIQTRTESTTYPE
      !IV_ADM_AYEAR type PIQPERYR
      !IV_ADM_PERID type PIQPERID
      !IV_OBJID_ST type PIQSTUDENT
    exporting
      !EV_OBSERVACION type ZEDU_ED_VALORACION
      !EV_CONCEPTO type PIQSCALE_ID
      !EV_MOMENTO type ZEDU_ED_MOMENTO .
  methods GET_RESULTADOS_FROM_DOCS
    importing
      !IT_PRDNI type ZEDU_T_NRO_IDENT
    returning
      value(RT_RESULTADOS) type ZEDU_T_PRADM .
  methods GET_NAME_USERS
    importing
      !IT_USERS type USMD_T_USER
    returning
      value(RT_USER_ADDR) type TABTYPE_USER_ADDR .
  methods GET_TEXTOS_RESULTADOS_ALL
    importing
      !IT_KEY type ZEDU_T_ED_KEYOBS
    returning
      value(R_TEXTOS) type ZEDU_T_PRADT .
  methods GET_TIPOS_PRUEBAS
    returning
      value(RT_TIPO_PRUEBAS) type ZEDU_T_T7PIQTRSUBTESTT .
protected section.

  data GT_FECHAS_EN type ZEDU_T_FEC_EXAM_ENTREVISTA .
  data GT_FECHAS_EX type ZEDU_T_FEC_EXAM_ENTREVISTA .
private section.

  types:
    BEGIN OF ty_admi_1,
      nr_formulario TYPE zpre_admi_1-nr_formulario,
      tipo_documen  TYPE zpre_admi_1-tipo_documen,
      nro_documen   TYPE zpre_admi_1-nro_documen,
      nombre        TYPE zpre_admi_1-nombre,
      sdo_nombre    TYPE zpre_admi_1-sdo_nombre,
      apellido      TYPE zpre_admi_1-apellido,
      sdo_apellido  TYPE zpre_admi_1-sdo_apellido,
      fech_nacim    TYPE zpre_admi_1-fech_nacim,
      direccion     TYPE zpre_admi_1-direccion,
      pais          TYPE zpre_admi_1-pais,
      departamento  TYPE zpre_admi_1-departamento,
      municipio     TYPE zpre_admi_1-municipio,
      estrato       TYPE zpre_admi_1-estrato,
      tel_movil     TYPE zpre_admi_1-tel_movil,
      tel_fijo      TYPE zpre_admi_1-tel_fijo,
      email         TYPE zpre_admi_1-email,
      sdo_email     TYPE zpre_admi_1-sdo_email,
      genero        TYPE zpre_admi_1-genero,
      estado_civil  TYPE zpre_admi_1-estado_civil,
      eps           TYPE zpre_admi_1-eps,
*	Begin	-->	MgM DCEK901698 ajuste x renombre de campo ciudad_alter 23/11/2016
*      ciudad_alter  TYPE zpre_admi_1-ciudad_alter,
      ciudad_alter  TYPE zpre_admi_1-ciudad_fuera,
*	End	  -->	MgM DCEK901698
    END OF ty_admi_1 .
  types:
    BEGIN OF ty_admi_2,
      nr_formulario  TYPE zpre_admi_2-nr_formulario,
      facultad       TYPE zpre_admi_2-facultad,
      tipo_program   TYPE zpre_admi_2-tipo_program,
      t_posgrado     TYPE zpre_admi_2-t_posgrado,
      sede           TYPE zpre_admi_2-sede,
      periodo_acad   TYPE zpre_admi_2-periodo_acad,
      tipo_aspirante TYPE zpre_admi_2-tipo_aspirante,
      programa_1     TYPE zpre_admi_2-programa_1,
      programa_2     TYPE zpre_admi_2-programa_2,
      medio          TYPE zpre_admi_2-medio,
    END OF ty_admi_2 .
  types:
    BEGIN OF ty_admi_3,
      nr_formulario TYPE zpre_admi_3-nr_formulario,
*	Begin	-->	MgM DCEK903458 modificaron nombres en estructura DDIC 13/02/2017
*      registro_snp  TYPE zpre_admi_3-registro_snp,
*      colegio_proc  TYPE zpre_admi_3-colegio_proc,
*      ciudad_coleg  TYPE zpre_admi_3-ciudad_coleg,
*      titulo_coleg  TYPE zpre_admi_3-insti_idioma,
*      ano_grado     TYPE zpre_admi_3-ano_grado,
*      univ_procede  TYPE zpre_admi_3-univ_procede,
*      titulo_univ   TYPE zpre_admi_3-titulo_univ,
*      ciudad_univ   TYPE zpre_admi_3-ciudad_univ,
*      ano_gruni     TYPE zpre_admi_3-ano_gruni,
*      idioma        TYPE zpre_admi_3-idioma,
*      insti_idioma  TYPE zpre_admi_3-insti_idioma,
*      ciudad_idio   TYPE zpre_admi_3-ciudad_idio,
*      prueba        TYPE zpre_admi_3-prueba,
*      punta_idioma  TYPE zpre_admi_3-punta_idioma,
*      curso         TYPE zpre_admi_3-curso,
*      insti_curso   TYPE zpre_admi_3-insti_curso,
*      ciudad_curs   TYPE zpre_admi_3-ciudad_curs,
*      titulo_curs   TYPE zpre_admi_3-titulo_curs,
*	End	  -->	MgM DCEK903458
    END OF ty_admi_3 .
  types:
    BEGIN OF ty_admi_4,
      nr_formulario TYPE zpre_admi_4-nr_formulario,
      vivecon       TYPE zpre_admi_4-vivecon,
    END OF ty_admi_4 .
  types:
    BEGIN OF ty_admi_5,
      nr_formulario TYPE zpre_admi_5-nr_formulario,
      empresa       TYPE zpre_admi_5-empresa,
      cargo         TYPE zpre_admi_5-cargo,
      fech_inic     TYPE zpre_admi_5-fech_inic,
      fech_fina     TYPE zpre_admi_5-fech_fina,
      duracion      TYPE zpre_admi_5-duracion,
      partic_inv    TYPE zpre_admi_5-partic_inv,
      nomb_inv      TYPE zpre_admi_5-nomb_inv,
      publicacion   TYPE zpre_admi_5-publicacion,
      nomb_inv2     TYPE zpre_admi_5-nomb_inv2,
    END OF ty_admi_5 .
  types:
    BEGIN OF ty_range_nr_formulario,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-nr_formulario,
      high   TYPE zpre_admi_1-nr_formulario,
    END OF ty_range_nr_formulario .
  types:
    BEGIN OF ty_range_programa,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_2-tipo_program,
      high   TYPE zpre_admi_2-tipo_program,
    END OF ty_range_programa .
  types:
    BEGIN OF ty_range_facultad,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_2-facultad,
      high   TYPE zpre_admi_2-facultad,
    END OF ty_range_facultad .
  types:
    BEGIN OF ty_range_periodo,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_2-periodo_acad,
      high   TYPE zpre_admi_2-periodo_acad,
    END OF ty_range_periodo .
  types:
    BEGIN OF ty_range_plan_estudio,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_2-programa_1,
      high   TYPE zpre_admi_2-programa_1,
    END OF ty_range_plan_estudio .
  types:
    BEGIN OF ty_range_preposgrado,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_2-programa_1,
      high   TYPE zpre_admi_2-programa_1,
    END OF ty_range_preposgrado .
  types:
    BEGIN OF ty_range_nro_documen,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-nro_documen,
      high   TYPE zpre_admi_1-nro_documen,
    END OF ty_range_nro_documen .
  types:
    BEGIN OF ty_range_nombre,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-nombre,
      high   TYPE zpre_admi_1-nombre,
    END OF ty_range_nombre .
  types:
    BEGIN OF ty_range_sdo_nombre,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-sdo_nombre,
      high   TYPE zpre_admi_1-sdo_nombre,
    END OF ty_range_sdo_nombre .
  types:
    BEGIN OF ty_range_apellido,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-apellido,
      high   TYPE zpre_admi_1-apellido,
    END OF ty_range_apellido .
  types:
    BEGIN OF ty_range_sdo_apellido,
      sign   TYPE sign,
      option TYPE option,
      low    TYPE zpre_admi_1-sdo_apellido,
      high   TYPE zpre_admi_1-sdo_apellido,
    END OF ty_range_sdo_apellido .
  types:
    tt_admi_1 TYPE TABLE OF ty_admi_1 .
  types:
    tt_admi_2 TYPE TABLE OF ty_admi_2 .
  types:
    tt_admi_3 TYPE TABLE OF ty_admi_3 .
  types:
    tt_admi_4 TYPE TABLE OF ty_admi_4 .
  types:
    tt_admi_5 TYPE TABLE OF ty_admi_5 .
  types:
    tt_range_programa TYPE TABLE OF ty_range_programa .
  types:
    tt_range_facultad TYPE TABLE OF ty_range_programa .
  types:
    tt_range_periodo TYPE TABLE OF ty_range_periodo .
  types:
    tt_range_preposgrado TYPE TABLE OF ty_range_programa .
  types:
    tt_range_plan_estudio TYPE TABLE OF ty_range_plan_estudio .
  types:
    tt_range_nro_documen TYPE TABLE OF ty_range_nro_documen .
  types:
    tt_range_nombre TYPE TABLE OF ty_range_nombre .
  types:
    tt_range_sdo_nombre TYPE TABLE OF ty_range_sdo_nombre .
  types:
    tt_range_apellido TYPE TABLE OF ty_range_apellido .
  types:
    tt_range_sdo_apellido TYPE TABLE OF ty_range_sdo_apellido .
  types:
    tt_range_nr_formulario TYPE TABLE OF ty_range_nr_formulario .

  methods CARGAR_RANGOS
    importing
      !IV_SELECCION type ZEDU_S_SEL_PRU_ADMI
      !IT_ADMI_2 type TT_ADMI_2 optional
    exporting
      !ER_NR_FORMULARIO type TT_RANGE_NR_FORMULARIO
      !ER_PROGRAMA type TT_RANGE_PROGRAMA
      !ER_FACULTAD type TT_RANGE_FACULTAD
      !ER_PERIODO type TT_RANGE_PERIODO
      !ER_PREPOSGRADO type TT_RANGE_PREPOSGRADO
      !ER_PLAN_ESTUDIO type TT_RANGE_PLAN_ESTUDIO
      !ER_NRO_DOCUMENTO type TT_RANGE_NRO_DOCUMEN
      !ER_NOMBRE type TT_RANGE_NOMBRE
      !ER_SDO_NOMBRE type TT_RANGE_SDO_NOMBRE
      !ER_APELLIDO type TT_RANGE_APELLIDO
      !ER_SDO_APELLIDO type TT_RANGE_SDO_APELLIDO .
  methods VALIDA_ESTUDIANTE_INSCRIPTO
    changing
      !CH_ADMI_1 type TT_ADMI_1
      !CH_ADMI_2 type TT_ADMI_2 .
ENDCLASS.



CLASS ZWDC_PRU_ADMI_ASS IMPLEMENTATION.


  METHOD cargar_rangos.

    DATA: lrs_nr_formulario TYPE ty_range_nr_formulario,
          lrs_programa      TYPE ty_range_programa,
          lrs_facultad      TYPE ty_range_facultad,
          lrs_periodo       TYPE ty_range_periodo,
          lrs_preposgrado   TYPE ty_range_preposgrado,
          lrs_plan_estudio  TYPE ty_range_plan_estudio,
          lrs_nro_documento TYPE ty_range_nro_documen,
          lrs_nombre        TYPE ty_range_nombre,
          lrs_sdo_nombre    TYPE ty_range_sdo_nombre,
          lrs_apellido      TYPE ty_range_apellido,
          lrs_sdo_apellido  TYPE ty_range_sdo_apellido,
          lv_nombre(40).

    FIELD-SYMBOLS: <fs_admi_2> TYPE ty_admi_2.

    IF it_admi_2[] IS NOT INITIAL.
      lrs_nr_formulario = 'IEQ'.
      LOOP AT it_admi_2 ASSIGNING <fs_admi_2>.
        lrs_nr_formulario-low = <fs_admi_2>-nr_formulario.
        APPEND lrs_nr_formulario TO er_nr_formulario.
      ENDLOOP.
    ENDIF.

    IF iv_seleccion-programa IS NOT INITIAL.
      lrs_programa = 'IEQ'.
      lrs_programa-low = iv_seleccion-programa.
      APPEND lrs_programa TO er_programa.
    ENDIF.

    IF iv_seleccion-facultad IS NOT INITIAL.
      lrs_facultad = 'IEQ'.
      lrs_facultad-low = iv_seleccion-facultad.
      APPEND lrs_facultad TO er_facultad.
    ENDIF.

    IF iv_seleccion-periodo IS NOT INITIAL.
      lrs_periodo = 'IEQ'.
      lrs_periodo-low = iv_seleccion-periodo.
      APPEND lrs_periodo TO er_periodo.
    ENDIF.

    IF iv_seleccion-preposgrado IS NOT INITIAL.
      lrs_preposgrado = 'IEQ'.
      lrs_preposgrado-low = iv_seleccion-preposgrado.
      APPEND lrs_preposgrado TO er_preposgrado.
    ENDIF.

    IF iv_seleccion-plan_estudio IS NOT INITIAL.
      lrs_plan_estudio = 'IEQ'.
      lrs_plan_estudio-low = iv_seleccion-plan_estudio.
      APPEND lrs_plan_estudio TO er_plan_estudio.
    ENDIF.

    IF iv_seleccion-nro_documen IS NOT INITIAL.
      lrs_nro_documento = 'IEQ'.
      lrs_nro_documento-low = iv_seleccion-nro_documen.
      APPEND lrs_nro_documento TO er_nro_documento.
    ENDIF.

    IF iv_seleccion-nombre IS NOT INITIAL.
      lrs_nombre = 'IEQ'.
      lv_nombre = iv_seleccion-nombre.
      me->formateo_nombres( CHANGING ch_nombre = lv_nombre ).
      lrs_nombre-low = lv_nombre.
      APPEND lrs_nombre TO er_nombre.
    ENDIF.

    IF iv_seleccion-sdo_nombre IS NOT INITIAL.
      lrs_sdo_nombre = 'IEQ'.
      lv_nombre = iv_seleccion-sdo_nombre.
      me->formateo_nombres( CHANGING ch_nombre = lv_nombre ).
      lrs_sdo_nombre-low = lv_nombre.
      APPEND lrs_sdo_nombre TO er_sdo_nombre.
    ENDIF.

    IF iv_seleccion-apellido IS NOT INITIAL.
      lrs_apellido = 'IEQ'.
      lv_nombre = iv_seleccion-apellido.
      me->formateo_nombres( CHANGING ch_nombre = lv_nombre ).
      lrs_apellido-low = lv_nombre.
      APPEND lrs_apellido TO er_apellido.
    ENDIF.

    IF iv_seleccion-sdo_apellido IS NOT INITIAL.
      lrs_sdo_apellido = 'IEQ'.
      lv_nombre = iv_seleccion-sdo_apellido.
      me->formateo_nombres( CHANGING ch_nombre = lv_nombre ).
      lrs_sdo_apellido-low = lv_nombre.
      APPEND lrs_sdo_apellido TO er_sdo_apellido.
    ENDIF.

  ENDMETHOD.


  method get_candidatos.

    data: lt_admi_1    type tt_admi_1,
          lt_admi_3    type tt_admi_3,
          ls_resultado type zedu_s_tab_pru_admi,
          lv_condicion type string.

    field-symbols: <fs_admi_1> type ty_admi_1,
                   <fs_admi_3> type ty_admi_3.

    try.
        select nr_formulario
               nro_documen
               nombre
               sdo_nombre
               apellido
               sdo_apellido
               direccion
               municipio
               estrato
               tel_movil
               tel_fijo
               email
               genero
               estado_civil
               eps
               tipo_documen
          from zpre_admi_1
          into CORRESPONDING FIELDS OF TABLE lt_admi_1
          where (lv_condicion).

        if lt_admi_1[] is not initial.

          select  nr_formulario
                  colegio as colegio_proc
                  ciudad as ciudad_coleg
            from zpre_admi_3
              into table lt_admi_3
                for all entries in lt_admi_1[]
                  where nr_formulario = lt_admi_1-nr_formulario.

        endif.

      catch cx_sy_dynamic_osql_error.
    endtry.

    loop at lt_admi_1 assigning <fs_admi_1>.
      move-corresponding <fs_admi_1> to ls_resultado.
      read table lt_admi_3 assigning <fs_admi_3> with key nr_formulario = <fs_admi_1>-nr_formulario.
      if sy-subrc is initial.
        move-corresponding <fs_admi_3> to ls_resultado.
      endif.
      append ls_resultado to rt_resultado.
      clear ls_resultado.
    endloop.

  endmethod.


  method get_ddbk_anio_periodo.

    types: begin of ty_hrp1001,
             otype type hrp1001-otype,
             objid type hrp1001-objid,
             plvar type hrp1001-plvar,
             sclas type hrp1001-sclas,
             sobid type hrp1001-sobid,
           end of ty_hrp1001,

           begin of ty_hrp1750,
             plvar type hrp1750-plvar,
             otype type hrp1750-otype,
             objid type hrp1750-objid,
             peryr type hrp1750-peryr,
             tabnr type hrp1750-tabnr,
           end of ty_hrp1750,

           begin of ty_hrt1750,
             tabnr     type hrt1750-tabnr,
             tabseqnr  type hrt1750-tabseqnr,
             timelimit type hrt1750-timelimit,
             peryr     type hrt1750-peryr,
             perid     type hrt1750-perid,
           end of ty_hrt1750.

    data: lt_hrp1001     type table of ty_hrp1001,
*  Begin  -->  MgM DCEK908199 combo períodos 13/04/2018
*          lt_hrp1750     TYPE TABLE OF ty_hrp1750,
*          lt_hrt1750     TYPE TABLE OF ty_hrt1750,
          lt_hrp1750     type sorted table of ty_hrp1750
                              with non-unique key primary_key
                                components tabnr,
          lt_hrt1750     type sorted table of ty_hrt1750
                              with non-unique key primary_key
                                components peryr  perid,
*  End    -->  MgM DCEK908199
          lr_objid       type range of hrobjid,
          lrs_objid      like line of lr_objid,
          lt_peri_descr  type zedu_t_context_attr_value,
          lt_anio_descr  type zedu_t_context_attr_value,
          ls_peri_descr  type wdr_context_attr_value,
          ls_anio_descr  type wdr_context_attr_value,
          ls_attr_value  type wdr_context_attr_value,
          lv_anio_act(4),
          lv_anio_sig(4).

    field-symbols: <fs_hrt1750> type ty_hrt1750,
                   <fs_hrp1001> type ty_hrp1001.

*  Begin  -->  MgM DCEK908039  21/03/2018
*    SELECT otype objid plvar sclas sobid
*       FROM hrp1001
*       INTO TABLE lt_hrp1001
*      WHERE sobid  = iv_programa " Programa seleccionado
*         AND otype = 'CA'
*         AND sclas = 'O'.
*
*    DELETE lt_hrp1001 WHERE sclas NE 'O'.
*
*    lrs_objid = 'IEQ'.
*    LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001>.
*      lrs_objid-low = <fs_hrp1001>-objid.
*      APPEND lrs_objid TO lr_objid.
*    ENDLOOP.
*  End    -->  MgM DCEK908039

    select plvar otype objid peryr tabnr
    from hrp1750
    into table lt_hrp1750
    where plvar = '01'
      and otype = 'CA'
      and objid in lr_objid
        order by tabnr.

    lv_anio_act = sy-datum(4).
    lv_anio_sig = lv_anio_act + 1.

    delete lt_hrp1750
      where peryr ne lv_anio_act
        and peryr ne lv_anio_sig.

*  Begin  -->  MgM DCEK908039  21/03/2018
    check lt_hrp1750[] is not initial.

    delete adjacent duplicates from lt_hrp1750
      comparing tabnr.
*  End    -->  MgM DCEK908039

    select tabnr tabseqnr timelimit peryr perid
      from hrt1750
        into table lt_hrt1750
          for all entries in lt_hrp1750
            where tabnr = lt_hrp1750-tabnr.

*    DELETE lt_hrt1750 WHERE timelimit NE 'ADMI'. "-->  MgM DCEK908199

*  Begin  -->  MgM DCEK908199 combo períodos 13/04/2018
    delete adjacent duplicates from lt_hrt1750 comparing peryr
                                                         perid.
*  End    -->  MgM DCEK908199
    select perid as value
           perit as text
      from t7piqperiodt
        into table lt_peri_descr
          where spras eq sy-langu
            order by perid.

    select peryr as value
           peryt as text
     from t7piqyeart
       into table lt_anio_descr
         where spras eq sy-langu
           order by peryr.

    loop at lt_hrt1750 assigning <fs_hrt1750>.
      read table lt_anio_descr
        into ls_anio_descr
          with key value = <fs_hrt1750>-peryr binary search.

      read table lt_peri_descr
        into ls_peri_descr
          with key value = <fs_hrt1750>-perid binary search.

      concatenate <fs_hrt1750>-peryr
                  <fs_hrt1750>-perid
        into ls_attr_value-value.

      concatenate ls_anio_descr-text
                  ls_peri_descr-text
        into ls_attr_value-text
          separated by '-'.
      append ls_attr_value to rt_attr_value.
    endloop.

  endmethod.


  METHOD get_ddbk_pruebas.
    DATA: lt_ttype      TYPE TABLE OF t7piqtrtesttypet,
          ls_ttype      TYPE t7piqtrtesttypet,
          ls_attr_value TYPE wdr_context_attr_value.

    SELECT * FROM t7piqtrtesttypet
      INTO TABLE lt_ttype
      FOR ALL ENTRIES IN it_pruebas
      WHERE spras    EQ sy-langu AND
            testtype EQ it_pruebas-testtype.
    LOOP AT lt_ttype INTO ls_ttype.
      ls_attr_value-text  = ls_ttype-testtypetext.
      ls_attr_value-value = ls_ttype-testtype.
      APPEND ls_attr_value TO rt_ddbk_pruebas.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_fecha_exam_entr.

    DATA: lt_fechas     TYPE zedu_t_fec_exam_entrevista,
          ls_attr_value TYPE wdr_context_attr_value.

    FIELD-SYMBOLS: <fs_fechas> TYPE zedu_s_fec_exam_entrevista.

    CALL FUNCTION 'Z_EDU_GET_FECHA_EXAMEN_ENTREV'
      EXPORTING
        iv_plan_estudio = iv_plan_estudio
        iv_anio         = iv_anio
        iv_periodo      = iv_periodo
        iv_tipo_fecha   = iv_tipo_fecha
      TABLES
        et_fechas       = lt_fechas.

*** Filtro la tabla global para q no se repitan los estudiantes.
    IF iv_tipo_fecha = 'EN'.
      APPEND LINES OF lt_fechas TO gt_fechas_en.
*      SORT gt_fechas_en BY objid.
*      DELETE ADJACENT DUPLICATES FROM gt_fechas_en COMPARING objid.
    ELSEIF iv_tipo_fecha = 'EX'.
      APPEND LINES OF lt_fechas TO gt_fechas_ex.
*      SORT gt_fechas_ex BY objid.
*      DELETE ADJACENT DUPLICATES FROM gt_fechas_ex COMPARING objid.
    ENDIF.


*** Filtro la tabla local para q no se repitan las fechas.
    SORT lt_fechas BY examdate.
    DELETE ADJACENT DUPLICATES FROM lt_fechas COMPARING examdate.
    LOOP AT lt_fechas ASSIGNING <fs_fechas>.
      ls_attr_value-value = <fs_fechas>-examdate.
      CONCATENATE <fs_fechas>-examdate+6(2)
                  <fs_fechas>-examdate+4(2)
                  <fs_fechas>-examdate(4)
                  INTO ls_attr_value-text SEPARATED BY '/'.
      APPEND ls_attr_value TO rt_attr_value.
    ENDLOOP.



  ENDMETHOD.


  method GET_NAME_USERS.

   SELECT *
    FROM user_addr
    INTO TABLE rt_user_addr
    FOR ALL ENTRIES IN it_users
    WHERE bname = it_users-table_line.

  endmethod.


  METHOD get_observacion.

    DATA: lv_keyobs TYPE zedu_pradm-keyobs.

*Inicio M7394 - HRESTREPO - 31/08/2018
    "Inicializa retornos
    CLEAR:
      ev_observacion,
      ev_concepto,
      ev_momento.
*Fin M7394 - HRESTREPO - 31/08/2018

    SELECT SINGLE momento concepto keyobs       "M7394 - HRESTREPO - 31/08/2018 - Add Momento
      FROM zedu_pradm
      INTO (ev_momento, ev_concepto, lv_keyobs) "M7394 - HRESTREPO - 31/08/2018 - Add Momento
      WHERE testtype  = iv_testtype
        AND adm_ayear = iv_adm_ayear
        AND adm_perid = iv_adm_perid
        AND objid_st  = iv_objid_st.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE valoracion
      FROM zedu_pradt
      INTO ev_observacion
      WHERE keyobs = lv_keyobs.

  ENDMETHOD.


  method GET_PRUEBAS.
  endmethod.


  METHOD get_pruebas_admision.

    TYPES: BEGIN OF ty_ident,
             type     TYPE but0id-type,
             idnumber TYPE but0id-idnumber,
           END OF ty_ident.

    DATA: lt_cmacbpst      TYPE TABLE OF cmacbpst,
          lt_ident         TYPE TABLE OF ty_ident,
          ls_ident         TYPE ty_ident,
          lt_admi_1        TYPE tt_admi_1,
          lt_admi_2        TYPE tt_admi_2,
*          lt_admi_3        type tt_admi_3,   "-->  MgM DCEK908039  21/03/2018
          lt_admi_4        TYPE tt_admi_4,
          lt_admi_5        TYPE tt_admi_5,
          lt_fechas        TYPE zedu_t_fec_exam_entrevista,
          lr_programa	     TYPE tt_range_programa,
          lr_facultad	     TYPE tt_range_facultad,
          lr_periodo       TYPE tt_range_periodo,
          lr_plan_estudio  TYPE tt_range_plan_estudio,
          lr_preposgrado   TYPE tt_range_preposgrado,
          lr_nr_formulario TYPE tt_range_nr_formulario,
          lr_nro_documento TYPE tt_range_nro_documen,
          lr_nombre	       TYPE tt_range_nombre,
          lr_sdo_nombre	   TYPE tt_range_sdo_nombre,
          lr_apellido	     TYPE tt_range_apellido,
          lr_sdo_apellido	 TYPE tt_range_sdo_apellido,
          ls_resultado     TYPE zedu_s_result_prue_adm,
          lv_condicion     TYPE string.
*	Begin	-->	MgM DCEK908039  21/03/2018
    TYPES:BEGIN OF lty_prog2,
            programa_2 TYPE hrobjid,
          END OF lty_prog2.
    TYPES:  BEGIN OF lty_eps_n,
              eps TYPE n LENGTH 4,
            END OF lty_eps_n.
    DATA lt_prog_2 TYPE STANDARD TABLE OF lty_prog2.
    DATA lt_eps_n TYPE STANDARD TABLE OF lty_eps_n.
*	End	  -->	MgM DCEK908039

    FIELD-SYMBOLS: <fs_admi_1> TYPE ty_admi_1,
                   <fs_admi_2> TYPE ty_admi_2,
*                   <fs_admi_3> type ty_admi_3, "-->  MgM DCEK908039  21/03/2018
                   <fs_admi_4> TYPE ty_admi_4,
                   <fs_admi_5> TYPE ty_admi_5.


    cargar_rangos( EXPORTING iv_seleccion     = iv_seleccion
                             it_admi_2        = lt_admi_2
                   IMPORTING er_nr_formulario = lr_nr_formulario
                             er_programa      = lr_programa
                             er_facultad      = lr_facultad
                             er_periodo       = lr_periodo
                             er_plan_estudio  = lr_plan_estudio
                             er_preposgrado   = lr_preposgrado
                             er_nro_documento = lr_nro_documento
                             er_nombre        = lr_nombre
                             er_sdo_nombre    = lr_sdo_nombre
                             er_apellido      = lr_apellido
                             er_sdo_apellido  = lr_sdo_apellido ).

    IF lr_facultad IS NOT INITIAL OR
       lr_programa IS NOT INITIAL OR
       lr_periodo  IS NOT INITIAL OR
       lr_plan_estudio IS NOT INITIAL.

      SELECT nr_formulario
             facultad
             tipo_program
             t_posgrado
             sede
             periodo_acad
             tipo_aspirante
             programa_1
             programa_2
             medio
       FROM zpre_admi_2
       INTO TABLE lt_admi_2
       WHERE facultad     IN lr_facultad
         AND tipo_program IN lr_programa
         AND periodo_acad IN lr_periodo
*Inicio M7394 - HRESTREPO - 10/09/2018
*         and programa_1   in lr_plan_estudio
         AND ( programa_1 IN lr_plan_estudio OR
               programa_2 IN lr_plan_estudio )
*Fin M7394 - HRESTREPO - 10/09/2018
         AND t_posgrado   IN lr_preposgrado.


    ENDIF.

    IF lt_admi_2 IS NOT INITIAL.

      cargar_rangos( EXPORTING iv_seleccion     = iv_seleccion
                               it_admi_2        = lt_admi_2
                     IMPORTING er_nr_formulario = lr_nr_formulario ).

    ELSE.
      EXIT.
    ENDIF.

    SELECT nr_formulario
           tipo_documen
           nro_documen
           nombre
           sdo_nombre
           apellido
           sdo_apellido
           fech_nacim
           direccion
           pais
           departamento
           municipio
           estrato
           tel_movil
           tel_fijo
           email
           sdo_email
           genero
           estado_civil
           eps
*	Begin	-->	MgM DCEK901698 ajuste x renombre de campo ciudad_alter 23/11/2016
*           ciudad_alter
           ciudad_fuera
*	End	  -->	MgM DCEK901698
       FROM zpre_admi_1
      INTO TABLE lt_admi_1
      WHERE nr_formulario IN lr_nr_formulario
        AND nro_documen   IN lr_nro_documento
        AND nombre        IN lr_nombre
        AND sdo_nombre    IN lr_sdo_nombre
        AND apellido      IN lr_apellido
        AND sdo_apellido  IN lr_sdo_apellido.

    IF lt_admi_2 IS INITIAL AND lt_admi_1 IS NOT INITIAL.
      SELECT nr_formulario
             facultad
             tipo_program
             t_posgrado
             sede
             periodo_acad
             tipo_aspirante
             programa_1
             programa_2
             medio
       FROM zpre_admi_2
       INTO TABLE lt_admi_2
       FOR ALL ENTRIES IN lt_admi_1
       WHERE nr_formulario = lt_admi_1-nr_formulario.
    ENDIF.


    IF lt_admi_1[] IS NOT INITIAL.
* Inicio comentado por error sintaxis: Leonardo Pavia ( ABAP_ADP ), del 07/04/2017
      SELECT nr_formulario,
             snp AS registro_snp,
*             ciudad_coleg
*             titulo_coleg
*             ano_grado
*             univ_procede
*             titulo_univ
*             ciudad_univ
*             ano_gruni
*             idioma
*             insti_idioma
*             ciudad_idio
*             prueba
*             punta_idioma
*             curso
*             insti_curso
*             ciudad_curs
*             titulo_curs
        ciudad AS ciudad_coleg,
        colegio AS colegio_proc,
        grado_act,
        titulo AS titulo_coleg,
        saber_11,
        ano AS ano_grado,
        universidad AS univ_procede,
        ciudad_u AS ciudad_univ,
        titulo_u AS titulo_univ,
        ano_u AS ano_gruni,
        idioma,
        institucion_i AS insti_idioma,
        prueba,
        puntaje_i AS punta_idioma,
        curso,
        institucion_c AS insti_curso,
        titulo_c AS titulo_curs,
        pais_proc AS pais_coleg,   "País proced. aspirante:
        dpto_proc AS depart_coleg, "Depto. proced. aspirante:
*        ciudad_proc "Ciudad proced. aspirante:
        tarjeta_pr1   " -->	MgM DCEK908500 18/06/2018
       FROM zpre_admi_3
*       INTO TABLE lt_admi_3
        INTO TABLE @DATA(lt_admi_3)
       FOR ALL ENTRIES IN @lt_admi_1[]
       WHERE nr_formulario EQ @lt_admi_1-nr_formulario.
* Fin comentado por error sintaxis: Leonardo Pavia ( ABAP_ADP ), del 07/04/2017

      IF sy-subrc EQ 0.
        SELECT  codigo,
                idioma
          FROM zedu_param_idiom
            INTO TABLE @DATA(lt_idioma)
              FOR ALL ENTRIES IN @lt_admi_3
                WHERE codigo EQ @lt_admi_3-idioma.
      ENDIF.

*	Begin	-->	MgM DCEK908039  21/03/2018
      "Recupero regiones
      SELECT  spras,
              land1,
              bland,
              bezei
        FROM t005u
          INTO TABLE @DATA(lt_regios)
            FOR ALL ENTRIES IN @lt_admi_1
              WHERE spras EQ @sy-langu
                AND land1 EQ @lt_admi_1-pais
                AND bland EQ @lt_admi_1-departamento
        ORDER BY PRIMARY KEY.

      SELECT  spras,
              land1,
              bland,
              bezei
        FROM t005u
          APPENDING TABLE @lt_regios
            FOR ALL ENTRIES IN @lt_admi_3
              WHERE spras EQ @sy-langu
                AND land1 EQ @lt_admi_3-pais_coleg
                AND bland EQ @lt_admi_3-depart_coleg
        ORDER BY PRIMARY KEY.

      "Recupero Municipios
      SELECT  spras,
              land1,
              regio,
              cityc,
              bezei
        FROM t005h
          INTO TABLE @DATA(lt_municipios)
            FOR ALL ENTRIES IN @lt_admi_1
              WHERE spras EQ @sy-langu
                AND land1 EQ @lt_admi_1-pais
                AND regio EQ @lt_admi_1-departamento
                AND cityc EQ @lt_admi_1-municipio
        ORDER BY PRIMARY KEY.

      SELECT  spras,
              land1,
              regio,
              cityc,
              bezei
        FROM t005h
          APPENDING TABLE @lt_municipios
            FOR ALL ENTRIES IN @lt_admi_3
              WHERE spras EQ @sy-langu
                AND land1 EQ @lt_admi_3-pais_coleg
                AND regio EQ @lt_admi_3-depart_coleg
                AND ( cityc EQ @lt_admi_3-ciudad_coleg OR
                      cityc EQ @lt_admi_3-ciudad_univ )
        ORDER BY PRIMARY KEY.

      MOVE-CORRESPONDING lt_admi_1 TO lt_eps_n.
      SORT lt_eps_n.
      DELETE ADJACENT DUPLICATES FROM lt_eps_n.

      "Recupero descripción de EPS
      SELECT  codigo,
              descripcion
        FROM zedu_eps
          INTO TABLE @DATA(lt_eps_desc)
            FOR ALL ENTRIES IN @lt_eps_n
              WHERE codigo EQ @lt_eps_n-eps
        ORDER BY PRIMARY KEY.
*	End	  -->	MgM DCEK908039

      SELECT nr_formulario
             vivecon
       FROM zpre_admi_4
       INTO TABLE lt_admi_4
       FOR ALL ENTRIES IN lt_admi_1[]
       WHERE nr_formulario = lt_admi_1-nr_formulario.

      SELECT nr_formulario
             empresa
             cargo
             fech_inic
             fech_fina
             duracion
             partic_inv
             nomb_inv
             publicacion
             nomb_inv2
        FROM zpre_admi_5
          INTO TABLE lt_admi_5
            FOR ALL ENTRIES IN lt_admi_1[]
          WHERE nr_formulario = lt_admi_1-nr_formulario.

      IF sy-subrc EQ 0.

        TYPES:  BEGIN OF lty_cargo,
                  cargo TYPE taete,
                END OF lty_cargo.
        DATA lt_cargos TYPE STANDARD TABLE OF lty_cargo
                WITH NON-UNIQUE SORTED KEY cargo COMPONENTS cargo.
        MOVE-CORRESPONDING lt_admi_5 TO lt_cargos.

        DELETE ADJACENT DUPLICATES FROM lt_cargos.

        SELECT  spras,
                taete,
                ltext
          FROM t513c
            INTO TABLE @DATA(lt_actividades)
              FOR ALL ENTRIES IN @lt_cargos
                WHERE spras EQ @sy-langu
                  AND taete EQ @lt_cargos-cargo.
      ENDIF.
    ENDIF.

*	Begin	-->	MgM DCEK908039  21/03/2018
    IF lt_admi_2 IS NOT INITIAL.
      "Recuperamos descripción de categorías
      SELECT  spras,
              adm_categ,
              adm_categt
        FROM t7piqadmcategt
          INTO TABLE @DATA(lt_categorias)
            FOR ALL ENTRIES IN @lt_admi_2
              WHERE spras     EQ @sy-langu
                AND adm_categ EQ @lt_admi_2-tipo_aspirante(2)
        ORDER BY PRIMARY KEY.

      "Recuperamos descripción de medios
      SELECT  id,
              medio
        FROM zedu_medios
          INTO TABLE @DATA(lt_medios)
            FOR ALL ENTRIES IN @lt_admi_2
              WHERE id EQ @lt_admi_2-medio(2)
        ORDER BY PRIMARY KEY.

      MOVE-CORRESPONDING lt_admi_2 TO lt_prog_2.
      SORT lt_prog_2.
      DELETE ADJACENT DUPLICATES FROM lt_prog_2.

      "Recuperamos descripción de programas
      SELECT  objid,
              stext
        FROM hrp1000
          INTO TABLE @DATA(lt_prog_desc)
            FOR ALL ENTRIES IN @lt_prog_2
              WHERE objid EQ @lt_prog_2-programa_2.

    ENDIF.
*	End	  -->	MgM DCEK908039

*** Valido q esten inscriptos.
    me->valida_estudiante_inscripto( CHANGING  ch_admi_1    = lt_admi_1
                                               ch_admi_2    = lt_admi_2 ).

*** Filtro por fechas de examen y entrevista
    IF gt_fechas_en IS NOT INITIAL AND iv_seleccion-fecha_entrevista IS NOT INITIAL.

      REFRESH lt_fechas[].
*	Begin	-->	MgM DCEK908500 fecha entrev. sin filtro 18/06/2018
*      lt_fechas[] = gt_fechas_ex[].
      lt_fechas[] = gt_fechas_en[].
*	End	  -->	MgM DCEK908500
      DELETE lt_fechas WHERE examdate NE iv_seleccion-fecha_entrevista.

      SELECT *
        FROM cmacbpst
        INTO TABLE lt_cmacbpst
        FOR ALL ENTRIES IN lt_fechas
        WHERE stobjid = lt_fechas-objid.

      SELECT type idnumber
        FROM but0id
        INTO TABLE lt_ident
        FOR ALL ENTRIES IN lt_cmacbpst
        WHERE partner = lt_cmacbpst-partner.

      LOOP AT lt_admi_1 ASSIGNING <fs_admi_1>.
        CLEAR ls_ident.
        READ TABLE lt_ident INTO ls_ident WITH KEY type     = <fs_admi_1>-tipo_documen
                                                   idnumber = <fs_admi_1>-nro_documen.
        IF sy-subrc IS NOT INITIAL.
          DELETE TABLE lt_admi_1 FROM <fs_admi_1>.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF gt_fechas_ex IS NOT INITIAL AND iv_seleccion-fecha_examen IS NOT INITIAL.

      REFRESH lt_fechas[].
      lt_fechas[] = gt_fechas_ex[].
      DELETE lt_fechas WHERE examdate NE iv_seleccion-fecha_examen.

      SELECT *
        FROM cmacbpst
        INTO TABLE lt_cmacbpst
        FOR ALL ENTRIES IN lt_fechas
        WHERE stobjid = lt_fechas-objid.

      SELECT type idnumber
        FROM but0id
        INTO TABLE lt_ident
        FOR ALL ENTRIES IN lt_cmacbpst
        WHERE partner = lt_cmacbpst-partner.

      LOOP AT lt_admi_1 ASSIGNING <fs_admi_1>.
        CLEAR ls_ident.
        READ TABLE lt_ident INTO ls_ident WITH KEY type     = <fs_admi_1>-tipo_documen
                                                   idnumber = <fs_admi_1>-nro_documen.
        IF sy-subrc IS NOT INITIAL.
          DELETE TABLE lt_admi_1 FROM <fs_admi_1>.
        ENDIF.
      ENDLOOP.

    ENDIF.

*** Armo la tabla de salida
    LOOP AT lt_admi_1 ASSIGNING <fs_admi_1>.
      MOVE-CORRESPONDING <fs_admi_1> TO ls_resultado.

*	Begin	-->	MgM DCEK908039  21/03/2018
      READ TABLE lt_regios
        ASSIGNING FIELD-SYMBOL(<fsl_regio>)
          WITH KEY spras  = sy-langu
                   land1  = <fs_admi_1>-pais
                   bland  = <fs_admi_1>-departamento.

      IF sy-subrc EQ 0.
        MOVE <fsl_regio>-bezei TO ls_resultado-departamento.
      ENDIF.

      READ TABLE lt_municipios
        ASSIGNING FIELD-SYMBOL(<fsl_muni>)
          WITH KEY spras = sy-langu
                   land1 = <fs_admi_1>-pais
                   regio = <fs_admi_1>-departamento
                   cityc = <fs_admi_1>-municipio.

      IF sy-subrc EQ 0.
        MOVE <fsl_muni>-bezei TO ls_resultado-municipio.
      ENDIF.

      READ TABLE lt_eps_desc
        ASSIGNING FIELD-SYMBOL(<fsl_eps_desc>)
          WITH KEY codigo = <fs_admi_1>-eps.

      IF sy-subrc EQ 0.
        MOVE <fsl_eps_desc>-descripcion TO ls_resultado-eps.
      ENDIF.
*	End	  -->	MgM DCEK908039

      READ TABLE lt_admi_2 ASSIGNING <fs_admi_2> WITH KEY nr_formulario = <fs_admi_1>-nr_formulario.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_admi_2> TO ls_resultado.
*        ls_resultado-periodo_acad = <fs_admi_2>-periodo_acad.
*	Begin	-->	MgM DCEK908039  21/03/2018
        READ TABLE lt_categorias
          ASSIGNING FIELD-SYMBOL(<fsl_cat>)
            WITH KEY  spras     = sy-langu
                      adm_categ = <fs_admi_2>-tipo_aspirante.

        IF sy-subrc EQ 0.
          MOVE <fsl_cat>-adm_categt TO ls_resultado-tipo_aspirante.
        ENDIF.

        READ TABLE lt_medios
          ASSIGNING FIELD-SYMBOL(<fsl_medio>)
            WITH KEY id = <fs_admi_2>-medio.

        IF sy-subrc EQ 0.
          MOVE <fsl_medio>-medio TO ls_resultado-medio.
        ENDIF.

        "Programa_2
        READ TABLE lt_prog_desc
          ASSIGNING FIELD-SYMBOL(<fsl_prog>)
            WITH KEY objid = <fs_admi_2>-programa_2.

        IF sy-subrc EQ 0.
          MOVE <fsl_prog>-stext TO ls_resultado-programa_2.
        ENDIF.
*	End	  -->	MgM DCEK908039
      ENDIF.
*	Begin	-->	MgM DCEK908039  21/03/2018
*      read table lt_admi_3 assigning <fs_admi_3> with key nr_formulario = <fs_admi_1>-nr_formulario.
      READ TABLE lt_admi_3
        ASSIGNING FIELD-SYMBOL(<fs_admi_3>)
          WITH KEY nr_formulario = <fs_admi_1>-nr_formulario.
*	End	  -->	MgM DCEK908039

      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_admi_3> TO ls_resultado.

        "Ciudad de colegio
        READ TABLE lt_municipios
          ASSIGNING <fsl_muni>
            WITH KEY spras  = sy-langu
                     land1  = <fs_admi_3>-pais_coleg
                     regio  = <fs_admi_3>-depart_coleg
                     cityc  = <fs_admi_3>-ciudad_coleg.

        IF sy-subrc EQ 0.
          MOVE <fsl_muni>-bezei TO ls_resultado-ciudad_coleg.
        ENDIF.

        "Ciudad de Universidad
        READ TABLE lt_municipios
          ASSIGNING <fsl_muni>
            WITH KEY spras  = sy-langu
                     land1  = <fs_admi_3>-pais_coleg
                     regio  = <fs_admi_3>-depart_coleg
                     cityc  = <fs_admi_3>-ciudad_univ.

        IF sy-subrc EQ 0.
          MOVE <fsl_muni>-bezei TO ls_resultado-ciudad_univ.
        ENDIF.

        READ TABLE lt_regios
          ASSIGNING <fsl_regio>
            WITH KEY spras  = sy-langu
                     land1  = <fs_admi_3>-pais_coleg
                     bland  = <fs_admi_3>-depart_coleg.

        IF sy-subrc EQ 0.
          MOVE <fsl_regio>-bezei TO ls_resultado-depart_coleg.
        ENDIF.

        READ TABLE lt_idioma
          ASSIGNING FIELD-SYMBOL(<fsl_idioma>)
            WITH KEY codigo = <fs_admi_3>-idioma.

        IF sy-subrc EQ 0.
          MOVE <fsl_idioma>-idioma TO ls_resultado-idioma.
        ENDIF.

      ENDIF.
      READ TABLE lt_admi_4 ASSIGNING <fs_admi_4> WITH KEY nr_formulario = <fs_admi_1>-nr_formulario.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_admi_4> TO ls_resultado.
      ENDIF.
      READ TABLE lt_admi_5 ASSIGNING <fs_admi_5> WITH KEY nr_formulario = <fs_admi_1>-nr_formulario.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_admi_5> TO ls_resultado.

        READ TABLE lt_actividades
          ASSIGNING FIELD-SYMBOL(<fsl_activ>)
            WITH KEY spras  = sy-langu
                     taete  = <fs_admi_5>-cargo.

        IF sy-subrc EQ 0.
          MOVE <fsl_activ>-ltext TO ls_resultado-cargo.
        ENDIF.
      ENDIF.
      APPEND ls_resultado TO rt_resultado.
      CLEAR ls_resultado.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_resultados.
    IF i_test IS INITIAL.
      SELECT * FROM zedu_pradm
        INTO TABLE rt_resultados
         WHERE prdni EQ i_prdni.
    ELSE.
      SELECT * FROM zedu_pradm
         INTO TABLE rt_resultados
          WHERE testtype EQ i_test AND
                prdni    EQ i_prdni.
    ENDIF.
  ENDMETHOD.


  METHOD get_resultados_from_docs.
    IF it_prdni IS NOT INITIAL.
      SELECT * FROM zedu_pradm
         INTO TABLE rt_resultados
        FOR ALL ENTRIES IN it_prdni
          WHERE prdni EQ it_prdni-table_line.
    ENDIF.
  ENDMETHOD.


  METHOD get_textos_resultados.
    SELECT SINGLE valoracion FROM zedu_pradt
      INTO r_texto
    WHERE keyobs EQ i_key.
  ENDMETHOD.


  METHOD get_textos_resultados_all.

    SELECT *
      FROM zedu_pradt
      INTO TABLE r_textos
      FOR ALL ENTRIES IN it_key
    WHERE keyobs EQ it_key-table_line.

  ENDMETHOD.


  METHOD get_tipos_pruebas.

    SELECT *
      FROM t7piqtrsubtestt
      INTO TABLE rt_tipo_pruebas
    WHERE spras = sy-langu
      AND testtype = 'PRUEBAS_ADMI'.
  ENDMETHOD.


  METHOD grabar_pruebas.

    DATA: ls_error  TYPE bapiret2.

    MODIFY zedu_pradm FROM is_pradm.

    IF sy-subrc IS NOT INITIAL.
*   No se ha podido grabar el evento.
      IF 1 = 2. MESSAGE e004(zedu_wd_message). ENDIF.

      ls_error-type   = 'E'.
      ls_error-id     = ga_id_message.
      ls_error-number = 004.
      APPEND ls_error TO rt_error.

      EXIT.

    ELSE.

      MODIFY zedu_pradt FROM is_pradt.

*   El evento se grabó satisfactoriamente.
      IF 1 = 2. MESSAGE i003(zedu_wd_message). ENDIF.

      ls_error-type   = 'I'.
      ls_error-id     = ga_id_message.
      ls_error-number = 003.
      APPEND ls_error TO rt_error.

    ENDIF.


  ENDMETHOD.


  METHOD valida_estudiante_inscripto.

    TYPES: BEGIN OF ty_bp,
             partner  TYPE but0id-partner,
             type     TYPE but0id-type,
             idnumber TYPE but0id-idnumber,
           END OF ty_bp,

           BEGIN OF ty_stobjid,
             partner TYPE cmacbpst-partner,
             stobjid TYPE cmacbpst-stobjid,
           END OF ty_stobjid.

    DATA: lt_bp           TYPE TABLE OF ty_bp,
          lt_stobjid      TYPE TABLE OF ty_stobjid,
          lt_t7piqyearprd TYPE TABLE OF t7piqyearprd,
          ls_t7piqyearprd TYPE t7piqyearprd,
          ls_t7piqpkeyi   TYPE t7piqpkeyi,
          lt_t7piqpkeyi   TYPE TABLE OF t7piqpkeyi,
          lt_st_objid     TYPE hrobject_t,
          lt_studies      TYPE piqbw_cs_t,
          ls_st_objid     TYPE hrobject,
          ls_studies      TYPE piqbw_cs,
          lt_objid        TYPE TABLE OF ty_stobjid,
          ls_objid        TYPE ty_stobjid,
          lr_objid        TYPE RANGE OF hrobjid,
          lrs_objid       LIKE LINE OF lr_objid,
          lr_idnumber     TYPE RANGE OF bu_id_number,
          lrs_idnumber    LIKE LINE OF lr_idnumber,
          lr_type         TYPE RANGE OF bu_id_type,
          lrs_type        LIKE LINE OF lr_type,
          ls_stobjid      TYPE ty_stobjid,
          ls_admi_1	      TYPE ty_admi_1,
          ls_admi_2	      TYPE ty_admi_2,
          ls_bp           TYPE ty_bp,
          lv_type         TYPE but0id-type,
          lv_idnumber     TYPE but0id-idnumber,
          lv_period       TYPE piqperid,
          lv_year         TYPE piqperyr,
          lv_begdat       TYPE begdat,
          lv_enddat       TYPE enddat.

    lrs_type = 'IEQ'.
    lrs_idnumber = 'IEQ'.
    LOOP AT ch_admi_1 INTO ls_admi_1.
      lrs_type-low = ls_admi_1-tipo_documen.
      APPEND lrs_type TO lr_type.
      lrs_idnumber-low = ls_admi_1-nro_documen.
      APPEND lrs_idnumber TO lr_idnumber.
    ENDLOOP.

    SELECT partner type idnumber
      FROM but0id
      INTO TABLE lt_bp
         WHERE type     IN lr_type
           AND idnumber IN lr_idnumber.


    IF lt_bp IS NOT INITIAL.
      SELECT partner stobjid
        FROM cmacbpst
        INTO TABLE lt_stobjid
        FOR ALL ENTRIES IN lt_bp
        WHERE partner = lt_bp-partner.
    ENDIF.

    SELECT *
      FROM t7piqyearprd
      INTO TABLE lt_t7piqyearprd.

    SELECT *
      FROM t7piqpkeyi
      INTO TABLE lt_t7piqpkeyi.


    LOOP AT ch_admi_1 INTO ls_admi_1  .
      CLEAR: ls_bp, ls_stobjid, lv_type, lv_idnumber.
      lv_type     = ls_admi_1-tipo_documen.
      lv_idnumber = ls_admi_1-nro_documen.
      READ TABLE lt_bp INTO ls_bp WITH KEY type     = lv_type
                                           idnumber = lv_idnumber.
      IF sy-subrc IS NOT INITIAL.
        DELETE TABLE ch_admi_1 FROM ls_admi_1.
        CONTINUE.
      ELSE.

        READ TABLE lt_stobjid INTO ls_stobjid WITH KEY partner = ls_bp-partner.
        IF sy-subrc IS NOT INITIAL.
          DELETE TABLE ch_admi_1 FROM ls_admi_1.
          CONTINUE.
        ELSE.


          CLEAR: ls_admi_2, lv_period, lv_year, ls_st_objid, ls_studies.
          READ TABLE ch_admi_2 INTO ls_admi_2 WITH KEY nr_formulario = ls_admi_1-nr_formulario.

          lv_period = ls_admi_2-periodo_acad+4(3).
          lv_year   = ls_admi_2-periodo_acad(4).

          CLEAR: ls_t7piqyearprd.
          READ TABLE lt_t7piqyearprd INTO ls_t7piqyearprd WITH KEY peryr = lv_year
                                                                   perid = lv_period.

          CLEAR: ls_t7piqpkeyi.
          READ TABLE lt_t7piqpkeyi INTO ls_t7piqpkeyi WITH KEY persl = ls_t7piqyearprd-persl.

          REFRESH lt_st_objid.

*** valido status
          ls_st_objid-plvar = '01'.
          ls_st_objid-otype = 'ST'.
          ls_st_objid-objid = ls_stobjid-stobjid.
          APPEND ls_st_objid TO lt_st_objid.

          CALL FUNCTION 'HRIQ_BW_STUDIES_READ'
            EXPORTING
              it_st_objid = lt_st_objid
              iv_begda    = ls_t7piqpkeyi-openfrom
              iv_endda    = ls_t7piqpkeyi-opento
            IMPORTING
              et_studies  = lt_studies.

          READ TABLE lt_studies INTO ls_studies WITH KEY admis_status = '2'
                                                         program_objectid = ls_admi_2-programa_1.
          IF sy-subrc IS NOT INITIAL.
            DELETE TABLE ch_admi_1 FROM ls_admi_1.
            DELETE TABLE ch_admi_2 FROM ls_admi_2.
            CONTINUE.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDLOOP.





  ENDMETHOD.
ENDCLASS.
