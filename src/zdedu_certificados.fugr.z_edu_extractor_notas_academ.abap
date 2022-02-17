FUNCTION z_edu_extractor_notas_academ.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_PLAN_ESTUDIO) TYPE  HROBJID
*"  TABLES
*"      IT_NRO_IDENT TYPE  ZEDU_T_NRO_IDENT
*"      ET_NOTAS_ACADEMICAS TYPE  ZEDU_T_NOTAS_ACADEMICAS
*"      ET_PROMEDIO_SEMESTRE TYPE  ZEDU_T_PROMEDIO_SEMESTRE_NOTAS
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_ident,
           objid     TYPE hrobjid,
           nro_ident TYPE bu_id_number,
         END OF ty_ident.

  DATA: lr_class             TYPE REF TO zcl_wd_general_ass,
        lt_moduleregs        TYPE TABLE OF piqmoduletab_ext_text,
        lt_appraisal         TYPE TABLE OF piqagr_appraisal_buffer,
        lt_appraisal_tot     TYPE TABLE OF piqagr_appraisal_buffer,
        lt_hrp1000           TYPE TABLE OF hrp1000,
        lt_hrp1001           TYPE TABLE OF hrp1001,
        lt_hrpad500          TYPE TABLE OF hrpad500,
        lt_ident             TYPE TABLE OF ty_ident,
        lr_objid             TYPE RANGE OF hrobjid,
        lrs_objid            LIKE LINE OF lr_objid,
        ls_ident             TYPE ty_ident,
        ls_hrp1000           TYPE hrp1000,
        ls_hrp1001           TYPE hrp1001,
        ls_hrpad500          TYPE hrpad500,
        ls_notas_academicas  TYPE zedu_s_notas_academicas,
        ls_promedio_semestre TYPE zedu_s_promedio_semestre_notas,
        ls_module            TYPE hrobject,
        lv_stobjid           TYPE hrobjid,
        lv_notecred          TYPE piqcpattemp, "zedu_notecred,
        lv_notecred_tot      TYPE piqcpattemp, "zedu_notecred,
        lv_notesem_tot       TYPE piqcpattemp, "zedu_notecred,
        lv_cpattemp          TYPE piqcpattemp,
        lv_cpattemp_tot      TYPE piqcpattemp,
        lv_gradesym          TYPE piqcpattemp,
        lv_agrrealoasg       TYPE realo.


  FIELD-SYMBOLS: <fs_nro_ident>  TYPE bu_id_number,
                 <fs_moduleregs> TYPE piqmoduletab_ext_text,
                 <fs_appraisal>  TYPE piqagr_appraisal_buffer.


  CREATE OBJECT lr_class.

  LOOP AT it_nro_ident ASSIGNING <fs_nro_ident>. " Nros de identifiación

    REFRESH: lt_moduleregs.

*** Busco el OBJID del estudiante
    lv_stobjid = lr_class->obtener_objid_st( EXPORTING iv_idnumber = <fs_nro_ident> ).

    ls_ident-nro_ident = <fs_nro_ident>.
    ls_ident-objid     = lv_stobjid.
    APPEND ls_ident TO lt_ident.

*** Obtengo las asignaturas.
    CALL FUNCTION 'HRIQ_RFC_STUDENT_MODREG_GET'
      EXPORTING
        i_student     = lv_stobjid
      TABLES
        et_moduleregs = lt_moduleregs
        et_message    = et_return.

    SORT lt_moduleregs BY varyf.
    DELETE ADJACENT DUPLICATES FROM lt_moduleregs COMPARING varyf.


*** Traigo los resultados de las asignaturas
    LOOP AT lt_moduleregs ASSIGNING <fs_moduleregs>.

      REFRESH: lt_appraisal.
      CLEAR ls_module.
      CONCATENATE '01' <fs_moduleregs>-varyf INTO ls_module.

      CALL FUNCTION 'HRIQ_AGR_RFC_APPRMODUL_GETDATA'
        EXPORTING
          i_module     = ls_module
          i_ayear      = <fs_moduleregs>-peryr
          i_period     = <fs_moduleregs>-perid
        TABLES
          et_appraisal = lt_appraisal
          et_return    = et_return.

      DELETE lt_appraisal WHERE st_objid NE lv_stobjid. " Filtro x estudiante
      DELETE lt_appraisal WHERE agrtype  NE '0001'.


      IF iv_plan_estudio IS NOT INITIAL. " Filtro x plan de estudio si es q se cargó
        DELETE lt_appraisal WHERE sc_objid NE iv_plan_estudio.
      ENDIF.

*** Cargo todas las asignaturas en un rango
      lrs_objid     = 'IEQ'.
      lrs_objid-low = <fs_moduleregs>-varyf+2(8).
      APPEND lrs_objid TO lr_objid.

      APPEND LINES OF lt_appraisal TO lt_appraisal_tot.

    ENDLOOP.

  ENDLOOP.

*** Obtengo las descripciones de las asignaturas
  SELECT *
    FROM hrp1000
    INTO TABLE lt_hrp1000
    WHERE plvar = '01'
      AND otype = 'SM'
      AND objid IN lr_objid.

*** Obtengo los niveles de las asignaturas
  SELECT *
    FROM hrp1001
    INTO TABLE lt_hrp1001
    WHERE plvar = '01'
      AND otype = 'SM'
      AND objid IN lr_objid
      AND sclas = 'SC'.

  IF lt_hrp1001 IS NOT INITIAL.
    SELECT *
      FROM hrpad500
      INTO TABLE lt_hrpad500
      FOR ALL ENTRIES IN lt_hrp1001
      WHERE adatanr = lt_hrp1001-adatanr.
  ENDIF.


  SORT lt_appraisal_tot BY st_objid agrrealoasg agrdate ASCENDING.

  LOOP AT lt_appraisal_tot ASSIGNING <fs_appraisal>.

    CLEAR: ls_notas_academicas,
           lv_notecred,
           lv_cpattemp,
           lv_gradesym,
           ls_hrp1000,
           ls_hrp1001,
           ls_hrpad500.

    MOVE-CORRESPONDING <fs_appraisal> TO ls_notas_academicas.

    READ TABLE lt_hrp1000 INTO ls_hrp1000 WITH KEY objid = <fs_appraisal>-agrrealoasg.
    ls_notas_academicas-descr_asignatura = ls_hrp1000-mc_stext.

    READ TABLE lt_hrp1001  INTO ls_hrp1001  WITH KEY objid   = <fs_appraisal>-agrrealoasg.
    READ TABLE lt_hrpad500 INTO ls_hrpad500 WITH KEY adatanr = ls_hrp1001-adatanr.
    ls_notas_academicas-nivel_asignatura = ls_hrpad500-stgfin.


    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = <fs_appraisal>-gradesym
      IMPORTING
        num             = lv_gradesym
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.

*** Si hay mas de un registro x asignatura, solo calculo el mas viejo.
    IF lv_agrrealoasg NE <fs_appraisal>-agrrealoasg.
      lv_notecred     = lv_gradesym        * <fs_appraisal>-cpattemp.
      lv_notecred_tot = lv_notecred_tot    + lv_notecred.
      IF lv_notecred IS NOT INITIAL.
        lv_cpattemp_tot = lv_cpattemp_tot    + <fs_appraisal>-cpattemp.
      ENDIF.

      PERFORM f_quitar_decimales USING lv_notecred
                                 CHANGING ls_notas_academicas-notecred.
      lv_agrrealoasg = <fs_appraisal>-agrrealoasg.
    ENDIF.

    AT END OF st_objid.
      IF lv_cpattemp_tot IS NOT INITIAL.
        CLEAR ls_ident.
        READ TABLE lt_ident INTO ls_ident WITH KEY objid = <fs_appraisal>-st_objid.
        ls_promedio_semestre-nro_ident = ls_ident-nro_ident.
        lv_notesem_tot                 = lv_notecred_tot / lv_cpattemp_tot.
        PERFORM f_quitar_decimales USING lv_notesem_tot
                                   CHANGING ls_promedio_semestre-notesem.
        ls_promedio_semestre-ayear     = <fs_appraisal>-ayear.
        ls_promedio_semestre-aperiod   = <fs_appraisal>-aperiod.
        APPEND ls_promedio_semestre TO et_promedio_semestre.
        CLEAR: ls_promedio_semestre, lv_notecred_tot, lv_cpattemp_tot.
      ENDIF.
    ENDAT.

    APPEND ls_notas_academicas TO et_notas_academicas.

  ENDLOOP.

ENDFUNCTION.
