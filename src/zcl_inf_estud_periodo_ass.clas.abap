class ZCL_INF_ESTUD_PERIODO_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  data GV_STEP type I .
  data GV_PARTNER type BU_PARTNER .
  data GV_OBJID type PIQSTUDENT .

  methods GET_FIRST_DATA
    importing
      !IV_UNAME type UNAME
    exporting
      !ES_FIRST_DATA type ZEDU_S_DAT_BASE_ESTUD_PERIODO .
  methods GET_SECOND_DATA
    importing
      !IV_UNAME type UNAME
    exporting
      !ES_SECOND_DATA type ZEDU_S_DAT_FORM_ESTUD_PERIODO .
  methods GET_THIRD_DATA
    importing
      !IV_PROGRAMA type HROBJID optional
      !IV_UNAME type UNAME
    exporting
      !ES_THIRD_DATA type ZEDU_S_DAT_PROGR_ESTUD_PERIODO .
  methods GUARDAR_INFORMACION_ESTUDIANTE
    importing
      !IS_FIRST_DATA type ZEDU_S_DAT_BASE_ESTUD_PERIODO
      !IS_SECOND_DATA type ZEDU_S_DAT_FORM_ESTUD_PERIODO
      !IS_THIRD_DATA type ZEDU_S_DAT_PROGR_ESTUD_PERIODO
    exporting
      !ET_ERRORES type BAPIRETURN_T .
  methods GET_PROGRAMAS
    importing
      !IV_UNAME type UNAME
    exporting
      !ET_PROGRAMAS type ZEDU_T_DAT_PROGRAMAS_ESTUD .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INF_ESTUD_PERIODO_ASS IMPLEMENTATION.


  METHOD get_first_data.

    DATA: lv_name      TYPE sy-uname,
          lv_perit     TYPE t7piqperiodt-perit,
          lv_adm_ayear TYPE hrpad530-adm_ayear,
          lv_adm_perid TYPE hrpad530-adm_perid,
          lv_adatanr   TYPE hrp1001-adatanr,
          lv_objkey    TYPE usapplref-objkey,
          lv_stobjid   TYPE cmacbpst-stobjid,
          lv_value     TYPE t7piqswitchvalue-value,
          lv_stext     TYPE hrp1000-stext,
          lv_tdoc_text TYPE tb039b-text,
          ls_but000    TYPE but000,
          lv_type      TYPE but0id-type,
          lv_idnumber  TYPE but0id-idnumber,
          lt_hrp1001   TYPE TABLE OF hrp1001,
          ls_hrp1001   TYPE hrp1001.

    CLEAR es_first_data.
    lv_name = iv_uname.

    " Selecciona interlocutor comercial
    SELECT SINGLE objkey
      FROM usapplref
      INTO  lv_objkey
      WHERE bname EQ lv_name.

    CHECK sy-subrc IS INITIAL.

    " Selecciona ID de estudiante
    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO lv_stobjid
      WHERE partner EQ lv_objkey.

    " Selecciona ID de IES
    SELECT SINGLE value
      FROM  t7piqswitchvalue
      INTO lv_value
      WHERE  grpid EQ 'CAMPU' AND
             valid EQ 'HIORG'.
    IF sy-subrc IS INITIAL.
      " Selecciona Descripción de IES
      SELECT SINGLE stext
        FROM hrp1000
        INTO lv_stext
        WHERE objid EQ lv_value.
    ENDIF.

    " Selecciona ID de objeto de Año/Período académico
    SELECT SINGLE *
      FROM hrp1001
      INTO ls_hrp1001
      WHERE otype EQ 'ST' AND
            objid EQ lv_stobjid AND
            sclas EQ 'CS' AND
            subty EQ 'A530'. "AND     M6714 - HRESTREPO - 21/08/2018
*            begda LE sy-datum AND    M6714 - HRESTREPO - 21/08/2018
*            endda GE sy-datum.       M6714 - HRESTREPO - 21/08/2018
    IF sy-subrc IS INITIAL.
      lv_adatanr = ls_hrp1001-adatanr.

      " Selecciona Año/Período académico
      SELECT SINGLE adm_ayear adm_perid
        FROM hrpad530
        INTO ( lv_adm_ayear, lv_adm_perid )
        WHERE adatanr EQ lv_adatanr.
      IF sy-subrc IS INITIAL.
        " Selecciona descripción de período
        SELECT SINGLE perit
          FROM t7piqperiodt
          INTO lv_perit
          WHERE perid EQ lv_adm_perid AND
                spras EQ sy-langu.
      ENDIF.
    ENDIF.

    " Selecciona tipo y número de documento
    SELECT SINGLE type idnumber
      FROM but0id
      INTO ( lv_type, lv_idnumber )
      WHERE partner EQ lv_objkey.
    IF sy-subrc IS INITIAL.
      " Selecciona descripción de tipo de documento
      SELECT SINGLE text
        FROM tb039b
        INTO lv_tdoc_text
        WHERE spras EQ sy-langu AND
              type  EQ lv_type.
    ENDIF.

    " Selecciona datos personales de estudiante
    SELECT SINGLE *
      FROM but000
      INTO ls_but000
      WHERE partner EQ lv_objkey.


    es_first_data-objid       = lv_stobjid.
    es_first_data-partner     = lv_objkey.
    es_first_data-cod_ies     = lv_value.
    es_first_data-nom_ies     = lv_stext.
    es_first_data-tipdoc      = lv_type.
    es_first_data-tipdoc_text = lv_tdoc_text.
    es_first_data-numdoc      = lv_idnumber.
    es_first_data-namefirst   = ls_but000-name_first.
    es_first_data-namemiddle  = ls_but000-namemiddle.
    es_first_data-name_last   = ls_but000-name_last.
    es_first_data-name_lst2   = ls_but000-name_lst2.
    es_first_data-adm_ayear   = lv_adm_ayear.
    es_first_data-adm_perid   = lv_adm_perid.
    es_first_data-perid_text  = lv_perit.

  ENDMETHOD.


  METHOD get_programas.
    TYPES:
      BEGIN OF ty_hrp1001,
        objid    TYPE hrp1001-objid,
        sobid    TYPE hrp1001-sobid,
        objid_cs TYPE hrp1001-objid,
      END OF ty_hrp1001.

    DATA: lt_hrp1000   TYPE TABLE OF hrp1000,
          lt_hrp1001   TYPE TABLE OF ty_hrp1001,
          ls_hrp1000   TYPE hrp1000,
          ls_hrp1001   TYPE ty_hrp1001,
          lv_objkey    TYPE usapplref-objkey,
          lv_stobjid   TYPE cmacbpst-stobjid,
          lv_name      TYPE sy-uname,
          ls_programas TYPE zedu_s_dat_programas_estud.


    FIELD-SYMBOLS: <fs_hrp1001> TYPE ty_hrp1001.

    REFRESH et_programas.
    lv_name = iv_uname.
**********************************************************************
    " Usuarios en DCE 110:
    " AGOMEZ
    " CPEREZ
    " STAGUZMAN
**********************************************************************

    " Selecciona interlocutor comercial
    SELECT SINGLE objkey
      FROM usapplref
      INTO  lv_objkey
      WHERE bname EQ lv_name.

    CHECK sy-subrc IS INITIAL.

    " Selecciona ID de estudiante
    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO lv_stobjid
      WHERE partner EQ lv_objkey.

    CHECK sy-subrc IS INITIAL.
    " Selecciona relación Estudiante/Programa
    SELECT objid sobid
      FROM hrp1001
      INTO TABLE lt_hrp1001
      WHERE plvar EQ '01' AND
            otype EQ 'ST' AND
            objid EQ lv_stobjid AND
            sclas EQ 'CS' AND
*            begda LE sy-datum AND    "M6714 - HRESTREPO - 21/08/2018
*            endda GE sy-datum AND    "M6714 - HRESTREPO - 21/08/2018
            subty EQ 'A530'.
    IF sy-subrc IS INITIAL.

      LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001>.
        <fs_hrp1001>-objid_cs = <fs_hrp1001>-sobid.
      ENDLOOP.

      " Selecciona datos de programa
      SORT lt_hrp1001 BY objid_cs.
      SELECT *
        FROM hrp1000
        INTO TABLE lt_hrp1000
        FOR ALL ENTRIES IN lt_hrp1001
        WHERE plvar EQ '01' AND
              otype EQ 'CS' AND
              objid EQ lt_hrp1001-objid_cs.
      IF sy-subrc IS INITIAL.
        SORT lt_hrp1000 BY objid.
      ENDIF.

      LOOP AT lt_hrp1001 INTO ls_hrp1001.
        ls_programas-objid_st = ls_hrp1001-objid.
        ls_programas-objid_cs = ls_hrp1001-objid_cs.
        READ TABLE lt_hrp1000 INTO ls_hrp1000
          WITH KEY objid = ls_hrp1001-objid_cs
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_programas-stext_cs = ls_hrp1000-stext.
        ENDIF.
        APPEND ls_programas TO et_programas.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD GET_SECOND_DATA.
    DATA: lv_name    TYPE sy-uname,
          lv_objkey  TYPE usapplref-objkey,
          lv_stobjid TYPE cmacbpst-stobjid,
          lv_socialt TYPE t7piqsocialt-socialt,
          lv_social  TYPE hrp1704-social,
          lv_famst   TYPE hrp1702-famst,
          lv_ftext   TYPE t502t-ftext.


    CLEAR es_second_data.
    lv_name = iv_uname.

    " Selecciona interlocutor comercial
    SELECT SINGLE objkey
      FROM usapplref
      INTO  lv_objkey
      WHERE bname EQ lv_name.

    CHECK sy-subrc IS INITIAL.

    " Selecciona ID de estudiante
    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO lv_stobjid
      WHERE partner EQ lv_objkey.


    CHECK sy-subrc IS INITIAL.

    " Selecciona grupo social
    SELECT SINGLE social
      FROM hrp1704
      INTO lv_social
      WHERE plvar EQ '01' AND
            otype EQ 'ST' AND
            objid EQ lv_stobjid AND
            begda LE sy-datum AND
            endda GE sy-datum.
    IF sy-subrc IS INITIAL.

      " Selecciona descripción de grupo social
      SELECT SINGLE socialt
        FROM t7piqsocialt
        INTO lv_socialt
        WHERE spras EQ sy-langu AND
              social EQ lv_social.
    ENDIF.

    " Selecciona estado civil
    SELECT SINGLE famst
      FROM hrp1702
      INTO lv_famst
      WHERE plvar EQ '01' AND
            otype EQ 'ST' AND
            objid EQ lv_stobjid AND
            begda LE sy-datum   AND
            endda GE sy-datum.
    IF sy-subrc IS INITIAL.
      " Selecciona descripción de estado civil
      SELECT SINGLE ftext
        FROM t502t
        INTO lv_ftext
        WHERE sprsl EQ sy-langu AND
              famst EQ lv_famst.
    ENDIF.

    es_second_data-estado_civil      = lv_famst.
    es_second_data-estado_civil_desc = lv_ftext.
    es_second_data-estrato           = lv_social.
    es_second_data-estrato_desc      = lv_socialt.

  ENDMETHOD.


  METHOD get_third_data.

    DATA:
      lt_param_apli       TYPE TABLE OF zedu_param_apli,
      lt_return           TYPE TABLE OF bapiret2,
      lt_appraisal        TYPE TABLE OF piqagr_appraisal_buffer,
      lt_student          TYPE hrobject_t,       " Filtro de estudiantes
      lt_hrp1001_st_sm    TYPE TABLE OF hrp1001, " Relación Estudiante/Asignatura
      lt_hrp1001_sc_sm    TYPE TABLE OF hrp1001, " Relación Plan de estudios/Asignaturas

      lt_moduleregs       TYPE TABLE OF piqmoduletab_ext_text,

      ls_hrp1730          TYPE hrp1730,
      ls_hrt1737          TYPE hrt1737,
      ls_hrp1737          TYPE hrp1737,
      ls_hrp1769          TYPE hrp1769,
      ls_hrp1001_st_sm    TYPE hrp1001,
      ls_hrp1001_st_cs    TYPE hrp1001,
      ls_hrp1001_cs_sc    TYPE hrp1001, " Relación Estudio/Plan de estudio
      ls_param_apli       TYPE zedu_param_apli,
      ls_module           TYPE hrobject,
      ls_appraisal        TYPE piqagr_appraisal_buffer,
      ls_student          TYPE hrobject,

      lv_name             TYPE sy-uname,
      lv_objkey           TYPE usapplref-objkey,
      lv_objid_st         TYPE cmacbpst-stobjid,
      lv_snies            TYPE hrp1730-snies,
      lv_stext            TYPE hrp1000-stext,
      lv_bezei            TYPE t005u-bezei,
      lv_city_name        TYPE adrcityt-city_name,
      lv_value            TYPE t7piqswitchvalue-value,
      lv_param_apli_valor TYPE char9,
      lv_cod_snies        TYPE char10,
      lv_reasontext       TYPE t7piqreasont-reasontext,
      lv_adm_ayear        TYPE hrpad530-adm_ayear,
      lv_adm_perid        TYPE hrpad530-adm_perid,
      lv_adatanr          TYPE hrp1001-adatanr,
      lv_cred_aprob       TYPE piqagr_appraisal_buffer-cpattempfol,
      lv_um_cred          TYPE piqagr_appraisal_buffer-cpunitfol.


    FIELD-SYMBOLS:
      <fs_moduleregs> TYPE piqmoduletab_ext_text,
      <fs_appraisal>  TYPE piqagr_appraisal_buffer.



    CLEAR es_third_data.
    lv_name = iv_uname.

    " Selecciona interlocutor comercial
    SELECT SINGLE objkey
      FROM usapplref
      INTO  lv_objkey
      WHERE bname EQ lv_name.

    CHECK sy-subrc IS INITIAL.

    " Selecciona ID de estudiante
    SELECT SINGLE stobjid
      FROM cmacbpst
      INTO lv_objid_st
      WHERE partner EQ lv_objkey.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE stext
      FROM hrp1000
      INTO lv_stext
      WHERE plvar EQ '01' AND
            otype EQ 'CS' AND
            objid EQ iv_programa.
    IF sy-subrc IS INITIAL.
      " Selecciona plan de estudio del estudio
      SELECT SINGLE *
        FROM hrp1001
        INTO ls_hrp1001_cs_sc
        WHERE otype EQ 'CS' AND
              objid EQ iv_programa AND
              plvar EQ '01' AND
              rsign EQ 'A' AND
              relat EQ '514' AND
              sclas EQ 'SC'.
      IF sy-subrc IS INITIAL.

        " Selecciona dirección de plan de estudios
        SELECT SINGLE  *
          FROM hrp1730
          INTO ls_hrp1730
          WHERE plvar EQ '01' AND
                otype EQ 'SC' AND
                objid EQ ls_hrp1001_cs_sc-sobid.
        IF sy-subrc IS INITIAL.
          " Selecciona descripción de región
          SELECT SINGLE bezei
            FROM t005u
            INTO lv_bezei
            WHERE spras EQ sy-langu         AND
                  land1 EQ 'CO'             AND
                  bland EQ ls_hrp1730-regio.

          "Asigna el formato del codigo de ciudad
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_hrp1730-city_code
            IMPORTING
              output = ls_hrp1730-city_code.

          " Selecciona descripción de ciudad
          SELECT SINGLE city_name
            FROM adrcityt
            INTO lv_city_name
            WHERE langu     EQ sy-langu             AND
                  country   EQ 'CO'                 AND
                  city_code EQ ls_hrp1730-city_code.

        ENDIF.
      ENDIF.
    ENDIF.

    " Selecciona motivo de retiro
    SELECT SINGLE *
      FROM hrp1769
      INTO ls_hrp1769
      WHERE otype EQ 'CS' AND
            objid EQ iv_programa AND
            endda LT '99991231' AND
            ( end_process EQ 'RW01' OR
              end_process EQ 'RV01' ). "AND  "M6714 - HRESTREPO - 23/08/2018
*              end_reason  NE '1018'.        "M6714 - HRESTREPO - 23/08/2018
    IF sy-subrc IS INITIAL.
      SELECT SINGLE reasontext
        FROM t7piqreasont
        INTO lv_reasontext
        WHERE spras  EQ sy-langu AND
              reason EQ ls_hrp1769-end_reason.
      IF sy-subrc IS INITIAL.
        es_third_data-motivo = lv_reasontext.
      ENDIF.
      es_third_data-retirado = 'X'.
      es_third_data-retirado_txt = 'Si'.
    ELSE.
      es_third_data-retirado = space.
      es_third_data-retirado_txt = 'No'.
    ENDIF.

    " Selecciona ID de IES
    SELECT SINGLE value
      FROM  t7piqswitchvalue
      INTO lv_value
      WHERE  grpid EQ 'CAMPU' AND
             valid EQ 'HIORG'.
    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zedu_param_apli
        INTO TABLE lt_param_apli
        WHERE idparam    EQ 'UNI_CTO_CO' "'UNI_CTO_COSTO' AND   "M6714 - HRESTREPO - 23/08/2018
          AND idparampos EQ 1.
      IF sy-subrc IS INITIAL .
        SORT lt_param_apli BY valor.
        READ TABLE lt_param_apli INTO ls_param_apli
         WITH KEY valor(8) = lv_value(8)
         BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          SPLIT ls_param_apli-valor AT '-' INTO lv_param_apli_valor lv_cod_snies.
        ENDIF.
      ENDIF.
    ENDIF.

    es_third_data-cod_snies     = ls_hrp1730-snies. " lv_cod_snies. "M6714 - HRESTREPO - 28/08/2018
    es_third_data-cod_entid     = lv_cod_snies.                     "M6714 - HRESTREPO - 28/08/2018
    es_third_data-programa      = ls_hrp1001_cs_sc-sobid.
    es_third_data-nom_prog      = lv_stext.
    es_third_data-dep_prog      = ls_hrp1730-regio.
    es_third_data-dep_prog_desc = lv_bezei.
    es_third_data-mun_prog      = ls_hrp1730-city_code.
    es_third_data-mun_prog_desc = lv_city_name.

    " Candidato de grado?
*Inicio M6714 - HRESTREPO - 21/08/2018
**    SELECT SINGLE *
**      FROM hrp1737
**      INTO ls_hrp1737
**      WHERE otype EQ 'ST' AND
**            objid EQ lv_objid_st AND
**            prog_type EQ '2'.
**
    "Hace la seleccion haciendo uso del indice estandar 3 - PROG_TYPE / OTYPE /PROGC_VAR / OBJID
    SELECT SINGLE *
      FROM hrp1737
      INTO ls_hrp1737
      WHERE prog_type EQ '2'
        AND otype     EQ 'ST'
        AND progc_var EQ ls_hrp1730-progcvar
        AND objid     EQ lv_objid_st.
*Fin M6714 - HRESTREPO - 21/08/2018
    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM hrt1737
        INTO ls_hrt1737
        WHERE tabnr EQ ls_hrp1737-tabnr.
      IF sy-subrc IS INITIAL.
        IF ls_hrt1737-acst EQ 'GRAD'.
          es_third_data-candidato_grado = 'X'.
          es_third_data-candidato_txt = 'Si'.
        ELSE.
*          es_third_data-motivo = '-'.  "M6714 - HRESTREPO - 28/08/2018
          es_third_data-candidato_grado = space.
          es_third_data-candidato_txt = 'No'.
        ENDIF.
      ELSE.
*        es_third_data-motivo = '-'.    "M6714 - HRESTREPO - 28/08/2018
        es_third_data-candidato_grado = space.
        es_third_data-candidato_txt = 'No'.
      ENDIF.
    ELSE.
*      es_third_data-motivo = '-'.      "M6714 - HRESTREPO - 28/08/2018
      es_third_data-candidato_grado = space.
      es_third_data-candidato_txt = 'No'.
    ENDIF.

*Inicio M6714 - HRESTREPO - 23/08/2018
**    " Selecciona las asignaturas del plan de estudios
**    SELECT *
**      FROM hrp1001
**      INTO TABLE lt_hrp1001_sc_sm
**      WHERE otype EQ 'SC' AND
**            objid EQ ls_hrp1001_cs_sc-sobid AND
**            sclas EQ 'SM' AND
**            subty EQ 'A500'.
**    IF sy-subrc IS INITIAL.
**      " Selecciona las asignaturas a las cuales está inscripto el estudiante
**      SELECT *
**        FROM hrp1001
**        INTO TABLE lt_hrp1001_st_sm
**        FOR ALL ENTRIES IN lt_hrp1001_sc_sm
**        WHERE otype EQ 'ST' AND
**              objid EQ lv_objid_st AND
**              sclas EQ 'SM' AND
**              sobid EQ lt_hrp1001_sc_sm-sobid AND
**              subty EQ 'A506'.
**      IF sy-subrc IS INITIAL.
**        " Selecciona ID de objeto de Año/Período académico
**        SELECT SINGLE *
**          FROM hrp1001
**          INTO ls_hrp1001_st_cs
**          WHERE otype EQ 'ST' AND
**                objid EQ lv_objid_st AND
**                sclas EQ 'CS' AND
**                subty EQ 'A530' AND
**                begda LE sy-datum AND
**                endda GE sy-datum.
**
**        IF sy-subrc IS INITIAL.
**          lv_adatanr = ls_hrp1001_st_cs-adatanr.
**
**          " Selecciona Año/Período académico
**          SELECT SINGLE adm_ayear adm_perid
**            FROM hrpad530
**            INTO ( lv_adm_ayear, lv_adm_perid )
**            WHERE adatanr EQ lv_adatanr.
**          IF sy-subrc IS INITIAL.
**
**            " Filtra por estudiante actual
**            ls_student-plvar  = '01'.
**            ls_student-otype  = 'ST'.
**            ls_student-objid  = lv_objid_st.
**            APPEND ls_student TO lt_student.
**
**            " Recorre asignaturas en las cuales el estudiante está inscripto
**            LOOP AT lt_hrp1001_st_sm INTO ls_hrp1001_st_sm.
**              REFRESH: lt_appraisal, lt_return.
**              ls_module-plvar = '01'.
**              ls_module-otype = 'SM'.
**              ls_module-objid = ls_hrp1001_st_sm-sobid.
**
**              " Selecciona créditos de estudiante/asignatura
**              CALL FUNCTION 'HRIQ_AGR_RFC_APPRMODUL_GETDATA'
**                EXPORTING
**                  i_module     = ls_module
**                  i_ayear      = lv_adm_ayear
**                  i_period     = lv_adm_perid
***                 I_BEGDA      =
***                 I_ENDDA      =
***                 I_OBJECT_SE  =
***                 I_BASIS      = 'X'
***                 I_ELEMENT_DETAIL       =
**                TABLES
**                  et_appraisal = lt_appraisal
***                 ET_APPR_TEMPL          =
***                 ET_APPR_TRANS          =
***                 ET_ELEMENT   =
**                  et_return    = lt_return
**                  it_student   = lt_student.
**
**              DELETE lt_appraisal WHERE cpattempfol IS INITIAL.
**
**              " Acumula crédito en variable LV_CRED_APROB
**              LOOP AT lt_appraisal INTO ls_appraisal WHERE smstatus EQ '02'.
**                lv_cred_aprob = lv_cred_aprob + ls_appraisal-cpattempfol.
**                lv_um_cred   = ls_appraisal-cpunitfol.
**              ENDLOOP.
**            ENDLOOP.
**
**    es_third_data-cred_aprob = lv_cred_aprob.
**    es_third_data-um_cred   = lv_um_cred.
**
**    " Selecciona texto de unidad de medida de crédito
**    SELECT SINGLE msehl
**      FROM t006a
**      INTO es_third_data-um_cred_txt
**      WHERE spras EQ sy-langu AND
**            msehi EQ lv_um_cred.
**
**      ENDIF.
**    ENDIF.
**  ENDIF.
**ENDIF.
**
    "Obtiene las asignaturas del estudiante
    CALL FUNCTION 'HRIQ_RFC_STUDENT_MODREG_GET'
      EXPORTING
        i_student     = lv_objid_st
      TABLES
        et_moduleregs = lt_moduleregs.

    "Deja unicamente las asignaturas aprobadas
    DELETE lt_moduleregs
      WHERE smstatus NE '02'
        OR  lockflag NE 'X'.

    "Ordena las asignaturas por fecha dejando la ultima vista de primero
    SORT lt_moduleregs BY varyf ASCENDING endda DESCENDING begda DESCENDING.
    "Elimina los registros duplicados
    DELETE ADJACENT DUPLICATES FROM lt_moduleregs
      COMPARING varyf.

    "Recorre las asignaturas
    LOOP AT lt_moduleregs ASSIGNING <fs_moduleregs>.
      REFRESH: lt_appraisal.
      CLEAR: ls_module.

      "Arma la consulta
      CONCATENATE '01' <fs_moduleregs>-varyf INTO ls_module.

      "Obtiene el detalle de la asignatura
      CALL FUNCTION 'HRIQ_AGR_RFC_APPRMODUL_GETDATA'
        EXPORTING
          i_module     = ls_module
          i_ayear      = <fs_moduleregs>-peryr
          i_period     = <fs_moduleregs>-perid
        TABLES
          et_appraisal = lt_appraisal
          et_return    = lt_return.

      "Elimina los registros no válidos
      DELETE lt_appraisal
        WHERE st_objid NE lv_objid_st " Filtro x estudiante
          OR  cs_objid NE iv_programa " Filtro x programa
          OR  agrtype  NE '0001'      " Filtro x tipo de calificacion
          OR  smstatus NE '02'        " Filtro x asignaturas aprobadas
          OR  lockflag NE 'X'.        " FIltro x bloqueo de modificacion

      "Recorre los registros validos
      LOOP AT lt_appraisal ASSIGNING <fs_appraisal>.
        "Aumenta la cantidad de creditos aprobados
        ADD <fs_appraisal>-cpattempfol TO lv_cred_aprob.
        "Asigna la unidad de medida de los creditos aprobados
        lv_um_cred = <fs_appraisal>-cpunitfol.
      ENDLOOP.
    ENDLOOP.

    "Asigna la suma de creditos y la unidad de medida
    es_third_data-cred_aprob = lv_cred_aprob.
    es_third_data-um_cred   = lv_um_cred.

    "Selecciona texto de unidad de medida de crédito
    SELECT SINGLE msehl
      FROM t006a
      INTO es_third_data-um_cred_txt
      WHERE spras EQ sy-langu AND
            msehi EQ lv_um_cred.
*Fin M6714 - HRESTREPO - 23/08/2018

    es_third_data-saber_pro = space.
    es_third_data-saber_pro_txt = 'No'.

  ENDMETHOD.


  METHOD guardar_informacion_estudiante.

    DATA: ls_9119    TYPE pt9119,
          ls_errores TYPE bapiret2.
    CONSTANTS: lc_9119 TYPE i VALUE 9119,
               lc_st   TYPE otype VALUE 'ST',
               lc_s    TYPE char1 VALUE 'S',
               lc_e    TYPE char1 VALUE 'E'.

    ls_9119-entidad  = is_second_data-entidad.
    ls_9119-porc_fin = is_second_data-porcentaje.
    ls_9119-ayud_fin = is_second_data-req_ayuda.
    ls_9119-apoy_fin = is_second_data-rec_apoyo.
    ls_9119-porc_ayu = is_second_data-porc_ayuda.
    ls_9119-ayud_aca = is_second_data-req_ayuda_acad.
    ls_9119-apoy_aca = is_second_data-rec_apoyo_acad.
    ls_9119-sati_apo = is_second_data-nivel_satis.
    ls_9119-otra_ayu = is_second_data-req_ayuda_otro.
    ls_9119-otro_apo = is_second_data-rec_otro.
    ls_9119-sati_otr = is_second_data-nivel_satis_otro.
    ls_9119-vlor_pag = is_second_data-valor_total.
    ls_9119-famst    = is_second_data-estado_civil.
    ls_9119-pers_car = is_second_data-nro_pers.
    ls_9119-social   = is_second_data-estrato.
    ls_9119-discapc  = is_second_data-discapacidad.
    ls_9119-trabaja  = is_second_data-trabaja.
    ls_9119-tipo_tra = is_second_data-tipo_trabajo.
    ls_9119-ingresos = is_second_data-rango_ingreso.
    ls_9119-dura_tra = is_second_data-duracion_trabajo.

    CALL FUNCTION 'ZMF_ACTUALIZAR_ITXXXX'
      EXPORTING
        is_xxxx    = ls_9119
        i_objid    = is_first_data-objid
        i_infotype = lc_9119
*       IS_KEY     =
        i_otype    = lc_st
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.
    IF sy-subrc IS INITIAL.
      ls_errores-type       = lc_s.
      ls_errores-id         = ga_id_message.
      ls_errores-number     = '019'.
    ELSE.
      ls_errores-type       = lc_e.
      ls_errores-id         = ga_id_message.
      ls_errores-number     = '020'.
    ENDIF.
    APPEND ls_errores TO et_errores.


  ENDMETHOD.
ENDCLASS.
