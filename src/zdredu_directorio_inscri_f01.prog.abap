*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_datos .

  DATA: lt_planes        TYPE tyt_objid,
        lt_estudios      TYPE tyt_objid,
        lt_estudiantes   TYPE tyt_objid,
        lt_programas     TYPE tyt_programas,
        lt_hrp1702       TYPE tyt_hrp1702,
        lt_hrp1721       TYPE tyt_hrp1721,
        lt_hrp9003       TYPE tyt_hrp9003,
        lt_partner_id    TYPE tyt_partner_id,
        lt_pers_addr_num TYPE tyt_pers_addr_num,
        lt_entrevista    TYPE zedu_t_fec_exam_entrevista,
        lt_examen        TYPE zedu_t_fec_exam_entrevista,
        lt_procesos_ant  TYPE tyt_procesos_ant,
        tablaIntEPS      TYPE tablaEPS,
        tablaIntMedio    TYPE tablaMedio.
*        lt_solic_prog    TYPE tyt_procesos_ant. "--> Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
  DATA lt_entrevista_tmp  TYPE zedu_t_fec_exam_entrevista.
  DATA lt_examen_tmp      TYPE zedu_t_fec_exam_entrevista.
  DATA lt_colegios        TYPE ty_t_colegios.
  DATA lt_universidades   TYPE ty_t_universidad.
*	End	  -->	MgM DCEK903347

  CLEAR gv_primer.

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
  PERFORM f_obtener_rango_planestudio USING     p_tippro
                                                p_planes
                                                p_perid
                                                p_peryr
                                      CHANGING  gr_planes.
*	End	  -->	MgM DCEK903347

  PERFORM f_obtener_rango_periodo.

  PERFORM f_obtener_estudios
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*  using lt_planes
    USING gr_planes
*	End	  -->	MgM DCEK903347
    CHANGING lt_estudios.

  PERFORM f_obtener_estudiantes
  USING lt_estudios
  CHANGING lt_estudiantes.

  PERFORM f_obtener_datos_maestros
  USING lt_estudiantes
  CHANGING lt_hrp1702
           lt_partner_id
           lt_pers_addr_num
           lt_hrp1721
           tablaIntEPS
           tablaIntMedio.

  PERFORM f_obtener_procedencia
  USING lt_estudiantes
  CHANGING lt_hrp9003
*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017
           lt_colegios
           lt_universidades.
*	End	  -->	MgM DCEK903347

  PERFORM f_obtener_programas
  USING lt_estudiantes
*        lt_partner_id      "-->  MgM DCEK904012 Procedencia 03/04/2017
  CHANGING lt_programas
           lt_procesos_ant
*           lt_solic_prog   "--> Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
           lt_partner_id.   "-->  MgM DCEK904012 Procedencia 03/04/2017

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
  LOOP AT gr_planes
    INTO DATA(ls_plan).
*	End	  -->	MgM DCEK903347

    CALL FUNCTION 'Z_EDU_GET_FECHA_EXAMEN_ENTREV'
      EXPORTING
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*       iv_plan_estudio = p_planes
        iv_plan_estudio = ls_plan-low
*	End	  -->	MgM DCEK903347
        iv_anio         = p_peryr
        iv_periodo      = p_perid
        iv_tipo_fecha   = c_en
      TABLES
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*       et_fechas       = lt_entrevista.
        et_fechas       = lt_entrevista_tmp.
*	End	  -->	MgM DCEK903347

    CALL FUNCTION 'Z_EDU_GET_FECHA_EXAMEN_ENTREV'
      EXPORTING
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*       iv_plan_estudio = p_planes
        iv_plan_estudio = ls_plan-low
*	End	  -->	MgM DCEK903347
        iv_anio         = p_peryr
        iv_periodo      = p_perid
        iv_tipo_fecha   = c_ex
      TABLES
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*       et_fechas       = lt_examen.
        et_fechas       = lt_examen_tmp.

    APPEND LINES OF lt_entrevista_tmp TO lt_entrevista.
    APPEND LINES OF lt_examen_tmp     TO lt_examen.

    CLEAR:  lt_entrevista_tmp[],
            lt_examen_tmp[].
*	End	  -->	MgM DCEK903347

  ENDLOOP.  "-->  MgM DCEK903347

  PERFORM f_armar_salida
    USING lt_estudiantes
          lt_hrp1702
          lt_hrp9003
          lt_partner_id
          lt_hrp1721
          lt_programas
          lt_procesos_ant
*          lt_solic_prog "--> Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
          lt_entrevista
          lt_examen
          lt_pers_addr_num
*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017
          lt_estudios
          lt_colegios
          lt_universidades
          tablaIntEPS
          tablaIntMedio.
*	End	  -->	MgM DCEK903347


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_ESTUDIANTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_ESTUDIANTES  text
*----------------------------------------------------------------------*
FORM f_obtener_estudiantes USING p_lt_estudios TYPE tyt_objid
                           CHANGING p_lt_estudiantes TYPE tyt_objid.

  DATA: lt_estudios_aux TYPE tyt_objid_aux,
        ls_estudiantes  TYPE tys_objid,
        lt_admissions   TYPE piqst_adm_t,
        ls_admissions   TYPE piqst_adm,
        lv_piqstudent   TYPE piqstudent.

  PERFORM f_completar_auxiliar
  USING p_lt_estudios
  CHANGING lt_estudios_aux.

  CHECK lt_estudios_aux IS NOT INITIAL.

  SELECT a~objid
         a~sobid
         a~otype
         a~sclas
         a~rsign
         a~relat
         a~subty
         a~adatanr
         a~istat
         b~adm_enrcateg
         b~adm_categ
         b~choice_no
         b~adm_aclevel
         b~adm_ayear
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*         b~adm_perid
         c~perit
*	End	  -->	MgM DCEK903347
    FROM hrp1001 AS a
      INNER JOIN hrpad530 AS b
        ON a~adatanr = b~adatanr
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
      LEFT OUTER JOIN t7piqperiodt AS c
        ON b~adm_perid EQ c~perid AND
           c~spras     EQ sy-langu
*	End	  -->	MgM DCEK903347
    INTO TABLE p_lt_estudiantes
      FOR ALL ENTRIES IN lt_estudios_aux
        WHERE a~otype     = c_cs
          AND a~sclas     = c_st
          AND a~plvar     = c_01
*	Begin	-->	MgM DCEK903347 Ajustes varios 24/02/2017
*          and b~adm_categ in s_tiasp
*	End	  -->	MgM DCEK903347
*	Begin	-->	MgM DCEK903347 solo filtrar para activos 08/03/2017
*          and b~adm_ayear = p_peryr
*          and b~adm_perid = p_perid
*	End	  -->	MgM DCEK903347
          AND a~objid     = lt_estudios_aux-sobid
          AND a~sobid IN ( SELECT stobjid
                            FROM cmacbpst AS a
                              INNER JOIN but0id AS b          "-->  MgM DCEK903347
                                ON a~partner EQ b~partner     "-->  MgM DCEK903347
                                WHERE student12 IN s_nromat
                                  AND idnumber  IN s_nroid ) "-->  MgM DCEK903347
*	Begin	-->	MgM DCEK903347 Ajustes varios 24/02/2017
*          and a~sobid in ( select objid
*                            from hrp1702
*                           where prdni in s_nroid
*                             and endda = c_99991231 ).
*	End	  -->	MgM DCEK903347
*	Begin	-->	MgM  DCEK904012 Anyely solicita aplicar filtro de parámetros de entrada 18/04/2017
          AND b~adm_ayear EQ p_peryr
          AND b~adm_perid EQ p_perid.
*	End	  -->	MgM  DCEK904012

  IF sy-subrc = 0.

*	Begin	-->	MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
    DELETE p_lt_estudiantes
*      where objid not in zedu_cl_perfiles_struct=>get_range(
      WHERE sobid NOT IN zedu_cl_perfiles_struct=>get_range(
                            cl_hrpiq00const=>c_otype_st ).
*	End	  -->	MgM DCEK904012

    SORT p_lt_estudiantes
      BY objid.

    LOOP AT p_lt_estudiantes INTO ls_estudiantes.
      REFRESH lt_admissions.

      lv_piqstudent = ls_estudiantes-sobid.

      CALL FUNCTION 'HRIQ_STUDENT_ADMIS_READ'
        EXPORTING
          iv_plvar        = c_01
          iv_st_objid     = lv_piqstudent
          iv_cs_objid     = ls_estudiantes-objid
          iv_first_perid  = p_perid
          iv_first_peryr  = p_peryr
        IMPORTING
          et_admissions   = lt_admissions
        EXCEPTIONS
          wrong_import    = 1
          technical_error = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        READ TABLE lt_admissions INTO ls_admissions
        INDEX 1.
        IF sy-subrc = 0.
          ls_estudiantes-istat = ls_admissions-istat.
"Alter info: Agregar Campos adicionales
"Fecha: 15/07/2019
          ls_estudiantes-FechaInscripcion = ls_admissions-ADM_RECPT.
"Fin alter
          MODIFY p_lt_estudiantes
          FROM ls_estudiantes
          TRANSPORTING istat FechaInscripcion.
        ENDIF.
      ELSE.
        DELETE p_lt_estudiantes.
      ENDIF.
    ENDLOOP.

    PERFORM f_filtrar_estudiantes
    CHANGING p_lt_estudiantes.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_ESTUDIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_ESTUDIOS  text
*----------------------------------------------------------------------*
FORM f_obtener_estudios USING "p_lt_planes type tyt_objid   "-->  MgM DCEK903347
                              ur_planes TYPE ty_ra_planes   "-->  MgM DCEK903347
                        CHANGING p_lt_estudios TYPE tyt_objid.

  DATA: lt_planes_aux   TYPE tyt_objid_aux.

  DATA: lt_value_tab  TYPE TABLE OF tys_f4_help,
        lt_return_tab TYPE TABLE OF ddshretval,
        lt_tipo_prog  TYPE TABLE OF dynpread,
        ls_return_tab TYPE ddshretval.

  CHECK ur_planes[] IS NOT INITIAL. "-->  MgM DCEK903347

  SELECT objid
         sobid
         otype
         sclas
         rsign
         relat
         subty
    FROM hrp1001
    INTO TABLE p_lt_estudios
    WHERE otype = c_sc
      AND sclas = c_cs
      AND plvar = c_01
      AND rsign = c_b
      AND relat = c_514
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*      and objid = p_planes.
      AND objid IN ur_planes.
*	End	  -->	MgM DCEK903347
  IF sy-subrc = 0.

*	Begin	-->	MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
    DELETE p_lt_estudios
      WHERE sobid NOT IN  zedu_cl_perfiles_struct=>get_range(
                            cl_hrpiq00const=>c_otype_cs ).
*	End	  -->	MgM DCEK904012

    SORT p_lt_estudios
    BY objid.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETAR_AUXILIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIOS  text
*      <--P_P_LT_ESTUDIOS_AUX  text
*----------------------------------------------------------------------*
FORM f_completar_auxiliar  USING    p_lt_original TYPE tyt_objid
                           CHANGING p_lt_auxiliar TYPE tyt_objid_aux.

  DATA: ls_original TYPE tys_objid,
        ls_auxiliar TYPE tys_objid_aux.

  REFRESH p_lt_auxiliar.

  LOOP AT p_lt_original INTO ls_original.
    ls_auxiliar-objid = ls_original-objid.
    ls_auxiliar-sobid = ls_original-sobid.

    APPEND ls_auxiliar TO
    p_lt_auxiliar.
    CLEAR ls_auxiliar.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETAR_AUXILIAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIOS  text
*      <--P_P_LT_ESTUDIOS_AUX  text
*----------------------------------------------------------------------*
FORM f_completar_auxiliar_tab USING p_lt_original TYPE tswhactor
                           CHANGING p_lt_auxiliar TYPE tyt_swhactor_aux.

  DATA: ls_original TYPE swhactor,
        ls_auxiliar TYPE swhactor.

  LOOP AT p_lt_original INTO ls_original.
    ls_auxiliar-objid = ls_original-objid.
    ls_auxiliar-otype = ls_original-otype.

    APPEND ls_auxiliar TO
    p_lt_auxiliar.
    CLEAR ls_auxiliar.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS_MAESTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_HRP1702  text
*----------------------------------------------------------------------*
FORM f_obtener_datos_maestros  USING p_lt_estudiantes TYPE tyt_objid
                               CHANGING p_lt_hrp1702    TYPE tyt_hrp1702
                                        p_lt_partner_id TYPE tyt_partner_id
                                        p_lt_pers_addr_num TYPE tyt_pers_addr_num
                                        p_lt_hrp1721    TYPE tyt_hrp1721
                                        p_tablaIntEPS   TYPE tablaEPS
                                        p_tablaMedio    TYPE tablaMedio.

  DATA: lt_estudiantes_aux TYPE tyt_objid_aux.

  PERFORM f_completar_auxiliar
  USING p_lt_estudiantes
  CHANGING lt_estudiantes_aux.

  CHECK lt_estudiantes_aux IS NOT INITIAL.

  SELECT a~objid
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
         a~begda
         a~endda
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
         a~vorna
         a~nachn
         a~gbdat
*         a~gbdep   "-->  MgM DCEK903347
         u~bezei    "-->  MgM DCEK903347
         a~gbort
         a~gblnd
         a~gesch
         b~dr_lic_no
    FROM hrp1702 AS a
      LEFT JOIN hrp1704 AS b
        ON a~objid = b~objid AND
           a~otype = b~otype
*	Begin	-->	MgM DCEK903347 Ajustes Varios 03/03/2017
      LEFT OUTER JOIN t005u AS u
        ON a~gbdep EQ u~bland AND
           a~gblnd EQ u~land1 AND
           u~spras EQ sy-langu
*	End	  -->	MgM DCEK903347
    INTO TABLE p_lt_hrp1702
      FOR ALL ENTRIES IN lt_estudiantes_aux
        WHERE a~objid = lt_estudiantes_aux-sobid
          AND a~otype = c_st.

  IF sy-subrc = 0.
    SORT p_lt_hrp1702
      BY objid ASCENDING
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
         endda DESCENDING
         begda DESCENDING.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
  ENDIF.

  SELECT a~stobjid
         a~partner
         a~student12
         b~idnumber
         c~text
     FROM cmacbpst AS a
     JOIN but0id AS b
     ON a~partner = b~partner
     JOIN tb039b AS c
     ON b~type = c~type
     INTO TABLE p_lt_partner_id
     FOR ALL ENTRIES IN lt_estudiantes_aux
     WHERE a~stobjid = lt_estudiantes_aux-sobid
       AND c~spras   = sy-langu
       AND b~type LIKE `FS%`. "DCEK904012 issue

  IF sy-subrc = 0.
    SORT p_lt_partner_id
    BY stobjid.

    SELECT a~partner
           a~persnumber
           b~addrnumber
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
           name_first
           namemiddle
           name_last
           name_lst2
*	End	  -->	MgM DCEK903347
      FROM but000 AS a
*	Begin	-->	MgM DCEK903347 addrnumber 02/03/2017
*      join but020 as b
*      on a~partner = b~partner
        LEFT OUTER JOIN but021_fs AS b
          ON a~partner EQ b~partner
*	End	  -->	MgM DCEK903347
      INTO TABLE p_lt_pers_addr_num
      FOR ALL ENTRIES IN p_lt_partner_id
      WHERE a~partner = p_lt_partner_id-partner.

  ENDIF.

  SELECT objid
         testtotres
    FROM hrp1721
    INTO TABLE p_lt_hrp1721
      FOR ALL ENTRIES IN lt_estudiantes_aux
      WHERE objid = lt_estudiantes_aux-sobid
        AND otype = c_st
        AND plvar = c_01.
  IF sy-subrc = 0.
    SORT p_lt_hrp1721
    BY objid.
  ENDIF.

  SELECT CODIGO, DESCRIPCION
  FROM ZEDU_EPS
  INTO TABLE @p_tablaIntEPS.
  IF sy-subrc = 0.
    SORT p_tablaIntEPS
    BY CodEPS.
  ENDIF.

  SELECT ID, MEDIO
  FROM ZEDU_MEDIOS
  INTO TABLE @p_tablaMedio.
  IF sy-subrc = 0.
    SORT p_tablaMedio
    BY CodMedio.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILTRAR_ESTUDIANTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LT_ESTUDIANTES  text
*----------------------------------------------------------------------*
FORM f_filtrar_estudiantes  CHANGING p_lt_estudiantes TYPE tyt_objid.


  DATA: lt_estudiantes_aux TYPE tyt_objid_aux,
        lt_estudiantes_ins TYPE tyt_objid,
        lt_estudiantes_adm TYPE tyt_objid,
        lt_estudiantes_mat TYPE tyt_objid,
*	Begin	--> FROSENZVAIG - DCEK906683 - 10/10/2017
        lt_estudiantes_rech TYPE tyt_objid.
*	End	--> FROSENZVAIG - DCEK906683 - 10/10/2017

  IF p_inscr = c_x.
    PERFORM f_obtener_inscritos
    USING    p_lt_estudiantes
    CHANGING lt_estudiantes_ins.
  ENDIF.

  IF p_admit = c_x.
    PERFORM f_obtener_admitidos
    USING    p_lt_estudiantes
    CHANGING lt_estudiantes_adm.
  ENDIF.

  IF p_matr_a IS NOT INITIAL
  OR p_matr_n IS NOT INITIAL.
    PERFORM f_obtener_matriculados
    USING    p_lt_estudiantes
    CHANGING lt_estudiantes_mat.
  ENDIF.
*	Begin	--> FROSENZVAIG - DCEK906683 - 10/10/2017
  IF p_rech EQ c_x.
    lt_estudiantes_rech[] = p_lt_estudiantes[].
    "Alter info: Determinación de Estado Estudiante según parámetro seleccionado en la pantalla de Selección
      FIELD-SYMBOLS: <fs_estudiantes_rech> TYPE tys_objid.
      LOOP AT lt_estudiantes_rech ASSIGNING <fs_estudiantes_rech>.
        MOVE 'Rechazados' TO <fs_estudiantes_rech>-EstadoEstParametro.
      ENDLOOP.
    "Fin alter.
    DELETE lt_estudiantes_rech[] WHERE istat NE '5'
                                    OR adm_enrcateg NE '03'.
  ENDIF.
*	End	--> FROSENZVAIG - DCEK906683 - 10/10/2017
  REFRESH p_lt_estudiantes.

  APPEND LINES OF lt_estudiantes_ins
  TO p_lt_estudiantes.
  APPEND LINES OF lt_estudiantes_adm
   TO p_lt_estudiantes.
  APPEND LINES OF lt_estudiantes_mat
   TO p_lt_estudiantes.
*	Begin	--> FROSENZVAIG - DCEK906683 - 10/10/2017
  APPEND LINES OF lt_estudiantes_rech
  TO p_lt_estudiantes.
*	End	--> FROSENZVAIG - DCEK906683 - 10/10/2017
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MOSTRAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_mostrar_datos .

  DATA: lt_catalog TYPE slis_t_fieldcat_alv,
        ls_layout  TYPE slis_layout_alv,
        ls_variant TYPE disvariant.

  PERFORM f_armar_catalogo
  CHANGING lt_catalog.

  PERFORM f_completar_layout
  CHANGING ls_layout.

  ls_variant-report = sy-repid.
  IF p_layout IS NOT INITIAL.
    ls_variant-variant = p_layout.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat   = lt_catalog
      is_layout     = ls_layout
      is_variant    = ls_variant
      i_save        = c_a
    TABLES
      t_outtab      = gt_output
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_CATALOGO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_CATALOG  text
*----------------------------------------------------------------------*
FORM f_armar_catalogo CHANGING p_lt_catalog TYPE slis_t_fieldcat_alv.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = c_zedu_s_directorio_inscritos
    CHANGING
      ct_fieldcat            = p_lt_catalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    REFRESH p_lt_catalog.
  ENDIF.

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
  "Munic.Nacim
  PERFORM f_catalog_cambia_text USING     `GBDEP`
                                          text-006
                                CHANGING  p_lt_catalog.
  "Estado inscrito
  PERFORM f_catalog_cambia_text USING     `ADM_ENRCATEG`
                                          text-007
                                CHANGING  p_lt_catalog.
  "Estado estudiante
  PERFORM f_catalog_cambia_text USING     `ADM_CATEGT`
                                          text-008
                                CHANGING  p_lt_catalog.
  "Tarjeta Profesional
  PERFORM f_catalog_cambia_text USING     `DR_LIC_NO`
                                          text-009
                                CHANGING  p_lt_catalog.
  "Ciudad Nacimiento
  PERFORM f_catalog_cambia_text USING     `GBORT`
                                          text-010
                                CHANGING  p_lt_catalog.
  "Dirección
  PERFORM f_catalog_cambia_text USING     `STREET`
                                          text-011
                                CHANGING  p_lt_catalog.
  "País.direcc
  PERFORM f_catalog_cambia_text USING     `COUNTRY`
                                          text-012
                                CHANGING  p_lt_catalog.
  "Munic.direcc
  PERFORM f_catalog_cambia_text USING     `REGION`
                                          text-013
                                CHANGING  p_lt_catalog.
  "Ciudad.direcc
  PERFORM f_catalog_cambia_text USING     `CITY1`
                                          text-014
                                CHANGING  p_lt_catalog.
  "Programa
  PERFORM f_catalog_cambia_text USING     `PROGRAMA`
                                          text-015
                                CHANGING  p_lt_catalog.
  "Período académico
  PERFORM f_catalog_cambia_text USING     `ADM_PERID`
                                          text-016
                                CHANGING  p_lt_catalog.
  "Tipo asp/est
  PERFORM f_catalog_cambia_text USING     `ADM_CATEGT`
                                          text-017
                                CHANGING  p_lt_catalog.
  "Asistencia.exam
  PERFORM f_catalog_cambia_text USING     `ASSISTANCE_EX`
                                          text-018
                                CHANGING  p_lt_catalog.
  "Puntaje exam
  PERFORM f_catalog_cambia_text USING     `SCORE_EX`
                                          text-019
                                CHANGING  p_lt_catalog.

*	Begin	-->	MgM DCEK904012 Procedencia 03/04/2017
  "País de Procedencia
  PERFORM f_catalog_cambia_text USING     `PAIS_PRCDNC`
                                          text-021
                                CHANGING  p_lt_catalog.

  "Dpto. procedencia
  PERFORM f_catalog_cambia_text USING     `DEPTO_PRCDNC`
                                          text-022
                                CHANGING  p_lt_catalog.

  "Ciudad de procedencia
  PERFORM f_catalog_cambia_text USING     `CIUDAD_PROC`
                                          text-023
                                CHANGING  p_lt_catalog.
  "Primera Opción
  PERFORM f_catalog_cambia_text USING     `PRIMERA_OPCION`
                                          text-024
                                CHANGING  p_lt_catalog.
  "Segunda Opción
  PERFORM f_catalog_cambia_text USING     `SEGUNDA_OPCION`
                                          text-025
                                CHANGING  p_lt_catalog.
  "Título bachiller
  PERFORM f_catalog_cambia_text USING     `TITULO_C`
                                          text-026
                                CHANGING  p_lt_catalog.
*	End	  -->	MgM DCEK904012

***INICIO HIRS 02/10/2017 "Solucion Issues directorio de Inscritos
***Se comenta debido a que esto hace que no se muestren estos campos en el ALV
**  READ TABLE p_lt_catalog
**    ASSIGNING FIELD-SYMBOL(<fs_cata>)
**      WITH KEY fieldname = `TIME_PLACE_DATE_INT`.
**
**  IF sy-subrc EQ 0.
**    <fs_cata>-tech  = cl_bp_const=>true.
**  ENDIF.
**
**  READ TABLE p_lt_catalog
**    ASSIGNING <fs_cata>
**      WITH KEY fieldname = `ASSISTANCE_INT`.
**
**  IF sy-subrc EQ 0.
**    <fs_cata>-tech  = cl_bp_const=>true.
**  ENDIF.
**
**  READ TABLE p_lt_catalog
**    ASSIGNING <fs_cata>
**      WITH KEY fieldname = `SCORE_INT`.
**
**  IF sy-subrc EQ 0.
**    <fs_cata>-tech  = cl_bp_const=>true.
**  ENDIF.
**
**  READ TABLE p_lt_catalog
**    ASSIGNING <fs_cata>
**      WITH KEY fieldname = `OBS_INT`.
**
**  IF sy-subrc EQ 0.
**    <fs_cata>-tech  = cl_bp_const=>true.
**  ENDIF.
**
**  READ TABLE p_lt_catalog
**    ASSIGNING <fs_cata>
**      WITH KEY fieldname = `CITY_INT`.
**
**  IF sy-subrc EQ 0.
**    <fs_cata>-tech  = cl_bp_const=>true.
**  ENDIF.
***FIN HIRS 02/10/2017 "Solucion Issues directorio de Inscritos

*	End	  -->	MgM DCEK903347

  "Estado de Estudiante según parámetros de la pantalla de Selección
  PERFORM f_catalog_cambia_text USING     'STOBJID'
                                          text-048
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'ESTADOESTPARAMETRO'
                                          text-050
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'PIN'
                                          text-051
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'EPS'
                                          text-052
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'FACULTAD'
                                          text-053
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'TIPOPROGRAMA'
                                          text-054
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'MEDIO'
                                          text-055
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'FECHAINSCRIPCION'
                                          text-056
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'NROFORMULARIO'
                                          text-057
                                CHANGING  p_lt_catalog.
  PERFORM f_catalog_cambia_text USING     'ANNOTITULO'
                                          text-058
                                CHANGING  p_lt_catalog.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_SALIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      -->P_LT_HRP1702  text
*----------------------------------------------------------------------*
FORM f_armar_salida
    USING p_lt_estudiantes    TYPE tyt_objid
          p_lt_hrp1702        TYPE tyt_hrp1702
          p_lt_hrp9003        TYPE tyt_hrp9003
          p_lt_partner_id     TYPE tyt_partner_id
          p_lt_hrp1721        TYPE tyt_hrp1721
          p_lt_programas      TYPE tyt_programas
          p_lt_procesos_ant   TYPE tyt_procesos_ant
*          p_lt_solic_prog     TYPE tyt_procesos_ant "--> Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
          p_lt_entrevista     TYPE zedu_t_fec_exam_entrevista
          p_lt_examen         TYPE zedu_t_fec_exam_entrevista
          p_lt_pers_addr_num  TYPE tyt_pers_addr_num
*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017
          p_t_estudios        TYPE tyt_objid
          p_t_colegios        TYPE ty_t_colegios
          p_t_universidades   TYPE ty_t_universidad
*	End	  -->	MgM DCEK903347
"Alter Info: Adicionar Campos
"Autor: Sebastian Espinosa Marin
"Fecha: 12/07/2019
         p_tablaIntEPS        TYPE tablaEPS
         p_tablaIntMedio      TYPE tablaMedio.
"Fin later
  DATA: ls_output        TYPE zedu_s_directorio_inscritos,
        ls_estudiantes   TYPE tys_objid,
        ls_hrp1072       TYPE tys_hrp1702,
        ls_hrp1721       TYPE tys_hrp1721,
        ls_hrp9003       TYPE tys_hrp9003,
        ls_hrp1028       TYPE tys_hrp1028,
        ls_partner_id    TYPE tys_partner_id,
        ls_programas     TYPE tys_programas,
        ls_procesos_ant  TYPE tys_procesos_ant,
        ls_solic_prog    TYPE tys_procesos_ant,
        ls_entrevista    TYPE zedu_s_fec_exam_entrevista,
        ls_niveles       TYPE tys_niveles,
        ls_tipos         TYPE tys_tipos,
        ls_categorias    TYPE tys_categorias,
        ls_examen        TYPE zedu_s_fec_exam_entrevista,
        ls_pers_addr_num TYPE tys_pers_addr_num,
        lv_plan_estudio  TYPE hrp1000-stext,
        lv_years         TYPE tfmatage,
        lv_fecha         TYPE char10,
        lv_hora          TYPE char8,
        lv_colegio       TYPE hrp9003-colegio,
        ls_EPS           TYPE estructuraEPS,
        ls_Medio         TYPE estructuraMedio.

  DATA: lt_usage                      TYPE crmt_adsuse_tab,
        lt_adtel                      TYPE crmt_admob_tab,
        lt_niveles                    TYPE tyt_niveles,
        lt_categorias                 TYPE tyt_categorias,
        lt_tipos                      TYPE tyt_tipos,
        lt_hrp1028                    TYPE tyt_hrp1028,
        lv_table_type                 TYPE szad_field-table_type,
        ls_address_personal_selection TYPE addr2_sel,
        ls_address_personal_value     TYPE addr2_val,
        ls_address_personal_info      TYPE ad2_flags.

  DATA: lv_NombreFacultad(100)  TYPE C,
        lv_NombreTipoPrograma(100) TYPE C.

  IF p_lt_estudiantes IS NOT INITIAL.

    PERFORM f_obtener_niveles
    USING p_lt_estudiantes
    CHANGING lt_niveles.

    PERFORM f_obtener_categorias
    USING p_lt_estudiantes
    CHANGING lt_categorias.

    PERFORM f_obtener_tipo
    USING p_lt_estudiantes
    CHANGING lt_tipos.

  ENDIF.

  SORT p_t_estudios BY sobid.

*Denominacion programa
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*  select single stext
*    from hrp1000
*    into lv_plan_estudio
*    where objid = p_planes
*      and otype = c_sc
*      and plvar = c_01
*      and langu = sy-langu.
*  if sy-subrc ne 0.
*    clear lv_plan_estudio.
*  endif.
  SELECT  objid,
          stext
    FROM hrp1000
      INTO TABLE @DATA(lt_planes_txt)
    WHERE objid IN @gr_planes
      AND otype = @c_sc
      AND plvar = @c_01
      AND langu = @sy-langu
        ORDER BY objid.

  IF sy-subrc NE 0.
    CLEAR lt_planes_txt[].
  ENDIF.
*	End	  -->	MgM DCEK903347

*Ciudades de entrevista y examen
  IF p_lt_entrevista IS NOT INITIAL.
    SELECT objid
           ort01
      FROM hrp1028
      INTO TABLE lt_hrp1028
      FOR ALL ENTRIES IN p_lt_entrevista
      WHERE plvar = c_01
        AND otype = c_f
        AND objid = p_lt_entrevista-location_objid.
  ENDIF.
  IF p_lt_examen IS NOT INITIAL.
    SELECT objid
           ort01
      FROM hrp1028
      APPENDING TABLE lt_hrp1028
      FOR ALL ENTRIES IN p_lt_examen
      WHERE plvar = c_01
        AND otype = c_f
        AND objid = p_lt_examen-location_objid.
  ENDIF.

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
  SORT p_lt_partner_id BY stobjid.
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*  SORT p_lt_hrp1702 BY objid.
  SORT p_lt_hrp1702 BY objid ASCENDING endda DESCENDING begda DESCENDING.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
"Alter Info: Ordenar registros de mayor a menor para que lea el primer registro (más actual)
"Autor: Sebastián Espinosa Marín
"Fecha: 22/074/2019
  SORT p_lt_hrp9003 BY objid ASCENDING endda DESCENDING begda DESCENDING.
"Fin alter
  SORT p_lt_hrp1721 BY objid.
*	End	  -->	MgM DCEK903347

"Alter Info: Agregar campos adicionales
"Autor: Sebastian Espinosa Marin
"Fecha: 12/07/2019
  SELECT SINGLE MC_STEXT FROM HRP1000
  INTO lv_NombreFacultad
  WHERE plvar = c_01
        AND otype = c_o
        AND objid = P_FACUL.
  SELECT SINGLE MC_STEXT FROM HRP1000
  INTO lv_NombreTipoPrograma
  WHERE plvar = c_01
        AND otype = c_o
        AND objid = p_tippro.
"Fin alter
  LOOP AT p_lt_estudiantes INTO ls_estudiantes.
* Inicio Agregado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
    READ TABLE p_t_estudios
      WITH KEY sobid = ls_estudiantes-objid BINARY SEARCH
        INTO DATA(ls_estudio).

    IF sy-subrc EQ 0.
      READ TABLE lt_planes_txt
        WITH KEY objid = ls_estudio-objid BINARY SEARCH
          INTO DATA(ls_plan).

      IF sy-subrc EQ 0.
        ls_output-programa =  ls_plan-stext.
      ENDIF.
    ENDIF.
* Fin Agregado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

    ls_output-adm_ayear     = ls_estudiantes-adm_ayear.
    ls_output-adm_perid     = ls_estudiantes-adm_perid.
    ls_output-aclevelt      = ls_estudiantes-adm_aclevel.

    READ TABLE lt_niveles INTO ls_niveles
    WITH KEY aclevel = ls_estudiantes-adm_aclevel.
    IF sy-subrc = 0.
      ls_output-aclevelt = ls_niveles-aclevelt.
    ENDIF.

    READ TABLE lt_categorias INTO ls_categorias
    WITH KEY enrcateg = ls_estudiantes-adm_enrcateg.
    IF sy-subrc = 0.
      ls_output-adm_enrcateg = ls_categorias-enrcategt.
    ENDIF.

    READ TABLE p_lt_partner_id INTO ls_partner_id
    WITH KEY stobjid = ls_estudiantes-sobid
    BINARY SEARCH.
    IF sy-subrc = 0.
      ls_output-student12 = ls_partner_id-student12.
      ls_output-idnumber  = ls_partner_id-idnumber.
      ls_output-type_id   = ls_partner_id-type.

* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*      READ TABLE p_lt_procesos_ant INTO ls_procesos_ant
*      WITH KEY nro_documen = ls_partner_id-idnumber.
*      IF sy-subrc = 0.
*        ls_output-quan_inscr = ls_procesos_ant-cant_inscr.
*      ENDIF.
*
*      READ TABLE p_lt_solic_prog INTO ls_solic_prog
*      WITH KEY nro_documen = ls_partner_id-idnumber.
*      IF sy-subrc = 0.
**        ls_output-sta1_solpago = ls_solic_prog-sta1_solpago.
*        ls_output-sta2_solpago = ls_solic_prog-sta2_solpago. "-->  MgM DCEK903347
*      ENDIF.
*
      ls_output-quan_inscr = ls_partner_id-cant_inscr.

      "Inicializa la estructura
      CLEAR ls_procesos_ant.

      "Busca el registro de preinscripcion al programa en primera opcion
      READ TABLE p_lt_procesos_ant INTO ls_procesos_ant
        WITH KEY nro_documen = ls_partner_id-idnumber
                 programa_1  = ls_estudio-objid.

      "Si no encuentra el registro
      IF sy-subrc <> 0.
        "Busca el registro de preinscripcion al programa en segunda opcion
        READ TABLE p_lt_procesos_ant INTO ls_procesos_ant
          WITH KEY nro_documen = ls_partner_id-idnumber
                   programa_2  = ls_estudio-objid.

        "Si no encuentra el registro
        IF sy-subrc <> 0.
          "Busca el registro de preinscripcion por identificacion
          READ TABLE p_lt_procesos_ant INTO ls_procesos_ant
            WITH KEY nro_documen = ls_partner_id-idnumber
            BINARY SEARCH.
        ENDIF.
      ENDIF.

      ls_output-sta2_solpago = ls_procesos_ant-sta2_solpago.

      MOVE ls_procesos_ant-pais_prcdnc_tx  TO ls_output-pais_prcdnc.
      MOVE ls_procesos_ant-depto_prcdnc_tx TO ls_output-depto_prcdnc.
      MOVE ls_procesos_ant-ciudad_proc_tx  TO ls_output-ciudad_proc.
      MOVE ls_procesos_ant-primera_opcion  TO ls_output-primera_opcion.
      MOVE ls_procesos_ant-segunda_opcion  TO ls_output-segunda_opcion.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
"Alter info: Agregar campo PIN y EPS
"Autor: Sebastián Espinosa Marin
"Fecha: 08/07/2019
      MOVE ls_procesos_ant-nr_formulario TO ls_output-NroFormulario.
      MOVE ls_procesos_ant-facultad  TO ls_output-facultad.
      MOVE ls_procesos_ant-tipoprograma  TO ls_output-tipoprograma.
      MOVE ls_procesos_ant-Medio TO ls_output-medio.
      MOVE ls_procesos_ant-Pin TO ls_output-Pin.

      READ TABLE p_tablaIntEPS INTO ls_EPS
      WITH KEY CodEPS = ls_procesos_ant-EPS.
      IF sy-subrc = 0.
        ls_output-EPS = ls_EPS-NombreEPS.
      ENDIF.
      READ TABLE p_tablaIntMedio INTO ls_Medio
      WITH KEY CodMedio = ls_procesos_ant-Medio.
      IF sy-subrc = 0.
        ls_output-Medio = ls_Medio-NombreMedio.
      ENDIF.

"Fin Alter
      READ TABLE p_lt_pers_addr_num INTO ls_pers_addr_num
      WITH KEY partner = ls_partner_id-partner.
      IF sy-subrc = 0.
        PERFORM f_armar_direccion_telefono
        USING ls_pers_addr_num
        CHANGING ls_output.

*Alter Info: Incluir información de correo electrónico y de Credencial
* Autor: Sebastián Espinosa Marín
* Fecha: 31/05/2019
        PERFORM f_traer_direccion_correo
        USING ls_pers_addr_num
        CHANGING ls_output.
        ls_output-STOBJID = ls_estudiantes-sobid.
*Fin alter info.
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
        CONCATENATE ls_pers_addr_num-name_first
                    ls_pers_addr_num-namemiddle
          INTO ls_output-vorna
            SEPARATED BY space.

        CONCATENATE ls_pers_addr_num-name_last
                    ls_pers_addr_num-name_lst2
          INTO ls_output-nachn
            SEPARATED BY space.
*	End	  -->	MgM DCEK903347

      ENDIF.
* Inicio Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
**  Begin --> MgM DCEK904012 Procedencia 03/04/2017
*      MOVE ls_partner_id-pais_prcdnc  TO ls_output-pais_prcdnc.
*      MOVE ls_partner_id-depto_prcdnc TO ls_output-depto_prcdnc.
*      MOVE ls_partner_id-ciudad_proc  TO ls_output-ciudad_proc.
*      MOVE ls_partner_id-primera_opcion  TO ls_output-primera_opcion.
*      MOVE ls_partner_id-segunda_opcion  TO ls_output-segunda_opcion.
**  End   --> MgM DCEK904012
* Fin Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
    ENDIF.

    READ TABLE p_lt_hrp1702 INTO ls_hrp1072
    WITH KEY objid = ls_estudiantes-sobid
    BINARY SEARCH.
    IF sy-subrc = 0.
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*      ls_output-vorna     = ls_hrp1072-vorna.
*      ls_output-nachn     = ls_hrp1072-nachn.
*	End	  -->	MgM DCEK903347
      ls_output-dr_lic_no = ls_hrp1072-dr_lic_no.
      ls_output-gbdep     = ls_hrp1072-gbdep.
      ls_output-gbort     = ls_hrp1072-gbort.
      ls_output-gblnd     = ls_hrp1072-gblnd.
      CASE ls_hrp1072-gesch..
        WHEN c_1.
          ls_output-gesch = c_m.
        WHEN c_2.
          ls_output-gesch = c_f.
      ENDCASE.
      IF ls_hrp1072-gbdat IS NOT INITIAL.

        ls_output-gbdat   = ls_hrp1072-gbdat.

        CALL FUNCTION 'COMPUTE_YEARS_BETWEEN_DATES'
          EXPORTING
            first_date                  = ls_hrp1072-gbdat
            second_date                 = sy-datum
          IMPORTING
            years_between_dates         = lv_years
          EXCEPTIONS
            sequence_of_dates_not_valid = 1
            others>                     = 2.
        IF sy-subrc = 0.
          ls_output-age = lv_years.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE p_lt_hrp9003 INTO ls_hrp9003
    WITH KEY objid = ls_estudiantes-sobid
    BINARY SEARCH.
    IF sy-subrc = 0.
*      ls_output-colegio      = ls_hrp9003-colegio.
*      ls_output-universidad  = ls_hrp9003-universidad.
*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017

* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*      read table p_t_colegios
*        with key codigo = ls_hrp9003-colegio
*          into data(ls_colegio) binary search.
*
*      if sy-subrc eq 0.
*        move ls_colegio-nombre_colegio  to ls_output-colegio.
*      elseif ls_hrp9003-colegio_otro is not initial.
*        move ls_hrp9003-colegio_otro to ls_output-colegio.
*      else.
*        move ls_hrp9003-colegio to ls_output-colegio.
*      endif.
*
      "Asigna el valor del colegio, elimina espacios y pasa a mayusculas
      lv_colegio = ls_hrp9003-colegio.
      TRANSLATE lv_colegio TO UPPER CASE.
      CONDENSE lv_colegio NO-GAPS.

      "Si no se tiene colegio
      IF lv_colegio IS INITIAL OR lv_colegio = 'OTRO'.
        "Asigna el nombre que se encuentra en el campo otro colegio
        MOVE ls_hrp9003-colegio_otro TO ls_output-colegio.

        "Si se tienen datos
      ELSE.
        "Obtiene el nombre del colegio a partir del codigo
        READ TABLE p_t_colegios
          WITH KEY codigo = ls_hrp9003-colegio
            INTO DATA(ls_colegio) BINARY SEARCH.

        "Si encuentra el nombre del colegio
        IF sy-subrc EQ 0.
          "Asigna el nombre del colegio
          MOVE ls_colegio-nombre_colegio  TO ls_output-colegio.

          "Si no lo encuentra
        ELSE.
          "Asigna el codigo o nombre ingresado en el campo colegio
          MOVE ls_hrp9003-colegio TO ls_output-colegio.
        ENDIF.
      ENDIF.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

"Alter Info: Incluir Año de Titulo del Colegio
"Autor: Sebastián Espinosa Marín
      MOVE ls_hrp9003-AnnoTitulo TO ls_output-AnnoTitulo.
"Fin alter.
      READ TABLE p_t_universidades
        WITH KEY codigo = ls_hrp9003-universidad
          INTO DATA(ls_univer) BINARY SEARCH.

      IF sy-subrc EQ 0.
        MOVE ls_univer-universidad  TO ls_output-universidad.
*	Begin	-->	MgM DCEK904012 issue universidad 28/03/2017
*      else.
      ELSEIF ls_hrp9003-universidad_otra IS NOT INITIAL.
*	End	  -->	MgM DCEK904012
        MOVE ls_hrp9003-universidad_otra TO ls_output-universidad.
*	Begin	-->	MgM DCEK904012 issue universidad 28/03/2017
      ELSE.
        MOVE ls_hrp9003-universidad TO ls_output-universidad.
*	End	  -->	MgM DCEK904012
      ENDIF.
*	End	  -->	MgM DCEK903347
      ls_output-titulo_u     = ls_hrp9003-titulo_u.
      ls_output-testtotres   = ls_hrp9003-saber_11. "MgM DCEK903347
      ls_output-titulo_c     = ls_hrp9003-titulo_c. "MgM DCEK904012
    ENDIF.

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*    read table p_lt_hrp1721 into ls_hrp1721
*    with key objid = ls_estudiantes-sobid
*    binary search.
*    if sy-subrc = 0.
*      ls_output-testtotres = ls_hrp1721-testtotres.
*    endif.
*	End	  -->	MgM DCEK903347

    READ TABLE p_lt_programas INTO ls_programas
    WITH KEY choice_no = ls_estudiantes-choice_no.
    IF sy-subrc = 0.
      ls_output-program_choice = ls_programas-choice_not.
    ENDIF.

    READ TABLE p_lt_entrevista INTO ls_entrevista
    WITH KEY objid = ls_estudiantes-sobid.
    IF sy-subrc = 0.

      ls_output-score_int = ls_entrevista-valoracion.
      ls_output-obs_int   = ls_entrevista-concepto.

      CONCATENATE
      ls_entrevista-examdate+6(2)
      ls_entrevista-examdate+4(2)
      ls_entrevista-examdate(4)
      INTO
      lv_fecha
      SEPARATED BY c_barra.

      CONCATENATE
      ls_entrevista-exambegtime(2)
      ls_entrevista-exambegtime+2(2)
      ls_entrevista-exambegtime+4(2)
      INTO
      lv_hora
      SEPARATED BY c_dos_puntos.

      CONCATENATE
      lv_fecha
      lv_hora
*	Begin	-->	MgM DCEK903347 Ajustes Varios 06/03/2017
*      ls_entrevista-location_objid
      ls_entrevista-location_stext
*	End	  -->	MgM DCEK903347
      INTO
      ls_output-time_place_date_int
      SEPARATED BY
      c_coma.

      READ TABLE lt_hrp1028 INTO ls_hrp1028
      WITH KEY objid = ls_entrevista-location_objid.
      IF sy-subrc = 0.
        ls_output-city_int = ls_hrp1028-ort01.
      ENDIF.

    ENDIF.

    READ TABLE p_lt_examen INTO ls_examen
    WITH KEY objid = ls_estudiantes-sobid.
    IF sy-subrc = 0.

      ls_output-score_ex = ls_examen-valoracion.
      ls_output-obs_ex   = ls_examen-concepto.

      CONCATENATE
      ls_examen-examdate+6(2)
      ls_examen-examdate+4(2)
      ls_examen-examdate(4)
      INTO
      lv_fecha
      SEPARATED BY c_barra.

      CONCATENATE
      ls_examen-exambegtime(2)
      ls_examen-exambegtime+2(2)
      ls_examen-exambegtime+4(2)
      INTO
      lv_hora
      SEPARATED BY c_dos_puntos.

      CONCATENATE
      lv_fecha
      lv_hora
*	Begin	-->	MgM DCEK903347 Ajustes Varios 06/03/2017
*      ls_examen-location_objid
      ls_examen-location_stext
*	End	  -->	MgM DCEK903347
      INTO
      ls_output-time_place_date_ex
      SEPARATED BY
      c_coma.

      READ TABLE lt_hrp1028 INTO ls_hrp1028
      WITH KEY objid = ls_examen-location_objid.
      IF sy-subrc = 0.
        ls_output-city_ex = ls_hrp1028-ort01.
      ENDIF.

    ENDIF.

    READ TABLE lt_tipos INTO ls_tipos
    WITH KEY adm_categ = ls_estudiantes-adm_categ.
    IF sy-subrc = 0.
      ls_output-adm_categt = ls_tipos-adm_categt.
    ENDIF.

* Inicio Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
* Se borra debido a que se pasa esta parte para el inicio del ciclo
*
**  Begin --> MgM DCEK903347 Ajustes varios 09/02/2017
**    ls_output-programa = lv_plan_estudio.
*
*    READ TABLE p_t_estudios
*      WITH KEY sobid = ls_estudiantes-objid BINARY SEARCH
*        INTO DATA(ls_estudio).
*
*    IF sy-subrc EQ 0.
*      READ TABLE lt_planes_txt
*        WITH KEY objid = ls_estudio-objid BINARY SEARCH
*          INTO DATA(ls_plan).
*
*      IF sy-subrc EQ 0.
*        ls_output-programa =  ls_plan-stext.
*      ENDIF.
*    ENDIF.
**  End   --> MgM DCEK903347
* Fin Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

    "Alter info: Incluir campos adicionales
    "Autor: Sebastián Espinosa Marin
    "Fecha: 08/07/2019
    ls_output-EstadoEstParametro = ls_estudiantes-EstadoEstParametro.
    ls_output-FechaInscripcion = ls_estudiantes-FechaInscripcion.
    "Fin alter

    APPEND ls_output TO gt_output.
    CLEAR ls_output.

  ENDLOOP.

*	Begin	-->	MgM  DCEK904012 ordenar por apellido 18/04/2017
  SORT gt_output
    BY nachn ASCENDING.
*	End	  -->	MgM  DCEK904012

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COMPLETAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_LAYOUT  text
*----------------------------------------------------------------------*
FORM f_completar_layout  CHANGING p_ls_layout TYPE slis_layout_alv.

  p_ls_layout-zebra             = c_x.
  p_ls_layout-colwidth_optimize = c_x.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_LAYOUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_layouts .

  DATA: ls_layout_key  TYPE salv_s_layout_key,
        ls_layout_info TYPE salv_s_layout_info.

  ls_layout_key-report = sy-repid.
  ls_layout_info       = cl_salv_layout_service=>f4_layouts( ls_layout_key ).
  p_layout             = ls_layout_info-layout.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_RANGO_PERIODO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_rango_periodo.

  SELECT SINGLE b~openfrom
                b~opento
    FROM t7piqyearprd AS a
    INNER JOIN t7piqpkeyi AS b
    ON a~persl = b~persl
    INTO (gv_begda,
          gv_endda)
    WHERE a~peryr = p_peryr
      AND a~perid = p_perid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_INSCRITOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_ESTUDIANTES_INS  text
*----------------------------------------------------------------------*
FORM f_obtener_inscritos USING  p_lt_estudiantes TYPE tyt_objid
                         CHANGING p_lt_estudiantes_ins TYPE tyt_objid.

  p_lt_estudiantes_ins = p_lt_estudiantes.
"Alter info: Determinación de Estado Estudiante según parámetro seleccionado en la pantalla de Selección
  FIELD-SYMBOLS: <fs_estudiantes_ins> TYPE tys_objid.
  LOOP AT p_lt_estudiantes_ins ASSIGNING <fs_estudiantes_ins>.
    MOVE 'Inscritos' TO <fs_estudiantes_ins>-EstadoEstParametro.
  ENDLOOP.
"Fin alter.
  DELETE p_lt_estudiantes_ins
  WHERE   rsign        NE c_b
     OR   relat        NE c_530
     OR   istat        NE c_2.
***INICIO HIRS 03/10/2017 "Solucion Issues directorio de Inscritos
**     OR ( adm_enrcateg NE c_01
**    AND   adm_enrcateg NE c_06 ).
***FIN HIRS 03/10/2017 "Solucion Issues directorio de Inscritos

*  Begin --> MgM  DCEK904012 agregado filtro 18/04/2017
  PERFORM f_filtrar_1771 CHANGING p_lt_estudiantes_ins.
*  End   --> MgM  DCEK904012

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_ADMITIDOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_ESTUDIANTES_ADM  text
*----------------------------------------------------------------------*
FORM f_obtener_admitidos USING  p_lt_estudiantes TYPE tyt_objid
                         CHANGING p_lt_estudiantes_adm TYPE tyt_objid.

  DATA: lt_estudiantes_no_adm TYPE tyt_objid,
        ls_estudiantes_no_adm TYPE tys_objid,
        ls_estudiantes_adm    TYPE tys_objid.

  p_lt_estudiantes_adm = p_lt_estudiantes.
"Alter info: Determinación de Estado Estudiante según parámetro seleccionado en la pantalla de Selección
  FIELD-SYMBOLS: <fs_estudiantes_adm> TYPE tys_objid.
  LOOP AT p_lt_estudiantes_adm ASSIGNING <fs_estudiantes_adm>.
    MOVE 'Admitidos' TO <fs_estudiantes_adm>-EstadoEstParametro.
  ENDLOOP.
"Fin alter.
  DELETE p_lt_estudiantes_adm
  WHERE   rsign        NE c_b
     OR   relat        NE c_530
     OR   istat        NE c_1.
***INICIO HIRS 03/10/2017 "Solucion Issues directorio de Inscritos
**     OR ( adm_enrcateg NE c_02
**    AND   adm_enrcateg NE c_05
**    AND   adm_enrcateg NE c_07
**    AND   adm_enrcateg NE c_08 ).
***FIN HIRS 03/10/2017 "Solucion Issues directorio de Inscritos

*  Begin --> MgM  DCEK904012 acceso erroneo 18/04/2017
*  if p_lt_estudiantes_adm is not initial.
*
*    select objid
*      from hrp1769
*      into table lt_estudiantes_no_adm
*      for all entries in p_lt_estudiantes_adm
*      where objid = p_lt_estudiantes_adm-objid
*        and plvar = c_01
*        and otype = c_cs.
*    if sy-subrc = 0.
**  Begin --> MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
*      delete lt_estudiantes_no_adm
*        where objid not in  zedu_cl_perfiles_struct=>get_range(
*                              cl_hrpiq00const=>c_otype_cs ).
**  End   --> MgM DCEK904012
*
*      sort lt_estudiantes_no_adm
*        by objid.
*
*      loop at p_lt_estudiantes_adm into ls_estudiantes_adm.
*        read table lt_estudiantes_no_adm into ls_estudiantes_no_adm
*        with key objid = ls_estudiantes_adm-objid.
*        if sy-subrc = 0.
*          delete p_lt_estudiantes_adm.
*        endif.
*      endloop.
*    endif.
*
*  endif.
  PERFORM f_filtrar_1771 CHANGING p_lt_estudiantes_adm.
*  End   --> MgM  DCEK904012

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_MATRICULADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_ESTUDIANTES_MAT  text
*----------------------------------------------------------------------*
FORM f_obtener_matriculados USING p_lt_estudiantes     TYPE tyt_objid
                         CHANGING p_lt_estudiantes_mat TYPE tyt_objid.

  DATA: lt_estudiantes_mat_aux TYPE tyt_objid,
        ls_estudiantes_mat_aux TYPE tys_objid,
        ls_estudiantes_mat     TYPE tys_objid.
*	Begin	-->	MgM DCEK903347 Filtro matriculados nuevos y antiguos 08/03/2017
  DATA lv_cont      TYPE i.
  DATA lt_antiguos  TYPE STANDARD TABLE OF hrobjid.
  DATA lt_nuevos    TYPE STANDARD TABLE OF hrobjid.
*	End	  -->	MgM DCEK903347

  p_lt_estudiantes_mat = p_lt_estudiantes.
"Alter info: Determinación de Estado Estudiante según parámetro seleccionado en la pantalla de Selección
"  FIELD-SYMBOLS: <fs_estudiantes_mat> TYPE tys_objid.
"  LOOP AT p_lt_estudiantes_mat ASSIGNING <fs_estudiantes_mat>.
"    MOVE 'Matriculados' TO <fs_estudiantes_mat>-EstadoEstParametro.
"  ENDLOOP.
"Fin alter.
  DELETE p_lt_estudiantes_mat
  WHERE   rsign        NE c_b
     OR   relat        NE c_530
     OR   istat        NE c_1.
***INICIO HIRS 03/10/2017 "Solucion Issues directorio de Inscritos
**     OR ( adm_enrcateg NE c_02
**    AND   adm_enrcateg NE c_05
**    AND   adm_enrcateg NE c_07
**    AND   adm_enrcateg NE c_08 ).
***FIN HIRS 03/10/2017 "Solucion Issues directorio de Inscritos

  IF p_lt_estudiantes_mat IS NOT INITIAL.

*	Begin	-->	MgM DCEK903347 Filtro matriculados nuevos y antiguos 08/03/2017
*    select a~objid
*      from hrp1769 as a
*      inner join hrp1771 as b
*      on a~objid = b~objid
*     and a~otype = b~otype
*      into table lt_estudiantes_mat_aux
*      for all entries in p_lt_estudiantes_mat
*      where a~objid       = p_lt_estudiantes_mat-objid
*        and a~istat       = c_1
*        and a~endda       = c_99991231
*        and a~otype       = c_cs
*        and a~beg_process = c_ra01
*        and b~istat       = c_1
*        and b~pr_status   = c_1
*        and b~prs_state   = c_a
*        and b~ayear       = p_peryr
*        and b~perid       = p_perid.

    SELECT b~plvar,
           b~otype,
           b~objid,
           b~subty,
           b~istat,
           b~begda,
           b~endda,
           b~varyf,
           b~seqnr,
*	Begin	-->	MgM DCEK903794 filtro nuevos por periodo 08/03/2017
           b~ayear,
           b~perid
*	End	  -->	MgM DCEK903794
      FROM hrp1769 AS a
      INNER JOIN hrp1771 AS b
      ON a~objid = b~objid
     AND a~otype = b~otype
      INTO TABLE @DATA(lt_estud_1771)
      FOR ALL ENTRIES IN @p_lt_estudiantes_mat
      WHERE a~objid       = @p_lt_estudiantes_mat-objid
        AND a~istat       = @c_1
        AND a~endda       = @c_99991231
        AND a~otype       = @c_cs
        AND a~beg_process = @c_ra01
        AND b~istat       = @c_1
        AND b~pr_status   = @c_1
        AND b~prs_state   = @c_a.
*	End	  -->	MgM DCEK903347

    IF sy-subrc = 0.

*	Begin	-->	MgM DCEK903347 Filtro matriculados nuevos y antiguos 08/03/2017
*      sort lt_estudiantes_mat_aux
*      by objid.
*
*      loop at p_lt_estudiantes_mat into ls_estudiantes_mat.
*        read table lt_estudiantes_mat_aux into ls_estudiantes_mat_aux
*        with key objid = ls_estudiantes_mat-objid.
*        if sy-subrc ne 0.
*          delete p_lt_estudiantes_mat.
*        endif.
*      endloop.

      "Determino nuevos/antiguos. Si hay más de un registro es antiguo
      LOOP AT lt_estud_1771
*	Begin	-->	MgM  DCEK904012 Nuevos *** 18/04/2017
*        into data(ls_est_1771).
        ASSIGNING FIELD-SYMBOL(<lfs_est_1771>).
*	End	  -->	MgM  DCEK904012
        AT NEW objid.
          CLEAR lv_cont.
        ENDAT.
        ADD 1 TO lv_cont.
        AT END OF objid.
          IF lv_cont > 1.
*	Begin	-->	MgM  DCEK904012 Nuevos *** 18/04/2017
*            append ls_est_1771-objid to lt_antiguos.
            APPEND <lfs_est_1771>-objid TO lt_antiguos.
*	End	  -->	MgM  DCEK904012
          ELSE.
*	Begin	-->	MgM DCEK903794 filtro nuevos por periodo 08/03/2017
*	Begin	-->	MgM  DCEK904012 Nuevos *** 18/04/2017
*            if ls_est_1771-ayear eq p_peryr and
*               ls_est_1771-perid eq p_perid.
            IF <lfs_est_1771>-ayear EQ p_peryr AND
               <lfs_est_1771>-perid EQ p_perid.
*	End	  -->	MgM  DCEK904012
*	End	  -->	MgM DCEK903794
*	Begin	-->	MgM  DCEK904012 Nuevos *** 18/04/2017
*              append ls_est_1771-objid to lt_nuevos.
              APPEND <lfs_est_1771>-objid TO lt_nuevos.
*	End	  -->	MgM  DCEK904012
            ENDIF.
          ENDIF.
        ENDAT.
      ENDLOOP.

      CLEAR p_lt_estudiantes_mat[].

      IF p_matr_a EQ cl_bp_const=>true.
        LOOP AT lt_antiguos
          INTO DATA(ls_antiguo).

          READ TABLE p_lt_estudiantes
            INTO DATA(ls_estudiante)
              WITH KEY objid = ls_antiguo.
          MOVE 'Matriculados Antiguos' TO ls_estudiante-EstadoEstParametro.
          IF sy-subrc EQ 0.
            APPEND ls_estudiante
              TO p_lt_estudiantes_mat.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF p_matr_n EQ cl_bp_const=>true.
        LOOP AT lt_nuevos
          INTO DATA(ls_nuevo).

          READ TABLE p_lt_estudiantes
            INTO ls_estudiante
              WITH KEY objid = ls_nuevo.
          MOVE 'Matriculados Nuevos' TO ls_estudiante-EstadoEstParametro.
          IF sy-subrc EQ 0.
            APPEND ls_estudiante
              TO p_lt_estudiantes_mat.
          ENDIF.
        ENDLOOP.
      ENDIF.
*	End	  -->	MgM DCEK903347
*  Begin --> MgM  DCEK904012 18/04/2017
    ELSE.

      CLEAR p_lt_estudiantes_mat[].
*  End   --> MgM  DCEK904012

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PROCEDENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_HRP9003  text
*----------------------------------------------------------------------*
FORM f_obtener_procedencia  USING p_lt_estudiantes TYPE tyt_objid
                            CHANGING p_lt_hrp9003  TYPE tyt_hrp9003
*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017
                                     pc_t_colegios TYPE ty_t_colegios
                                     pc_t_univers TYPE ty_t_universidad.
*	End	  -->	MgM DCEK903347

  DATA: lt_estudiantes_aux TYPE tyt_objid_aux.

  PERFORM f_completar_auxiliar
  USING p_lt_estudiantes
  CHANGING lt_estudiantes_aux.

  CHECK lt_estudiantes_aux IS NOT INITIAL.

  SELECT objid
         colegio
         otro_col
         titulo       "-->  MgM DCEK904012
         universidad
         otra_uni
         titulo_u
         saber_11         "MgM DCEK903347
         begda
         endda
         ano
    FROM hrp9003 AS h
    INTO TABLE p_lt_hrp9003
    FOR ALL ENTRIES IN lt_estudiantes_aux
    WHERE objid = lt_estudiantes_aux-sobid
      AND plvar = c_01
      AND otype = c_st
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*      AND colegio NE space.
      AND ( colegio NE space OR otro_col NE space ).
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

  IF sy-subrc EQ 0.

    SELECT  codigo
            nombre_colegio
      FROM zedu_coleg
        INTO TABLE pc_t_colegios
          FOR ALL ENTRIES IN p_lt_hrp9003
            WHERE codigo EQ p_lt_hrp9003-colegio(12).

    IF sy-subrc EQ 0.
      SORT pc_t_colegios BY codigo.
    ENDIF.

    SELECT  codigo
            universidad
      FROM zedu_univ
        INTO TABLE pc_t_univers
          FOR ALL ENTRIES IN p_lt_hrp9003
            WHERE codigo EQ p_lt_hrp9003-colegio(12).

    IF sy-subrc EQ 0.
      SORT pc_t_univers BY codigo.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PROGRAMAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ESTUDIANTES  text
*      <--P_LT_PROGRAMAS  text
*----------------------------------------------------------------------*
FORM f_obtener_programas  USING p_lt_estudiantes TYPE tyt_objid
*                                p_lt_partner_id  type tyt_partner_id  ""-->  MgM DCEK904012 Procedencia 03/04/2017
                          CHANGING p_lt_programas    TYPE tyt_programas
                                   p_lt_procesos_ant TYPE tyt_procesos_ant
*                                   p_lt_solic_prog   TYPE tyt_procesos_ant "--> Borrado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
                                   pc_t_partner_id   TYPE tyt_partner_id. "-->  MgM DCEK904012 Procedencia 03/04/2017.

  DATA: ls_procesos_ant  TYPE tys_procesos_ant,
        ls_procesos_temp TYPE tys_procesos_ant,
        lv_cant_insc     TYPE i.

* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
  FIELD-SYMBOLS:
    <fs_procesos_ant>  TYPE tys_procesos_ant.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos

  CHECK p_lt_estudiantes IS NOT INITIAL.

  SELECT choice_no
         spras      "-->  MgM DCEK903347
         choice_not
    FROM t7piqchoicest
    INTO TABLE p_lt_programas
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE choice_no = p_lt_estudiantes-choice_no
      AND spras = sy-langu.

  CHECK pc_t_partner_id IS NOT INITIAL.

  SELECT a~tipo_documen
         a~nro_documen
         a~nr_formulario
*         b~sta1_solpago
         b~sta2_solpago  "-->  MgM DCEK903347
*	Begin	-->	MgM DCEK904012 Procedencia 03/04/2017
         a~pais_prcdnc
         a~depto_prcdnc
         c~ciudad_proc
         b~programa_1
         d~stext
         b~programa_2
         e~stext
         a~EPS
*	End	  -->	MgM DCEK904012
"Alter info: Agregar campos adicionales
"Autor: Sebastián Espinosa Marin
"Fecha: 12/07/2019
        TXT_FAC~STEXT "Código Facultad
        TXT_TIPOPROG~STEXT "Código Tipo de Programa
        b~MEDIO
        PIN~Pin
"Fin alter
    FROM zpre_admi_1 AS a
      INNER JOIN zpre_admi_2 AS b
        ON a~nr_formulario = b~nr_formulario
    INNER JOIN hrp1000 AS TXT_FAC
      ON b~FACULTAD = TXT_FAC~objid
      AND TXT_FAC~otype = c_o
    INNER JOIN hrp1000 AS TXT_TIPOPROG
      ON b~TIPO_PROGRAM = TXT_TIPOPROG~objid
      AND TXT_TIPOPROG~otype = c_o
*	Begin	-->	MgM DCEK904012 Procedencia 03/04/2017
      LEFT OUTER JOIN hrp1000 AS d
        ON b~programa_1 EQ d~objid AND
           d~otype      EQ cl_hrpiq00const=>c_otype_sc
      LEFT OUTER JOIN hrp1000 AS e
        ON b~programa_2 EQ e~objid AND
           e~otype      EQ cl_hrpiq00const=>c_otype_sc
      LEFT OUTER JOIN zpre_admi_3 AS c
        ON a~nr_formulario EQ c~nr_formulario
      LEFT OUTER JOIN ZSLCM_PIN AS PIN
        ON a~nr_formulario EQ PIN~NUM_FORM
*	End	  -->	MgM DCEK904012
    INTO TABLE p_lt_procesos_ant
      FOR ALL ENTRIES IN pc_t_partner_id
        WHERE a~nro_documen  = pc_t_partner_id-idnumber(20).

  IF sy-subrc = 0.

*	Begin	-->	MgM DCEK904012 Procedencia 03/04/2017
    DELETE p_lt_procesos_ant
      WHERE programa_1 NOT IN zedu_cl_perfiles_struct=>get_range(
                                cl_hriq_course_constants=>c_otype_sc ) OR
            programa_2 NOT IN zedu_cl_perfiles_struct=>get_range(
                          cl_hriq_course_constants=>c_otype_sc ).

    SORT pc_t_partner_id
      BY idnumber.

    "Recuperamos texto de paises
    SELECT  spras,
            land1,
            landx
      FROM t005t
        INTO TABLE @DATA(lt_paises)
          FOR ALL ENTRIES IN @p_lt_procesos_ant
            WHERE land1 EQ @p_lt_procesos_ant-pais_prcdnc
              AND spras EQ @sy-langu
              ORDER BY PRIMARY KEY.

    IF sy-subrc NE 0.
      CLEAR lt_paises[].
    ENDIF.

    "Recuperamos texto de deptartamentos
    SELECT  spras,
            land1,
            bland,
            bezei
      FROM t005u
        INTO TABLE @DATA(lt_deptos)
          FOR ALL ENTRIES IN @p_lt_procesos_ant
            WHERE spras EQ @sy-langu
              AND land1 EQ @p_lt_procesos_ant-pais_prcdnc
              AND bland EQ @p_lt_procesos_ant-depto_prcdnc.

    IF sy-subrc NE 0.
      CLEAR lt_deptos[].
    ENDIF.

    "Recuperamos texto de ciudad
    SELECT  spras,
            land1,
            regio,
            cityc,
            bezei
      FROM t005h
        INTO TABLE @DATA(lt_ciudades)
          FOR ALL ENTRIES IN @p_lt_procesos_ant
            WHERE spras EQ @sy-langu
              AND land1 EQ @p_lt_procesos_ant-pais_prcdnc
              AND regio EQ @p_lt_procesos_ant-depto_prcdnc
              AND cityc EQ @p_lt_procesos_ant-ciudad_proc.

    IF sy-subrc NE 0.
      CLEAR lt_ciudades[].
    ENDIF.
*	End	  -->	MgM DCEK904012

* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*    p_lt_solic_prog = p_lt_procesos_ant.
*
*    CLEAR lv_cant_insc.
*
*    LOOP AT p_lt_procesos_ant INTO ls_procesos_ant.
*      ls_procesos_temp  = ls_procesos_ant.
*      lv_cant_insc = lv_cant_insc + 1.
*
*      AT END OF nro_documen.
*        ls_procesos_ant-cant_inscr = lv_cant_insc.
*        CLEAR lv_cant_insc.
*
*        MODIFY p_lt_procesos_ant
*        FROM ls_procesos_ant
*        TRANSPORTING cant_inscr.
*
**  Begin --> MgM DCEK904012 Procedencia 03/04/2017
*        READ TABLE pc_t_partner_id
*          ASSIGNING FIELD-SYMBOL(<fs_partner>)
*            WITH KEY idnumber = ls_procesos_temp-nro_documen
**                     type     = ls_procesos_ant-tipo_documen
*              BINARY SEARCH.
*
*        IF sy-subrc EQ 0.
*
*          MOVE ls_procesos_temp-primera_opcion
*            TO <fs_partner>-primera_opcion.
*
*          MOVE ls_procesos_temp-segunda_opcion
*            TO <fs_partner>-segunda_opcion.
*
*          READ TABLE lt_paises
*            INTO DATA(ls_pais)
*              WITH KEY spras  = sy-langu
*                       land1  = ls_procesos_temp-pais_prcdnc.
*          IF sy-subrc EQ 0.
*            <fs_partner>-pais_prcdnc =  ls_pais-landx.
*          ENDIF.
*
*          READ TABLE lt_deptos
*            INTO DATA(ls_depto)
*              WITH KEY  spras = sy-langu
*                        land1 = ls_procesos_temp-pais_prcdnc
*                        bland = ls_procesos_temp-depto_prcdnc.
*          IF sy-subrc EQ 0.
*            <fs_partner>-depto_prcdnc = ls_depto-bezei.
*          ENDIF.
*
*          READ TABLE lt_ciudades
*            INTO DATA(ls_ciudad)
*              WITH KEY  spras = sy-langu
*                        land1 = ls_procesos_temp-pais_prcdnc
*                        regio = ls_procesos_temp-depto_prcdnc
*                        cityc = ls_procesos_temp-ciudad_proc.
*          IF sy-subrc EQ 0.
*            <fs_partner>-ciudad_proc  = ls_ciudad-bezei.
*          ENDIF.
*
*        ENDIF.
**  End   --> MgM DCEK904012
*
*      ENDAT.
*
*    ENDLOOP.
*
*    DELETE p_lt_procesos_ant
*    WHERE cant_inscr = 0.

    "Ordena los registros
    SORT p_lt_procesos_ant BY
      nro_documen ASCENDING
      nr_formulario DESCENDING
      programa_1 ASCENDING
      programa_2 ASCENDING.

    "Recorre los registros encontrados y asigna los nombres de la procedencia
    LOOP AT p_lt_procesos_ant ASSIGNING <fs_procesos_ant>.
      "Crea una copia del regisrto
      ls_procesos_temp  = <fs_procesos_ant>.

      "En el primer registro del documento
      AT NEW nro_documen.
        "Inicializa la cantidad de registros
        CLEAR lv_cant_insc.
      ENDAT.

      "Adiciona uno a la cantidad de inscripciones
      ADD 1 TO lv_cant_insc.

      "Obtiene la descripcion del pais
      READ TABLE lt_paises
        INTO DATA(ls_pais)
          WITH KEY spras  = sy-langu
                   land1  = <fs_procesos_ant>-pais_prcdnc.
      IF sy-subrc EQ 0.
        <fs_procesos_ant>-pais_prcdnc_tx =  ls_pais-landx.
      ENDIF.

      READ TABLE lt_deptos
        INTO DATA(ls_depto)
          WITH KEY  spras = sy-langu
                    land1 = <fs_procesos_ant>-pais_prcdnc
                    bland = <fs_procesos_ant>-depto_prcdnc.
      IF sy-subrc EQ 0.
        <fs_procesos_ant>-depto_prcdnc_tx = ls_depto-bezei.
      ENDIF.

      READ TABLE lt_ciudades
        INTO DATA(ls_ciudad)
          WITH KEY  spras = sy-langu
                    land1 = <fs_procesos_ant>-pais_prcdnc
                    regio = <fs_procesos_ant>-depto_prcdnc
                    cityc = <fs_procesos_ant>-ciudad_proc.
      IF sy-subrc EQ 0.
        <fs_procesos_ant>-ciudad_proc_tx  = ls_ciudad-bezei.
      ENDIF.

      "En el ultimo registro del documento
      AT END OF nro_documen.
        "Obtiene el registro del interlocutor
        READ TABLE pc_t_partner_id
          ASSIGNING FIELD-SYMBOL(<fs_partner>)
          WITH KEY idnumber = ls_procesos_temp-nro_documen
          BINARY SEARCH.

        IF sy-subrc EQ 0.
          "Asigna la cantidad de inscripciones que tiene el interlocutor
          <fs_partner>-cant_inscr = lv_cant_insc.
        ENDIF.
      ENDAT.
    ENDLOOP.
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_NIVELES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIANTES  text
*      <--P_LT_NIVELES  text
*----------------------------------------------------------------------*
FORM f_obtener_niveles  USING p_lt_estudiantes TYPE tyt_objid
                        CHANGING p_lt_niveles TYPE tyt_niveles.

  SELECT aclevel
         aclevelt
    FROM t7piqlevelt
    INTO TABLE p_lt_niveles
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE aclevel = p_lt_estudiantes-adm_aclevel
      AND langu   = sy-langu.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CATEGORIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIANTES  text
*      <--P_LT_CATEGORIAS  text
*----------------------------------------------------------------------*
FORM f_obtener_categorias  USING p_lt_estudiantes TYPE tyt_objid
                           CHANGING p_lt_categorias TYPE tyt_categorias.

  SELECT enrcateg
         enrcategt
    FROM t7piqenrcategt
    INTO TABLE p_lt_categorias
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE enrcateg = p_lt_estudiantes-adm_enrcateg
      AND spras    = sy-langu.

ENDFORM.

FORM f_traer_direccion_correo USING p_pers_addr_num TYPE tys_pers_addr_num
                                 CHANGING p_output TYPE zedu_s_directorio_inscritos.
  DATA:
        ld_addressdata  TYPE BAPIBUS1006_ADDRESS,
        it_bapiadsmtp	TYPE STANDARD TABLE OF BAPIADSMTP,
        wa_bapiadsmtp	LIKE LINE OF it_bapiadsmtp.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
    EXPORTING
      BUSINESSPARTNER            = p_pers_addr_num-partner
    IMPORTING
      addressdata                = ld_addressdata
    TABLES
      bapiadsmtp                 = it_bapiadsmtp
    EXCEPTIONS
      parameter_error            = 1
      address_not_exist          = 2
      person_not_exist           = 3
      version_not_exist          = 4
      internal_error             = 5
      person_blocked             = 6
      OTHERS                     = 7.
  IF sy-subrc = 0.
    READ TABLE it_bapiadsmtp INTO wa_bapiadsmtp WITH KEY STD_NO = 'X'.
    IF sy-subrc = 0.
      p_output-SMTP_ADDR = wa_bapiadsmtp-E_MAIL.
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_DIRECCION_TELEFONO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PERS_ADDR_NUM  text
*      <--P_LS_OUTPUT  text
*----------------------------------------------------------------------*
FORM f_armar_direccion_telefono  USING p_pers_addr_num TYPE tys_pers_addr_num
                                 CHANGING p_output TYPE zedu_s_directorio_inscritos.

  CONSTANTS: c_usage_nmb_default TYPE ad_cusage VALUE 'AD_NMBDEFA',
             c_usage_mb_default  TYPE ad_cusage VALUE 'AD_MBDEFAU'.

  DATA: lt_usage                      TYPE crmt_adsuse_tab,
        lt_adtel                      TYPE crmt_admob_tab,
        lt_niveles                    TYPE tyt_niveles,
        lt_categorias                 TYPE tyt_categorias,
        lv_table_type                 TYPE szad_field-table_type,
*	Begin	-->	MgM DCEK903347 teléfono y movil 02/03/2017
*        ls_usage                      type aduse,
        ls_usage                      TYPE adsuse,
*	End	  -->	MgM DCEK903347
        ls_adtel                      TYPE adtel,
        ls_address_personal_selection TYPE addr2_sel,
        ls_address_personal_value     TYPE addr2_val,
        ls_address_personal_info      TYPE ad2_flags.
  DATA    ls_address_personal_text      TYPE addr2_text.  "-->  MgM DCEK904012

  MOVE: p_pers_addr_num-persnumber TO
        ls_address_personal_selection-persnumber,
        p_pers_addr_num-addrnumber TO
        ls_address_personal_selection-addrnumber,
        c_default_date_from TO
        ls_address_personal_selection-date.

  CALL FUNCTION 'ADDR_PERSONAL_GET'
    EXPORTING
      address_personal_selection = ls_address_personal_selection
      read_texts                 = cl_bp_const=>true  "-->  MgM DCEK904012
    IMPORTING
      address_personal_value     = ls_address_personal_value
      address_personal_info      = ls_address_personal_info
      address_personal_text      = ls_address_personal_text
    EXCEPTIONS
      parameter_error            = 1
      address_not_exist          = 2
      person_not_exist           = 3
      version_not_exist          = 4
      internal_error             = 5
      person_blocked             = 6
      OTHERS                     = 7.
  IF sy-subrc = 0.

*	Begin	-->	MgM DCEK904012 país y región en texto 03/04/2017
*    move: ls_address_personal_value-region  to
*          p_output-region,
*          ls_address_personal_value-city1   to
*          p_output-city1,
*          ls_address_personal_value-country to
*          p_output-country,
*          ls_address_personal_value-street  to
*          p_output-street.
    MOVE ls_address_personal_text-bezei   TO  p_output-region.
    MOVE ls_address_personal_text-landx   TO  p_output-country.
    MOVE ls_address_personal_value-city1  TO  p_output-city1.
    MOVE ls_address_personal_value-street TO  p_output-street.
*	End	  -->	MgM DCEK904012

    lv_table_type = 'ADTEL'.

    CALL FUNCTION 'ADDR_PERSONAL_COMM_GET'
      EXPORTING
        address_number    = p_pers_addr_num-addrnumber
        person_number     = p_pers_addr_num-persnumber
        table_type        = lv_table_type
      TABLES
        comm_table        = lt_adtel
        et_usage          = lt_usage
      EXCEPTIONS
        parameter_error   = 1
        address_not_exist = 2
        person_not_exist  = 3
        internal_error    = 4
        person_blocked    = 5
        OTHERS            = 6.
    IF sy-subrc = 0.

      READ TABLE lt_usage INTO ls_usage
       WITH KEY comm_usage = c_usage_nmb_default.
      IF sy-subrc = 0.
        READ TABLE lt_adtel INTO ls_adtel
             WITH KEY consnumber = ls_usage-consnumber.
        IF sy-subrc = 0.
          p_output-tel_number = ls_adtel-tel_number.
        ENDIF.
      ENDIF.

      READ TABLE lt_usage INTO ls_usage
       WITH KEY comm_usage = c_usage_mb_default.
      IF sy-subrc = 0.
        READ TABLE lt_adtel INTO ls_adtel
             WITH KEY consnumber = ls_usage-consnumber.
        IF sy-subrc = 0.
          p_output-mob_number = ls_adtel-tel_number.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LLENAR_AÑO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_anio .

  DATA: lt_value_tab TYPE wdr_context_attr_value_list.

  IF sy-ucomm = 'TIPPRO'.

    CREATE OBJECT wd_assist.

    lt_value_tab =  wd_assist->get_ddbk_anio_periodo(
                    EXPORTING iv_programa = p_tippro ).

    PERFORM f_cargar_listbox
    USING lt_value_tab
          c_p_peryr.

  ELSEIF sy-ucomm = 'FACUL'.
    CLEAR: p_tippro.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LLENAR_PERIODOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_periodos .

  DATA: lt_value_tab TYPE wdr_context_attr_value_list.

  CHECK sy-ucomm = 'ANIO'.


  CLEAR: p_perid.

  CREATE OBJECT wd_assist.

  lt_value_tab =  wd_assist->get_ddbk_anio_periodo(
                  EXPORTING iv_programa = p_tippro ).

  PERFORM f_cargar_listbox
  USING lt_value_tab
        c_p_perid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LLENAR_FACULTAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_facultad.

  DATA: lt_value_tab TYPE wdr_context_attr_value_list.

  IF gv_primer IS INITIAL.
    CLEAR: p_facul,
           p_tippro,
           p_planes,
           p_peryr,
           p_perid,
           s_nromat[],
           s_nroid[].
    gv_primer = c_x.
  ENDIF.

  CREATE OBJECT wd_assist.

  lt_value_tab =  wd_assist->get_dropdown_key(
                  EXPORTING im_field = 'FACULTADES'
                  im_null_value = abap_false ).

  PERFORM f_cargar_listbox
  USING lt_value_tab
        c_p_facul.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LLENAR_TIPOS_PROGRAMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_tipos_programa .

  DATA: lt_value_tab TYPE wdr_context_attr_value_list,
        lv_facultad  TYPE string.

  CHECK sy-ucomm = 'FACUL'.
  CLEAR: p_tippro,
         p_peryr,
         p_perid,
         p_planes.

  CREATE OBJECT wd_assist.

  lv_facultad = p_facul.

  lt_value_tab =  wd_assist->get_dropdown_key(
                  EXPORTING im_field = 'TIPO_PROGRAMA'
                            im_value = lv_facultad ).

  PERFORM f_cargar_listbox
  USING lt_value_tab
        c_p_tippro.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VALUE_TAB  text
*      -->P_LV_ID  text
*----------------------------------------------------------------------*
FORM f_cargar_listbox  USING p_lt_value_tab TYPE wdr_context_attr_value_list
                             p_id           TYPE c.

  DATA: lt_vrm_values TYPE vrm_values,
        ls_vrm_values TYPE vrm_value,
        ls_value_tab  TYPE wdr_context_attr_value,
        lv_id         TYPE vrm_id.

  IF p_id = c_p_peryr.

    LOOP AT p_lt_value_tab INTO ls_value_tab.
      ls_vrm_values-key  = ls_value_tab-value.
*      ls_vrm_values-text = ls_value_tab-text.
      ls_vrm_values-key = ls_vrm_values-key(4).
      APPEND ls_vrm_values TO lt_vrm_values.
      CLEAR ls_vrm_values.
    ENDLOOP.

  ELSEIF p_id = c_p_perid.

    LOOP AT p_lt_value_tab INTO ls_value_tab.
      ls_vrm_values-key  = ls_value_tab-value.
      ls_vrm_values-text = ls_value_tab-text.
      IF ls_vrm_values-key(4) = p_peryr.
        ls_vrm_values-key  = ls_vrm_values-key+4(3).
        ls_vrm_values-text = ls_vrm_values-text+5.
        APPEND ls_vrm_values TO lt_vrm_values.
      ENDIF.
      CLEAR ls_vrm_values.
    ENDLOOP.

  ELSE.

    LOOP AT p_lt_value_tab INTO ls_value_tab.
      ls_vrm_values-key  = ls_value_tab-value.
      ls_vrm_values-text = ls_value_tab-text.
      APPEND ls_vrm_values TO lt_vrm_values.
      CLEAR ls_vrm_values.
    ENDLOOP.

  ENDIF.

  SORT lt_vrm_values
  BY key.

  DELETE ADJACENT DUPLICATES FROM lt_vrm_values
  COMPARING key.

  lv_id = p_id.

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
*&      Form  F_LLENAR_PLANES_ESTUDIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_llenar_planes_estudio .

  DATA: lt_value_tab TYPE wdr_context_attr_value_list,
        lv_tipo_prog TYPE string.

  CHECK sy-ucomm = 'TIPPRO' OR
        sy-ucomm = `PERIOD`.  "-->  MgM DCEK903347

  CREATE OBJECT wd_assist.

*	Begin	-->	MgM DCEK903347 Ajustes varios 07/02/2017
*  lv_tipo_prog = p_tippro.
*
*  lt_value_tab =  wd_assist->get_dropdown_key(
*                  exporting im_field = 'PLANES_ESTUDIO'
*                            im_value = lv_tipo_prog ).

  PERFORM f_get_planes  USING     p_tippro
                                  p_perid
                                  p_peryr
                        CHANGING  lt_value_tab.
*	End	  -->	MgM DCEK903347

  PERFORM f_cargar_listbox
  USING lt_value_tab
        c_p_planes.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VACIAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_vaciar_campos .

  CASE sy-ucomm.
    WHEN 'FACUL'.
      CLEAR: p_tippro,
             p_peryr,
             p_perid,
             p_planes.
    WHEN 'TIPPRO'.
      CLEAR: p_peryr,
             p_perid,
             p_planes.
    WHEN 'ANIO'.
      CLEAR: p_perid.
    WHEN 'ONLI'.
*      if p_facul  is initial
*      or p_tippro is initial
      IF p_peryr  IS INITIAL
      OR p_perid  IS INITIAL.
*	Begin	-->	MgM DCEK903347 Ajustes varios 07/02/2017
*      OR p_planes IS INITIAL.
*	End	  -->	MgM DCEK903347
        MESSAGE text-005
        TYPE c_e.
      ENDIF.

*	Begin	-->	MgM DCEK903347 Ajustes Varios 06/03/2017
      IF p_inscr  IS INITIAL AND
         p_admit  IS INITIAL AND
         p_matr_a IS INITIAL AND
         p_matr_n IS INITIAL
*	Begin	--> FROSENZVAIG - DCEK906683 - 10/10/2017
         AND p_rech IS INITIAL.
*	End	--> FROSENZVAIG - DCEK906683 - 10/10/2017
        "Debe seleccionar al menos 1 Parámetro general
        MESSAGE text-020
          TYPE c_e.
      ENDIF.
*	End	  -->	MgM DCEK903347

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_TIPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_ESTUDIANTES  text
*      <--P_LT_TIPOS  text
*----------------------------------------------------------------------*
FORM f_obtener_tipo  USING p_lt_estudiantes TYPE tyt_objid
                     CHANGING p_lt_tipos TYPE tyt_tipos.

  CHECK p_lt_estudiantes IS NOT INITIAL.

  SELECT adm_categ
         adm_categt
    FROM t7piqadmcategt
    INTO TABLE p_lt_tipos
    FOR ALL ENTRIES IN p_lt_estudiantes
    WHERE adm_categ = p_lt_estudiantes-adm_categ
      AND spras     = sy-langu.

  IF sy-subrc = 0.
    SORT p_lt_tipos BY adm_categ.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_RANGO_PLANESTUDIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PLANES  text
*      <--P_GR_PLANES  text
*----------------------------------------------------------------------*
FORM f_obtener_rango_planestudio  USING     uv_tippro
                                            uv_planes
                                            uv_perid
                                            uv_peryr
                                  CHANGING  cr_planes TYPE ty_ra_planes.

  DATA lt_value_tab TYPE wdr_context_attr_value_list.

  IF uv_planes IS INITIAL.
    PERFORM f_get_planes  USING     uv_tippro
                                    uv_perid
                                    uv_peryr
                          CHANGING  lt_value_tab.
*	Begin	-->	MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
*  else.
  ELSEIF uv_planes IN zedu_cl_perfiles_struct=>get_range(
                          cl_hriq_course_constants=>c_otype_sc ).
*	End	  -->	MgM DCEK904012
    APPEND INITIAL LINE TO lt_value_tab ASSIGNING FIELD-SYMBOL(<fs_value>).
    <fs_value>-value  = uv_planes.
  ENDIF.

  LOOP AT lt_value_tab
    INTO DATA(ls_value).
    APPEND INITIAL LINE TO cr_planes ASSIGNING FIELD-SYMBOL(<fs_plan>).
    <fs_plan>-option  = `EQ`.
    <fs_plan>-sign    = `I`.
    <fs_plan>-low     = ls_value-value.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_PLANES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TIPPRO  text
*      -->P_P_PERID  text
*      -->P_P_PERYR  text
*      <--P_LT_VALUE_TAB  text
*----------------------------------------------------------------------*
FORM f_get_planes  USING    uv_tippro
                            uv_perid
                            uv_peryr
                   CHANGING ct_value_tab TYPE wdr_context_attr_value_list.

  DATA lv_tipo_prog TYPE string.

  lv_tipo_prog = uv_tippro.

  LOOP AT wd_assist->get_dropdown_key( im_field  = wd_assist->c_preposgrado
                                       im_value  = lv_tipo_prog )
    INTO DATA(ls_pre_pos)
*	Begin	-->	MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
      WHERE value IN zedu_cl_perfiles_struct=>get_range( cl_hr_in_pe_agents=>c_org_unit ).
*	End	  -->	MgM DCEK904012

    APPEND LINES OF wd_assist->get_dropdown_key(
                      EXPORTING im_field = wd_assist->c_field_plan_est
                                im_value = ls_pre_pos-value ) TO ct_value_tab.

  ENDLOOP.

*	Begin	-->	MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
  DELETE ct_value_tab
    WHERE value NOT IN  zedu_cl_perfiles_struct=>get_range(
                          cl_hriq_course_constants=>c_otype_sc ).
*	End	  -->	MgM DCEK904012

****INICIO HIRS 03/10/2017 "Solucion Issues directorio de Inscritos
***  IF uv_perid IS NOT INITIAL AND
***     uv_peryr IS NOT INITIAL AND
***     ct_value_tab[] IS NOT INITIAL.
***
***    DATA lt_planes TYPE STANDARD TABLE OF hrobjid.
***
***    LOOP AT ct_value_tab
***      INTO ls_pre_pos.
***      APPEND INITIAL LINE TO lt_planes ASSIGNING FIELD-SYMBOL(<fs_plan>).
***      <fs_plan> = ls_pre_pos-value.
***    ENDLOOP.
***
***    "Restrinjo por año y periodo
***    SELECT objid
***      FROM hrp1739
***        INTO TABLE @DATA(lt_plan_x_periodo)
***          FOR ALL ENTRIES IN @lt_planes
***            WHERE objid EQ @lt_planes-table_line
***              AND peryr EQ @uv_peryr
***              AND perid EQ @uv_perid.
***
***    IF sy-subrc EQ 0.
***      SORT lt_plan_x_periodo BY table_line.
***
***      LOOP AT ct_value_tab
***        INTO ls_pre_pos.
***
***        READ TABLE lt_plan_x_periodo
***          TRANSPORTING NO FIELDS
***            WITH KEY table_line = ls_pre_pos-value
***              BINARY SEARCH.
***
***        IF sy-subrc NE 0.
***          DELETE ct_value_tab.
***        ENDIF.
***      ENDLOOP.
***
***    ELSE.
***      CLEAR ct_value_tab[].
***    ENDIF.
***  ENDIF.
****FIN HIRS 03/10/2017 "Solucion Issues directorio de Inscritos

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CATALOG_CAMBIA_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_`GBDEP`  text
*      -->P_TEXT_006  text
*      <--P_P_LT_CATALOG  text
*----------------------------------------------------------------------*
FORM f_catalog_cambia_text  USING    pu_field
                                     pu_text
                            CHANGING pc_t_catalog TYPE slis_t_fieldcat_alv.

  READ TABLE pc_t_catalog
    ASSIGNING FIELD-SYMBOL(<fs_cata>)
      WITH KEY fieldname = pu_field.

  IF sy-subrc EQ 0.
    MOVE pu_text TO <fs_cata>-seltext_l.
    MOVE pu_text TO <fs_cata>-seltext_m.
    MOVE pu_text TO <fs_cata>-seltext_s.
    MOVE pu_text TO <fs_cata>-reptext_ddic.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILTRAR_1771
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LT_ESTUDIANTES_INS  text
*----------------------------------------------------------------------*
FORM f_filtrar_1771  CHANGING ct_estudiantes TYPE tyt_objid.

  IF ct_estudiantes IS NOT INITIAL.

    SELECT objid
      FROM hrp1771
      INTO TABLE @DATA(lt_estudiantes_1771)
        FOR ALL ENTRIES IN @ct_estudiantes
          WHERE objid = @ct_estudiantes-objid
            AND plvar = @c_01
            AND otype = @c_cs.

    IF sy-subrc = 0.
*  Begin --> MgM DCEK904012 Aplica perfiles estructurales 23/03/2017
      DELETE ct_estudiantes
        WHERE objid NOT IN  zedu_cl_perfiles_struct=>get_range(
                              cl_hrpiq00const=>c_otype_cs ).
*  End   --> MgM DCEK904012

      SORT lt_estudiantes_1771
        BY objid.

      LOOP AT ct_estudiantes
        INTO DATA(ls_estudiantes).

        READ TABLE lt_estudiantes_1771
          TRANSPORTING NO FIELDS
            WITH KEY objid = ls_estudiantes-objid.

        IF sy-subrc = 0.
          DELETE ct_estudiantes.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.
