class ZCL_REP_AULAS_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  final
  create public .

public section.

  methods BUSCAR_AULAS_DISPONIBLES
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA
      !IT_PROGRAMA type ZEDU_T_PROGRAMA_EVENTO
    exporting
      !ET_AULAS type ZEDU_T_AULA .
  methods MODIFICAR_PROGRAMA
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA
      !IT_PROGRAMA type ZEDU_T_PROGRAMA_EVENTO
    returning
      value(RT_ERRORES) type BAPIRETURN_T .
  methods MODIFICAR_EVENTO
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA
    returning
      value(RT_ERRORES) type BAPIRETURN_T .
  methods BUSCAR_EVENTOS
    importing
      !IV_PLAN_ESTUDIO type HRP1222-OBJID
      !IV_ASIGNATURA type HRP1222-OBJID
      !IV_ANIO type PIQACADOFFERSCREENFIELDS-PERYR
      !IV_PERIODO type PIQACADOFFERSCREENFIELDS-PERID
      !IV_EVENTO type HROBJID
    exporting
      !ET_EVENTOS type ZEDU_T_EVENTOS_AULA .
  methods GET_ROOM_NAME
    importing
      !IV_ROOM_OBJID type HROBJID
    exporting
      !EV_ROOM_NAME type STEXT
    returning
      value(RT_ERROR) type BAPIRETURN_T .
  methods BUSCAR_PROGRAMA_EVENTO
    importing
      !IV_EVENTO type HROBJID
      !IS_HORA_EVENTO type PIQEVENTDATES optional
    exporting
      !ET_PROGRAMA_EVENTO type ZEDU_T_PROGRAMA_EVENTO .
protected section.
private section.

  types:
  BEGIN OF ty_resources,
    plvar TYPE hrp1000-plvar,
    otype TYPE hrp1000-otype,
    objid type hrp1000-objid,
  END OF ty_resources.
ENDCLASS.



CLASS ZCL_REP_AULAS_ASS IMPLEMENTATION.


  METHOD buscar_aulas_disponibles.
    " Se utilizan las funcionas estandar utilizadas por la transacción PIQACADOFFER00
    " en la ayuda de búsqueda dinámica

    DATA:
      lv_ortid         TYPE plog-objid,
      lt_res_tab       TYPE TABLE OF hrobject,
      lt_1023          TYPE TABLE OF hrresty,
      lt_r_g           TYPE TABLE OF hrrestytab,
      ls_r_g           TYPE hrrestytab,
      lt_schedule      TYPE piq_pt1035_t,
      lt_resources     TYPE piq_hrsobid_t,
      ls_resources     TYPE hrsobid,
      lt_resources_aux TYPE TABLE OF ty_resources,
      ls_resources_aux TYPE ty_resources,
      lt_occupy        TYPE piq_hrsobid_t,
      lt_hrp1001       TYPE TABLE OF hrp1001,
      ls_hrp1001       TYPE hrp1001,
      lv_eventtype     TYPE plog-objid,
      lt_needed_resty  TYPE TABLE OF hrevtyneed,
      ls_programa      TYPE zedu_programa_evento,
      ls_schedule      TYPE pt1035.


    " Arma tabla LT_SCHEDULE a partir de programa de evento
    LOOP AT it_programa INTO ls_programa.
      ls_schedule-evdat = ls_programa-begda.
      ls_schedule-beguz = ls_programa-beguz.
      ls_schedule-enduz = ls_programa-enduz.
      APPEND ls_schedule TO lt_schedule.
    ENDLOOP.

    " Selecciona Tipo de evento y Locación de evento a partir de las relaciones
    SELECT *
      FROM hrp1001
      INTO TABLE lt_hrp1001
      WHERE otype EQ is_evento_aula-otype AND
            objid EQ is_evento_aula-objid AND
            plvar EQ is_evento_aula-plvar AND
*            begda LE sy-datum             AND
*            endda GE sy-datum             AND
            rsign EQ 'A'                  AND
          ( relat EQ '020'  OR  " Tipo de evento
            relat EQ '024' ).   " Locación
    IF sy-subrc IS INITIAL.
      READ TABLE lt_hrp1001 INTO ls_hrp1001
        WITH KEY relat = '020'.
      IF sy-subrc IS INITIAL.
        " Tipo de evento
        lv_eventtype = ls_hrp1001-sobid.
      ENDIF.

      READ TABLE lt_hrp1001 INTO ls_hrp1001
        WITH KEY relat = '024'.
      IF sy-subrc IS INITIAL.
        " Locación
        lv_ortid = ls_hrp1001-sobid.
      ENDIF.

    ENDIF.

    " Recupera tipos de recursos según evento
    CALL FUNCTION 'HRIQ_RESTYS_FOR_EVENTTYPE_READ'
      EXPORTING
        plvar           = is_evento_aula-plvar
        eventtype       = lv_eventtype
*       ISTAT           = '1'
        begda           = is_evento_aula-begda
        endda           = is_evento_aula-endda
*       CHECK_STRU_AUTH = ' '
      TABLES
        r_tab           = lt_res_tab
        needed_resty    = lt_needed_resty
      EXCEPTIONS
        no_resty_needed = 1
        OTHERS          = 2.
    CHECK sy-subrc IS INITIAL.

    " Recupera eventos según tipos de recursos
    CALL FUNCTION 'HRIQ_RESOURCES_FOR_RESTYS_READ'
      EXPORTING
        plvar           = is_evento_aula-plvar
        otype           = is_evento_aula-otype
        objid           = is_evento_aula-objid
        istat           = '1' " 1 - Activo. 2 - Planificado (no trae recursos)
        begda           = is_evento_aula-begda
        endda           = is_evento_aula-endda
        evtyp_id        = lv_eventtype
        ortid           = lv_ortid
        kapaz           = 'X'
        check_stru_auth = 'X'
      TABLES
        r_tab           = lt_res_tab
        resourcetypes   = lt_1023
*       R1023           =
*       R1024           =
        r_g_tab         = lt_r_g
*       G1024           =
      EXCEPTIONS
        r_tab_empty     = 1
        OTHERS          = 2.
    CHECK sy-subrc IS INITIAL.

    " Elimino recursos que no son aulas
    DELETE lt_r_g WHERE otype NE 'G'.

    LOOP AT lt_r_g INTO ls_r_g.
      ls_resources-plvar = ls_r_g-plvar.
      ls_resources-otype = ls_r_g-otype.
      ls_resources-sobid = ls_r_g-objid.
      APPEND ls_resources TO lt_resources.
    ENDLOOP.

    " Elimina recursos que no estén libres
    PERFORM check_resource_free IN PROGRAM saplhrpiq00acadoffer_tools
     USING    is_evento_aula-plvar
              lv_eventtype
              is_evento_aula-objid
              lt_schedule
     CHANGING lt_resources
              lt_occupy.


    CHECK lt_resources[] IS NOT INITIAL.

    " Se pasa a LT_RESOURCES_AUX por el tipo de datos SOBID->OBJID
    LOOP AT lt_resources INTO ls_resources.
      ls_resources_aux-plvar = ls_resources-plvar.
      ls_resources_aux-otype = ls_resources-otype.
      ls_resources_aux-objid = ls_resources-sobid.
      APPEND ls_resources_aux TO lt_resources_aux.
    ENDLOOP.

    " Se buscan descripciones de aulas
    SELECT objid short stext
      FROM hrp1000
      INTO TABLE et_aulas
      FOR ALL ENTRIES IN lt_resources_aux
      WHERE plvar EQ lt_resources_aux-plvar AND
            otype EQ lt_resources_aux-otype AND
            objid EQ lt_resources_aux-objid.

    SORT et_aulas BY room_objid.

  ENDMETHOD.


  METHOD buscar_eventos.
    TYPES:
      BEGIN OF ty_cant_inscriptos,
        plvar TYPE hrp1001-plvar,
        otype TYPE hrp1001-otype,
        objid TYPE hrp1001-objid,
        bcont TYPE kapz1,
      END OF ty_cant_inscriptos.

    DATA: ls_hrobject        TYPE  hrobject,
          lt_modules         TYPE  hrobject_t,
          lt_baseobjects     TYPE  piqrfc_offerrelations_t,
          ls_baseobjects     TYPE  piqrfc_offerrelations,
          lt_schedule_elem   TYPE  piqrfc_schedule_elements_t,
          lt_eventos         TYPE  zedu_t_eventos_aula,
          ls_eventos         TYPE  zedu_eventos_aula,
          lt_hrp1716         TYPE TABLE OF hrp1716,
          lt_hrt1716         TYPE TABLE OF hrt1716,
          lt_hrp1024         TYPE TABLE OF hrp1024,
          lt_hrp1001         TYPE TABLE OF hrp1001,
          ls_hrp1001         TYPE hrp1001,
          lt_hrp1000         TYPE TABLE OF hrp1000,
          ls_hrp1000_e       TYPE hrp1000,
          ls_hrp1000         TYPE hrp1000,
          ls_hrp1716         TYPE hrp1716,
          ls_hrt1716         TYPE hrt1716,
          ls_hrp1024         TYPE hrp1024,
          ls_hrp1035         TYPE hrp1035,
          ls_hrt1035         TYPE hrt1035,
          lv_sobid           TYPE hrp1001-sobid,
          lt_cant_inscriptos TYPE STANDARD TABLE OF ty_cant_inscriptos,
          ls_cant_inscriptos TYPE ty_cant_inscriptos,
          lt_hrp1739         TYPE TABLE OF hrp1739,
          ls_hrp1739         TYPE hrp1739,
          lv_lines           TYPE sy-tabix.

*	Begin	-->	MgM DCEK901885 agregado de sede 07/12/2016
    TYPES:  BEGIN OF lty_sedes,
              otype TYPE hrp1001-otype,
              objid TYPE hrp1001-objid,
              sclas TYPE hrp1001-sclas,
              sobid TYPE hrp1001-sobid,
              stext TYPE hrp1000-stext,
            END OF lty_sedes.
    DATA lt_sedes TYPE STANDARD TABLE OF lty_sedes.
    DATA ls_sede TYPE lty_sedes.
*	End	  -->	MgM DCEK901885

    IF iv_asignatura IS NOT INITIAL.
      ls_hrobject-plvar = '01'.
      ls_hrobject-otype = 'SM'. " Módulo

      ls_hrobject-objid = iv_asignatura.
      APPEND ls_hrobject TO lt_modules.

    ELSEIF iv_plan_estudio IS NOT INITIAL AND
           iv_asignatura   IS INITIAL.

      ls_hrobject-plvar = '01'.
      ls_hrobject-otype = 'SC'. " Plan de estudios
      ls_hrobject-objid = iv_plan_estudio.

      CALL FUNCTION 'HRIQ_ACADOFFER_MODULES_GET'
        EXPORTING
          is_hrobject   = ls_hrobject
*         IV_KEYDATE    = SY-DATUM
*         IV_YEAR       = iv_anio
*         IV_PERIOD     = iv_periodo
*         IV_STAGE      =
        TABLES
*         ET_ERRORTAB   =
          et_module     = lt_modules
        EXCEPTIONS
          wrong_call    = 1
          nothing_found = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


    ENDIF.

    IF iv_plan_estudio IS NOT INITIAL.

      " Ingresaron Plan de estudios o Plan de estudios/Asignatura
      CALL FUNCTION 'HRIQ_ACADOFFER_BASEOBJECTS_GET'
        EXPORTING
          is_hrobject    = ls_hrobject
          iv_year        = iv_anio
          iv_period      = iv_periodo
          iv_keydate     = sy-datum
*         IV_FILTER_CAMPUS       =
*         IV_ISTAT_EV    =
*         IV_PROC_STAT   =
*         IV_OFFERED     =
        TABLES
          it_modules     = lt_modules
          et_baseobjects = lt_baseobjects
*         ET_PERSON_EVENTS       =
        .
      SORT lt_baseobjects BY e_objid.

      IF iv_evento IS NOT INITIAL.
        " Si se está buscando un evento en particular, elimino eventos no relevantes
        DELETE lt_baseobjects WHERE e_objid NE iv_evento.
      ENDIF.

    ELSE.
      SELECT SINGLE *
        FROM hrp1000
        INTO ls_hrp1000_e
        WHERE plvar EQ '01'      AND
              otype EQ 'E'       AND
              objid EQ iv_evento.

      " Se está buscando por evento
      ls_baseobjects-e_objid = iv_evento.
      ls_baseobjects-e_short = ls_hrp1000_e-short.
      ls_baseobjects-e_stext = ls_hrp1000_e-stext.
      APPEND ls_baseobjects TO lt_baseobjects.
    ENDIF.

    CHECK lt_baseobjects IS NOT INITIAL.

*	Begin	-->	MgM DCEK901885 agregado de sede 07/12/2016
    "recupero Sedes de Eventos
    SELECT  h01~otype
            h01~objid
            h01~sclas
            h01~sobid
            h00~stext
        INTO TABLE lt_sedes
      FROM hrp1001 AS h01
          INNER JOIN hrp1000 AS h00
            ON  h01~sclas EQ h00~otype AND
                h01~sobid EQ h00~objid
        FOR ALL ENTRIES IN lt_baseobjects
      WHERE h01~objid EQ lt_baseobjects-e_objid
        AND h01~sclas EQ `F`.

    IF sy-subrc NE 0.
      CLEAR lt_sedes[].
    ENDIF.
*	End	  -->	MgM DCEK901885


    SELECT *
      FROM hrp1716
      INTO TABLE lt_hrp1716
      FOR ALL ENTRIES IN lt_baseobjects
      WHERE otype EQ 'E'                    AND
            objid EQ lt_baseobjects-e_objid.
*	Begin	-->	MgM DCEK901885 eventos ant. a hoy pero del año-periodo 07/12/2016
    " AND
*            begda LE sy-datum               AND
*            endda GE sy-datum.
*	End	  -->	MgM DCEK901885
    IF sy-subrc IS INITIAL.
      " Ordeno y elimino duplicados para tomar el último vigente
      SORT lt_hrp1716 BY plvar otype objid endda.
      DELETE ADJACENT DUPLICATES FROM lt_hrp1716 COMPARING plvar otype objid.

      SELECT *
        FROM hrt1716
        INTO TABLE lt_hrt1716
        FOR ALL ENTRIES IN lt_hrp1716
        WHERE tabnr EQ lt_hrp1716-tabnr.
      IF sy-subrc IS INITIAL.
        SORT lt_hrt1716 BY tabnr.

        " Selecciona descripción de aulas
        SELECT *
          INTO TABLE lt_hrp1000
          FROM hrp1000
          FOR ALL ENTRIES IN lt_hrt1716
          WHERE plvar EQ '01' AND
                otype EQ 'G '  AND
                objid EQ lt_hrt1716-room_objid. "AND "HIRS 29/08/2017 --> DCEK906192
*                begda LE sy-datum              AND "HIRS 29/08/2017 --> DCEK906192
*                endda GE sy-datum.                 "HIRS 29/08/2017 --> DCEK906192
        IF sy-subrc IS INITIAL.
          SORT lt_hrp1000 BY objid.
        ENDIF.
      ENDIF.

      SELECT *
        FROM hrp1739
        INTO TABLE lt_hrp1739
        FOR ALL ENTRIES IN lt_hrp1716
        WHERE plvar EQ lt_hrp1716-plvar AND
              otype EQ lt_hrp1716-otype AND
              objid EQ lt_hrp1716-objid. "AND "HIRS 29/08/2017 --> DCEK906192
*              begda LE sy-datum         AND "HIRS 29/08/2017 --> DCEK906192
*              endda GE sy-datum.            "HIRS 29/08/2017 --> DCEK906192
      IF sy-subrc IS INITIAL.
        SORT lt_hrp1739 BY plvar otype objid.
      ENDIF.

      SELECT *
        FROM hrp1001
        INTO TABLE lt_hrp1001
        FOR ALL ENTRIES IN lt_hrp1716
        WHERE plvar EQ lt_hrp1716-plvar AND
              otype EQ lt_hrp1716-otype AND
              objid EQ lt_hrp1716-objid AND
              rsign EQ 'A'              AND
              relat EQ '025'.
      IF sy-subrc IS INITIAL.
        SORT lt_hrp1001 BY plvar otype objid.

        LOOP AT lt_hrp1001 INTO ls_hrp1001.
          ls_cant_inscriptos-plvar = ls_hrp1001-plvar.
          ls_cant_inscriptos-otype = ls_hrp1001-otype.
          ls_cant_inscriptos-objid = ls_hrp1001-objid.
          ls_cant_inscriptos-bcont = 1.
          COLLECT ls_cant_inscriptos INTO lt_cant_inscriptos.
        ENDLOOP.

      ENDIF.

      SELECT *
        FROM hrp1024
        INTO TABLE lt_hrp1024
        FOR ALL ENTRIES IN lt_hrp1716
        WHERE plvar EQ lt_hrp1716-plvar AND
              otype EQ lt_hrp1716-otype AND
              objid EQ lt_hrp1716-objid. "AND "HIRS 29/08/2017 --> DCEK906192
*              begda LE sy-datum         AND  "HIRS 29/08/2017 --> DCEK906192
*              endda GE sy-datum.             "HIRS 29/08/2017 --> DCEK906192
      IF sy-subrc IS INITIAL.
        SORT lt_hrp1024 BY plvar otype objid.
      ENDIF.

    ENDIF.

    LOOP AT lt_hrp1716 INTO ls_hrp1716.
      ls_eventos-plvar = ls_hrp1716-plvar.
      ls_eventos-otype = ls_hrp1716-otype.
      ls_eventos-istat = ls_hrp1716-istat.
      ls_eventos-begda = ls_hrp1716-begda.
      ls_eventos-endda = ls_hrp1716-endda.

      READ TABLE lt_hrp1739 INTO ls_hrp1739
        WITH KEY plvar = ls_hrp1716-plvar
                 otype = ls_hrp1716-otype
                 objid = ls_hrp1716-objid
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_eventos-peryr = ls_hrp1739-peryr.
        ls_eventos-perid = ls_hrp1739-perid.
      ENDIF.

      READ TABLE lt_cant_inscriptos INTO ls_cant_inscriptos
        WITH KEY plvar = ls_hrp1716-plvar
                 otype = ls_hrp1716-otype
                 objid = ls_hrp1716-objid
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_eventos-bcont = ls_cant_inscriptos-bcont.
      ENDIF.

      READ TABLE lt_hrp1024 INTO ls_hrp1024
        WITH KEY plvar = ls_hrp1716-plvar
                 otype = ls_hrp1716-otype
                 objid = ls_hrp1716-objid
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_eventos-kapz1 = ls_hrp1024-kapz1.  "HIRS 29/08/2017 --> DCEK906192
        ls_eventos-kapz2 = ls_hrp1024-kapz2.
        ls_eventos-kapz3 = ls_hrp1024-kapz3.
      ENDIF.

      READ TABLE lt_baseobjects INTO ls_baseobjects
        WITH KEY e_objid = ls_hrp1716-objid
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_eventos-stext = ls_baseobjects-e_stext.
        ls_eventos-short = ls_baseobjects-e_short.
        ls_eventos-objid = ls_baseobjects-e_objid.
      ENDIF.
*	Begin	-->	MgM DCEK902955 descomenta modificación de DCEK902825 20/01/2017
*	Begin	-->	MgM DCEK902825 más de una franja horaria por evento 16/01/2017
      READ TABLE lt_hrt1716 INTO ls_hrt1716
        WITH KEY tabnr = ls_hrp1716-tabnr
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_eventos-beguz     = ls_hrt1716-beguz.
        ls_eventos-enduz     = ls_hrt1716-enduz.
        ls_eventos-monday    = ls_hrt1716-monday.
        ls_eventos-tuesday   = ls_hrt1716-tuesday.
        ls_eventos-wednesday = ls_hrt1716-wednesday.
        ls_eventos-thursday  = ls_hrt1716-thursday.
        ls_eventos-friday    = ls_hrt1716-friday.
        ls_eventos-saturday  = ls_hrt1716-saturday.
        ls_eventos-sunday    = ls_hrt1716-sunday.
        ls_eventos-room_objid = ls_hrt1716-room_objid.
      ENDIF.

      IF ls_eventos-room_objid IS NOT INITIAL.
        READ TABLE lt_hrp1000 INTO ls_hrp1000
          WITH KEY objid = ls_eventos-room_objid
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_eventos-room_name = ls_hrp1000-stext.
        ENDIF.

      ENDIF.
*	End	  -->	MgM DCEK902825
*	End	  -->	MgM DCEK902955

*	Begin	-->	MgM DCEK901885 agregado de sede 07/12/2016
      READ TABLE lt_sedes
        INTO ls_sede
          WITH KEY objid = ls_eventos-objid
                   otype = `E`.

      IF sy-subrc EQ 0.

        ls_eventos-sede_id    = ls_sede-sobid.
        ls_eventos-sede_text  = ls_sede-stext.

      ENDIF.
*	End	  -->	MgM DCEK901885

*	Begin	-->	MgM DCEK902955 comenta modificación de DCEK902825 20/01/2017
**  Begin --> MgM DCEK902825 más de una franja horaria por evento 16/01/2017
*      loop at lt_hrt1716
*        into ls_hrt1716
*        where tabnr = ls_hrp1716-tabnr.
*
*        ls_eventos-beguz     = ls_hrt1716-beguz.
*        ls_eventos-enduz     = ls_hrt1716-enduz.
*        ls_eventos-monday    = ls_hrt1716-monday.
*        ls_eventos-tuesday   = ls_hrt1716-tuesday.
*        ls_eventos-wednesday = ls_hrt1716-wednesday.
*        ls_eventos-thursday  = ls_hrt1716-thursday.
*        ls_eventos-friday    = ls_hrt1716-friday.
*        ls_eventos-saturday  = ls_hrt1716-saturday.
*        ls_eventos-sunday    = ls_hrt1716-sunday.
*        ls_eventos-room_objid = ls_hrt1716-room_objid.
*
*        if ls_eventos-room_objid is not initial.
*          read table lt_hrp1000 into ls_hrp1000
*            with key objid = ls_eventos-room_objid
*            binary search.
*          if sy-subrc is initial.
*            ls_eventos-room_name = ls_hrp1000-stext.
*          endif.
*
*        endif.
**  End   --> MgM DCEK902825
*	End	  -->	MgM DCEK902955

      APPEND ls_eventos TO lt_eventos.
      CLEAR ls_eventos.  "-->  MgM DCEK902825 "descomenta -->	MgM DCEK902955

    ENDLOOP.

*    endloop.  "-->  MgM DCEK902825

    IF iv_evento IS NOT INITIAL AND
       lt_eventos IS INITIAL.
      " Se puede tratar de un evento HCM
      CLEAR ls_eventos.

      SELECT SINGLE *
        FROM hrp1000
        INTO ls_hrp1000_e
        WHERE plvar EQ '01'      AND
              otype EQ 'E'       AND
              objid EQ iv_evento.
      CHECK sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM hrp1035
        INTO ls_hrp1035
        WHERE plvar EQ '01'      AND
              otype EQ 'E'       AND
              objid EQ iv_evento.
      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
          FROM hrt1035
          INTO ls_hrt1035
          WHERE tabnr EQ ls_hrp1035-tabnr.
      ENDIF.

      SELECT SINGLE *
        FROM hrp1024
        INTO ls_hrp1024
        WHERE plvar EQ '01'      AND
              otype EQ 'E'       AND
              objid EQ iv_evento.


      SELECT *
        FROM hrp1001
        INTO TABLE lt_hrp1001
        WHERE plvar EQ '01'      AND
              otype EQ 'E'       AND
              objid EQ iv_evento AND
              rsign EQ 'A'       AND
              relat EQ '025'     .

      DESCRIBE TABLE lt_hrp1001 LINES lv_lines.

      ls_eventos-plvar = ls_hrp1000_e-plvar.
      ls_eventos-otype = ls_hrp1000_e-otype.
      ls_eventos-objid = ls_hrp1000_e-objid.
      ls_eventos-istat = ls_hrp1000_e-istat.
      ls_eventos-begda = ls_hrp1000_e-begda.
      ls_eventos-endda = ls_hrp1000_e-endda.
      ls_eventos-langu = ls_hrp1000_e-langu.
      ls_eventos-stext = ls_hrp1000_e-stext.
      ls_eventos-short = ls_hrp1000_e-short.
      ls_eventos-bcont = lv_lines.
      ls_eventos-kapz1 = ls_hrp1024-kapz1.  "HIRS 29/08/2017 --> DCEK906192
      ls_eventos-kapz2 = ls_hrp1024-kapz2.
      ls_eventos-kapz3 = ls_hrp1024-kapz3.
      ls_eventos-beguz = ls_hrt1035-beguz.
      ls_eventos-enduz = ls_hrt1035-enduz.

      APPEND ls_eventos TO lt_eventos.

    ENDIF.
    et_eventos[] = lt_eventos[].

  ENDMETHOD.


  METHOD buscar_programa_evento.

    DATA: ls_programa_evento TYPE zedu_programa_evento,
          lt_resources       TYPE TABLE OF piqrfc_resources,
          lt_schedule        TYPE TABLE OF piqrfc_schedules,
          ls_resources       TYPE piqrfc_resources,
          ls_schedule        TYPE piqrfc_schedules.
*	Begin	-->	MgM DCEK902955 comenta modificación de DCEK902825 20/01/2017
**  Begin --> MgM DCEK902825 filtra por día de cursada 16/01/2017
*    data lt_days type wdr_date_nav_day_name_tab.
*    data ls_day type t246.
*
*    call function 'WEEKDAY_GET'
*      tables
*        weekday           = lt_days
*      exceptions
*        weekday_not_found = 1
*        others            = 2.
*
*    if sy-subrc <> 0.
** Implement suitable error handling here
*    endif.
*
*    case cl_bp_const=>true.
*      when is_hora_evento-monday.
*        read table lt_days
*          into ls_day with key wotnr = 1.
*      when is_hora_evento-tuesday.
*        read table lt_days
*          into ls_day with key wotnr = 2.
*      when is_hora_evento-wednesday.
*        read table lt_days
*          into ls_day with key wotnr = 3.
*      when is_hora_evento-thursday.
*        read table lt_days
*          into ls_day with key wotnr = 4.
*      when is_hora_evento-friday.
*        read table lt_days
*          into ls_day with key wotnr = 5.
*      when is_hora_evento-saturday.
*        read table lt_days
*          into ls_day with key wotnr = 6.
*      when is_hora_evento-sunday.
*        read table lt_days
*          into ls_day with key wotnr = 7.
*      when others.
*    endcase.
**  End   --> MgM DCEK902825
*	End	  -->	MgM DCEK902955

    REFRESH et_programa_evento[].

    " Recupera recursos y programa de evento
    CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
      EXPORTING
        event_id       = iv_evento
        event_otype    = 'E'
        plvar          = '01'
*       LANGUAGE       = SY-LANGU
*       LANGUAGE_ISO   =
*       BEGDA          = '19000101'
*       ENDDA          = '99991231'
*       ISTAT          = ' '
*       SUBTY          = ' '
*       READ_DESCRIPTIONS               = ' '
*       READ_CAPACITIES                 = ' '
*       READ_CAMPUS_RELATIONS           = ' '
*       READ_WEBLINKS  = ' '
        read_resources = 'X'
        read_schedules = 'X'
*       READ_SCHED_DESCRIPTION          = ' '
*       READ_OCCUPATIONS                = ' '
*       READ_EVENTINFOS                 = ' '
*       READ_RESOURCE_ADDRESSES         = ' '
*       READ_RESOURCE_DESCRIPTION       = ' '
*       READ_FAC_WORKLOAD               = ' '
*       READ_OFFERSESSION               = ' '
*       READ_PROC_STATUS                = ' '
*       READ_SCHED_ELEM                 = ' '
*       READ_ROOM_CAPACITY              = ' '
*       READ_RESPONSIBILITY             = ' '
      TABLES
*       EVENTS         =
*       DESCRIPTIONS   =
*       CAPACITIES     =
*       CAMPUS_RELATIONS                =
*       OCCUPATIONS    =
        resources      = lt_resources
        schedules      = lt_schedule
*       WEBLINKS       =
*       SCHEDULE_DESCRIPTIONS           =
*       EVENTINFOS     =
*       ROOM_ADDRESSES =
*       RESOURCES_DESCRIPTION           =
*       ERRORTAB       =
*       OCCUPATIONS_E_AND_EL            =
*       ET_OFFERSESSION                 =
*       ET_FACWORKLOAD =
*       ET_CONTACTHOURS                 =
*       ET_SCHEDULE_RELPARAM            =
*       ET_SCHEDULE_ELEM                =
*       ET_EVPROCSTAT  =
*       ET_ROOM_CAPACITY                =
*       ET_EL_RESPONSIBILITY            =
      .

    " Ordena tablas
    SORT lt_schedule BY evdat.
    SORT lt_resources BY resbg beguz. "HIRS 22.09.2017  -- Se adiciona ordenamiento BEGUZ

    " Elimina recursos que no son aulas
    DELETE lt_resources WHERE restp NE 'G'.

*	Begin	-->	MgM DCEK902955 comenta modificación de DCEK902825 20/01/2017
**  Begin --> MgM DCEK902825 filtra por día de cursada 16/01/2017
*    delete lt_schedule where  beguz ne is_hora_evento-beguz
*                          and enduz ne is_hora_evento-enduz
*                          and daytxt ne ls_day-langt.
**  End   --> MgM DCEK902825
*	End	  -->	MgM DCEK902955

    " Recorre programa asignando recursos por dia
    LOOP AT lt_schedule INTO ls_schedule.
      CLEAR ls_programa_evento.
      READ TABLE lt_resources INTO ls_resources
        WITH KEY resbg = ls_schedule-evdat
                 beguz = ls_schedule-beguz  "HIRS 22.09.2017  -- Se adiciona filtro BEGUZ
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_programa_evento-begda = ls_resources-resbg.
        ls_programa_evento-endda = ls_resources-resed.
        ls_programa_evento-beguz = ls_resources-beguz.
        ls_programa_evento-enduz = ls_resources-enduz.
        ls_programa_evento-room_objid = ls_resources-resid.
        ls_programa_evento-room_name = ls_resources-resxt.
      ELSE.
        ls_programa_evento-begda = ls_schedule-evdat.
        ls_programa_evento-endda = ls_schedule-evdat.
        ls_programa_evento-beguz = ls_schedule-beguz.
        ls_programa_evento-enduz = ls_schedule-enduz.
      ENDIF.
      APPEND ls_programa_evento TO et_programa_evento.

    ENDLOOP.

    SORT et_programa_evento[] BY begda endda.

  ENDMETHOD.


  METHOD get_room_name.
    DATA: ls_error TYPE bapiret2,
          lv_stext TYPE hrp1000-stext.

    " Selecciona descripción para aula
    SELECT SINGLE stext
      FROM hrp1000
      INTO lv_stext
      WHERE plvar EQ '01'          AND
            otype EQ 'G'           AND
            objid EQ iv_room_objid AND
            begda LE sy-datum      AND
            endda GE sy-datum.
    IF sy-subrc IS INITIAL.
      ev_room_name = lv_stext.
    ELSE.
      clear ev_room_name.
      ls_error-type   = 'E'.
      ls_error-id     = ga_id_message.
      ls_error-number = 011.
      ls_error-message_v1 = iv_room_objid.
      APPEND ls_error TO rt_error.
    ENDIF.

  ENDMETHOD.


  METHOD modificar_evento.

**********************************************************************
    " Para modificar el aula de un evento se debe:
    " - Modificar las aulas de todo el programa del evento, es decir,
    "   modificar los recursos G (aula) de todos los dias de evento
    "
    " - Modificar el aula asignada en el infotipo 1716 (tablas HRP1716 y
    "   HRT1716)
    "
    " Al modificar un aula de un evento, también se debe modificar el tipo
    " de aula, ya que el aula vieja y nueva puede diferer en su tipo
**********************************************************************

    DATA: lv_evtypid         TYPE  etyid,
          ls_text            TYPE  hri1000,
          ls_capacity        TYPE  hri1024,
          ls_eventinfo       TYPE  hri1026,
          ls_sched_relatparm TYPE  hri1716,
          ls_acadeventinfo   TYPE  hri1722,
          ls_period          TYPE  hri1739,
          ls_teach_hours     TYPE  hri1753,
          lt_facwl           TYPE  piqfacworkload_t,
          lv_location        TYPE  hrobjid,
          lt_desc            TYPE  piq_bapidesc_t,
          lt_schedelem       TYPE  piq_pt1716_t,
          ls_schedule        TYPE  bapisched,
          lt_schedule        TYPE  piq_bapisched_t,
          lt_resources       TYPE  piq_bapiresou_t,
          lt_resources_old   TYPE  TABLE OF piqrfc_resources,
          lt_schedules_old   TYPE  TABLE OF piqrfc_schedules,
          ls_resources_old   TYPE  piqrfc_resources,
          ls_schedules_old   TYPE  piqrfc_schedules,
          ls_resources       TYPE  bapiresou,
          lt_eventpackage    TYPE  piq_objec_t,
          lt_return          TYPE  piq_bapiret2_tab,
          ls_return          TYPE  bapiret2,
          lt_infty_1716_upd  TYPE  piq_p1716_t,
          lt_pt1716_upd      TYPE  piq_pt1716_t,
          ls_pt1716_upd      TYPE  pt1716,
          ls_hrp1001_room    TYPE  hrp1001.


    " Selecciona tipo de recurso del aula nueva
    SELECT SINGLE *
      FROM hrp1001
      INTO ls_hrp1001_room
      WHERE otype EQ 'G' AND " Tipo de objeto Aula
            objid EQ is_evento_aula-room_objid AND " Objeto aula nueva
            rsign EQ 'A' AND
            relat EQ '020' AND
            begda LE sy-datum AND
            endda GE sy-datum.

    " Recupera recursos y programa de evento
    CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
      EXPORTING
        event_id       = is_evento_aula-objid
        event_otype    = is_evento_aula-otype
        plvar          = is_evento_aula-plvar
*       LANGUAGE       = SY-LANGU
*       LANGUAGE_ISO   =
        begda          = is_evento_aula-begda
        endda          = is_evento_aula-endda
        istat          = is_evento_aula-istat
*       SUBTY          = ' '
*       READ_DESCRIPTIONS               = ' '
*       READ_CAPACITIES                 = ' '
*       READ_CAMPUS_RELATIONS           = ' '
*       READ_WEBLINKS  = ' '
        read_resources = 'X'
        read_schedules = 'X'
*       READ_SCHED_DESCRIPTION          = ' '
*       READ_OCCUPATIONS                = ' '
*       READ_EVENTINFOS                 = ' '
*       READ_RESOURCE_ADDRESSES         = ' '
*       READ_RESOURCE_DESCRIPTION       = ' '
*       READ_FAC_WORKLOAD               = ' '
*       READ_OFFERSESSION               = ' '
*       READ_PROC_STATUS                = ' '
*       READ_SCHED_ELEM                 = ' '
*       READ_ROOM_CAPACITY              = ' '
*       READ_RESPONSIBILITY             = ' '
      TABLES
*       EVENTS         =
*       DESCRIPTIONS   =
*       CAPACITIES     =
*       CAMPUS_RELATIONS                =
*       OCCUPATIONS    =
        resources      = lt_resources_old
        schedules      = lt_schedules_old
*       WEBLINKS       =
*       SCHEDULE_DESCRIPTIONS           =
*       EVENTINFOS     =
*       ROOM_ADDRESSES =
*       RESOURCES_DESCRIPTION           =
*       ERRORTAB       =
*       OCCUPATIONS_E_AND_EL            =
*       ET_OFFERSESSION                 =
*       ET_FACWORKLOAD =
*       ET_CONTACTHOURS                 =
*       ET_SCHEDULE_RELPARAM            =
*       ET_SCHEDULE_ELEM                =
*       ET_EVPROCSTAT  =
*       ET_ROOM_CAPACITY                =
*       ET_EL_RESPONSIBILITY            =
      .

    " Recorro programa del evento
    LOOP AT lt_schedules_old INTO ls_schedules_old.
      " Agrego programa del evento a tabla LT_SCHEDULE
      MOVE-CORRESPONDING ls_schedules_old TO ls_schedule.
      APPEND ls_schedule TO lt_schedule.
    ENDLOOP.

    " Recorro recursos del evento
    LOOP AT lt_resources_old INTO ls_resources_old.
      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
      " Agrego recursos que no son aula (Ej: Docentes)
      IF ls_resources_old-restp NE 'G'. " Recursos que no son Aula
        APPEND ls_resources TO lt_resources.
      ENDIF.
    ENDLOOP.


    IF is_evento_aula-room_objid IS NOT INITIAL.
      " Agrega recursos Aula según programa
      LOOP AT lt_schedule INTO ls_schedule.
        ls_resources-restp = 'G'. " Aula
        ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
        ls_resources-resht = is_evento_aula-room_name.
        ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
        ls_resources-retxt = ''.
        ls_resources-reknz = ls_hrp1001_room-sclas.
        ls_resources-resbg = ls_schedule-evdat.
        ls_resources-resed = ls_schedule-evdat.
        ls_resources-beguz = ls_schedule-beguz.
        ls_resources-enduz = ls_schedule-enduz.
        APPEND ls_resources TO lt_resources.
      ENDLOOP.
    ENDIF.

    " Información de evento
    ls_eventinfo-langu = sy-langu.
    ls_eventinfo-bcont = is_evento_aula-bcont.

    " Año y período de evento
    ls_period-peryr = is_evento_aula-peryr.
    ls_period-perid = is_evento_aula-perid.

    " Modifica asignación de recursos aula del programa del evento
    CALL FUNCTION 'HRIQ_EVENT_CHANGE'
      EXPORTING
*       IV_PLVAR            =
        iv_evtypid          = lv_evtypid
        iv_eventid          = is_evento_aula-objid
        iv_istat            = is_evento_aula-istat
        iv_begda            = is_evento_aula-begda
        iv_endda            = is_evento_aula-endda
        iv_langu            = sy-langu
*       IV_CHANGE_TEXT      = ' '
        is_text             = ls_text
*       IV_CHANGE_CAPACITY  = ' '
        is_capacity         = ls_capacity
*       IV_CHANGE_EVINFO    = ' '
        is_eventinfo        = ls_eventinfo
*       IV_CHANGE_SCHED_DESCR         = ' '
        is_sched_relatparm  = ls_sched_relatparm
*       IV_CHANGE_ACADEVINFO          =
        is_acadeventinfo    = ls_acadeventinfo
        is_period           = ls_period
*       IV_CHANGE_TEACH_HOURS         = ' '
        is_teach_hours      = ls_teach_hours
        it_facwl            = lt_facwl
*       IV_CHANGE_LOCATION  = ' '
        iv_location         = lv_location
*       IV_CHANGE_DESC      = ' '
        it_desc             = lt_desc
*       IT_DESC_OLD         =
        it_schedelem        = lt_schedelem
        iv_change_schedule  = ' '
        it_schedule         = lt_schedule
        iv_change_resources = 'X'
        it_resources        = lt_resources " Nuevos recursos aula
*       IV_CHANGE_INSTR_RESPONS       =
*       IT_INSTR_RESPONS    =
*       IV_CHANGE_EVENTPACKAGE        = ' '
        it_eventpackage     = lt_eventpackage
*       IV_CHANGE_OTHER     = ' '
*       IT_OTHERINFO        =
*       IT_OTHERINFO_TAB    =
*       IV_CHECK_RESOURCES  = 'X'
*       IV_ENQUEUE          = 'X'
*       IV_COMMIT_FLG       = ' '
*       IV_ENQUEUE_EVENT    = ' '
*       IT_MODULES          =
*       IV_SM_OBJID         =
*       IT_ROOMCAP          =
      IMPORTING
*       EV_OPEN_RES_EXIST   =
        et_return           = lt_return.
    IF lt_return[] IS INITIAL.

      " Recupera información de infotipo 1716 (HRP1716 y HRT1716)
      " para cambiar la asignación de aula
      SELECT *
        FROM hrp1716
        INTO CORRESPONDING FIELDS OF TABLE lt_infty_1716_upd
        WHERE plvar EQ is_evento_aula-plvar AND
              otype EQ is_evento_aula-otype AND
              objid EQ is_evento_aula-objid. "AND  "HIRS 29/08/2017 --> DCEK906192
*              begda LE sy-datum             AND  "HIRS 29/08/2017 --> DCEK906192
*              endda GE sy-datum.                 "HIRS 29/08/2017 --> DCEK906192
      IF sy-subrc IS INITIAL.
        SELECT *
          FROM hrt1716
          INTO CORRESPONDING FIELDS OF TABLE lt_pt1716_upd
          FOR ALL ENTRIES IN lt_infty_1716_upd
          WHERE tabnr EQ lt_infty_1716_upd-tabnr.
      ENDIF.

      " Se pasa la nueva aula y el nuevo tipo de aula
      LOOP AT lt_pt1716_upd INTO ls_pt1716_upd.
        ls_pt1716_upd-room_objid = is_evento_aula-room_objid.
        ls_pt1716_upd-room_retid = ls_hrp1001_room-sobid.
        MODIFY  lt_pt1716_upd FROM ls_pt1716_upd INDEX sy-tabix.
      ENDLOOP.

      " Se modifica infotipo 1716
      CALL FUNCTION 'HRIQ_MODUL_SAVE_INFTYS_TO_DB'
        EXPORTING
*         IV_VTASK          = 'D'
*         IV_COMMIT_FLG     = 'X'
*         IV_NO_VALIDITY    = ' '
*         IT_INFTY_NNNN_INS =
*         IT_INFTY_NNNN_DEL =
*         IT_INFTY_NNNN_UPD =
*         IT_INFTY_1000_DEL =
*         IT_INFTY_1002_INS =
*         IT_DESCRIPTION_INS         =
*         IT_INFTY_1002_UPD =
*         IT_DESCRIPTION_UPD         =
*         IT_INFTY_1716_INS =
*         IT_PT1716_INS     =
*         IT_INFTY_1753_INS =
*         IT_HRT1753_INS    =
*         IT_INFTY_1753_UPD =
*         IT_HRT1753_UPD    =
          it_infty_1716_upd = lt_infty_1716_upd
          it_pt1716_upd     = lt_pt1716_upd
*         IT_OTHERINFTY_UPD =
*         IT_OTHERINFTYTAB_UPD       =
        EXCEPTIONS
          no_authorisation  = 1
          internal_error    = 2
          corr_exit         = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        " Programa modificado exitosamente, infotipo no modificado
        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 015.
        ls_return-message_v1 = is_evento_aula-objid.
        INSERT ls_return INTO lt_return INDEX 1.
      ELSE.
        " Commit a la base de datos
        CALL FUNCTION 'HRIQ_UPDATE_DATABASE'
          EXPORTING
            vtask          = 'D'
*           BUFFER_UPD     = ' '
*           ORDER_FLG      = 'X'
*           COMMIT_FLG     = 'X'
*           CLEAR_BUFFER   = 'X'
*           KEEP_LUPD      =
*           WORKF_ACTV     = 'X'
          EXCEPTIONS
            corr_exit      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          " Error al actualizar la base de datos
          " Se muestra mensaje que muestra la transacción estandar
          CLEAR ls_return.
          ls_return-id = 'HRPIQ000'.
          ls_return-type = 'E'.
          ls_return-number = 816.
          INSERT ls_return INTO lt_return INDEX 1.
          EXIT.
          IF 1 = 2. MESSAGE x816(hrpiq000). ENDIF.
        ELSE.
          " Programa e Infotipo modificados exitosamente
          CLEAR ls_return.
          ls_return-id = ga_id_message.
          ls_return-type = 'S'.
          ls_return-number = 014.
          ls_return-message_v1 = is_evento_aula-objid.
          INSERT ls_return INTO lt_return INDEX 1.
        ENDIF.

      ENDIF.

    ELSE.
      " Error al modificar programa de evento
      CLEAR ls_return.
      ls_return-id = ga_id_message.
      ls_return-type = 'E'.
      ls_return-number = 013.
      ls_return-message_v1 = is_evento_aula-objid.
      INSERT ls_return INTO lt_return INDEX 1.
    ENDIF.

    rt_errores[] = lt_return[].


  ENDMETHOD.


  METHOD modificar_programa.

**********************************************************************
    " Para modificar el programa de un evento se debe:
    " - Modificar las aulas de todo el programa del evento, es decir,
    "   modificar los recursos G (aula) de todos los dias de evento
    "
    " Al modificar un aula de un evento, también se debe modificar el tipo
    " de aula, ya que el aula vieja y nueva puede diferer en su tipo
**********************************************************************

    DATA: lv_evtypid         TYPE  etyid,
          ls_text            TYPE  hri1000,
          ls_capacity        TYPE  hri1024,
          ls_eventinfo       TYPE  hri1026,
          ls_sched_relatparm TYPE  hri1716,
          ls_acadeventinfo   TYPE  hri1722,
          ls_period          TYPE  hri1739,
          ls_teach_hours     TYPE  hri1753,
          lt_facwl           TYPE  piqfacworkload_t,
          lv_location        TYPE  hrobjid,
          lt_desc            TYPE  piq_bapidesc_t,
          lt_schedelem       TYPE  piq_pt1716_t,
          ls_schedule        TYPE  bapisched,
          lt_schedule        TYPE  piq_bapisched_t,
          lt_resources       TYPE  piq_bapiresou_t,
          lt_resources_old   TYPE  TABLE OF piqrfc_resources,
          lt_schedules_old   TYPE  TABLE OF piqrfc_schedules,
          ls_resources_old   TYPE  piqrfc_resources,
          ls_schedules_old   TYPE  piqrfc_schedules,
          ls_resources       TYPE  bapiresou,
          lt_eventpackage    TYPE  piq_objec_t,
          lt_return          TYPE  piq_bapiret2_tab,
          ls_return          TYPE  bapiret2,
          lt_infty_1716_upd  TYPE  piq_p1716_t,
          lt_pt1716_upd      TYPE  piq_pt1716_t,
          ls_pt1716_upd      TYPE  pt1716,
          lt_hrp1001_room    TYPE  TABLE OF hrp1001,
          ls_hrp1001_room    TYPE  hrp1001,
          ls_programa        TYPE  zedu_programa_evento,
          lt_aulas           TYPE TABLE OF zedu_aula,
          lt_programa        TYPE zedu_t_programa_evento,
          lv_error           TYPE char1.


    " Selecciona tipo de recurso del aula nueva
    SELECT *
      FROM hrp1001
      INTO TABLE lt_hrp1001_room
      FOR ALL ENTRIES IN it_programa
      WHERE otype EQ 'G' AND " Tipo de objeto Aula
            objid EQ it_programa-room_objid AND " Objeto aula nueva
            rsign EQ 'A' AND
            relat EQ '020' AND
            begda LE sy-datum AND
            endda GE sy-datum.

    " Recupera recursos y programa de evento
    CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
      EXPORTING
        event_id       = is_evento_aula-objid
        event_otype    = is_evento_aula-otype
        plvar          = is_evento_aula-plvar
*       LANGUAGE       = SY-LANGU
*       LANGUAGE_ISO   =
        begda          = is_evento_aula-begda
        endda          = is_evento_aula-endda
        istat          = is_evento_aula-istat
*       SUBTY          = ' '
*       READ_DESCRIPTIONS               = ' '
*       READ_CAPACITIES                 = ' '
*       READ_CAMPUS_RELATIONS           = ' '
*       READ_WEBLINKS  = ' '
        read_resources = 'X'
        read_schedules = 'X'
*       READ_SCHED_DESCRIPTION          = ' '
*       READ_OCCUPATIONS                = ' '
*       READ_EVENTINFOS                 = ' '
*       READ_RESOURCE_ADDRESSES         = ' '
*       READ_RESOURCE_DESCRIPTION       = ' '
*       READ_FAC_WORKLOAD               = ' '
*       READ_OFFERSESSION               = ' '
*       READ_PROC_STATUS                = ' '
*       READ_SCHED_ELEM                 = ' '
*       READ_ROOM_CAPACITY              = ' '
*       READ_RESPONSIBILITY             = ' '
      TABLES
*       EVENTS         =
*       DESCRIPTIONS   =
*       CAPACITIES     =
*       CAMPUS_RELATIONS                =
*       OCCUPATIONS    =
        resources      = lt_resources_old
        schedules      = lt_schedules_old
*       WEBLINKS       =
*       SCHEDULE_DESCRIPTIONS           =
*       EVENTINFOS     =
*       ROOM_ADDRESSES =
*       RESOURCES_DESCRIPTION           =
*       ERRORTAB       =
*       OCCUPATIONS_E_AND_EL            =
*       ET_OFFERSESSION                 =
*       ET_FACWORKLOAD =
*       ET_CONTACTHOURS                 =
*       ET_SCHEDULE_RELPARAM            =
*       ET_SCHEDULE_ELEM                =
*       ET_EVPROCSTAT  =
*       ET_ROOM_CAPACITY                =
*       ET_EL_RESPONSIBILITY            =
      .

    " Recorro programa del evento
    LOOP AT lt_schedules_old INTO ls_schedules_old.
      " Agrego programa del evento a tabla LT_SCHEDULE
      MOVE-CORRESPONDING ls_schedules_old TO ls_schedule.
      APPEND ls_schedule TO lt_schedule.
    ENDLOOP.

    " Recorro recursos del evento
    LOOP AT lt_resources_old INTO ls_resources_old.
      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
      " Agrego recursos que no son aula (Ej: Docentes)
      IF ls_resources_old-restp NE 'G'. " Recursos que no son Aula
        APPEND ls_resources TO lt_resources.
      ENDIF.
    ENDLOOP.

    " Agrega recursos Aula según programa
    LOOP AT it_programa INTO ls_programa.
      IF ls_programa-room_objid IS INITIAL.
        CONTINUE.
      ENDIF.


      " Valida aula ingresada por el usuario
      REFRESH lt_programa.
      APPEND ls_programa TO lt_programa.

      CALL METHOD buscar_aulas_disponibles
        EXPORTING
          is_evento_aula = is_evento_aula
          it_programa    = lt_programa
        IMPORTING
          et_aulas       = lt_aulas.


      " Busca aula ingresada como aula disponible
      READ TABLE lt_aulas TRANSPORTING NO FIELDS
        WITH KEY room_objid = ls_programa-room_objid
        BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        " Aula no está disponible
        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 026.
        ls_return-message_v1 = ls_programa-room_objid.
        WRITE ls_programa-endda TO ls_return-message_v2.
        INSERT ls_return INTO lt_return INDEX 1.
        lv_error = 'X'.
        EXIT.
      ENDIF.

      ls_resources-restp = 'G'. " Aula
      ls_resources-resid = ls_programa-room_objid. " Nueva aula
      ls_resources-resht = ls_programa-room_name.

      " Busca tipo de aula según aula
      READ TABLE lt_hrp1001_room INTO ls_hrp1001_room
        WITH KEY objid = ls_programa-room_objid
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
        ls_resources-retxt = ''.
        ls_resources-reknz = ls_hrp1001_room-sclas.
      ENDIF.

      ls_resources-resbg = ls_programa-begda.
      ls_resources-resed = ls_programa-endda.
      ls_resources-beguz = ls_programa-beguz.
      ls_resources-enduz = ls_programa-enduz.
      APPEND ls_resources TO lt_resources.

    ENDLOOP.

    IF lv_error IS INITIAL.
      " Si no hay errores de disponibilidad de aulas, continúa

      " Información de evento
      ls_eventinfo-langu = sy-langu.
      ls_eventinfo-bcont = is_evento_aula-bcont.

      " Año y período de evento
      ls_period-peryr = is_evento_aula-peryr.
      ls_period-perid = is_evento_aula-perid.

      " Modifica asignación de recursos aula del programa del evento
      CALL FUNCTION 'HRIQ_EVENT_CHANGE'
        EXPORTING
*         IV_PLVAR            =
          iv_evtypid          = lv_evtypid
          iv_eventid          = is_evento_aula-objid
          iv_istat            = is_evento_aula-istat
          iv_begda            = is_evento_aula-begda
          iv_endda            = is_evento_aula-endda
          iv_langu            = sy-langu
*         IV_CHANGE_TEXT      = ' '
          is_text             = ls_text
*         IV_CHANGE_CAPACITY  = ' '
          is_capacity         = ls_capacity
*         IV_CHANGE_EVINFO    = ' '
          is_eventinfo        = ls_eventinfo
*         IV_CHANGE_SCHED_DESCR         = ' '
          is_sched_relatparm  = ls_sched_relatparm
*         IV_CHANGE_ACADEVINFO          =
          is_acadeventinfo    = ls_acadeventinfo
          is_period           = ls_period
*         IV_CHANGE_TEACH_HOURS         = ' '
          is_teach_hours      = ls_teach_hours
          it_facwl            = lt_facwl
*         IV_CHANGE_LOCATION  = ' '
          iv_location         = lv_location
*         IV_CHANGE_DESC      = ' '
          it_desc             = lt_desc
*         IT_DESC_OLD         =
          it_schedelem        = lt_schedelem
          iv_change_schedule  = ' '
          it_schedule         = lt_schedule
          iv_change_resources = 'X'
          it_resources        = lt_resources " Nuevos recursos aula
*         IV_CHANGE_INSTR_RESPONS       =
*         IT_INSTR_RESPONS    =
*         IV_CHANGE_EVENTPACKAGE        = ' '
          it_eventpackage     = lt_eventpackage
*         IV_CHANGE_OTHER     = ' '
*         IT_OTHERINFO        =
*         IT_OTHERINFO_TAB    =
*         IV_CHECK_RESOURCES  = 'X'
*         IV_ENQUEUE          = 'X'
*         IV_COMMIT_FLG       = ' '
*         IV_ENQUEUE_EVENT    = ' '
*         IT_MODULES          =
*         IV_SM_OBJID         =
*         IT_ROOMCAP          =
        IMPORTING
*         EV_OPEN_RES_EXIST   =
          et_return           = lt_return.
      IF lt_return[] IS INITIAL.
        " Commit a la base de datos
        CALL FUNCTION 'HRIQ_UPDATE_DATABASE'
          EXPORTING
            vtask          = 'D'
*           BUFFER_UPD     = ' '
*           ORDER_FLG      = 'X'
*           COMMIT_FLG     = 'X'
*           CLEAR_BUFFER   = 'X'
*           KEEP_LUPD      =
*           WORKF_ACTV     = 'X'
          EXCEPTIONS
            corr_exit      = 1
            internal_error = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
          " Error al actualizar la base de datos
          " Se muestra mensaje que muestra la transacción estandar
          CLEAR ls_return.
          ls_return-id = 'HRPIQ000'.
          ls_return-type = 'E'.
          ls_return-number = 816.
          INSERT ls_return INTO lt_return INDEX 1.
          EXIT.
          IF 1 = 2. MESSAGE x816(hrpiq000). ENDIF.
        ELSE.
          " Programa modificado exitosamente
          CLEAR ls_return.
          ls_return-id = ga_id_message.
          ls_return-type = 'S'.
          ls_return-number = 017.
          ls_return-message_v1 = is_evento_aula-objid.
          INSERT ls_return INTO lt_return INDEX 1.
        ENDIF.

      ELSE.
        " Error al modificar programa de evento
        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 018.
        ls_return-message_v1 = is_evento_aula-objid.
        INSERT ls_return INTO lt_return INDEX 1.
      ENDIF.
    ENDIF.
    rt_errores[] = lt_return[].

  ENDMETHOD.
ENDCLASS.
