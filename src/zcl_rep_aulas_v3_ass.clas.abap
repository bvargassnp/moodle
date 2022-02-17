class ZCL_REP_AULAS_V3_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  final
  create public .

public section.

  methods VALIDAR_DISPO_AULA
    importing
      !IM_EVENTO type ZEDU_EVENTOS_AULA_V3
      !IM_CHECK_BEGUZ type ABAP_BOOL optional
    returning
      value(RE_SUBRC) type ABAP_BOOL .
  methods BUSCAR_AULAS_DISPONIBLES
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA_V3
      !IT_PROGRAMA type ZEDU_T_PROGRAMA_EVENTO_V2
    exporting
      !ET_AULAS type ZEDU_T_AULA .
  methods MODIFICAR_PROGRAMA
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA_V3
      !IT_PROGRAMA type ZEDU_T_PROGRAMA_EVENTO_V2
    returning
      value(RT_ERRORES) type BAPIRETURN_T .
  methods MODIFICAR_EVENTO
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA_V3
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
      !ET_EVENTOS type ZEDU_T_EVENTOS_AULA_V3 .
  methods GET_ROOM_NAME
    importing
      !IV_ROOM_OBJID type HROBJID
    exporting
      !EV_ROOM_NAME type STEXT
    returning
      value(RT_ERROR) type BAPIRETURN_T .
  methods BUSCAR_PROGRAMA_EVENTO
    importing
      !IS_EVENTO type ZEDU_EVENTOS_AULA_V3
    exporting
      !ET_PROGRAMA_EVENTO type ZEDU_T_PROGRAMA_EVENTO_V2 .
  methods GET_R_DAYTXT
    importing
      !IS_EVENTO type ZEDU_EVENTOS_AULA_V3
    exporting
      !ER_DAYTXT type ZEDU_R_DAYTXT_T .
  methods AULAS_NO_VIRTUALES
    importing
      !IM_AULAS type ZEDU_T_AULA
      !IM_VIRTUAL type SY-SUBRC
    returning
      value(RE_AULAS) type ZEDU_T_AULA .
  methods BUSCAR_AULAS_VIRTUALES_DISP
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA_V3
      !IT_PROGRAMA type ZEDU_T_PROGRAMA_EVENTO_V2
    exporting
      !ET_AULAS type ZEDU_T_AULA .
  methods MODIFICAR_EVENTO_V
    importing
      !IS_EVENTO_AULA type ZEDU_EVENTOS_AULA_V3
    returning
      value(RT_ERRORES) type BAPIRETURN_T .
protected section.
private section.

  types:
    BEGIN OF ty_resources,
      plvar TYPE hrp1000-plvar,
      otype TYPE hrp1000-otype,
      objid TYPE hrp1000-objid,
    END OF ty_resources .
  types:
    gty_t_hrt1716 TYPE TABLE OF hrt1716 WITH EMPTY KEY .
  types:
    gty_t_hrp1716 TYPE TABLE OF hrp1716 WITH EMPTY KEY .

  methods MODIFICAR_EVENTO_VIRTUAL
    importing
      !IM_T_PIQ_P1716_T type PIQ_P1716_T
      !IM_HRT1716 type HRT1716
      !IM_EVENTO type ZEDU_EVENTOS_AULA_V3 .
  methods VALIDAR_DISPO_INDV
    importing
      !IM_T_HRP1716 type GTY_T_HRP1716
      !IM_T_HRT1716 type GTY_T_HRT1716
      !IM_EVENTO type ZEDU_EVENTOS_AULA_V3
    returning
      value(RE_ABAP_BOOL) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_REP_AULAS_V3_ASS IMPLEMENTATION.


  METHOD aulas_no_virtuales.

    " buscar la configuración para aulas virtuales
    SELECT objid,build
      FROM hrp1028
      INTO TABLE @DATA(lt_hrp1028)
      FOR ALL ENTRIES IN @im_aulas
      WHERE objid EQ @im_aulas-room_objid
        AND build NE 'VIRTUAL'.

    SORT lt_hrp1028 ASCENDING BY objid.
    " validar las aulas que no son vistuales
    LOOP AT im_aulas INTO DATA(wa_aulas).
      READ TABLE lt_hrp1028 TRANSPORTING NO FIELDS
        WITH KEY objid = wa_aulas-room_objid
        BINARY SEARCH.

      IF im_virtual EQ 0.
        CHECK sy-subrc EQ 0.
      ELSE.
        CHECK sy-subrc NE 0.
      ENDIF.
      APPEND wa_aulas TO re_aulas.
    ENDLOOP.


  ENDMETHOD.


  METHOD BUSCAR_AULAS_DISPONIBLES.
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
      ls_programa      TYPE zedu_programa_evento_v2,
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


  METHOD buscar_aulas_virtuales_disp.
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
      ls_programa      TYPE zedu_programa_evento_v2,
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

    " SELECCIONAR AULAS VIRTUALES
    DATA(lt_aulas_v) = me->aulas_no_virtuales(
                              " Tipo tabla para ayuda de búsqueda en ZWDC_REP_AULAS
                              EXPORTING
                                im_aulas   = VALUE #(
                                               FOR wa_resources IN lt_resources
                                               ( room_objid = wa_resources-sobid )  )
                                im_virtual = 4    " Indicador 0 = No Virtuales 1 = Virtuales
                                ).

    CHECK lt_aulas_v[] IS NOT INITIAL.

    " Se pasa a LT_RESOURCES_AUX por el tipo de datos SOBID->OBJID
    LOOP AT lt_resources INTO ls_resources.
      CHECK line_exists( lt_aulas_v[
                            room_objid = ls_resources-sobid ] ).
      ls_resources_aux-plvar = ls_resources-plvar.
      ls_resources_aux-otype = ls_resources-otype.
      ls_resources_aux-objid = ls_resources-sobid.

      CHECK me->validar_dispo_aula(
                 im_evento =
                   VALUE #( BASE is_evento_aula
                            room_objid_v = ls_resources_aux-objid )  ) EQ abap_false.

      APPEND ls_resources_aux TO lt_resources_aux.
    ENDLOOP.

    " Se buscan descripciones de aulas
    CHECK lt_resources_aux[] IS NOT INITIAL.
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
          lt_eventos         TYPE  zedu_t_eventos_aula_v3,
          ls_eventos         TYPE  zedu_eventos_aula_v3,
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
    TYPES: BEGIN OF lty_sedes,
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
*Inicio HIRS 22/12/2017 --> DCEK907312
*        SORT lt_hrt1716 BY tabnr.
        SORT lt_hrt1716 BY tabnr beguz enduz monday tuesday wednesday
                           thursday friday saturday sunday.
        DELETE ADJACENT DUPLICATES FROM lt_hrt1716
          COMPARING tabnr beguz enduz monday tuesday wednesday
                    thursday friday saturday sunday.
        SORT lt_hrt1716 BY tabnr tabseqnr.
*Fin HIRS 22/12/2017 --> DCEK907312
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

    "$. Region AD_SOPORT_SLCM_Aulas Virtuales
    IF lt_hrp1716[] IS NOT INITIAL.

      SELECT *
        FROM zedut_hrp1000v
        INTO TABLE @DATA(lt_zedut_hrp1000v)
        FOR ALL ENTRIES IN @lt_hrp1716
        WHERE plvar EQ @lt_hrp1716-plvar
          AND otype EQ @lt_hrp1716-otype
          AND objid EQ @lt_hrp1716-objid
          AND subty EQ @lt_hrp1716-subty
          AND istat EQ @lt_hrp1716-istat
          AND begda EQ @lt_hrp1716-begda
          AND endda EQ @lt_hrp1716-endda
          AND varyf EQ @lt_hrp1716-varyf
          AND seqnr EQ @lt_hrp1716-seqnr.


      " descripción aulas virtuales
      IF lt_zedut_hrp1000v IS NOT INITIAL.
        SELECT *
          APPENDING TABLE lt_hrp1000
          FROM hrp1000
          FOR ALL ENTRIES IN lt_zedut_hrp1000v
          WHERE plvar EQ '01' AND
                otype EQ 'G '  AND
                objid EQ lt_zedut_hrp1000v-room_objid.

        " consultar datos de conexión Aula VIrtual
        SELECT hp~objid , hp~linktxt , ht~url
          FROM hrp1061 AS hp
          INNER JOIN hrt1061 AS ht ON ht~tabnr = hp~tabnr
          INTO TABLE @DATA(lt_hr1061)
          FOR ALL ENTRIES IN @lt_zedut_hrp1000v
          WHERE hp~objid EQ @lt_zedut_hrp1000v-room_objid.

      ENDIF.


      SORT lt_hrp1000 BY objid.
      SORT lt_hr1061  BY objid.
      SORT lt_zedut_hrp1000v ASCENDING BY tabnr tabseqnr objid.
    ENDIF.
    "$. Endregion AD_SOPORT_SLCM_Aulas Virtuales

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

*	Begin	-->	MgM DCEK902955 descomenta modificación de DCEK902825 20/01/2017
*	Begin	-->	MgM DCEK902825 más de una franja horaria por evento 16/01/2017

      LOOP AT lt_hrt1716 INTO ls_hrt1716    "HIRS 20/12/2017 --> DCEK907283
        WHERE tabnr = ls_hrp1716-tabnr.     "HIRS 20/12/2017 --> DCEK907283


**        READ TABLE lt_hrt1716 INTO ls_hrt1716   "HIRS 20/12/2017 --> DCEK907283
**          WITH KEY tabnr = ls_hrp1716-tabnr     "HIRS 20/12/2017 --> DCEK907283
**                   BINARY SEARCH.               "HIRS 20/12/2017 --> DCEK907283
**        IF sy-subrc IS INITIAL.                 "HIRS 20/12/2017 --> DCEK907283
        ls_eventos-tabseqnr   = ls_hrt1716-tabseqnr. "HIRS 21/12/2017 --> DCEK907283
        ls_eventos-beguz      = ls_hrt1716-beguz.
        ls_eventos-enduz      = ls_hrt1716-enduz.
        ls_eventos-monday     = ls_hrt1716-monday.
        ls_eventos-tuesday    = ls_hrt1716-tuesday.
        ls_eventos-wednesday  = ls_hrt1716-wednesday.
        ls_eventos-thursday   = ls_hrt1716-thursday.
        ls_eventos-friday     = ls_hrt1716-friday.
        ls_eventos-saturday   = ls_hrt1716-saturday.
        ls_eventos-sunday     = ls_hrt1716-sunday.
        ls_eventos-room_objid = ls_hrt1716-room_objid.
**        ENDIF.                                  "HIRS 20/12/2017 --> DCEK907283

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

        "$. Region AD_SOPORTE_SLCM_Aulas Vistuale
        CLEAR: ls_eventos-room_objid_v , ls_eventos-room_name_v,
               ls_eventos-url , ls_eventos-user_url,ls_eventos-pass_url.
        READ TABLE lt_zedut_hrp1000v INTO DATA(wa_zedut_hrp1000v)
         WITH KEY tabnr     = ls_hrt1716-tabnr
                  tabseqnr  = ls_eventos-tabseqnr
                  objid     = ls_eventos-objid
         BINARY SEARCH.

        IF sy-subrc EQ 0.
          ls_eventos-room_objid_v = wa_zedut_hrp1000v-room_objid.
          READ TABLE lt_hrp1000 INTO ls_hrp1000
            WITH KEY objid = ls_eventos-room_objid_v
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_eventos-room_name_v = ls_hrp1000-stext.
          ENDIF.
        ENDIF.

        " datos conexión aula virtual
        READ TABLE lt_hr1061 INTO DATA(wa_hr1061)
          WITH KEY objid = ls_eventos-room_objid_v
          BINARY SEARCH.

        IF sy-subrc EQ 0.
          ls_eventos-url = wa_hr1061-url.
          SPLIT wa_hr1061-linktxt AT '/'
            INTO  ls_eventos-user_url
                  ls_eventos-pass_url.
        ENDIF.
        "$. Endregion AD_SOPORTE_SLCM_Aulas Vistuale

        APPEND ls_eventos TO lt_eventos.
      ENDLOOP.    "HIRS 20/12/2017 --> DCEK907283

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

      ls_eventos-plvar    = ls_hrp1000_e-plvar.
      ls_eventos-otype    = ls_hrp1000_e-otype.
      ls_eventos-objid    = ls_hrp1000_e-objid.
      ls_eventos-tabseqnr = '000000'.          "HIRS 24/04/2017 --> DCEK907442
      ls_eventos-istat    = ls_hrp1000_e-istat.
      ls_eventos-begda    = ls_hrp1000_e-begda.
      ls_eventos-endda    = ls_hrp1000_e-endda.
      ls_eventos-langu    = ls_hrp1000_e-langu.
      ls_eventos-stext    = ls_hrp1000_e-stext.
      ls_eventos-short    = ls_hrp1000_e-short.
      ls_eventos-bcont    = lv_lines.
      ls_eventos-kapz1    = ls_hrp1024-kapz1.  "HIRS 29/08/2017 --> DCEK906192
      ls_eventos-kapz2    = ls_hrp1024-kapz2.
      ls_eventos-kapz3    = ls_hrp1024-kapz3.
      ls_eventos-beguz    = ls_hrt1035-beguz.
      ls_eventos-enduz    = ls_hrt1035-enduz.

      "$. Region AD_SOPORTE_SLCM_Aulas Vistuale
      SELECT SINGLE room_objid
        FROM zedut_hrp1000v
        INTO ( ls_eventos-room_objid_v )
        WHERE tabseqnr  = ls_eventos-tabseqnr
          AND objid     = ls_eventos-objid.

      IF sy-subrc EQ 0.
        SELECT SINGLE stext
        FROM hrp1000
        INTO ( ls_eventos-room_name_v )
        WHERE plvar EQ '01' AND
              otype EQ 'G ' AND
              objid EQ ls_eventos-room_objid_v.
      ENDIF.

      " datos conexión
      SELECT SINGLE hp~objid , hp~linktxt , ht~url
       FROM hrp1061 AS hp
       INNER JOIN hrt1061 AS ht ON ht~tabnr = hp~tabnr
       INTO @DATA(ls_hr1061)
       WHERE hp~objid EQ @ls_eventos-room_objid_v.

      IF sy-subrc EQ 0.
        ls_eventos-url = ls_hr1061-url.
        SPLIT ls_hr1061-linktxt AT '/'
          INTO  ls_eventos-user_url
                ls_eventos-pass_url.
      ENDIF.
      "$. Endregion AD_SOPORTE_SLCM_Aulas Vistuale

      APPEND ls_eventos TO lt_eventos.

    ENDIF.
    et_eventos[] = lt_eventos[].

  ENDMETHOD.


  METHOD BUSCAR_PROGRAMA_EVENTO.

    DATA: ls_programa_evento TYPE zedu_programa_evento_v2,
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

* Inicio Adicionado HIRS 21/12/2017 --> DCEK907283
    "Declaraciones
    DATA:
      lr_daytxt TYPE zedu_r_daytxt_t.

    "Obtiene el rango de denominaciones de día
    CALL METHOD me->get_r_daytxt
      EXPORTING
        is_evento = is_evento
      IMPORTING
        er_daytxt = lr_daytxt.
* Fin Adicionado HIRS 21/12/2017 --> DCEK907283

    REFRESH et_programa_evento[].

    " Recupera recursos y programa de evento
    CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
      EXPORTING
        event_id       = is_evento-objid
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
    SORT lt_resources BY resbg beguz enduz. "HIRS 21.12.2017  -- Se adiciona ordenamiento BEGUZ y ENDUZ

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

      "Si el evento no es de continua
      IF is_evento-tabseqnr NE '000000'.  "HIRS 24/04/2017 --> DCEK907442
* Inicio Adicionado HIRS 21/12/2017 --> DCEK907283
        CHECK ls_schedule-daytxt IN lr_daytxt.
        CHECK ls_schedule-evdat BETWEEN is_evento-begda AND is_evento-endda.
        CHECK ls_schedule-beguz EQ is_evento-beguz.
        CHECK ls_schedule-enduz EQ is_evento-enduz.
* Fin Adicionado HIRS 21/12/2017 --> DCEK907283
      ENDIF.                              "HIRS 24/04/2017 --> DCEK907442

      READ TABLE lt_resources INTO ls_resources
        WITH KEY resbg = ls_schedule-evdat
                 beguz = ls_schedule-beguz  "HIRS 22.09.2017  -- Se adiciona filtro BEGUZ
                 enduz = ls_schedule-enduz  "HIRS 21.12.2017  -- Se adiciona filtro ENDUZ
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_programa_evento-begda = ls_resources-resbg.
        ls_programa_evento-endda = ls_resources-resed.
        ls_programa_evento-beguz = ls_resources-beguz.
        ls_programa_evento-enduz = ls_resources-enduz.
        ls_programa_evento-daytxt = ls_schedule-daytxt. "HIRS 21/12/2017 --> DCEK907283
        ls_programa_evento-room_objid = ls_resources-resid.
        ls_programa_evento-room_name = ls_resources-resxt.
      ELSE.
        ls_programa_evento-begda = ls_schedule-evdat.
        ls_programa_evento-endda = ls_schedule-evdat.
        ls_programa_evento-beguz = ls_schedule-beguz.
        ls_programa_evento-enduz = ls_schedule-enduz.   "HIRS 21/12/2017 --> DCEK907283
        ls_programa_evento-daytxt = ls_schedule-daytxt.
      ENDIF.
      APPEND ls_programa_evento TO et_programa_evento.

    ENDLOOP.

    SORT et_programa_evento[] BY begda endda beguz enduz.

  ENDMETHOD.


  METHOD GET_ROOM_NAME.
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


  METHOD GET_R_DAYTXT.

    "Declaraciones
    DATA:
      lt_day    TYPE TABLE OF t246,
      ls_day    TYPE t246,
      ls_daytxt TYPE zedu_r_daytxt.


    "Inicializa retorno
    CLEAR:
     er_daytxt.

    "Obtiene los dias de la semana
    CALL FUNCTION 'WEEKDAY_GET'
      TABLES
        weekday           = lt_day
      EXCEPTIONS
        weekday_not_found = 1
        OTHERS            = 2.

    "Ordena los registros
    SORT lt_day BY wotnr.

    "Indica que los dias son incluyentes
    ls_daytxt-sign = 'I'.
    ls_daytxt-option = 'EQ'.

    "Si la franja tiene el dia Lunes
    IF is_evento-monday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '1'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Martes
    IF is_evento-tuesday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '2'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Miercoles
    IF is_evento-wednesday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '3'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Jueves
    IF is_evento-thursday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '4'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Viernes
    IF is_evento-friday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '5'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Sabado
    IF is_evento-saturday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '6'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
    ENDIF.

    "Si la franja tiene el dia Domingo
    IF is_evento-sunday EQ abap_true.
      READ TABLE lt_day INTO ls_day
        WITH KEY wotnr = '7'
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        ls_daytxt-low = ls_day-langt.
        APPEND ls_daytxt TO er_daytxt.
      ENDIF.
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
          lt_resources_old_p TYPE  TABLE OF piqrfc_resources, "HIRS 21/12/2017 --> DCEK907283
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
          ls_pt1716_upd_tmp  TYPE  pt1716,  "HIRS 21/12/2017 --> DCEK907312
          ls_hrp1001_room    TYPE  hrp1001.

    FIELD-SYMBOLS: <fs_pt1716_upd> TYPE  pt1716. "HIRS 21/12/2017 --> DCEK907312


* Inicio Adicionado HIRS 21/12/2017 --> DCEK907283
    "Declaraciones
    DATA:
      lt_hrt1716 TYPE TABLE OF hrt1716,
      lr_daytxt  TYPE zedu_r_daytxt_t,
      ls_hrt1716 TYPE hrt1716.


    "Obtiene el rango de denominaciones de día
    CALL METHOD me->get_r_daytxt
      EXPORTING
        is_evento = is_evento_aula
      IMPORTING
        er_daytxt = lr_daytxt.
* Fin Adicionado HIRS 21/12/2017 --> DCEK907283

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

    "$. Region AD_SOPORTE_SLCM_Aulas virtuales
    " validar quer la aula ingresada no sea Virtual
    IF is_evento_aula-room_objid IS NOT INITIAL.
      IF me->aulas_no_virtuales(
          EXPORTING
            im_aulas   = VALUE #(
                            ( room_objid = is_evento_aula-room_objid ) )    " Tipo tabla para ayuda de búsqueda en ZWDC_REP_AULAS
            im_virtual =  0   " Indicador 0 = No Virtuales 1 = Virtuales
      ) IS INITIAL.

        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 082.
        ls_return-message_v1 = is_evento_aula-room_objid.
        INSERT ls_return INTO rt_errores INDEX 1.
        EXIT.
      ENDIF.
    ENDIF.

*    " validar aula virtual ingressda
*    IF is_evento_aula-room_objid_v IS NOT INITIAL.
*      IF me->aulas_no_virtuales(
*          EXPORTING
*            im_aulas   = VALUE #(
*                            ( room_objid = is_evento_aula-room_objid_v ) )    " Tipo tabla para ayuda de búsqueda en ZWDC_REP_AULAS
*            im_virtual =  4   " Indicador 0 = No Virtuales 1 = Virtuales
*      ) IS INITIAL.
*
*        CLEAR ls_return.
*        ls_return-id = ga_id_message.
*        ls_return-type = 'E'.
*        ls_return-number = 083.
*        ls_return-message_v1 = is_evento_aula-room_objid_v.
*        INSERT ls_return INTO rt_errores INDEX 1.
*        EXIT.
*      ENDIF.
*
*      " Validar disponibilidad de Aula
*      IF me->validar_dispo_aula( is_evento_aula ) EQ abap_true.
*        CLEAR ls_return.
*        ls_return-id = ga_id_message.
*        ls_return-type = 'E'.
*        ls_return-number = 084.
*        ls_return-message_v1 = is_evento_aula-room_objid_v.
*        INSERT ls_return INTO rt_errores INDEX 1.
*        EXIT.
*      ENDIF.
*    ENDIF.
    "$. Endregion AD_SOPORTE_SLCM_Aulas virtuales

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

* Inicio HIRS 21/12/2017 --> DCEK907283
    " Ordena tablas
    SORT lt_schedules_old BY evdat.
    SORT lt_resources_old BY resbg beguz enduz.

    "Crea una copia de los recursos
    lt_resources_old_p = lt_resources_old.

    "Elimina los recursos que son aulas
    DELETE lt_resources_old_p WHERE restp EQ 'G'.

    "Elimina los recursos que no son aulas
    DELETE lt_resources_old WHERE restp NE 'G'.

    "Recorre los recursos que no son aulas
    LOOP AT lt_resources_old_p INTO ls_resources_old.
      "Asigna los datos del recurso
      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
      " Agrego recursos que no son aula (Ej: Docentes)
      APPEND ls_resources TO lt_resources.
    ENDLOOP.
* Fin HIRS 21/12/2017 --> DCEK907283

    " Recorro programa del evento
    LOOP AT lt_schedules_old INTO ls_schedules_old.
      " Agrego programa del evento a tabla LT_SCHEDULE
      MOVE-CORRESPONDING ls_schedules_old TO ls_schedule.
      APPEND ls_schedule TO lt_schedule.

* Inicio HIRS 24/04/2017 --> DCEK907442
      "Si el evento es de continua
      IF is_evento_aula-tabseqnr EQ '000000'.
        "Continua solo si el evento tiene un aula asignada
        CHECK is_evento_aula-room_objid IS NOT INITIAL.

        "Asigna los datos del recurso aula nuevo
        ls_resources-restp = 'G'. " Aula
        ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
        ls_resources-resht = is_evento_aula-room_name.
        ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
        ls_resources-retxt = ''.
        ls_resources-reknz = ls_hrp1001_room-sclas.
        ls_resources-resbg = ls_schedules_old-evdat.
        ls_resources-resed = ls_schedules_old-evdat.
        ls_resources-beguz = ls_schedules_old-beguz.
        ls_resources-enduz = ls_schedules_old-enduz.

        APPEND ls_resources TO lt_resources.

        "Si el evento no es de continua
      ELSE.
* Fin HIRS 24/04/2017 --> DCEK907442
* Inicio HIRS 21/12/2017 --> DCEK907283
        "Si el programa pertenece a la franja que se esta procesando
        IF ls_schedules_old-daytxt IN lr_daytxt            AND
           ls_schedules_old-beguz  EQ is_evento_aula-beguz AND
           ls_schedules_old-enduz  EQ is_evento_aula-enduz AND
           ls_schedules_old-evdat  BETWEEN is_evento_aula-begda AND is_evento_aula-endda.

          "Continua solo si el evento tiene un aula asignada
          CHECK is_evento_aula-room_objid IS NOT INITIAL.

          "Asigna los datos del recurso aula nuevo
          ls_resources-restp = 'G'. " Aula
          ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
          ls_resources-resht = is_evento_aula-room_name.
          ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
          ls_resources-retxt = ''.
          ls_resources-reknz = ls_hrp1001_room-sclas.
          ls_resources-resbg = ls_schedules_old-evdat.
          ls_resources-resed = ls_schedules_old-evdat.
          ls_resources-beguz = ls_schedules_old-beguz.
          ls_resources-enduz = ls_schedules_old-enduz.

          APPEND ls_resources TO lt_resources.

          "Si el programa no pertenece a la franja que se esta procesando
        ELSE.
          "Obtiene el recurso aula actual del programa
          READ TABLE lt_resources_old INTO ls_resources_old
            WITH KEY resbg = ls_schedules_old-evdat
                     beguz = ls_schedules_old-beguz
                     enduz = ls_schedules_old-enduz
            BINARY SEARCH.

          "Si encuentra el recurso
          IF sy-subrc EQ 0.
            "Asigna los datos del recurso
            MOVE-CORRESPONDING ls_resources_old TO ls_resources.
            " Agrego el recurso aula actual tal cual esta
            APPEND ls_resources TO lt_resources.
          ENDIF.
        ENDIF.
* Fin HIRS 21/12/2017 --> DCEK907283
      ENDIF. "HIRS 24/04/2017 --> DCEK907442
    ENDLOOP.

* Inicio HIRS 21/12/2017 --> DCEK907283
**    " Recorro recursos del evento
**    LOOP AT lt_resources_old INTO ls_resources_old.
**      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
**      " Agrego recursos que no son aula (Ej: Docentes)
**      IF ls_resources_old-restp NE 'G'. " Recursos que no son Aula
**        APPEND ls_resources TO lt_resources.
**      ENDIF.
**    ENDLOOP.
**
**    IF is_evento_aula-room_objid IS NOT INITIAL.
**      " Agrega recursos Aula según programa
**      LOOP AT lt_schedule INTO ls_schedule.
**        ls_resources-restp = 'G'. " Aula
**        ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
**        ls_resources-resht = is_evento_aula-room_name.
**        ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
**        ls_resources-retxt = ''.
**        ls_resources-reknz = ls_hrp1001_room-sclas.
**        ls_resources-resbg = ls_schedule-evdat.
**        ls_resources-resed = ls_schedule-evdat.
**        ls_resources-beguz = ls_schedule-beguz.
**        ls_resources-enduz = ls_schedule-enduz.
**
**        APPEND ls_resources TO lt_resources.
**
**      ENDLOOP.
**    ENDIF.
* Fin HIRS 21/12/2017 --> DCEK907283

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
**          INTO CORRESPONDING FIELDS OF TABLE lt_pt1716_upd  "HIRS 21/12/2017 --> DCEK907283
          INTO TABLE lt_hrt1716                               "HIRS 21/12/2017 --> DCEK907283
          FOR ALL ENTRIES IN lt_infty_1716_upd
          WHERE tabnr EQ lt_infty_1716_upd-tabnr.
      ENDIF.

* Inicio HIRS 21/12/2017 --> DCEK907283
**      " Se pasa la nueva aula y el nuevo tipo de aula
**      LOOP AT lt_pt1716_upd INTO ls_pt1716_upd.
**        ls_pt1716_upd-room_objid = is_evento_aula-room_objid.
**        ls_pt1716_upd-room_retid = ls_hrp1001_room-sobid.
**        MODIFY  lt_pt1716_upd FROM ls_pt1716_upd INDEX sy-tabix.
**      ENDLOOP.

      "Recorre los registros de aulas
      LOOP AT lt_hrt1716 INTO ls_hrt1716.
        "Asigna los datos del aula
        MOVE-CORRESPONDING ls_hrt1716 TO ls_pt1716_upd.

        "Si el registro pertenece al programa que se esta modificando
        IF ls_hrt1716-tabseqnr EQ is_evento_aula-tabseqnr.
          ls_pt1716_upd-room_objid = is_evento_aula-room_objid.
          ls_pt1716_upd-room_retid = ls_hrp1001_room-sobid.

          "Crea una copia del registro modificado
          ls_pt1716_upd_tmp = ls_pt1716_upd.
        ENDIF.

        "Crea el registro de modificacion
        APPEND ls_pt1716_upd TO lt_pt1716_upd.
      ENDLOOP.

      "Recorre los registros de aula, reasignando el aula a
      "los recursos que coincidan en la programacion
      LOOP AT lt_pt1716_upd ASSIGNING <fs_pt1716_upd>.
        IF <fs_pt1716_upd>-beguz     EQ ls_pt1716_upd_tmp-beguz     AND
           <fs_pt1716_upd>-enduz     EQ ls_pt1716_upd_tmp-enduz     AND
           <fs_pt1716_upd>-monday    EQ ls_pt1716_upd_tmp-monday    AND
           <fs_pt1716_upd>-tuesday   EQ ls_pt1716_upd_tmp-tuesday   AND
           <fs_pt1716_upd>-wednesday EQ ls_pt1716_upd_tmp-wednesday AND
           <fs_pt1716_upd>-thursday  EQ ls_pt1716_upd_tmp-thursday  AND
           <fs_pt1716_upd>-friday    EQ ls_pt1716_upd_tmp-friday    AND
           <fs_pt1716_upd>-saturday  EQ ls_pt1716_upd_tmp-saturday  AND
           <fs_pt1716_upd>-sunday    EQ ls_pt1716_upd_tmp-sunday.

          "Reasigna el aula, ya que es la misma franja pero con otro docente
          <fs_pt1716_upd>-room_objid = is_evento_aula-room_objid.
          <fs_pt1716_upd>-room_retid = ls_hrp1001_room-sobid.
        ENDIF.
      ENDLOOP.
* Fin HIRS 21/12/2017 --> DCEK907283

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

*        "$. Region AD_SOPORTE_SLCM_Aulas Virtuales
*        " modificar aula virtual
*        me->modificar_evento_virtual(
*          EXPORTING
*            im_t_piq_p1716_t = lt_infty_1716_upd    " CM: Tipo de tabla para infotipo 1716 (descripción proceso)
*            im_hrt1716       = ls_hrt1716    " Parte de tabla infotipo 1716
*            im_evento        = is_evento_aula    " Estructura eventos para modifcación de aulas
*        ).
*        "$. Endregion AD_SOPORTE_SLCM_Aulas Virtuales

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


  METHOD modificar_evento_v.
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
          lt_resources_old_p TYPE  TABLE OF piqrfc_resources, "HIRS 21/12/2017 --> DCEK907283
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
          ls_pt1716_upd_tmp  TYPE  pt1716,  "HIRS 21/12/2017 --> DCEK907312
          ls_hrp1001_room    TYPE  hrp1001.

    FIELD-SYMBOLS: <fs_pt1716_upd> TYPE  pt1716. "HIRS 21/12/2017 --> DCEK907312


* Inicio Adicionado HIRS 21/12/2017 --> DCEK907283
    "Declaraciones
    DATA:
      lt_hrt1716 TYPE TABLE OF hrt1716,
      lr_daytxt  TYPE zedu_r_daytxt_t,
      ls_hrt1716 TYPE hrt1716.


    "Obtiene el rango de denominaciones de día
    CALL METHOD me->get_r_daytxt
      EXPORTING
        is_evento = is_evento_aula
      IMPORTING
        er_daytxt = lr_daytxt.
* Fin Adicionado HIRS 21/12/2017 --> DCEK907283

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

    "$. Region AD_SOPORTE_SLCM_Aulas virtuales
    " validar aula virtual ingressda
    IF is_evento_aula-room_objid_v IS NOT INITIAL.
      IF me->aulas_no_virtuales(
          EXPORTING
            im_aulas   = VALUE #(
                            ( room_objid = is_evento_aula-room_objid_v ) )    " Tipo tabla para ayuda de búsqueda en ZWDC_REP_AULAS
            im_virtual =  4   " Indicador 0 = No Virtuales 1 = Virtuales
      ) IS INITIAL.

        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 083.
        ls_return-message_v1 = is_evento_aula-room_objid_v.
        INSERT ls_return INTO rt_errores INDEX 1.
        EXIT.
      ENDIF.

      " Validar disponibilidad de Aula
      IF me->validar_dispo_aula( is_evento_aula ) EQ abap_true.
        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 084.
        ls_return-message_v1 = is_evento_aula-room_objid_v.
        INSERT ls_return INTO rt_errores INDEX 1.
        EXIT.
      ENDIF.
    ENDIF.
    "$. Endregion AD_SOPORTE_SLCM_Aulas virtuales

    " Recupera recursos y programa de evento
    CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
      EXPORTING
        event_id       = is_evento_aula-objid
        event_otype    = is_evento_aula-otype
        plvar          = is_evento_aula-plvar
        begda          = is_evento_aula-begda
        endda          = is_evento_aula-endda
        istat          = is_evento_aula-istat
        read_resources = 'X'
        read_schedules = 'X'
      TABLES
        resources      = lt_resources_old
        schedules      = lt_schedules_old.

* Inicio HIRS 21/12/2017 --> DCEK907283
    " Ordena tablas
    SORT lt_schedules_old BY evdat.
    SORT lt_resources_old BY resbg beguz enduz.

    "Crea una copia de los recursos
    lt_resources_old_p = lt_resources_old.

    "Elimina los recursos que son aulas
    DELETE lt_resources_old_p WHERE restp EQ 'G'.

    "Elimina los recursos que no son aulas
    DELETE lt_resources_old WHERE restp NE 'G'.

    "Recorre los recursos que no son aulas
    LOOP AT lt_resources_old_p INTO ls_resources_old.
      "Asigna los datos del recurso
      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
      " Agrego recursos que no son aula (Ej: Docentes)
      APPEND ls_resources TO lt_resources.
    ENDLOOP.
* Fin HIRS 21/12/2017 --> DCEK907283

    " Recorro programa del evento
    LOOP AT lt_schedules_old INTO ls_schedules_old.
      " Agrego programa del evento a tabla LT_SCHEDULE
      MOVE-CORRESPONDING ls_schedules_old TO ls_schedule.
      APPEND ls_schedule TO lt_schedule.

* Inicio HIRS 24/04/2017 --> DCEK907442
      "Si el evento es de continua
      IF is_evento_aula-tabseqnr EQ '000000'.
        "Continua solo si el evento tiene un aula asignada
        CHECK is_evento_aula-room_objid IS NOT INITIAL.

        "Asigna los datos del recurso aula nuevo
        ls_resources-restp = 'G'. " Aula
        ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
        ls_resources-resht = is_evento_aula-room_name.
        ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
        ls_resources-retxt = ''.
        ls_resources-reknz = ls_hrp1001_room-sclas.
        ls_resources-resbg = ls_schedules_old-evdat.
        ls_resources-resed = ls_schedules_old-evdat.
        ls_resources-beguz = ls_schedules_old-beguz.
        ls_resources-enduz = ls_schedules_old-enduz.

        APPEND ls_resources TO lt_resources.

        "Si el evento no es de continua
      ELSE.
* Fin HIRS 24/04/2017 --> DCEK907442
* Inicio HIRS 21/12/2017 --> DCEK907283
        "Si el programa pertenece a la franja que se esta procesando
        IF ls_schedules_old-daytxt IN lr_daytxt            AND
           ls_schedules_old-beguz  EQ is_evento_aula-beguz AND
           ls_schedules_old-enduz  EQ is_evento_aula-enduz AND
           ls_schedules_old-evdat  BETWEEN is_evento_aula-begda AND is_evento_aula-endda.

          "Continua solo si el evento tiene un aula asignada
          CHECK is_evento_aula-room_objid IS NOT INITIAL.

          "Asigna los datos del recurso aula nuevo
          ls_resources-restp = 'G'. " Aula
          ls_resources-resid = is_evento_aula-room_objid. " Nueva aula
          ls_resources-resht = is_evento_aula-room_name.
          ls_resources-retid = ls_hrp1001_room-sobid.     " Tipo de aula nueva
          ls_resources-retxt = ''.
          ls_resources-reknz = ls_hrp1001_room-sclas.
          ls_resources-resbg = ls_schedules_old-evdat.
          ls_resources-resed = ls_schedules_old-evdat.
          ls_resources-beguz = ls_schedules_old-beguz.
          ls_resources-enduz = ls_schedules_old-enduz.

          APPEND ls_resources TO lt_resources.

          "Si el programa no pertenece a la franja que se esta procesando
        ELSE.
          "Obtiene el recurso aula actual del programa
          READ TABLE lt_resources_old INTO ls_resources_old
            WITH KEY resbg = ls_schedules_old-evdat
                     beguz = ls_schedules_old-beguz
                     enduz = ls_schedules_old-enduz
            BINARY SEARCH.

          "Si encuentra el recurso
          IF sy-subrc EQ 0.
            "Asigna los datos del recurso
            MOVE-CORRESPONDING ls_resources_old TO ls_resources.
            " Agrego el recurso aula actual tal cual esta
            APPEND ls_resources TO lt_resources.
          ENDIF.
        ENDIF.
* Fin HIRS 21/12/2017 --> DCEK907283
      ENDIF. "HIRS 24/04/2017 --> DCEK907442
    ENDLOOP.

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
**          INTO CORRESPONDING FIELDS OF TABLE lt_pt1716_upd  "HIRS 21/12/2017 --> DCEK907283
        INTO TABLE lt_hrt1716                               "HIRS 21/12/2017 --> DCEK907283
        FOR ALL ENTRIES IN lt_infty_1716_upd
        WHERE tabnr EQ lt_infty_1716_upd-tabnr.
    ENDIF.

    "Recorre los registros de aulas
    LOOP AT lt_hrt1716 INTO ls_hrt1716.
      "Asigna los datos del aula
      MOVE-CORRESPONDING ls_hrt1716 TO ls_pt1716_upd.

      "Si el registro pertenece al programa que se esta modificando
      IF ls_hrt1716-tabseqnr EQ is_evento_aula-tabseqnr.
        ls_pt1716_upd-room_objid = is_evento_aula-room_objid.
        ls_pt1716_upd-room_retid = ls_hrp1001_room-sobid.

        "Crea una copia del registro modificado
        ls_pt1716_upd_tmp = ls_pt1716_upd.
      ENDIF.

      "Crea el registro de modificacion
      APPEND ls_pt1716_upd TO lt_pt1716_upd.
    ENDLOOP.

    "Recorre los registros de aula, reasignando el aula a
    "los recursos que coincidan en la programacion
    LOOP AT lt_pt1716_upd ASSIGNING <fs_pt1716_upd>.
      IF <fs_pt1716_upd>-beguz     EQ ls_pt1716_upd_tmp-beguz     AND
         <fs_pt1716_upd>-enduz     EQ ls_pt1716_upd_tmp-enduz     AND
         <fs_pt1716_upd>-monday    EQ ls_pt1716_upd_tmp-monday    AND
         <fs_pt1716_upd>-tuesday   EQ ls_pt1716_upd_tmp-tuesday   AND
         <fs_pt1716_upd>-wednesday EQ ls_pt1716_upd_tmp-wednesday AND
         <fs_pt1716_upd>-thursday  EQ ls_pt1716_upd_tmp-thursday  AND
         <fs_pt1716_upd>-friday    EQ ls_pt1716_upd_tmp-friday    AND
         <fs_pt1716_upd>-saturday  EQ ls_pt1716_upd_tmp-saturday  AND
         <fs_pt1716_upd>-sunday    EQ ls_pt1716_upd_tmp-sunday.

        "Reasigna el aula, ya que es la misma franja pero con otro docente
        <fs_pt1716_upd>-room_objid = is_evento_aula-room_objid.
        <fs_pt1716_upd>-room_retid = ls_hrp1001_room-sobid.
      ENDIF.
    ENDLOOP.


    "$. Region AD_SOPORTE_SLCM_Aulas Virtuales
    " modificar aula virtual
    me->modificar_evento_virtual(
      EXPORTING
        im_t_piq_p1716_t = lt_infty_1716_upd    " CM: Tipo de tabla para infotipo 1716 (descripción proceso)
        im_hrt1716       = ls_hrt1716    " Parte de tabla infotipo 1716
        im_evento        = is_evento_aula    " Estructura eventos para modifcación de aulas
    ).

    IF sy-subrc EQ 0.
      " Programa e Infotipo modificados exitosamente
      CLEAR ls_return.
      ls_return-id = ga_id_message.
      ls_return-type = 'S'.
      ls_return-number = 014.
      ls_return-message_v1 = is_evento_aula-objid.
      INSERT ls_return INTO lt_return INDEX 1.
    ENDIF.
    "$. Endregion AD_SOPORTE_SLCM_Aulas Virtuales

    rt_errores[] = lt_return[].


  ENDMETHOD.


  METHOD modificar_evento_virtual.

    IF im_t_piq_p1716_t[] IS NOT INITIAL.
      DATA(ls_zedut_hrp1000v) = CORRESPONDING zedut_hrp1000v(
                                     VALUE #( im_t_piq_p1716_t[ 1 ] OPTIONAL )  ).
      ls_zedut_hrp1000v-tabnr = im_hrt1716-tabnr.

    ELSE.

      SELECT SINGLE tabnr
        FROM hrp1035
        INTO ( ls_zedut_hrp1000v-tabnr )
        WHERE objid EQ im_evento-objid.

    ENDIF.

    MOVE-CORRESPONDING im_evento TO ls_zedut_hrp1000v.
    ls_zedut_hrp1000v-tabseqnr = im_evento-tabseqnr.
    ls_zedut_hrp1000v-room_objid = im_evento-room_objid_v.


    IF ls_zedut_hrp1000v-room_objid IS NOT INITIAL.
      MODIFY zedut_hrp1000v FROM ls_zedut_hrp1000v.
    ELSE.
      DELETE FROM zedut_hrp1000v
        WHERE objid       = ls_zedut_hrp1000v-objid
          AND tabnr       = ls_zedut_hrp1000v-tabnr
          AND tabseqnr    = ls_zedut_hrp1000v-tabseqnr.
    ENDIF.

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
          lt_resources_old_p TYPE  TABLE OF piqrfc_resources, "HIRS 21/12/2017 --> DCEK907283
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
          ls_programa        TYPE  zedu_programa_evento_v2,
          lt_aulas           TYPE TABLE OF zedu_aula,
          lt_programa        TYPE zedu_t_programa_evento_v2,
          lv_error           TYPE char1.

* Inicio Adicionado HIRS 21/12/2017 --> DCEK907283
    "Declaraciones
    DATA:
      lr_daytxt  TYPE zedu_r_daytxt_t.

    "Obtiene el rango de denominaciones de día
    CALL METHOD me->get_r_daytxt
      EXPORTING
        is_evento = is_evento_aula
      IMPORTING
        er_daytxt = lr_daytxt.
* Fin Adicionado HIRS 21/12/2017 --> DCEK907283

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

    "$. Region AD_SOPORTE_SLCM_Aulas virtuales
    " validar quer la aula ingresada no sea Virtual
    DATA(lt_aulas_aux) = me->aulas_no_virtuales(
                      EXPORTING
                        im_aulas   = VALUE #(
                                       FOR wa_prog IN it_programa
                                       ( room_objid = wa_prog-room_objid ) )
                        im_virtual =  0   " Indicador 0 = No Virtuales 1 = Virtuales
                      ).
    LOOP AT it_programa INTO DATA(wa_programa).
      CHECK wa_programa-room_objid IS NOT INITIAL.
      IF NOT line_exists( lt_aulas_aux[ room_objid = wa_programa-room_objid ] ).

        CLEAR ls_return.
        ls_return-id = ga_id_message.
        ls_return-type = 'E'.
        ls_return-number = 082.
        ls_return-message_v1 = wa_programa-room_objid.
        INSERT ls_return INTO rt_errores INDEX 1.
      ENDIF.
    ENDLOOP.
    CHECK rt_errores[] IS INITIAL.
    "$. Endregion AD_SOPORTE_SLCM_Aulas virtuales

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

* Inicio HIRS 21/12/2017 --> DCEK907283
    " Ordena tablas
    SORT lt_schedules_old BY evdat.
    SORT lt_resources_old BY resbg beguz enduz.

    "Crea una copia de los recursos
    lt_resources_old_p = lt_resources_old.

    "Elimina los recursos que son aulas
    DELETE lt_resources_old_p WHERE restp EQ 'G'.

    "Elimina los recursos que no son aulas
    DELETE lt_resources_old WHERE restp NE 'G'.

    "Recorre los recursos que no son aulas
    LOOP AT lt_resources_old_p INTO ls_resources_old.
      "Asigna los datos del recurso
      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
      " Agrego recursos que no son aula (Ej: Docentes)
      APPEND ls_resources TO lt_resources.
    ENDLOOP.
* Fin HIRS 21/12/2017 --> DCEK907283

    " Recorro programa del evento
    LOOP AT lt_schedules_old INTO ls_schedules_old.
      " Agrego programa del evento a tabla LT_SCHEDULE
      MOVE-CORRESPONDING ls_schedules_old TO ls_schedule.
      APPEND ls_schedule TO lt_schedule.

      "Solo hace la siguiente parte si el evento no es de continua
      CHECK is_evento_aula-tabseqnr NE '000000'.  "HIRS 24/04/2017 --> DCEK907442

* Inicio HIRS 21/12/2017 --> DCEK907283
      "Si el programa pertenece a la franja que se esta procesando
      IF ls_schedules_old-daytxt IN lr_daytxt            AND
         ls_schedules_old-beguz  EQ is_evento_aula-beguz AND
         ls_schedules_old-enduz  EQ is_evento_aula-enduz AND
         ls_schedules_old-evdat  BETWEEN is_evento_aula-begda AND is_evento_aula-endda.

        "Omite la creacion del registro ya que crea a partir del parametro IT_PROGRAMA
        CONTINUE.

        "Si el programa no pertenece a la franja que se esta procesando
      ELSE.
        "Obtiene el recurso aula actual del programa
        READ TABLE lt_resources_old INTO ls_resources_old
          WITH KEY resbg = ls_schedule-evdat
                   beguz = ls_schedule-beguz
                   enduz = ls_schedule-enduz
          BINARY SEARCH.

        "Si encuentra el recurso
        IF sy-subrc EQ 0.
          "Asigna los datos del recurso
          MOVE-CORRESPONDING ls_resources_old TO ls_resources.
          " Agrego el recurso aula actual tal cual esta
          APPEND ls_resources TO lt_resources.
        ENDIF.
      ENDIF.
* Fin HIRS 21/12/2017 --> DCEK907283
    ENDLOOP.

* Inicio HIRS 21/12/2017 --> DCEK907283
**    " Recorro recursos del evento
**    LOOP AT lt_resources_old INTO ls_resources_old.
**      MOVE-CORRESPONDING ls_resources_old TO ls_resources.
**      " Agrego recursos que no son aula (Ej: Docentes)
**      IF ls_resources_old-restp NE 'G'. " Recursos que no son Aula
**        APPEND ls_resources TO lt_resources.
**      ENDIF.
**    ENDLOOP.
* Fin HIRS 21/12/2017 --> DCEK907283

    " Agrega recursos Aula según programa
    LOOP AT it_programa INTO ls_programa.
      IF ls_programa-room_objid IS INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR ls_resources. "HIRS 21/12/2017 --> DCEK907283

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


  METHOD validar_dispo_aula.

    TYPES: lty_t_piqrfc_schedules TYPE TABLE OF piqrfc_schedules WITH EMPTY KEY.

    DATA: lt_hrt1716    TYPE gty_t_hrt1716,
          lt_hrp1716    TYPE gty_t_hrp1716,

          lt_schedule_e TYPE lty_t_piqrfc_schedules,
          lt_schedule   TYPE lty_t_piqrfc_schedules.


    CHECK im_evento-room_objid_v IS NOT INITIAL.
    re_subrc = abap_false.

    " extraer eventos que tienen el aula
    SELECT *
      FROM zedut_hrp1000v
      INTO TABLE @DATA(lt_zedut_hrp1000v)
      WHERE room_objid EQ @im_evento-room_objid_v.

    IF im_check_beguz IS NOT INITIAL.
      DELETE lt_zedut_hrp1000v WHERE objid    EQ im_evento-objid
                                AND  tabseqnr EQ im_evento-tabseqnr.
    ENDIF.

    CHECK lt_zedut_hrp1000v IS NOT INITIAL.


    " extraer horarios de eventos con la misma aula virtual
    SELECT *
      FROM hrt1716
      INTO TABLE @lt_hrt1716
      FOR ALL ENTRIES IN @lt_zedut_hrp1000v
      WHERE tabnr     EQ @lt_zedut_hrp1000v-tabnr
        AND tabseqnr  EQ @lt_zedut_hrp1000v-tabseqnr.

    IF sy-subrc EQ 0.
      SELECT *
      FROM hrp1716
      INTO TABLE @lt_hrp1716
      FOR ALL ENTRIES IN @lt_hrt1716
      WHERE tabnr EQ @lt_hrt1716-tabnr.
    ENDIF.

    " DATOS INFORMACIÓN CONTINUA AULAS ASIGNADAS
    SELECT  hp~plvar,hp~otype,hp~objid,hp~subty,hp~istat,
            hp~begda,hp~endda,hp~varyf,hp~seqnr,ht~tabnr,
            ht~tabseqnr,ht~evdat,ht~beguz,ht~enduz
      FROM hrp1035 AS hp
      INNER JOIN hrt1035 AS ht ON ht~tabnr EQ hp~tabnr
      INTO TABLE @DATA(lt_h1035)
      FOR ALL ENTRIES IN @lt_zedut_hrp1000v
      WHERE ht~tabnr     EQ @lt_zedut_hrp1000v-tabnr.

    LOOP AT lt_h1035 INTO DATA(wa_h1035)
      GROUP BY ( tabnr = wa_h1035-tabnr )
      ASCENDING
      ASSIGNING FIELD-SYMBOL(<fs_head>).

      LOOP AT GROUP <fs_head> ASSIGNING FIELD-SYMBOL(<fs_items>)
        GROUP BY (  beguz = <fs_items>-beguz
                    enduz = <fs_items>-enduz   )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<fs_head_i>).

        DATA(ls_hrt1716) = VALUE hrt1716( ).
        LOOP AT GROUP <fs_head_i> ASSIGNING FIELD-SYMBOL(<fs_items_i>).

          DATA(lv_day) = VALUE cind( ).
          DATA(lv_tabix_h35) = CONV sy-tabix( 5 ).
          CALL FUNCTION 'DATE_COMPUTE_DAY'
            EXPORTING
              date = <fs_items_i>-evdat
            IMPORTING
              day  = lv_day.

          lv_tabix_h35 = lv_tabix_h35 + lv_day.

          ASSIGN COMPONENT lv_tabix_h35 OF STRUCTURE ls_hrt1716
            TO FIELD-SYMBOL(<fs_field_35>).

          CHECK <fs_field_35> IS ASSIGNED.
          <fs_field_35> = 'X'.
        ENDLOOP.

        ls_hrt1716-tabnr = <fs_items_i>-tabnr.
        ls_hrt1716-beguz = <fs_items_i>-beguz.
        ls_hrt1716-enduz = <fs_items_i>-enduz.
        APPEND ls_hrt1716 TO lt_hrt1716.
      ENDLOOP.

      APPEND VALUE #(
                objid = <fs_items_i>-objid
                begda = <fs_items_i>-begda
                endda = <fs_items_i>-endda
                tabnr = <fs_items_i>-tabnr
                 ) TO lt_hrp1716.
    ENDLOOP.

    " DATOS INFORMACIÓN CONTINUA AULAS EVENTO SI APLICA
    SELECT   hp~plvar,hp~otype,hp~objid,hp~subty,hp~istat,
             hp~begda,hp~endda,hp~varyf,hp~seqnr,ht~tabnr,
             ht~tabseqnr,ht~evdat,ht~beguz,ht~enduz
     FROM hrp1035 AS hp
     INNER JOIN hrt1035 AS ht ON ht~tabnr EQ hp~tabnr
     INTO TABLE @DATA(lt_h1035_evnt)
     WHERE hp~objid   EQ @im_evento-objid.

    IF sy-subrc EQ 0.
      DATA(lv_cont) = abap_true.
    ENDIF.

    SORT lt_hrt1716 ASCENDING BY tabnr tabseqnr.
    SORT lt_hrp1716 ASCENDING BY tabnr.
    SORT lt_h1035   ASCENDING BY tabnr.

    DATA(lv_continua) = abap_false.

    "$. Region 11.12.2021
    " ACTUALIZAR FECHA DE EVENTO CON FRANJA EXACTA
    DATA(ls_evento) = im_evento.
    SELECT SINGLE *
      FROM hrp1716
      INTO @DATA(ls_hrp1716_e)
      WHERE objid EQ @im_evento-objid.

    IF sy-subrc EQ 0.
      SELECT SINGLE *
        FROM hrt1716
        INTO @DATA(ls_hrt1716_e)
        WHERE tabnr     EQ @ls_hrp1716_e-tabnr
          AND tabseqnr  EQ @im_evento-tabseqnr.

      IF sy-subrc EQ 0.
        ls_evento-begda = ls_hrt1716_e-schedelebeg.
        ls_evento-endda = ls_hrt1716_e-schedeleend.
      ENDIF.
    ELSE.
      lv_continua = abap_true.
    ENDIF.
    "$. Endregion 11.12.2021

    IF lv_cont EQ abap_true.

      "$. Region traer días por evento
      LOOP AT lt_hrt1716 INTO DATA(wa_hrt1716).
        " Días asigandos
        DATA(lr_daytxt) = VALUE zedu_r_daytxt_t( ).

        "Obtiene el rango de denominaciones de día
        CALL METHOD me->get_r_daytxt
          EXPORTING
            is_evento = CORRESPONDING #( wa_hrt1716 )
          IMPORTING
            er_daytxt = lr_daytxt.

        " Días del evento
        READ TABLE lt_hrp1716 INTO DATA(wa_hrp1716)
          WITH KEY tabnr = wa_hrt1716-tabnr
          BINARY SEARCH.

        DATA(lv_hrobjid) = CONV hrobjid( wa_hrp1716-objid ).
        DATA(lt_schedule_a) = VALUE lty_t_piqrfc_schedules( ).
        CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
          EXPORTING
            event_id       = lv_hrobjid
            event_otype    = 'E'
            plvar          = '01'
            read_schedules = 'X'
          TABLES
            schedules      = lt_schedule_a.

        LOOP AT lt_schedule_a INTO DATA(wa_schedule_a).
          CHECK wa_schedule_a-daytxt IN lr_daytxt.
          CHECK wa_schedule_a-evdat BETWEEN wa_hrp1716-begda AND wa_hrp1716-endda.
          CHECK wa_schedule_a-beguz EQ wa_hrt1716-beguz.
          CHECK wa_schedule_a-enduz EQ wa_hrt1716-enduz.

          APPEND wa_schedule_a TO lt_schedule.
        ENDLOOP.
      ENDLOOP.

      "$. Region Días del evento
      " Días asigandos
      lr_daytxt = VALUE zedu_r_daytxt_t( ).

      "Obtiene el rango de denominaciones de día
      CALL METHOD me->get_r_daytxt
        EXPORTING
          is_evento = im_evento
        IMPORTING
          er_daytxt = lr_daytxt.

      lt_schedule_a = VALUE lty_t_piqrfc_schedules( ).
      lv_hrobjid    = CONV hrobjid( im_evento-objid ).
      CALL FUNCTION 'HRIQ_RFC_GET_EVENT_DETAILS'
        EXPORTING
          event_id       = lv_hrobjid
          event_otype    = 'E'
          plvar          = '01'
          read_schedules = 'X'
        TABLES
          schedules      = lt_schedule_a.

      LOOP AT lt_schedule_a INTO wa_schedule_a.
        CHECK wa_schedule_a-daytxt IN lr_daytxt.
        CHECK wa_schedule_a-evdat BETWEEN im_evento-begda AND im_evento-endda.

        IF im_check_beguz IS NOT INITIAL.
          IF im_evento-beguz IS NOT INITIAL.
            CHECK wa_schedule_a-beguz EQ im_evento-beguz.
            CHECK wa_schedule_a-enduz EQ im_evento-enduz.
          ENDIF.
        ELSE.
          IF lv_continua EQ abap_false.
            CHECK wa_schedule_a-beguz EQ im_evento-beguz.
            CHECK wa_schedule_a-enduz EQ im_evento-enduz.
          ENDIF.
        ENDIF.


        APPEND wa_schedule_a TO lt_schedule_e.
      ENDLOOP.
      "$. Endregion Días del evento

      " VALIDAR SI SE CRUZAN DÍAS
      SORT lt_schedule ASCENDING BY evdat.
      SORT lt_schedule_e ASCENDING BY evdat.
      LOOP AT lt_schedule_e INTO DATA(wa_schedule_e).
        DATA(lv_val) = VALUE abap_bool( ).
        LOOP AT lt_schedule INTO DATA(wa_schedule).
          " VALIDAR DÍA
          IF wa_schedule_e-evdat NE wa_schedule-evdat.
            CONTINUE.
          ENDIF.

          " Validar hora
          IF ( wa_schedule_e-beguz > wa_schedule-beguz
                AND wa_schedule_e-beguz < wa_schedule-enduz )
            OR ( wa_schedule_e-beguz <= wa_schedule-beguz
                 AND wa_schedule_e-enduz > wa_schedule-beguz ).
            lv_val = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_val EQ abap_true.
          re_subrc = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.


      "$. Endregion traer días por evento

*      LOOP AT lt_h1035_evnt INTO DATA(wa_h1035_evnt)
*        GROUP BY (  beguz = wa_h1035_evnt-beguz
*                    enduz = wa_h1035_evnt-enduz   )
*        ASCENDING
*        ASSIGNING FIELD-SYMBOL(<fs_head_e>).
*
**        DATA(ls_evento) = im_evento.
*
*        re_subrc = me->validar_dispo_indv(
*                   EXPORTING
*                     im_t_hrp1716 = lt_hrp1716     " CM: Tipo de tabla para infotipo 1716 (descripción proceso)
*                     im_t_hrt1716 = lt_hrt1716     " Parte de tabla infotipo 1716
*                     im_evento    = ls_evento    " Estructura eventos para modifcación de aulas
*                 ).
*
*        IF re_subrc EQ abap_true.
*          EXIT.
*        ENDIF.
*
*      ENDLOOP.

    ELSE.
      re_subrc = me->validar_dispo_indv(
                    EXPORTING
                      im_t_hrp1716 = lt_hrp1716    " CM: Tipo de tabla para infotipo 1716 (descripción proceso)
                      im_t_hrt1716 = lt_hrt1716    " Parte de tabla infotipo 1716
                      im_evento    = im_evento  " Estructura eventos para modifcación de aulas
                  ).

    ENDIF.

*    LOOP AT lt_hrt1716 INTO DATA(wa_hrt1716).
*
*      READ TABLE lt_hrp1716 INTO DATA(wa_hrp1716)
*        WITH KEY tabnr = wa_hrt1716-tabnr
*        BINARY SEARCH.
*
*      DATA(lv_val) = abap_false.
*      " validar fecha de inicio y fin
*      IF ( im_evento-begda >= wa_hrp1716-begda
*        AND im_evento-begda <= wa_hrp1716-endda )
*       OR ( im_evento-begda <= wa_hrp1716-begda ).
*        lv_val = abap_true.
*      ENDIF.
*
*      CHECK lv_val EQ abap_true.
*
*      " Validar si se cruzan horas
*      lv_val = abap_false.
*      IF ( im_evento-beguz > wa_hrt1716-beguz
*        AND im_evento-beguz < wa_hrt1716-enduz )
*       OR ( im_evento-beguz <= wa_hrt1716-beguz
*         AND im_evento-enduz > wa_hrt1716-beguz ).
*        lv_val = abap_true.
*      ENDIF.
*
*      CHECK lv_val EQ abap_true.
*
*      " validar si se cruzan días
*      DATA(lv_tabix_s) = CONV sy-tabix( 19 ).
*      DATA(lv_tabix_h) = CONV sy-tabix( 6 ).
*      DO 7 TIMES.
*        DATA(lv_cruza) = abap_false.
*        " validar días
*        ASSIGN COMPONENT lv_tabix_s OF STRUCTURE im_evento
*          TO FIELD-SYMBOL(<fs_field>).
*
*        IF <fs_field> IS ASSIGNED.
*          DATA(lv_dia_evento) = CONV abap_bool( <fs_field> ).
*          UNASSIGN <fs_field>.
*          ASSIGN COMPONENT lv_tabix_h OF STRUCTURE wa_hrt1716
*          TO <fs_field>.
*          IF <fs_field> IS ASSIGNED.
*            DATA(lv_dia_h) = CONV abap_bool( <fs_field> ).
*            UNASSIGN <fs_field>.
*            IF lv_dia_evento EQ abap_true
*              AND lv_dia_h EQ abap_true.
*              lv_cruza = abap_true.
*              EXIT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*
*        ADD 1 TO lv_tabix_s.
*        ADD 1 TO lv_tabix_h.
*      ENDDO.
*
*      IF lv_cruza EQ abap_true.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*
*    IF lv_cruza EQ abap_true.
*      re_subrc = lv_cruza.
*    ENDIF.
  ENDMETHOD.


  METHOD validar_dispo_indv.

    LOOP AT im_t_hrt1716 INTO DATA(wa_hrt1716).

      READ TABLE im_t_hrp1716 INTO DATA(wa_hrp1716)
        WITH KEY tabnr = wa_hrt1716-tabnr
        BINARY SEARCH.

      DATA(lv_val) = abap_false.
      " validar fecha de inicio y fin
*      IF ( im_evento-begda >= wa_hrp1716-begda
*        AND im_evento-begda <= wa_hrp1716-endda )
*       OR ( im_evento-begda <= wa_hrp1716-begda
*          AND im_evento-endda >= wa_hrp1716-begda  ).
*        lv_val = abap_true.
*      ENDIF.
      IF ( im_evento-begda >= wa_hrt1716-schedelebeg
        AND im_evento-begda <= wa_hrt1716-schedeleend )
       OR ( im_evento-begda <= wa_hrt1716-schedelebeg
          AND im_evento-endda >= wa_hrt1716-schedelebeg  ).
        lv_val = abap_true.
      ENDIF.

      CHECK lv_val EQ abap_true.

      " Validar si se cruzan horas
      lv_val = abap_false.
      IF ( im_evento-beguz > wa_hrt1716-beguz
        AND im_evento-beguz < wa_hrt1716-enduz )
       OR ( im_evento-beguz <= wa_hrt1716-beguz
         AND im_evento-enduz > wa_hrt1716-beguz ).
        lv_val = abap_true.
      ENDIF.

      CHECK lv_val EQ abap_true.

      " validar si se cruzan días
      DATA(lv_tabix_s) = CONV sy-tabix( 19 ).
      DATA(lv_tabix_h) = CONV sy-tabix( 6 ).
      DO 7 TIMES.
        DATA(lv_cruza) = abap_false.
        " validar días
        ASSIGN COMPONENT lv_tabix_s OF STRUCTURE im_evento
          TO FIELD-SYMBOL(<fs_field_e>).

        IF <fs_field_e> IS ASSIGNED.
          DATA(lv_dia_evento) = CONV abap_bool( <fs_field_e> ).
          UNASSIGN <fs_field_e>.
          ASSIGN COMPONENT lv_tabix_h OF STRUCTURE wa_hrt1716
          TO FIELD-SYMBOL(<fs_field_o>).
          IF <fs_field_o> IS ASSIGNED.
            DATA(lv_dia_h) = CONV abap_bool( <fs_field_o> ).
            UNASSIGN <fs_field_o>.
            IF lv_dia_evento EQ abap_true
              AND lv_dia_h EQ abap_true.
              lv_cruza = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.


        ADD 1 TO lv_tabix_s.
        ADD 1 TO lv_tabix_h.
      ENDDO.

      IF lv_cruza EQ abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_cruza EQ abap_true.
      re_abap_bool = lv_cruza.
    ENDIF.



  ENDMETHOD.
ENDCLASS.
