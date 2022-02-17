class ZWDC_BUSQUEDA_DOCENTES_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  constants C_TIPO_ACT type STRING value 'TIPO_ACT' ##NO_TEXT.
  constants C_TIPO_SAC type STRING value 'TIPO_SAC' ##NO_TEXT.
  constants C_COD_INV type STRING value 'COD_INV' ##NO_TEXT.

  class-methods GET_DDBK_TIPO_DOC
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DOCENTES
    importing
      !IS_SELECCION type ZEDU_S_SEL_DOCENTES
    returning
      value(RT_DOCENTE) type ZTT_ACT_PLAN_DOC .
  class-methods GET_DROPDOWN_KEY_DOCENTE
    importing
      !IM_FIELD type STRING
      !IM_VALUE type STRING optional
      !IM_VALUE_2 type STRING optional
      value(IM_NULL_VALUE) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  class-methods GET_DDBK_TIPO_DOC_2
    returning
      value(RT_ATTR_VALUE) type WDY_KEY_VALUE_TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_BUSQUEDA_DOCENTES_ASS IMPLEMENTATION.


  METHOD GET_DDBK_TIPO_DOC.

    SELECT ictyp AS value
           ictxt AS text
     FROM t5r06
       INTO TABLE rt_attr_value
         WHERE sprsl = sy-langu
           AND molga = '38'.  " Colombia

      if sy-subrc eq 0.
        sort rt_attr_value by value.
      endif.

  ENDMETHOD.


  METHOD get_ddbk_tipo_doc_2.

    SELECT ictyp AS key
           ictxt AS value
     FROM t5r06
       INTO TABLE rt_attr_value
         WHERE sprsl = sy-langu
           AND molga = '38'.  " Colombia

    IF sy-subrc EQ 0.
**      SORT rt_attr_value BY key.
    ENDIF.

  ENDMETHOD.


  METHOD get_docentes.

    "Tipos
    TYPES:
      BEGIN OF lty_perit,
        perid TYPE piqperid,
        perit TYPE piqperit,
      END OF lty_perit,

      BEGIN OF lty_ictxt,
        ictyp TYPE ictyp,
        ictxt TYPE ictxt,
      END OF lty_ictxt,

      BEGIN OF ty_docentes_pernr,
        pernr TYPE persno.
            INCLUDE TYPE zstr_read_docentes.
    TYPES: END OF ty_docentes_pernr.

    TYPES:
      BEGIN OF ty_resumen_perid,
        peryr TYPE piqperyr,
        perid TYPE piqperid.
            INCLUDE TYPE zedue123_2.
    TYPES: END OF ty_resumen_perid.

    TYPES:
      BEGIN OF ty_out_perid,
        peryr TYPE piqperyr,
        perid TYPE piqperid.
            INCLUDE TYPE zedu_extrac_out.
    TYPES: END OF ty_out_perid.

    "Declaraciones locales
    DATA:
      lv_tabix               TYPE sy-tabix,
      lv_tabix_ret           TYPE sy-tabix,
      lv_ultimo              TYPE abap_bool,
      lv_cantidad            TYPE i,
      lv_super_usuario       TYPE  c,
      lv_decano              TYPE  c,
      lv_jefe_programa       TYPE  c,
      lv_administrador       TYPE  c,
      lv_docente             TYPE  c,
      lv_begda               TYPE  datum,
      lv_endda               TYPE  datum,
      lv_memory_name(30)     TYPE  c,
      lt_perit               TYPE TABLE OF lty_perit,
      lt_ictxt               TYPE TABLE OF lty_ictxt,
      lt_zedu_pd_movinv      TYPE TABLE OF zedu_pd_movinv,
      lt_zedu_pd_movotr      TYPE TABLE OF zedu_pd_movotr,
      lt_zedu_pd_movbie      TYPE TABLE OF zedu_pd_movbie,
      lt_zedu_pd_movadm      TYPE TABLE OF zedu_pd_movadm,
      lt_zedu_pd_movext      TYPE TABLE OF zedu_pd_movext,
      lt_facultad            TYPE TABLE OF zdedu_cons_facultad,
      lt_in                  TYPE TABLE OF zedu_extrac_curs,
      lt_out                 TYPE TABLE OF zedu_extrac_out,
      lt_out_perid           TYPE TABLE OF ty_out_perid,
      lt_read_docentes       TYPE ztt_read_docentes,
      lt_read_docentes_pernr TYPE TABLE OF ty_docentes_pernr,
      lt_entrada_123         TYPE zedutt123_1,
      lt_resumen             TYPE zedutt123_2_2,
      lt_resumen_perid       TYPE TABLE OF ty_resumen_perid,
      lt_entrada_122         TYPE zedutt122_1,
      lt_salida_122          TYPE zedutt122_2,
      e_t_authority          TYPE STANDARD TABLE OF hrms_bw_is_authority,
      lr_tipo_doc            TYPE RANGE OF ictyp,
      ls_in                  TYPE zedu_extrac_curs,
      ls_entrada_123         LIKE LINE OF lt_entrada_123,
      ls_entrada_122         LIKE LINE OF lt_entrada_122,
      ls_salida_122          TYPE zedue122_2,
      lrs_tipo_doc           LIKE LINE OF lr_tipo_doc.

    "Field-Symbols
    FIELD-SYMBOLS:
      <fs_perit>               TYPE lty_perit,
      <fs_facultad>            TYPE zdedu_cons_facultad,
      <fs_authority>           TYPE hrms_bw_is_authority,
      <fs_read_docentes>       TYPE zstr_read_docentes,
      <fs_read_docentes_pernr> TYPE ty_docentes_pernr,
      <fs_resumen>             TYPE zedue123_2_2,
      <fs_resumen_perid>       TYPE ty_resumen_perid,
      <fs_out>                 TYPE zedu_extrac_out,
      <fs_out_perid>           TYPE ty_out_perid,
      <fs_salida_alv>          TYPE zstr_act_plan_doc,
      <fs_ictxt>               TYPE lty_ictxt,
      <fs_zedu_pd_movinv>      TYPE zedu_pd_movinv,
      <fs_zedu_pd_movotr>      TYPE zedu_pd_movotr,
      <fs_zedu_pd_movbie>      TYPE zedu_pd_movbie,
      <fs_zedu_pd_movadm>      TYPE zedu_pd_movadm,
      <fs_zedu_pd_movext>      TYPE zedu_pd_movext.


    "Si se tiene filtro por tipo de documento
    IF is_seleccion-tipo_doc IS NOT INITIAL.
      "Asigna el filtro al rango y crea el registro
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = is_seleccion-tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    "Obtiene las descripciones de los periodos seleccionados
    SELECT perid perit
      INTO TABLE lt_perit
      FROM t7piqperiodt
      WHERE spras EQ sy-langu
        AND perid IN is_seleccion-periodo.

    "Ordena las descripciones de los periodos
    SORT lt_perit BY perid.

    "Obtiene las descripciones de los tipos de documentos de identidad
    SELECT ictyp ictxt
      INTO TABLE lt_ictxt
      FROM t5r06
      WHERE sprsl = sy-langu
        AND molga = '38'.

    "Ordena las descripciones de los tipos de documento de identidad
    SORT lt_ictxt BY ictyp.

    "Obtiene las facultades que tiene la ayuda de busqueda
    CALL FUNCTION 'ZDEDU_CONS_FACULTADES_CP'
      EXPORTING
        is_eventos           = 'X'
      TABLES
        ti_edu_cons_facultad = lt_facultad.

    "Elimina las facultades que no hacen parte de la seleccion
    DELETE lt_facultad
      WHERE NOT objid IN is_seleccion-facultad.

    "Ordena los registros
    SORT lt_facultad BY objid.

    "Obtiene las autorizaciones del usuario de ejecucion
    CALL FUNCTION 'ZDEDU_CONSULTAR_USUARIO'
      EXPORTING
        i_user          = sy-uname
      IMPORTING
        e_super_usuario = lv_super_usuario
        e_decano        = lv_decano
        e_jefe_programa = lv_jefe_programa
        e_administrador = lv_administrador
        e_docente       = lv_docente
        begda           = lv_begda
        endda           = lv_endda
        e_memory_name   = lv_memory_name.

    "Obtiene los perfiles de autorizacion
    IMPORT gt_perfiles TO e_t_authority FROM SHARED MEMORY indx(aa) ID 'MC'.

    "Si es un decano o Jefe de programa
    IF lv_decano EQ 'X' OR lv_jefe_programa EQ 'X'.
      "Si se tienen perfiles de autorizacion
      IF e_t_authority[] IS NOT INITIAL.
        "Recorre las facultades a consultar
        LOOP AT lt_facultad ASSIGNING <fs_facultad>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_facultad>-objid.
          IF sy-subrc NE 0.
            "Elimina el registro cuya facultad no esta autorizada
            DELETE lt_facultad INDEX lv_tabix.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        "Si no se tienen perfiles de autorizacion
      ELSE.
        "Borra la totalidad de registros obtenidos
        REFRESH:
          rt_docente,
          lt_facultad.
      ENDIF.
    ENDIF.

    "Continua solo si se tiene facultades
    CHECK NOT lt_facultad IS INITIAL.

    "Otras actividades
    SELECT * INTO TABLE lt_zedu_pd_movotr
      FROM zedu_pd_movotr FOR ALL ENTRIES IN lt_facultad
     WHERE peryr           EQ is_seleccion-anio
       AND perid           IN is_seleccion-periodo
       AND tipo_identifica IN lr_tipo_doc
       AND nro_identifica  IN is_seleccion-nro_doc
       AND facultad        EQ lt_facultad-objid.
    "Ordena los registros
    SORT lt_zedu_pd_movotr BY peryr perid tipo_identifica nro_identifica facultad.

    "Investigación
    SELECT * INTO TABLE lt_zedu_pd_movinv
      FROM zedu_pd_movinv FOR ALL ENTRIES IN lt_facultad
     WHERE peryr           EQ is_seleccion-anio
       AND perid           IN is_seleccion-periodo
       AND tipo_identifica IN lr_tipo_doc
       AND nro_identifica  IN is_seleccion-nro_doc
       AND facultad        EQ lt_facultad-objid.
    "Ordena los registros
    SORT lt_zedu_pd_movinv BY peryr perid tipo_identifica nro_identifica facultad.

    "Bienestar Universitario
    SELECT * INTO TABLE lt_zedu_pd_movbie
      FROM zedu_pd_movbie FOR ALL ENTRIES IN lt_facultad
     WHERE peryr           EQ is_seleccion-anio
       AND perid           IN is_seleccion-periodo
       AND tipo_identifica IN lr_tipo_doc
       AND nro_identifica  IN is_seleccion-nro_doc
       AND facultad        EQ lt_facultad-objid.
    "Ordena los registros
    SORT lt_zedu_pd_movbie BY peryr perid tipo_identifica nro_identifica facultad.

    "Administración docencia
    SELECT * INTO TABLE lt_zedu_pd_movadm
      FROM zedu_pd_movadm FOR ALL ENTRIES IN lt_facultad
     WHERE peryr           EQ is_seleccion-anio
       AND perid           IN is_seleccion-periodo
       AND tipo_identifica IN lr_tipo_doc
       AND nro_identifica  IN is_seleccion-nro_doc
       AND facultad        EQ lt_facultad-objid.
    "Ordena los registros
    SORT lt_zedu_pd_movadm BY peryr perid tipo_identifica nro_identifica facultad.

    "Actividad extension
    SELECT * INTO TABLE lt_zedu_pd_movext
      FROM zedu_pd_movext FOR ALL ENTRIES IN lt_facultad
     WHERE peryr           EQ is_seleccion-anio
       AND perid           IN is_seleccion-periodo
       AND tipo_identifica IN lr_tipo_doc
       AND nro_identifica  IN is_seleccion-nro_doc
       AND facultad        EQ lt_facultad-objid.
    "Ordena los registros
    SORT lt_zedu_pd_movext BY peryr perid tipo_identifica nro_identifica facultad.

    "Recorre los periodos seleccionados
    LOOP AT lt_perit ASSIGNING <fs_perit>.
      "Inicializa declaraciones
      CLEAR:
        lv_ultimo,
        lt_read_docentes,
        lt_read_docentes_pernr,
        lt_entrada_123,
        lt_resumen_perid,
        lt_in,
        lt_out_perid.

      "Obtiene los docentes del periodo seleccionado
*      CALL FUNCTION 'Z_EDU_RFC_READ_DOCENTES'
      CALL FUNCTION 'Z_EDU_RFC_READ_DOCENTES_V2'
        EXPORTING
          peryr      = is_seleccion-anio
          perid      = <fs_perit>-perid
        IMPORTING
          t_docentes = lt_read_docentes
        EXCEPTIONS
          no_data    = 1
          OTHERS     = 2.

      "Elimina los docentes que no hacen parte de la seleccion
      DELETE lt_read_docentes
        WHERE NOT tipo_id IN lr_tipo_doc
          OR  NOT num_id  IN is_seleccion-nro_doc.

      "Ordena los docentes por identificacion
      SORT lt_read_docentes BY tipo_id num_id.

      "Recorre los docentes del periodo
      LOOP AT lt_read_docentes ASSIGNING <fs_read_docentes>.
        "Si es el ultimo registro
        AT LAST.
          "Indica que es el ultimo registro
          lv_ultimo = abap_true.
        ENDAT.

        "Crea el registro con los datos del docente y el periodo
        APPEND INITIAL LINE TO lt_read_docentes_pernr ASSIGNING <fs_read_docentes_pernr>.
        "Asigna los datos del registro
        MOVE-CORRESPONDING <fs_read_docentes> TO <fs_read_docentes_pernr>.
        "Asigna el numero de personal
        SELECT SINGLE pernr INTO <fs_read_docentes_pernr>-pernr
          FROM pa0185
          WHERE icnum EQ <fs_read_docentes_pernr>-num_id
            AND ictyp EQ <fs_read_docentes_pernr>-tipo_id.

        "Recorre las facultades
        LOOP AT lt_facultad ASSIGNING <fs_facultad>.
          "Inicializa declaraciones
          CLEAR:
            ls_entrada_123,
            ls_in.

          "Asigna los datos a consultar y crea el registro
          ls_entrada_123-icnum     = <fs_read_docentes>-num_id.
          ls_entrada_123-linea_edu = 'F'.
          ls_entrada_123-objid     = <fs_facultad>-objid.
          APPEND ls_entrada_123 TO lt_entrada_123.

          ls_in-facultad   = <fs_facultad>-objid.
          ls_in-id_docente = <fs_read_docentes>-num_id.
          APPEND ls_in TO lt_in.
        ENDLOOP.

        "Obtiene la cantidad de registros actuales a consultar
        DESCRIBE TABLE lt_entrada_123 LINES lv_cantidad.

        "Continua solo si se llego al bloque o es el ultimo registro
        CHECK lv_cantidad GE '5000' OR lv_ultimo EQ abap_true.
        "Continua solo si se tienen datos a consultar
        CHECK NOT lt_entrada_123 IS INITIAL.

        "Inicializa retornos
        CLEAR:
          lt_resumen,
          lt_out.

        "Obtiene los datos de la programacion academica del docente
        CALL FUNCTION 'Z_EDU_RFC_123_1'
          EXPORTING
            i_peryr        = is_seleccion-anio
            i_perid        = <fs_perit>-perid
            i_entrada      = lt_entrada_123
          IMPORTING
            e_resumen      = lt_resumen
          EXCEPTIONS
            internal_error = 1
            OTHERS         = 2.

        "Recorre los registros obtenidos
        LOOP AT lt_resumen ASSIGNING <fs_resumen>.
          "Crea el registro con los datos obtenidos y el periodo
          APPEND INITIAL LINE TO lt_resumen_perid ASSIGNING <fs_resumen_perid>.
          "Asigna los datos obtenidos
          MOVE-CORRESPONDING <fs_resumen> TO <fs_resumen_perid>.
          "Asigna el año y periodo
          <fs_resumen_perid>-peryr = is_seleccion-anio.
          <fs_resumen_perid>-perid = <fs_perit>-perid.
        ENDLOOP.

        "Obtiene los datos de Educación Continua
        CALL FUNCTION 'Z_EDU_EXTR_CURSOS_EDU_CONTI_V2'
          EXPORTING
            i_peryr = is_seleccion-anio
            i_perid = <fs_perit>-perid
          TABLES
            lt_in   = lt_in
            lt_out  = lt_out.

        "Recorre los registros obtenidos
        LOOP AT lt_out ASSIGNING <fs_out>.
          "Crea el registro con los datos obtenidos y el periodo
          APPEND INITIAL LINE TO lt_out_perid ASSIGNING <fs_out_perid>.
          "Asigna los datos obtenidos
          MOVE-CORRESPONDING <fs_out> TO <fs_out_perid>.
          "Asigna el año y periodo
          <fs_out_perid>-peryr = is_seleccion-anio.
          <fs_out_perid>-perid = <fs_perit>-perid.
        ENDLOOP.

        "Inicializa declaraciones
        CLEAR:
          lv_cantidad,
          lt_entrada_123,
          lt_in.
      ENDLOOP.

      "Ordena los registros obtenidos
      SORT lt_resumen_perid BY peryr perid icnum objid_o.
      SORT lt_out_perid     BY peryr perid id_docente facultad.

      "Recorre los docentes del periodo consultados
      LOOP AT lt_read_docentes_pernr ASSIGNING <fs_read_docentes_pernr>.
        "Recorre las facultades
        LOOP AT lt_facultad ASSIGNING <fs_facultad>.
          "Inicializa declaraciones
          CLEAR:
          ls_entrada_122,
          lt_entrada_122,
          lt_salida_122,
          ls_salida_122.

          "Asigna los datos del BP a consultar y crea el registro
          ls_entrada_122-peryr          = is_seleccion-anio.
          ls_entrada_122-perid          = <fs_perit>-perid.
          ls_entrada_122-objid          = <fs_facultad>-objid.
          ls_entrada_122-ictyp          = <fs_read_docentes_pernr>-tipo_id.
          ls_entrada_122-identificacion = <fs_read_docentes_pernr>-num_id.
          APPEND ls_entrada_122 TO lt_entrada_122.

          "Obtiene los datos del docente para la facultad y periodo registrado
          CALL FUNCTION 'Z_EDU_RFC_122_1'
            EXPORTING
              i_entrada = lt_entrada_122
            IMPORTING
              e_salida  = lt_salida_122.

          "Lee el registro obtenido
          READ TABLE lt_salida_122 INTO ls_salida_122 INDEX 1.

          "Crea el registro de retorno en el alv
          APPEND INITIAL LINE TO rt_docente ASSIGNING <fs_salida_alv>.

          "Almacena el indice del registro creado
          lv_tabix_ret = sy-tabix.

          "Asigna los datos
          <fs_salida_alv>-ano         = is_seleccion-anio.  "Año
          <fs_salida_alv>-periodo     = <fs_perit>-perid.   "Periodo
          <fs_salida_alv>-txt_periodo = <fs_perit>-perit.   "Descripcion Periodo

          "Obtiene la descripcion del tipo de documento
          READ TABLE lt_ictxt ASSIGNING <fs_ictxt>
            WITH KEY ictyp = <fs_read_docentes_pernr>-tipo_id
            BINARY SEARCH.

          "Si encuentra regitro
          IF sy-subrc EQ 0.
            "Asigna la descripcion del tipo de documento
            <fs_salida_alv>-tipo_doc = <fs_ictxt>-ictxt.

            "Si no encuentra registro.
          ELSE.
            "Asigna el codigo del tipo de documento
            <fs_salida_alv>-tipo_doc = <fs_read_docentes_pernr>-tipo_id.
          ENDIF.

          <fs_salida_alv>-identificacion  = <fs_read_docentes_pernr>-num_id.  "Identificacion
          <fs_salida_alv>-nombre1         = ls_salida_122-nombre1.            "Primer Nombre
          <fs_salida_alv>-nombre2         = ls_salida_122-nombre2.            "Segundo Nombre
          <fs_salida_alv>-apellido1       = ls_salida_122-apellido1.          "Primer Apellido
          <fs_salida_alv>-apellido2       = ls_salida_122-apellido2.          "Segundo Apellido
          <fs_salida_alv>-correo_email    = ls_salida_122-correo_email.       "Correo Electronico
          <fs_salida_alv>-txt_contrato    = ls_salida_122-txt_contrato.       "Contrato
          <fs_salida_alv>-tex_dedicacion  = ls_salida_122-tex_clasificacion.  "Dedicacion
          <fs_salida_alv>-horas_vincula   = ls_salida_122-horas_vincula.      "Horas Vinculacion
          <fs_salida_alv>-horas_honorario = ls_salida_122-horas_honorario.    "Horas Honorario
          <fs_salida_alv>-facultad        = <fs_facultad>-objid.              "Codigo Facultad
          <fs_salida_alv>-txt_facultad    = <fs_facultad>-stext.              "Descripcion Facultad

          "Obtiene el primer registro de Actividades administracion docencia
          READ TABLE lt_zedu_pd_movadm TRANSPORTING NO FIELDS
            WITH KEY peryr           = is_seleccion-anio
                     perid           = <fs_perit>-perid
                     tipo_identifica = <fs_read_docentes_pernr>-tipo_id
                     nro_identifica  = <fs_read_docentes_pernr>-num_id
                     facultad        = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de las actividades de administracion docencia
            LOOP AT lt_zedu_pd_movadm ASSIGNING <fs_zedu_pd_movadm> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_zedu_pd_movadm>-peryr           NE is_seleccion-anio                OR
                 <fs_zedu_pd_movadm>-perid           NE <fs_perit>-perid                 OR
                 <fs_zedu_pd_movadm>-tipo_identifica NE <fs_read_docentes_pernr>-tipo_id OR
                 <fs_zedu_pd_movadm>-nro_identifica  NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_zedu_pd_movadm>-facultad        NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades administracion docencia
              <fs_salida_alv>-horas_adm_doc = <fs_salida_alv>-horas_adm_doc + <fs_zedu_pd_movadm>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Actividades docencia
          READ TABLE lt_resumen_perid TRANSPORTING NO FIELDS
            WITH KEY peryr   = is_seleccion-anio
                     perid   = <fs_perit>-perid
                     icnum   = <fs_read_docentes_pernr>-num_id
                     objid_o = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Actividades docencia
            LOOP AT lt_resumen_perid ASSIGNING <fs_resumen_perid> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_resumen_perid>-peryr   NE is_seleccion-anio                OR
                 <fs_resumen_perid>-perid   NE <fs_perit>-perid                 OR
                 <fs_resumen_perid>-icnum   NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_resumen_perid>-objid_o NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades docencia
              <fs_salida_alv>-horas_act_doc = <fs_salida_alv>-horas_act_doc + <fs_resumen_perid>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Actividades Investigacion
          READ TABLE lt_zedu_pd_movinv TRANSPORTING NO FIELDS
            WITH KEY peryr           = is_seleccion-anio
                     perid           = <fs_perit>-perid
                     tipo_identifica = <fs_read_docentes_pernr>-tipo_id
                     nro_identifica  = <fs_read_docentes_pernr>-num_id
                     facultad        = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Actividades Investigación
            LOOP AT lt_zedu_pd_movinv ASSIGNING <fs_zedu_pd_movinv> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_zedu_pd_movinv>-peryr           NE is_seleccion-anio                OR
                 <fs_zedu_pd_movinv>-perid           NE <fs_perit>-perid                 OR
                 <fs_zedu_pd_movinv>-tipo_identifica NE <fs_read_docentes_pernr>-tipo_id OR
                 <fs_zedu_pd_movinv>-nro_identifica  NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_zedu_pd_movinv>-facultad        NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades Investigacion
              <fs_salida_alv>-horas_inv = <fs_salida_alv>-horas_inv + <fs_zedu_pd_movinv>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Actividades extension
          READ TABLE lt_out_perid TRANSPORTING NO FIELDS
            WITH KEY peryr      = is_seleccion-anio
                     perid      = <fs_perit>-perid
                     id_docente = <fs_read_docentes_pernr>-pernr
                     facultad   = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Actividades extensión
            LOOP AT lt_out_perid ASSIGNING <fs_out_perid> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_out_perid>-peryr      NE is_seleccion-anio              OR
                 <fs_out_perid>-perid      NE <fs_perit>-perid               OR
                 <fs_out_perid>-id_docente NE <fs_read_docentes_pernr>-pernr OR
                 <fs_out_perid>-facultad   NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades extension
              <fs_salida_alv>-horas_act_ext = <fs_salida_alv>-horas_act_ext + <fs_out_perid>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Actividades adicionales extension
          READ TABLE lt_zedu_pd_movext TRANSPORTING NO FIELDS
            WITH KEY peryr           = is_seleccion-anio
                     perid           = <fs_perit>-perid
                     tipo_identifica = <fs_read_docentes_pernr>-tipo_id
                     nro_identifica  = <fs_read_docentes_pernr>-num_id
                     facultad        = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Actividades adicionales extension
            LOOP AT lt_zedu_pd_movext ASSIGNING <fs_zedu_pd_movext> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_zedu_pd_movext>-peryr           NE is_seleccion-anio                OR
                 <fs_zedu_pd_movext>-perid           NE <fs_perit>-perid                 OR
                 <fs_zedu_pd_movext>-tipo_identifica NE <fs_read_docentes_pernr>-tipo_id OR
                 <fs_zedu_pd_movext>-nro_identifica  NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_zedu_pd_movext>-facultad        NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades adicionales extension
              <fs_salida_alv>-horas_act_ext = <fs_salida_alv>-horas_act_ext + <fs_zedu_pd_movext>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Otras actividades
          READ TABLE lt_zedu_pd_movotr TRANSPORTING NO FIELDS
            WITH KEY peryr           = is_seleccion-anio
                     perid           = <fs_perit>-perid
                     tipo_identifica = <fs_read_docentes_pernr>-tipo_id
                     nro_identifica  = <fs_read_docentes_pernr>-num_id
                     facultad        = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Otras actividades
            LOOP AT lt_zedu_pd_movotr ASSIGNING <fs_zedu_pd_movotr> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_zedu_pd_movotr>-peryr           NE is_seleccion-anio                OR
                 <fs_zedu_pd_movotr>-perid           NE <fs_perit>-perid                 OR
                 <fs_zedu_pd_movotr>-tipo_identifica NE <fs_read_docentes_pernr>-tipo_id OR
                 <fs_zedu_pd_movotr>-nro_identifica  NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_zedu_pd_movotr>-facultad        NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Otras actividades
              <fs_salida_alv>-horas_otras_a = <fs_salida_alv>-horas_otras_a + <fs_zedu_pd_movotr>-horas.
            ENDLOOP.
          ENDIF.

          "Obtiene el primer registro de Actividades Bienestar
          READ TABLE lt_zedu_pd_movbie TRANSPORTING NO FIELDS
            WITH KEY peryr           = is_seleccion-anio
                     perid           = <fs_perit>-perid
                     tipo_identifica = <fs_read_docentes_pernr>-tipo_id
                     nro_identifica  = <fs_read_docentes_pernr>-num_id
                     facultad        = <fs_facultad>-objid
            BINARY SEARCH.

          "Si encuentra el registro
          IF sy-subrc EQ 0.
            "Guarda el indice del primer registro encontrado
            lv_tabix = sy-tabix.
            "Recorre los registros de Actividades Bienestar
            LOOP AT lt_zedu_pd_movbie ASSIGNING <fs_zedu_pd_movbie> FROM lv_tabix.
              "Si el registro no esta relacionado
              IF <fs_zedu_pd_movbie>-peryr           NE is_seleccion-anio                OR
                 <fs_zedu_pd_movbie>-perid           NE <fs_perit>-perid                 OR
                 <fs_zedu_pd_movbie>-tipo_identifica NE <fs_read_docentes_pernr>-tipo_id OR
                 <fs_zedu_pd_movbie>-nro_identifica  NE <fs_read_docentes_pernr>-num_id  OR
                 <fs_zedu_pd_movbie>-facultad        NE <fs_facultad>-objid.
                "Deja de recorrer los registros
                EXIT.
              ENDIF.
              "Aumenta la cantidad de horas de Actividades Bienestar
              <fs_salida_alv>-horas_bien   = <fs_salida_alv>-horas_bien + <fs_zedu_pd_movbie>-horas.
            ENDLOOP.
          ENDIF.

          "(Esta es la suma de todas las horas de cada una de las diferentes actividades en un semestre).
          <fs_salida_alv>-horas_semestre = <fs_salida_alv>-horas_adm_doc + <fs_salida_alv>-horas_act_doc + <fs_salida_alv>-horas_inv +
                                           <fs_salida_alv>-horas_act_ext + <fs_salida_alv>-horas_otras_a + <fs_salida_alv>-horas_bien.

          "Si se tienen horas calculadas
          IF <fs_salida_alv>-horas_semestre GT 0.
            "Asigna los porcentajes
            <fs_salida_alv>-porcentaje_adm  = <fs_salida_alv>-horas_adm_doc / <fs_salida_alv>-horas_semestre * 100.
            <fs_salida_alv>-porcentaje_act  = <fs_salida_alv>-horas_act_doc / <fs_salida_alv>-horas_semestre * 100.
            <fs_salida_alv>-porcentaje_inv  = <fs_salida_alv>-horas_inv     / <fs_salida_alv>-horas_semestre * 100.
            <fs_salida_alv>-porcentaje_ext  = <fs_salida_alv>-horas_act_ext / <fs_salida_alv>-horas_semestre * 100.
            <fs_salida_alv>-porcentaje_oa   = <fs_salida_alv>-horas_otras_a / <fs_salida_alv>-horas_semestre * 100.
            <fs_salida_alv>-porcentaje_bien = <fs_salida_alv>-horas_bien    / <fs_salida_alv>-horas_semestre * 100.

            "Si no se tienen horas calculadas
          ELSE.
            "Elimina el regisrto creado
            DELETE rt_docente INDEX lv_tabix_ret.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    "Ordena los registros
    SORT rt_docente BY tipo_doc identificacion ano periodo facultad.

  ENDMETHOD.


  METHOD GET_DROPDOWN_KEY_DOCENTE.

    DATA: ls_attr_value TYPE wdr_context_attr_value.

    CASE im_field.
      WHEN c_tipo_act. " Tipo de Actividad

        IF im_value IS NOT INITIAL.
          SELECT tipo_act        AS value
                 descripcion_act AS text
           FROM zedu_pd_tipoact
           INTO TABLE rt_attr_value
            WHERE cod_act = im_value.
        ELSE.
          SELECT tipo_act        AS value
                 descripcion_act AS text
           FROM zedu_pd_tipoact
           INTO TABLE rt_attr_value.
        ENDIF.

      WHEN c_tipo_sac. " Tipo de Subactividad

        IF im_value IS NOT INITIAL AND im_value_2 IS NOT INITIAL.
          SELECT tipo_sac        AS value
                 descripcion_act AS text
           FROM zedu_pd_tiposac
           INTO TABLE rt_attr_value
            WHERE cod_act  = im_value
              AND tipo_act = im_value_2.
        ELSE.
          SELECT tipo_sac        AS value
                 descripcion_act AS text
           FROM zedu_pd_tiposac
           INTO TABLE rt_attr_value.
        ENDIF.

      WHEN c_cod_inv. " Investiga

        SELECT cod_inv         AS value
               descripcion_inv AS text
         FROM zedu_pd_grupoin
         INTO TABLE rt_attr_value.

      WHEN OTHERS.

    ENDCASE.

    IF im_null_value EQ cl_bp_const=>true.

      "Agrego línea en blanco para poder seleccionar blanco
      APPEND INITIAL LINE TO rt_attr_value.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
