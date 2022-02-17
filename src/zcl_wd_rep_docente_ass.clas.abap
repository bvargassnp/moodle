class ZCL_WD_REP_DOCENTE_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  constants C_TIPO_ACT type STRING value 'TIPO_ACT' ##NO_TEXT.
  constants C_TIPO_SAC type STRING value 'TIPO_SAC' ##NO_TEXT.
  constants C_COD_INV type STRING value 'COD_INV' ##NO_TEXT.

  methods GET_ACT_OTRAS
    importing
      !IV_ANIO type PIQPERYR
      !IR_PERIODO type ZEDU_RT_PERID
      !IV_TIPO_DOC type ICTYP
      !IR_NUM_DOC type ZEDU_RT_PSG_IDNUM
      !IR_FACULTAD type /ISDFPS/RT_HROBJID
      !IV_COD_ACT type ZEDU_COD_ACT
      !IR_TIPO_ACT type ZEDU_RT_TIPO_ACT
      !IR_TIPO_SAC type ZEDU_RT_TIPO_SAC
    returning
      value(RT_ACT_OTRAS) type ZEDU_TT_PD_MOVOTR .
  class-methods GET_DDBK_TIPO_DOC
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_ACT_BIENESTAR_UNIV
    importing
      !IV_ANIO type PIQPERYR
      !IR_PERIODO type ZEDU_RT_PERID
      !IV_TIPO_DOC type ICTYP
      !IR_NUM_DOC type ZEDU_RT_PSG_IDNUM
      !IR_FACULTAD type /ISDFPS/RT_HROBJID
      !IV_COD_ACT type ZEDU_COD_ACT
      !IR_TIPO_ACT type ZEDU_RT_TIPO_ACT
      !IR_TIPO_SAC type ZEDU_RT_TIPO_SAC
    returning
      value(RT_ACT_BIENESTAR_UNIV) type ZEDU_TT_PD_MOVBIE .
  methods GET_ACT_ADMINISTRACION
    importing
      !IV_ANIO type PIQPERYR
      !IV_PERIODO type PIQPERID
      !IV_TIPO_DOC type ICTYP
      !IV_NUM_DOC type PSG_IDNUM
      !IV_FACULTAD type HROBJID
    returning
      value(RT_ACT_ADMINISTRACION) type ZEDU_TT_PD_MOVADM .
  methods GET_ACT_INVESTIGACION
    importing
      !IV_ANIO type PIQPERYR
      !IR_PERIODO type ZEDU_RT_PERID
      !IV_TIPO_DOC type ICTYP
      !IR_NUM_DOC type ZEDU_RT_PSG_IDNUM
      !IR_FACULTAD type /ISDFPS/RT_HROBJID
      !IV_COD_ACT type ZEDU_COD_ACT optional
      !IR_TIPO_ACT type ZEDU_RT_TIPO_ACT optional
      !IR_TIPO_SAC type ZEDU_RT_TIPO_SAC optional
    returning
      value(RT_ACT_INVESTIGACION) type ZEDU_TT_PD_MOVINV .
  methods GET_ACT_EXTENSION
    importing
      !IV_ANIO type PIQPERYR
      !IR_PERIODO type ZEDU_RT_PERID
      !IV_TIPO_DOC type ICTYP
      !IR_NUM_DOC type ZEDU_RT_PSG_IDNUM
      !IR_FACULTAD type /ISDFPS/RT_HROBJID
      !IV_COD_ACT type ZEDU_COD_ACT optional
      !IR_TIPO_ACT type ZEDU_RT_TIPO_ACT optional
      !IR_TIPO_SAC type ZEDU_RT_TIPO_SAC optional
    exporting
      value(ET_ACT_EXTENSION) type ZEDU_TT_PD_MOVEXT
      value(ET_ACT_CONTINUA) type ZEDU_TT_EXTRAC_OUT_PERID .
  methods GET_ACT_EXTENSION_V2
    importing
      !IV_ANIO type PIQPERYR
      !IV_PERIODO type PIQPERID
      !IV_TIPO_DOC type ICTYP
      !IV_NUM_DOC type PSG_IDNUM
      !IV_FACULTAD type HROBJID
      !IV_COD_ACT type ZEDU_COD_ACT optional
      !IR_TIPO_ACT type ZEDU_RT_TIPO_ACT optional
      !IR_TIPO_SAC type ZEDU_RT_TIPO_SAC optional
    exporting
      value(ET_ACT_EXTENSION) type ZEDU_TT_PD_MOVEXT
      value(ET_ACT_CONTINUA) type ZEDU_TT_EXTRAC_OUT .
  methods GET_ACT_DOCENCIA
    importing
      !IV_ANIO type PIQPERYR
      !IV_PERIODO type PIQPERID
      !IV_NUM_DOC type PSG_IDNUM
      !IV_FACULTAD type HROBJID
    returning
      value(RT_ACT_DOCENCIA) type ZEDUTT123_2 .
  methods SET_TEXT_COLUMN_ALV
    importing
      !IV_TEXT type WDR_TEXT_KEY
    changing
      !CS_COLUMN type SALV_WD_S_COLUMN_REF .
  class-methods GET_DROPDOWN_KEY_DOCENTE
    importing
      !IM_FIELD type STRING
      !IM_VALUE type STRING optional
      !IM_VALUE_2 type STRING optional
      value(IM_NULL_VALUE) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DESC_ACT_SAC
    importing
      !IV_COD_ACT type ZEDU_COD_ACT
    exporting
      !ET_PD_TIPOACT type ZEDU_TT_PD_TIPOACT
      !ET_PD_TIPOSAC type ZEDU_TT_PD_TIPOSAC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_WD_REP_DOCENTE_ASS IMPLEMENTATION.


  METHOD GET_ACT_ADMINISTRACION.

    SELECT *
      FROM zedu_pd_movadm
      INTO TABLE rt_act_administracion
      WHERE peryr = iv_anio
        AND perid = iv_periodo
        AND tipo_identifica = iv_tipo_doc
        AND nro_identifica = iv_num_doc
***COD_ACT
***TIPO_ACT
***TIPO_SAC
        AND facultad = iv_facultad.

  ENDMETHOD.


  METHOD get_act_bienestar_univ.

    DATA:
      lv_tabix           TYPE sy-tabix,
      lv_super_usuario   TYPE  c,
      lv_decano          TYPE  c,
      lv_jefe_programa   TYPE  c,
      lv_administrador   TYPE  c,
      lv_docente         TYPE  c,
      lv_begda           TYPE  datum,
      lv_endda           TYPE  datum,
      lv_memory_name(30) TYPE  c,
      lr_tipo_doc        TYPE RANGE OF ictyp,
      lrs_tipo_doc       LIKE LINE OF lr_tipo_doc,
      e_t_authority      TYPE STANDARD TABLE OF hrms_bw_is_authority.

    FIELD-SYMBOLS:
      <fs_act_bie>   TYPE zedu_pd_movbie,
      <fs_authority> TYPE hrms_bw_is_authority.


    IF iv_tipo_doc IS NOT INITIAL.
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = iv_tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    SELECT *
      FROM zedu_pd_movbie
      INTO TABLE rt_act_bienestar_univ
      WHERE peryr           EQ iv_anio
        AND perid           IN ir_periodo
        AND tipo_identifica IN lr_tipo_doc
        AND nro_identifica  IN ir_num_doc
        AND cod_act         EQ iv_cod_act
        AND tipo_act        IN ir_tipo_act
        AND tipo_sac        IN ir_tipo_sac
        AND facultad        IN ir_facultad.

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

    IMPORT gt_perfiles TO e_t_authority FROM SHARED MEMORY indx(aa) ID 'MC'.

    IF lv_decano EQ 'X' OR lv_jefe_programa EQ 'X'.
      IF e_t_authority[] IS NOT INITIAL.
        LOOP AT rt_act_bienestar_univ ASSIGNING <fs_act_bie>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_bie>-facultad.
          IF sy-subrc NE 0.
            DELETE rt_act_bienestar_univ INDEX lv_tabix.
          ENDIF.
        ENDLOOP.
      ELSE.
        REFRESH rt_act_bienestar_univ.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_ACT_DOCENCIA.

    DATA: lt_entrada   TYPE zedutt123_1,
          ls_entrada   TYPE zedue123_1,
          lt_salida    TYPE zedutt123_2_1,
          lv_resultado TYPE vvmsgtxt.

    ls_entrada-icnum     = iv_num_doc.
*    ls_entrada-linea_edu = 'E'.
    ls_entrada-objid     = iv_facultad.

    APPEND ls_entrada TO lt_entrada.

    CALL FUNCTION 'Z_EDU_RFC_123_1'
      EXPORTING
        i_peryr        = iv_anio
        i_perid        = iv_periodo
        i_entrada      = lt_entrada
      IMPORTING
        e_salida       = lt_salida
        e_resultado    = lv_resultado
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD get_act_extension.

    "Tipos de datos
    TYPES:
      BEGIN OF lty_periodo,
        perid TYPE piqperid,
      END OF lty_periodo.

    "Declaraciones locales
    DATA:
      lt_periodo         TYPE TABLE OF lty_periodo,
      lt_facultad        TYPE TABLE OF zdedu_cons_facultad,
      lt_read_docentes   TYPE ztt_read_docentes,
      lt_entrada         TYPE TABLE OF zedu_extrac_curs,
      lt_entrada_blq     TYPE TABLE OF zedu_extrac_curs,
      lt_extrac_out      TYPE zedu_tt_extrac_out,
      lt_act_continua    TYPE zedu_tt_extrac_out,
      ls_entrada         TYPE zedu_extrac_curs,
      lv_cantidad        TYPE i,
      lv_ultimo          TYPE abap_bool,
      lv_fin_id          TYPE abap_bool,
      lv_tabix           TYPE sy-tabix,
      lv_super_usuario   TYPE  c,
      lv_decano          TYPE  c,
      lv_jefe_programa   TYPE  c,
      lv_administrador   TYPE  c,
      lv_docente         TYPE  c,
      lv_begda           TYPE  datum,
      lv_endda           TYPE  datum,
      lv_memory_name(30) TYPE  c,
      lr_tipo_doc        TYPE RANGE OF ictyp,
      lrs_tipo_doc       LIKE LINE OF lr_tipo_doc,
      lrs_periodo        LIKE LINE OF ir_periodo,
      e_t_authority      TYPE STANDARD TABLE OF hrms_bw_is_authority.

    "Field-Symbols
    FIELD-SYMBOLS:
      <fs_act_extension> TYPE zedu_pd_movext,
      <fs_act_continua>  TYPE zedu_extrac_out_perid,
      <fs_facultad>      TYPE zdedu_cons_facultad,
      <fs_read_docentes> TYPE zstr_read_docentes,
      <fs_periodo>       TYPE lty_periodo,
      <fs_extrac_out>    TYPE zedu_extrac_out,
      <fs_authority>     TYPE hrms_bw_is_authority.


    "Obtiene la totalidad de periodos que tiene la ayuda de busqueda
    SELECT perid INTO TABLE lt_periodo
      FROM t7piqperiodt
      WHERE spras EQ 'S'.

    "Elimina los periodos que no hacen parte de la seleccion
    DELETE lt_periodo
      WHERE NOT perid IN ir_periodo.

    "Ordena los registos
    SORT lt_periodo BY perid.

    "Obtiene las facultades que tiene la ayuda de busqueda
    CALL FUNCTION 'ZDEDU_CONS_FACULTADES_CP'
      EXPORTING
        is_eventos           = 'X'
      TABLES
        ti_edu_cons_facultad = lt_facultad.

    "Elimina las facultades que no hacen parte de la seleccion
    DELETE lt_facultad
      WHERE NOT objid IN ir_facultad.

    "Ordena los registros
    SORT lt_facultad BY objid.

    "Si se tiene filtro por tipo de documento
    IF iv_tipo_doc IS NOT INITIAL.
      "Asigna el filtro al rango y crea el registro
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = iv_tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    "Obtiene los registros de extension
    SELECT *
      FROM zedu_pd_movext
      INTO TABLE et_act_extension
      WHERE peryr           EQ iv_anio
        AND perid           IN ir_periodo
        AND tipo_identifica IN lr_tipo_doc
        AND nro_identifica  IN ir_num_doc
        AND cod_act         EQ iv_cod_act
        AND tipo_act        IN ir_tipo_act
        AND tipo_sac        IN ir_tipo_sac
        AND facultad        IN ir_facultad.

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
        "Recorre los registros de extension
        LOOP AT et_act_extension ASSIGNING <fs_act_extension>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_extension>-facultad.
          IF sy-subrc NE 0.
            "Elimina el registro cuya facultad no esta autorizada
            DELETE et_act_extension INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

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
          et_act_extension,
          lt_facultad.
      ENDIF.
    ENDIF.

    "Ordena los registros de extension
    SORT et_act_extension.

    "Continua solo si se tiene facultades
    CHECK NOT lt_facultad IS INITIAL.

    "Recorre todos los posibles periodos que pueden ser seleccionados
    LOOP AT lt_periodo ASSIGNING <fs_periodo>.
      "Inicializa declaraciones
      CLEAR:
        lv_ultimo,
        lt_read_docentes,
        lt_entrada.

      "Obtiene los docentes del periodo seleccionado
*      CALL FUNCTION 'Z_EDU_RFC_READ_DOCENTES'
      CALL FUNCTION 'Z_EDU_RFC_READ_DOCENTES_V2'
        EXPORTING
          peryr      = iv_anio
          perid      = <fs_periodo>-perid
        IMPORTING
          t_docentes = lt_read_docentes
        EXCEPTIONS
          no_data    = 1
          OTHERS     = 2.

      "Elimina los docentes que no hacen parte de la seleccion
      DELETE lt_read_docentes
        WHERE NOT tipo_id IN lr_tipo_doc
          OR  NOT num_id  IN ir_num_doc.

      "Recorre los docentes del periodo
      LOOP AT lt_read_docentes ASSIGNING <fs_read_docentes>.
        "Si es el ultimo registro
        AT LAST.
          "Indica que es el ultimo registro
          lv_ultimo = abap_true.
        ENDAT.

        "Recorre las facultades
        LOOP AT lt_facultad ASSIGNING <fs_facultad>.
          "Inicializa declaraciones
          CLEAR:
            ls_entrada.

          "Asigna los datos a consultar y crea el registro
          ls_entrada-facultad   = <fs_facultad>-objid.
          ls_entrada-id_docente = <fs_read_docentes>-num_id.
          APPEND ls_entrada TO lt_entrada.
        ENDLOOP.

        "Obtiene la cantidad de registros actuales a consultar
        DESCRIBE TABLE lt_entrada LINES lv_cantidad.

        "Continua solo si se llego al bloque o es el ultimo registro
        CHECK lv_cantidad GE '5000' OR lv_ultimo EQ abap_true.

        "Inicializa retorno
        CLEAR lt_extrac_out.

        "Obtiene los datos de Educación Continua
*        CALL FUNCTION 'Z_EDU_EXTRACT_CURSOS_EDU_CONTI'
        CALL FUNCTION 'Z_EDU_EXTR_CURSOS_EDU_CONTI_V2'
          EXPORTING
            i_peryr = iv_anio
            i_perid = <fs_periodo>-perid
          TABLES
            lt_in   = lt_entrada
            lt_out  = lt_extrac_out.

        "Recorre los registros obtenidos
        LOOP AT lt_extrac_out ASSIGNING <fs_extrac_out>.
          "Crea un registro inicial en el retorno
          APPEND INITIAL LINE TO et_act_continua
            ASSIGNING <fs_act_continua>.
          "Asigna los datos del registro
          MOVE-CORRESPONDING <fs_extrac_out> TO <fs_act_continua>.
          "Asigna el año y periodo de la consulta
          <fs_act_continua>-peryr = iv_anio.
          <fs_act_continua>-perid = <fs_periodo>-perid.
        ENDLOOP.

        "Inicializa declaraciones
        CLEAR:
          lv_cantidad,
          lt_entrada.
      ENDLOOP.
    ENDLOOP.

    "Ordena los registros de continua
    SORT et_act_continua.

  ENDMETHOD.


  METHOD get_act_extension_v2.

    DATA:
      lt_entrada         TYPE TABLE OF zedu_extrac_curs,
      ls_act_extension   TYPE zedu_pd_movext,
      ls_entrada         TYPE zedu_extrac_curs,
      lv_tabix           TYPE sy-tabix,
      lv_super_usuario   TYPE  c,
      lv_decano          TYPE  c,
      lv_jefe_programa   TYPE  c,
      lv_administrador   TYPE  c,
      lv_docente         TYPE  c,
      lv_begda           TYPE  datum,
      lv_endda           TYPE  datum,
      lv_memory_name(30) TYPE  c,
      lr_tipo_doc        TYPE RANGE OF ictyp,
      lrs_tipo_doc       LIKE LINE OF lr_tipo_doc,
      lr_num_doc         TYPE RANGE OF psg_idnum,
      lrs_num_doc        LIKE LINE OF lr_num_doc,
      lr_facultad        TYPE RANGE OF hrobjid,
      lrs_facultad       LIKE LINE OF lr_facultad,
      e_t_authority      TYPE STANDARD TABLE OF hrms_bw_is_authority.

    FIELD-SYMBOLS:
      <fs_act_extension> TYPE zedu_pd_movext,
      <fs_act_continua>  TYPE zedu_extrac_out,
      <fs_authority>     TYPE hrms_bw_is_authority.


    IF NOT iv_tipo_doc IS INITIAL.
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = iv_tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    IF NOT iv_num_doc IS INITIAL.
      lrs_num_doc     = 'IEQ'.
      lrs_num_doc-low = iv_num_doc.
      APPEND lrs_num_doc TO lr_num_doc.

      SELECT SINGLE icnum
        INTO ls_entrada-id_docente
        FROM pa0185
        WHERE ictyp IN lr_tipo_doc
          AND icnum EQ iv_num_doc.
    ENDIF.

    IF NOT iv_facultad IS INITIAL.
      lrs_facultad     = 'IEQ'.
      lrs_facultad-low = iv_facultad.
      APPEND lrs_facultad TO lr_facultad.
      ls_entrada-facultad   = iv_facultad.
    ENDIF.

    IF NOT ls_entrada IS INITIAL.
      APPEND ls_entrada TO lt_entrada.
    ENDIF.

    SELECT *
      FROM zedu_pd_movext
      INTO TABLE et_act_extension
      WHERE peryr           EQ iv_anio
        AND perid           EQ iv_periodo
        AND tipo_identifica IN lr_tipo_doc
        AND nro_identifica  IN lr_num_doc
        AND cod_act         EQ iv_cod_act
        AND tipo_act        IN ir_tipo_act
        AND tipo_sac        IN ir_tipo_sac
        AND facultad        IN lr_facultad.

*    CALL FUNCTION 'Z_EDU_EXTRACT_CURSOS_EDU_CONTI'
    CALL FUNCTION 'Z_EDU_EXTR_CURSOS_EDU_CONTI_V2'
      EXPORTING
        i_peryr = iv_anio
        i_perid = iv_periodo
      TABLES
        lt_in   = lt_entrada
        lt_out  = et_act_continua.

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

    IMPORT gt_perfiles TO e_t_authority FROM SHARED MEMORY indx(aa) ID 'MC'.

    IF lv_decano EQ 'X' OR lv_jefe_programa EQ 'X'.
      IF e_t_authority[] IS NOT INITIAL.
        LOOP AT et_act_extension ASSIGNING <fs_act_extension>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_extension>-facultad.
          IF sy-subrc NE 0.
            DELETE et_act_extension INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

        LOOP AT et_act_continua ASSIGNING <fs_act_continua>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_continua>-facultad.
          IF sy-subrc NE 0.
            DELETE et_act_continua INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

      ELSE.
        REFRESH:
          et_act_extension,
          et_act_continua.
      ENDIF.
    ENDIF.

    SORT et_act_extension.
    SORT et_act_continua.

  ENDMETHOD.


  METHOD get_act_investigacion.

    DATA:
      lv_tabix           TYPE sy-tabix,
      lv_super_usuario   TYPE  c,
      lv_decano          TYPE  c,
      lv_jefe_programa   TYPE  c,
      lv_administrador   TYPE  c,
      lv_docente         TYPE  c,
      lv_begda           TYPE  datum,
      lv_endda           TYPE  datum,
      lv_memory_name(30) TYPE  c,
      lr_tipo_doc        TYPE RANGE OF ictyp,
      lrs_tipo_doc       LIKE LINE OF lr_tipo_doc,
      e_t_authority      TYPE STANDARD TABLE OF hrms_bw_is_authority.

    FIELD-SYMBOLS:
      <fs_act_investigacion> TYPE zedu_pd_movinv,
      <fs_authority>         TYPE hrms_bw_is_authority.


    IF iv_tipo_doc IS NOT INITIAL.
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = iv_tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    SELECT *
      FROM zedu_pd_movinv
      INTO TABLE rt_act_investigacion
      WHERE peryr           EQ iv_anio
        AND perid           IN ir_periodo
        AND tipo_identifica IN lr_tipo_doc
        AND nro_identifica  IN ir_num_doc
        AND cod_act         EQ iv_cod_act
        AND tipo_act        IN ir_tipo_act
        AND tipo_sac        IN ir_tipo_sac
        AND facultad        IN ir_facultad.

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

    IMPORT gt_perfiles TO e_t_authority FROM SHARED MEMORY indx(aa) ID 'MC'.

    IF lv_decano EQ 'X' OR lv_jefe_programa EQ 'X'.
      IF e_t_authority[] IS NOT INITIAL.
        LOOP AT rt_act_investigacion ASSIGNING <fs_act_investigacion>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_investigacion>-facultad.
          IF sy-subrc NE 0.
            DELETE rt_act_investigacion INDEX lv_tabix.
          ENDIF.
        ENDLOOP.
      ELSE.
        REFRESH rt_act_investigacion.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_act_otras.

    DATA:
      lv_tabix           TYPE sy-tabix,
      lv_super_usuario   TYPE  c,
      lv_decano          TYPE  c,
      lv_jefe_programa   TYPE  c,
      lv_administrador   TYPE  c,
      lv_docente         TYPE  c,
      lv_begda           TYPE  datum,
      lv_endda           TYPE  datum,
      lv_memory_name(30) TYPE  c,
      lr_tipo_doc        TYPE RANGE OF ictyp,
      lrs_tipo_doc       LIKE LINE OF lr_tipo_doc,
      e_t_authority      TYPE STANDARD TABLE OF hrms_bw_is_authority.

    FIELD-SYMBOLS:
      <fs_act_otras> TYPE zedu_pd_movotr,
      <fs_authority> TYPE hrms_bw_is_authority.


    IF iv_tipo_doc IS NOT INITIAL.
      lrs_tipo_doc     = 'IEQ'.
      lrs_tipo_doc-low = iv_tipo_doc.
      APPEND lrs_tipo_doc TO lr_tipo_doc.
    ENDIF.

    SELECT *
      FROM zedu_pd_movotr
      INTO TABLE rt_act_otras
      WHERE peryr           EQ iv_anio
        AND perid           IN ir_periodo
        AND tipo_identifica IN lr_tipo_doc
        AND nro_identifica  IN ir_num_doc
        AND cod_act         EQ iv_cod_act
        AND tipo_act        IN ir_tipo_act
        AND tipo_sac        IN ir_tipo_sac
        AND facultad        IN ir_facultad.

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

    IMPORT gt_perfiles TO e_t_authority FROM SHARED MEMORY indx(aa) ID 'MC'.

    IF lv_decano EQ 'X' OR lv_jefe_programa EQ 'X'.
      IF e_t_authority[] IS NOT INITIAL.
        LOOP AT rt_act_otras ASSIGNING <fs_act_otras>.
          lv_tabix = sy-tabix.
          READ TABLE e_t_authority ASSIGNING <fs_authority>
            WITH KEY objid = <fs_act_otras>-facultad.
          IF sy-subrc NE 0.
            DELETE rt_act_otras INDEX lv_tabix.
          ENDIF.
        ENDLOOP.
      ELSE.
        REFRESH rt_act_otras.
      ENDIF.
    ENDIF.

  ENDMETHOD.


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


  METHOD get_desc_act_sac.

    "Inicializa retornos
    CLEAR:
      et_pd_tipoact,
      et_pd_tiposac.

    "Obtiene las descripciones de los tipos de actividad
    SELECT *
      INTO TABLE et_pd_tipoact
      FROM zedu_pd_tipoact
      WHERE cod_act EQ iv_cod_act.

    "Obtiene las descripciones de los subtipos de actividad
    SELECT *
      INTO TABLE et_pd_tiposac
      FROM zedu_pd_tiposac
      WHERE cod_act EQ iv_cod_act.

    "Ordena los registros
    SORT et_pd_tipoact BY cod_act tipo_act.
    SORT et_pd_tiposac BY cod_act tipo_act tipo_sac.

  ENDMETHOD.


  METHOD get_dropdown_key_docente.

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


  method SET_TEXT_COLUMN_ALV.

    data: lr_column_header  type ref to cl_salv_wd_column_header .

    lr_column_header = cs_column-r_column->get_header( )  .
    lr_column_header->set_ddic_binding_field( if_salv_wd_c_column_settings=>ddic_bind_none )     .
    lr_column_header->set_text( me->if_wd_component_assistance~get_text( iv_text ) ).

  endmethod.
ENDCLASS.
