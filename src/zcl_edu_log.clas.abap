class ZCL_EDU_LOG definition
  public
  final
  create public .

public section.

  types:
*"* public components of class ZCL_EDU_LOG
*"* do not include other source files here!!!
*"* protected components of class ZCL_EDU_LOG
*"* do not include other source files here!!!
    BEGIN OF gtp_es_vigencia,
      fecha_ini   TYPE datum,
      fecha_fin   TYPE datum,
      hora_ini    TYPE uzeit,
      hora_fin    TYPE uzeit,
      END OF gtp_es_vigencia .

  data GS_OBJECT type BALOBJ_D .

  methods CONSTRUCTOR
    importing
      !I_S_OBJECT type BALOBJ_D
      !I_S_SUBOBJECT type BALSUBOBJ optional
      !I_ES_SYST type SYST optional
      !I_ES_VIGENCIA type GTP_ES_VIGENCIA optional .
  methods SAVE
    importing
      value(I_TI_LOG_TAB) type STANDARD TABLE
      value(I_IN_UPDATE_TASK) type CHECK optional
      value(I_DATE_DEL) type ALDATE_DEL optional .
  methods DISPLAY
    importing
      !I_S_TITULO type STRING optional .
  class-methods DISPLAY_LOG
    importing
      !I_S_TITULO type STRING optional
      !I_S_OBJECT type BALOBJ_D
      !I_S_SUBOBJECT type BALSUBOBJ optional
      !I_ES_VIGENCIA type GTP_ES_VIGENCIA optional
      !I_ES_SYST type SYST optional
      !I_C_HANDLE type BALLOGHNDL optional .
  class-methods ADD_MESSAGE
    importing
      !I_C_MSGTY type BAPIRET2-TYPE
      !I_C_MSGID type BAPIRET2-ID
      !I_N_MSGNO type BAPIRET2-NUMBER
      !I_C_MESSAGE type BAPIRET2-MESSAGE optional
      !I_C_MSGV1 type BAPIRET2-MESSAGE_V1 optional
      !I_C_MSGV2 type BAPIRET2-MESSAGE_V2 optional
      !I_C_MSGV3 type BAPIRET2-MESSAGE_V3 optional
      !I_C_MSGV4 type BAPIRET2-MESSAGE_V4 optional
      !I_C_PARAMETER type BAPI_PARAM optional
      !I_I_ROW type BAPI_LINE optional
      !I_C_FIELD type BAPI_FLD optional
    changing
      !C_TI_RETURN type BAPIRET2_TAB .
  class-methods ADD_MSG_STR
    importing
      !I_C_MSGTY type BAPIRET2-TYPE
      !I_C_MSGID type BAPIRET2-ID optional
      !I_N_MSGNO type BAPIRET2-NUMBER optional
      !I_S_MSG type STRING optional
      !I_C_PARAMETER type BAPI_PARAM optional
      !I_I_ROW type BAPI_LINE optional
      !I_C_FIELD type BAPI_FLD optional
    changing
      !C_TI_RETURN type BAPIRET2_TAB .
  class-methods ADD_MSG_PROXY
    importing
      !I_C_MSGTY type BAPIRET2-TYPE
      !I_C_MSGID type BAPIRET2-ID optional
      !I_N_MSGNO type BAPIRET2-NUMBER optional
      !I_C_COD_ERROR type SYMSGV
      !I_S_MSG_ERROR type STRING
      !I_C_PARAMETER type BAPI_PARAM optional
      !I_I_ROW type BAPI_LINE optional
      !I_C_FIELD type BAPI_FLD optional
    changing
      !C_TI_RETURN type BAPIRET2_TAB .
  class-methods SPLIT_MESSAGE
    importing
      !I_S_TEXT type STRING
    exporting
      !E_C_MSGV1 type SYMSGV
      !E_C_MSGV2 type SYMSGV
      !E_C_MSGV3 type SYMSGV
      !E_C_MSGV4 type SYMSGV .
  methods SAVE_MESSAGES
    importing
      value(I_TI_LOG_TAB) type STANDARD TABLE
      value(I_IN_UPDATE_TASK) type CHECK optional
      value(I_COMMIT) type CHECK optional .
  methods SAVE_MSG_STR
    importing
      !I_C_MSGTY type BAPIRET2-TYPE
      !I_C_MSGID type BAPIRET2-ID optional
      !I_N_MSGNO type BAPIRET2-NUMBER optional
      !I_S_MSG type STRING optional
      !I_C_PARAMETER type BAPI_PARAM optional
      !I_I_ROW type BAPI_LINE optional
      !I_C_FIELD type BAPI_FLD optional
      !I_IN_UPDATE_TASK type CHECK optional
      !I_COMMIT type CHECK optional
    changing
      !C_TI_RETURN type BAPIRET2_TAB .
  methods SAVE_MESSAGE
    importing
      !I_C_MSGTY type BAPIRET2-TYPE
      !I_C_MSGID type BAPIRET2-ID
      !I_N_MSGNO type BAPIRET2-NUMBER
      !I_C_MESSAGE type BAPIRET2-MESSAGE optional
      !I_C_MSGV1 type BAPIRET2-MESSAGE_V1 optional
      !I_C_MSGV2 type BAPIRET2-MESSAGE_V2 optional
      !I_C_MSGV3 type BAPIRET2-MESSAGE_V3 optional
      !I_C_MSGV4 type BAPIRET2-MESSAGE_V4 optional
      !I_C_PARAMETER type BAPI_PARAM optional
      !I_I_ROW type BAPI_LINE optional
      !I_C_FIELD type BAPI_FLD optional
      !I_IN_UPDATE_TASK type CHECK optional
      !I_COMMIT type CHECK optional
    changing
      !C_TI_RETURN type BAPIRET2_TAB .
protected section.
*"* private components of class ZCL_EDU_LOG
*"* do not include other source files here!!!
private section.

  data GS_SUBOBJECT type BALSUBOBJ .
  data GES_SYST type SYST .
  data GES_VIGENCIA type GTP_ES_VIGENCIA .
  data GC_LOG_HANDLE type BALLOGHNDL .
ENDCLASS.



CLASS ZCL_EDU_LOG IMPLEMENTATION.


METHOD ADD_MESSAGE.

  DATA: les_return    TYPE bapiret2,
        lc_message    TYPE bapiret2-message.

  lc_message = i_c_message.

  IF lc_message IS INITIAL OR i_c_message IS NOT SUPPLIED OR lc_message EQ space.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = i_c_msgid
        lang      = 'S'
        no        = i_n_msgno
        v1        = i_c_msgv1
        v2        = i_c_msgv2
        v3        = i_c_msgv3
        v4        = i_c_msgv4
      IMPORTING
        msg       = lc_message
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
  ENDIF.

  les_return-type      = i_c_msgty.
  les_return-id        = i_c_msgid.
  les_return-number    = i_n_msgno.
  les_return-message   = lc_message.
  les_return-message_v1 = i_c_msgv1.
  les_return-message_v2 = i_c_msgv2.
  les_return-message_v3 = i_c_msgv3.
  les_return-message_v4 = i_c_msgv4.
  les_return-parameter  = i_c_parameter.
  les_return-row = i_i_row.
  les_return-field = i_c_field.
  les_return-log_no = ''.
  les_return-log_msg_no = ''.
  les_return-system = sy-sysid.
  APPEND les_return TO c_ti_return.

ENDMETHOD.


METHOD ADD_MSG_PROXY.

  DATA: ls_error      TYPE string,
        lc_cod_error  TYPE symsgv,
        lc_msgv1      TYPE symsgv,
        lc_msgv2      TYPE symsgv,
        lc_msgv3      TYPE symsgv,
        lc_msgv4      TYPE symsgv,
        lc_msgid      TYPE symsgid,
        lc_msgno      TYPE symsgno.

  lc_cod_error = i_c_cod_error.
  CONDENSE lc_cod_error.
  ls_error = i_s_msg_error.
  IF lc_cod_error IS INITIAL.
    CONCATENATE 'Error:' ls_error INTO ls_error SEPARATED BY space.
  ELSE.
    CONCATENATE 'Cod.Error:' lc_cod_error 'Error:' ls_error INTO ls_error SEPARATED BY space.
  ENDIF.

  zcl_edu_log=>add_msg_str( EXPORTING  i_c_msgty     = i_c_msgty
                                      i_s_msg       = ls_error
                                      i_c_parameter = i_c_parameter
                           CHANGING   c_ti_return   = c_ti_return ).

ENDMETHOD.


METHOD ADD_MSG_STR.

  DATA: lc_msgv1  TYPE symsgv,
        lc_msgv2  TYPE symsgv,
        lc_msgv3  TYPE symsgv,
        lc_msgv4  TYPE symsgv,
        lc_msgid  TYPE symsgid,
        lc_msgno  TYPE symsgno.

  IF i_c_msgid IS INITIAL OR i_n_msgno IS INITIAL.
    lc_msgid = 'ZDEDU_RECAUDACION'.
    lc_msgno = '043'.
  ELSE.
    lc_msgid = i_c_msgid.
    lc_msgno = i_n_msgno.
  ENDIF.

  zcl_edu_log=>split_message( EXPORTING i_s_text   = i_s_msg
                             IMPORTING e_c_msgv1  = lc_msgv1
                                       e_c_msgv2  = lc_msgv2
                                       e_c_msgv3  = lc_msgv3
                                       e_c_msgv4  = lc_msgv4 ).

  zcl_edu_log=>add_message( EXPORTING   i_c_msgty     = i_c_msgty
                                       i_c_msgid     = lc_msgid
                                       i_n_msgno     = lc_msgno
                                       i_c_msgv1     = lc_msgv1
                                       i_c_msgv2     = lc_msgv2
                                       i_c_msgv3     = lc_msgv3
                                       i_c_msgv4     = lc_msgv4
                                       i_c_parameter = i_c_parameter
                                       i_i_row       = i_i_row
                                       i_c_field     = i_c_field
                            CHANGING   c_ti_return   = c_ti_return ).

ENDMETHOD.


METHOD CONSTRUCTOR.

  DATA: les_log_tab     TYPE bapiret2,
        les_log         TYPE bal_s_log,
        les_mdef        TYPE bal_s_mdef.

  gs_object = i_s_object.
  gs_subobject = i_s_subobject.

* Se copian los valores del sistema
  IF i_es_syst IS NOT INITIAL.
    ges_syst = i_es_syst.
  ELSE.
    MOVE-CORRESPONDING sy TO ges_syst.
  ENDIF.

* Se inicializa la fecha y hora en que se comezara a grabar el log
  IF i_es_vigencia IS NOT INITIAL.
    MOVE-CORRESPONDING i_es_vigencia TO ges_vigencia.
  ELSE.
    ges_vigencia-fecha_ini = sy-datum.
    ges_vigencia-hora_ini = sy-uzeit.
  ENDIF.


  CLEAR les_log.
  les_log-object    = gs_object.
  les_log-subobject = gs_subobject.
  les_log-aldate    = ges_syst-datum.
  les_log-altime    = ges_syst-uzeit.
  les_log-aluser    = ges_syst-uname.
  les_log-altcode   = ges_syst-tcode.
  les_log-alprog    = ges_syst-cprog.
* Se asigna la identificacion externa para el log
  CONCATENATE 'Mensajes' gs_object INTO les_log-extnumber SEPARATED BY space.

* En caso de no existir el Objeto Log, se produce una excepcion LOG_HEADER_INCONSISTENT.
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = les_log
    IMPORTING
      e_log_handle            = gc_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CLEAR les_mdef.
  les_mdef-log_handle = gc_log_handle.

* Asigna los valores por defecto
  CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
    EXPORTING
      i_s_msg_defaults = les_mdef
    EXCEPTIONS
      OTHERS           = 0.

ENDMETHOD.


METHOD DISPLAY.

  ges_vigencia-fecha_fin = sy-datum.
  ges_vigencia-hora_fin = sy-uzeit.

* Se invoca el metodo estatico DISPLAY_LOG con los atributos del objeto
  zcl_edu_log=>display_log( EXPORTING i_s_titulo    = i_s_titulo
                                     i_s_object    = gs_object
                                     i_s_subobject = gs_subobject
                                     i_es_vigencia = ges_vigencia
                                     i_c_handle    = gc_log_handle ).

ENDMETHOD.


METHOD DISPLAY_LOG.

  TYPE-POOLS: abap.

  DATA: les_display_profile   TYPE bal_s_prof,
        les_fcat              TYPE bal_s_fcat,
        les_vigencia          TYPE gtp_es_vigencia,
        ls_titulo             TYPE string,
        lf_fecha(10)          TYPE c.

  les_vigencia = i_es_vigencia.
  IF les_vigencia-fecha_ini IS INITIAL.
    les_vigencia-fecha_ini = sy-datum.
  ENDIF.
  IF les_vigencia-hora_ini IS INITIAL.
    les_vigencia-hora_ini = '000000'.
  ENDIF.
  IF les_vigencia-fecha_fin IS INITIAL.
    les_vigencia-fecha_fin = sy-datum.
  ENDIF.
  IF les_vigencia-hora_fin IS INITIAL.
    les_vigencia-hora_fin = sy-uzeit.
  ENDIF.

**********************************************************************
* macro to simplify creation of field catalog entry
**********************************************************************
  DEFINE macro_add_fields.
    clear les_fcat.
    if not &8 is initial.
      concatenate 'BAL_S_SHOW' '_' &8 into les_fcat-ref_table.
    else.
      les_fcat-ref_table = 'BAL_S_SHOW'.
    endif.
    if &2 = 0.
      les_fcat-no_out    = abap_true.
    else.
      les_fcat-col_pos   = &2.
    endif.
    les_fcat-is_treecol = &3.
    les_fcat-ref_field  = &4.
    les_fcat-outputlen  = &5.
    les_fcat-cltxt_add  = &6.
    les_fcat-colddictxt = &7.
    append les_fcat to les_display_profile-&1_fcat.
  END-OF-DEFINITION.

**********************************************************************
* define fields in tree and list
**********************************************************************
  macro_add_fields:
*   Level Col InList FieldName    Len AddColumnText DDIC-Text reftab_ext
*   lev1
    lev1  0   abap_false  'LOG_HANDLE' 0   abap_false         ' '       ' ',
    lev1  1   abap_false  'ALDATE'     0   abap_false         ' '       ' ',
    lev1  2   abap_false  'ALTIME'     0   abap_false         ' '       ' ',
    lev1  3   abap_false  'ALUSER'     0   abap_false         ' '       ' ',
    lev1  4   abap_true   'EXTNUMBER'  20  abap_false         'R'       ' ',
    lev1  5   abap_true   'T_OBJECT'   20  abap_false         'R'       ' ',
    lev1  6   abap_true   'T_SUBOBJ'   20  abap_false         'R'       ' ',
    lev1  7   abap_true   'ALTCODE'    14  abap_false         'R'       ' ',
    lev1  8   abap_true   'ALPROG'     15  abap_false         ' '       ' ',
    lev1  9   abap_true   'T_ALMODE'   15  abap_false         ' '       ' ',
    lev1 10   abap_true   'LOGNUMBER'  30  abap_false         'L'       ' ',
*   lev2
    lev2  0   abap_false  'PROBCLASS'  0   abap_false         ' '       ' ',
    lev2  1   abap_false  'T_PROBCLSS' 0   abap_true          'R'       ' ',
*   mess
    mess  0   abap_false  'LOGNUMBER'  0   abap_false         ' '       ' ',
    mess  0   abap_false  'MSGNUMBER'  0   abap_false         ' '       ' ',
    mess  2   abap_false  'T_MSG'      85  abap_false         ' '       ' ',
    mess  0   abap_false  'MSGTY'      0   abap_false         ' '       ' ',
    mess  0   abap_false  'MSGID'      0   abap_false         ' '       ' ',
    mess  0   abap_false  'MSGNO'      0   abap_false         ' '       ' ',
    mess  0   abap_false  'DETLEVEL'   0   abap_false         ' '       ' ',
    mess  0   abap_false  'T_PROBCLSS' 0   abap_false         ' '       ' ',
    mess  0   abap_false  'ALSORT'     0   abap_false         ' '       ' ',
    mess  0   abap_false  'MSG_COUNT'  0   abap_false         ' '       ' ',
* for color-information
    mess  0   abap_false  'TABCOL'     0   abap_false         ' '       'COL'.


  les_display_profile-show_all = 'X'.
  les_display_profile-disvariant-report = i_es_syst-cprog.
  les_display_profile-disvariant-username = i_es_syst-uname.
  les_display_profile-disvariant-handle = i_c_handle.
  les_display_profile-use_grid = 'X'.
  les_display_profile-no_toolbar = ''.
  IF i_s_titulo IS INITIAL OR i_s_titulo IS NOT SUPPLIED.
    ls_titulo = i_s_titulo.
  ELSE.
    ls_titulo = 'Visualización de Log'.
  ENDIF.
  les_display_profile-title = ls_titulo.
  WRITE les_vigencia-fecha_ini DD/MM/YYYY TO lf_fecha.
  CONCATENATE 'Log' lf_fecha INTO les_display_profile-root_text SEPARATED BY space.

  CALL FUNCTION 'BAL_DSP_OUTPUT_INIT'
    EXPORTING
      i_s_display_profile = les_display_profile.

  CALL FUNCTION 'APPL_LOG_DISPLAY'
    EXPORTING
      object                    = i_s_object
      subobject                 = i_s_subobject
      date_from                 = les_vigencia-fecha_ini
      time_from                 = les_vigencia-hora_ini
      date_to                   = les_vigencia-fecha_fin
      time_to                   = les_vigencia-hora_fin
      suppress_selection_dialog = 'X'
      i_s_display_profile       = les_display_profile
      i_srt_by_timstmp          = 'X'
    EXCEPTIONS
      no_authority              = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD SAVE.

   data: les_log_tab     type bapiret2,
         les_log         type bal_s_log,
         les_mdef        type bal_s_mdef,
         les_msg         type bal_s_msg,
         ls_dummy        type string.

  FIELD-SYMBOLS: <lfs_log_tab>  TYPE any.

  IF gc_log_handle IS INITIAL.
    CLEAR les_log.
    les_log-object     = gs_object.
    les_log-subobject  = gs_subobject.
    les_log-aldate     = ges_syst-datum.
    les_log-altime     = ges_syst-uzeit.
    les_log-aluser     = ges_syst-uname.
    les_log-altcode    = ges_syst-tcode.
    les_log-alprog     = ges_syst-cprog.

*   Se asigna la fecha de vencimiento en caso de haber sido enviada
    IF i_date_del IS NOT INITIAL.
      les_log-aldate_del = i_date_del.
      les_log-del_before = 'X'.
    ENDIF.

*   Se asigna la identificacion externa para el log
    CONCATENATE 'Mensajes' gs_object INTO les_log-extnumber SEPARATED BY space.

*   En caso de no existir el Objeto Log, se produce una excepcion LOG_HEADER_INCONSISTENT.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = les_log
      IMPORTING
        e_log_handle            = gc_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR les_mdef.
    les_mdef-log_handle = gc_log_handle.

*   Asigna los valores por defecto
    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = les_mdef
      EXCEPTIONS
        OTHERS           = 0.
  ENDIF.

  LOOP AT i_ti_log_tab ASSIGNING <lfs_log_tab>.
    MOVE-CORRESPONDING <lfs_log_tab> TO les_log_tab.

    MESSAGE ID les_log_tab-id TYPE les_log_tab-type NUMBER les_log_tab-number
            WITH les_log_tab-message_v1   les_log_tab-message_v2
                 les_log_tab-message_v3   les_log_tab-message_v4
            INTO ls_dummy.

    les_msg-msgty = sy-msgty.
    les_msg-msgid = sy-msgid.
    les_msg-msgno = sy-msgno.
    les_msg-msgv1 = sy-msgv1.
    les_msg-msgv2 = sy-msgv2.
    les_msg-msgv3 = sy-msgv3.
    les_msg-msgv4 = sy-msgv4.
    les_msg-alsort = sy-msgty.

*   Asignacion de la clase de problema para que agrupe por E, W y S
    CASE les_msg-msgty.
      WHEN 'E'.
        les_msg-probclass = 1.
      WHEN 'W'.
        les_msg-probclass = 2.
      WHEN 'S'.
        les_msg-probclass = 3.
      WHEN OTHERS.
    ENDCASE.

*   Adiciona la linea o mensaje al Application Log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = les_msg
        i_log_handle  = gc_log_handle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

* Guarda el log completo en la base de datos
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = i_in_update_task
      i_save_all       = 'X'
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


METHOD SAVE_MESSAGE.

  DATA: lti_return    TYPE bapiret2_tab,
        les_return    TYPE bapiret2,
        lc_message    TYPE bapiret2-message.

  zcl_edu_log=>add_message( EXPORTING  i_c_msgty     = i_c_msgty       i_c_msgid = i_c_msgid
                                      i_n_msgno     = i_n_msgno       i_c_message = i_c_message
                                      i_c_msgv1     = i_c_msgv1       i_c_msgv2     = i_c_msgv2
                                      i_c_msgv3     = i_c_msgv3       i_c_msgv4     = i_c_msgv4
                                      i_c_parameter = i_c_parameter   i_i_row       = i_i_row
                                      i_c_field     = i_c_field
                           CHANGING   c_ti_return   = lti_return ).

  me->save_messages( i_ti_log_tab     = lti_return
                     i_in_update_task = i_in_update_task
                     i_commit         = i_commit ).

ENDMETHOD.


METHOD SAVE_MESSAGES.

  DATA: les_log_tab     TYPE bapiret2,
        les_log         TYPE bal_s_log,
        les_msg         TYPE bal_s_msg,
        ls_dummy        TYPE string,
        lti_handle      TYPE bal_t_logh,
        lwa_handle      LIKE LINE OF lti_handle.

  FIELD-SYMBOLS: <lfs_log_tab>  TYPE any.

  LOOP AT i_ti_log_tab ASSIGNING <lfs_log_tab>.
    MOVE-CORRESPONDING <lfs_log_tab> TO les_log_tab.

    MESSAGE ID les_log_tab-id TYPE les_log_tab-type NUMBER les_log_tab-number
            WITH les_log_tab-message_v1   les_log_tab-message_v2
                 les_log_tab-message_v3   les_log_tab-message_v4
            INTO ls_dummy.

    les_msg-msgty = sy-msgty.
    les_msg-msgid = sy-msgid.
    les_msg-msgno = sy-msgno.
    les_msg-msgv1 = sy-msgv1.
    les_msg-msgv2 = sy-msgv2.
    les_msg-msgv3 = sy-msgv3.
    les_msg-msgv4 = sy-msgv4.
    les_msg-alsort = sy-msgty.

*   Asignacion de la clase de problema para que agrupe por E, W y S
    CASE les_msg-msgty.
      WHEN 'E'.
        les_msg-probclass = 1.
      WHEN 'W'.
        les_msg-probclass = 2.
      WHEN 'S'.
        les_msg-probclass = 3.
      WHEN OTHERS.
    ENDCASE.

*   Adiciona la linea o mensaje al Application Log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = les_msg
        i_log_handle  = gc_log_handle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.


  lwa_handle = gc_log_handle.
  APPEND lwa_handle TO lti_handle.

* Guarda el log completo en la base de datos
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_in_update_task = i_in_update_task
      i_save_all       = 'X'
      i_t_log_handle   = lti_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF i_in_update_task IS INITIAL AND i_commit IS NOT INITIAL.
    COMMIT WORK.
  ENDIF.

ENDMETHOD.


METHOD SAVE_MSG_STR.

  DATA: lc_msgv1  TYPE symsgv,
        lc_msgv2  TYPE symsgv,
        lc_msgv3  TYPE symsgv,
        lc_msgv4  TYPE symsgv,
        lc_msgid  TYPE symsgid,
        lc_msgno  TYPE symsgno.

  IF i_c_msgid IS INITIAL OR i_n_msgno IS INITIAL.
    lc_msgid = 'ZDEDU_RECAUDACION'.
    lc_msgno = '043'.
  ELSE.
    lc_msgid = i_c_msgid.
    lc_msgno = i_n_msgno.
  ENDIF.

  zcl_edu_log=>split_message( EXPORTING i_s_text   = i_s_msg
                             IMPORTING e_c_msgv1  = lc_msgv1
                                       e_c_msgv2  = lc_msgv2
                                       e_c_msgv3  = lc_msgv3
                                       e_c_msgv4  = lc_msgv4 ).

  me->save_message( EXPORTING   i_c_msgty         = i_c_msgty
                                i_c_msgid         = lc_msgid
                                i_n_msgno         = lc_msgno
                                i_c_msgv1         = lc_msgv1
                                i_c_msgv2         = lc_msgv2
                                i_c_msgv3         = lc_msgv3
                                i_c_msgv4         = lc_msgv4
                                i_c_parameter     = i_c_parameter
                                i_i_row           = i_i_row
                                i_c_field         = i_c_field
                                i_in_update_task  = i_in_update_task
                                i_commit          = i_commit
                     CHANGING   c_ti_return       = c_ti_return ).

ENDMETHOD.


METHOD SPLIT_MESSAGE.

  DATA: ln_contador     TYPE n,
        lc_namevar(15)  TYPE c,
        ls_texto        TYPE string.

  FIELD-SYMBOLS: <lfs_var>  TYPE any,
                 <lfs_line> TYPE any.

  TYPES: ltp_line(50) TYPE c,
         ltp_ti_lines TYPE STANDARD TABLE OF ltp_line.
  DATA: lt_tdline   TYPE ltp_ti_lines,
        lt_lines    TYPE STANDARD TABLE OF tline,
        lt_stream   TYPE string_table.

  IF i_s_text IS NOT INITIAL.
    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = i_s_text
      TABLES
        ftext_tab = lt_tdline.

    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      EXPORTING
        stream_lines = lt_stream
        language     = sy-langu
        lf           = ' '
      TABLES
        text_stream  = lt_tdline
        itf_text     = lt_lines.
  ENDIF.

  WHILE ln_contador LE 4.
    ln_contador = ln_contador + 1.
    CONCATENATE 'E_C_MSGV' ln_contador INTO lc_namevar.
    ASSIGN (lc_namevar) TO <lfs_var>.
    IF sy-subrc EQ 0.
      READ TABLE lt_tdline ASSIGNING <lfs_line> INDEX ln_contador.
      IF sy-subrc EQ 0.
        <lfs_var> = <lfs_line>.
      ENDIF.
    ENDIF.
  ENDWHILE.

ENDMETHOD.
ENDCLASS.
