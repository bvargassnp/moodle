*&---------------------------------------------------------------------*
*& Report  ZDREDU_ANUL_EST_ESTUD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdredu_anul_est_estud.

"Declaraciones
DATA:
  lv_nr_formulario TYPE zdedu_nr_formulario,
  lv_log_handle    TYPE balloghndl,
  ls_s_log         TYPE bal_s_log,
  ls_s_msg         TYPE bal_s_msg,
  ls_disp_profile  TYPE bal_s_prof,
  lr_nform         TYPE RANGE OF zdedu_nr_formulario,
  lt_t_logh        TYPE bal_t_logh,
  lt_t_logh_tot    TYPE bal_t_logh,
  lt_pre_adm_2     TYPE SORTED TABLE OF zpre_admi_2 WITH UNIQUE KEY primary_key COMPONENTS nr_formulario.

"Select-Options
SELECT-OPTIONS:
  pa_nform  FOR lv_nr_formulario NO INTERVALS NO-EXTENSION,
  so_nform  FOR lv_nr_formulario NO-DISPLAY.

"Parametros
PARAMETERS:
  pa_ev020 TYPE c NO-DISPLAY.


*&---------------------------------------------------------------------*
* Inicio de la ejecucion
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  "Si se lanzo el programa desde la transaccion
  IF pa_ev020 IS INITIAL.
    lr_nform[] = pa_nform[].

    "Si se lanzo el programa desde el evento 20
  ELSE.
    lr_nform[] = so_nform[].
  ENDIF.

  "Continua solo si se tienen datos
  CHECK NOT lr_nform IS INITIAL.

  "Obtiene la totalidad de datos de la tabla
  SELECT *
    INTO TABLE lt_pre_adm_2
    FROM zpre_admi_2
    WHERE nr_formulario IN lr_nform.

  "Continua solo si encuentra registros
  CHECK sy-subrc EQ 0.

  "Actualizar info de formularios
  LOOP AT lt_pre_adm_2 ASSIGNING FIELD-SYMBOL(<fs_pre_adm2>).
    "inicializa declaraciones
    CLEAR:
      lv_log_handle,
      ls_s_msg,
      lt_t_logh.

    "Asigna los datos del LOG
    ls_s_log-object = 'ZEDU_RECAUDACION'.
    ls_s_log-subobject = 'ZEDU_PREIN'.
    ls_s_log-extnumber = <fs_pre_adm2>-nr_formulario.

    "Crea el LOG
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_s_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    "Asigna los datos del mensaje del LOG
    ls_s_msg-msgid = 'ZDEDU_RECAUDACION'.
    ls_s_msg-msgty = 'S'.
    ls_s_msg-msgno = '061'.
    ls_s_msg-msgv1 = <fs_pre_adm2>-nr_formulario.
    ls_s_msg-msgv2 = sy-tcode.

    "Adiciona el mensaje al log
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = lv_log_handle
        i_s_msg          = ls_s_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    "Actualizar campos de pago y status
    <fs_pre_adm2>-fecha_pago = sy-datum.
    <fs_pre_adm2>-hora_pago = sy-uzeit.
    <fs_pre_adm2>-sta2_solpago = 'AN'.

    "Asigna el manejador del LOG
    APPEND lv_log_handle TO lt_t_logh.
    APPEND lv_log_handle TO lt_t_logh_tot.

    "Guarda el log
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = 'X'
        i_t_log_handle   = lt_t_logh
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
  ENDLOOP.

  "Modifica los estados de los formularios
  MODIFY zpre_admi_2 FROM TABLE lt_pre_adm_2.

  "Continua solo si el proceso no se lanzo desde el evento 20
  CHECK pa_ev020 IS INITIAL.

  "Establece los cambios
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  "Visualiza el log
  CALL FUNCTION 'APPL_LOG_DISPLAY'
    EXPORTING
      object                    = ls_s_log-object
      subobject                 = ls_s_log-subobject
      external_number           = ls_s_log-extnumber
      date_from                 = sy-datum
      time_from                 = '000000'
      date_to                   = sy-datum
      time_to                   = '235959'
      suppress_selection_dialog = 'X'
    EXCEPTIONS
      no_authority              = 1
      OTHERS                    = 2.

  "Si ocurre algun error
  IF sy-subrc NE 0.
    "Genera el mensaje
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
