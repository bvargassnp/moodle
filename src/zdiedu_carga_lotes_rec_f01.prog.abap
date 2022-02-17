*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CARGA_LOTES_REC_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_LOG
*&---------------------------------------------------------------------*
FORM f_create_log  CHANGING p_log_handle TYPE  balloghndl.

  DATA: e_s_log TYPE  bal_s_log.

  e_s_log-aluser    = sy-uname.
  e_s_log-alprog    = sy-repid.
  e_s_log-aldate    = sy-datum.
  e_s_log-altime    = sy-uzeit.
  e_s_log-object    = gc_object.
  e_s_log-subobject = gc_subobject.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = e_s_log
    IMPORTING
      e_log_handle            = p_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "f_create_log

*&---------------------------------------------------------------------*
*&      Form  F_LEER_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_leer_archivo .
  DATA: lv_line        TYPE string,
        lv_tipo(1)     TYPE c,
        lv_detalle(1)  TYPE c,
        lt_param       TYPE SORTED TABLE OF zedu_c_param WITH UNIQUE KEY primary_key COMPONENTS repid idparam idparampos,
        lr_aliass      TYPE RANGE OF dirname,
        ls_aliass      LIKE LINE OF lr_aliass,
        lt_user_dir    TYPE SORTED TABLE OF user_dir WITH UNIQUE KEY primary_key COMPONENTS dirname.

  DATA: isplit TYPE TABLE OF string WITH HEADER LINE.
  SELECT mandt repid idparam idparampos valor descripcion
   INTO TABLE lt_param
   FROM zedu_c_param
   WHERE repid = '' AND
         ( idparam = 'LOTEDIRERR' OR
           idparam = 'LOTEDIRIN'  OR
           idparam = 'LOTEDIROUT' ).

  ls_aliass-sign = 'I'.
  ls_aliass-option = 'EQ'.
  LOOP AT lt_param INTO DATA(ls_param).
    ls_aliass-low = ls_param-valor.
    APPEND ls_aliass TO lr_aliass.
  ENDLOOP.

  SELECT dirname aliass svrname sp_name sp_cs
    INTO TABLE lt_user_dir
    FROM user_dir
    FOR ALL ENTRIES IN lr_aliass
    WHERE aliass = lr_aliass-low.

  LOOP AT lt_user_dir INTO DATA(ls_user_dir).
    READ TABLE lt_param INTO ls_param WITH KEY valor = ls_user_dir-aliass.
    CASE ls_param-idparam.
      WHEN 'LOTEDIRERR'.
        gv_dir_err = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIRIN'.
        gv_path = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIROUT'.
        gv_dir_out = ls_user_dir-dirname && '\'.
    ENDCASE.
  ENDLOOP.


* Se obtienen los directorios lógicos
*  CALL FUNCTION 'Z_EDU_OBTENER_DIR_FIS_X_PRG'
*    EXPORTING
*      i_programa                = 'ZDREDU_CARGA_ARCH_REC'
*    IMPORTING
*      e_dir_in                  = gv_path
*      e_dir_out                 = gv_dir_out
*      e_dir_err                 = gv_dir_err
*    EXCEPTIONS
*      programa_no_parametrizado = 1
*      OTHERS                    = 2.




* Abrir Archivo
  gv_arch = p_arch.
  CONCATENATE gv_dir_out p_arch INTO p_arch.
  CONCATENATE gv_dir_out 'tmp_' gv_arch INTO gv_file.
*  WAIT UP TO 6 SECONDS.
  OPEN DATASET p_arch FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc EQ 0.
*   Leer registros
    DO.
      CLEAR: lv_line, isplit[], isplit, lv_tipo.
      READ DATASET p_arch INTO lv_line.
      IF sy-subrc EQ 0.
        lv_tipo = lv_line(1).
        CASE lv_tipo.
          WHEN '0'. "Cabecera Fichero
            MOVE lv_line TO gs_fichero_h.
          WHEN '1'. "Cabecera Lote
            IF lv_detalle = 'X'.
              READ TABLE gt_lote_p INDEX 1 TRANSPORTING NO FIELDS.
              IF sy-subrc EQ 0.
                PERFORM f_completar_datos.
              ENDIF.
              CLEAR: lv_detalle, gt_taxnum, gt_lote_p, gs_lote_h, gv_total_detalle.
            ENDIF.
            MOVE lv_line TO gs_lote_h.
            "Valida si no se ha cargado previamente ese archivo
            "Consulta la cabecera de lotes
            SELECT SINGLE keyz1 budat valut FROM dfkkzk INTO gs_dfkkzk
              WHERE keyz1 = gs_lote_h-keyz1.
            IF sy-subrc EQ 0. "Ya se cargó el lote
              gv_error = 'X'.
              gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
              gs_mensajes-msgty = 'E'.
              gs_mensajes-msgno = '004'.
              gs_mensajes-msgv1 = gs_lote_h-keyz1.
              gs_mensajes-msgv2 = gs_dfkkzk-budat.
              gs_mensajes-msgv3 = gs_dfkkzk-valut.
              APPEND gs_mensajes TO gt_mensajes.
              CLOSE DATASET gv_file.
              DELETE DATASET gv_file.
*              CLOSE DATASET p_arch.
              EXIT.
*            ELSE.
*              CALL FUNCTION 'FKK_FIKEY_ENQUEUE'
*                EXPORTING
*                  i_fikey       = gs_lote_h-keyz1
*                  i_parmodus    = 'E'
*                EXCEPTIONS
*                  error_message = 1.
            ENDIF.

          WHEN '2'.
            lv_detalle = 'X'.
            MOVE lv_line TO gs_lote_p.
*            IF gs_lote_p-selw2 EQ '05'. "Solo para educacion
*             Si el nombre del archivo comienza por 'CASOBAN' es Educacion Continua
            IF gv_arch(7) = 'CASOBAN'.
*               Obtiene el numero de identificacion y crea el registro
              gs_taxnum-taxnum = gs_lote_p-selw1+1(20).
              APPEND gs_taxnum TO gt_taxnum.
            ENDIF.
            gv_total_detalle = gv_total_detalle + gs_lote_p-betrz.
            APPEND gs_lote_p TO gt_lote_p.
*            ENDIF.
          WHEN OTHERS.

        ENDCASE.

      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    IF lv_detalle EQ 'X'.
      READ TABLE gt_lote_p INDEX 1 TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        PERFORM f_completar_datos.
      ENDIF.
      CLEAR: lv_detalle, gt_taxnum, gt_lote_p, gs_lote_h, gv_total_detalle.
      CLOSE DATASET gv_file.
    ENDIF.
*    CLOSE DATASET p_arch.

  ELSE.
*   Error al abrir el archivo.
    gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
    gs_mensajes-msgty = 'E'.
    gs_mensajes-msgno = '001'.
    APPEND gs_mensajes TO gt_mensajes.
    gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
    gs_mensajes-msgty = 'E'.
    gs_mensajes-msgno = '002'.
    gs_mensajes-msgv1 = gv_path.
    APPEND gs_mensajes TO gt_mensajes.
    gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
    gs_mensajes-msgty = 'E'.
    gs_mensajes-msgno = '003'.
    gs_mensajes-msgv1 = gv_arch.
    APPEND gs_mensajes TO gt_mensajes.

  ENDIF.

ENDFORM.                    " F_LEER_ARCHIVO

*&---------------------------------------------------------------------*
*&      Form  F_CONSULTAR_DATOS
*&---------------------------------------------------------------------*
FORM f_consultar_datos .

* Obtiene los tipos de identificacion
  SELECT type id_taxtype_leg
    INTO TABLE gt_cstaxtype
    FROM zedu_c_cstaxtype
    WHERE type IS NOT NULL.

* Ordena los registros
  SORT gt_cstaxtype BY id_taxtype_leg.

*Consulta Cuentas Compensación por Entidad Recaudadora
  SELECT *
    FROM zedu_c_cta_comp
    INTO TABLE gt_cuentas
    WHERE cod_ent_rec IS NOT NULL.

*Consulta tabla de parámetros crédito
  SELECT * FROM zedu_c_param INTO TABLE gt_param
    WHERE repid = ''
    OR    repid = sy-repid.

*Clase Documento
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_blart.
  IF sy-subrc EQ 0.
    gv_blart = gs_param-valor.
  ENDIF.

*Moneda
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_waers.
  IF sy-subrc EQ 0.
    gv_waers = gs_param-valor.
  ENDIF.

*Motivo Compensacion
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_augrd.
  IF sy-subrc EQ 0.
    gv_augrd = gs_param-valor.
  ENDIF.

*Seleccion 1
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_selt1.
  IF sy-subrc EQ 0.
    gv_selt1 = gs_param-valor.
  ENDIF.

*Seleccion 2
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_selt2.
  IF sy-subrc EQ 0.
    gv_selt2 = gs_param-valor.
  ENDIF.

*Sociedadd
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = gc_bukrs.
  IF sy-subrc EQ 0.
    gv_bukrs = gs_param-valor.
  ENDIF.

ENDFORM.                    " F_CONSULTAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  F_COMPLETAR_DATOS
*&---------------------------------------------------------------------*
FORM f_completar_datos .
  DATA: lv_entidad(3) TYPE n,
        lv_lote(4)    TYPE n,
        lv_cont       TYPE i,
        lv_cuenta(17) TYPE c,
        lv_valor      TYPE p DECIMALS 2,
        lv_total      TYPE zcre_total_lote.

  CLEAR: lv_cont, gv_id_arch, gt_bptaxnum, gt_dpsob_bp_acc.

* Ordena los numeros de identificacion
  SORT gt_taxnum BY taxnum.
* Elimina los registros duplicados
  DELETE ADJACENT DUPLICATES FROM gt_taxnum
    COMPARING taxnum.
* Elimina los registros inválidos
  DELETE gt_taxnum
    WHERE taxnum IS INITIAL
      OR  taxnum = ''.

* Si el nombre del archivo comienza por 'CASOBAN' y se tienen registros
* de numeros de identificacion
  IF gv_arch(7) = 'CASOBAN' AND NOT gt_taxnum IS INITIAL.
*   Obtiene los BP asociados a los numeros de identificacion
*   haciendo uso del indice estandar TAX - Tax Numbers
    SELECT partner taxtype taxnum
      INTO TABLE gt_bptaxnum
      FROM dfkkbptaxnum
      FOR ALL ENTRIES IN gt_taxnum
      WHERE taxnum = gt_taxnum-taxnum.

*   Si se encontraron interlocutores
    IF NOT gt_bptaxnum IS INITIAL.
*     Ordena los registros
      SORT gt_bptaxnum BY partner.
*     Obtiene las cuentas contrato de los interlocutores
*     haciendo uso del indice estandar 1 - Index: Partner
      SELECT psobkey partner partneracctyp
        INTO TABLE gt_dpsob_bp_acc
        FROM dpsob_bp_acc
        FOR ALL ENTRIES IN gt_bptaxnum
        WHERE partner = gt_bptaxnum-partner.
*     Elimina los tipos de cuenta contrato no válidos
      DELETE gt_dpsob_bp_acc
        WHERE partneracctyp NE 'E6'.
*     Ordena los registros
      SORT gt_dpsob_bp_acc BY partner.
      SORT gt_bptaxnum BY taxtype taxnum.
    ENDIF.
  ENDIF.

  IF gv_lote IS INITIAL."si es la primera vez envío cabecera Archivo

    OPEN DATASET gv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    gv_lote = gs_lote_h-keyz1.
    gs_fichero_h-mandt = sy-mandt.
    TRANSFER gs_fichero_h TO gv_file.
  ENDIF.

*Cuenta Compensacion
  lv_cuenta = gs_lote_h-infof.
  CLEAR gs_lote_h-infof.
  SPLIT gs_lote_h-keyz2 AT '-' INTO lv_entidad lv_lote.
  READ TABLE gt_cuentas INTO gs_cuentas
  WITH KEY cod_ent_rec = lv_entidad
           num_cuenta  = lv_cuenta.
  IF sy-subrc EQ 0.
    gs_lote_h-bvrko = gs_cuentas-cta_comp.
    gs_lote_h-prctr = gs_cuentas-prctr.
    gs_lote_h-blart = gs_cuentas-blart.
  ENDIF.
  gs_lote_h-bukrs = gv_bukrs.
  gs_lote_h-waers = gv_waers.
*  gs_lote_h-budat = sy-datum.
  gs_lote_h-augrd = gv_augrd.
  gv_id_arch = gs_lote_h-ktsuh.
  CLEAR: gs_lote_h-ktsuh, lv_valor.
  DESCRIBE TABLE gt_lote_p LINES lv_cont.
  gs_lote_h-ksump = lv_cont.
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = gv_waers
*      amount_internal = gv_total_detalle
*    IMPORTING
*      amount_external = lv_valor.

  gs_lote_h-ktsus = gv_total_detalle.
  gv_fikey_lote = gs_lote_h-keyz1.
  TRANSFER gs_lote_h TO gv_file."Envio Cabecera lote

  CLEAR lv_total.
  LOOP AT gt_lote_p INTO gs_lote_p.
    CLEAR: gs_lote_p-selw2.
    gs_lote_p-selt1 = gv_selt1.
    gs_lote_p-selt2 = gv_selt2.
    lv_total = lv_total + gs_lote_p-betrz.
*   Si el nombre del archivo comienza por 'CASOBAN'
    IF gv_arch(7) = 'CASOBAN'.
*     Inicializa estructuras
      CLEAR: gs_cstaxtype, gs_bptaxnum, gs_dpsob_bp_acc.
*     Obtiene el tipo de identificacion
      READ TABLE gt_cstaxtype INTO gs_cstaxtype
        WITH KEY id_taxtype_leg = gs_lote_p-selw1(1)
        BINARY SEARCH.
*     Si encuentra el registro
      IF sy-subrc = 0.
*       Obtiene el BP
        READ TABLE gt_bptaxnum INTO gs_bptaxnum
          WITH KEY taxtype = gs_cstaxtype-type
                   taxnum  = gs_lote_p-selw1+1(20)
          BINARY SEARCH.
*       Si encuentra el registro
        IF sy-subrc = 0.
*         Obtiene la cuenta contrato
          READ TABLE gt_dpsob_bp_acc INTO gs_dpsob_bp_acc
            WITH KEY partner = gs_bptaxnum-partner
            BINARY SEARCH.
*         Si encuentra el registro
          IF sy-subrc = 0.
*           Limpia el campo y le Asigna la cuenta contrato
            CLEAR gs_lote_p-selw1.
            gs_lote_p-selw1 = gs_dpsob_bp_acc-psobkey+10(10).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    TRANSFER gs_lote_p TO gv_file. "envio detalle para el lote en proceso
  ENDLOOP.

  gs_tab_ctrl-id_archivo   = gv_id_arch.
  gs_tab_ctrl-keyz1        = gs_lote_h-keyz1.
  gs_tab_ctrl-imp_total    = lv_total.
  gs_tab_ctrl-cant_reg     = lv_cont.
  gs_tab_ctrl-f_proc       = sy-datum."gs_lote_h-budat.
  gs_tab_ctrl-waers        = gv_waers.
  gs_tab_ctrl-cod_ent_rec  = lv_entidad.
  gs_tab_ctrl-lote_aso2001 = lv_lote.
  APPEND gs_tab_ctrl TO gt_tab_ctrl.


ENDFORM.                    " F_COMPLETAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  F_EJECUTAR_TRANSF
*&---------------------------------------------------------------------*
FORM f_ejecutar_transf .

  DATA: lv_job_number      TYPE tbtcjob-jobcount,
        lv_job_name        TYPE tbtcjob-jobname.

*  PERFORM f_obtener_id_ejecucion.
*  CONCATENATE 'C' gv_id_arch INTO gv_runid.
  gv_runid = gv_fikey_lote.
  CONCATENATE 'ZEDU_JOB_LOTESREC_' gv_fikey_lote INTO lv_job_name.


  IF gt_tab_ctrl[] IS NOT INITIAL.
** Se crea un job con dicho nombre y se obtene el número generado
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_job_name
      IMPORTING
        jobcount         = lv_job_number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc EQ 0.
*      CALL FUNCTION 'FKK_FIKEY_DEQUEUE'
*        EXPORTING
*          i_fikey    = gv_fikey_lote
*          i_parmodus = 'E'.

      SUBMIT rfkkze00
      VIA JOB lv_job_name NUMBER lv_job_number AND RETURN
              WITH p_runid  = gv_runid
              WITH r_norm   = 'X'
              WITH as_fname = gv_file
              WITH r_err    = ' '
              WITH r_rst    = ' '
              WITH p_xprot  = 'X'
              WITH p_xclos  = 'X'
              WITH p_xbuch  = 'X'
              WITH p_xlist  = 'X'
*          WITH p_xcont  = ' '
*          WITH p_xpara  = 'X'
*          WITH p_strtm  = lv_hora.
              WITH p_xsofst = 'X'.

** Se lanza el job generado
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_job_number
          jobname              = lv_job_name
          strtimmed            = 'X' "inicio inmediato
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.

      PERFORM f_espera_job USING lv_job_name lv_job_number.
      DELETE DATASET gv_file.
      DELETE DATASET p_arch.

    ELSE.
      gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
      gs_mensajes-msgty = 'E'.
      gs_mensajes-msgno = '040'.
      gs_mensajes-msgv1 = gv_arch.
      APPEND gs_mensajes TO gt_mensajes.
    ENDIF.

  ELSE.
    gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
    gs_mensajes-msgty = 'I'.
    gs_mensajes-msgno = '039'.
    gs_mensajes-msgv1 = gv_arch.
    APPEND gs_mensajes TO gt_mensajes.
  ENDIF.

ENDFORM.                    " F_EJECUTAR_TRANSF


*&---------------------------------------------------------------------*
*&      Form  F_ADD_MSGS
*&---------------------------------------------------------------------*

FORM f_add_msgs USING    p_gt_mensajes TYPE tt_balmi
                         p_gv_log_handle     TYPE balloghndl.

  DATA: ls_msg  TYPE bal_s_msg.


* Traslada gt_mensajes a lt_return_bapiret2.
  LOOP AT p_gt_mensajes INTO gs_mensajes.
    CLEAR ls_msg.

    ls_msg-msgty = gs_mensajes-msgty.
    ls_msg-msgid = gs_mensajes-msgid.
    ls_msg-msgno = gs_mensajes-msgno.
    ls_msg-msgv1 = gs_mensajes-msgv1.
    ls_msg-msgv2 = gs_mensajes-msgv2.
    ls_msg-msgv3 = gs_mensajes-msgv3.
    ls_msg-msgv4 = gs_mensajes-msgv4.
    ls_msg-detlevel = gc_msg_level.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle  = p_gv_log_handle
        i_s_msg       = ls_msg
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
  ENDLOOP.

ENDFORM.                    " F_ADD_MSGS

*&---------------------------------------------------------------------*
*&      Form  F_SAVE_LOG
*&---------------------------------------------------------------------*
FORM f_save_log  USING    p_gv_log_handle TYPE balloghndl.

  DATA:
    lv_subrc      TYPE sy-subrc,
    lt_log_handle TYPE bal_t_logh.


  APPEND p_gv_log_handle TO lt_log_handle.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_client         = sy-mandt
      i_t_log_handle   = lt_log_handle
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  lv_subrc = sy-subrc.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

ENDFORM.         " F_SAVE_LOG

*&---------------------------------------------------------------------*
*&      Form  F_COPIAR_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_copiar_archivo .
  DATA: lv_line   TYPE string.

  CLOSE DATASET p_arch.
  CONCATENATE gv_dir_err gv_arch INTO gv_file_err.

  OPEN DATASET gv_file_err  FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  OPEN DATASET p_arch FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  DO.
    READ DATASET p_arch INTO lv_line.
    IF sy-subrc = 0.
      TRANSFER lv_line TO gv_file_err.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  CLOSE DATASET p_arch.
  DELETE DATASET p_arch.

  CLOSE DATASET  gv_file_err.

ENDFORM.                    " F_COPIAR_ARCHIVO

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_ID_EJECUCION
*&---------------------------------------------------------------------*
FORM f_obtener_id_ejecucion.

  DATA: lv_runid TYPE runidze_kk,
        lv_id(4) TYPE n.

  CLEAR: gv_runid.
  CONCATENATE 'RF' gv_lote+1(6) '%'
        INTO gv_runid.

  SELECT MAX( runid )
   INTO lv_runid
  FROM fkkzest
  WHERE runid LIKE gv_runid.

  IF lv_runid IS NOT INITIAL.
    lv_id = lv_runid+8(4).
    lv_id = lv_id + 1.
    CONCATENATE lv_runid+0(8) lv_id INTO gv_runid.
  ELSE.
    lv_id = '0001'.
    CONCATENATE 'RF' gv_lote+1(6) lv_id INTO gv_runid.
  ENDIF.

ENDFORM.                    " F_OBTENER_ID_EJECUCION

*&---------------------------------------------------------------------*
*&      Form  F_ESPERA_JOB
*&---------------------------------------------------------------------*
FORM f_espera_job USING p_jobname   TYPE tbtco-jobname
                        p_jobcount  TYPE tbtco-jobcount.

* Espera un tiempo a que se ejecute el JOB
  DATA: lv_time      LIKE sy-uzeit,
        lv_sec       TYPE i VALUE 5,
        lv_job_err   TYPE i,
        lt_logjob    TYPE STANDARD TABLE OF tbtc5,
        ls_logjob    TYPE tbtc5,
        lv_error_job TYPE c.

*  DATA: wa_tbtco  TYPE tbtco.
  DATA: status_job  TYPE tbtco-status.

* Espera hasta que el job finalice o este cancelado
  WHILE status_job NE 'F'  AND status_job NE 'A'. " No haya sido cancelado o no finalice
    SELECT SINGLE status INTO status_job FROM tbtco
      WHERE jobname = p_jobname
      AND   jobcount = p_jobcount.
    CHECK status_job NE 'F' AND status_job NE 'A'.
*  se espera durante 10 segundos y despues se vuelve a preguntar a la BD
*  Se hace esta espera para que no se pregunte tanto a la BD
    lv_time = sy-uzeit.
    lv_time = lv_time + lv_sec.
    DO.
      GET TIME.
      IF lv_time < sy-uzeit.
        EXIT.
      ENDIF.
    ENDDO.
  ENDWHILE.

  CASE status_job.
    WHEN 'F'.
      CALL FUNCTION 'BP_JOBLOG_READ'
        EXPORTING
          jobcount              = p_jobcount
          jobname               = p_jobname
        TABLES
          joblogtbl             = lt_logjob
        EXCEPTIONS
          cant_read_joblog      = 1
          jobcount_missing      = 2
          joblog_does_not_exist = 3
          joblog_is_empty       = 4
          joblog_name_missing   = 5
          jobname_missing       = 6
          job_does_not_exist    = 7
          OTHERS                = 8.

      CLEAR lv_error_job.
      LOOP AT lt_logjob INTO ls_logjob WHERE msgtype = 'E'.
        lv_error_job = 'X'.
        gs_mensajes-msgid = ls_logjob-msgid.
        gs_mensajes-msgty = ls_logjob-msgtype.
        gs_mensajes-msgno = ls_logjob-msgno.
        gs_mensajes-msgv1 = ls_logjob-msgv1.
        gs_mensajes-msgv2 = ls_logjob-msgv2.
        gs_mensajes-msgv3 = ls_logjob-msgv3.
        gs_mensajes-msgv4 = ls_logjob-msgv4.
        APPEND gs_mensajes TO gt_mensajes.
      ENDLOOP.
      READ TABLE lt_logjob INTO ls_logjob
      WITH KEY msgid = '>U'
               msgno = '190'
               msgtype = 'S'.
      IF sy-subrc = 0 AND lv_error_job = ''.
        lv_job_err = ls_logjob-msgv1.
        IF lv_job_err <> 0.
          lv_error_job = 'X'.
          gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
          gs_mensajes-msgty = 'E'.
          gs_mensajes-msgno = '040'.
          gs_mensajes-msgv1 = gv_arch.
          APPEND gs_mensajes TO gt_mensajes.
        ENDIF.
      ENDIF.

      IF lv_error_job = ''.
        gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
        gs_mensajes-msgty = 'S'.
        gs_mensajes-msgno = '038'.
        gs_mensajes-msgv1 = gv_arch.
        gs_mensajes-msgv2 = gv_runid.
        APPEND gs_mensajes TO gt_mensajes.
        INSERT zedu_p_tab_ctrl FROM TABLE gt_tab_ctrl.
      ENDIF.

    WHEN 'A'.
      gs_mensajes-msgid = 'ZDEDU_RECAUDACION'.
      gs_mensajes-msgty = 'E'.
      gs_mensajes-msgno = '045'.
      gs_mensajes-msgv1 = p_jobname.
      gs_mensajes-msgv2 = p_arch.
      APPEND gs_mensajes TO gt_mensajes.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " F_ESPERA_JOB
