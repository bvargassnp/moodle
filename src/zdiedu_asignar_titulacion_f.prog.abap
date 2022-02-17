*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_ASIGNAR_TITULACION_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_LEER_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_leer_archivo
  CHANGING
    ptc_datos   TYPE gtyt_datos.

  "Declaraciones
  DATA:
    lv_linea  TYPE string,
    lv_promed TYPE string,
    ls_datos  TYPE gty_datos.


  "Inicializa retorno
  CLEAR:
    ptc_datos.

  "Abre el archivo plano
  OPEN DATASET pa_arch FOR INPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    MESSAGE 'Error al leer el archivo' TYPE 'E'.
    EXIT.
  ENDIF.

  "Mientras se encuentren registros
  DO.
    "Lee la linea del archivo
    READ DATASET pa_arch INTO lv_linea.

    "Si pudo leer la linea
    IF sy-subrc EQ 0.
      "Asigna los campos a la estructura de datos
      SPLIT lv_linea
        AT cl_abap_char_utilities=>horizontal_tab
        INTO ls_datos-stobjid
             ls_datos-csobjid
             ls_datos-scobjid
             ls_datos-student12
             ls_datos-nombre_comp
             ls_datos-tipo_doc
             ls_datos-nro_doc
             ls_datos-lug_exp
             ls_datos-apoderado
             ls_datos-fecha_fin_seg
             ls_datos-titulo
             ls_datos-acto
             ls_datos-acta
             ls_datos-libro
             ls_datos-folio
             ls_datos-diploma
             ls_datos-resolu
             ls_datos-fecact
             ls_datos-insc_per
             ls_datos-puesto_cohorte
             ls_datos-puesto_acto
             ls_datos-estud_coho_grad
             ls_datos-estudiantes_coho
             ls_datos-estudiantes_acto
             lv_promed
             ls_datos-noasistio.

      "Asigna el promedio
      ls_datos-promed = lv_promed.

      "Asigna los ceros a la izquierda
      PERFORM f_conv_alpha_input
        CHANGING
          ls_datos-stobjid.
      PERFORM f_conv_alpha_input
        CHANGING
          ls_datos-csobjid.
      PERFORM f_conv_alpha_input
        CHANGING
          ls_datos-scobjid.
      PERFORM f_conv_alpha_input
        CHANGING
          ls_datos-stobjid.
      PERFORM f_conv_alpha_input
        CHANGING
          ls_datos-acto.

      "Elimina los ceros a la izquieda de los siguientes campos
      SHIFT ls_datos-acta    LEFT DELETING LEADING '0'.
      SHIFT ls_datos-libro   LEFT DELETING LEADING '0'.
      SHIFT ls_datos-folio   LEFT DELETING LEADING '0'.
      SHIFT ls_datos-diploma LEFT DELETING LEADING '0'.
      SHIFT ls_datos-resolu  LEFT DELETING LEADING '0'.

      "Crea el registro
      APPEND ls_datos TO ptc_datos.

      "Si no pudo leer la linea
    ELSE.
      "Termina la lectura del archivo plano
      EXIT.
    ENDIF.
  ENDDO.

  "Cierra el archivo
  CLOSE DATASET pa_arch.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CONV_ALPHA_INPUT
*&---------------------------------------------------------------------*
FORM f_conv_alpha_input
  CHANGING
    pvc_valor.


  "Asigna los cetos a la izquierda
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pvc_valor
    IMPORTING
      output = pvc_valor.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_DATOS
*&---------------------------------------------------------------------*
FORM f_procesar_datos
  CHANGING
    ptc_datos TYPE gtyt_datos
    ptc_log   TYPE gtyt_log.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_datos>  TYPE gty_datos.


  "Recorre los registros
  LOOP AT ptc_datos ASSIGNING <fs_datos>.
    "Crear registro infotipo 9117
    PERFORM f_crear_reg_9117
      CHANGING
        <fs_datos>
        ptc_log.

    "Finaliza el segmento
    PERFORM f_finalizar_segmento
      CHANGING
        <fs_datos>
        ptc_log.

    "Crea el registro en la tabla zedu_estacto
    PERFORM f_crear_reg_estacto
      CHANGING
        <fs_datos>
        ptc_log.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_REG_9117
*&---------------------------------------------------------------------*
FORM f_crear_reg_9117
  CHANGING
    psc_datos TYPE gty_datos
    ptc_log   TYPE gtyt_log.

  "Declaraciones
  DATA:
    ls_log         TYPE gty_log,
    lt_hrp9117_aux TYPE hrp9117,
    lt_innnn_9117  TYPE STANDARD TABLE OF p9117 WITH HEADER LINE.


  "Asigna los datos al registro del posible log
  MOVE-CORRESPONDING psc_datos TO ls_log.

  "Asigna los datos al infotipo
  lt_innnn_9117-mandt            = sy-mandt.
  lt_innnn_9117-plvar            = '01'.
  lt_innnn_9117-otype            = cl_hrpiq00const=>c_otype_st.
  lt_innnn_9117-objid            = psc_datos-stobjid.
  lt_innnn_9117-infty            = '9117'.
***  lt_innnn_9117-subty = ''.
***  lt_innnn_9117-istat = ''.
***  lt_innnn_9117-priox = ''.
  lt_innnn_9117-begda            = psc_datos-fecact.
  lt_innnn_9117-endda            = cl_hrpiq00constants=>c_highdate..
***  lt_innnn_9117-varyf = ''.
***  lt_innnn_9117-seqnr = ''.
  lt_innnn_9117-aedtm            = sy-datum.
***  lt_innnn_9117-uname = ''.
***  lt_innnn_9117-reasn = ''.
***  lt_innnn_9117-histo    = ''.
***  lt_innnn_9117-itxnr    = ''.
  lt_innnn_9117-objid_st         = psc_datos-stobjid.
  lt_innnn_9117-short            = psc_datos-student12.
  lt_innnn_9117-plaest           = psc_datos-scobjid.
  lt_innnn_9117-titulo           = psc_datos-titulo.
  lt_innnn_9117-acto             = psc_datos-acto.
  lt_innnn_9117-acta             = psc_datos-acta.
  lt_innnn_9117-libro            = psc_datos-libro.
  lt_innnn_9117-folio            = psc_datos-folio.
  lt_innnn_9117-diploma          = psc_datos-diploma.
  lt_innnn_9117-resolu           = psc_datos-resolu.
  lt_innnn_9117-fecact           = psc_datos-fecact.
  lt_innnn_9117-insc_per         = psc_datos-insc_per.
  lt_innnn_9117-puesto_cohorte   = psc_datos-puesto_cohorte.
  lt_innnn_9117-puesto_acto      = psc_datos-puesto_acto.
  lt_innnn_9117-estud_coho_grad  = psc_datos-estud_coho_grad.
  lt_innnn_9117-estudiantes_coho = psc_datos-estudiantes_coho.
  lt_innnn_9117-estudiantes_acto = psc_datos-estudiantes_acto.
  lt_innnn_9117-promed           = psc_datos-promed.
  lt_innnn_9117-noasistio        = psc_datos-noasistio.

  "Crea el registro
  APPEND lt_innnn_9117.

  "Valida si el infotipo ya existe
  SELECT SINGLE * FROM hrp9117
    INTO lt_hrp9117_aux
    WHERE objid   = lt_innnn_9117-objid
      AND otype   = cl_hrpiq00const=>c_otype_st
      AND plvar   = '01'
      AND plaest  = lt_innnn_9117-plaest
      AND acto    = lt_innnn_9117-acto
      AND acta    = lt_innnn_9117-acta
      AND libro   = lt_innnn_9117-libro
      AND folio   = lt_innnn_9117-folio
      AND diploma = lt_innnn_9117-diploma
      AND resolu  = lt_innnn_9117-resolu
      AND fecact  = lt_innnn_9117-fecact.

  IF sy-subrc EQ 0.
    "Indica que no crea el registro en el infotipo
    psc_datos-hrp9117 = ''.
    "Asigna el mensaje del log
    ls_log-mensaje = 'Ya existe registro en Infotipo 9117 para el estudiante'.
    "crea el registro del log
    APPEND ls_log TO ptc_log.

  ELSE.
    "Crea el registro del infotipo
    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        vtask               = 'D'
      TABLES
        innnn               = lt_innnn_9117
*       ILFCODE             =
      EXCEPTIONS
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        OTHERS              = 6.

    "Si ocurre algun error
    IF sy-subrc NE 0.
      "Indica que no crea el registro en el infotipo
      psc_datos-hrp9117 = ''.
      "Si no se tiene tipo de mensaje
      IF sy-msgty IS INITIAL.
        sy-msgty = 'I'.
      ENDIF.
      "Asigna el mensaje del log
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        INTO ls_log-mensaje
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "crea el registro del log
      APPEND ls_log TO ptc_log.

      "Deshace los posibles cambios
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      "Si no ocurre ningun error
    ELSE.
      "Indica que se crea el registro en el infotipo
      psc_datos-hrp9117 = abap_true.

      "Establece el cambio
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FINALIZAR_SEGMENTO
*&---------------------------------------------------------------------*
FORM f_finalizar_segmento
  CHANGING
    psc_datos TYPE gty_datos
    ptc_log   TYPE gtyt_log.

  "Declaraciones
  DATA:
    lv_result  TYPE sysubrc,
    ls_log     TYPE gty_log,
    ls_stdereg TYPE piqstdereg_p,
    lt_return  TYPE bapiret2_t,
    ls_return  TYPE bapiret2.


  "Asigna los datos al registro del posible log
  MOVE-CORRESPONDING psc_datos TO ls_log.

  "Asigna los datos del Estudiante y Programa
  ls_stdereg-st_objid    = psc_datos-stobjid.
  ls_stdereg-cs_objid    = psc_datos-csobjid.
  ls_stdereg-sc_objid    = psc_datos-scobjid.
  ls_stdereg-endda       = psc_datos-fecha_fin_seg.
  ls_stdereg-derapptype  = 'RV01'.
  ls_stdereg-derreason   = '1018'.
  ls_stdereg-derdate     = psc_datos-fecha_fin_seg.
  ls_stdereg-last_attend = psc_datos-fecha_fin_seg.

  "Anula la matricula
  CALL FUNCTION 'HRIQ_STUDENT_DEREG_CREATE_DB'
    EXPORTING
      plvar            = '01'
      iv_process       = 'RV01'
      stdereg          = ls_stdereg
*     check_only       = p_test
      rule_check       = 'X'
      iv_mode          = 'B'
      iv_cancelmodules = 'X'
    IMPORTING
      ev_result        = lv_result
      et_return        = lt_return
    EXCEPTIONS
      internal_error   = 1
      no_authorisation = 2
      corr_exit        = 3
      cancelled        = 4
      OTHERS           = 5.

  "Si no ocurrio ninguna excepcion
  IF sy-subrc EQ 0.
    "Si finalizo correctamente
    IF lv_result EQ 0.
      "Indica que finalizo el segmento correctamente
      psc_datos-fin_segmento = abap_true.

      "Si no finalizo correctamente
    ELSE.
      "Indica que no finalizo el segmento
      psc_datos-fin_segmento = ''.
    ENDIF.

    "Si ocurrio alguna excepcion
  ELSE.
    "Indica que no finalizo el segmento
    psc_datos-fin_segmento = ''.

    "Si no se tienen mensajes de retorno y se tiene mensaje de excepcion
    IF lt_return IS INITIAL AND NOT sy-msgty IS INITIAL AND NOT sy-msgid IS INITIAL.
      "Asigna el mensaje del log
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        INTO ls_log-mensaje
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      "crea el registro del log
      APPEND ls_log TO ptc_log.
    ENDIF.
  ENDIF.

  "Si finalizo correctamente el segmento
  IF psc_datos-fin_segmento EQ abap_true.
    "Establece los cambios
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "Si no finalizo correctamente el segmento
  ELSE.
    "Recorre los mensaje de error obtenidos
    LOOP AT lt_return INTO ls_return.
      "Asigna el mensaje del log
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
        INTO ls_log-mensaje
        WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      "crea el registro del log
      APPEND ls_log TO ptc_log.
    ENDLOOP.

    "Deshace los posibles cambios
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_REG_ESTACTO
*&---------------------------------------------------------------------*
FORM f_crear_reg_estacto
  CHANGING
    psc_datos TYPE gty_datos
    ptc_log   TYPE gtyt_log.

  "Declaraciones
  DATA:
    ls_estacto TYPE zedu_estacto.


  "Asigna los datos del registro
  ls_estacto-acto             = psc_datos-acto.
  ls_estacto-numero_estud     = psc_datos-stobjid.
  ls_estacto-numero_matricula = psc_datos-student12.
  ls_estacto-plan_estudios    = psc_datos-scobjid.
  ls_estacto-fecha_acto       = psc_datos-fecact.
  ls_estacto-nompreap         = psc_datos-nombre_comp.
  ls_estacto-tdocap           = psc_datos-tipo_doc.
  ls_estacto-ndocap           = psc_datos-nro_doc.
  ls_estacto-lugexpap         = psc_datos-lug_exp.
  ls_estacto-zcheck           = psc_datos-apoderado.

  "Crea el registro
  MODIFY zedu_estacto
    FROM ls_estacto.

  "Si puede crear el registro
  IF sy-subrc EQ 0.
    "Indica que no crea el registro
    psc_datos-zedu_estacto = abap_true.
    "Establece los cambios
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "Si no pudo crear el registro
  ELSE.
    "Indica que no crea el registro
    psc_datos-zedu_estacto = ''.
    "Deshace los posibles cambios
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_LOG
*&---------------------------------------------------------------------*
FORM f_crear_log
  USING
    pti_datos TYPE gtyt_datos
    pti_log   TYPE gtyt_log.

  "Declaraciones
  DATA:
    lv_archivo   TYPE string,
    lv_ruta      TYPE string,
    lv_nombre    TYPE string,
    lv_extension TYPE string,
    lv_arch_log  TYPE string,
    lv_arch_ret  TYPE string.


  "Obtiene el nombre del archivo
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = pa_arch
    IMPORTING
      stripped_name = lv_archivo
      file_path     = lv_ruta
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.

  "Divide el archivo en nombre y extension
  SPLIT lv_archivo AT '.'
    INTO lv_nombre
         lv_extension.

  "Arma el nombre del archivo de retorno
  CONCATENATE lv_ruta
              lv_nombre
              '_RET.'
              lv_extension
    INTO lv_arch_ret.

  "Arma el nombre del archivo de log
  CONCATENATE lv_ruta
              lv_nombre
              '_LOG.'
              lv_extension
    INTO lv_arch_log.

  "Crea el archivo de retorno
  PERFORM f_crear_ret
    USING
      lv_arch_ret
      pti_datos.

  "Crea el archivo de log
  PERFORM f_crear_arch_log
    USING
      lv_arch_log
      pti_log.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_RET
*&---------------------------------------------------------------------*
FORM f_crear_ret
  USING
    pvi_arch_ret  TYPE string
    pti_datos     TYPE gtyt_datos.

  "Declaraciones
  DATA:
    lv_promed   TYPE string,
    lv_registro TYPE string,
    ls_datos    TYPE gty_datos.


  "Abre el archivo plano para escritura
  OPEN DATASET pvi_arch_ret FOR OUTPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    WRITE / 'Error al crear el archivo de retorno'.

    "Crea el registro de encabezado
    CONCATENATE 'ID_ST   '
                'ID_CS   '
                'ID_SC   '
                'N_MATRICULA '
                'NOMBRE_COMPLETO                                                                 '
                'TIPO_DOC_IDENTIDAD            '
                'NUMERO_DOC_IDENTIDAD                                        '
                'I'
                'S'
                'R'
      INTO lv_registro
      SEPARATED BY '|' RESPECTING BLANKS.

    "Genera el mensaje en pantalla
    WRITE / lv_registro.

    "Recorre los registros de retorno
    LOOP AT pti_datos INTO ls_datos.
      "Crea la linea del registro
      CONCATENATE ls_datos-stobjid
                  ls_datos-csobjid
                  ls_datos-scobjid
                  ls_datos-student12
                  ls_datos-nombre_comp
                  ls_datos-tipo_doc
                  ls_datos-nro_doc
                  ls_datos-hrp9117
                  ls_datos-fin_segmento
                  ls_datos-zedu_estacto
        INTO lv_registro
        SEPARATED BY '|' RESPECTING BLANKS.

      "Genera el mensaje en pantalla
      WRITE / lv_registro.
    ENDLOOP.

    "Si puede abrir el archivo
  ELSE.
    "Crea el registro de encabezado
    CONCATENATE 'ID_OBJ_ESTUDIANTE'
                'ID_ESTUDIO'
                'ID_PLAN_ESTUDIO'
                'NUMERO_MATRICULA'
                'NOMBRE_COMPLETO'
                'TIPO_DOC_IDENTIDAD'
                'NUMERO_DOC_IDENTIDAD'
                'LUGAR_EXPEDICION'
                'APODERADO'
                'FECHA_FIN_SEGMENTO'
                'TITULO'
                'ACTO'
                'ACTA'
                'LIBRO'
                'FOLIO'
                'DIPLOMA'
                'RESOLUCION'
                'FECHA_ACTO'
                'PERIODO_INSCRIPCION'
                'PUESTO_COHORTE'
                'PUESTO_ACTO'
                'ESTUD_COHO_GRAD'
                'ESTUDIANTES_COHO'
                'ESTUDIANTES_ACTO'
                'PROMED'
                'NOASISTIO'
                'INFOTIPO_9117'
                'FIN_SEGMENTO'
                'REL_EST_ACTO'
      INTO lv_registro
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    "Crea el registro en el retorno
    TRANSFER lv_registro TO pvi_arch_ret.

    "Recorre los registros de retorno
    LOOP AT pti_datos INTO ls_datos.
      "Asigna el valor del promedio
      lv_promed = ls_datos-promed.
      "Crea la linea del registro
      CONCATENATE ls_datos-stobjid
                  ls_datos-csobjid
                  ls_datos-scobjid
                  ls_datos-student12
                  ls_datos-nombre_comp
                  ls_datos-tipo_doc
                  ls_datos-nro_doc
                  ls_datos-lug_exp
                  ls_datos-apoderado
                  ls_datos-fecha_fin_seg
                  ls_datos-titulo
                  ls_datos-acto
                  ls_datos-acta
                  ls_datos-libro
                  ls_datos-folio
                  ls_datos-diploma
                  ls_datos-resolu
                  ls_datos-fecact
                  ls_datos-insc_per
                  ls_datos-puesto_cohorte
                  ls_datos-puesto_acto
                  ls_datos-estud_coho_grad
                  ls_datos-estudiantes_coho
                  ls_datos-estudiantes_acto
                  lv_promed
                  ls_datos-noasistio
                  ls_datos-hrp9117
                  ls_datos-fin_segmento
                  ls_datos-zedu_estacto
        INTO lv_registro
        SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

      "Crea el registro en el retorno
      TRANSFER lv_registro TO pvi_arch_ret.
    ENDLOOP.

    "Cierra el archivo
    CLOSE DATASET pvi_arch_ret.

    "Se creo el archivo de retorno
    WRITE / 'Se creo el archivo de retorno'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_ARCH_LOG
*&---------------------------------------------------------------------*
FORM f_crear_arch_log
  USING
    pvi_arch_log  TYPE string
    pti_log       TYPE gtyt_log.

  "Declaraciones
  DATA:
    lv_registro TYPE string,
    ls_log      TYPE gty_log.


  "Continua solo si te tiene registros de log
  CHECK NOT pti_log IS INITIAL.

  "Abre el archivo plano para escritura
  OPEN DATASET pvi_arch_log FOR OUTPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    WRITE / 'Error al crear el archivo de LOG'.

    "Si puede abrir el archivo
  ELSE.
    "Crea el registro de encabezado
    CONCATENATE 'ID_OBJ_ESTUDIANTE'
                'ID_ESTUDIO'
                'ID_PLAN_ESTUDIO'
                'NUMERO_MATRICULA'
                'NOMBRE_COMPLETO'
                'TIPO_DOC_IDENTIDAD'
                'NUMERO_DOC_IDENTIDAD'
                'MENSAJE'
      INTO lv_registro
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    "Crea el registro en el retorno
    TRANSFER lv_registro TO pvi_arch_log.

    "Recorre los registros de retorno
    LOOP AT pti_log INTO ls_log.
      "Crea la linea del registro
      CONCATENATE ls_log-stobjid
                  ls_log-csobjid
                  ls_log-scobjid
                  ls_log-student12
                  ls_log-nombre_comp
                  ls_log-tipo_doc
                  ls_log-nro_doc
                  ls_log-mensaje
        INTO lv_registro
        SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

      "Crea el registro en el retorno
      TRANSFER lv_registro TO pvi_arch_log.
    ENDLOOP.

    "Cierra el archivo
    CLOSE DATASET pvi_arch_log.

    "Se creo el archivo de retorno
    WRITE / 'Se creo el archivo de LOG'.
  ENDIF.

ENDFORM.
