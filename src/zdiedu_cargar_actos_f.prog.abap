*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CARGAR_ACTOS_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_procesar_archivo.

  "Declaraciones
  DATA:
    lv_archivo   TYPE string,
    lv_ruta      TYPE string,
    lv_nombre    TYPE string,
    lv_extension TYPE string,
    lv_arch_ret  TYPE string,
    lv_linea     TYPE string,
    lv_valido    TYPE abap_bool,
    lv_resultado TYPE c,
    ls_acto      TYPE zedu_actos.


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

  "Abre el archivo plano
  OPEN DATASET pa_arch FOR INPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    MESSAGE 'Error al leer el archivo' TYPE 'E'.
    EXIT.
  ENDIF.

  "Abre el archivo de retorno
  OPEN DATASET lv_arch_ret FOR OUTPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    MESSAGE 'Error al crear el archivo de retorno' TYPE 'E'.
    EXIT.
  ENDIF.

  "Mientras se encuentren registros
  DO.
    "Lee la linea del archivo
    READ DATASET pa_arch INTO lv_linea.

    "Si pudo leer la linea
    IF sy-subrc EQ 0.
      CASE lv_linea+0(1).
          "Si es un registro de cabecera
        WHEN '1'.
          "Crea el registro en la tabla ZEDU_ACTOS
          PERFORM f_crea_acto
            USING
              lv_linea
            CHANGING
              lv_valido
              lv_resultado
              ls_acto.

          "Si es un registro de detalle
        WHEN '2'.
          "Crea el registro en la tabla ZEDU_DETAL_ACTOS
          PERFORM f_crea_detalle
            USING
              lv_linea
              lv_valido
              ls_acto
            CHANGING
              lv_resultado.

          "Si es un registro de convenio
        WHEN '3'.
          "Crea el registro en la tabla ZEDU_DETAL_CON
          PERFORM f_crea_convenio
            USING
              lv_linea
              lv_valido
              ls_acto
            CHANGING
              lv_resultado.
      ENDCASE.

      "Adiciona el resultado a la linea de retorno
      CONCATENATE lv_resultado
                  lv_linea
        INTO lv_linea
        SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

      "Crea el registro en el retorno
      TRANSFER lv_linea TO lv_arch_ret.

      "Si no pudo leer la linea
    ELSE.
      "Termina la lectura del archivo plano
      EXIT.
    ENDIF.
  ENDDO.

  "Cierra el archivo de lectura
  CLOSE DATASET pa_arch.

  "Cierra el archivo de retorno
  CLOSE DATASET lv_arch_ret.

  "Se creo el archivo de retorno
  WRITE / 'Se creo el archivo de retorno'.

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
*&      Form  F_CREA_ACTO
*&---------------------------------------------------------------------*
FORM f_crea_acto
  USING
    pvi_linea     TYPE string
  CHANGING
    pvc_valido    TYPE abap_bool
    pvc_resultado TYPE c
    psc_acto      TYPE zedu_actos.

  "Declaraciones
  DATA:
    lv_tipo_reg TYPE c,
    lv_dummy    TYPE string.


  "Inicializa retornos
  CLEAR:
    pvc_valido,
    pvc_resultado,
    psc_acto.

  "Asigna los campos a la estructura de datos
  SPLIT pvi_linea
    AT cl_abap_char_utilities=>horizontal_tab
    INTO lv_tipo_reg
         psc_acto-acto
         psc_acto-tipo_acto
         psc_acto-resolucion
         psc_acto-fecha_resolucion
         psc_acto-fecha_acto
         psc_acto-hora_grado
         psc_acto-pais_ceremonia
         psc_acto-departamento
         psc_acto-ciudad
         psc_acto-lugar
         psc_acto-rector_ces
         psc_acto-pres_ceremonia_ces
         psc_acto-pres_delegado_ces
         psc_acto-secretario_ces
         psc_acto-universidad_con
         psc_acto-ano_convenio
         psc_acto-rector_convenio
         psc_acto-pres_ceremonia_con
         psc_acto-pres_delegado_con
         psc_acto-secretario_con
         lv_dummy.

  "Asigna los ceros a la izquierda
  PERFORM f_conv_alpha_input
    CHANGING
      psc_acto-acto.
  PERFORM f_conv_alpha_input
    CHANGING
      psc_acto-resolucion.

  "Crea el registro
  MODIFY zedu_actos
    FROM psc_acto.

  "Si puede crear el registro
  IF sy-subrc EQ 0.
    "Indica que creó el registro
    pvc_valido    = abap_true.
    pvc_resultado = 'S'.
    "Establece los cambios
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "Si no pudo crear el registro
  ELSE.
    "Indica que no crea el registro
    pvc_resultado = 'N'.
    "Deshace los posibles cambios
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREA_DETALLE
*&---------------------------------------------------------------------*
FORM f_crea_detalle
  USING
    pvi_linea     TYPE string
    pvi_valido    TYPE abap_bool
    psi_acto      TYPE zedu_actos
  CHANGING
    pvc_resultado TYPE c.

  "Declaraciones
  DATA:
    lv_tipo_reg   TYPE c,
    lv_objid_ces  TYPE c LENGTH 8,
    lv_dummy      TYPE string,
    ls_detal_acto TYPE zedu_detal_actos.


  "Inicializa retorno
  pvc_resultado = 'N'.

  "Continua solo si el registro de cabecera es valido
  CHECK pvi_valido EQ abap_true.

  "Asigna los datos de la cabecera
  ls_detal_acto-acto        = psi_acto-acto.
  ls_detal_acto-tipo_acto   = psi_acto-tipo_acto.
  ls_detal_acto-resolucion  = psi_acto-resolucion.

  "Asigna los campos a la estructura de datos
  SPLIT pvi_linea
    AT cl_abap_char_utilities=>horizontal_tab
    INTO lv_tipo_reg
         lv_objid_ces
         ls_detal_acto-plan_ces
         ls_detal_acto-decano_name
         ls_detal_acto-snies
         ls_detal_acto-decano_ces
         lv_dummy.

  "Asigna los ceros a la izquierda
  PERFORM f_conv_alpha_input
    CHANGING
      lv_objid_ces.
  PERFORM f_conv_alpha_input
    CHANGING
      ls_detal_acto-plan_ces.
  PERFORM f_conv_alpha_input
    CHANGING
      ls_detal_acto-decano_ces.

  ls_detal_acto-objid_ces = lv_objid_ces.

  "Crea el registro
  MODIFY zedu_detal_actos
    FROM ls_detal_acto.

  "Si puede crear el registro
  IF sy-subrc EQ 0.
    "Indica que creó el registro
    pvc_resultado = 'S'.
    "Establece los cambios
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    "Si no pudo crear el registro
  ELSE.
    "Indica que no crea el registro
    pvc_resultado = 'N'.
    "Deshace los posibles cambios
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CREA_CONVENIO
*&---------------------------------------------------------------------*
FORM f_crea_convenio
  USING
    pvi_linea     TYPE string
    pvi_valido    TYPE abap_bool
    psi_acto      TYPE zedu_actos
  CHANGING
    pvc_resultado TYPE c.

  "Declaraciones
  DATA:
    lv_tipo_reg       TYPE c,
    lv_id_universidad TYPE string,
    lv_objid_con      TYPE c LENGTH 8,
    lv_dummy          TYPE string,
    ls_detal_con      TYPE zedu_detal_con,
    ls_univ_con       TYPE zedu_univ_con.


  "Inicializa retorno
  pvc_resultado = 'N'.

  "Continua solo si el registro de cabecera es valido
  CHECK pvi_valido EQ abap_true.

  "Asigna los datos de la cabecera
  ls_univ_con-acto       = ls_detal_con-acto        = psi_acto-acto.
  ls_univ_con-tipo_acto  = ls_detal_con-tipo_acto   = psi_acto-tipo_acto.
  ls_univ_con-resolucion = ls_detal_con-resolucion  = psi_acto-resolucion.

  "Asigna los campos a la estructura de datos
  SPLIT pvi_linea
    AT cl_abap_char_utilities=>horizontal_tab
    INTO lv_tipo_reg
         lv_id_universidad
         ls_detal_con-decano_convenio
         lv_objid_con
         ls_detal_con-plan_convenio
         ls_detal_con-decano_name
         ls_detal_con-snies
         lv_dummy.

  ls_univ_con-id_universidad     = ls_detal_con-id_universidad = lv_id_universidad.
  ls_univ_con-universidad_con    = psi_acto-universidad_con.
  ls_univ_con-ano_convenio       = psi_acto-ano_convenio.
  ls_univ_con-rector_convenio    = psi_acto-rector_convenio.
  ls_univ_con-pres_ceremonia_con = psi_acto-pres_ceremonia_con.
  ls_univ_con-pres_delegado_con  = psi_acto-pres_delegado_con.
  ls_univ_con-secretario_con     = psi_acto-secretario_con.

  "Asigna los ceros a la izquierda
  PERFORM f_conv_alpha_input
    CHANGING
      ls_detal_con-decano_convenio.
  PERFORM f_conv_alpha_input
    CHANGING
      lv_objid_con.
  PERFORM f_conv_alpha_input
    CHANGING
      ls_detal_con-plan_convenio.

  ls_detal_con-objid_con = lv_objid_con.

  "Crea el registro
  MODIFY zedu_univ_con
    FROM ls_univ_con.

  "Si puede crear el registro
  IF sy-subrc EQ 0.
    "Crea el registro
    MODIFY zedu_detal_con
      FROM ls_detal_con.

    "Si puede crear el registro
    IF sy-subrc EQ 0.
      "Indica que creó el registro
      pvc_resultado = 'S'.

      "Si no pudo crear el registro
    ELSE.
      "Indica que no crea el registro
      pvc_resultado = 'N'.
    ENDIF.

    "Si no pudo crear el registro
  ELSE.
    "Indica que no crea el registro
    pvc_resultado = 'N'.
  ENDIF.

  "Si el resultado fue correcto
  IF pvc_resultado EQ 'S'.
    "Establece los cambios
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

  ELSE.
    "Deshace los posibles cambios
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.

ENDFORM.
