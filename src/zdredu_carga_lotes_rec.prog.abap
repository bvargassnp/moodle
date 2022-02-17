*&---------------------------------------------------------------------*
*& Report  ZDREDU_CARGA_LOTES_REC
*&
*&---------------------------------------------------------------------*


REPORT  zdredu_carga_lotes_rec.

INCLUDE zdiedu_carga_lotes_rec_top.
INCLUDE zdiedu_carga_lotes_rec_f01.

START-OF-SELECTION.

* Crear Log
  PERFORM f_create_log CHANGING gv_log_handle.

** Verifica Permisos
*  CALL FUNCTION 'ZFM_AUTHORITY_CHECK'
*    EXPORTING
*      i_object     = 'ZCA_ZEDU'
*      i_programm   = sy-cprog
*    EXCEPTIONS
*      autorizacion = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0. "No posee permiso de ejecucion
*    gs_mensajes-msgid = 'ZCA_MESSAGE_CROSS'.
*    gs_mensajes-msgty = 'E'.
*    gs_mensajes-msgno = '099'.
*    APPEND gs_mensajes TO gt_mensajes.
*  ELSE.

* Selecciona Datos
    PERFORM f_consultar_datos.

* Lee el archivo
    PERFORM f_leer_archivo.

    IF gv_error EQ 'X'.
      "Copia archivo en carpeta de erróneos.
      PERFORM f_copiar_archivo.
    ELSE.
      "Ejecuta programa transferencia lotes
      PERFORM f_ejecutar_transf.
    ENDIF.
*  ENDIF.

  IF gt_mensajes[] IS NOT INITIAL.
* Agrega los mensajes
    PERFORM f_add_msgs USING  gt_mensajes
                              gv_log_handle.

* Guardar Log
    PERFORM f_save_log USING gv_log_handle.
  ENDIF.
