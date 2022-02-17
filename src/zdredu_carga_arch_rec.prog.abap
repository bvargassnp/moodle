REPORT ZDREDU_CARGA_ARCH_REC.


INCLUDE zdiedu_carga_arch_rec_top.
INCLUDE zdiedu_carga_arch_rec_f01.

INITIALIZATION.

*  CALL FUNCTION 'ZFM_AUTHORITY_CHECK'
*    EXPORTING
*      i_object     = 'ZCA_EDU'
*      i_programm   = sy-cprog
*    EXCEPTIONS
*      autorizacion = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
*    MESSAGE e099(zca_message_cross).
*  ENDIF.

START-OF-SELECTION.
  PERFORM f_crear_log.
* Leo el archivo
  PERFORM f_leer_archivo.
  IF gv_error_param IS INITIAL.
    PERFORM f_procesar_datos.
  ENDIF.

  "Se genera el log solo si tiene algun mensaje
  IF gt_mensajes[] IS NOT INITIAL.
    PERFORM f_grabar_log.
  ENDIF.
