REPORT zdredu_liquidacion_pago_form.

INCLUDE zdredu_liquidac_pago_form_top.
INCLUDE zdredu_liquidac_pago_form_sel.
INCLUDE zdredu_liquidac_pago_form_f01.

START-OF-SELECTION.

  DATA: l_message   TYPE char100,
        lt_mensajes TYPE bapiret2_t.

  PERFORM f_validar_entrada
  CHANGING l_message.

  IF l_message IS NOT INITIAL.
    MESSAGE l_message
    TYPE co_s
    DISPLAY LIKE co_e.
    LEAVE TO LIST-PROCESSING.
  ELSE.
    PERFORM f_obtener_datos.
    PERFORM f_imprimir_formulario CHANGING lt_mensajes.
    PERFORM f_mostrar_alv USING lt_mensajes.
  ENDIF.
