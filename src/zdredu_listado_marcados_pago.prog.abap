REPORT zdredu_listado_marcados_pago.

INCLUDE zdiedu_listado_marcados_top.
INCLUDE zdiedu_listado_marcados_f01.

START-OF-SELECTION.
  PERFORM f_leer_datos.
  PERFORM f_procesar_datos.
  IF gt_data[] IS NOT INITIAL.
    PERFORM mostrar_alv.
  ELSE.
    MESSAGE s000(zdfica) WITH 'No se encontraron datos' 'para los parametros ingresados'.
  ENDIF.
