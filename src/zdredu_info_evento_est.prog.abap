*&---------------------------------------------------------------------*
*& Report  ZDREDU_INFO_EVENTO_EST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zdredu_info_evento_est.

INCLUDE zdiedu_info_evento_est_top.
INCLUDE zdiedu_info_evento_est_f01.

START-OF-SELECTION.
  PERFORM f_leer_datos.
  PERFORM f_procesar_datos.
  IF gt_data[] IS NOT INITIAL.
    PERFORM mostrar_alv.
  ELSE.
    MESSAGE s000(zdfica) WITH 'No se encontraron datos' 'para los parametros ingresados'.
  ENDIF.
