*&-----------------------------------------------------------------------------------*
*& Report  ZDREDU_LISTADO_INSCRITOS
*&
*&-----------------------------------------------------------------------------------*
*& Autor          : ADP - LMARIZALDE.
*& Fecha creaciòn : 2017.01.19
*& Modificaciones : ALTASER  Luis A. Tafur.
*& Fecha modifica.: Septiembre 2021
*&                : Se mejora la opción de inscripciones para que parta del expediente
*&                : del estudiante y luego si tablas zetas de admisiones.
*&------------------------------------------------------------------------------------*

report zdredu_listado_inscritos.

include zdiedu_listado_inscritos_top. "Include Variables
include zdiedu_listado_inscritos_par. "Include parámetros
include zdiedu_listado_inscritos_cla. "clases
include zdiedu_listado_inscritos_f01. "Include rutinas

start-of-selection.
  if rb_ins eq 'X'.
    perform leer_datos_ins.
    perform f_procesar_datos_ins.
  elseif rb_pre eq 'X'.
    perform f_leer_datos.
    perform f_procesar_datos_pre.
  endif.

  if gt_data[] is not initial.
    perform mostrar_alv.
  else.
    message s000(zdfica) with 'No se encontraron datos' 'para los parametros ingresados'.
  endif.
