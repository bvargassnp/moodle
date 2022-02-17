*&---------------------------------------------------------------------*
*& Report  ZDREDU_MARCADO_ASIGNATURA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT zdredu_marcado_asignatura.

INCLUDE zdiedu_marcado_asignatura_top.
INCLUDE zdiedu_marcado_asignatura_f01.

START-OF-SELECTION.

  PERFORM f_leer_archivo.
  IF gt_archivo[] IS NOT INITIAL.
    PERFORM f_leer_datos.
    PERFORM f_procesar_arch.
    IF gt_log IS NOT INITIAL.
      PERFORM mostrar_alv.
    ENDIF.
  ELSE.
    WRITE: / 'Archivo sin datos'.
  ENDIF.
