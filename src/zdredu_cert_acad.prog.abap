*&---------------------------------------------------------------------*
* Creado por: Hector Ivan Restrepo Saldarriaga ( HRESTREPO ).          *
* Creado el: 26/11/2017 .                                              *
* Frente: Educación                                                    *
*&---------------------------------------------------------------------*
* ID    | Fecha      | Orden      | Responsable | Descripción          *
*&---------------------------------------------------------------------*
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*
*&---------------------------------------------------------------------*

REPORT zdredu_cert_acad.

INCLUDE zdiedu_cert_acad_t.
INCLUDE zdiedu_cert_acad_s.
INCLUDE zdiedu_cert_acad_f.


*&---------------------------------------------------------------------*
*   Proceso principal
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Obtiene los datos
  PERFORM f_obtener_datos
    CHANGING
      gt_informe
      gt_informe_pe
      gt_informe_pm
      gt_informe_sm
      gt_informe_na
      gt_informe_ps
      gt_informe_ro
      gt_informe_gr.


*&---------------------------------------------------------------------*
*   Proceso final
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  "Muestra el reporte
  PERFORM f_mostrar_reporte.
