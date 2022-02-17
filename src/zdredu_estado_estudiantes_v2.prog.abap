*&---------------------------------------------------------------------*
* Creado por: Hector Ivan Restrepo Saldarriaga ( HRESTREPO ).          *
* Creado el: 30/01/2018 .                                              *
* Orden de Transporte: DCEK907483                                      *
*                      PS EDU: Reporte Estado de Estudiantes V2        *
* Frente: Educación                                                    *
* Descripción: Reporte de estado de estudiantes                        *
*&---------------------------------------------------------------------*
* ID    | Fecha      | Orden      | Responsable | Descripción          *
*&---------------------------------------------------------------------*
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*
*&---------------------------------------------------------------------*

REPORT zdredu_estado_estudiantes_v2 .

INCLUDE zdiedu_estado_estudiantes_v2_t.
INCLUDE zdiedu_estado_estudiantes_v2_s.
INCLUDE zdiedu_estado_estudiantes_v2_f.


*&---------------------------------------------------------------------*
*   Proceso principal
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Obtiene los datos
  PERFORM f_obtener_datos
    CHANGING
      gt_informe.


*&---------------------------------------------------------------------*
*   Proceso final
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  "Muestra el reporte
  PERFORM f_mostrar_reporte
    USING
      gt_informe.
