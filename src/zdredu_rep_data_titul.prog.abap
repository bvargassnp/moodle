*&---------------------------------------------------------------------*
* Creado por: Hector Ivan Restrepo Saldarriaga ( HRESTREPO ).          *
* Creado el: 26/09/2018 .                                              *
* Orden de Transporte: DCEK909330 PS EDU: Desatrazo Titulaciones M7846 *
* Frente: Educación                                                    *
* ID de desarrollo: Mantis 7846: Programa para desatrazo titulaciones  *
*&---------------------------------------------------------------------*
* ID    | Fecha      | Orden      | Responsable | Descripción          *
*&---------------------------------------------------------------------*
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*
*&---------------------------------------------------------------------*

REPORT zdredu_rep_data_titul.

INCLUDE zdiedu_rep_data_titul_t.
INCLUDE zdiedu_rep_data_titul_s.
INCLUDE zdiedu_rep_data_titul_f.


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
