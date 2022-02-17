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

REPORT zdredu_cargar_actos.

INCLUDE zdiedu_cargar_actos_t.
INCLUDE zdiedu_cargar_actos_s.
INCLUDE zdiedu_cargar_actos_f.


*&---------------------------------------------------------------------*
*   Proceso principal
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Lee el archivo de entrada
  PERFORM f_procesar_archivo.
