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

REPORT zdredu_asignar_titulacion.

INCLUDE zdiedu_asignar_titulacion_t.
INCLUDE zdiedu_asignar_titulacion_s.
INCLUDE zdiedu_asignar_titulacion_f.


*&---------------------------------------------------------------------*
*   Declaraciones
*&---------------------------------------------------------------------*
DATA:
  lt_datos TYPE gtyt_datos,
  lt_log   TYPE gtyt_log.


*&---------------------------------------------------------------------*
*   Proceso principal
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Lee el archivo de entrada
  PERFORM f_leer_archivo
    CHANGING
      lt_datos.

*&---------------------------------------------------------------------*
*   Proceso final
*&---------------------------------------------------------------------*
  PERFORM f_procesar_datos
    CHANGING
      lt_datos
      lt_log.

  "Crea los archivos de log
  PERFORM f_crear_log
    USING
      lt_datos
      lt_log.
