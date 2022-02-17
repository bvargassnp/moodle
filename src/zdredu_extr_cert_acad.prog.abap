*&---------------------------------------------------------------------*
* Creado por: Hector Ivan Restrepo Saldarriaga ( HRESTREPO ).          *
* Creado el: 20/11/2017 .                                              *
* Frente: Educación                                                    *
*&---------------------------------------------------------------------*
* ID    | Fecha      | Orden      | Responsable | Descripción          *
*&---------------------------------------------------------------------*
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*
*&---------------------------------------------------------------------*

REPORT zdredu_extr_cert_acad.

INCLUDE zdiedu_extr_cert_acad_t.
INCLUDE zdiedu_extr_cert_acad_s.
INCLUDE zdiedu_extr_cert_acad_f.

*&---------------------------------------------------------------------*
*   Declaraciones
*&---------------------------------------------------------------------*
DATA:
  lv_opbel       TYPE opbel_kk,
  lt_certacad_h  TYPE gtyt_certacad_h,
  lt_certacad_pe TYPE gtyt_certacad_pe,
  lt_certacad_pm TYPE gtyt_certacad_pm,
  lt_certacad_sm TYPE gtyt_certacad_sm,
  lt_certacad_na TYPE gtyt_certacad_na,
  lt_certacad_ps TYPE gtyt_certacad_ps,
  lt_certacad_ro TYPE gtyt_certacad_ro,
  lt_certacad_gr TYPE gtyt_certacad_gr.


*&---------------------------------------------------------------------*
*   Proceso principal
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  "Obtiene los datos
  PERFORM f_obtener_datos
    CHANGING
      lv_opbel
      lt_certacad_h
      lt_certacad_pe
      lt_certacad_pm
      lt_certacad_sm
      lt_certacad_na
      lt_certacad_ps
      lt_certacad_ro
      lt_certacad_gr.


*&---------------------------------------------------------------------*
*   Proceso final
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  "Almacena los datos
  PERFORM f_guardar_datos
    USING
      lv_opbel
      lt_certacad_h
      lt_certacad_pe
      lt_certacad_pm
      lt_certacad_sm
      lt_certacad_na
      lt_certacad_ps
      lt_certacad_ro
      lt_certacad_gr.
