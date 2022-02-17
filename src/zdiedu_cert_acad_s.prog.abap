*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CERT_ACAD_S
*&---------------------------------------------------------------------*

"Parametros de entrada
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS:
  so_opbel FOR gv_opbel." OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

* Parametros de Ejecución
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
PARAMETERS :
  pa_vari   TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b03.


*&---------------------------------------------------------------------*
*   Eventos de pantalla
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_vari.
  PERFORM f_buscar_variante.
