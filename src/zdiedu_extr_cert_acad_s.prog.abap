*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_EXTR_CERT_ACAD_S
*&---------------------------------------------------------------------*

"Parametros de entrada
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS:
  pa_opbel TYPE opbel_kk OBLIGATORY,
  pa_block TYPE int4 DEFAULT 5000 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.
