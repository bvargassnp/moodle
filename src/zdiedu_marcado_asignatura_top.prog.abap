*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_MARCADO_ASIGNATURA_TOP
*&---------------------------------------------------------------------*


TYPES: BEGIN OF gty_archivo,
         bp      TYPE gpart_kk,
* Inicio M6704 - HRESTREPO - 04/07/2018
*         opbel   TYPE opbel_kk,
*         begda   TYPE begdatum,
*         endda   TYPE enddatum,
* Fin M6704 - HRESTREPO - 04/07/2018
         csobjid TYPE piqcsobjid,
* Inicio M6704 - HRESTREPO - 04/07/2018
         peryr   TYPE piqperyr,
         perid   TYPE piqperid,
* Fin M6704 - HRESTREPO - 04/07/2018
       END OF gty_archivo.

TYPES: BEGIN OF gty_log,
         bp     TYPE gpart_kk,
         opbel  TYPE opbel_kk,
         begda  TYPE begdatum,
         endda  TYPE enddatum,
         st     TYPE hrobjid,
         st_ant TYPE hrobjid,
         icono  TYPE icon_d,
         resul  TYPE char50,
       END OF gty_log.
*TYPES: BEGIN OF gt_
DATA: gt_archivo TYPE STANDARD TABLE OF gty_archivo,
      gt_log     TYPE STANDARD TABLE OF gty_log,
      gt_hrp1001 TYPE STANDARD TABLE OF hrp1001,
      gt_bpst    TYPE STANDARD TABLE OF cmacbpst,
      gt_rel_st  TYPE STANDARD TABLE OF zedu_matricula.

DATA: go_alv    TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK grupo1 WITH FRAME TITLE text-000.
PARAMETERS:  p_file LIKE rlgrap-filename DEFAULT '.txt',
             p_pago TYPE zedu_pago.
*             p_test TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK grupo1.
