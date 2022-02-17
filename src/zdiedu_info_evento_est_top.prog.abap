*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_INFO_EVENTO_EST_TOP
*&---------------------------------------------------------------------*

TABLES: hrp1001.

TYPES: BEGIN OF gty_hrp1001,
         otype  TYPE hrp1001-otype,
         objid  TYPE hrp1001-objid,
         sclas  TYPE hrp1001-sclas,
         sobid  TYPE hrp1001-sobid,
         begda  TYPE hrp1001-begda,
         objid2 TYPE hrp1001-objid,
       END OF gty_hrp1001.

TYPES: BEGIN OF gty_data,
         id_st_a   TYPE hrp1001-objid,
         id_st_v   TYPE hrp1001-objid,
         id_evento TYPE hrp1001-objid,
         id_sm_a   TYPE hrp1001-objid,
         id_sm_v   TYPE hrp1001-objid,
       END OF gty_data.

*TYPES: BEGIN OF gty_edu_matri,
*         id_anterior	TYPE hrobjid,
*         id_actual	  TYPE hrobjid,
*       END OF gty_edu_matri.

DATA: gt_sm_d   TYPE STANDARD TABLE OF gty_hrp1001,
      gt_d_e    TYPE STANDARD TABLE OF gty_hrp1001,
      gt_e_st   TYPE STANDARD TABLE OF gty_hrp1001,
      gt_rel_sm TYPE STANDARD TABLE OF zedu_matricula,
      gt_rel_st TYPE STANDARD TABLE OF zedu_matricula,
      gt_data   TYPE STANDARD TABLE OF gty_data.

*---------------------------------------------------------------------*
*         Declaración de objetos
*---------------------------------------------------------------------*
DATA: go_alv    TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK  b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_asig FOR hrp1001-objid.
SELECTION-SCREEN END OF BLOCK b1.
