*&---------------------------------------------------------------------*
*&  Include           ZDEDU_TABLERO_CTRL_LOTE_TOP
*&---------------------------------------------------------------------*

***************************************************************
*                         TABLES                              *
***************************************************************
TABLES: zedu_p_tab_ctrl.

***************************************************************
*                      TIPOS GLOBALES                         *
***************************************************************
TYPES: BEGIN OF ty_dfkkzk,
         keyz1 TYPE keyz1_kk,
         stazs TYPE stazs_kk,
       END OF ty_dfkkzk.

*TYPES: BEGIN OF ty_tab_ctrl.
*        INCLUDE STRUCTURE zcre_p_tab_ctrl.
*TYPES: status_lote_txt TYPE dd07v-ddtext,
*       END OF ty_tab_ctrl.
TYPES: BEGIN OF ty_tab_ctrl,
         f_proc          TYPE zedu_f_proc,
         keyz1           TYPE keyz1_kk,
         id_archivo      TYPE zedu_id_archivo,
         cod_ent_rec     TYPE zedu_cod_ent_rec,
         lote_aso2001    TYPE zedu_lote_aso2001,
         cant_reg        TYPE zedu_cant_reg_lote,
         imp_total       TYPE zedu_total_lote,
         waers           TYPE waers,
         status          TYPE stazs_kk,
         status_lote_txt TYPE dd07v-ddtext,
       END OF ty_tab_ctrl.

TYPES: gty_tab_ctrl TYPE TABLE OF ty_tab_ctrl.

DATA:
  lv_dir_out TYPE pathextern,
  lv_file    TYPE string.
***************************************************************
*                      DATOS GLOBALES                         *
***************************************************************
DATA: gt_tab_ctrl TYPE STANDARD TABLE OF ty_tab_ctrl,
      gs_tab_ctrl TYPE ty_tab_ctrl,
      gt_upd      TYPE STANDARD TABLE OF zedu_p_tab_ctrl,
      gs_upd      TYPE zedu_p_tab_ctrl,
      gt_dfkkzk   TYPE STANDARD TABLE OF ty_dfkkzk,
      gs_dfkkzk   TYPE ty_dfkkzk,
      gt_values   TYPE STANDARD TABLE OF dd07v,
      gs_values   TYPE dd07v.

CONSTANTS: gc_status  TYPE dd07l-domname VALUE 'STAZS_KK'.

***************************************************************
*                        OBJETOS                              *
***************************************************************
DATA: go_alv          TYPE REF TO cl_salv_table.

***************************************************************
*                 PANTALLA DE SELECCIÓN                       *
***************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
            so_date FOR zedu_p_tab_ctrl-f_proc DEFAULT sy-datum,
            so_lote FOR zedu_p_tab_ctrl-keyz1,
            so_enti FOR zedu_p_tab_ctrl-cod_ent_rec,
            so_aso  FOR zedu_p_tab_ctrl-lote_aso2001.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_gen TYPE xfeld AS CHECKBOX..
SELECTION-SCREEN END OF BLOCK b2.
