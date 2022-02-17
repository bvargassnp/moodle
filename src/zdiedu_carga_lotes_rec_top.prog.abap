*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CARGA_LOTES_REC_TOP
*&---------------------------------------------------------------------*

***************************************************************
*                      TIPOS GLOBALES                         *
***************************************************************

TYPES:
  BEGIN OF ty_dfkkzk,
    keyz1 TYPE keyz1_kk,
    budat TYPE budat_kk,
    valut TYPE valut,
  END OF ty_dfkkzk,

  BEGIN OF ty_cstaxtype,
    type           TYPE zedu_c_cstaxtype-type,
    id_taxtype_leg TYPE zedu_c_cstaxtype-id_taxtype_leg,
  END OF ty_cstaxtype,

  BEGIN OF ty_taxnum,
    taxnum TYPE bptaxnum,
  END OF ty_taxnum,

  BEGIN OF ty_bptaxnum,
    partner TYPE bu_partner,
    taxtype TYPE bptaxtype,
    taxnum  TYPE bptaxnum,
  END OF ty_bptaxnum,

  BEGIN OF ty_dpsob_bp_acc,
    psobkey       TYPE psobkey_ps,
    partner       TYPE gpart_kk,
    partneracctyp TYPE vktyp_kk,
  END OF ty_dpsob_bp_acc.

TYPES: tt_balmi         TYPE TABLE OF balmi.

***************************************************************
*                      DATOS GLOBALES                         *
***************************************************************
*Tablas Interas Lectura Archivo
DATA:  gs_fichero_h    TYPE bfkkzgr00,
       gs_lote_h       TYPE bfkkzk,
       gt_cstaxtype    TYPE TABLE OF ty_cstaxtype,
       gs_cstaxtype    TYPE ty_cstaxtype,
       gt_taxnum       TYPE TABLE OF ty_taxnum,
       gs_taxnum       TYPE ty_taxnum,
       gt_bptaxnum     TYPE TABLE OF ty_bptaxnum,
       gs_bptaxnum     TYPE ty_bptaxnum,
       gt_dpsob_bp_acc TYPE TABLE OF ty_dpsob_bp_acc,
       gs_dpsob_bp_acc TYPE ty_dpsob_bp_acc,
       gt_lote_p       TYPE STANDARD TABLE OF bfkkzp,
       gs_lote_p       TYPE bfkkzp,
       gt_cuentas      TYPE STANDARD TABLE OF zedu_c_cta_comp,
       gs_cuentas      TYPE zedu_c_cta_comp,
       gs_dfkkzk       TYPE ty_dfkkzk,
       gt_param        TYPE STANDARD TABLE OF zedu_c_param,
       gs_param        TYPE zedu_c_param,
       gt_tab_ctrl     TYPE STANDARD TABLE OF zedu_p_tab_ctrl,
       gs_tab_ctrl     TYPE zedu_p_tab_ctrl.

DATA:  gt_mensajes TYPE STANDARD TABLE OF balmi,
       gs_mensajes TYPE balmi.


DATA:  gv_log_handle    TYPE balloghndl,
       gv_error(1)      TYPE c,
       gv_path          TYPE pathextern,
       gv_dir_out       TYPE pathextern,
       gv_dir_err       TYPE pathextern,
       gv_waers         TYPE waers,
       gv_blart         TYPE blart_kk,
       gv_augrd         TYPE augrd_kk,
       gv_selt1         TYPE seltx_kk,
       gv_selt2         TYPE seltx_kk,
       gv_bukrs         TYPE bukrs,
       gv_file          TYPE fnameze_kk,
       gv_file_err      TYPE fnameze_kk,
       gv_lote          TYPE keyz1_kk,
       gv_arch          TYPE rlgrap-filename,
       gv_total_detalle TYPE p DECIMALS 2, "ktsus_b_kk,
       gv_runid         TYPE fkkzest-runid,
       gv_fikey_lote    TYPE dfkksumc-fikey,
       gv_id_arch       TYPE zedu_id_archivo.

***************************************************************
*                        CONSTANTES                           *
***************************************************************
CONSTANTS: gc_object    TYPE bal_s_log-object VALUE 'ZEDU_RECAUDACION',
           gc_subobject TYPE bal_s_log-subobject VALUE 'ZEDU_LOTE',
           gc_msg_level TYPE bal_s_msg-detlevel VALUE '1',
           gc_blart(5)  TYPE c VALUE 'BLART',
           gc_waers(5)  TYPE c VALUE 'WAERS',
           gc_augrd(5)  TYPE c VALUE 'AUGRD',
           gc_selt1(5)  TYPE c VALUE 'SELT1',
           gc_selt2(5)  TYPE c VALUE 'SELT2',
           gc_bukrs(10) TYPE c VALUE 'ZEDU_SOCIE'.

***************************************************************
*                 PANTALLA DE SELECCIÓN                       *
***************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_arch   LIKE file_table-filename OBLIGATORY." Nombre Archivo
SELECTION-SCREEN END OF BLOCK b1.
