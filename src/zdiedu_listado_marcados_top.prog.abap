*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_LISTADO_MARCADOS_TOP
*&---------------------------------------------------------------------*

TABLES: hrpad506, cmacbpst.

TYPES: BEGIN OF gty_data,
*         nro_documen TYPE bu_id_number,
         partner     TYPE bu_partner,
         matricula   TYPE piqstudent12,
         st          TYPE piqstudent,
         nombre      TYPE char120,
*         prog          TYPE stext,
*         sdo_direccion TYPE char45,
*         tel_movil     TYPE numc10,
*         tel_fijo      TYPE numc10,
*         email         TYPE text60,
*         pin           TYPE zedu_pin,
       END OF gty_data.

TYPES: BEGIN OF gty_marca,
         adatanr TYPE hradatanr,

       END OF gty_marca.

TYPES: BEGIN OF gty_prog,
         objid TYPE hrobjid,
         short TYPE short_d,
         stext TYPE stext,
       END OF gty_prog.

TYPES: BEGIN OF gty_1001,
         objid   TYPE hrobjid,
         otype   TYPE otype,
         endda   TYPE enddatum,
         adatanr TYPE hradatanr,
       END OF gty_1001.

TYPES: BEGIN OF gty_but000,
         partner    TYPE bu_partner,
         name_last  TYPE bu_namep_l,
         name_first TYPE bu_namep_f,
         name_lst2  TYPE bu_namepl2,
         namemiddle TYPE bu_namemid,
       END OF gty_but000.

DATA: gt_hrpad  TYPE STANDARD TABLE OF hrpad506,
      gt_1001   TYPE STANDARD TABLE OF gty_1001,
      gt_bpst   TYPE STANDARD TABLE OF cmacbpst,
      gt_but000 TYPE STANDARD TABLE OF gty_but000,
      gt_data   TYPE STANDARD TABLE OF gty_data.

*DATA: gt_data     TYPE STANDARD TABLE OF gty_data,
*      gt_zpre_adm TYPE STANDARD TABLE OF gty_zpre_adm,
*      gt_but0id   TYPE STANDARD TABLE OF but0id,
*      gt_prog     TYPE STANDARD TABLE OF gty_prog,
*      gt_but000   TYPE STANDARD TABLE OF gty_but000,
*      gt_pin      TYPE STANDARD TABLE OF zslcm_pin,
*      gt_cmacbpst TYPE STANDARD TABLE OF cmacbpst,
*      gt_hrp1001  TYPE STANDARD TABLE OF hrp1001.

*---------------------------------------------------------------------*
*         Declaración de objetos
*---------------------------------------------------------------------*
DATA: go_alv    TYPE REF TO cl_salv_table.

SELECTION-SCREEN BEGIN OF BLOCK  b1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS: so_bp    FOR cmacbpst-partner OBLIGATORY.
PARAMETERS: p_est   LIKE hrpad506-pago,
            pa_fecha TYPE sy-datum.
*PARAMETERS: rb_ins  TYPE xfeld USER-COMMAND x RADIOBUTTON GROUP g1 DEFAULT 'X',
*            rb_pre  TYPE xfeld                RADIOBUTTON GROUP g1,
*            ch_stat TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
