*&---------------------------------------------------------------------*
*&  Include           ZDREDU_LIQUIDAC_PAGO_FORM_TOP
*&---------------------------------------------------------------------*
TABLES: sscrfields.

CONSTANTS: co_andpersand(1)  TYPE c VALUE '&',
           co_x(1)           TYPE c VALUE 'X',
           co_o(1)           TYPE c VALUE 'O',
           co_d(1)           TYPE c VALUE 'D',
           co_a(1)           TYPE c VALUE 'A',
           co_g(1)           TYPE C VALUE 'G',
           co_s(1)           TYPE c VALUE 'S',
           co_e(1)           TYPE c VALUE 'E',
           co_i(1)           TYPE c VALUE 'I',
           co_p(1)           TYPE c VALUE 'P',
           co_01(2)          TYPE c VALUE '01',
           co_st(2)          TYPE c VALUE 'ST',
           co_rc(2)          TYPE c VALUE 'RC',
           co_eq(2)          TYPE c VALUE 'EQ',
           co_pp(2)          TYPE c VALUE 'PP',
           co_fm(2)          TYPE c VALUE 'FM',
           co_nf(2)          TYPE c VALUE 'NF',
           co_fc(2)          TYPE c VALUE 'FC',
           co_ml(2)          TYPE c VALUE 'ML',
           co_fo(2)          TYPE c VALUE 'FO',
           co_fe1(3)         TYPE c VALUE 'FE1',
           co_fe2(3)         TYPE c VALUE 'FE2',
           co_cop(3)         TYPE c VALUE 'COP',
           co_fov(3)         TYPE c VALUE 'FOV',
           co_uces(4)        TYPE c VALUE 'UCES',
           co_disp(4)        TYPE c VALUE 'DISP',
           co_0800(4)        TYPE c VALUE '0800',
           co_o033(4)        TYPE c VALUE 'O041',
           co_cursor(6)      TYPE c VALUE 'CURSOR',
           co_p_selliq(8)    TYPE c VALUE 'P_SELLIQ',
           co_p_ordina(8)    TYPE c VALUE 'P_ORDINA',
           co_p_extra1(8)    TYPE c VALUE 'P_EXTRA1',
           co_p_extra2(8)    TYPE c VALUE 'P_EXTRA2',
           co_p_nrform(8)    TYPE c VALUE 'P_NRFORM',
           co_p_ordval(8)    TYPE c VALUE 'P_ORDVAL',
           co_zedu_rcupo(10) TYPE c VALUE 'ZEDU_RCUPO',
           co_doc_type(10)   TYPE c VALUE 'DOC_TYPE',
           co_main_trans(10) TYPE c VALUE 'MAIN_TRANS',
           co_sub_trans(10)  TYPE c VALUE 'SUB_TRANS',
           co_appl_area(10)  TYPE c VALUE 'APPL_AREA',
           co_comp_code(10)  TYPE c VALUE 'COMP_CODE',
           co_currency(10)   TYPE c VALUE 'CURRENCY',
           co_segment(10)    TYPE c VALUE 'SEGMENT',
           co_prctr(10)      TYPE c VALUE 'PRCTR',
           gc_fac_academ     TYPE blart_kk VALUE `FA`,  "-->  MgM DCEK901536
           gc_reser_cupo     TYPE blart_kk VALUE `RC`.

TYPES: BEGIN OF tys_contratos,
         vtref    TYPE dfkkop-vtref,
         blart    TYPE dfkkop-blart,
         hvorg    TYPE dfkkop-hvorg,
         psobtyp  TYPE dfkkop-psobtyp,
         psobtypt TYPE tpsob001t-psobtypt,
       END OF tys_contratos,

       BEGIN OF tys_contr_liqui,
         psobtyp TYPE zedu_contr_liqui-psobtyp,
       END OF tys_contr_liqui,

       BEGIN OF tys_estudiantes,
         plvar          TYPE plvar,
         otype          TYPE otype,
         objid          TYPE objektid,
         partner        TYPE bu_partner,
         no_generar_liq TYPE check,
       END OF tys_estudiantes,

       BEGIN OF tys_cmacbpst,
         partner TYPE cmacbpst-partner,
         stobjid TYPE cmacbpst-stobjid,
       END OF tys_cmacbpst,

       BEGIN OF tys_dfkkop,
         xblnr TYPE dfkkop-xblnr,
         opbel TYPE dfkkop-opbel,
         gpart TYPE dfkkop-gpart,
         blart TYPE dfkkop-blart,
       END OF tys_dfkkop,

       BEGIN OF tys_dpsob_bp_acc,
         psobkey       TYPE dpsob_bp_acc-psobkey,
         partner       TYPE dpsob_bp_acc-partner,
         partneracctyp TYPE dpsob_bp_acc-partneracctyp,
         partneracc    TYPE dpsob_bp_acc-partneracc,
       END OF tys_dpsob_bp_acc,

       BEGIN OF tys_range_psobtyp,
         sign   TYPE char1,
         option TYPE char2,
         low    TYPE dfkkop-psobtyp,
         high   TYPE dfkkop-psobtyp,
       END OF tys_range_psobtyp,

       BEGIN OF tys_param,
         idparam TYPE zedu_c_param-idparam,
         valor   TYPE zedu_c_param-valor,
       END OF tys_param,

       tyt_dfkkop       TYPE STANDARD TABLE OF tys_dfkkop,
       tyt_param        TYPE STANDARD TABLE OF tys_param,
       tyt_contratos    TYPE STANDARD TABLE OF tys_contratos,
       tyt_contr_liqui  TYPE STANDARD TABLE OF tys_contr_liqui,
       tyt_cmacbpst     TYPE STANDARD TABLE OF tys_cmacbpst,
       tyt_estudiantes  TYPE STANDARD TABLE OF tys_estudiantes,
       tyt_dpsob_bp_acc TYPE STANDARD TABLE OF tys_dpsob_bp_acc.

DATA: gt_main             TYPE zedu_t_liquidacion_pago_items,
      gt_metodos_liq      TYPE TABLE OF zedu_metodos_liq,
      gs_header           TYPE zedu_s_liquidacion_pago_header,
      gt_documentos       TYPE tyt_dfkkop,
      gs_metodo_liq       TYPE zedu_metodos_liq,
*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 09/11/2016
      gt_documentos_acad  TYPE SORTED TABLE OF tys_dfkkop WITH NON-UNIQUE KEY primary_key  COMPONENTS xblnr opbel gpart,
      gt_documentos_otros TYPE STANDARD TABLE OF tys_dfkkop WITH KEY opbel gpart,
      gt_estudiantes      TYPE tyt_estudiantes.

*	End	  -->	MgM DCEK901536

DATA: gv_cursor TYPE char30.

DATA:
*Variable de referencia del método de selección
  gr_selappif      TYPE REF TO if_hrpiq00selmethod_appif,
*  Variable de referencia de la interface
  g_selmethods_ref TYPE REF TO cl_hrpiq00selmethods,
*  Período de selección
  gs_selperiod     TYPE piq_period.

*3. definición de una clase local
CLASS lcl_selappif DEFINITION.
  PUBLIC SECTION.
*   Implementación de la interface del método de selección
    INTERFACES: if_hrpiq00selmethod_appif.
ENDCLASS.                    "
