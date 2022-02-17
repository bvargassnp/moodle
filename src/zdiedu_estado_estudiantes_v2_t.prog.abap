*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_ESTADO_ESTUDIANTES_V2_T
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*   Declaracion de estructuras
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF gty_hrp1000_sh,
    objid TYPE hrobjid,
    short TYPE short_d,
    stext TYPE stext,
  END OF gty_hrp1000_sh,

  BEGIN OF gty_progclasst,
    progclass  TYPE piqprogclass,
    progclasst TYPE piqprogclasst,
  END OF gty_progclasst,

  BEGIN OF gty_marstt,
    marst TYPE bu_marst,
    bez20 TYPE bu_bez20,
  END OF gty_marstt,

  BEGIN OF gty_idtypet,
    category TYPE bu_id_category,
    text     TYPE bu_text40,
  END OF gty_idtypet,

  BEGIN OF gty_prog_acstt,
    progc_var TYPE piqprogc_var,
    acst      TYPE piqprog_gr_acst,
    acst_txt  TYPE piqprog_gr_acst_txt,
  END OF gty_prog_acstt,

  BEGIN OF gty_smstatust,
    smstatus  TYPE piqsmstatus,
    smstatust TYPE piqsmstatust,
  END OF gty_smstatust,

  BEGIN OF gty_hrp1737_idx,
    prog_type TYPE piqprog_type,
    otype     TYPE otype,
    progc_var TYPE piqprogc_var,
    objid     TYPE hrobjid,
  END OF gty_hrp1737_idx,

  BEGIN OF gty_hrp1737,
    plvar     TYPE plvar,
    otype     TYPE otype,
    objid     TYPE hrobjid,
    subty     TYPE subtyp,
    istat     TYPE istat_d,
    begda     TYPE begdatum,
    endda     TYPE enddatum,
    varyf     TYPE varyf,
    seqnr     TYPE seqnr,
    otjid     TYPE otjid,
    progc_var TYPE piqprogc_var,
    prog_type TYPE piqprog_type,
    tabnr     TYPE hrtabnr,
  END OF gty_hrp1737,

  BEGIN OF gty_tabnr,
    tabnr TYPE hrtabnr,
  END OF gty_tabnr,

  BEGIN OF gty_hrt1737,
    tabnr     TYPE hrtabnr,
    tabseqnr  TYPE hrtabseqnr,
    progc_var TYPE piqprogc_var,
    prog_type TYPE piqprog_type,
    acst      TYPE piqprog_gr_acst,
    prcl      TYPE piqprogclass,
    peryr     TYPE piqprog_gr_peryr,
    perid     TYPE piqprog_gr_perid,
  END OF gty_hrt1737,

  BEGIN OF gty_hsstatush,
    subty    TYPE subtyp,
    sutxt    TYPE sutext,
    hs_otype TYPE otype,
  END OF gty_hsstatush,

  BEGIN OF gty_objid,
    objid TYPE hrobjid,
  END OF gty_objid,

  BEGIN OF gty_hrp1728,
    plvar    TYPE plvar,
    otype    TYPE otype,
    objid    TYPE hrobjid,
    subty    TYPE subtyp,
    istat    TYPE istat_d,
    begda    TYPE begdatum,
    endda    TYPE enddatum,
    varyf    TYPE varyf,
    seqnr    TYPE seqnr,
    otjid    TYPE otjid,
    hs_state TYPE piqhs_state,
  END OF gty_hrp1728,

  BEGIN OF gty_hrp9121,
    plvar  TYPE plvar,
    otype  TYPE otype,
    objid  TYPE hrobjid,
    subty  TYPE subtyp,
    istat  TYPE istat_d,
    begda  TYPE begdatum,
    endda  TYPE enddatum,
    varyf  TYPE varyf,
    seqnr  TYPE seqnr,
    infty  TYPE infotyp,
    otjid  TYPE otjid,
    epscod TYPE zedu_epscod,
  END OF gty_hrp9121,

  BEGIN OF gty_eps,
    codigo      TYPE numc4,
    descripcion TYPE char100,
  END OF gty_eps,

  BEGIN OF gty_informe,
    mat_ayear          TYPE piqperyr,
    mat_perid          TYPE piqperid,
    i01_type           TYPE bu_id_type,
    i01_typet          TYPE bu_text40,
    i01_idnumber       TYPE bu_id_number,
    i02_name_last      TYPE bu_namep_l,
    i02_name_first     TYPE bu_namep_f,
    i02_name_lst2      TYPE bu_namepl2,
    i02_namemiddle     TYPE bu_namemid,
    i02_genero         TYPE c LENGTH 6,
    i02_birthdt        TYPE bu_birthdt,
    i02_marst          TYPE bu_marst,
    i02_marstt         TYPE bu_bez20,
    partner            TYPE bu_partner,
    stobjid            TYPE piqst_objid,
    csobjid            TYPE piqcsobjid,
    student12          TYPE piqstudent12,
    matriculado_n	     TYPE zies_matric_n,
    scobjid            TYPE piqscobjid,
    scstext            TYPE hr_mcstext,
    i10_linea_edu      TYPE zedu_linea_e,
    i10_snies          TYPE zedu_snies,
    i10_regio          TYPE regio,
    i10_city_code      TYPE city_code,
    sm_inscritas       TYPE zies_sm_inscritas,
    sm_aprobadas       TYPE zies_sm_aprobadas,
    sm_canceladas      TYPE zies_sm_canceladas,
    sm_cpattemp        TYPE piqcredit, "piqcpattemp,
    acst               TYPE piqprog_gr_acst,
    acst_txt           TYPE piqprog_gr_acst_txt,
    prcl               TYPE piqprogclass,
    progclasst         TYPE piqprogclasst,
    i00_adm_ayear      TYPE piqperyr,
    i00_adm_perid      TYPE piqperid,
    i00_adm_aclevel    TYPE piqlevel,
    i00_adm_categ      TYPE piqadm_categ,
    i00_adm_categt     TYPE piqadm_categt,
    i00_adm_enrcateg   TYPE piqenrcateg,
    i00_enrcategt      TYPE piqenrcategt,
    i00_adm_recpt      TYPE piqadm_recpt,
    i03_country        TYPE land1,
    i03_region         TYPE regio,
    i03_city_code      TYPE ad_citynum,
    i03_city1          TYPE ad_city1,
    i04_tel_number     TYPE ad_tlnmbr,
    i04_cel_number     TYPE ad_tlnmbr,
    i05_smtp_addr      TYPE ad_smtpadr,
    i05_smtp_addr_aux  TYPE ad_smtpadr,
    i07_social         TYPE piqsocial,
    epscod             TYPE zedu_epscod,
    epstext            TYPE zedu_eps-descripcion,
    i09_snp            TYPE char30,
    i09_saber_11       TYPE zsaber11,
    i09_colegio        TYPE zies_slcm-i09_colegio,
    i09_otro_col       TYPE zotro_col,
    i09_grado_act      TYPE zedu_grado,
    i09_titulo         TYPE text30,
    i09_ano            TYPE numc4,
    i09_pais_proc      TYPE land1,
    i09_pais_proct     TYPE landx50,
    i09_dpto_proc      TYPE regio,
    i09_dpto_proct     TYPE bezei20,
    i09_ciudad         TYPE cityc,
    i09_ciudadt        TYPE bezei20,
    i09_tarjeta_pr1    TYPE ztar_pr,
    i09_universidad    TYPE char30,
    i09_otra_uni       TYPE zotra_uni,
    i09_titulo_u       TYPE text30,
    i09_ano_u          TYPE numc4,
    i09_pais_u         TYPE land1,
    i09_pais_ut        TYPE landx50,
    i09_region_u       TYPE regio,
    i09_region_ut      TYPE bezei20,
    i09_ciudad_u       TYPE cityc,
    i09_ciudad_ut      TYPE bezei20,
    i11_beg_key_date   TYPE piqregdate,
    i11_beg_process    TYPE piqprocess_beg,
    i11_beg_processt   TYPE piqprocess2t,
    i11_beg_reason     TYPE piqprocreason,
    i11_beg_reasontext TYPE piqreasont,
    i11_end_key_date   TYPE piqderdate,
    i11_end_process    TYPE piqprocess_end,
    i11_end_processt   TYPE piqprocess2t,
    i11_end_reason     TYPE piqderreason,
    i11_end_reasontext TYPE piqreasont,
    i11_last_attend    TYPE piqreg_last_attend,
    profit_ctr         TYPE prctr,
    cs_bloqueos        TYPE zies_csbloqueos_cod,
    mat_financiera_n   TYPE i,
    mat_persl          TYPE persl_kk,
    ult_faedn          TYPE faedn_kk,
    pago_de            TYPE zies_pago_de,
    pago_dr            TYPE zies_pago_dr,
    pago_ce            TYPE zies_pago_ce,
    pago_cr            TYPE zies_pago_cr,
    pago_total         TYPE zies_pago_total,
    total_facturado    TYPE zies_total_facturado,
    total_deuda        TYPE zies_total_deuda,
    deuda_ar           TYPE zies_deuda_ar,
    deuda_ae           TYPE zies_deuda_ae,
    deuda_a_re         TYPE zies_deuda_a_re,
    deuda_ao           TYPE zies_deuda_ao,
    recargo            TYPE zies_recargo,
    descuento          TYPE zies_descuento,
    descuento_cod      TYPE zies_descuento_cod,
    pp_finan           TYPE zies_pp_financ,
    pp_finan_cod       TYPE zies_pp_financ_cod,
    subvencion         TYPE zies_subvencion,
    subvencion_cod     TYPE zies_subvencion_cod,
    intereses          TYPE zies_intereses,
    mat_prs_state      TYPE piqreg_prs_state,
    smobjid            TYPE piqsmobjid,
    smstext            TYPE hr_mcstext,
    seqnr              TYPE seqnr,
    smstatus           TYPE piqsmstatus,
    smstatust          TYPE piqsmstatust,
    smpago             TYPE zedu_pago,
  END OF gty_informe.


*&---------------------------------------------------------------------*
*   Declaracion de tipos de tabla
*&---------------------------------------------------------------------*
TYPES:
  gtyt_hrp1000_sh  TYPE TABLE OF gty_hrp1000_sh,
  gtyt_progclasst  TYPE TABLE OF gty_progclasst,
  gtyt_marstt      TYPE TABLE OF gty_marstt,
  gtyt_idtypet     TYPE TABLE OF gty_idtypet,
  gtyt_prog_acstt  TYPE TABLE OF gty_prog_acstt,
  gtyt_smstatust   TYPE TABLE OF gty_smstatust,
  gtyt_hrp1737_idx TYPE TABLE OF gty_hrp1737_idx,
  gtyt_hrp1737     TYPE TABLE OF gty_hrp1737,
  gtyt_tabnr       TYPE TABLE OF gty_tabnr,
  gtyt_hrt1737     TYPE TABLE OF gty_hrt1737,
  gtyt_hsstatush   TYPE TABLE OF gty_hsstatush,
  gtyt_objid       TYPE TABLE OF gty_objid,
  gtyt_hrp1728     TYPE TABLE OF gty_hrp1728,
  gtyt_hrp9121     TYPE TABLE OF gty_hrp9121,
  gtyt_eps         TYPE TABLE OF gty_eps,
  gtyt_informe     TYPE TABLE OF gty_informe.


*&---------------------------------------------------------------------*
*   Declaraciones globales
*&---------------------------------------------------------------------*
DATA:
  gv_datum     TYPE datum,
  gv_fcat      TYPE hrp1732-scfeecat,
  gv_objid     TYPE hrobjid,
  gv_student12 TYPE piqstudent12,
  gv_partner   TYPE bu_partner,
  gv_idtype    TYPE but0id-type,
  gv_idnumber  TYPE bu_id_number,
  gs_informe   TYPE gty_informe,
  gt_informe   TYPE gtyt_informe.


*&-----------------------------------------------------------------------*
*   Objetos y variables para la visualizacion del ALV
*&-----------------------------------------------------------------------*
* Defino la clase para manejar los eventos del reporte ALV
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:

*     Metodo para las funciones adicionales puestas en el menu
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,

*     Metodo para el doble clic.
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.

ENDCLASS.                    "lcl_handle_events DEFINITION


*&---------------------------------------------------------------------*
*   Declaraciones ALV
*&---------------------------------------------------------------------*
DATA:
  gs_key        TYPE        salv_s_layout_key,
  gr_table      TYPE REF TO cl_salv_table,
  gr_functions  TYPE REF TO cl_salv_functions,
  gr_display    TYPE REF TO cl_salv_display_settings,
  gr_columns    TYPE REF TO cl_salv_columns_table,
  gr_column     TYPE REF TO cl_salv_column_table,
  gr_layout     TYPE REF TO cl_salv_layout,
  gr_grid       TYPE REF TO cl_salv_form_layout_grid,
  gr_grid_1     TYPE REF TO cl_salv_form_layout_grid,
  gr_flow       TYPE REF TO cl_salv_form_layout_flow,
  gr_label      TYPE REF TO cl_salv_form_label,
  gr_selections TYPE REF TO cl_salv_selections,
  gr_events     TYPE REF TO cl_salv_events_table,
  event_handler TYPE REF TO lcl_handle_events.
