*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_REP_DATA_TITUL_T
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

  BEGIN OF gty_hrp1001,
    otype   TYPE otype,
    objid   TYPE hrobjid,
    plvar   TYPE plvar,
    rsign   TYPE rsign,
    relat   TYPE relat,
    istat   TYPE istat_d,
    priox   TYPE priox,
    begda   TYPE begdatum,
    endda   TYPE enddatum,
    varyf   TYPE varyf,
    seqnr   TYPE seqnr,
    infty   TYPE infotyp,
    otjid   TYPE otjid,
    subty   TYPE subtyp,
    sclas   TYPE sclas,
    sobid   TYPE sobid,
    adatanr TYPE hradatanr,
  END OF gty_hrp1001,

  BEGIN OF gty_otjid,
    otjid TYPE otjid,
  END OF gty_otjid,

  BEGIN OF gty_r_otjid,
    sign   TYPE ddsign,
    option TYPE ddoption,
    low    TYPE otjid,
    high   TYPE otjid,
  END OF gty_r_otjid,

  BEGIN OF gty_adatanr,
    adatanr TYPE hradatanr,
  END OF gty_adatanr,

  BEGIN OF gty_hrpad530,
    adatanr   TYPE hradatanr,
    adm_ayear TYPE piqperyr,
    adm_perid TYPE piqperid,
  END OF gty_hrpad530,

  BEGIN OF gty_objid,
    objid TYPE hrobjid,
  END OF gty_objid,

  BEGIN OF gty_tipo_doc,
    category TYPE bu_id_category,
    text     TYPE bu_text40,
  END OF gty_tipo_doc,

  BEGIN OF gty_data_general,
    taxtype    TYPE bu_id_type,
    idnumber   TYPE bu_id_number,
    institute  TYPE bu_id_institute,
    partner    TYPE bu_partner,
    stobjid    TYPE piqstudent,
    student12  TYPE piqstudent12,
    name_last  TYPE bu_namep_l,
    name_first TYPE bu_namep_f,
    name_lst2  TYPE bu_namepl2,
    namemiddle TYPE bu_namemid,
  END OF gty_data_general,

  BEGIN OF gty_tb039a,
    type TYPE bu_id_type,
  END OF gty_tb039a,

  BEGIN OF gty_cmacbpst,
    partner   TYPE bu_partner,
    student12 TYPE piqstudent12,
    stobjid   TYPE piqstudent,
  END OF gty_cmacbpst,

  BEGIN OF gty_partner,
    partner TYPE bu_partner,
  END OF gty_partner,

  BEGIN OF gty_hrp1000,
    plvar TYPE plvar,
    otype TYPE otype,
    objid TYPE hrobjid,
    istat TYPE istat_d,
    begda TYPE begdatum,
    endda TYPE enddatum,
    langu TYPE langu,
    seqnr TYPE seqnr,
    otjid TYPE otjid,
    stext TYPE stext,
  END OF gty_hrp1000,

  BEGIN OF gty_hrp1002,
    plvar  TYPE plvar,
    otype  TYPE otype,
    objid  TYPE hrobjid,
    subty  TYPE subtyp,
    istat  TYPE istat_d,
    begda  TYPE begdatum,
    endda  TYPE enddatum,
    langu  TYPE langu,
    filler TYPE dummy_9,
    seqnr  TYPE seqnr,
    otjid  TYPE otjid,
    tabnr  TYPE hrtabnr,
  END OF gty_hrp1002,

  BEGIN OF gty_tabnr,
    tabnr TYPE hrtabnr,
  END OF gty_tabnr,

  BEGIN OF gty_hrt1002,
    tabnr    TYPE hrtabnr,
    tabseqnr TYPE hrtabseqnr,
    tline    TYPE hrline79,
  END OF gty_hrt1002,

  BEGIN OF gty_informe,
    stobjid      TYPE piqstudent,
    csobjid      TYPE piqcsobjid,
    student12    TYPE piqstudent12,
    taxtype      TYPE bu_id_type,
    taxtype_text TYPE ztipdoc,
    idnumber     TYPE bu_id_number,
    lug_exp      TYPE zedua_lugced,
    partner      TYPE bu_partner,
    nombre_comp  TYPE zedua_name,
    scobjid      TYPE piqscobjid,
    sc_stext     TYPE stext,
    titulo       TYPE ztitulo,
    insc_per     TYPE zinsc_per,
    procesar     TYPE c,
    result       TYPE icon_d,
  END OF gty_informe.


*&---------------------------------------------------------------------*
*   Declaracion de tipos de tabla
*&---------------------------------------------------------------------*
TYPES:
  gtyt_hrp1000_sh   TYPE TABLE OF gty_hrp1000_sh,
  gtyt_hrp1001      TYPE TABLE OF gty_hrp1001,
  gtyt_otjid        TYPE TABLE OF gty_otjid,
  gtyt_r_otjid      TYPE TABLE OF gty_r_otjid,
  gtyt_adatanr      TYPE TABLE OF gty_adatanr,
  gtyt_hrpad530     TYPE TABLE OF gty_hrpad530,
  gtyt_objid        TYPE TABLE OF gty_objid,
  gtyt_tipo_doc     TYPE TABLE OF gty_tipo_doc,
  gtyt_data_general TYPE TABLE OF gty_data_general,
  gtyt_tb039a       TYPE TABLE OF gty_tb039a,
  gtyt_partner      TYPE TABLE OF gty_partner,
  gtyt_hrp1000      TYPE TABLE OF gty_hrp1000,
  gtyt_hrp1002      TYPE TABLE OF gty_hrp1002,
  gtyt_tabnr        TYPE TABLE OF gty_tabnr,
  gtyt_hrt1002      TYPE TABLE OF gty_hrt1002,
  gtyt_informe      TYPE TABLE OF gty_informe.


*&---------------------------------------------------------------------*
*   Declaracion de constantes
*&---------------------------------------------------------------------*
CONSTANTS:
  gc_icon_gl TYPE icon_d VALUE '@08@',
  gc_icon_rl TYPE icon_d VALUE '@0A@'.


*&---------------------------------------------------------------------*
*   Declaraciones globales
*&---------------------------------------------------------------------*
DATA:
  gv_otjid     TYPE otjid,
  gv_objid     TYPE hrobjid,
  gv_mat       TYPE piqstudent12,
  gv_id        TYPE bu_id_number,
  gv_bname     TYPE usapplref-bname,
  gv_partner   TYPE bu_partner,
  gs_informe   TYPE gty_informe,
  gs_save_st00 LIKE piqst00,
  gt_informe   TYPE gtyt_informe.

FIELD-SYMBOLS:
  <gfs_informe> TYPE gty_informe.

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
        IMPORTING row column,

*     Metodo para el link clic
      on_link_click FOR EVENT link_click OF cl_salv_events_table
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
