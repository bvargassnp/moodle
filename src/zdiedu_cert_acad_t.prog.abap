*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CERT_ACAD_T
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*   Declaracion de estructuras de datos
*&---------------------------------------------------------------------*
TYPES:
  gty_informe    TYPE zedu_certacad_h,
  gty_informe_pe TYPE zedu_certacad_pe,
  gty_informe_pm TYPE zedu_certacad_pm,
  gty_informe_sm TYPE zedu_certacad_sm,
  gty_informe_na TYPE zedu_certacad_na,
  gty_informe_ps TYPE zedu_certacad_ps,
  gty_informe_gr TYPE zedu_certacad_gr.

TYPES:
  BEGIN OF gty_informe_ro.
        INCLUDE TYPE zedu_certacad_ro.
TYPES:
  stext_rec TYPE stext,
  END OF gty_informe_ro,

  BEGIN OF gty_hrp1000,
    plvar TYPE plvar,
    otype TYPE otype,
    objid TYPE hrobjid,
    istat TYPE istat_d,
    begda TYPE begdatum,
    endda TYPE enddatum,
    langu TYPE langu,
    seqnr TYPE seqnr,
    stext TYPE stext,
  END OF gty_hrp1000.


*&---------------------------------------------------------------------*
*   Declaracion de tipos de tablas de datos
*&---------------------------------------------------------------------*
TYPES:
  gtyt_informe    TYPE TABLE OF gty_informe,
  gtyt_informe_pe TYPE TABLE OF gty_informe_pe,
  gtyt_informe_pm TYPE TABLE OF gty_informe_pm,
  gtyt_informe_sm TYPE TABLE OF gty_informe_sm,
  gtyt_informe_na TYPE TABLE OF gty_informe_na,
  gtyt_informe_ps TYPE TABLE OF gty_informe_ps,
  gtyt_informe_ro TYPE TABLE OF gty_informe_ro,
  gtyt_informe_gr TYPE TABLE OF gty_informe_gr,
  gtyt_hrp1000    TYPE TABLE OF gty_hrp1000.


*&---------------------------------------------------------------------*
*   Declaracion de variables globales
*&---------------------------------------------------------------------*
DATA:
  gv_opbel      TYPE opbel_kk,
  gt_informe    TYPE gtyt_informe,
  gt_informe_pe TYPE gtyt_informe_pe,
  gt_informe_pm TYPE gtyt_informe_pm,
  gt_informe_sm TYPE gtyt_informe_sm,
  gt_informe_na TYPE gtyt_informe_na,
  gt_informe_ps TYPE gtyt_informe_ps,
  gt_informe_ro TYPE gtyt_informe_ro,
  gt_informe_gr TYPE gtyt_informe_gr.


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
