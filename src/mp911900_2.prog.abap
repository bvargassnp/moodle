* modulpool infotype 9119
PROGRAM mp911900 MESSAGE-ID 5a.
INCLUDE mph5atop.                      "header
TABLES: wplog,
        pppar, pphdr, pphdx, ppsel, ppenq,
        t777o, t777p, t777s, t777t, HRT9119,
        p1000, p1001, p9119,
*       Estas son la SLCM
        hri9119, hrp9119.

INCLUDE mphcom00.                      "common areas
INCLUDE fhvtab00.                      "update tables
INCLUDE fhview00.                      "USER-VIEW
INCLUDE mphfcod0.                      "function codes
INCLUDE mphdat00.                      "general data
INCLUDE mphpbo00.                      "PBO modules
INCLUDE mphpai00.                      "PAI modules
INCLUDE MP911920.                      "OUTPUT modules
INCLUDE mp9119bi.                      "OUTPUT modules

INCLUDE <icon>.
TABLES : sscrfields.

DATA : tab_dat     TYPE REF TO data,
       dock_cont   TYPE REF TO cl_gui_custom_container, "cl_gui_docking_container,
       dialog_cont TYPE REF TO cl_gui_dialogbox_container,
       alv_grid    TYPE REF TO cl_gui_alv_grid,
       i_fcat      TYPE lvc_t_fcat,
       s_layo      TYPE lvc_s_layo,
       dynnr       TYPE sy-dynnr,
       repid       TYPE sy-repid.

CONSTANTS:
  gc_yes          TYPE char1 VALUE 'X',
  gc_no           TYPE char1 VALUE '',
  gc_create       TYPE ui_func VALUE 'FC_CREATE',
  gc_modify       TYPE ui_func VALUE 'FC_MODIFY',
  gc_delete       TYPE ui_func VALUE 'FC_DELETE',
  gc_display      TYPE ui_func VALUE 'FC_DISPLAY',
  gc_ok           TYPE ui_func VALUE 'FC_ENTER',
  gc_save         TYPE ui_func VALUE 'FC_SAVE',
  gc_cancel       TYPE ui_func VALUE 'FC_CANCEL',
  gc_list_changed TYPE ui_func VALUE 'FC_LIST_CHANGED',
*    gc_struct           TYPE dd02l-tabname VALUE 'PIQADVASSGNDETAIL',
  gc_end_date     TYPE enddatum VALUE '99991231',
  gc_x_pos        TYPE i VALUE 5,
  gc_y_pos        TYPE i VALUE 5.

DATA objid_sc TYPE hrp1000-objid.
DATA otype_sc TYPE hrp1000-otype.

DATA ucomm TYPE sy-ucomm.

DATA: dynpro_name   LIKE d020s-prog,
      dynpro_number LIKE d020s-dnum.

DATA: BEGIN OF dynpro_fields OCCURS 0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpro_fields.

TYPES: BEGIN OF employee.
        INCLUDE STRUCTURE hri9119.
TYPES END OF employee.

DATA: BEGIN OF i_hrp9119 OCCURS 0.
        INCLUDE STRUCTURE hrp9119.
DATA: END OF i_hrp9119.

DATA: BEGIN OF i_employee OCCURS 0.
DATA: traffic_light TYPE c.
DATA: line_color(4) TYPE c.
DATA: END OF i_employee.

DATA: ok_code         LIKE sy-ucomm,
      g_ok_code       TYPE sy-ucomm,
      g_modal_ok_code TYPE sy-ucomm,
      wa_employee     LIKE LINE OF i_employee,
      wa_hrp9119      LIKE LINE OF i_hrp9119,
      gv_old_logic    TYPE c LENGTH 1,
      l_repid         TYPE sy-repid,
      l_variant       TYPE disvariant,
      gs_layout       TYPE lvc_s_layo,
      v_mes           TYPE i VALUE 1.

DATA: wa_estudiante TYPE employee.

DATA: wa_change LIKE hrp9119.
DATA:
  gi_index_rows  TYPE lvc_t_row,     " Internal table
  g_selected_row LIKE lvc_s_row.    " Information about 1 row


DATA: grid1              TYPE REF TO cl_gui_alv_grid,
      i_custom_container TYPE REF TO cl_gui_custom_container.

INITIALIZATION.

  dynnr = 1000.
  repid = sy-repid.



*&---------------------------------------------------------------------*
*&      Module  MP911900_ST_GENERALDATA_PBOO01  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mp911900_st_generaldata_pboo01 OUTPUT.

  DATA: lv_ok_code     TYPE sy-ucomm,
        ls_info        TYPE piqst_nf_dtinfo,
        lv_conv_needed TYPE flag.

  CALL FUNCTION 'HRIQ_ST_NF_MD_DETAIL_INFO_GET'
    IMPORTING
      info = ls_info.

  CLEAR wa_hrp9119.
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF  wa_hrp9119 FROM hrp9119
      WHERE plvar = ls_info-plvar AND
            otype = ls_info-otype AND
            objid = ls_info-objid AND
            endda = '99991231'.

ENDMODULE.                 " MP911900_ST_GENERALDATA_PBOO01  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MP911900_USER_COMMAND_2000I01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mp911900_user_command_2000i01 INPUT.


ENDMODULE. " MP911900_USER_COMMAND_2000I01  INPUT
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

ENDMODULE.                 " status_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  change_details
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_details.
  REFRESH gi_index_rows.
  CLEAR g_selected_row.
  DATA:
    l_lines TYPE i.
  DESCRIBE TABLE gi_index_rows LINES l_lines.
  IF l_lines > 0.
    CALL METHOD grid1->set_selected_rows
      EXPORTING
        it_index_rows = gi_index_rows.
  ENDIF.

* Read index of selected rows
  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = gi_index_rows.
* Check if any row are selected at all. If not
* table gi_index_rows will be empty
  DESCRIBE TABLE gi_index_rows LINES l_lines.
  IF l_lines = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        textline1 = 'You must choose a line'.
    EXIT.
  ENDIF.
* Read indexes of selected rows. In this example only one
* row can be selected as we are using gs_layout-sel_mode = 'B',
* so it is only ncessary to read the first entry in
* table gi_index_rows
  LOOP AT gi_index_rows INTO g_selected_row.
    IF sy-tabix = 1.
      READ TABLE i_employee INDEX g_selected_row-index INTO wa_employee.
    ENDIF.
  ENDLOOP.


  CALL SCREEN 2000 STARTING AT 5 5.
ENDFORM.                    "change_details
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

    SET PF-STATUS 'DISP'.

ENDMODULE.                 " STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_display OUTPUT.
  PERFORM screen_display.
ENDMODULE.                 " SCREEN_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SCREEN_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_display .
  DATA: lv_ok_code     TYPE sy-ucomm,
        ls_info        TYPE piqst_nf_dtinfo,
        lv_conv_needed TYPE flag.

  CALL FUNCTION 'HRIQ_ST_NF_MD_DETAIL_INFO_GET'
    IMPORTING
      info = ls_info.

  SELECT SINGLE *
    INTO hrp9119 FROM hrp9119
      WHERE plvar = ls_info-plvar AND
            otype = ls_info-otype AND
            objid = ls_info-objid AND
            endda = '99991231'.
  IF sy-subrc is INITIAL.
    SELECT SINGLE *
      INTO hrt9119 FROM hrt9119
      WHERE tabnr = hrp9119-tabnr.
  ENDIF.
ENDFORM.                    " SCREEN_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 OUTPUT.
  CASE sy-ucomm .
    WHEN gc_cancel.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CARGAR_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cargar_dynpro .
  CALL SCREEN 2000 STARTING AT 5 5.
ENDFORM.                    " CARGAR_DYNPRO
*&---------------------------------------------------------------------*
*&      Module  PIQ_TAB_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE piq_tab_pbo OUTPUT.
** read infty
*  CALL FUNCTION 'HRIQ_NF_DT_PERIODS_INFTY_INIT'
*    EXPORTING
*      infty           = piq_infty
*      subty           = piq_subty
*    CHANGING
*      sub_prog_period = piq_sub_prog_period
*      sub_dynp_period = piq_sub_dynp_period
*      pnnnn_exp       = <piq_pnnnn>.
*
*
*  CALL FUNCTION 'HRIQ_ST_NF_MD_DETAIL_INFO_GET'
*    IMPORTING
*      info = piq_dtinfo.

ENDMODULE.
