*&---------------------------------------------------------------------*
*&  Include           ZDREDU_CTRL_ARCH_RECAUDO_TOP
*&---------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&                               TABLES                               &*
*&--------------------------------------------------------------------&*
TABLES:     zedu_p_ctrl_arch,
            bal_s_dttm.
TYPE-POOLS: slis.

***************************************************************
*                      TIPOS GLOBALES                         *
***************************************************************
TYPES: tt_zedu_r_ctrl_arch TYPE TABLE OF zedu_r_ctrl_arch.

***************************************************************
*                      DATOS GLOBALES                         *
***************************************************************
DATA: gt_arch           TYPE tt_zedu_r_ctrl_arch.

DATA: gv_dir_in  TYPE pathextern,
      gv_dir_out TYPE pathextern,
      gv_dir_err TYPE pathextern,
      gv_dir_bkp TYPE pathextern.

DATA:
  lv_dir_out TYPE pathextern,
  lv_file    TYPE string.
***************************************************************
*                        OBJETOS                              *
***************************************************************
DATA: go_alv          TYPE REF TO cl_salv_table.

CLASS lcl_handle_events DEFINITION DEFERRED.

*... §5 object for handling the events of cl_salv_table
DATA: gr_events TYPE REF TO lcl_handle_events.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* §5.1 define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* §5.2 implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
*    PERFORM f_borrar_registros.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION


***************************************************************
*                 PANTALLA DE SELECCIÓN                       *
***************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
            s_aldate FOR bal_s_dttm-date_from DEFAULT sy-datum.
PARAMETERS: p_extnum TYPE balnrext,
            p_user   TYPE sy-uname NO-DISPLAY,
            p_status TYPE zedu_r_ctrl_arch-status.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_gen TYPE xfeld AS CHECKBOX..
SELECTION-SCREEN END OF BLOCK b2.
