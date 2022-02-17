*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_INFO_EVENTO_EST_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_LEER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_leer_datos.

  SELECT * FROM hrpad506 INTO TABLE gt_hrpad
    WHERE pago = p_est.
  IF sy-subrc EQ 0.
    SELECT objid otype endda adatanr FROM hrp1001 INTO TABLE gt_1001
      FOR ALL ENTRIES IN gt_hrpad
      WHERE adatanr = gt_hrpad-adatanr.
    IF sy-subrc EQ 0.
      DELETE gt_1001 WHERE otype <> 'ST'.
      DELETE gt_1001 WHERE endda <= pa_fecha.
      SORT gt_1001 BY objid.
      DELETE ADJACENT DUPLICATES FROM gt_1001 COMPARING objid.

      SELECT * FROM cmacbpst INTO TABLE gt_bpst
        FOR ALL ENTRIES IN gt_1001
        WHERE stobjid = gt_1001-objid.
      IF sy-subrc EQ 0.
        SELECT partner name_last name_first name_lst2 namemiddle
          FROM but000 INTO TABLE gt_but000
          FOR ALL ENTRIES IN gt_bpst
          WHERE partner = gt_bpst-partner.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_LEER_DATOS

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_procesar_datos .
  FIELD-SYMBOLS: <fs_hrpad>    TYPE hrpad506,
                 <fs_hrp1001>  TYPE gty_1001,
                 <fs_cmacbpst> TYPE cmacbpst,
                 <fs_but000>   TYPE gty_but000.
  DATA: ls_data TYPE gty_data.


*  LOOP AT gt_hrpad ASSIGNING <fs_hrpad>.

  LOOP AT gt_1001 ASSIGNING <fs_hrp1001>.
*    WHERE adatanr = <fs_hrp1001>-adatanr.
*    IF sy-subrc EQ 0.
    READ TABLE gt_bpst ASSIGNING <fs_cmacbpst>
    WITH KEY stobjid = <fs_hrp1001>-objid.
    IF sy-subrc EQ 0.
      READ TABLE gt_but000 ASSIGNING <fs_but000>
      WITH KEY partner = <fs_cmacbpst>-partner.
      IF sy-subrc EQ 0.
        ls_data-partner = <fs_but000>-partner.
        ls_data-matricula = <fs_cmacbpst>-student12.
        ls_data-st = <fs_cmacbpst>-stobjid.
        CONCATENATE <fs_but000>-name_first <fs_but000>-namemiddle
        <fs_but000>-name_last <fs_but000>-name_lst2 INTO ls_data-nombre
        SEPARATED BY space.
        CONDENSE ls_data-nombre.
        TRANSLATE ls_data-nombre TO UPPER CASE.
        APPEND ls_data TO gt_data.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostrar_alv.

  DATA: lo_columns      TYPE REF TO cl_salv_columns_table,
        lo_display      TYPE REF TO cl_salv_display_settings,
        lo_layout       TYPE REF TO cl_salv_layout,
        lo_sorts        TYPE REF TO cl_salv_sorts,
        lo_filters      TYPE REF TO cl_salv_filters,
        lo_functions    TYPE REF TO cl_salv_functions,
        lo_events       TYPE REF TO cl_salv_events_table,
        lo_aggregations TYPE REF TO cl_salv_aggregations,
        wa_lay_key      TYPE salv_s_layout_key,
        lv_tittle       TYPE lvc_title.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_data.

      lo_columns = go_alv->get_columns( ).
      PERFORM set_columns USING lo_columns.

*     Configura opciones de visualizacion (zebra)
      lo_display = go_alv->get_display_settings( ).
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*      PERFORM set_top.

*     Activa el status estandar del ALV con todas sus funciones
      lo_functions = go_alv->get_functions( ).
*      lt_functions = lo_functions->get_functions( ).
      lo_functions->set_all( 'X' ).

*     Configura el ordenamiento del ALV
      lo_sorts = go_alv->get_sorts( ).
*      PERFORM set_sorts   USING lo_sorts.

*     Configura el Layout del ALV, si se tiene alguno por defecto se adopta este
      lo_layout = go_alv->get_layout( ).
      wa_lay_key-report = sy-repid.
      lo_layout->set_key( wa_lay_key ).
      lo_layout->set_save_restriction( 1 ). " 1 -Válido para todos los usuarios y específico de usuario
      lo_layout->set_default( 'X' ).

      go_alv->display( ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " MOSTRAR_ALV

*&---------------------------------------------------------------------*
*&      Form  set_columns
*&---------------------------------------------------------------------*
*&   Rutina que modifica los titulos de algunas columnas del ALV..
*&---------------------------------------------------------------------*
*&     -->PR_COLUMNS  Objeto que contiene las columnas del ALV.
*&---------------------------------------------------------------------*
FORM set_columns  USING    pr_columns   TYPE REF TO cl_salv_columns_table.

  DATA: lr_column   TYPE REF TO cl_salv_column_table,
        ls_ddic_ref TYPE salv_s_ddic_reference.

  pr_columns->set_key_fixation( abap_true ).

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'NOMBRE' ).
      lr_column->set_long_text( 'Noombre' ).
      lr_column->set_medium_text( 'Nombre' ).
      lr_column->set_short_text( 'Nombre' ).
      lr_column->set_output_length( 80 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*  CLEAR lr_column.
*  TRY.
*      lr_column ?= pr_columns->get_column( 'ID_SM_A' ).
*      lr_column->set_long_text( 'Asig Nuevo' ).
*      lr_column->set_medium_text( 'Asig Nuevo' ).
*      lr_column->set_short_text( 'Asig Nue' ).
*      lr_column->set_output_length( 10 ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*  CLEAR lr_column.
*  TRY.
*      lr_column ?= pr_columns->get_column( 'ID_SM_V' ).
*      lr_column->set_long_text( 'Asig Viejo' ).
*      lr_column->set_medium_text( 'Asig Viejo' ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*  CLEAR lr_column.
*  TRY.
*      lr_column ?= pr_columns->get_column( 'ID_ST_A' ).
*      lr_column->set_long_text( 'ID ST (NUEVO)' ).
*      lr_column->set_medium_text( 'ID ST (NUEVO)' ).
*      lr_column->set_short_text( 'ST Nuevo' ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*  CLEAR lr_column.
*  TRY.
*      lr_column ?= pr_columns->get_column( 'ID_ST_V' ).
*      lr_column->set_long_text( 'ST (VIEJO)' ).
*      lr_column->set_medium_text( 'ST (VIEJO)' ).
*      lr_column->set_short_text( 'ST Viejo' ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.


*  CLEAR lr_column.
*  TRY.
*      lr_column ?= pr_columns->get_column( 'MANDT' ).
*      lr_column->set_visible( abap_false ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

ENDFORM.                    " set_columns
