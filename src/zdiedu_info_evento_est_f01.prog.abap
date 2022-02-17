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

  FIELD-SYMBOLS: <fs_hrp1001> TYPE gty_hrp1001.

  SELECT * FROM zedu_matricula INTO TABLE gt_rel_sm
    WHERE id_anterior IN so_asig.

  IF sy-subrc EQ 0.
    SELECT otype objid sclas sobid  begda FROM hrp1001
      INTO TABLE gt_sm_d
      FOR ALL ENTRIES IN gt_rel_sm
      WHERE otype = 'SM'
        AND objid = gt_rel_sm-id_actual.
    DELETE gt_sm_d WHERE sclas <> 'D'.
    LOOP AT gt_sm_d ASSIGNING <fs_hrp1001>.
      <fs_hrp1001>-objid2 = <fs_hrp1001>-sobid.
    ENDLOOP.

    IF gt_sm_d[] IS NOT INITIAL.
      SELECT otype objid sclas sobid begda FROM hrp1001
        INTO TABLE gt_d_e
        FOR ALL ENTRIES IN gt_sm_d
        WHERE otype = 'D'
          AND objid = gt_sm_d-objid2.
      DELETE gt_d_e WHERE sclas <> 'E'.
*                    OR    begda < '20170101'.
      LOOP AT gt_d_e ASSIGNING <fs_hrp1001>.
        <fs_hrp1001>-objid2 = <fs_hrp1001>-sobid.
      ENDLOOP.

      IF gt_d_e[] IS NOT INITIAL.
        SELECT otype objid sclas sobid begda FROM hrp1001
          INTO TABLE gt_e_st
          FOR ALL ENTRIES IN gt_d_e
          WHERE otype = 'E'
            AND objid = gt_d_e-objid2.
        DELETE gt_e_st WHERE sclas <> 'ST'
                       OR    begda < '20170101'.
        LOOP AT gt_e_st ASSIGNING <fs_hrp1001>.
          <fs_hrp1001>-objid2 = <fs_hrp1001>-sobid.
        ENDLOOP.
        IF gt_e_st[] IS NOT INITIAL.
          SELECT * FROM zedu_matricula INTO TABLE gt_rel_st
            FOR ALL ENTRIES IN gt_e_st
             WHERE id_actual = gt_e_st-objid2.
          SORT gt_rel_st.
          DELETE ADJACENT DUPLICATES FROM gt_rel_st.
        ENDIF.
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
  FIELD-SYMBOLS: <fs_sm_d>   TYPE gty_hrp1001,
                 <fs_d_e>    TYPE gty_hrp1001,
                 <fs_e_st>   TYPE gty_hrp1001,
                 <fs_rel_st> TYPE zedu_matricula,
                 <fs_rel_sm> TYPE zedu_matricula.
  DATA: ls_data TYPE gty_data.

  LOOP AT gt_e_st ASSIGNING <fs_e_st>.
    ls_data-id_evento = <fs_e_st>-objid.
    ls_data-id_st_a = <fs_e_st>-objid2.
    READ TABLE gt_rel_st ASSIGNING <fs_rel_st>
    WITH KEY id_actual = <fs_e_st>-objid2.
    IF sy-subrc EQ 0.
      ls_data-id_st_v = <fs_rel_st>-id_anterior.
    ENDIF.

    READ TABLE gt_d_e ASSIGNING <fs_d_e>
    WITH KEY objid2 = <fs_e_st>-objid.
    IF sy-subrc EQ 0.
      READ TABLE gt_sm_d ASSIGNING <fs_sm_d>
      WITH KEY objid2 = <fs_d_e>-objid.
      IF sy-subrc EQ 0.
        ls_data-id_sm_a = <fs_sm_d>-objid.
        READ TABLE gt_rel_sm ASSIGNING <fs_rel_sm>
        WITH KEY id_actual = <fs_sm_d>-objid.
        IF sy-subrc EQ 0.
          ls_data-id_sm_v = <fs_rel_sm>-id_anterior.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND ls_data TO gt_data.
  ENDLOOP.

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
      lr_column ?= pr_columns->get_column( 'ID_EVENTO' ).
      lr_column->set_long_text( 'Evento' ).
      lr_column->set_medium_text( 'Evento' ).
      lr_column->set_short_text( 'Evento' ).
*      lr_column->set_output_length( 10 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'ID_SM_A' ).
      lr_column->set_long_text( 'Asig Nuevo' ).
      lr_column->set_medium_text( 'Asig Nuevo' ).
      lr_column->set_short_text( 'Asig Nue' ).
      lr_column->set_output_length( 10 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'ID_SM_V' ).
      lr_column->set_long_text( 'Asig Viejo' ).
      lr_column->set_medium_text( 'Asig Viejo' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'ID_ST_A' ).
      lr_column->set_long_text( 'ID ST (NUEVO)' ).
      lr_column->set_medium_text( 'ID ST (NUEVO)' ).
      lr_column->set_short_text( 'ST Nuevo' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'ID_ST_V' ).
      lr_column->set_long_text( 'ST (VIEJO)' ).
      lr_column->set_medium_text( 'ST (VIEJO)' ).
      lr_column->set_short_text( 'ST Viejo' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.


  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( abap_false ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " set_columns
