*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_MARCADO_ASIGNATURA_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_LEER_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_leer_archivo .

  DATA:       lv_tab   TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
              lv_linea TYPE string,
              ls_arch  TYPE gty_archivo.

  OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / 'No se pudo abrir el fichero'.
    EXIT.
  ELSE.

    DO.
      CLEAR: lv_linea.
      READ DATASET p_file INTO lv_linea.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      SPLIT lv_linea AT lv_tab
        INTO ls_arch-bp
* Inicio M6704 - HRESTREPO - 04/07/2018
*             ls_arch-opbel
*             ls_arch-begda
*             ls_arch-endda
* Fin M6704 - HRESTREPO - 04/07/2018
             ls_arch-csobjid
* Inicio M6704 - HRESTREPO - 04/07/2018
             ls_arch-peryr
             ls_arch-perid.
* Fin M6704 - HRESTREPO - 04/07/2018
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_arch-bp
        IMPORTING
          output = ls_arch-bp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_arch-csobjid
        IMPORTING
          output = ls_arch-csobjid.

      APPEND ls_arch TO gt_archivo.
    ENDDO.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_ARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_procesar_arch .
  DATA: ls_archivo TYPE gty_archivo,
        ls_log     TYPE gty_log.

  LOOP AT gt_archivo INTO ls_archivo.
    CLEAR: ls_log.
    MOVE-CORRESPONDING ls_archivo TO ls_log.
    CALL FUNCTION 'Z_EDU_MARCADO_ASIGNATURAS'
      EXPORTING
        i_gpart        = ls_archivo-bp
*       I_OPBEL        =
        i_pago         = p_pago
        i_log_fact     = ''
*       i_begda        = ls_archivo-begda  "M6704 - HRESTREPO - 04/07/2018
*       i_endda        = ls_archivo-endda  "M6704 - HRESTREPO - 04/07/2018
        i_cs_objid     = ls_archivo-csobjid
        i_peryr        = ls_archivo-peryr
        i_perid        = ls_archivo-perid
      EXCEPTIONS
        no_estudiante  = 1
        no_opbel       = 2
        no_hrp1001     = 3
        no_begda_endda = 4
        no_hrpad506    = 5
        no_hrp1724     = 6
        OTHERS         = 7.
    CASE sy-subrc.
      WHEN '1'.
        ls_log-icono = '@0A@'.
        ls_log-resul = 'No existe ST'.
        MESSAGE s000(zdfica) WITH 'Error procesando estudiante' ls_archivo-bp.
      WHEN '3' OR '5' OR '6'.
        ls_log-icono = '@0A@'.
        ls_log-resul = 'Estudiante sin asignaturas'.
        MESSAGE s000(zdfica) WITH 'Error procesando estudiante' ls_archivo-bp.
      WHEN '4'.
        ls_log-icono = '@0A@'.
        ls_log-resul = 'No se ingreso Año y Periodo'.
        MESSAGE s000(zdfica) WITH 'Error procesando estudiante' ls_archivo-bp.
      WHEN '0'.
        ls_log-icono = '@08@'.
        ls_log-resul = 'Se marca BP'.
        MESSAGE s000(zdfica) WITH 'Se marca asignatura de BP' ls_archivo-bp.
    ENDCASE.
    APPEND ls_log TO gt_log.
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
          t_table      = gt_log.

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
      lr_column ?= pr_columns->get_column( 'ICONO' ).
      lr_column->set_medium_text( 'Status').
      lr_column->set_output_length( 10 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'RESUL' ).
      lr_column->set_medium_text( 'Resultado').
      lr_column->set_output_length( 30 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'OPBEL' ).
      lr_column->set_visible( abap_false ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( abap_false ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    " set_columns

*&---------------------------------------------------------------------*
*&      Form  F_LEER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_leer_datos .


  SELECT * FROM cmacbpst INTO TABLE gt_bpst
    FOR ALL ENTRIES IN gt_archivo
    WHERE partner = gt_archivo-bp.

  IF gt_bpst IS NOT INITIAL.
    SELECT * FROM zedu_matricula INTO TABLE gt_rel_st
            FOR ALL ENTRIES IN gt_bpst
             WHERE id_actual = gt_bpst-stobjid.
    SORT gt_rel_st.
    DELETE ADJACENT DUPLICATES FROM gt_rel_st.

    SELECT * FROM hrp1001 INTO TABLE gt_hrp1001
      FOR ALL ENTRIES IN gt_bpst
      WHERE otype = 'ST'
      AND   objid = gt_bpst-stobjid.
    DELETE gt_hrp1001 WHERE subty <> 'A530'
                      OR    sclas <> 'CS'.
*    DELETE gt_hrp1001 WHERE begda < '20170101'.
  ENDIF.

ENDFORM.
