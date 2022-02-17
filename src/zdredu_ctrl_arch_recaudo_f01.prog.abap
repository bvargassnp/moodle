*&---------------------------------------------------------------------*
*&  Include           ZDREDU_CTRL_ARCH_RECAUDO_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  F_OBTIENE_TABLA_ARCHIVOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_ZSUB_P_CTRL_ARCH  text
*----------------------------------------------------------------------*
FORM f_obtiene_tabla_archivos.

  DATA: lv_extnum      TYPE balnrext,
        lr_nom_archivo TYPE RANGE OF zedu_r_ctrl_arch-nom_archivo,
        lr_status      TYPE RANGE OF zedu_r_ctrl_arch-status,
        ls_status      LIKE LINE OF lr_status.

* En caso de recibir status, se arma range
  IF p_status IS NOT INITIAL.
    ls_status(3)   = 'IEQ'.
    ls_status-low  = p_status.
    APPEND ls_status TO lr_status.
  ENDIF.

* Se realiza la selección según los parámetros recibidos
  IF p_extnum IS NOT INITIAL.
    lv_extnum = p_extnum.
    REPLACE ALL OCCURRENCES OF '*' IN lv_extnum WITH '%'.
    SELECT *
      FROM zedu_r_ctrl_arch
      INTO TABLE gt_arch
      WHERE f_proc      IN    s_aldate[]
      AND   nom_archivo LIKE  lv_extnum
      AND   status      IN    lr_status.
  ELSE.
    SELECT *
      FROM zedu_r_ctrl_arch
      INTO TABLE gt_arch
      WHERE f_proc      IN    s_aldate[]
      AND   nom_archivo IN    lr_nom_archivo
      AND   status      IN    lr_status.
  ENDIF.

ENDFORM.                    " F_OBTIENE_TABLA_ARCHIVOS

*&---------------------------------------------------------------------*
*&      Form  F_DIRECTORIOS_LÓGICOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_directorios_logicos .

* Se obtienen los directorios lógicos
  CALL FUNCTION 'Z_EDU_OBTENER_DIR_FIS_X_PRG'
    EXPORTING
      i_programa                = 'ZDRCRE_CARGA_ARCH_REC'
    IMPORTING
      e_dir_in                  = gv_dir_in
      e_dir_out                 = gv_dir_out
      e_dir_err                 = gv_dir_err
      e_dir_bkp_in              = gv_dir_bkp
    EXCEPTIONS
      programa_no_parametrizado = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
*MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_DIRECTORIOS_LÓGICOS

*&---------------------------------------------------------------------*
*&      Form  F_MOSTRAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ALV  text
*----------------------------------------------------------------------*
FORM f_mostrar_alv  USING    p_tabla TYPE any.

  DATA: lo_columns      TYPE REF TO cl_salv_columns_table,
        lo_display      TYPE REF TO cl_salv_display_settings,
        lo_layout       TYPE REF TO cl_salv_layout,
        lo_sorts        TYPE REF TO cl_salv_sorts,
        lo_filters      TYPE REF TO cl_salv_filters,
        lo_functions    TYPE REF TO cl_salv_functions,
        lo_events       TYPE REF TO cl_salv_events_table,
        lo_aggregations TYPE REF TO cl_salv_aggregations,
        wa_lay_key      TYPE salv_s_layout_key,
        lv_tittle       TYPE lvc_title,
        lr_functions    TYPE REF TO cl_salv_functions_list.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = p_tabla.

      lo_columns = go_alv->get_columns( ).
      PERFORM set_columns USING lo_columns.

*     Configura opciones de visualizacion (zebra)
      lo_display = go_alv->get_display_settings( ).
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

*     Activa el status estandar del ALV con todas sus funciones
      lo_functions = go_alv->get_functions( ).
*      lr_functions->set_default( 'X' ).
      lo_functions->set_all( 'X' ).

*      go_alv->set_screen_status(
*      pfstatus      =  'SALV_STANDARD'
*      report        =  sy-repid
*      set_functions = go_alv->c_functions_all ).

*... *** GENERAL Settings ***
*... §6 register to the events of cl_salv_hierseq_table
      DATA: lr_events TYPE REF TO cl_salv_events_table.
      lr_events = go_alv->get_event( ).
      CREATE OBJECT gr_events.
*... §6.1 register to the event USER_COMMAND
      SET HANDLER gr_events->on_user_command FOR lr_events.

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

ENDFORM.                    " F_MOSTRAR_ALV

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
      lr_column ?= pr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( abap_false ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  CLEAR lr_column.
  TRY.
      lr_column ?= pr_columns->get_column( 'STATUS' ).
      lr_column->set_output_length( 10 ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

ENDFORM.                    "set_columns
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_RUTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_DIR_OUT  text
*----------------------------------------------------------------------*
FORM f_obtener_rutas
  CHANGING
    pvc_dir_out   TYPE pathextern.

  "Inicializa los parametros de retorno
  CLEAR:
    pvc_dir_out.

  "Se obtienen las rutas para los archivos

  CALL FUNCTION 'Z_EDU_OBTENER_DIR_FIS_X_PRG'
    EXPORTING
      i_programa                = sy-repid
    IMPORTING
*     E_DIR_IN                  =
      e_dir_out                 = pvc_dir_out
*     E_DIR_ERR                 =
*     E_DIR_BKP_IN              =
    EXCEPTIONS
      programa_no_parametrizado = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_OBTENER_RUTAS
*&---------------------------------------------------------------------*
*&      Form  F_VAL_CREAR_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DIR_OUT  text
*      <--P_LV_FILE  text
*----------------------------------------------------------------------*
FORM f_val_crear_archivo
  USING
    pvi_dir_out   TYPE pathextern
  CHANGING
    pvc_file      TYPE string.

  "Declaraciones
  DATA:
    lv_ruta_c TYPE c LENGTH 200.


  "Inicializa la variable de retorno
  CLEAR pvc_file.

  "Establece el nombre del archivo.
  CONCATENATE pvi_dir_out 'ControlArchivosCre' sy-datum '.txt' "'_' sy-uzeit
      INTO pvc_file.

  "Copia la ruta a una variable tipo c
  lv_ruta_c = pvc_file.

  "Crea un archivo plano para escritura
  OPEN DATASET pvc_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  "Si no puede crear el archivo plano
  IF sy-subrc <> 0.
    "Error al crear el archivo &1 &2 &3 &4.
    MESSAGE e006(zedu_cross)
      WITH lv_ruta_c(50) lv_ruta_c+50(50) lv_ruta_c+100(50) lv_ruta_c+150(50).
    "Termina la ejecucion.
    EXIT.

    "si puede crear el archivo.
  ELSE.
    "Cierra el archivo
    CLOSE DATASET pvc_file.
  ENDIF.
ENDFORM.                    " F_VAL_CREAR_ARCHIVO
*&---------------------------------------------------------------------*
*&      Form  F_GENERAR_ARCHIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FILE  text
*      -->P_GT_TAB_CTRL  text
*----------------------------------------------------------------------*
FORM f_generar_archivo
  USING
    pvi_file     TYPE string
    pti_tab_ctrl TYPE tt_zedu_r_ctrl_arch.

*  "Declaracion de estructuras locales
  TYPES: BEGIN OF str_fc_archivo,
           column  TYPE lvc_fname,
           tab_pos TYPE sy-index,
           var_pos TYPE sy-index,
         END OF str_fc_archivo.
*
*  "Declaracion de Constantes
  CONSTANTS: gc_tab TYPE c       VALUE '|'."cl_abap_char_utilities=>horizontal_tab.

*  "Declaracion de Field-symbol local
  FIELD-SYMBOLS: <lfs_field> TYPE any. " Field-Symbol para campos dinámicos

*  "Declaracion de Variables locales
  DATA: lv_first       TYPE c,            "flag para el primer registro.
        lv_p_campo     TYPE c,            "flag para el primer campo.
        lv_file_c      TYPE c LENGTH 200, "Ruta archivo plano en C
        lv_linesarch   TYPE string,       "Linea archivo plano
        lv_linesarch_t TYPE string,       "Linea archivo plano
        lv_valor       TYPE string,       "Valor del campo
        lv_long_t      TYPE scrtext_l,    "Indica la descripcion de la columna
        lv_tabix       TYPE sy-tabix.     "Indica el registro sobre el cual esta.

*  "Declaracion de tablas y estructuras locales
  DATA: lt_fc_archivo TYPE TABLE OF str_fc_archivo, "Catalogo campos para archivo plano
        lt_dbfieldcat TYPE TABLE OF ltdxdata,       "Catalogo campos variante ALV
        ls_tab_ctrl   TYPE          zedu_r_ctrl_arch,             "Area de trabajo del informe
        ls_fc_archivo TYPE          str_fc_archivo,          "Area trabajo catalogo archivo
        ls_dbfiedlcat TYPE          ltdxdata,                "Area trabajo catalogo ALV
        ls_varkey     TYPE          ltdxkey.                 "Area trabajo variante ALV

*  "Declaracion de objetos locales
  DATA: lo_struct TYPE REF TO  cl_abap_structdescr,
        lt_comp   TYPE         cl_abap_structdescr=>component_table,
        ls_comp   LIKE LINE OF lt_comp.

*  "Si tiene una ruta parametrizada.
  IF NOT pvi_file IS INITIAL.
*    "Crea una copia de la ruta y nombre del archivo
    lv_file_c = pvi_file.
*
*    "Crea el archivo de Reproceso
    OPEN DATASET pvi_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc = 0.
*      "Recupera los componentes de la estructura de datos del ALV
      lo_struct ?= cl_abap_typedescr=>describe_by_name( 'ZEDU_R_CTRL_ARCH' ).
      lt_comp = lo_struct->get_components( ).

*        "Recorre todos los campos de la estructura y los registra
      LOOP AT lt_comp INTO ls_comp.
        ls_fc_archivo-column  = ls_comp-name.
        ls_fc_archivo-tab_pos = sy-tabix.
        ls_fc_archivo-var_pos = sy-tabix.
        APPEND ls_fc_archivo TO lt_fc_archivo.
      ENDLOOP.

      CLEAR lv_first.
*      "Recorre los registros y los va incluyendo en el archivo
      LOOP AT pti_tab_ctrl INTO ls_tab_ctrl.
        CLEAR: lv_linesarch,
               lv_p_campo.
*        "Recorre cada uno de los campos visibles de la estructura
        LOOP AT lt_fc_archivo INTO ls_fc_archivo.
          ASSIGN COMPONENT ls_fc_archivo-tab_pos OF STRUCTURE ls_tab_ctrl TO <lfs_field>.
          IF sy-subrc = 0.
*            "Asigna el valor de la columna
            lv_valor = <lfs_field>.
*            "Elimina los espacios en blanco al inicio y al final
            CONDENSE lv_valor.
*            "Verifica si es el primer campo
            IF lv_p_campo IS INITIAL.
*              "Asigna el valor del primer campo
              lv_linesarch = lv_valor.
            ELSE.
*              "Asigna el valor a la linea del registro
              CONCATENATE lv_linesarch lv_valor
                INTO lv_linesarch
                SEPARATED BY gc_tab.
            ENDIF.
            lv_p_campo = abap_true.
          ENDIF.
        ENDLOOP.
**        IF lv_first IS INITIAL.
**          "Crea el encabezado de las columnas en el archivo.
**          TRANSFER lv_linesarch_t TO pvi_file.
**          lv_first = abap_true.
**        ENDIF.
*        "Mueve el registro al archivo.
        TRANSFER lv_linesarch TO pvi_file.
      ENDLOOP.
*
*      "Cierra el archivo de Reproceso
      CLOSE DATASET pvi_file.
*      "Genera mensaje "Se creó el archivo &1 &2 &3 &4."
      MESSAGE s007(zedu_cross) WITH lv_file_c+0(50)
                                     lv_file_c+50(50)
                                     lv_file_c+100(50)
                                     lv_file_c+150(50).
    ELSE.
*      "Cierra el archivo de Reproceso
      CLOSE DATASET pvi_file.
      "Genera mensaje "Error al crear el archivo &1 &2 &3 &4."
      MESSAGE e006(zedu_cross) WITH lv_file_c+0(50)
                                     lv_file_c+50(50)
                                     lv_file_c+100(50)
                                     lv_file_c+150(50).
    ENDIF.
  ENDIF.
ENDFORM.                    " F_GENERAR_ARCHIVO
