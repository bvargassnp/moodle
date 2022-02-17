*----------------------------------------------------------------------*
***INCLUDE ZDIEDU_CARGA_ARCH_REC_F01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_LEER_ARCHIVO
*&---------------------------------------------------------------------*
FORM f_leer_archivo .
  DATA: lv_line        TYPE string,
        lv_dir_out     TYPE pathextern,
        lv_ruta_fisica TYPE pathextern,
        lt_param       TYPE SORTED TABLE OF zedu_c_param WITH UNIQUE KEY primary_key COMPONENTS repid idparam idparampos,
        lr_aliass      TYPE RANGE OF dirname,
        ls_aliass      LIKE LINE OF lr_aliass,
        lt_user_dir    TYPE SORTED TABLE OF user_dir WITH UNIQUE KEY primary_key COMPONENTS dirname.

  PERFORM f_inicio_programa.

  SELECT mandt repid idparam idparampos valor descripcion
    INTO TABLE lt_param
    FROM zedu_c_param
    WHERE repid = '' AND
          ( idparam = 'LOTEDIRERR' OR
            idparam = 'LOTEDIRIN'  OR
            idparam = 'LOTEDIROUT' ).

  ls_aliass-sign = 'I'.
  ls_aliass-option = 'EQ'.
  LOOP AT lt_param INTO DATA(ls_param).
    ls_aliass-low = ls_param-valor.
    APPEND ls_aliass TO lr_aliass.
  ENDLOOP.

  SELECT dirname aliass svrname sp_name sp_cs
    INTO TABLE lt_user_dir
    FROM user_dir
    FOR ALL ENTRIES IN lr_aliass
    WHERE aliass = lr_aliass-low.

  LOOP AT lt_user_dir INTO DATA(ls_user_dir).
    READ TABLE lt_param INTO ls_param WITH KEY valor = ls_user_dir-aliass.
    CASE ls_param-idparam.
      WHEN 'LOTEDIRERR'.
        gv_dir_err = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIRIN'.
        gv_dir_in = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIROUT'.
        gv_dir_out = ls_user_dir-dirname && '\'.
    ENDCASE.
  ENDLOOP.


  CLEAR: gv_error_param.
*  CALL FUNCTION 'Z_EDU_OBTENER_DIR_FIS_X_PRG'
*    EXPORTING
*      i_programa                = 'ZDREDU_CARGA_ARCH_REC'
*    IMPORTING
*      e_dir_in                  = gv_dir_in
*      e_dir_out                 = gv_dir_out
*      e_dir_err                 = gv_dir_err
*      e_dir_bkp_in              = gv_dir_bkp
*    EXCEPTIONS
*      programa_no_parametrizado = 1
*      OTHERS                    = 2.

  IF sy-subrc <> 0.
    gv_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                            '033'
                            space
                            space
                            space
                            space
                   CHANGING gt_mensajes.
  ELSE.

*Archivo Entrada
    CONCATENATE gv_dir_in p_arch INTO gv_archivo_in.
*    WAIT UP TO 10 SECONDS.
    OPEN DATASET gv_archivo_in FOR INPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc NE 0.
*   Error al abrir el archivo.
      gv_error_param = 'X'.
      PERFORM agregar_mensaje   USING gc_msg_type_e
                              '001'
                              space
                              space
                              space
                              space
                     CHANGING gt_mensajes.
      PERFORM agregar_mensaje   USING gc_msg_type_e
                          '002'
                          gv_dir_in
                          sy-subrc"space
                          space
                          space
                 CHANGING gt_mensajes.
      PERFORM agregar_mensaje   USING gc_msg_type_e
                      '003'
                      p_arch
                      space
                      space
                      space
             CHANGING gt_mensajes.

    ELSE.

      DO.
        CLEAR: lv_line.

        READ DATASET gv_archivo_in INTO lv_line.
        IF sy-subrc EQ 0.
          APPEND lv_line TO gt_archivo_str.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.
      IF gt_archivo_str[] IS INITIAL.
        PERFORM agregar_mensaje   USING gc_msg_type_e
                                  '005'
                                  p_arch
                                  space
                                  space
                                  space
                         CHANGING gt_mensajes.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_LEER_ARCHIVO

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_DATOS
*&---------------------------------------------------------------------*
FORM f_procesar_datos .
  DATA:
**   Estructuras de parseo con campos string correspondientes a cada linea del archivo
    ls_recau_c_h1 TYPE zedu_recaudo_c_h1, " Estructura de parseo registro H1
    ls_recau_c_h2 LIKE zedu_recaudo_c_h2, " Estructura de parseo registro H2
    ls_recau_c_d  LIKE zedu_recaudo_c_d,  " Estructura de parseo registro D
    ls_recau_c_f1 LIKE zedu_recaudo_c_f1, " Estructura de parseo registro F1
    ls_recau_c_f2 LIKE zedu_recaudo_c_f2, " Estructura de parseo registro F2
**   Estructuras para validaciones con campos con definiciones correspondientes
    ls_recau_h1   TYPE zedu_recaudo_h1,   " Registro de encabezado de archivo
    ls_recau_h2   LIKE zedu_recaudo_h2,   " Registro de encabezado de lote
    ls_recau_d    LIKE zedu_recaudo_d,    " Registro de detalle de planillas
    ls_recau_f1   LIKE zedu_recaudo_f1,   " Registro de control de archivo
    ls_recau_f2   LIKE zedu_recaudo_f2.   " Registro de control de lotes

  DATA:
    lt_recau_d TYPE STANDARD TABLE OF zedu_recaudo_d,
    ls_totales TYPE ty_totales.

  DATA:
**   Id de archivo registrado en SAP
    lv_id_archivo     TYPE zedu_id_archivo,
**   Indicador de error de sintaxis en el registro
    lv_error_regis    TYPE xfeld,
**   Indicador de error de sintaxis en el el archivo
    lv_error_sintaxis TYPE xfeld,
**   Indicador de error de validaciones de negocio en el el archivo
    lv_error_valneg   TYPE xfeld,
**   Indica Tipo Linea Anterior
    lv_cl_reg(2)      TYPE c,
    lv_x.
**Se verifica que el archivo no haya sido cargado en el sistema aun,
*  y se obtiene un ID para el archivo
  PERFORM obtener_id_ctrl    USING p_arch
                                   p_correc
                          CHANGING gv_id_archivo
                                   gv_archivo.
  IF gv_id_archivo IS NOT INITIAL.
    PERFORM consultar_param CHANGING lv_error_sintaxis.

*Archivo Entrada
    CONCATENATE gv_dir_out p_arch INTO gv_archivo_out.

    LOOP AT gt_archivo_str INTO gs_archivo_str.
      gv_nro_registro = sy-tabix.

      IF gs_archivo_str IS INITIAL AND lv_cl_reg EQ '09'.
        EXIT.
      ELSEIF gs_archivo_str IS INITIAL AND lv_cl_reg <> '09'.
        lv_error_sintaxis = 'X'.
        PERFORM agregar_mensaje    USING gc_msg_type_e
                                 '041'
                                 p_arch
                                 space
                                 space
                                 space
                        CHANGING gt_mensajes.
        EXIT.
      ENDIF.

      CLEAR lv_error_regis.

      CASE gs_archivo_str+0(2).
**    Registro cabecera de archivo
        WHEN '01'.
**      Divido el registro string en los diferentes campos de la linea.
          MOVE gs_archivo_str TO ls_recau_c_h1.
          gv_tipo_registro = 1.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = ls_recau_c_h1-num_cta
            IMPORTING
              output = ls_recau_c_h1-num_cta.

          IF ls_recau_c_h1-num_cta EQ 0.
            CLEAR ls_recau_c_h1-num_cta.
          ENDIF.

          PERFORM validar_campos USING 'ZEDU_RECAUDO_C_H1'
                                       'ZEDU_RECAUDO_H1'
                                       ls_recau_c_h1
                                       ls_recau_h1
                                 CHANGING      lv_error_sintaxis.
          IF lv_error_regis IS INITIAL.
            PERFORM mapear_cab_arch.
          ENDIF.

**    Registro cabecera de lote
        WHEN '05'.
*      Divido el registro string en los diferentes campos de la linea.
          MOVE gs_archivo_str TO ls_recau_c_h2.
          gv_tipo_registro = 5.

          PERFORM validar_campos USING 'ZEDU_RECAUDO_C_H2'
                                       'ZEDU_RECAUDO_H2'
                                       ls_recau_c_h2
                                       ls_recau_h2
                                 CHANGING      lv_error_sintaxis.
**    Registro detalle lote
        WHEN '06'.
*      Divido el registro string en los diferentes campos de la linea.
          MOVE gs_archivo_str TO ls_recau_c_d.
          gv_tipo_registro = 5.

          PERFORM validar_campos USING 'ZEDU_RECAUDO_C_D'
                                       'ZEDU_RECAUDO_D'
                                       ls_recau_c_d
                                       ls_recau_d
                                 CHANGING  lv_error_sintaxis.

          APPEND ls_recau_d TO lt_recau_d.
**    Registro control de lote
        WHEN '08'.
*      Divido el registro string en los diferentes campos de la linea.
          MOVE gs_archivo_str TO ls_recau_c_f2.
          gv_tipo_registro = 5.

          PERFORM validar_campos USING 'ZEDU_RECAUDO_C_F2'
                                       'ZEDU_RECAUDO_F2'
                                       ls_recau_c_f2
                                       ls_recau_f2
                           CHANGING    lv_error_sintaxis.
**    Se valida líneas y totales de los lotes.
          PERFORM validar_lin_y_tot_ach_lote    USING lt_recau_d[] " Detalle
                                                      ls_recau_h2  " Cabecera de lotes
                                                      ls_recau_f2  " Control de lotes
                                             CHANGING ls_totales
                                                      lv_error_sintaxis
                                                      lv_error_valneg.

          IF lv_error_regis IS INITIAL.
            PERFORM mapear_cab_det_lote USING ls_recau_h1
                                          ls_recau_h2
                                          ls_recau_f2
                                          lt_recau_d[]
                                        CHANGING
                                          lv_error_sintaxis.
          ENDIF.

          CLEAR lt_recau_d[].

**    Registro control de archivo
        WHEN '09'.
*      Divido el registro string en los diferentes campos de la linea.
          MOVE gs_archivo_str TO ls_recau_c_f1.
          gv_tipo_registro = 5.

          PERFORM validar_campos USING 'ZEDU_RECAUDO_C_F1'
                                       'ZEDU_RECAUDO_F1'
                                       ls_recau_c_f1
                                       ls_recau_f1
                                 CHANGING      lv_error_sintaxis.

        WHEN OTHERS.
          lv_error_regis = 'X'.
          gv_tipo_registro = gs_archivo_str+0(1).
          "&1-&2. El tipo de registro debe ser 1,5,6,8 o 9. Valor &3.
          PERFORM agregar_mensaje    USING gc_msg_type_e
                                           '017'
                                           gv_nro_registro
                                           'TIP_REG'
                                           gv_tipo_registro
                                           space
                                  CHANGING gt_mensajes.
      ENDCASE.
      lv_cl_reg = gs_archivo_str+0(2).

    ENDLOOP.

**    Se valida líneas y totales de los diferentes tipos de registros.
    PERFORM validar_lin_y_tot_ach    USING ls_recau_h1  " Cabecera de archivo
                                           ls_recau_f1  " Control de archivo
                                           ls_totales
                                  CHANGING lv_error_sintaxis
                                           lv_error_valneg.

    PERFORM grabar_ctrl_arch    USING gv_archivo
                                      lv_error_sintaxis
                                      lv_error_valneg.

    IF lv_error_sintaxis IS INITIAL AND lv_error_valneg IS INITIAL.
      CLOSE DATASET gv_archivo_out.

      DELETE DATASET gv_archivo_in.

      "llama al programa de creación de lotes
      SUBMIT zdredu_carga_lotes_rec WITH p_arch = p_arch
      AND RETURN.
    ELSE.
      PERFORM copiar_archivo_error.
      CLOSE DATASET gv_archivo_out.
      DELETE DATASET gv_archivo_out.
    ENDIF.
  ELSE.
    PERFORM copiar_archivo_error.
  ENDIF.
ENDFORM.                    " F_PROCESAR_DATOS

*&---------------------------------------------------------------------*
*&      Form  OBTENER_ID_CTRL
*&---------------------------------------------------------------------*
FORM obtener_id_ctrl  USING    pv_arch
                               p_correccion
                      CHANGING pv_id_archivo TYPE zedu_id_archivo
                               pv_archivo.

  DATA: lv_id_archivo TYPE zedu_id_archivo,
        lv_status     TYPE zedu_status_arch_pila,
        lr_f_proc     TYPE RANGE OF zedu_r_ctrl_arch-f_proc,
        lv_x.

  pv_archivo = pv_arch.

  TRANSLATE pv_archivo TO UPPER CASE.

  SELECT SINGLE id_archivo status
    FROM zedu_r_ctrl_arch
    INTO  (lv_id_archivo, lv_status)
    WHERE "f_proc       = sy-datum AND
          nom_archivo  = pv_archivo.

  IF sy-subrc = 0.
    IF lv_status <> 'ES' AND p_correccion IS INITIAL.
      PERFORM agregar_mensaje   USING gc_msg_type_e
                '006'
                p_arch
                space
                space
                space
       CHANGING gt_mensajes.
    ELSE.
      pv_id_archivo = lv_id_archivo.
    ENDIF.
  ELSE.
    PERFORM obtener_id_archivo CHANGING pv_id_archivo.
  ENDIF.

ENDFORM.                    " OBTENER_ID_CTRL

*&---------------------------------------------------------------------*
*&      Form  OBTENER_ID_ARCHIVO
*&---------------------------------------------------------------------*
FORM obtener_id_archivo  CHANGING  p_id_archivo TYPE zedu_id_archivo.

  DATA: lv_rc   TYPE inri-returncode,                       "#EC NEEDED
        lv_cont TYPE i.

  DO.
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = 'ZEDUIDRECA'
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        OTHERS           = 4.
    IF sy-subrc NE 0 AND lv_cont > 10.
      PERFORM agregar_mensaje   USING gc_msg_type_e
                  '007'
                  space
                  space
                  space
                  space
         CHANGING gt_mensajes.
      EXIT.
    ELSEIF sy-subrc NE 0 AND lv_cont < 20.
      WAIT UP TO 1 SECONDS.
    ELSEIF sy-subrc EQ 0.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZEDUIDRECA'
          toyear                  = sy-datum+0(4)
        IMPORTING
          number                  = p_id_archivo
          returncode              = lv_rc
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
        PERFORM agregar_mensaje   USING gc_msg_type_e
                    '008'
                    space
                    space
                    space
                    space
           CHANGING gt_mensajes.
      ENDIF.

      CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
        EXPORTING
          object = 'ZEDUIDRECA'
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc <> 0.
        PERFORM agregar_mensaje   USING gc_msg_type_e
                    '009'
                    space
                    space
                    space
                    space
           CHANGING gt_mensajes.
      ENDIF.

      COMMIT WORK.
      EXIT.

    ENDIF.
    lv_cont = lv_cont + 1.
  ENDDO.

ENDFORM.                    " OBTENER_ID_ARCHIVO

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
FORM validar_campos  USING VALUE(lv_nom_estruct_char)
                           VALUE(lv_nom_estruct_form)
                           ls_datos_char TYPE any
                           ls_datos_form TYPE any
                     CHANGING  p_error_regis.


  DATA:
    lp_struct_char      TYPE REF TO data,
    lp_struct_form      TYPE REF TO data,
    lt_dd03l            TYPE TABLE OF dd03l,
    ls_dd03l            TYPE dd03l,
    lt_dfies            TYPE STANDARD TABLE OF dfies WITH HEADER LINE,
    lt_dd07v            TYPE STANDARD TABLE OF dd07v WITH HEADER LINE,
    lv_error_campo      TYPE xfeld,
    lv_error_regis      TYPE xfeld,
    lv_nombre_campo(50) TYPE c.
*       lt_error       TYPE STANDARD TABLE OF fimsg.

  RANGES:
         lr_fixvalues FOR lt_dd07v-domvalue_l.

  FIELD-SYMBOLS:
    <et_char>       TYPE any,             "Estructura con formatos de campos correspondientes
    <et_format>     TYPE any,             "Estructura con formatos de campos correspondientes
    <ls_field_text> TYPE any,
    <ls_field_form> TYPE any.

  DATA: oref      TYPE REF TO cx_root,
        text(200) TYPE c.

*  Creacion dinamica del estructura de formato texto
  CREATE DATA lp_struct_char TYPE (lv_nom_estruct_char).
  ASSIGN lp_struct_char->* TO <et_char>.

  CREATE DATA lp_struct_form TYPE (lv_nom_estruct_form).
  ASSIGN lp_struct_form->* TO <et_format>.

** Doy formato a la estructura de salida recibida.
  ls_datos_form = <et_format>.
  <et_char> = ls_datos_char.

  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = lv_nom_estruct_form
    TABLES
      dfies_tab = lt_dfies
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
**    error
  ENDIF.

  LOOP AT lt_dfies.

    CLEAR lv_error_campo.

    gv_campo = lt_dfies-fieldname.

    ASSIGN COMPONENT lt_dfies-fieldname OF STRUCTURE <et_char> TO <ls_field_text>.

    CHECK <ls_field_text> IS NOT INITIAL.

    "Convertir a Mayusculas los valores de campos de tipos permitidos.
    IF lt_dfies-inttype = 'C' OR
       lt_dfies-inttype = 'N' OR
       lt_dfies-inttype = 'D' OR
       lt_dfies-inttype = 'T'.
      TRANSLATE <ls_field_text> TO UPPER CASE.
    ENDIF.

** Aplican validaciones generales de formato.
    IF lt_dfies-inttype = 'N' AND <ls_field_text>  IS NOT INITIAL.
      IF  <ls_field_text> CO '0123456789. '.

      ELSE.
        lv_error_campo = lv_error_regis = 'X'.
        "&1-&2. El valor debe contener sólo digitos númericos. Valor: &3.
        PERFORM agregar_mensaje   USING gc_msg_type_e
                                        '011'
                                        gv_nro_registro
                                        gv_campo
                                        <ls_field_text>
                                        space
                               CHANGING gt_mensajes.
      ENDIF.
    ENDIF.

** Aplican validaciones de valores fijos en el diccionario de datos
    IF lv_error_campo IS INITIAL.
** Si para dicho campo existen valores fijos de dominio se obtienen dichos valores
      IF lt_dfies-valexi = 'X' AND <ls_field_text>  IS NOT INITIAL.

** Se obtiene información del campo de la estructura, ejemplo el dominio
        CLEAR ls_dd03l.

        SELECT *
          INTO TABLE lt_dd03l
          FROM dd03l
          WHERE tabname = lv_nom_estruct_form
            AND fieldname = lt_dfies-fieldname
            AND as4local = 'A'.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_dd03l INTO ls_dd03l INDEX 1.
        ENDIF.

        IF ls_dd03l-domname IS NOT INITIAL.
          CALL FUNCTION 'DDUT_DOMVALUES_GET'
            EXPORTING
              name          = ls_dd03l-domname
            TABLES
              dd07v_tab     = lt_dd07v
            EXCEPTIONS
              illegal_input = 1
              OTHERS        = 2.

** Se arma un rango con los valores fijos e intervalores de tenerlos
          CLEAR lr_fixvalues.
          REFRESH lr_fixvalues.

          LOOP AT lt_dd07v.
            lr_fixvalues-sign = 'I'.
            IF lt_dd07v-domvalue_l IS NOT INITIAL AND
               lt_dd07v-domvalue_h IS INITIAL.
              lr_fixvalues-option = 'EQ'.
              lr_fixvalues-low = lt_dd07v-domvalue_l.
              APPEND lr_fixvalues.
            ELSEIF lt_dd07v-domvalue_l IS NOT INITIAL AND
                   lt_dd07v-domvalue_h IS NOT INITIAL.
              lr_fixvalues-option = 'BT'.
              lr_fixvalues-low = lt_dd07v-domvalue_l.
              lr_fixvalues-high = lt_dd07v-domvalue_h.
              APPEND lr_fixvalues.
            ENDIF.
          ENDLOOP.

          IF lr_fixvalues[] IS NOT INITIAL.
            IF NOT <ls_field_text> IN lr_fixvalues.
**            Error
              lv_error_campo = lv_error_regis = 'X'.
              "&1-&2. El valor no es válido para el domino definido. Valor: &3.
              PERFORM agregar_mensaje   USING gc_msg_type_e
                                              '010'
                                              gv_nro_registro
                                              gv_campo
                                              <ls_field_text>
                                              space
                                     CHANGING gt_mensajes.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
    IF lv_error_campo IS INITIAL.

      CONCATENATE 'ls_datos_form-'  lt_dfies-fieldname INTO lv_nombre_campo.
      TRY.
          ASSIGN (lv_nombre_campo) TO <ls_field_form>.
          MOVE <ls_field_text> TO <ls_field_form>.
        CATCH cx_root INTO oref.

          text = oref->get_text( ).

          PERFORM agregar_mensaje   USING gc_msg_type_e
                                          '018'
                                          lv_nom_estruct_char
                                          lt_dfies-fieldname
                                          text+0(100)
                                          text+100(100)
                                    CHANGING gt_mensajes.
          lv_error_campo = lv_error_regis = 'X'.

      ENDTRY.
    ENDIF.

  ENDLOOP.

  p_error_regis = lv_error_regis.

ENDFORM.                    " VALIDAR_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  AGREGAR_MENSAJE
*&---------------------------------------------------------------------*
FORM agregar_mensaje     USING p_msgty   TYPE  bapi_mtype
                               p_msgno   TYPE  symsgno
                               p_msgv1
                               p_msgv2
                               p_msgv3
                               p_msgv4
                      CHANGING t_msg     TYPE  ty_mensajes.

  DATA: ls_msg   TYPE bapiret2,
        lv_msgv1 TYPE symsgv,
        lv_msgv2 TYPE symsgv,
        lv_msgv3 TYPE symsgv,
        lv_msgv4 TYPE symsgv.

  lv_msgv1 = p_msgv1.
  lv_msgv2 = p_msgv2.
  lv_msgv3 = p_msgv3.
  lv_msgv4 = p_msgv4.

  gv_log->add_message(
  EXPORTING i_c_msgty = p_msgty
    i_c_msgid = gc_msg_id
    i_n_msgno = p_msgno
    i_c_msgv1 = lv_msgv1
    i_c_msgv2 = lv_msgv2
    i_c_msgv3 = lv_msgv3
    i_c_msgv4 = lv_msgv4
  CHANGING c_ti_return = t_msg ).

ENDFORM.                    " AGREGAR_MENSAJE

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_LIN_Y_TOT_ACH_LOTE
*&---------------------------------------------------------------------*
FORM validar_lin_y_tot_ach_lote    USING pt_recau_d   TYPE  ty_recau_d
                                         ps_recau_h2  TYPE  zedu_recaudo_h2
                                         ps_recau_f2  TYPE  zedu_recaudo_f2
                                CHANGING ps_totales   TYPE  ty_totales
                                         p_error      TYPE  xfeld
                                         p_error_val  TYPE  xfeld.


  DATA:  lv_total_reg_lote    TYPE zedu_recaudo_f2-tot_reg,
         lv_total_recaud_lote TYPE zedu_recaudo_f2-valor.
  DATA:  ls_recau_d           TYPE zedu_recaudo_d.
  IF ps_recau_h2 IS INITIAL.
**    Error
    p_error = 'X'.
    "El archivo no tiene registro cabecera tipo 05.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '012'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

  IF pt_recau_d[] IS INITIAL.
**    Error
    p_error = 'X'.
    "El archivo no tiene lineas de detalle tipo 6.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '013'
                                     ps_recau_h2-num_lote
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ELSE.
    LOOP AT pt_recau_d INTO ls_recau_d.
      ADD 1                         TO lv_total_reg_lote.
      ADD ls_recau_d-valor          TO lv_total_recaud_lote.
    ENDLOOP.
  ENDIF.

  IF ps_recau_f2 IS INITIAL.
**    Error
    p_error = 'X'.
    "El archivo no tiene linea de total de control de lotes tipo 8.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '014'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ELSE.

    IF ps_recau_f2-tot_reg NE lv_total_reg_lote.
**        Error
      p_error_val = 'X'.
      "El total Nro de registros(8) no coincide con la suma de lineas detalle(6)
      PERFORM agregar_mensaje    USING gc_msg_type_e
                                       '015'
                                       gv_nro_registro
                                       ps_recau_h2-num_lote
                                       space
                                       space
                              CHANGING gt_mensajes.
    ENDIF.

    IF ps_recau_f2-valor NE lv_total_recaud_lote.
**        Error
      p_error_val = 'X'.
      "El total Valor recaudado(8) no coincide con la suma de lineas detalle(6)
      PERFORM agregar_mensaje    USING gc_msg_type_e
                                       '016'
                                       gv_nro_registro
                                       ps_recau_h2-num_lote
                                       space
                                       space
                              CHANGING gt_mensajes.
    ENDIF.

    ADD ps_recau_f2-tot_reg         TO ps_totales-tot_registro.
    ADD ps_recau_f2-valor           TO ps_totales-tot_recaudado.
    ADD 1                           TO ps_totales-tot_lotes.

  ENDIF.

ENDFORM.                    " VALIDAR_LIN_Y_TOT_ACH_LOTE

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_LIN_Y_TOT_ACH
*&---------------------------------------------------------------------*
FORM validar_lin_y_tot_ach  USING    ps_recau_h1  TYPE zedu_recaudo_h1
                                     ps_recau_f1  TYPE zedu_recaudo_f1
                                     ps_totales   TYPE ty_totales
                            CHANGING p_error      TYPE xfeld
                                     p_error_val  TYPE xfeld.


  IF ps_recau_h1 IS INITIAL.
**    Error
    p_error = 'X'.
    "El archivo no tiene registro cabecera tipo 1.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '019'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

  IF ps_recau_f1 IS INITIAL.
**    Error
    p_error = 'X'.
    "El archivo no tiene linea de total de aportes tipo 09.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '020'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

  IF ps_recau_f1-tot_reg NE ps_totales-tot_registro.
**        Error
    p_error_val = 'X'.
    "El total Nro de registros(9) no coincide con la suma de reg. de lote(8)
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '021'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

  IF ps_recau_f1-valor NE ps_totales-tot_recaudado.
**        Error
    p_error_val = 'X'.
    "El total Valor recaudado(9) no coincide con la suma de recaud. de lote(8)
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '022'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

ENDFORM.                    " VALIDAR_LIN_Y_TOT_ACH

*&---------------------------------------------------------------------*
*&      Form  GRABAR_CTRL_ARCH
*&---------------------------------------------------------------------*
FORM grabar_ctrl_arch     USING p_archivo      TYPE zedu_nom_archivo
                                p_error_sint   TYPE xfeld
                                p_error_neg    TYPE xfeld.

  DATA: ls_ctrl_arch TYPE zedu_r_ctrl_arch.

  ls_ctrl_arch-id_archivo     = gv_id_archivo.
  ls_ctrl_arch-nom_archivo    = p_archivo.
  ls_ctrl_arch-f_proc         = sy-datum.
  ls_ctrl_arch-h_proc         = sy-uzeit.

  IF p_error_sint = 'X'.
    ls_ctrl_arch-status = gc_status_es.
  ELSEIF p_error_neg = 'X'.
    ls_ctrl_arch-status = gc_status_ev.
  ELSE.
    ls_ctrl_arch-status = gc_status_ok.
  ENDIF.

  MODIFY zedu_r_ctrl_arch FROM ls_ctrl_arch.

  IF sy-subrc <> 0.
    "Error al grabar los datos en la Base de datos.
    PERFORM agregar_mensaje    USING gc_msg_type_e
                                     '018'
                                     space
                                     space
                                     space
                                     space
                            CHANGING gt_mensajes.
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " GRABAR_CTRL_ARCH

*&---------------------------------------------------------------------*
*&      Form  MAPEAR_CAB_ARCH
*&---------------------------------------------------------------------*
FORM mapear_cab_arch.
  DATA: ls_bfkkzgr00  TYPE bfkkzgr00.

  OPEN DATASET gv_archivo_out  FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  ls_bfkkzgr00-stype = '0'.
  ls_bfkkzgr00-applk = 'P'.

  TRANSFER ls_bfkkzgr00 TO gv_archivo_out LENGTH '8'.

ENDFORM.                    " MAPEAR_CAB_ARCH

*&---------------------------------------------------------------------*
*&      Form  MAPEAR_CAB_DET_LOTE
*&---------------------------------------------------------------------*
FORM mapear_cab_det_lote  USING p_recau_h1 TYPE zedu_recaudo_h1
                            p_recau_h2     TYPE zedu_recaudo_h2
                            p_recau_f2     TYPE zedu_recaudo_f2
                            pt_recaudo     TYPE ty_recau_d
                          CHANGING
                            p_error_param  TYPE c.
  DATA: ls_bfkkzk  TYPE bfkkzk, "estructura cabecera lote
        ls_bfkkzp  TYPE zbfkkzp, "estructura detalle lote
        ls_recaudo TYPE zedu_recaudo_d,
        lv_valor   TYPE bapicurr_d,
        lv_valor2  TYPE p DECIMALS 2,
        lv_xblnr   TYPE xblnr_kk.
******        lv_ind     TYPE c. "indicador si es un cobro por oficina externa de cobranza

  READ TABLE gt_cuentas TRANSPORTING NO FIELDS
  WITH KEY cod_ent_rec = p_recau_h1-cod_ent
           num_cuenta  = p_recau_h1-num_cta.
  IF sy-subrc <> 0.
    p_error_param = 'X'.
    PERFORM agregar_mensaje    USING gc_msg_type_i
                               '032'
                               p_recau_h1-cod_ent
                               'ZEDU_C_CTA_COMP'
                               space
                               space
                      CHANGING gt_mensajes.
  ELSE. "Si la parametrizacion está correcta se crea la clave de reconciliacion para el lote
    CONCATENATE 'E' p_recau_h1-fecha_rec+2(2) gv_id_archivo INTO gv_fikey_lote.
  ENDIF.

  IF p_error_param = ''.

    ls_bfkkzk-stype = '1'.
    ls_bfkkzk-tbnam = 'BFKKZK'.
    ls_bfkkzk-keyz1 = gv_fikey_lote.
******    CONCATENATE 'H' p_recau_h1-fecha_rec+2(2) p_recau_h1-fecha_rec+4(2)
******    p_recau_h1-fecha_rec+6(2) '0000' INTO gv_fikey_honor. "Clave de reconciliacion Honorarios
    CONCATENATE p_recau_h1-cod_ent p_recau_h2-num_lote
    INTO ls_bfkkzk-keyz2 SEPARATED BY '-'.
    ls_bfkkzk-fikey = ls_bfkkzk-keyz1.
    ls_bfkkzk-budat = p_recau_h1-fecha_rec.
    ls_bfkkzk-bldat = p_recau_h1-fecha_rec.
    ls_bfkkzk-valut = p_recau_h1-fecha_rec.
    ls_bfkkzk-infof = p_recau_h1-num_cta.
    CLEAR: lv_valor, lv_valor2.
    lv_valor2 =  p_recau_f2-valor / 100. "quitarle los decimales

    ls_bfkkzk-ktsus = lv_valor2.

    ls_bfkkzk-ksump = p_recau_f2-tot_reg.
    ls_bfkkzk-ktsuh = gv_id_archivo.
    TRANSFER ls_bfkkzk TO gv_archivo_out LENGTH '263'. "Envia cabecera lote

    LOOP AT pt_recaudo INTO ls_recaudo.
      CLEAR: lv_valor, lv_valor2."""""""", lv_ind.
      ls_bfkkzp-stype   = '2'.
*      ls_bfkkzp-stype   = '1'.
      ls_bfkkzp-tbnam   = 'BFKKZP'.
      ls_bfkkzp-selt1   = 'X'.
*     Si el nombre del archivo comienza por 'CASOBAN' es Educacion Continua
*      IF p_arch(7) = 'CASOBAN'.
*        ls_bfkkzp-selw1   = ls_recaudo-ref_ppal(21). "Tipo (1) y Nro ID (20) BP
*        ls_bfkkzp-selw2   = '05'. "Indicador tipo factura
**     En caso contrario
*      ELSE.
*        ls_bfkkzp-selw1   = ls_recaudo-ref_ppal+38(10). "Recaudo
*        "Si el tipo de factura es de educacion
*        IF ls_recaudo-ref_ppal+24(2) = '00'.
*          "Asigna el valor de la posicion modificada
*          ls_bfkkzp-selw2   = ls_recaudo-ref_ppal+28(2). "Indicador tipo factura
*
*          "Si el tipo de factura NO es de educacion
*        ELSE.
*          "Asigna el valor de la posicion indicada
*          ls_bfkkzp-selw2   = ls_recaudo-ref_ppal+24(2). "Indicador tipo factura
*        ENDIF.
*      ENDIF.
      CLEAR lv_xblnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_recaudo-ref_ppal
        IMPORTING
          output = lv_xblnr.

      "Verificar referencia y eliminar dígito de chequeo
      PERFORM f_validar_xblnr CHANGING lv_xblnr.

      ls_bfkkzp-selw1 = lv_xblnr.
      ls_bfkkzp-xeiph = 'X'.
      ls_bfkkzk-prctr = '0002080060'.
      CONCATENATE ls_recaudo-medio_pago ls_recaudo-reservado
        INTO ls_bfkkzp-txtrz SEPARATED BY '-'.
      "ls_bfkkzp-xakon = abap_true.
      lv_valor =  ls_recaudo-valor / 100. "quitarle los decimales
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        EXPORTING
          currency             = 'COP'
          amount_external      = lv_valor
          max_number_of_digits = 18
        IMPORTING
          amount_internal      = lv_valor2.
      ls_bfkkzp-betrz = lv_valor2.
      TRANSFER ls_bfkkzp TO gv_archivo_out LENGTH '736'. "Envia detalle lote
    ENDLOOP.
  ENDIF.

ENDFORM.                    " MAPEAR_CAB_DET_LOTE

*&---------------------------------------------------------------------*
*&      Form  COPIAR_ARCHIVO_ERROR
*&---------------------------------------------------------------------*
FORM copiar_archivo_error .

  CONCATENATE gv_dir_err p_arch INTO gv_archivo_err.

  DELETE DATASET gv_archivo_err.

  OPEN DATASET gv_archivo_err  FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  LOOP AT gt_archivo_str INTO gs_archivo_str.
    TRANSFER gs_archivo_str TO gv_archivo_err.
  ENDLOOP.

  CLOSE DATASET gv_archivo_err.
  DELETE DATASET gv_archivo_in.

  PERFORM agregar_mensaje    USING gc_msg_type_e
                                 '023'
                                 gv_dir_err
                                 space
                                 space
                                 space
                        CHANGING gt_mensajes.

ENDFORM.                    " COPIAR_ARCHIVO_ERROR

*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_PARAM
*&---------------------------------------------------------------------*
FORM consultar_param CHANGING p_error_param TYPE c.

  CLEAR: p_error_param.

*Consulta Cuentas Compensación por Entidad Recaudadora
  SELECT *
    FROM zedu_c_cta_comp
    INTO TABLE gt_cuentas
    WHERE cod_ent_rec IS NOT NULL.

*Se consulta la tabla de parámetros Cross
  SELECT * FROM zedu_c_param INTO TABLE gt_param
    WHERE repid = ''
    OR    repid = sy-repid.

  "Clase Doc
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = 'BLART'.
  IF sy-subrc EQ 0.
    gv_blart = gs_param-valor.
  ELSE.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        'BLART'
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

  "Moneda
  READ TABLE gt_param INTO gs_param
  WITH KEY repid   = sy-repid   "repid   = sy-repid
           idparam = 'WAERS'.
  IF sy-subrc EQ 0.
    gv_waers = gs_param-valor.
  ELSE.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        'WAERS'
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

  "Sociedad
  READ TABLE gt_param INTO gs_param
  WITH KEY idparam = 'ZEDU_SOCIE'.
  IF sy-subrc EQ 0.
    gv_bukrs = gs_param-valor.
  ELSE.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        'ZEDU_SOCIE'
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

* Motivo Compensacion
  READ TABLE gt_param TRANSPORTING NO FIELDS
  WITH KEY idparam = gc_augrd.
  IF sy-subrc <> 0.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        gc_augrd
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

*Seleccion 1
  READ TABLE gt_param TRANSPORTING NO FIELDS
  WITH KEY idparam = gc_selt1.
  IF sy-subrc <> 0.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        gc_selt1
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

*Seleccion 2
  READ TABLE gt_param TRANSPORTING NO FIELDS
  WITH KEY idparam = gc_selt2.
  IF sy-subrc <> 0.
    p_error_param = 'X'.
    PERFORM agregar_mensaje   USING gc_msg_type_e
                        '032'
                        gc_selt2
                        'ZEDU_C_PARAM'
                        space
                        space
               CHANGING gt_mensajes.
  ENDIF.

ENDFORM.                    " CONSULTAR_PARAM

*&---------------------------------------------------------------------*
*&      Form  F_CREAR_LOG
*&---------------------------------------------------------------------*
FORM f_crear_log .

  CREATE OBJECT gv_log
    EXPORTING
      i_s_object    = gc_object
      i_s_subobject = gc_subobject
      i_es_syst     = sy.

ENDFORM.                    " F_CREAR_LOG

*&---------------------------------------------------------------------*
*&      Form  F_GRABAR_LOG
*&---------------------------------------------------------------------*
FORM f_grabar_log.

  "Se guardan los mensajes que hay en la tabla y luego se borran para que
  "no inserte mensajes duplicados.
  gv_log->save( i_ti_log_tab = gt_mensajes ).
  CLEAR gt_mensajes[].

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

ENDFORM.                    " F_GRABAR_LOG

*&---------------------------------------------------------------------*
*&      Form  F_INICIO_PROGRAMA
*&---------------------------------------------------------------------*
FORM f_inicio_programa .

  PERFORM agregar_mensaje   USING gc_msg_type_i
                          '042'
                          p_arch
                          space
                          space
                          space
                 CHANGING gt_mensajes.

  PERFORM f_grabar_log.

ENDFORM.                    " F_INICIO_PROGRAMA
FORM f_validar_xblnr CHANGING pc_xblnr TYPE xblnr_kk.
  "Verificar Referencia y eliminar digito de verificación si es necesario.
  DATA: lv_xblnr TYPE xblnr_kk.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = pc_xblnr
    IMPORTING
      output = lv_xblnr.

  "Buscar si existe referencia con el código enviado por Asobancaria
  SELECT SINGLE opbel
   INTO @DATA(lv_opbel)
   FROM dfkkko
   WHERE xblnr = @lv_xblnr.

  IF sy-subrc <> 0. "Si la referencia no existe, intentar buscar quitando últmo digito de chequeo
    SHIFT lv_xblnr RIGHT BY 1 PLACES. "Quitar dígito de chequeo

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_xblnr
      IMPORTING
        output = lv_xblnr.

    SELECT SINGLE opbel "Buscar si existe referencia sin el último dígito.
     INTO @lv_opbel
     FROM dfkkko
     WHERE xblnr = @lv_xblnr.

    IF sy-subrc <> 0. "Si no existe referencia, dejar referencia original.
      lv_xblnr = pc_xblnr.
    ENDIF.
  ENDIF.
  pc_xblnr = lv_xblnr.
ENDFORM.
