*&---------------------------------------------------------------------*
*& Report  ZDRCRE_TABLERO_CTRL_LOTE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zdredu_tablero_ctrl_lote.

*&--------------------------------------------------------------------&*
*&                          INCLUDES                                  &*
*&--------------------------------------------------------------------&*

INCLUDE zdedu_tablero_ctrl_lote_top.
INCLUDE zdedu_tablero_ctrl_lote_f01.

INITIALIZATION.

*  CALL FUNCTION 'ZFM_AUTHORITY_CHECK'
*    EXPORTING
*      i_object     = 'ZCA_CRE'
*      i_programm   = sy-cprog
*    EXCEPTIONS
*      autorizacion = 1
*      OTHERS       = 2.
*  IF sy-subrc <> 0.
*    MESSAGE e099(zca_message_cross).
*  ENDIF.

*&--------------------------------------------------------------------&*
*&                         AT SELECTION-SCREEN                        &*
*&--------------------------------------------------------------------&*
AT SELECTION-SCREEN.
* Valida que se ingrese la fecha o el nombre del archivo
  IF so_date[] IS INITIAL AND
     so_lote   IS INITIAL.
    MESSAGE e026(zedu_recaudacion).
  ENDIF.

*&--------------------------------------------------------------------&*
*&                         START-OF-SELECTION                         &*
*&--------------------------------------------------------------------&*

START-OF-SELECTION.

  IF p_gen EQ 'X'..

    "Obtiene las rutas para los archivos
    PERFORM f_obtener_rutas
      CHANGING
        lv_dir_out.

    "Valida que se pueda crear el archivo
    PERFORM f_val_crear_archivo
      USING
        lv_dir_out
      CHANGING
        lv_file.
  ENDIF.

  PERFORM f_obtener_datos.


  IF gt_tab_ctrl[] IS NOT INITIAL.
    IF p_gen EQ 'X'.
      "Genera un archivo txt con todas las columnas del informe
      PERFORM f_generar_archivo
        USING
          lv_file
          gt_tab_ctrl.
    ELSE.
      PERFORM f_actualizar_datos.
      PERFORM f_mostrar_alv USING gt_tab_ctrl.
    ENDIF.
  ELSE.
    MESSAGE e025(zedu_recaudacion).
  ENDIF.
