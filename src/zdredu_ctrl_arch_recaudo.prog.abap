

REPORT zdredu_ctrl_arch_recaudo.

*&--------------------------------------------------------------------&*
*&                          INCLUDES                                  &*
*&--------------------------------------------------------------------&*

INCLUDE zdredu_ctrl_arch_recaudo_top.
INCLUDE zdredu_ctrl_arch_recaudo_f01.

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
  IF s_aldate[] IS INITIAL AND
     p_extnum   IS INITIAL.
    MESSAGE e024(zedu_recaudacion).
  ENDIF.

*&--------------------------------------------------------------------&*
*&                         START-OF-SELECTION                         &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.

  IF p_gen EQ 'X'.

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

* Busca entradas en la tabla: zcre_r_ctrl_arch
  PERFORM f_obtiene_tabla_archivos.

* Si la búsqueda no fué exitosa, se le envía un mensaje de información
  IF gt_arch[] IS INITIAL.
    MESSAGE i025(zedu_recaudacion).
    EXIT.
  ENDIF.

  IF p_gen EQ 'X'.
    "Genera un archivo txt con todas las columnas del informe
    PERFORM f_generar_archivo
      USING
        lv_file
        gt_arch.
  ELSE.
* Obtiene directorios lógicos
*    PERFORM f_directorios_logicos.

    PERFORM f_mostrar_alv USING gt_arch.
  ENDIF.
