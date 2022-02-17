FUNCTION z_edu_obtener_dir_fis_x_prg.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PROGRAMA) TYPE  PROGRAMM
*"  EXPORTING
*"     REFERENCE(E_DIR_IN) TYPE  PATHEXTERN
*"     REFERENCE(E_DIR_OUT) TYPE  PATHEXTERN
*"     REFERENCE(E_DIR_ERR) TYPE  PATHEXTERN
*"     REFERENCE(E_DIR_BKP_IN) TYPE  PATHEXTERN
*"  EXCEPTIONS
*"      PROGRAMA_NO_PARAMETRIZADO
*"----------------------------------------------------------------------

  DATA: ls_file_dirlog TYPE zedu_file_dirlog,
        lv_dir_in      TYPE fileintern,
        lv_dir_out     TYPE fileintern,
        lv_dir_err     TYPE fileintern,
        lv_dir_bkp_in  TYPE fileintern.

  SELECT SINGLE *
    FROM zedu_file_dirlog
    INTO ls_file_dirlog
    WHERE programa EQ i_programa.
  IF sy-subrc EQ 0.
*   Obtiene directorio IN
    PERFORM f_obtiene_directorio
      USING     ls_file_dirlog-prefijo
                co_sufijo_in
      CHANGING  e_dir_in.
*   Obtiene directorio OUT
    PERFORM f_obtiene_directorio
      USING     ls_file_dirlog-prefijo
                co_sufijo_out
      CHANGING  e_dir_out.
*   Obtiene directorio ERR
    PERFORM f_obtiene_directorio
      USING     ls_file_dirlog-prefijo
                co_sufijo_err
      CHANGING  e_dir_err.
*   Obtiene directorio BKP
    PERFORM f_obtiene_directorio
      USING     ls_file_dirlog-prefijo
                co_sufijo_bkp_in
      CHANGING  e_dir_bkp_in.
  ELSE.
    RAISE programa_no_parametrizado.
  ENDIF.

ENDFUNCTION.
