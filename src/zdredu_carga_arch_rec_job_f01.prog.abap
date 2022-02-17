FORM f_leer_ruta.
  DATA: lt_param    TYPE SORTED TABLE OF zedu_c_param WITH UNIQUE KEY primary_key COMPONENTS repid idparam idparampos,
        lr_aliass   TYPE RANGE OF dirname,
        ls_aliass   LIKE LINE OF lr_aliass,
        lt_user_dir TYPE SORTED TABLE OF user_dir WITH UNIQUE KEY primary_key COMPONENTS dirname,
        lv_dir_out  TYPE pathextern,
        lv_dir_err  TYPE pathextern,
        lv_dir_in   TYPE pathextern,
        lv_dir_name TYPE eps2filnam,
        lt_dir_list TYPE STANDARD TABLE OF eps2fili.

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
        lv_dir_err = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIRIN'.
        lv_dir_in = ls_user_dir-dirname && '\'.
      WHEN 'LOTEDIROUT'.
        lv_dir_out = ls_user_dir-dirname && '\'.
    ENDCASE.
  ENDLOOP.

  lv_dir_name = lv_dir_in.
  CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
    EXPORTING
      iv_dir_name            = lv_dir_name
    TABLES
      dir_list               = lt_dir_list
    EXCEPTIONS
      invalid_eps_subdir     = 1
      sapgparam_failed       = 2
      build_directory_failed = 3
      no_authorization       = 4
      read_directory_failed  = 5
      too_many_read_errors   = 6
      empty_directory_list   = 7
      OTHERS                 = 8.


  IF lt_dir_list IS INITIAL.
    MESSAGE ID 'ZDEDU_RECAUDACION' TYPE 'S' NUMBER '036' WITH lv_dir_name.
  ELSE.
    LOOP AT lt_dir_list INTO DATA(ls_dir_list).
      SUBMIT zdredu_carga_arch_rec WITH p_arch = ls_dir_list-name AND RETURN.
      MESSAGE ID 'ZDEDU_RECAUDACION' TYPE 'S' NUMBER '037' WITH ls_dir_list-name.
    ENDLOOP.
  ENDIF.
ENDFORM.
