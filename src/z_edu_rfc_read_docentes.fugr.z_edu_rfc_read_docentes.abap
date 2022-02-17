FUNCTION z_edu_rfc_read_docentes.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(PERYR) TYPE  PIQPERYR OPTIONAL
*"     VALUE(PERID) TYPE  PIQPERID OPTIONAL
*"     VALUE(P_SOLODOCENTES) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(T_DOCENTES) TYPE  ZTT_READ_DOCENTES
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
*-- LAT: Se deja la función semejante a  Z_EDU_RFC_READ_DOCENTES_V2 (12-11-2019)
  TYPES: BEGIN OF sty_docente,
           pernr TYPE  persno, "numero id docente
           icnum TYPE  psg_idnum,
           ictyp TYPE  ictyp, "tipo contrato/relacion laboral
           vorna TYPE  pad_vorna, "nombre
           name2 TYPE  pad_name2, "2do nombre
           nachn TYPE  pad_nachn, "apellido
           nach2 TYPE  pad_nach2, "2do apellido
         END OF sty_docente,

         BEGIN OF sty_pa0000,
           pernr TYPE persno,
           subty TYPE subty,
           objps TYPE objps,
           sprps TYPE sprps,
           endda TYPE endda,
           begda TYPE begda,
           seqnr TYPE seqnr,
           stat2 TYPE stat2,
         END OF sty_pa0000.

  DATA lt_pa0000 TYPE TABLE OF sty_pa0000.

  DATA msg TYPE string.

  DATA: lt_docentes TYPE TABLE OF sty_docente,
        wa_docentes TYPE sty_docente.
  DATA wa_t_docentes LIKE LINE OF t_docentes.

  DATA i_peryr TYPE piqperyr.
  DATA i_perid TYPE piqperid.
  DATA: gt_timelimits TYPE TABLE OF piqtimelimits,
        wa_timelimits TYPE piqtimelimits.

  DATA fecha_inicial TYPE sy-datum.
  DATA fecha_final TYPE sy-datum.

  DATA: lt_zedu_c_param TYPE TABLE OF zedu_c_param,
        wa_zedu_c_param TYPE zedu_c_param.

  DATA: lc_par_ed1 TYPE zedu_idparam VALUE 'DESDOCE1',   "par. ctto docente1
        lc_par_ed2 TYPE zedu_idparam VALUE 'DESDOCE2',   "par. ctto docente2
        lc_parprog TYPE repid        VALUE 'SAPLV61A'.   "Nombre programa para parámetros
  DATA: lt_persk TYPE TABLE OF persk,
        wa_persk TYPE persk.

  DATA lt_pa0001 TYPE TABLE OF pa0001.
  DATA lt_pa0002 TYPE TABLE OF pa0002.

  CONSTANTS: gco_stat2 TYPE stat2 VALUE '3'.

  i_perid = perid.
  i_peryr = peryr.

  CALL FUNCTION 'HRIQ_ACAD_GET_PERIOD_DATES_NEW'
    EXPORTING
      period                   = i_perid
      year                     = i_peryr
    TABLES
      timelimits               = gt_timelimits
    EXCEPTIONS
      no_data_found            = 1
      year_not_existent        = 2
      period_not_existent      = 3
      objecttype_not_supported = 4
      no_active_plvar          = 5
      no_periods_found         = 6
      customizing_error        = 7
      OTHERS                   = 8.

  IF sy-subrc <> 0 AND ( i_perid IS NOT INITIAL OR i_peryr IS NOT INITIAL ).

    CONCATENATE 'No existe fecha para el año periodo en calendarios ' i_perid '-' i_peryr INTO msg SEPARATED BY space.

    MESSAGE msg TYPE 'E' RAISING no_data.

  ELSEIF i_perid IS INITIAL AND i_peryr IS INITIAL.

    fecha_inicial = cl_hrpiq00const=>c_date_lowdate.
    fecha_final   = cl_hrpiq00const=>c_date_highdate.

  ELSE.

    READ TABLE gt_timelimits INTO wa_timelimits INDEX 1.

    fecha_inicial = wa_timelimits-ca_lbegda.
    fecha_final   = wa_timelimits-ca_lendda.

  ENDIF.

*-- Se buscan los docentes si ha sido seleccionado el parámetro en 'X'
  IF p_solodocentes = 'X'.
* buscar codigo de contratos
    SELECT valor INTO CORRESPONDING FIELDS OF TABLE lt_zedu_c_param
        FROM   zedu_c_param
         WHERE repid = lc_parprog
           AND ( idparam = lc_par_ed1 OR
                     idparam  = lc_par_ed2 ).

    LOOP AT lt_zedu_c_param INTO wa_zedu_c_param.
      wa_persk = wa_zedu_c_param-valor.
      APPEND wa_persk TO lt_persk.
    ENDLOOP.

* buscar datos solo de docentes
    SELECT a~pernr b~vorna b~name2 b~nachn b~nach2 c~ictyp c~icnum
    INTO CORRESPONDING FIELDS OF TABLE lt_docentes
    FROM pa0001 AS a
    INNER JOIN pa0002 AS b
    ON b~pernr = a~pernr
    INNER JOIN pa0185 AS c
    ON c~pernr = a~pernr
    FOR ALL ENTRIES IN lt_persk
    WHERE a~persk = lt_persk-table_line
    AND a~persg = 1
    AND a~begda <= fecha_final
    AND a~endda >= fecha_inicial.

  ELSE.
* buscar todo el personal
    SELECT a~pernr b~vorna b~name2 b~nachn b~nach2 c~ictyp c~icnum
    INTO CORRESPONDING FIELDS OF TABLE lt_docentes
    FROM pa0001 AS a
    INNER JOIN pa0002 AS b
    ON b~pernr = a~pernr
    INNER JOIN pa0185 AS c
    ON c~pernr = a~pernr
    WHERE a~persg = 1
      AND a~begda <= fecha_final
      AND a~endda >= fecha_inicial.
  ENDIF.

  "Obtiene la medida del empleado
  SELECT pernr subty objps sprps endda begda seqnr stat2
    INTO TABLE lt_pa0000
    FROM pa0000
    FOR ALL ENTRIES IN lt_docentes
    WHERE pernr = lt_docentes-pernr
      AND begda <= fecha_final
      AND endda >= fecha_inicial
      AND stat2 = gco_stat2.

  SORT lt_pa0000 BY pernr.

  LOOP AT lt_docentes INTO wa_docentes.
    "Valida que la medida indique que esta vigente
    READ TABLE lt_pa0000 TRANSPORTING NO FIELDS
      WITH KEY pernr = wa_docentes-pernr
      BINARY SEARCH.

    "continua solo si tiene datos
    CHECK sy-subrc = 0.

    "Continua solo si se tiene cedula
    CHECK NOT wa_docentes-icnum IS INITIAL.

    wa_t_docentes-num_id = wa_docentes-icnum.
    wa_t_docentes-tipo_id = wa_docentes-ictyp.
    CONCATENATE wa_docentes-vorna wa_docentes-name2 INTO wa_t_docentes-nombres SEPARATED BY space.
    CONCATENATE wa_docentes-nachn wa_docentes-nach2 INTO wa_t_docentes-apellidos SEPARATED BY space.
    APPEND wa_t_docentes TO t_docentes.
    CLEAR wa_t_docentes.
  ENDLOOP.

  SORT t_docentes.
  DELETE ADJACENT DUPLICATES FROM t_docentes
    COMPARING ALL FIELDS.

ENDFUNCTION.
