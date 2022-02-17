*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_EXTR_ENVIO_PAGO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_LEER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_leer_datos .

  CONSTANTS: lc_date TYPE sy-datum VALUE '99991231'.

  SELECT * FROM zedu_int_moodle INTO TABLE gt_moodle
    WHERE estado EQ pa_est.

  "Se validan si hay registros sin procesar
  IF sy-subrc EQ 0.
    SELECT partner type idnumber valid_date_to FROM but0id INTO TABLE gt_but0id
      FOR ALL ENTRIES IN gt_moodle
      WHERE partner = gt_moodle-gpart.

    DELETE gt_but0id WHERE valid_date_to <> lc_date
     OR type NP 'FS*'.
*    DELETE gt_but0id WHERE type NP 'FS*'.

    SELECT partner name_last name_lst2 name_first namemiddle FROM but000
      INTO TABLE gt_but000
      FOR ALL ENTRIES IN gt_moodle
      WHERE partner = gt_moodle-gpart.

    SELECT partner addrnumber FROM but021_fs
       INTO TABLE gt_but021
      FOR ALL ENTRIES IN gt_moodle
      WHERE partner = gt_moodle-gpart.

    IF sy-subrc EQ 0.
      SELECT addrnumber smtp_addr  flgdefault	home_flag  FROM adr6
        INTO TABLE gt_adr6
        FOR ALL ENTRIES IN gt_but021
        WHERE addrnumber = gt_but021-addrnumber.
    ENDIF.

    SELECT * FROM hrp1000 INTO TABLE gt_hrp
      WHERE plvar = '01'
        AND otype = 'E'.
  ENDIF.

  SORT gt_but000 BY partner.
  SORT gt_but0id BY partner.
  SORT gt_but021 BY partner.
  SORT gt_adr6   BY addrnumber.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_procesar_reg .

  TYPES: BEGIN OF lty_data,
           evtid        TYPE evtid,
           opbel        TYPE opbel_kk,
           idnumber     TYPE bu_id_number,
           nombre(80)   TYPE c,
           apellido(80) TYPE c,
           email        TYPE adr6-smtp_addr,
           desc         TYPE hrp1000-stext,
           tipo_pago    TYPE zedu_int_moodle-tipo_pago,
           betrw        TYPE zedu_int_moodle-betrw,
           budat        TYPE zedu_int_moodle-budat,
           xblnr        TYPE zedu_int_moodle-xblnr,
         END OF lty_data.

  DATA: lv_ident      TYPE string,
        lv_valor(20)  TYPE c,
        lv_monto(20)  TYPE n,
        ls_data       TYPE lty_data,
        ls_str_moodle TYPE zstedu_paymentreportprod_in,
        ls_payparam   TYPE zstedu_paymentparams,
        lt_return     TYPE bapiret2_t.

  FIELD-SYMBOLS: <fs_moodle> TYPE zedu_int_moodle,
                 <fs_but000> TYPE gty_but000,
                 <fs_but0id> TYPE gty_but0id,
                 <fs_but021> TYPE gty_but021,
                 <fs_adr6>   TYPE gty_adr6,
                 <fs_hrp>    TYPE hrp1000.

  LOOP AT gt_moodle ASSIGNING <fs_moodle>.

    MOVE-CORRESPONDING <fs_moodle> TO ls_data.

    READ TABLE gt_but000 ASSIGNING <fs_but000>
    WITH KEY partner = <fs_moodle>-gpart
    BINARY SEARCH .
    IF sy-subrc EQ 0.
      CONCATENATE <fs_but000>-name_last <fs_but000>-name_lst2
      INTO ls_data-apellido SEPARATED BY space.
      CONDENSE ls_data-apellido.
      ls_str_moodle-lastname = ls_data-apellido.
      CONCATENATE <fs_but000>-name_first <fs_but000>-namemiddle
      INTO ls_data-nombre SEPARATED BY space.
      CONDENSE ls_data-nombre.
      ls_str_moodle-firstname = ls_data-nombre.
    ENDIF.

    READ TABLE gt_but0id ASSIGNING <fs_but0id>
    WITH KEY partner = <fs_moodle>-gpart.
    IF sy-subrc EQ 0.
      ls_data-idnumber = <fs_but0id>-idnumber.
      ls_str_moodle-identification = <fs_but0id>-idnumber.
    ENDIF.

    READ TABLE gt_but021 ASSIGNING <fs_but021>
    WITH KEY partner = <fs_moodle>-gpart
    BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE gt_adr6 ASSIGNING <fs_adr6>
      WITH KEY addrnumber = <fs_but021>-addrnumber.
      IF sy-subrc EQ 0.
        ls_data-email = <fs_adr6>-smtp_addr.
        ls_str_moodle-email = <fs_adr6>-smtp_addr.
      ENDIF.
    ENDIF.

    READ TABLE gt_hrp ASSIGNING <fs_hrp>
    WITH KEY objid = <fs_moodle>-evtid.
    IF sy-subrc EQ 0.
      ls_data-desc = <fs_hrp>-stext.
      ls_str_moodle-description = <fs_hrp>-stext.
*      ls_payparam-name  = 'descripcion'.
*      ls_payparam-value = <fs_hrp>-stext.
*      APPEND ls_payparam TO ls_str_moodle-paymentparams.
    ENDIF.

    ls_str_moodle-remoteid = <fs_moodle>-opbel.
    ls_str_moodle-code     = <fs_moodle>-evtid.

    ls_payparam-name  = 'tipodepago'.
    ls_payparam-value = <fs_moodle>-tipo_pago.
    APPEND ls_payparam TO ls_str_moodle-paymentparams.

    WRITE <fs_moodle>-betrw TO lv_valor CURRENCY <fs_moodle>-waers.
    MOVE lv_valor TO lv_monto.
*  lv_monto = lv_monto * 100. "Las dos posiciones de los decimales
    REPLACE  ALL OCCURRENCES OF '.' IN  lv_valor WITH ''.
    CONDENSE lv_valor.

    ls_payparam-name  = 'valordelpago'.
    ls_payparam-value = lv_valor.
    APPEND ls_payparam TO ls_str_moodle-paymentparams.

    ls_payparam-name  = 'fechadepago'.
    ls_payparam-value = <fs_moodle>-budat.
    APPEND ls_payparam TO ls_str_moodle-paymentparams.

    ls_payparam-name  = 'referenciadepago'.
    ls_payparam-value = <fs_moodle>-xblnr.
    APPEND ls_payparam TO ls_str_moodle-paymentparams.

    CALL FUNCTION 'ZEDU_PAGOS_MOODLE'
      EXPORTING
        i_estructura_moodle = ls_str_moodle
      IMPORTING
        e_identificador     = lv_ident
        et_mensajes         = lt_return.

    "Si no hay error se cambia el estado a procesado.
   LOOP AT lt_return TRANSPORTING NO FIELDS
    WHERE type <> 'E'.
     <fs_moodle>-estado = 'PR'.
   ENDLOOP.


  ENDLOOP.

  MODIFY zedu_int_moodle FROM TABLE gt_moodle.


ENDFORM.
