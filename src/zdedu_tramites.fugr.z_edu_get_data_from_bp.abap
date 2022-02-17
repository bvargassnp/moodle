FUNCTION z_edu_get_data_from_bp .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_BP) TYPE  BU_PARTNER
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& HISTORIAL DE MODIFICACIONES                                         *
*&---------------------------------------------------------------------*
*& Solicitud   : 27288                                                 *
*& Versión     : 2.0.0                                                 *
*& Fecha       : 17/11/2021                                            *
*& Autor       : Flag Soluciones S.A.S. - Yumey Giraldo Cifuentes      *
*& Descripción : 01: Se adiciona consulta de responsabilidad fiscal
*&               actividad económica, grupo destino                    *
*&---------------------------------------------------------------------*

  DATA: lt_telefondata     TYPE TABLE OF bapiadtel,
        lt_faxdata         TYPE TABLE OF bapiadfax,
        lt_maildata        TYPE TABLE OF bapiadsmtp,
        lt_bankdetaildata  TYPE TABLE OF bapibus1006_bankdetails,
        ls_datos_bancarios TYPE zedu_s_datos_bancarios,
        ls_telefondata     TYPE bapiadtel,
        ls_faxdata         TYPE bapiadfax,
        ls_maildata        TYPE bapiadsmtp,
        ls_centraldata     TYPE bapibus1006_central,
        ls_persona         TYPE bapibus1006_central_person,
        ls_empresa         TYPE bapibus1006_central_organ,
        ls_address         TYPE bapibus1006_address,
        ls_identification	 TYPE bapibus1006_identification,
        ls_zedu_datos_pers TYPE zedu_datos_pers,
        ls_return          TYPE bapiret2,
        lv_message         TYPE string,
        lv_partnercategory TYPE bapibus1006_head-partn_cat,
        lv_partner         TYPE bu_partner,
        lv_taxtype         TYPE bu_id_category,
        lv_taxnum          TYPE bu_id_number,
        lv_attributes      TYPE bapi_str_bupa_fs_treasury,        "AD_2.0.0_01
        lt_return          TYPE STANDARD TABLE OF bapiret2.       "AD_2.0.0_01
  DATA lt_t005h TYPE STANDARD TABLE OF t005h. "AD_2.0.0_01
  DATA lt_t005u TYPE STANDARD TABLE OF t005u. "AD_2.0.0_01
  DATA lwa_t005u LIKE LINE OF lt_t005u. "AD_2.0.0_01

  FIELD-SYMBOLS: <fs_bankdetaildata> TYPE bapibus1006_bankdetails.


*** Verifico si el BP tiene una modificación de datos sensibles en curso.

  SELECT SINGLE *
    FROM zedu_datos_pers
    INTO ls_zedu_datos_pers
    WHERE partner = iv_bp.

  IF sy-subrc IS INITIAL.
*   No se puede modificar el BP, modificación pendiente de aprobación.
    MESSAGE e025(zedu_wd_message) INTO lv_message.
    MOVE sy-msgid TO ls_return-id.
    MOVE sy-msgno TO ls_return-number.
    MOVE sy-msgty TO ls_return-type.

    APPEND ls_return TO et_return.
    EXIT.
  ENDIF.

*** Busco los datos del BP.
  CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
    EXPORTING
      businesspartner         = iv_bp
    IMPORTING
      centraldata             = ls_centraldata
      centraldataperson       = ls_persona
      centraldataorganization = ls_empresa
    TABLES
      return                  = et_return.

  CHECK et_return IS INITIAL.

  SELECT SINGLE type idnumber institute entry_date country region
    FROM but0id
    INTO (gs_datos_personales-type,
          gs_datos_personales-idnumber,
          gs_datos_personales-id_institute,
          gs_datos_personales-id_entry_date,
          gs_datos_personales-id_country,
          gs_datos_personales-id_region)
    WHERE partner = iv_bp
      AND valid_date_from <= sy-datum
      AND valid_date_to   >= sy-datum.

  gs_datos_personales-type_old     = gs_datos_personales-type.
  gs_datos_personales-idnumber_old = gs_datos_personales-idnumber.

  SELECT SINGLE partner_guid
   FROM but000
   INTO gs_datos_personales-guid
   WHERE partner = iv_bp.

  IF ls_empresa IS NOT INITIAL.

    gs_datos_personales-partner   = iv_bp.
    gs_datos_personales-title     = ls_centraldata-title_key.

*    gs_datos_personales-idnumber  = ls_centraldata-searchterm1.
    gs_datos_personales-name_org1 = ls_empresa-name1.
    gs_datos_personales-name_org2 = ls_empresa-name2.
    gs_datos_personales-bu_langu  = ls_centraldata-partnerlanguage.
    gs_datos_personales-langu_corr  = ls_centraldata-partnerlanguage.

  ELSEIF ls_persona IS NOT INITIAL.
    gs_datos_personales-partner    = iv_bp.
    gs_datos_personales-title      = ls_centraldata-title_key.
    gs_datos_personales-name_last  = ls_persona-lastname.
    gs_datos_personales-name_first = ls_persona-firstname.
    gs_datos_personales-name_lst2  = ls_persona-secondname.
    gs_datos_personales-name_last2 = ls_persona-birthname.
    gs_datos_personales-namemiddle = ls_persona-middlename.
    gs_datos_personales-langu_corr = ls_persona-correspondlanguage.
    gs_datos_personales-marst      = ls_persona-maritalstatus.
    gs_datos_personales-birthdt    = ls_persona-birthdate.

*    gs_datos_personales-birthpl    = ls_persona-birthplace.                                          "CO_2.0.0_01
    SPLIT ls_persona-birthplace AT ',' INTO gs_datos_personales-birthpl gs_datos_personales-birthdep. "MO_2.0.0_01
*{AD_2.0.0_01 Se determina el país de nacimiento a partir de la ciudad y el departamento
    IF gs_datos_personales-birthpl IS NOT INITIAL.
      "Municipio
      SELECT * FROM t005h
        INTO TABLE lt_t005h
        WHERE bezei = gs_datos_personales-birthpl.
      IF sy-subrc = 0.
        "Departamento
        SELECT *
         FROM t005u
           INTO TABLE lt_t005u
          FOR ALL ENTRIES IN lt_t005h
             WHERE spras EQ sy-langu
             AND bezei EQ gs_datos_personales-birthdep
             AND bland EQ lt_t005h-regio
             AND land1 EQ lt_t005h-land1.
        IF sy-subrc = 0.
          READ TABLE lt_t005u INTO lwa_t005u INDEX 1.
          IF sy-subrc = 0.
            gs_datos_personales-id_country_nac = lwa_t005u-land1.
            gs_datos_personales-id_region_nac = lwa_t005u-bland.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    gs_datos_personales-jobgr      = ls_persona-occupation.
    gs_datos_personales-sexo       = ls_persona-sex.
    gs_datos_personales-bu_langu   = ls_centraldata-partnerlanguage.
    gs_datos_direccion-xblck = ls_centraldata-centralblock. " AD_2.0.0_01
  ENDIF.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_GETDETAIL'
    EXPORTING
      businesspartner = iv_bp
    IMPORTING
      addressdata     = ls_address
    TABLES
      bapiadtel       = lt_telefondata
      bapiadfax       = lt_faxdata
      bapiadsmtp      = lt_maildata
      return          = et_return.

*** Dirección
  gs_datos_direccion-street     =   ls_address-street.
  gs_datos_direccion-city1      =   ls_address-city.
  gs_datos_direccion-post_code1 =   ls_address-postl_cod1.
  gs_datos_direccion-region     =   ls_address-region.
  gs_datos_direccion-country    =   ls_address-country.
*{AD_2.0.0_01
  gs_datos_direccion-clase      = ls_address-postl_cod2. " Actividad económica
  gs_datos_direccion-codigo     = ls_address-po_box.     " Responsasbilidad fiscal
*}AD_2.0.0_01

  CLEAR ls_faxdata.
  READ TABLE lt_faxdata INTO ls_faxdata INDEX 1.
  gs_datos_direccion-fax_number = ls_faxdata-fax.
  gs_datos_direccion-fax_extens = ls_faxdata-extension.

  CLEAR ls_telefondata.
  LOOP AT lt_telefondata INTO ls_telefondata.
    CASE ls_telefondata-r_3_user.
      WHEN '1'. " fijo
        gs_datos_direccion-tel_number = ls_telefondata-telephone.
        gs_datos_direccion-tel_extens = ls_telefondata-extension.
      WHEN '3'. " celular
        gs_datos_direccion-tel_movil = ls_telefondata-telephone.
    ENDCASE.
  ENDLOOP.

  CLEAR ls_maildata.
  LOOP AT lt_maildata INTO ls_maildata.
    CASE sy-tabix.
      WHEN 1.
        gs_datos_direccion-smtp_addr = ls_maildata-e_mail.
      WHEN 2.
        gs_datos_direccion-smtp_addr_2 = ls_maildata-e_mail.
      WHEN 3.
        gs_datos_direccion-smtp_addr_3 = ls_maildata-e_mail.
    ENDCASE.
  ENDLOOP.


  CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
    EXPORTING
      businesspartner = iv_bp
    TABLES
      bankdetails     = lt_bankdetaildata.
*      return          = et_return.

  IF lt_bankdetaildata IS NOT INITIAL.

    LOOP AT lt_bankdetaildata ASSIGNING <fs_bankdetaildata>.

      ls_datos_bancarios-bkvid = <fs_bankdetaildata>-bankdetailid.
      ls_datos_bancarios-banks = <fs_bankdetaildata>-bank_ctry.
      ls_datos_bancarios-bankl = <fs_bankdetaildata>-bank_key.
      ls_datos_bancarios-bankn = <fs_bankdetaildata>-bank_acct.
      ls_datos_bancarios-bkont = <fs_bankdetaildata>-ctrl_key.
      ls_datos_bancarios-bkref = <fs_bankdetaildata>-bank_ref.
      ls_datos_bancarios-koinh = <fs_bankdetaildata>-accountholder.
      APPEND ls_datos_bancarios TO gt_datos_bancarios.
      CLEAR ls_datos_bancarios.

    ENDLOOP.

  ENDIF.
*{AD_2.0.0_01
  CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_GET'
    EXPORTING
      businesspartner = iv_bp
    IMPORTING
      fsattributes    = lv_attributes
    TABLES
      return          = lt_return.

  DATA lv_group_d_t TYPE tp13t-group_d_t.

  IF lv_attributes-target_group IS NOT INITIAL.
    SELECT SINGLE group_d_t FROM tp13t
      INTO lv_group_d_t
      WHERE langu = sy-langu
      AND group_d = lv_attributes-target_group.
  ENDIF.

  gs_datos_direccion-group_d = lv_group_d_t. "lv_attributes-target_group.
*}AD_2.0.0_01

  gs_datos_personales_ini  = gs_datos_personales.
  gs_datos_direccion_ini   = gs_datos_direccion.

ENDFUNCTION.
