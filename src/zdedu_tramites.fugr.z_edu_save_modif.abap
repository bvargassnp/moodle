FUNCTION z_edu_save_modif.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_NO_WF) TYPE  FLAG OPTIONAL
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------

*** Grabación de trámite de alta de personas o empresas.

  TABLES: zedu_usr_grupor.

  DATA: lt_return           TYPE TABLE OF bapiret2,
        lt_telefondata      TYPE TABLE OF bapiadtel,
        lt_telefondata_x    TYPE TABLE OF bapiadtelx,
        lt_telefondata_i    TYPE TABLE OF bapiadtel,
        lt_telefondata_i_x  TYPE TABLE OF bapiadtelx,
        lt_faxdata          TYPE TABLE OF bapiadfax,
        lt_faxdata_x        TYPE TABLE OF bapiadfaxx,
        lt_maildata         TYPE TABLE OF bapiadsmtp,
        lt_maildata_x       TYPE TABLE OF bapiadsmtx,
        lt_maildata_i       TYPE TABLE OF bapiadsmtp,
        lt_maildata_i_x     TYPE TABLE OF bapiadsmtx,
        ls_telefondata      TYPE bapiadtel,
        ls_telefondata_x    TYPE bapiadtelx,
        ls_faxdata          TYPE bapiadfax,
        ls_faxdata_x        TYPE bapiadfaxx,
        ls_maildata         TYPE bapiadsmtp,
        ls_maildata_x       TYPE bapiadsmtx,
        ls_centraldata      TYPE bapibus1006_central,
        ls_centraldata_x    TYPE bapibus1006_central_x,
        ls_bankdetaildata   TYPE bapibus1006_bankdetail,
        ls_bankdetaildata_x TYPE bapibus1006_bankdetail_x,
        ls_persona          TYPE bapibus1006_central_person,
        ls_persona_x        TYPE bapibus1006_central_person_x,
        ls_empresa          TYPE bapibus1006_central_organ,
        ls_empresa_x        TYPE bapibus1006_central_organ_x,
        ls_address          TYPE bapibus1006_address,
        ls_address_x        TYPE bapibus1006_address_x,
        ls_identification	  TYPE bapibus1006_identification,
        ls_identification_x	TYPE bapibus1006_identification_x,
        ls_datos_documento  TYPE zedu_s_datos_documento,
        ls_return           TYPE bapiret2,
        lv_taxtype          TYPE bu_id_category,
        lv_taxnum           TYPE bu_id_number,
        lv_exit             TYPE boolean,
        lv_subrc            TYPE char1.

  DATA: lt_mensaje   TYPE TABLE OF bapireturn1,
        ls_mensaje   TYPE bapireturn1,
        ls_0002      TYPE zhcm_it0002,
        ls_0185      TYPE zhcm_it0185,
        ls_0006      TYPE zhcm_it0006,
        ls_0009      TYPE zhcm_it0009,
        lv_email105  TYPE comm_id_long,
        lv_city_code TYPE city_code.

*{AD_2.0.0_01
  DATA: lv_attributes  TYPE  bapi_str_bupa_fs_treasury,
        lv_attributesx TYPE  bapi_str_bupa_fs_treasury2_x,
        lt_bapiret2    TYPE STANDARD TABLE OF bapiret2,
        lwa_bapiret2   LIKE LINE OF lt_bapiret2.

  DATA lwa_departamento TYPE t005u.
*}AD_2.0.0_01

  FIELD-SYMBOLS: <fs_datos_bancarios> TYPE zedu_s_datos_bancarios.

  IF iv_no_wf IS INITIAL. " Si no requiere llamar al WF, graba directgamente.
    PERFORM f_modif_datos_sensibles USING    lv_exit
                                    CHANGING lt_return.
    et_return[]             = lt_return.
  ENDIF.

  CHECK lv_exit = abap_false.

*-- Inicio Modificación Adepcon 28.03.2017 -------------------------------------------
  CALL FUNCTION 'Z_EDU_TRAMITE_WEB_SET'
    EXPORTING
      i_xtramite = abap_true.
*-- Fin Modificación Adepcon 28.03.2017 ----------------------------------------------

  IF gs_datos_personales-name_org1 IS NOT INITIAL.

    ls_centraldata-title_key       = gs_datos_personales-title.
    ls_centraldata-searchterm1     = gs_datos_personales-idnumber.
    ls_centraldata-partnerlanguage = 'S'.".gs_datos_personales-bu_langu.
    ls_empresa-name1               = gs_datos_personales-name_org1.
    ls_empresa-name2               = gs_datos_personales-name_org2.
    ls_centraldata-centralblock    = gs_datos_direccion-xblck.  "AD_2.0.0_01


    ls_centraldata_x-title_key       = 'X'.
    ls_centraldata_x-searchterm1     = 'X'.
    ls_centraldata_x-partnerlanguage = 'X'.
    ls_centraldata_x-centralblock    = 'X'.                     "AD_2.0.0_01
    ls_empresa_x-name1               = 'X'.
    ls_empresa_x-name2               = 'X'.

  ELSE.

* Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 22/03/2017
    "Modificar Concepto de Búsqueda en BP
    ls_centraldata-searchterm1      = gs_datos_personales-idnumber.
    ls_centraldata_x-searchterm1    = 'X'.

* Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 22/03/2017
    ls_centraldata-title_key        = gs_datos_personales-title.
    ls_centraldata-partnerlanguage  = 'S'."gs_datos_personales-bu_langu.
    ls_persona-title_aca1           = gc_ninguno.
    ls_persona-firstname            = gs_datos_personales-name_first. " Nombre
    ls_persona-middlename           = gs_datos_personales-namemiddle. " Sdo nombre
    ls_persona-lastname             = gs_datos_personales-name_last.  " Apellido
    ls_persona-secondname           = gs_datos_personales-name_lst2.  " Sdo Apellido
    ls_persona-birthname            = gs_datos_personales-name_last2. " vacio
    ls_persona-correspondlanguage   = gs_datos_personales-langu_corr.
    ls_persona-maritalstatus        = gs_datos_personales-marst.
    ls_persona-birthdate            = gs_datos_personales-birthdt.

*{AD_2.0.0_01
    CLEAR lwa_departamento.

    IF gs_datos_personales-id_region_nac IS INITIAL.
      gs_datos_personales-id_region_nac = gs_datos_personales-birthdep.
    ENDIF.

    IF gs_datos_personales-birthdep IS NOT INITIAL.
      SELECT SINGLE *
       FROM t005u
         INTO lwa_departamento
           WHERE spras EQ sy-langu
           AND bland = gs_datos_personales-id_region_nac "gs_datos_personales-birthdep
           AND land1 = gs_datos_personales-id_country_nac.
    ENDIF.
*}AD_2.0.0_01

    CONCATENATE gs_datos_personales-birthpl ', ' lwa_departamento-bezei  INTO ls_persona-birthplace. "MO_2.0.0_01
*    ls_persona-birthplace           = gs_datos_personales-birthpl.                                       "CO_2.0.0_01
    ls_persona-occupation           = gs_datos_personales-jobgr.
    ls_persona-sex                  = gs_datos_personales-sexo.
    ls_centraldata-centralblock    = gs_datos_direccion-xblck.  "AD_2.0.0_01

    ls_centraldata_x-centralblock    = 'X'.                     "AD_2.0.0_01
    ls_centraldata_x-title_key        = 'X'.
    ls_centraldata_x-partnerlanguage  = 'X'.
    ls_persona_x-title_aca1           = 'X'.
    ls_persona_x-secondname           = 'X'.
    ls_persona_x-firstname            = 'X'.
    ls_persona_x-lastname             = 'X'.
    ls_persona_x-birthname            = 'X'.
    ls_persona_x-middlename           = 'X'.
    ls_persona_x-correspondlanguage   = 'X'.
    ls_persona_x-maritalstatus        = 'X'.
    ls_persona_x-birthdate            = 'X'.
    ls_persona_x-birthplace           = 'X'.                    "AD_2.0.0_01
    ls_persona_x-occupation           = 'X'.
    ls_persona_x-sex                  = 'X'.

  ENDIF.

  lv_taxtype = gs_datos_personales-type.
  lv_taxnum  = gs_datos_personales-idnumber.

  CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
    EXPORTING
      businesspartner           = gs_datos_personales-partner
      centraldata               = ls_centraldata
      centraldataperson         = ls_persona
      centraldataorganization   = ls_empresa
      centraldata_x             = ls_centraldata_x
      centraldataperson_x       = ls_persona_x
      centraldataorganization_x = ls_empresa_x
    TABLES
      return                    = et_return.

  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.

* Inicio agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 22/03/2017
  "Modificar Nombres en Estudiante
  CALL FUNCTION 'Z_EDU_SAVE_MODIF_STUDENT'
    EXPORTING
      i_partner = gs_datos_personales-partner
    TABLES
      et_return = et_return.

  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.
* Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 22/03/2017


*** Dirección
  ls_address-street     = gs_datos_direccion-street.
*	Begin	-->	MgM DCEK903217 Urbanización 02/02/2017
  IF ls_address-street IS INITIAL.
    ls_address-street = gs_datos_direccion-urbanizacion.
  ENDIF.
*	End	  -->	MgM DCEK903217
  ls_address-city       = gs_datos_direccion-city1.
  ls_address-postl_cod1 = gs_datos_direccion-post_code1.
  ls_address-region     = gs_datos_direccion-region.
  ls_address-country    = gs_datos_direccion-country.
  ls_address-po_box     = gs_datos_direccion-codigo. " Responsabilidad fiscal
  ls_address-postl_cod2     = gs_datos_direccion-clase. " Actividad económica

  ls_address_x-street     = 'X'.
  ls_address_x-city       = 'X'.
  ls_address_x-postl_cod1 = 'X'.
  ls_address_x-region     = 'X'.
  ls_address_x-country    = 'X'.
  ls_address_x-po_box     = 'X'.
  ls_address_x-postl_cod2 = 'X'.

*  IF gs_datos_direccion-fax_number IS NOT INITIAL.  "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  ls_faxdata-consnumber = '001'.
  ls_faxdata-fax       = gs_datos_direccion-fax_number.
  ls_faxdata-extension = gs_datos_direccion-fax_extens.
  APPEND ls_faxdata TO lt_faxdata.
  ls_faxdata_x-consnumber = '001'.
  ls_faxdata_x-fax       = 'X'.
  ls_faxdata_x-extension = 'X'.
  IF gs_datos_direccion_ini-fax_number IS NOT INITIAL.
    ls_faxdata_x-updateflag = 'U'.
  ELSE.
    ls_faxdata_x-updateflag = 'I'.
  ENDIF.
  APPEND ls_faxdata_x TO lt_faxdata_x.
*  ENDIF.

*  IF gs_datos_direccion-tel_number IS NOT INITIAL. "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  ls_telefondata-consnumber = '001'.
  ls_telefondata-telephone = gs_datos_direccion-tel_number.
  ls_telefondata-extension = gs_datos_direccion-tel_extens.
  ls_telefondata-r_3_user  = '1'. "Fijo
  ls_telefondata_x-consnumber = '001'.
  ls_telefondata_x-telephone = 'X'.
  ls_telefondata_x-extension = 'X'.
  ls_telefondata_x-r_3_user  = 'X'. "Fijo
  IF gs_datos_direccion_ini-tel_number IS NOT INITIAL.
    ls_telefondata_x-updateflag = 'U'.
    APPEND ls_telefondata TO lt_telefondata.
    APPEND ls_telefondata_x TO lt_telefondata_x.
  ELSE.
    ls_telefondata_x-updateflag = 'I'.
    APPEND ls_telefondata TO lt_telefondata_i.
    APPEND ls_telefondata_x TO lt_telefondata_i_x.
  ENDIF.
*  ENDIF.

*  IF gs_datos_direccion-tel_movil IS NOT INITIAL. "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  CLEAR: ls_telefondata, ls_telefondata_x.
  ls_telefondata-consnumber = '002'.
  ls_telefondata-telephone = gs_datos_direccion-tel_movil.
  ls_telefondata-r_3_user  = '3'. " celular
  ls_telefondata_x-consnumber = '002'.
  ls_telefondata_x-telephone = 'X'.
  ls_telefondata_x-r_3_user  = 'X'.
  IF gs_datos_direccion_ini-tel_movil IS NOT INITIAL.
    ls_telefondata_x-updateflag = 'U'.
    APPEND ls_telefondata TO lt_telefondata.
    APPEND ls_telefondata_x TO lt_telefondata_x.
  ELSE.
    ls_telefondata_x-updateflag = 'I'.
    APPEND ls_telefondata TO lt_telefondata_i.
    APPEND ls_telefondata_x TO lt_telefondata_i_x.
  ENDIF.
*  ENDIF.

*  IF gs_datos_direccion-smtp_addr IS NOT INITIAL. "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  ls_maildata-consnumber = '001'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr.
  ls_maildata_x-consnumber = '001'.
  ls_maildata_x-e_mail = 'X'.
  IF gs_datos_direccion_ini-smtp_addr IS NOT INITIAL.
    ls_maildata_x-updateflag = 'U'.
    APPEND ls_maildata TO lt_maildata.
    APPEND ls_maildata_x TO lt_maildata_x.
  ELSE.
    ls_maildata_x-updateflag = 'I'.
    APPEND ls_maildata TO lt_maildata_i.
    APPEND ls_maildata_x TO lt_maildata_i_x.
  ENDIF.
*  ENDIF.

*  IF gs_datos_direccion-smtp_addr_2 IS NOT INITIAL.  "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  ls_maildata-consnumber = '002'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr_2.
  ls_maildata_x-consnumber = '002'.
  ls_maildata_x-e_mail = 'X'.
  IF gs_datos_direccion_ini-smtp_addr_2 IS NOT INITIAL.
    ls_maildata_x-updateflag = 'U'.
    APPEND ls_maildata TO lt_maildata.
    APPEND ls_maildata_x TO lt_maildata_x.
  ELSE.
    ls_maildata_x-updateflag = 'I'.
    APPEND ls_maildata TO lt_maildata_i.
    APPEND ls_maildata_x TO lt_maildata_i_x.
  ENDIF.
*  ENDIF.

*  IF gs_datos_direccion-smtp_addr_3 IS NOT INITIAL. "CO_2.0.0_01 Se comenta esta línea para que al borrar el dato, sea reemplazado por vacío
  ls_maildata-consnumber = '003'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr_3.
  ls_maildata_x-consnumber = '003'.
  ls_maildata_x-e_mail = 'X'.
  IF gs_datos_direccion_ini-smtp_addr_3 IS NOT INITIAL.
    ls_maildata_x-updateflag = 'U'.
    APPEND ls_maildata TO lt_maildata.
    APPEND ls_maildata_x TO lt_maildata_x.
  ELSE.
    ls_maildata_x-updateflag = 'I'.
    APPEND ls_maildata TO lt_maildata_i.
    APPEND ls_maildata_x TO lt_maildata_i_x.
  ENDIF.
*  ENDIF.

  "Asigna los registros a crear de ultimo
  APPEND LINES OF lt_telefondata_i   TO lt_telefondata.
  APPEND LINES OF lt_telefondata_i_x TO lt_telefondata_x.
  APPEND LINES OF lt_maildata_i      TO lt_maildata.
  APPEND LINES OF lt_maildata_i_x    TO lt_maildata_x.

  CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
    EXPORTING
      businesspartner = gs_datos_personales-partner
      addressdata     = ls_address
      addressdata_x   = ls_address_x
    TABLES
      bapiadtel       = lt_telefondata
      bapiadfax       = lt_faxdata
      bapiadsmtp      = lt_maildata
      bapiadtel_x     = lt_telefondata_x
      bapiadfax_x     = lt_faxdata_x
      bapiadsmt_x     = lt_maildata_x
      return          = et_return.

  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.

*{AD_2.0.0_01
* Actualiza el grupo destino
  DATA lv_group_d TYPE tp13t-group_d.

  IF gs_datos_direccion-group_d IS NOT INITIAL.

    SELECT SINGLE group_d FROM tp13t
      INTO lv_group_d
      WHERE langu = sy-langu
      AND group_d_t = gs_datos_direccion-group_d.

    lv_attributes-target_group = lv_group_d. "gs_datos_direccion-group_d.
    lv_attributesx-target_group = 'X'.
    CALL FUNCTION 'BAPI_BUPA_FS_ATTRIBUTES_SET'
      EXPORTING
        businesspartner = gs_datos_personales-partner
        attributes      = lv_attributes
        attributesx     = lv_attributesx
      TABLES
        return          = lt_bapiret2.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
*}AD_2.0.0_01

  IF gt_datos_documento IS INITIAL.
    IF gs_datos_personales-type     NE gs_datos_personales-type_old
    OR gs_datos_personales-idnumber NE gs_datos_personales-idnumber_old.

*	Begin	-->	MgM DCEK903217 cambio de identificación 03/02/2017
*      select single valid_date_from valid_date_to
*        from but0id
*        into (ls_identification-idvalidfromdate,
*              ls_identification-idvalidtodate)
*        where type     = gs_datos_personales-type_old
*          and idnumber = gs_datos_personales-idnumber_old
*          and valid_date_from <= sy-datum
*          and valid_date_to >= sy-datum.

      SELECT  type,
              idnumber,
              valid_date_from,
              valid_date_to
        FROM but0id
          INTO TABLE @DATA(lt_identificaciones)
            WHERE partner EQ  @gs_datos_personales-partner
              ORDER BY type, idnumber.

      IF sy-subrc NE 0.
        CLEAR lt_identificaciones[].
      ENDIF.

      READ TABLE lt_identificaciones
        WITH KEY  type     = gs_datos_personales-type_old
                  idnumber = gs_datos_personales-idnumber_old
          INTO DATA(ls_identif) BINARY SEARCH.

      IF sy-subrc EQ 0.
        MOVE ls_identif-valid_date_from TO ls_identification-idvalidfromdate.
        MOVE ls_identif-valid_date_to   TO ls_identification-idvalidtodate.
      ENDIF.
*	End	  -->	MgM DCEK903217

      IF ls_identification-idvalidfromdate = sy-datum.
        ls_identification-idvalidfromdate   = sy-datum - 1.
        ls_identification_x-idvalidfromdate = 'X'.
      ENDIF.
***      IF ls_identification-identrydate = sy-datum.
***        ls_identification-identrydate   = sy-datum - 1.
***        ls_identification_x-identrydate = 'X'.
***      ENDIF.
      ls_identification-idvalidtodate   = sy-datum - 1.
      ls_identification_x-idvalidtodate = 'X'.

      CALL FUNCTION 'BAPI_IDENTIFICATION_CHANGE'
        EXPORTING
          businesspartner        = gs_datos_personales-partner
          identificationcategory = gs_datos_personales-type_old
          identificationnumber   = gs_datos_personales-idnumber_old
          identification         = ls_identification
          identification_x       = ls_identification_x
        TABLES
          return                 = et_return.

      READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
      ENDIF.

*	Begin	-->	MgM DCEK903217 cambio de identificación 03/02/2017
      CLEAR ls_identif.

      "primero validamos si la nueva identificación ya existía para el bp
      READ TABLE lt_identificaciones
        WITH KEY  type     = gs_datos_personales-type
                  idnumber = gs_datos_personales-idnumber
          INTO ls_identif BINARY SEARCH.

      IF sy-subrc EQ 0.
        "modifico identificación
        CLEAR: ls_identification, ls_identification_x.
        ls_identification-idvalidfromdate = sy-datum.
        ls_identification-idvalidtodate   = '99991231'.
        ls_identification_x-idvalidfromdate = cl_bp_const=>true.
        ls_identification_x-idvalidtodate   = cl_bp_const=>true.
        CALL FUNCTION 'BAPI_IDENTIFICATION_CHANGE'
          EXPORTING
            businesspartner        = gs_datos_personales-partner
            identificationcategory = gs_datos_personales-type
            identificationnumber   = gs_datos_personales-idnumber
            identification         = ls_identification
            identification_x       = ls_identification_x
          TABLES
            return                 = et_return.

        READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          EXIT.
        ENDIF.
      ELSE.
        "agrego identificación
*	End	  -->	MgM DCEK903217

        ls_identification-identrydate     = gs_datos_personales-id_entry_date.
        ls_identification-idinstitute     = gs_datos_personales-id_institute.
        ls_identification-country         = gs_datos_personales-id_country.
        ls_identification-region          = gs_datos_personales-id_region.
        ls_identification-idvalidfromdate = sy-datum.
        ls_identification-idvalidtodate   = '99991231'.

        CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
          EXPORTING
            businesspartner        = gs_datos_personales-partner
            identificationcategory = gs_datos_personales-type
            identificationnumber   = gs_datos_personales-idnumber
            identification         = ls_identification
          TABLES
            return                 = et_return.
        .
        READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.

      ls_identification-identrydate     = gs_datos_personales-id_entry_date.
      ls_identification-idinstitute     = gs_datos_personales-id_institute.
      ls_identification-country         = gs_datos_personales-id_country.
      ls_identification-region          = gs_datos_personales-id_region.

      ls_identification_x-identrydate = 'X'.
      ls_identification_x-idinstitute = 'X'.
      ls_identification_x-country     = 'X'.
      ls_identification_x-region      = 'X'.

      CALL FUNCTION 'BAPI_IDENTIFICATION_CHANGE'
        EXPORTING
          businesspartner        = gs_datos_personales-partner
          identificationcategory = gs_datos_personales-type
          identificationnumber   = gs_datos_personales-idnumber
          identification         = ls_identification
          identification_x       = ls_identification_x
        TABLES
          return                 = et_return.

      READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
      ENDIF.

    ENDIF.

  ELSE.
    "Solo novedades
    LOOP AT gt_datos_documento INTO ls_datos_documento.

      ls_identification-idvalidfromdate = sy-datum.
      ls_identification-idvalidtodate   = '99991231'.
      ls_identification-identrydate     = gs_datos_personales-id_entry_date.
      ls_identification-idinstitute     = gs_datos_personales-id_institute.
      ls_identification-country         = gs_datos_personales-id_country.
      ls_identification-region          = gs_datos_personales-id_region.

      CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
        EXPORTING
          businesspartner        = gs_datos_personales-partner
          identificationcategory = ls_datos_documento-type
          identificationnumber   = ls_datos_documento-idnumber
          identification         = ls_identification
        TABLES
          return                 = et_return.
      .
      READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDIF.

*	Begin	-->	MgM DCEK903217 Elimina cuentas bancarias 01/02/2017
  DATA lt_bankdetaildata_bd   TYPE piqbapibus1006_bankdetails.

  "Recupero el detalle de bancos anterior a la edición
  CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
    EXPORTING
      businesspartner = gs_datos_personales-partner
    TABLES
      bankdetails     = lt_bankdetaildata_bd.

  LOOP AT lt_bankdetaildata_bd
    INTO DATA(ls_bankdetail).

    AT FIRST.
      SORT gt_datos_bancarios BY  banks
                                  bankl.
    ENDAT.

    READ TABLE gt_datos_bancarios
      WITH KEY  banks = ls_bankdetail-bank_ctry
                bankl = ls_bankdetail-bank_key
        TRANSPORTING NO FIELDS.

    IF sy-subrc NE 0.
      "si no existe debo eliminarlo
      CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_REMOVE'
        EXPORTING
          businesspartner = gs_datos_personales-partner
          bankdetailid    = ls_bankdetail-bankdetailid
        TABLES
          return          = et_return.

    ENDIF.

  ENDLOOP.
*	End	  -->	MgM DCEK903217

  IF gt_datos_bancarios IS NOT INITIAL.

    ls_bankdetaildata_x-bank_ctry     = 'X'.
    ls_bankdetaildata_x-bank_key      = 'X'.
    ls_bankdetaildata_x-bank_acct     = 'X'.
    ls_bankdetaildata_x-ctrl_key      = 'X'.
    ls_bankdetaildata_x-bank_ref      = 'X'.
    ls_bankdetaildata_x-accountholder = 'X'.

    LOOP AT gt_datos_bancarios ASSIGNING <fs_datos_bancarios>.

*	Begin	-->	MgM DCEK903341 banco existente 07/02/2017
*      if <fs_datos_bancarios>-bkvid is not initial.
      READ TABLE lt_bankdetaildata_bd
        WITH KEY  bank_ctry = <fs_datos_bancarios>-banks
                  bank_key  = <fs_datos_bancarios>-bankl
          INTO ls_bankdetail.

      IF sy-subrc EQ 0 AND "si existía
         ( ls_bankdetail-bank_acct     NE  <fs_datos_bancarios>-bankn OR
           ls_bankdetail-ctrl_key      NE  <fs_datos_bancarios>-bkont OR
           ls_bankdetail-bank_ref      NE  <fs_datos_bancarios>-bkref OR
           ls_bankdetail-accountholder NE  <fs_datos_bancarios>-koinh ).
*	End	  -->	MgM DCEK903341

        ls_bankdetaildata-bank_ctry     = <fs_datos_bancarios>-banks.
        ls_bankdetaildata-bank_key      = <fs_datos_bancarios>-bankl.
        ls_bankdetaildata-bank_acct     = <fs_datos_bancarios>-bankn.
        ls_bankdetaildata-ctrl_key      = <fs_datos_bancarios>-bkont.
        ls_bankdetaildata-bank_ref      = <fs_datos_bancarios>-bkref.
        ls_bankdetaildata-accountholder = <fs_datos_bancarios>-koinh.

        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_CHANGE'
          EXPORTING
            businesspartner  = gs_datos_personales-partner
*	Begin	-->	MgM DCEK903341 banco existente 07/02/2017
*           bankdetailid     = <fs_datos_bancarios>-bkvid
            bankdetailid     = ls_bankdetail-bankdetailid
*	End	  -->	MgM DCEK903341
            bankdetaildata   = ls_bankdetaildata
            bankdetaildata_x = ls_bankdetaildata_x
          TABLES
            return           = et_return.

*	Begin	-->	MgM DCEK903341 banco existente 07/02/2017
*      else.
      ELSEIF sy-subrc NE 0.
*	End	  -->	MgM DCEK903341

        ls_bankdetaildata-bank_ctry     = <fs_datos_bancarios>-banks.
        ls_bankdetaildata-bank_key      = <fs_datos_bancarios>-bankl.
        ls_bankdetaildata-bank_acct     = <fs_datos_bancarios>-bankn.
        ls_bankdetaildata-ctrl_key      = <fs_datos_bancarios>-bkont.
        ls_bankdetaildata-bank_ref      = <fs_datos_bancarios>-bkref.
        ls_bankdetaildata-accountholder = <fs_datos_bancarios>-koinh.

        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
          EXPORTING
            businesspartner = gs_datos_personales-partner
            bankdetaildata  = ls_bankdetaildata
          TABLES
            return          = et_return.

      ENDIF.

    ENDLOOP.

  ENDIF.


**********************************************************************
*** Si es persona actualizo los infotipos.
  IF gs_datos_personales-name_org1 IS INITIAL.

    SELECT SINGLE anred
      FROM zhcm_tratam_bp
      INTO ls_0002-anred
      WHERE title = gs_datos_personales-title.
    ls_0002-perid = gs_datos_personales-idnumber.
    ls_0002-vorna = gs_datos_personales-name_first.
    ls_0002-name2 = gs_datos_personales-namemiddle.
    ls_0002-nachn = gs_datos_personales-name_last.
    ls_0002-nach2 = gs_datos_personales-name_lst2.
    ls_0002-gbdat = gs_datos_personales-birthdt.
    ls_0002-gesch = gs_datos_personales-sexo.
    ls_0002-famst = gs_datos_personales-marst.

    SELECT SINGLE ictyp
      FROM zhcm_docum_bp
      INTO ls_0185-ictyp
      WHERE type = gs_datos_personales-type.
    ls_0185-icnum	= gs_datos_personales-idnumber.
    ls_0185-fpdat	= gs_datos_personales-id_entry_date.
    ls_0185-isspl	= gs_datos_personales-id_institute.
    ls_0185-iscot	= gs_datos_personales-id_country.

    ls_0006-stras = gs_datos_direccion-street.
    ls_0006-land1 = gs_datos_direccion-country.
    ls_0006-state = gs_datos_direccion-region.
    ls_0006-telnr = gs_datos_direccion-tel_number.
    ls_0006-num01 = gs_datos_direccion-tel_movil.
    ls_0006-num02 = gs_datos_direccion-fax_number.
    SELECT SINGLE city_code
      FROM adrcityt
      INTO lv_city_code
      WHERE country   = gs_datos_direccion-country
        AND langu     = sy-langu
        AND city_name = gs_datos_direccion-city1.
    ls_0006-fprcd = lv_city_code+9(3).

***    LOOP AT gt_datos_bancarios ASSIGNING <fs_datos_bancarios>.
***      ls_0009-banks = <fs_datos_bancarios>-banks.
***      ls_0009-bankl = <fs_datos_bancarios>-banks.
***      ls_0009-bankn = <fs_datos_bancarios>-banks.
***      ls_0009-bkont = <fs_datos_bancarios>-banks.
***      EXIT.
***    ENDLOOP.

    lv_email105 = gs_datos_direccion-smtp_addr. "????

    CALL FUNCTION 'ZHCM_ACT_INFOTIPOS'
      EXPORTING
        partner  = gs_datos_personales-partner
        it0002   = ls_0002
        it0185   = ls_0185
        it0006   = ls_0006
***        it0009   = ls_0009
        email105 = lv_email105
      IMPORTING
        subrc    = lv_subrc
      TABLES
        mensaje  = lt_mensaje.

*** si el sy-subrc es 2, debe dejar grabar el BP
    IF lv_subrc NE 2. " No existen registros asociados al BP.
      LOOP AT lt_mensaje INTO ls_mensaje.
        MOVE-CORRESPONDING ls_mensaje TO ls_return.
        APPEND ls_return TO et_return.
      ENDLOOP.
    ENDIF.

  ENDIF.
**********************************************************************

  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.

*-- Inicio Modificación Adepcon 28.03.2017 -------------------------------------------
  CALL FUNCTION 'Z_EDU_TRAMITE_WEB_SET'
    EXPORTING
      i_xtramite = abap_false.
*-- Fin Modificación Adepcon 28.03.2017 ----------------------------------------------

ENDFUNCTION.
