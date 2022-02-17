FUNCTION z_edu_save_alta.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     REFERENCE(EV_PARTNER) TYPE  BU_PARTNER
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
*& Descripción : 01: Se adiciona actualización de responsabilidad fiscal
*&               actividad económica,                     *
*&---------------------------------------------------------------------*



*** Grabación de trámite de alta de personas o empresas.

  DATA: lt_return          TYPE TABLE OF bapiret2,
        lt_telefondata     TYPE TABLE OF bapiadtel,
        lt_faxdata         TYPE TABLE OF bapiadfax,
        lt_maildata        TYPE TABLE OF bapiadsmtp,
        ls_telefondata     TYPE bapiadtel,
        ls_faxdata         TYPE bapiadfax,
        ls_maildata        TYPE bapiadsmtp,
        ls_centraldata     TYPE bapibus1006_central,
        ls_bankdetaildata  TYPE bapibus1006_bankdetail,
        ls_persona         TYPE bapibus1006_central_person,
        ls_empresa         TYPE bapibus1006_central_organ,
        ls_address         TYPE bapibus1006_address,
        ls_identification	 TYPE bapibus1006_identification,
        ls_return          TYPE bapiret2,
        lv_partnercategory TYPE bapibus1006_head-partn_cat,
        ls_documentos      TYPE zedu_s_datos_documento,
        lv_taxtype         TYPE bu_id_category,
        lv_taxnum          TYPE bu_id_number,
        lv_partnergroup    TYPE bu_group.
*{AD_2.0.0_01
  DATA: lv_attributes  TYPE  bapi_str_bupa_fs_treasury,
        lv_attributesx TYPE  bapi_str_bupa_fs_treasury2_x,
        lt_bapiret2    TYPE STANDARD TABLE OF bapiret2,
        lwa_bapiret2   LIKE LINE OF lt_bapiret2.

  DATA lwa_departamento TYPE t005u.
*}AD_2.0.0_01

  FIELD-SYMBOLS: <fs_datos_bancarios> TYPE zedu_s_datos_bancarios.

  IF gs_datos_personales-name_org1 IS NOT INITIAL.
    lv_partnercategory             = '2'. "  Organización
    lv_partnergroup                = 'ZORG'.
    ls_centraldata-partnertype     = 'ZORG'.
    ls_centraldata-title_key       = gs_datos_personales-title.
    ls_centraldata-searchterm1     = gs_datos_personales-idnumber.
    ls_centraldata-partnerlanguage = 'S'."gs_datos_personales-bu_langu.
    ls_empresa-name1               = gs_datos_personales-name_org1.
    ls_empresa-name2               = gs_datos_personales-name_org2.

  ELSE.

    lv_partnercategory              = '1'. "  Persona
    lv_partnergroup                 = 'ZPER'.
    ls_centraldata-partnertype      = 'ZPER'.
    ls_centraldata-title_key        = gs_datos_personales-title.
    ls_centraldata-partnerlanguage  = 'S'."gs_datos_personales-bu_langu.
    ls_centraldata-searchterm1      = gs_datos_personales-idnumber.
    ls_centraldata-centralblock     = gs_datos_direccion-xblck. " AD_2.0.0_01

    ls_persona-title_aca1           = gc_ninguno.
    ls_persona-firstname            = gs_datos_personales-name_first. " Nombre
    ls_persona-middlename           = gs_datos_personales-namemiddle. " Sdo nombre
    ls_persona-lastname             = gs_datos_personales-name_last.  " Apellido
    ls_persona-secondname           = gs_datos_personales-name_lst2.  " Sdo Apellido
    ls_persona-birthname            = gs_datos_personales-name_last2. " vacio
    "ls_persona-birthname            = gs_datos_personales-name_lst2. "Leonardo P. Marzo 20 / Apellido Soltera = Segundo Apellido
    ls_persona-correspondlanguage   = gs_datos_personales-langu_corr.
    ls_persona-maritalstatus        = gs_datos_personales-marst.
    ls_persona-birthdate            = gs_datos_personales-birthdt.
*{AD_2.0.0_01
    CLEAR lwa_departamento.
    IF gs_datos_personales-birthdep IS NOT INITIAL.
      SELECT SINGLE *
       FROM t005u
         INTO lwa_departamento
           WHERE spras EQ sy-langu
           AND bland = gs_datos_personales-birthdep
           AND land1 = gs_datos_personales-id_country_nac.
    ENDIF.
*}AD_2.0.0_01
*    ls_persona-birthplace           = gs_datos_personales-birthpl.                                            "MO_2.0.0_01
    CONCATENATE gs_datos_personales-birthpl ', ' lwa_departamento-bezei INTO ls_persona-birthplace. "MO_2.0.0_1
    ls_persona-occupation           = gs_datos_personales-jobgr.
    ls_persona-sex                  = gs_datos_personales-sexo.

  ENDIF.

  lv_taxtype = gs_datos_personales-type.
  lv_taxnum  = gs_datos_personales-idnumber.

  ls_faxdata-fax       = gs_datos_direccion-fax_number.
  ls_faxdata-extension = gs_datos_direccion-fax_extens.
  APPEND ls_faxdata TO lt_faxdata.

  ls_telefondata-telephone  = gs_datos_direccion-tel_number.
  ls_telefondata-extension  = gs_datos_direccion-tel_extens.
  ls_telefondata-r_3_user = '1'. "Fijo
  APPEND ls_telefondata TO lt_telefondata.

  CLEAR ls_telefondata.
  ls_telefondata-telephone = gs_datos_direccion-tel_movil.
  ls_telefondata-r_3_user  = '3'. " celular
  APPEND ls_telefondata TO lt_telefondata.

  CLEAR ls_maildata.
  ls_maildata-consnumber = '001'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr.
  APPEND ls_maildata TO lt_maildata.
  CLEAR ls_maildata.
  ls_maildata-consnumber = '002'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr_2.
  APPEND ls_maildata TO lt_maildata.
  CLEAR ls_maildata.
  ls_maildata-consnumber = '003'.
  ls_maildata-e_mail = gs_datos_direccion-smtp_addr_3.
  APPEND ls_maildata TO lt_maildata.

*** Dirección
  ls_address-street     = gs_datos_direccion-street.
*	Begin	-->	MgM DCEK906593 Urbanización para países <> Colombia 03/10/2017
  IF ls_address-street IS INITIAL.
    ls_address-street = gs_datos_direccion-urbanizacion.
  ENDIF.
*	End	  -->	MgM DCEK906593
  ls_address-city       = gs_datos_direccion-city1.
  ls_address-postl_cod1 = gs_datos_direccion-post_code1.
  ls_address-region     = gs_datos_direccion-region.
  ls_address-country    = gs_datos_direccion-country.
  ls_address-langu      = 'S'.

*{AD_2.0.0_01
  ls_address-po_box = gs_datos_direccion-codigo.    " Responsabilidad fiscal
  ls_address-postl_cod2 = gs_datos_direccion-clase. " Actividad económica
*}AD_2.0.0_01

  CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
    EXPORTING
      partnercategory         = lv_partnercategory
      partnergroup            = lv_partnergroup
      centraldata             = ls_centraldata
      centraldataperson       = ls_persona
      centraldataorganization = ls_empresa
      addressdata             = ls_address
    IMPORTING
      businesspartner         = ev_partner
    TABLES
      telefondata             = lt_telefondata
      faxdata                 = lt_faxdata
      e_maildata              = lt_maildata
      return                  = et_return.


  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.

  IF gt_datos_documento IS INITIAL.
    ls_identification-identrydate     = gs_datos_personales-id_entry_date.
    ls_identification-idinstitute     = gs_datos_personales-id_institute.
    ls_identification-country         = gs_datos_personales-id_country.
    ls_identification-region          = gs_datos_personales-id_region.
    ls_identification-idvalidfromdate = sy-datum.
    ls_identification-idvalidtodate   = '99991231'.

    CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
      EXPORTING
        businesspartner        = ev_partner
        identificationcategory = lv_taxtype
        identificationnumber   = lv_taxnum
        identification         = ls_identification
      TABLES
        return                 = et_return.

    READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      EXIT.
    ENDIF.

  ELSE.

    ls_identification-idvalidfromdate = sy-datum.
    ls_identification-idvalidtodate   = '99991231'.
    ls_identification-identrydate     = gs_datos_personales-id_entry_date.
    ls_identification-idinstitute     = gs_datos_personales-id_institute.
    ls_identification-country         = gs_datos_personales-id_country.
    ls_identification-region          = gs_datos_personales-id_region.

    LOOP AT gt_datos_documento INTO ls_documentos.

      CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
        EXPORTING
          businesspartner        = ev_partner
          identificationcategory = ls_documentos-type
          identificationnumber   = ls_documentos-idnumber
          identification         = ls_identification
        TABLES
          return                 = et_return.

      READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.


  IF gt_datos_bancarios IS NOT INITIAL.

    LOOP AT gt_datos_bancarios ASSIGNING <fs_datos_bancarios>.

      ls_bankdetaildata-bank_ctry     = <fs_datos_bancarios>-banks.
      ls_bankdetaildata-bank_key      = <fs_datos_bancarios>-bankl.
      ls_bankdetaildata-bank_acct     = <fs_datos_bancarios>-bankn.
      ls_bankdetaildata-ctrl_key      = <fs_datos_bancarios>-bkont.
      ls_bankdetaildata-bank_ref      = <fs_datos_bancarios>-bkref.
      ls_bankdetaildata-accountholder = <fs_datos_bancarios>-koinh.

      CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
        EXPORTING
          businesspartner = ev_partner
          bankdetaildata  = ls_bankdetaildata
        TABLES
          return          = et_return.

    ENDLOOP.

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
        businesspartner = ev_partner
        attributes      = lv_attributes
        attributesx     = lv_attributesx
      TABLES
        return          = lt_bapiret2.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.
*}AD_2.0.0_01


  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    EXIT.
  ENDIF.

  gs_datos_personales-partner = ev_partner.

ENDFUNCTION.
