FUNCTION z_edu_create_rol .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(I_TIPO_ROL) TYPE  CHAR1
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
*  FLCU00 Deudor
*  FLCU01 Cliente
*  FLVN00 Acreedor
*  FLVN01 Proveedor

  DATA: "lo_cvi          TYPE REF TO cvi_mapper,  "DCEK902844
        "lt_customers    TYPE cmds_ei_extern_t,
        lr_roles        TYPE RANGE OF bu_role,
        lrs_roles       LIKE LINE OF lr_roles,
        "ls_partners     TYPE bus_ei_main,
        "ls_partner      TYPE bus_ei_extern,
        "ls_adress       TYPE bus_ei_bupa_address,
        "ls_ident_number TYPE bus_ei_bupa_identification,
        "ls_bankdetail   TYPE bus_ei_bupa_bankdetail,
        "ls_rol          TYPE bus_ei_bupa_roles,
        ls_return       TYPE bapiret2,
        "lv_guid         TYPE bu_partner_guid,
        lv_message      TYPE string,
        lv_exit.

*** Cargo los roles a crear o modificar
  CASE i_tipo_rol.
    WHEN 'C'. " Cliente / Deudor
      lrs_roles = 'IEQ'.
      lrs_roles-low = 'FLCU00'. " Deudor
      APPEND lrs_roles TO lr_roles.
      lrs_roles-low = 'FLCU01'. " Cliente
      APPEND lrs_roles TO lr_roles.
    WHEN 'P'. " Proveedor / Acreedor
      lrs_roles = 'IEQ'.
      lrs_roles-low = 'FLVN00'. " Acreedor
      APPEND lrs_roles TO lr_roles.
      lrs_roles-low = 'FLVN01'. " Proveedor
      APPEND lrs_roles TO lr_roles.
    WHEN 'E'. " Persona Externa
      lrs_roles-low = 'CBIH10'. " Persona Externa
      APPEND lrs_roles TO lr_roles.
    WHEN 'M'.
      lrs_roles-low = 'MKK'.
      APPEND lrs_roles TO lr_roles.
  ENDCASE.


  LOOP AT lr_roles INTO lrs_roles.

    CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
      EXPORTING
        businesspartner             = i_partner
        businesspartnerrolecategory = lrs_roles-low
      TABLES
        return                      = et_return.

    READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      lv_exit = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

  CHECK lv_exit IS INITIAL.

  CASE i_tipo_rol.
    WHEN 'C'. " Cliente / Deudor
      CALL FUNCTION 'Z_EDU_REFRESH_DEUDOR'.
*   Se creó el rol de & para el BP.
      MESSAGE s027(zedu_wd_message) WITH 'Cliente / Deudor'(001) INTO lv_message.
    WHEN 'P'. " Proveedor / Acreedor
      CALL FUNCTION 'Z_EDU_REFRESH_ACREEDOR'.
*   Se creó el rol de & para el BP.
      MESSAGE s027(zedu_wd_message) WITH 'Proveedor / Acreedor'(002) INTO lv_message.
    WHEN 'M'. " MKK
      CALL FUNCTION 'Z_EDU_REFRESH_DEUDOR'.
*   Se creó el rol de & para el BP.
      MESSAGE s027(zedu_wd_message) WITH 'Mkk' INTO lv_message.
  ENDCASE.

  MOVE sy-msgid TO ls_return-id.
  MOVE sy-msgno TO ls_return-number.
  MOVE sy-msgty TO ls_return-type.
  MOVE sy-msgv1 TO ls_return-message_v1.

  APPEND ls_return TO et_return.
ENDFUNCTION.
