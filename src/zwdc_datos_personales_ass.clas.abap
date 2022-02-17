class ZWDC_DATOS_PERSONALES_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  class-methods VALIDA_EXISTE_BP
    importing
      !IV_TIPO_DOC type BU_ID_TYPE
      !IV_NUM_DOC type BU_ID_NUMBER
    exporting
      !EV_BP type BU_PARTNER
      !EV_TYPE type BU_TYPE
      !EV_NAME type STRING .
  methods GET_FROM_MEM
    returning
      value(RT_DATOS_PERSONALES) type ZEDU_S_DATOS_PERSONALES .
  methods SET_FROM_MEM
    importing
      !IS_DATOS_PERSONALES type ZEDU_S_DATOS_PERSONALES .
  methods SAVE_MODIF_FROM_MEM
    importing
      !IV_PARTNER type BU_PARTNER optional
    returning
      value(RT_ERRORES) type BAPIRET2_T .
  methods SAVE_ALTA_FROM_MEM
    returning
      value(RT_ERRORES) type BAPIRET2_T .
  methods GET_FROM_MEM_BP
    importing
      !IV_BP type BU_PARTNER
    returning
      value(RT_RETURN) type BAPIRET2_T .
  methods REFRESH_MEM .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_DATOS_PERSONALES_ASS IMPLEMENTATION.


  method GET_FROM_MEM.

      CALL FUNCTION 'Z_EDU_GET_DATA_TRM'
        IMPORTING
          es_datos_personales = rt_datos_personales.

  endmethod.


  METHOD get_from_mem_bp.


    CALL FUNCTION 'Z_EDU_GET_DATA_FROM_BP'
      EXPORTING
        iv_bp     = iv_bp
      TABLES
        et_return = rt_return.


  ENDMETHOD.


  method REFRESH_MEM.

   CALL FUNCTION 'Z_EDU_REFRESH_DATA_TRM'.

  endmethod.


  METHOD save_alta_from_mem.

    DATA: ls_errores TYPE bapiret2,
          lv_partner TYPE bu_partner.

    CALL FUNCTION 'Z_EDU_SAVE_ALTA'
      IMPORTING
        ev_partner = lv_partner
      TABLES
        et_return  = rt_errores.

    READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.

*** Llamo al MF que crea los roles.
    CALL FUNCTION 'Z_EDU_CREATE_ROL'
      EXPORTING
        i_partner  = lv_partner
        i_tipo_rol = 'M' " MKK
      TABLES
        et_return  = rt_errores.

    READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.

*** Cuenta contrato Continua
    CALL FUNCTION 'Z_EDU_CREATE_CC'
      EXPORTING
        iv_partner      = lv_partner
        iv_acct_cat     = 'CO'
        iv_psobjecttype = 'CO01'
      TABLES
        et_return       = rt_errores
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.

*** Cuenta contrato Servicios
    CALL FUNCTION 'Z_EDU_CREATE_CC'
      EXPORTING
        iv_partner      = lv_partner
        iv_acct_cat     = 'SE'
        iv_psobjecttype = 'SE01'
      TABLES
        et_return       = rt_errores
      EXCEPTIONS
        error           = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
      IF sy-subrc IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD save_modif_from_mem.

    TYPES: BEGIN OF ty_dpsob_bp_acc,
             partneracctyp TYPE dpsob_bp_acc-partneracctyp,
           END OF ty_dpsob_bp_acc.

    DATA: lt_dpsob_bp_acc TYPE TABLE OF ty_dpsob_bp_acc,
          ls_dpsob_bp_acc TYPE ty_dpsob_bp_acc,
          ls_errores      TYPE bapiret2.


    CALL FUNCTION 'Z_EDU_SAVE_MODIF'
      TABLES
        et_return = rt_errores.

    READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.

*** Si no se crearon las cuentas contrato, se crean!
    SELECT partneracctyp
      FROM dpsob_bp_acc
      INTO TABLE lt_dpsob_bp_acc
      WHERE partner = iv_partner.

    READ TABLE lt_dpsob_bp_acc INTO ls_dpsob_bp_acc WITH KEY partneracctyp = 'CO'.
    IF sy-subrc IS NOT INITIAL.
*** Cuenta contrato Continua
      CALL FUNCTION 'Z_EDU_CREATE_CC'
        EXPORTING
          iv_partner      = iv_partner
          iv_acct_cat     = 'CO'
          iv_psobjecttype = 'CO01'
        TABLES
          et_return       = rt_errores
        EXCEPTIONS
          error           = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
        IF sy-subrc IS INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_dpsob_bp_acc INTO ls_dpsob_bp_acc WITH KEY partneracctyp = 'SE'.
    IF sy-subrc IS NOT INITIAL.
*** Cuenta contrato Servicios
      CALL FUNCTION 'Z_EDU_CREATE_CC'
        EXPORTING
          iv_partner      = iv_partner
          iv_acct_cat     = 'SE'
          iv_psobjecttype = 'SE01'
        TABLES
          et_return       = rt_errores
        EXCEPTIONS
          error           = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        READ TABLE rt_errores INTO ls_errores WITH KEY type = 'E'.
        IF sy-subrc IS INITIAL.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_from_mem.

    CALL FUNCTION 'Z_EDU_SET_DATA_TRM'
      EXPORTING
        is_datos_personales = is_datos_personales.

  ENDMETHOD.


  METHOD valida_existe_bp.
    DATA: lv_name1 TYPE bu_mcname1,
          lv_name2 TYPE bu_mcname2.

    SELECT SINGLE partner
      FROM but0id
      INTO ev_bp
      WHERE type     = iv_tipo_doc
        AND idnumber = iv_num_doc
        AND valid_date_from <= sy-datum
        AND valid_date_to >= sy-datum.

    SELECT SINGLE type mc_name1 mc_name2
     FROM but000
     INTO (ev_type,lv_name1,lv_name2)
    WHERE partner = ev_bp.

    CONCATENATE lv_name1 lv_name2 INTO ev_name SEPARATED BY space.

  ENDMETHOD.
ENDCLASS.
