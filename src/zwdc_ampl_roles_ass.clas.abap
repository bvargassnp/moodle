class ZWDC_AMPL_ROLES_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  constants C_CREAR type ZEDU_TIPO_TRAMITE value 'C' ##NO_TEXT.
  constants C_MODIFICAR type ZEDU_TIPO_TRAMITE value 'M' ##NO_TEXT.

  methods GET_DATOS_ACREEDOR
    changing
      !CH_DATOS_ACREEDOR type ZEDU_S_ACREEDOR_CVI .
  methods GET_DATOS_DEUDOR
    changing
      !CH_DATOS_DEUDOR type ZEDU_S_DEUDOR_CVI .
  methods SAVE_ACREEDOR
    importing
      !IS_DATOS_ACREEDOR type ZEDU_S_ACREEDOR_CVI
    returning
      value(RT_ERRORES) type BAPIRET2_T .
  methods SAVE_DEUDOR
    importing
      value(IS_DATOS_DEUDOR) type ZEDU_S_DEUDOR_CVI
    returning
      value(RT_ERRORES) type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_AMPL_ROLES_ASS IMPLEMENTATION.


  METHOD get_datos_acreedor.


    CALL FUNCTION 'Z_EDU_GET_DATA_FROM_ACREEDOR'
      EXPORTING
        i_partner = ch_datos_acreedor-partner
        i_bukrs   = ch_datos_acreedor-datos_basicos-bukrs.

    CALL FUNCTION 'Z_EDU_GET_ACREEDOR'
      IMPORTING
        es_acreedor = ch_datos_acreedor.


  ENDMETHOD.


  METHOD get_datos_deudor.


    CALL FUNCTION 'Z_EDU_GET_DATA_FROM_DEUDOR'
      EXPORTING
        i_partner = ch_datos_deudor-partner
        i_bukrs   = ch_datos_deudor-datos_basicos-bukrs.

    CALL FUNCTION 'Z_EDU_GET_DEUDOR'
      IMPORTING
        es_deudor = ch_datos_deudor.


  ENDMETHOD.


  METHOD save_acreedor.

*** Seteo los datos en memoria.
    CALL FUNCTION 'Z_EDU_SET_ACREEDOR'
      EXPORTING
        is_acreedor = is_datos_acreedor.

*** Llamo al MF que crea los roles.
    CALL FUNCTION 'Z_EDU_SAVE_ROL'
      EXPORTING
        i_partner  = is_datos_acreedor-partner
        i_bukrs    = is_datos_acreedor-datos_basicos-bukrs
        i_tipo_rol = 'P' " Proveedor / Acreedor
      TABLES
        et_return  = rt_errores.

  ENDMETHOD.


  METHOD save_deudor.

*** Seteo los datos en memoria.
    CALL FUNCTION 'Z_EDU_SET_DEUDOR'
      EXPORTING
        is_deudor = is_datos_deudor.

*** Llamo al MF que crea los roles.
    CALL FUNCTION 'Z_EDU_SAVE_ROL'
      EXPORTING
        i_partner  = is_datos_deudor-partner
        i_tipo_rol = 'C' " Cliente / deudor
      TABLES
        et_return  = rt_errores.

*	Begin	-->	MgM DCEK906161 Datos adic. nuevos campos 21/09/2017
    if is_datos_deudor-tipo_tramite eq c_crear.

      is_datos_deudor-tipo_tramite = c_modificar.

      "volvemos a llamar para modificar
      save_deudor( is_datos_deudor ).

    endif.
*	End	  -->	MgM DCEK906161

  ENDMETHOD.
ENDCLASS.
