class ZWDC_DATOS_DIRECCION_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  methods GET_FROM_MEM
    returning
      value(RT_DATOS_DIRECCION) type ZEDU_S_DIRECCION .
  methods SET_FROM_MEM
    importing
      !IS_DATOS_DIRECCION type ZEDU_S_DIRECCION .
  methods ARMAR_DIRECCION
    changing
      !CH_DATOS_DIRECCION type ZEDU_S_DIRECCION .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_DATOS_DIRECCION_ASS IMPLEMENTATION.


  METHOD armar_direccion.

    CALL FUNCTION 'Z_EDU_ARMAR_DIRECCION'
      CHANGING
        ch_direccion = ch_datos_direccion.

  ENDMETHOD.


  method GET_FROM_MEM.

      CALL FUNCTION 'Z_EDU_GET_DATA_TRM'
        IMPORTING
          es_datos_direccion = rt_datos_direccion.

  endmethod.


  METHOD SET_FROM_MEM.

    CALL FUNCTION 'Z_EDU_SET_DATA_TRM'
      EXPORTING
        is_datos_direccion = is_datos_direccion.

  ENDMETHOD.
ENDCLASS.
