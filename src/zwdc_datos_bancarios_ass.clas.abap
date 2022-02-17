class ZWDC_DATOS_BANCARIOS_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  methods GET_FROM_MEM
    returning
      value(RT_DATOS_BANCARIOS) type ZEDU_T_DATOS_BANCARIOS .
  methods SET_FROM_MEM
    importing
      !IT_DATOS_BANCARIOS type ZEDU_T_DATOS_BANCARIOS .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_DATOS_BANCARIOS_ASS IMPLEMENTATION.


  method GET_FROM_MEM.

      CALL FUNCTION 'Z_EDU_GET_DATA_TRM'
        TABLES
          et_datos_bancarios = rt_datos_bancarios.

  endmethod.


  METHOD SET_FROM_MEM.

    CALL FUNCTION 'Z_EDU_SET_DATA_TRM'
      TABLES
        it_datos_bancarios = it_datos_bancarios.

  ENDMETHOD.
ENDCLASS.
