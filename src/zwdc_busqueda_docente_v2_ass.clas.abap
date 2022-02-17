class ZWDC_BUSQUEDA_DOCENTE_V2_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  constants C_TIPO_ACT type STRING value 'TIPO_ACT' ##NO_TEXT.
  constants C_TIPO_SAC type STRING value 'TIPO_SAC' ##NO_TEXT.
  constants C_COD_INV type STRING value 'COD_INV' ##NO_TEXT.

  class-methods GET_DDBK_TIPO_DOC
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DOCENTES
    importing
      !IS_SELECCION type ZEDU_S_SEL_DOCENTE
    returning
      value(RT_DOCENTE) type ZEDUE122_2 .
  class-methods GET_DROPDOWN_KEY_DOCENTE
    importing
      !IM_FIELD type STRING
      !IM_VALUE type STRING optional
      !IM_VALUE_2 type STRING optional
      value(IM_NULL_VALUE) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  class-methods GET_DDBK_TIPO_DOC_2
    returning
      value(RT_ATTR_VALUE) type WDY_KEY_VALUE_TABLE .
  class-methods GET_DDBK_COD_ACT
    returning
      value(RT_ATTR_VALUE) type WDY_KEY_VALUE_TABLE .
  class-methods GET_DDBK_ANIO
    returning
      value(RT_ATTR_VALUE) type WDY_KEY_VALUE_TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZWDC_BUSQUEDA_DOCENTE_V2_ASS IMPLEMENTATION.


  METHOD GET_DDBK_ANIO.

    SELECT peryr AS key
           peryt AS value
     FROM t7piqyeart
       INTO TABLE rt_attr_value
         WHERE spras EQ sy-langu.

    IF sy-subrc EQ 0.
**      SORT rt_attr_value BY value DESCENDING.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DDBK_COD_ACT.

    SELECT cod_act            AS key
           descripcion_codact AS value
     FROM zedu_pd_activida
       INTO TABLE rt_attr_value.

    IF sy-subrc EQ 0.
**      SORT rt_attr_value BY key.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DDBK_TIPO_DOC.

    SELECT ictyp AS value
           ictxt AS text
     FROM t5r06
       INTO TABLE rt_attr_value
         WHERE sprsl = sy-langu
           AND molga = '38'.  " Colombia

      if sy-subrc eq 0.
        sort rt_attr_value by value.
      endif.

  ENDMETHOD.


  METHOD GET_DDBK_TIPO_DOC_2.

    SELECT ictyp AS key
           ictxt AS value
     FROM t5r06
       INTO TABLE rt_attr_value
         WHERE sprsl = sy-langu
           AND molga = '38'.  " Colombia

    IF sy-subrc EQ 0.
**      SORT rt_attr_value BY key.
    ENDIF.

  ENDMETHOD.


  METHOD GET_DOCENTES.

    DATA: lt_entrada   TYPE zedutt122_1,
          lt_salida    TYPE zedutt122_2,
          ls_entrada   TYPE zedue122_1,
          ls_salida    TYPE zedue122_2,
          lv_resultado TYPE vvmsgtxt.

    ls_entrada-peryr          = is_seleccion-anio.
****    ls_entrada-perid          = is_seleccion-periodo.   "HIRS
****    ls_entrada-objid          = is_seleccion-facultad.  "HIRS
    ls_entrada-ictyp          = is_seleccion-tipo_doc.
****    ls_entrada-identificacion = is_seleccion-nro_doc.   "HIRS

    APPEND ls_entrada TO lt_entrada.

    CALL FUNCTION 'Z_EDU_RFC_122_1'
      EXPORTING
        i_entrada   = lt_entrada
      IMPORTING
        e_salida    = lt_salida
        e_resultado = lv_resultado.

    READ TABLE lt_salida into rt_docente index 1.

  ENDMETHOD.


  METHOD GET_DROPDOWN_KEY_DOCENTE.

    DATA: ls_attr_value TYPE wdr_context_attr_value.

    CASE im_field.
      WHEN c_tipo_act. " Tipo de Actividad

        IF im_value IS NOT INITIAL.
          SELECT tipo_act        AS value
                 descripcion_act AS text
           FROM zedu_pd_tipoact
           INTO TABLE rt_attr_value
            WHERE cod_act = im_value.
        ELSE.
          SELECT tipo_act        AS value
                 descripcion_act AS text
           FROM zedu_pd_tipoact
           INTO TABLE rt_attr_value.
        ENDIF.

      WHEN c_tipo_sac. " Tipo de Subactividad

        IF im_value IS NOT INITIAL AND im_value_2 IS NOT INITIAL.
          SELECT tipo_sac        AS value
                 descripcion_act AS text
           FROM zedu_pd_tiposac
           INTO TABLE rt_attr_value
            WHERE cod_act  = im_value
              AND tipo_act = im_value_2.
        ELSE.
          SELECT tipo_sac        AS value
                 descripcion_act AS text
           FROM zedu_pd_tiposac
           INTO TABLE rt_attr_value.
        ENDIF.

      WHEN c_cod_inv. " Investiga

        SELECT cod_inv         AS value
               descripcion_inv AS text
         FROM zedu_pd_grupoin
         INTO TABLE rt_attr_value.

      WHEN OTHERS.

    ENDCASE.

    IF im_null_value EQ cl_bp_const=>true.

      "Agrego línea en blanco para poder seleccionar blanco
      APPEND INITIAL LINE TO rt_attr_value.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
