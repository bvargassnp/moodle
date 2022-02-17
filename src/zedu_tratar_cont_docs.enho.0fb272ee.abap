"Name: \FU:CMAC_FEE_DOC_INIT\SE:BEGIN\EI
ENHANCEMENT 0 ZEDU_TRATAR_CONT_DOCS.
* data:
*    lv_fact_key_sc  TYPE zedu_fact_key_sc.            "Reg zedu_fac_key_sc
*
* CONSTANTS:
*    lc_persl_ofor      type PERSL_KK value 'FOTR',       "Persl formal otros conceptos
*    lc_linea_cont      type ZEDU_LINEA_E value 'C',      "Línea educativa cotninua.
*    lc_linea_form      type ZEDU_LINEA_E value 'F',      "Línea educativa cotninua.
*    lc_tipofac_o       type ZEDU_TIPOFAC value 'O'.      "Tipo de factura otros conceptos
*
* if lv_fact_key_sc-LINEA_EDU = lc_linea_form and lv_fact_key_sc-TIFA = lc_tipofac_o.
*    CALL FUNCTION 'Z_EDU_GET_FACT_KEY_SC'
*         IMPORTING
*           e_fact_key_sc = lv_fact_key_sc.
*
*    IS_FEE_CTRL-persl =  lc_persl_ofor .
*  endif.


ENDENHANCEMENT.
