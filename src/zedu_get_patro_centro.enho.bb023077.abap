"Name: \FU:CMAC_OLD_REAL_DOC_READ\SE:END\EI
ENHANCEMENT 0 ZEDU_GET_PATRO_CENTRO.
  DATA lv_centro TYPE prctr.
  READ TABLE gt_dfkkko INDEX 1.

  SELECT SINGLE prctr
    FROM dfkkop
    INTO lv_centro
  WHERE opbel EQ gt_dfkkko-opbel.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'ZEDU_MF_SET_PATRO_CENTRO'
      EXPORTING
        iv_centro       = lv_centro.
  ENDIF.

*&LAT 02-11-2016 001: Inicio. Revisar Clínicos Odontológicos.
DATA: lr_fact_key   type zedu_fact_key_sc.        "Registro de control de trabajo facturación.
"      lt_fac_anual  TYPE table of zedu_fac_anual, "Reg. zedu_fac_anual
"      lr_fac_anual  TYPE zedu_fac_anual.          "Reg. zedu_fac_anual

CONSTANTS: lc_tx1 type Tcode value 'PQ_GRANT_CALC',           "Tx: Subvenciones
           lc_tipo_reg_02   TYPE zedu_tipo_regfac VALUE '02'. "Tipos de planes clínicos odontológicos

   if sy-tcode <> lc_tx1.
      CALL FUNCTION 'Z_EDU_GET_FACT_KEY_SC'
         IMPORTING
           E_FACT_KEY_SC       = lr_fact_key.

*      SELECT * FROM zedu_fac_anual INTO TABLE lt_fac_anual
*           WHERE tipo_reg = lc_tipo_reg_02
*             AND objid_sc = lr_fact_key-OBJID_SC
*             and persl    = IS_FEE_CTRL-persl.
*
*      sort lt_fac_anual by begda_fica DESCENDING endda_fica DESCENDING.
*      READ TABLE lt_fac_anual into lr_fac_anual index 1.
*
*      if sy-subrc = 0 and lr_fac_anual-begda_fica <= sy-datum and lr_fac_anual-endda_fica >= sy-datum.
*         clear: et_ficadoc,
*                et_bp_items[],
*                et_gl_items[],
*                et_bp_items_m[],
*                et_gl_items_m[].
*      endif.
   endif.
*&LAT 02-11-2016 001: Fin. Revisar Clínicos Odontológicos.
ENDENHANCEMENT.
