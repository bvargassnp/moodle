FUNCTION z_edu_create_cc.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(IV_ACCT_CAT) TYPE  VKTYP_KK
*"     REFERENCE(IV_PSOBJECTTYPE) TYPE  PSOBTYP_PS
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
  DATA:
    ls_return        TYPE bapiret2,
    ls_cc_info       TYPE bapifkkvkci,
    ls_cc_detail     TYPE bapifkkvki,
    lt_partnerdetail TYPE TABLE OF bapifkkvkpi1,
    ls_partnerdetail TYPE bapifkkvkpi1,
    lv_cont_acct     TYPE bapifkkvk-cont_acct.
    "ls_deudor        TYPE zedu_s_datos_deudor_det. "DCEK902844


  ls_partnerdetail-buspartner =
  ls_cc_info-buspartner = iv_partner.
  ls_cc_info-acct_cat = iv_acct_cat. "'CO'.

  APPEND ls_partnerdetail TO lt_partnerdetail.

  CALL FUNCTION 'BAPI_CTRACCOUNT_EASYCREATE'
    EXPORTING
      ctraccreateinfo    = ls_cc_info
      ctracdetail        = ls_cc_detail
    IMPORTING
      contractaccount    = lv_cont_acct
    TABLES
      ctracpartnerdetail = lt_partnerdetail
      return             = et_return.

  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    RAISE error.
  ELSE.
    CALL FUNCTION 'BAPI_CTRACPSOBJECT_EASYCREATE'
      EXPORTING
        psobjecttype = iv_psobjecttype "'CO01'
        partner      = iv_partner
        ctraccount   = lv_cont_acct
      TABLES
        return       = et_return.
    READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      RAISE error.
    ENDIF.
  ENDIF.


ENDFUNCTION.
