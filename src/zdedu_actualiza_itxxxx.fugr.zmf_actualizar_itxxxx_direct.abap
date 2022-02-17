FUNCTION ZMF_ACTUALIZAR_ITXXXX_DIRECT.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IS_XXXX) TYPE  ANY
*"     VALUE(I_OBJID) TYPE  HROBJID OPTIONAL
*"     VALUE(I_INFOTYPE) TYPE  I
*"     VALUE(IS_KEY) TYPE  HRIPKEY OPTIONAL
*"     VALUE(I_OTYPE) TYPE  OTYPE
*"     VALUE(I_VTASK) TYPE  VTASK DEFAULT 'D'
*"  EXCEPTIONS
*"      ERROR
*"--------------------------------------------------------------------

  DATA: lt_nnnn TYPE TABLE OF hripkey,
        ls_nnnn TYPE hripkey.

  ASSIGN is_xxxx TO <text> CASTING.
  IF is_key IS INITIAL.
    ls_nnnn-plvar = '01'.
    ls_nnnn-otype = i_otype.
    ls_nnnn-objid = i_objid.
    ls_nnnn-infty = i_infotype.
    ls_nnnn-istat = '1'.
    ls_nnnn-begda = sy-datum.
    ls_nnnn-endda = '99991231'.
    ls_nnnn-varyf = ''.
  ELSE.
    ls_nnnn = is_key.
  ENDIF.
  APPEND ls_nnnn TO lt_nnnn.


  CALL FUNCTION 'RH_INSERT_INFTY_DIRECT'
    EXPORTING
      vtask               = i_vtask
      repid               = sy-repid
      form                = 'F_SET_DATA_XXXX'
    TABLES
      innnn               = lt_nnnn
    EXCEPTIONS
      no_authorization    = 1
      error_during_insert = 2
      repid_form_initial  = 3
      corr_exit           = 4
      begda_greater_endda = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
    RAISE error.
  ENDIF.


ENDFUNCTION.
