function zmf_actualizar_itxxxx.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IS_XXXX) TYPE  ANY
*"     VALUE(I_OBJID) TYPE  HROBJID OPTIONAL
*"     VALUE(I_INFOTYPE) TYPE  I
*"     VALUE(IS_KEY) TYPE  HRIPKEY OPTIONAL
*"     VALUE(I_OTYPE) TYPE  OTYPE
*"     VALUE(I_VTASK) TYPE  VTASK DEFAULT 'D'
*"     VALUE(I_FCODE) TYPE  OKCODE DEFAULT 'INSE'
*"     VALUE(IV_COMMIT_FLG) TYPE  COMMIT_FLG DEFAULT `X`
*"     VALUE(IV_TABNR) TYPE  OLD_TABNR DEFAULT `X`
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  data: lt_nnnn type table of hripkey,
        ls_nnnn type hripkey.

  assign is_xxxx to <text> casting.
  if is_key is initial.
    ls_nnnn-plvar = '01'.
    ls_nnnn-otype = i_otype.
    ls_nnnn-objid = i_objid.
    ls_nnnn-infty = i_infotype.
    ls_nnnn-istat = '1'.
    ls_nnnn-begda = sy-datum.
    ls_nnnn-endda = '99991231'.
    ls_nnnn-varyf = ''.
  else.
    ls_nnnn = is_key.
  endif.
  append ls_nnnn to lt_nnnn.

  call function 'RH_INSERT_INFTY'
    exporting
      fcode               = i_fcode
      vtask               = i_vtask
      commit_flg          = iv_commit_flg
      repid               = sy-repid
      form                = 'F_SET_DATA_XXXX'
    tables
      innnn               = lt_nnnn
    exceptions
      no_authorization    = 1
      error_during_insert = 2
      repid_form_initial  = 3
      corr_exit           = 4
      begda_greater_endda = 5
      others              = 6.

  if sy-subrc <> 0.
    raise error.
  endif.


endfunction.
