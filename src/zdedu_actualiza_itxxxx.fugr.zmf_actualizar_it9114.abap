function zmf_actualizar_it9114.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IS_9114) TYPE  PT9114
*"     VALUE(I_OBJID) TYPE  HROBJID OPTIONAL
*"     VALUE(I_INFOTYPE) TYPE  I
*"     VALUE(IS_KEY) TYPE  HRIPKEY OPTIONAL
*"     VALUE(I_OTYPE) TYPE  OTYPE
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------

  data: lt_nnnn type table of hripkey,
        ls_nnnn type hripkey.

  assign is_9114 to <text> casting.

  if is_key is initial.

    select  mandt
            plvar
            otype
            objid
            infty
            subty
            istat
*              priox
            begda
            endda
            varyf
            seqnr
      from hrp9114
        into corresponding fields of table lt_nnnn
          where plvar eq `01`
            and otype eq i_otype
            and objid eq i_objid
            and istat eq `1`
            and endda eq '99991231'.

    check sy-subrc eq 0.

  else.

    ls_nnnn = is_key.
    append ls_nnnn to lt_nnnn.

  endif.

  call function 'RH_UPDATE_INFTY'
    exporting
      vtask               = `D`
*     ORDER_FLG           = 'X'
      commit_flg          = space   "Flag commit en blanco por conflicto de evento fica
      repid               = sy-repid
      form                = 'F_SET_DATA_XXXX'
*     KEEP_LUPD           =
    tables
      innnn               = lt_nnnn
    exceptions
      error_during_update = 1
      no_authorization    = 2
      repid_form_initial  = 3
      corr_exit           = 4
      others              = 5.

  if sy-subrc is not initial.
    raise error.
  endif.

endfunction.
