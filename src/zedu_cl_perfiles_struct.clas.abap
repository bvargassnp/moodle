class ZEDU_CL_PERFILES_STRUCT definition
  public
  final
  create public .

public section.

  types:
    TY_T_AUTHORITY type standard table of hrms_bw_is_authority
    with DEFAULT KEY .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_RANGE
    importing
      !IV_OTYPE type OTYPE
    returning
      value(RR_RANGO) type FPB_T_COSEL2 .
protected section.
private section.

  class-data GR_AUTHORITY_O type FPB_T_COSEL2 .
  class-data GR_AUTHORITY_SC type FPB_T_COSEL2 .
  class-data GR_AUTHORITY_CS type FPB_T_COSEL2 .
  class-data GR_AUTHORITY_SM type FPB_T_COSEL2 .
  class-data GR_AUTHORITY_ST type FPB_T_COSEL2 .
  class-data GR_AUTHORITY_CA type FPB_T_COSEL2 .

  class-methods ARMA_RANGOS_AUTORIZADOS
    importing
      !IT_AUTORIZADOS type TY_T_AUTHORITY
      !IV_OTYPE type OTYPE .
ENDCLASS.



CLASS ZEDU_CL_PERFILES_STRUCT IMPLEMENTATION.


  method arma_rangos_autorizados.

    field-symbols <fs_range> type fpb_t_cosel2.

    concatenate `gr_authority_`
                iv_otype
      into data(lv_range_name).

    assign (lv_range_name) to <fs_range>.

    check sy-subrc eq 0.

    loop at it_autorizados
      into data(ls_autority)
        where otype eq iv_otype.

      if ls_autority-objid is initial.
        "tiene autorización a todas las O
        "se deja en blanco para que el rango tome todo
      else.
        append initial line to <fs_range> assigning field-symbol(<fs_auth>).

        if ls_autority-excluded eq cl_bp_const=>true.
          <fs_auth>-sign    = `E`.
        else.
          <fs_auth>-sign    = `I`.
        endif.

        <fs_auth>-option  = `EQ`.
        <fs_auth>-low     = ls_autority-objid.
      endif.

    endloop.

    if sy-subrc ne 0 and
       <fs_range>[] is initial.

      "Si no hay O se agrega una línea en rango para que no recupere ninguno
      append initial line to <fs_range> assigning <fs_auth>.
      <fs_auth>-sign    = `I`.
      <fs_auth>-option  = `EQ`.
    endif.

  endmethod.


  method class_constructor.

    data lt_authority type standard table of hrms_bw_is_authority with default key.

    "recupero objetos autorizados
    call function 'HR_BW_IS_AUTHORITY'
      exporting
        uname         = sy-uname
      tables
        e_t_authority = lt_authority.

    sort lt_authority by otype.

    "Descarto los no-validos a la fecha
    delete lt_authority where datefrom >  sy-datum or
                              dateto   <  sy-datum.

    loop at lt_authority
      into data(ls_authority).

      at end of otype.
        arma_rangos_autorizados(
          exporting
            it_autorizados = lt_authority         " BW: Fuente datos autorizaciones estructurales - Val.
            iv_otype       = ls_authority-otype )." Tp.objeto
      endat.

    endloop.

  endmethod.


  method get_range.

    field-symbols <fs_range> type fpb_t_cosel2.

    concatenate `gr_authority_`
                iv_otype
      into data(lv_range_name).

    assign (lv_range_name) to <fs_range>.

    check sy-subrc eq 0.

    rr_rango[] = <fs_range>[].

  endmethod.
ENDCLASS.
