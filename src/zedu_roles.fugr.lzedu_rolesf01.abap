*----------------------------------------------------------------------*
***INCLUDE LZEDU_ROLESF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_modif_cliente using p_partner
                     changing ch_error.
*	Begin	-->	MgM DCEK902699 agrega y borra Org. Ventas 12/01/2017
  types lty_t_knvv_key_sorted type standard table of knvv
                   with non-unique sorted key key_sort components  vkorg
                                                                   vtweg
                                                                  spart.
*	End	  -->	MgM DCEK902699

  data: ls_kna1           type kna1,
        ls_knb1           type knb1,
        ls_datos_org_vtas type zedu_s_datos_deudor_det,
*	Begin	-->	MgM DCEK902699 agrega y borra Org. Ventas 12/01/2017
*        lt_knvv           type table of knvv,
        lt_knvv           type lty_t_knvv_key_sorted,
        lt_knvv_add       type lty_t_knvv_key_sorted,
*	End	  -->	MgM DCEK902699
        lt_knvv_del       type table of knvv,
        ls_knvv           type knvv.

  field-symbols: <fs_knvv> type knvv.

  select single *
    from kna1
    into ls_kna1
    where kunnr = p_partner.

  check ls_kna1 is not initial.

  ls_kna1-knrza = gs_deudor-datos_basicos-knrza.
*	Begin	-->	MgM DCEK906161 datos adicionales cliente 27/08/2017
  ls_kna1-zgbdat  = gs_deudor-datos_basicos-zgbdat.
  ls_kna1-zsexo   = gs_deudor-datos_basicos-zsexo.
  ls_kna1-zcodtid = gs_deudor-datos_basicos-zcodtid.
  ls_kna1-zcodead = gs_deudor-datos_basicos-zcodead.
  ls_kna1-zcodtip = gs_deudor-datos_basicos-zcodtip.
  ls_kna1-zcodzon = gs_deudor-datos_basicos-zcodzon.
*	End	  -->	MgM DCEK906161

  modify kna1 from ls_kna1.
  if sy-subrc is not initial.
    ch_error = 'X'.
    exit.
  endif.

  select single *
    from knb1
    into ls_knb1
    where kunnr = p_partner
      and bukrs = gs_deudor-datos_basicos-bukrs.

  ls_knb1-mandt = sy-mandt.
  ls_knb1-kunnr = p_partner.
  ls_knb1-bukrs = gs_deudor-datos_basicos-bukrs.
  ls_knb1-zuawa = gs_deudor-datos_basicos-zuawa.
  ls_knb1-akont = gs_deudor-datos_basicos-akont.
  ls_knb1-zterm = gs_deudor-datos_basicos-zterm.
  ls_knb1-fdgrv = gs_deudor-datos_basicos-fdgrv.
  ls_knb1-togru = gs_deudor-datos_basicos-togru.

  modify knb1 from ls_knb1.
  if sy-subrc is not initial.
    ch_error = 'X'.
    exit.
  endif.

  select *
    from knvv
    into table lt_knvv
    where kunnr = p_partner.

*	Begin	-->	MgM DCEK902699 agrega y borra Org. Ventas 12/01/2017
*	End	  -->	MgM DCEK902699
*  CHECK lt_knvv IS NOT INITIAL.
*
*  LOOP AT lt_knvv ASSIGNING <fs_knvv>.
*    CLEAR ls_datos_org_vtas.
*    READ TABLE gs_deudor-datos_org_vtas INTO ls_datos_org_vtas WITH KEY vkorg = <fs_knvv>-vkorg.
*    IF sy-subrc IS INITIAL.
*      MOVE-CORRESPONDING ls_datos_org_vtas TO <fs_knvv>.
*    ELSE.
*      APPEND <fs_knvv> TO lt_knvv_del.
*      DELETE TABLE lt_knvv FROM <fs_knvv>.
*    ENDIF.
*  ENDLOOP.

  if sy-subrc ne 0.
    clear lt_knvv[].
  endif.

  sort gs_deudor-datos_org_vtas by  vkorg
                                    vtweg
                                    spart.

*Se comenta la opcion de borrar areas de venta 09.05.2017
  "determinamos org.ventas a borrar
*  LOOP AT lt_knvv INTO ls_knvv.
*    "Si no existe la org. en las Actuales editadas
*    READ TABLE gs_deudor-datos_org_vtas
*      TRANSPORTING NO FIELDS
*        WITH KEY vkorg = ls_knvv-vkorg
*                 vtweg = ls_knvv-vtweg
*                 spart = ls_knvv-spart.
*
*    IF sy-subrc NE 0. "se eliminan
*      APPEND ls_knvv TO lt_knvv_del.
*      DELETE TABLE lt_knvv FROM ls_knvv.
*    ENDIF.
*
*  ENDLOOP.


  "determinamos org.ventas a Insertar
  "Recorremos las organizaciones Actuales editadas
  loop at gs_deudor-datos_org_vtas
    into ls_datos_org_vtas.
    "Si no existe la org. en las Org.ventas de BD
    read table lt_knvv
      assigning <fs_knvv>
        with table key key_sort
          components vkorg = ls_datos_org_vtas-vkorg
                     vtweg = ls_datos_org_vtas-vtweg
                     spart = ls_datos_org_vtas-spart.

    if sy-subrc ne 0. "se insertan
      move-corresponding ls_datos_org_vtas to ls_knvv.
    move ls_datos_org_vtas-zterm_2  to ls_knvv-zterm. "-->MgM DCEK902866
      ls_knvv-kunnr = p_partner.
      ls_knvv-mandt = sy-mandt.
      append ls_knvv to lt_knvv_add.
    else.
      move-corresponding ls_datos_org_vtas to <fs_knvv>.
  move ls_datos_org_vtas-zterm_2  to <fs_knvv>-zterm. "-->MgM DCEK902866
    endif.

  endloop.
*	End	  -->	MgM DCEK902699

  if lt_knvv_del is not initial.
    delete knvv from table lt_knvv_del.
    if sy-subrc is not initial.
      ch_error = 'X'.
      exit.
    endif.
  endif.


  if lt_knvv is not initial.
    modify knvv from table lt_knvv.
    if sy-subrc is not initial.
      ch_error = 'X'.
      exit.
    endif.
*	Begin	-->	MgM DCEK902699 agrega y borra Org. Ventas 12/01/2017
*  elseif lt_knvv is initial and gs_deudor-datos_org_vtas[] is not initial.
*    loop at gs_deudor-datos_org_vtas into ls_datos_org_vtas.
*      move-corresponding ls_datos_org_vtas to ls_knvv.
*      append ls_knvv to lt_knvv.
*    endloop.
*    modify knvv from table lt_knvv.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*	End	  -->	MgM DCEK902699
  endif.

*	Begin	-->	MgM DCEK902699 agrega y borra Org. Ventas 12/01/2017
  if lt_knvv_add is not initial.
    insert knvv from table lt_knvv_add accepting duplicate keys.
    if sy-subrc is not initial.
      ch_error = 'X'.
      exit.
    endif.
  endif.
*	End	  -->	MgM DCEK902699

***    CALL FUNCTION 'SD_CUSTOMER_MAINTAIN_ALL'
***      EXPORTING
***        i_knb1                  = ls_knb1
***        i_knvv                  = <fs_knvv>
***      EXCEPTIONS
***        client_error            = 1
***        kna1_incomplete         = 2
***        knb1_incomplete         = 3
***        knb5_incomplete         = 4
***        knvv_incomplete         = 5
***        kunnr_not_unique        = 6
***        sales_area_not_unique   = 7
***        sales_area_not_valid    = 8
***        insert_update_conflict  = 9
***        number_assignment_error = 10
***        number_not_in_range     = 11
***        number_range_not_extern = 12
***        number_range_not_intern = 13
***        account_group_not_valid = 14
***        parnr_invalid           = 15
***        bank_address_invalid    = 16
***        tax_data_not_valid      = 17
***        no_authority            = 18
***        company_code_not_unique = 19
***        dunning_data_not_valid  = 20
***        knb1_reference_invalid  = 21
***        cam_error               = 22
***        OTHERS                  = 23.
***    IF sy-subrc <> 0.
**** Implement suitable error handling here
***    ENDIF.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_PROVEEDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_p_partner  text
*----------------------------------------------------------------------*
form f_modif_proveedor  using  p_partner
                               p_bukrs
                        changing ch_error.

  data: ls_lfb1     type lfb1,
        ls_lfa1     type lfa1,
        lt_lfbw_mod type table of lfbw,
        lt_lfbw_del type table of lfbw,
        lt_lfm1_del type table of lfm1,
        lt_lfm1_mod type table of lfm1,
        ls_lfbw     type lfbw,
        ls_lfm1     type lfm1.
*        ls_datos_retencion type zedu_s_datos_acreedor_reten,
*        ls_datos_logistica type zedu_s_datos_acreedor_logisti.

*  field-symbols: <fs_lfbw> type lfbw,
*                 <fs_lfm1> type lfm1.


  select single *
    from lfa1
    into ls_lfa1
    where lifnr = p_partner.

  ls_lfa1-stcd2 = gs_acreedor-datos_basicos-stcd2.
  ls_lfa1-lnrza = gs_acreedor-datos_basicos-lnrza.

  modify lfa1 from ls_lfa1.
  if sy-subrc is not initial.
    ch_error = 'X'.
    exit.
  endif.

  select single *
    from lfb1
    into ls_lfb1
    where lifnr = p_partner
      and bukrs = p_bukrs.

  ls_lfb1-mandt = sy-mandt.
  ls_lfb1-lifnr = p_partner.
  ls_lfb1-bukrs = p_bukrs.
  ls_lfb1-pernr = gs_acreedor-datos_basicos-pernr.
  ls_lfb1-zuawa = gs_acreedor-datos_basicos-zuawa.
  ls_lfb1-akont = gs_acreedor-datos_basicos-akont.
  ls_lfb1-zwels = gs_acreedor-datos_basicos-zwels.
  ls_lfb1-zahls = gs_acreedor-datos_basicos-zahls.
  ls_lfb1-zterm = gs_acreedor-datos_basicos-zterm.
  ls_lfb1-fdgrv = gs_acreedor-datos_basicos-fdgrv.
  ls_lfb1-reprf = gs_acreedor-datos_basicos-reprf.
  ls_lfb1-togru = gs_acreedor-datos_basicos-togru.

  modify lfb1 from ls_lfb1.
  if sy-subrc is not initial.
    ch_error = 'X'.
    exit.
  endif.

*	Begin	-->	MgM DCEK902844 Actualización Retenciones 17/01/2017
*  select * "lifnr bukrs witht wt_subjct qsrec wt_wtstcd wt_withcd
*    from lfbw
*    into table lt_lfbw
*    where lifnr = p_partner
*      and bukrs = p_bukrs.
*
*  if lt_lfbw is initial.
*    clear lt_lfbw.
*  endif.
*
*  loop at lt_lfbw assigning <fs_lfbw>.
*    clear ls_datos_retencion.
*    read table gs_acreedor-datos_retencion into ls_datos_retencion with key witht = <fs_lfbw>-witht.
*    if sy-subrc is initial.
*      move-corresponding ls_datos_retencion to <fs_lfbw>.
*    else.
*      append <fs_lfbw> to lt_lfbw_del.
*      delete table lt_lfbw from <fs_lfbw>.
*    endif.
*  endloop.
*
*  if lt_lfbw_del is not initial.
*    delete lfbw from table lt_lfbw_del.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  endif.
*
*
*  if lt_lfbw is not initial.
*    modify lfbw from table lt_lfbw.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  elseif lt_lfbw is initial and gs_acreedor-datos_retencion[] is not initial.
*    loop at gs_acreedor-datos_retencion into ls_datos_retencion.
*      move-corresponding ls_datos_retencion to ls_lfbw.
*      append ls_lfbw to lt_lfbw.
*    endloop.
*    modify lfbw from table lt_lfbw.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  endif.
*
*  select *
*    from lfm1
*    into table lt_lfm1
*    where lifnr = p_partner.
*
*  if lt_lfm1 is initial.
*    clear lt_lfm1.
*  endif.
*
*  loop at lt_lfm1 assigning <fs_lfm1>.
*    clear ls_datos_logistica.
*    read table gs_acreedor-datos_logistica into ls_datos_logistica with key ekorg = <fs_lfm1>-ekorg.
*    if sy-subrc is initial.
*      move-corresponding ls_datos_logistica to <fs_lfm1>.
*    else.
*      append <fs_lfm1> to lt_lfm1_del.
*      delete table lt_lfm1 from <fs_lfm1>.
*    endif.
*  endloop.
*
*  if lt_lfm1_del is not initial.
*    delete lfm1 from table lt_lfm1_del.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  endif.
*
*
*  if lt_lfm1 is not initial.
*    modify lfm1 from table lt_lfm1.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  elseif lt_lfm1 is initial and gs_acreedor-datos_logistica[] is not initial.
*    loop at gs_acreedor-datos_logistica into ls_datos_logistica.
*      move-corresponding ls_datos_logistica to ls_lfm1.
*      append ls_lfm1 to lt_lfm1.
*    endloop.
*    modify lfm1 from table lt_lfm1.
*    if sy-subrc is not initial.
*      ch_error = 'X'.
*      exit.
*    endif.
*  endif.

*-->> Actualización Retenciones <<--*
  select *
    from lfbw
      into table @data(lt_lfbw_bd)
        where lifnr = @p_partner
          and bukrs = @p_bukrs
            order by primary key.

  if sy-subrc ne 0.
    clear lt_lfbw_bd[].
  endif.

  sort gs_acreedor-datos_retencion by lifnr
                                      bukrs
                                      witht.

  "1.determinamos retenciones a borrar
  loop at lt_lfbw_bd
    into data(ls_lfbw_bd).

    read table gs_acreedor-datos_retencion
      transporting no fields
        with key lifnr = p_partner
                 bukrs = p_bukrs
                 witht = ls_lfbw_bd-witht binary search.

    if sy-subrc ne 0.
      append ls_lfbw_bd to lt_lfbw_del.
    endif.

  endloop.

  "2.determinamos org.compras a insertar o modificar
  loop at gs_acreedor-datos_retencion
    into data(ls_datos_ret).

    read table lt_lfbw_bd
      into ls_lfbw_bd
        with key  lifnr = p_partner
                  bukrs = p_bukrs
                  witht = ls_datos_ret-witht binary search.

    if sy-subrc eq 0.
      "Si existía validamos si cambió
      move-corresponding ls_datos_ret to ls_lfbw.
      ls_lfbw-mandt = sy-mandt.
      ls_lfbw-lifnr = p_partner.
      ls_lfbw-bukrs = p_bukrs.

      if ls_lfbw ne ls_lfbw_bd.
        "Si cambió lo agregamos para actualizar
        append ls_lfbw to lt_lfbw_mod.
      endif.

    else.
      "Si no existía lo agregamos para insertar
      move-corresponding ls_datos_ret to ls_lfbw.
      ls_lfbw-mandt = sy-mandt.
      ls_lfbw-lifnr = p_partner.
      ls_lfbw-bukrs = p_bukrs.
      append ls_lfbw to lt_lfbw_mod.

    endif.

  endloop.

  if lt_lfbw_mod[] is not initial.
    modify lfbw from table lt_lfbw_mod.
    if sy-subrc is not initial.
      ch_error = 'X'.
    endif.
  endif.
  if lt_lfbw_del[] is not initial.
    delete lfbw from table lt_lfbw_del.
    if sy-subrc is not initial.
      ch_error = 'X'.
    endif.
  endif.

*-->> Actualización Organización Compra <<--*
  select *
    from lfm1
      into table @data(lt_lfm1_bd)
        where lifnr = @p_partner
          order by primary key.

  if sy-subrc ne 0.
    clear lt_lfm1_bd[].
  endif.

  sort gs_acreedor-datos_logistica by lifnr
                                      ekorg.

  "1.determinamos org.compras a borrar
  loop at lt_lfm1_bd
    into data(ls_lfm1_bd).

    read table gs_acreedor-datos_logistica
      transporting no fields
        with key lifnr = p_partner
                 ekorg = ls_lfm1_bd-ekorg binary search.

    if sy-subrc ne 0.
      append ls_lfm1_bd to lt_lfm1_del.
    endif.

  endloop.

  "2.determinamos org.compras a insertar o modificar
  loop at gs_acreedor-datos_logistica
    into data(ls_datos).

    read table lt_lfm1_bd
      into ls_lfm1_bd
        with key  lifnr = p_partner
                  ekorg = ls_datos-ekorg binary search.

    if sy-subrc eq 0.
      "Si existía validamos si cambió
      move-corresponding ls_datos to ls_lfm1.
      ls_lfm1-mandt = sy-mandt.
      ls_lfm1-lifnr = p_partner.
      ls_lfm1-erdat = ls_lfm1_bd-erdat.
      ls_lfm1-ernam = ls_lfm1_bd-ernam.

      if ls_lfm1 ne ls_lfm1_bd.
        "Si cambió lo agregamos para actualizar
        append ls_lfm1 to lt_lfm1_mod.
      endif.

    else.
      "Si no existía lo agregamos para insertar
      move-corresponding ls_datos to ls_lfm1.
      ls_lfm1-mandt = sy-mandt.
      ls_lfm1-lifnr = p_partner.
      append ls_lfm1 to lt_lfm1_mod.

    endif.

  endloop.

  if lt_lfm1_mod[] is not initial.
    modify lfm1 from table lt_lfm1_mod.
    if sy-subrc is not initial.
      ch_error = 'X'.
    endif.
  endif.
  if lt_lfm1_del[] is not initial.
    delete lfm1 from table lt_lfm1_del.
    if sy-subrc is not initial.
      ch_error = 'X'.
    endif.
  endif.
*	End	  -->	MgM DCEK902844

endform.
