function z_edu_get_data_from_deudor .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"----------------------------------------------------------------------

  types: begin of ty_kna1,
           kunnr   type kna1-kunnr,
           knrza   type kna1-knrza.
*	Begin	-->	MgM DCEK906161 datos adicionales cliente 27/08/2017
  "datos adicionales
          include type zsd_kna1.
*	End	  -->	MgM DCEK906161
  types: end of ty_kna1,

         begin of ty_knb1,
           kunnr type knb1-kunnr,
           bukrs type knb1-bukrs,
           zuawa type knb1-zuawa,
           akont type knb1-akont,
           zterm type knb1-zterm,
           fdgrv type knb1-fdgrv,
           togru type knb1-togru,
         end of ty_knb1,

         begin of ty_knvv,
           vkorg type knvv-vkorg,
           vtweg type knvv-vtweg,
           spart type knvv-spart,
           kdgrp type knvv-kdgrp,
           bzirk type knvv-bzirk,
           konda type knvv-konda,
           pltyp type knvv-pltyp,
           awahr type knvv-awahr,
           antlf type knvv-antlf,
           waers type knvv-waers,
           ktgrd type knvv-ktgrd,
           zterm type knvv-zterm,
           vwerk type knvv-vwerk,
           vkgrp type knvv-vkgrp,
           vkbur type knvv-vkbur,
         end of ty_knvv.


*	Begin	-->	MgM DCEK902866 amplia campos Org.Vtas 18/01/2017
*  DATA: lt_knvv         TYPE TABLE OF ty_knvv,
  data: lt_knvv         type zedu_t_datos_deudor_det,
*	End	  -->	MgM DCEK902866
        lt_knb1         type table of ty_knb1,
        lr_nro_org_vtas type range of z_nro_org_vtas,
        ls_kna1         type ty_kna1,
        ls_knb1         type ty_knb1,
        lv_vkorg        type vkorg.


  gs_deudor-tipo_tramite = 'C'. " Crear.

  gs_deudor-partner             = i_partner.
  gs_deudor-datos_basicos-bukrs = i_bukrs.

  select single kunnr
                knrza
*	Begin	-->	MgM DCEK906161 datos adicionales cliente 27/08/2017
                zgbdat
                zsexo
                zcodtid
                zcodead
                zcodtip
                zcodzon
*	End	  -->	MgM DCEK906161
    from kna1
      into ls_kna1
        where kunnr = i_partner.

  gs_deudor-datos_basicos-knrza   = ls_kna1-knrza.
*	Begin	-->	MgM DCEK906161 datos adicionales cliente 27/08/2017
  gs_deudor-datos_basicos-zgbdat  = ls_kna1-zgbdat.
  gs_deudor-datos_basicos-zsexo   = ls_kna1-zsexo.
  gs_deudor-datos_basicos-zcodtid = ls_kna1-zcodtid.
  gs_deudor-datos_basicos-zcodead = ls_kna1-zcodead.
  gs_deudor-datos_basicos-zcodtip = ls_kna1-zcodtip.
  gs_deudor-datos_basicos-zcodzon = ls_kna1-zcodzon.
*	End	  -->	MgM DCEK906161

  select single vkorg
      from tvko
      into lv_vkorg
      where bukrs = i_bukrs.
  if sy-subrc is not initial.
    clear lv_vkorg.
  endif.

*	Begin	-->	MgM DCEK902866 amplia campos Org.Vtas 18/01/2017
*  select vkorg vtweg spart kdgrp bzirk konda pltyp awahr antlf waers ktgrd
*         zterm vwerk vkgrp vkbur
*    from knvv
*    into table lt_knvv
  select *
    from knvv
      into corresponding fields of table lt_knvv
*	End	  -->	MgM DCEK902866
      where kunnr = i_partner
        and vkorg = lv_vkorg.

  if lt_knvv is initial.
    select *
      from zedu_c_org_vtas
      into corresponding fields of table gs_deudor-datos_org_vtas
      where nro_org_vtas in lr_nro_org_vtas
        and vkorg = lv_vkorg.
  else.

*	Begin	-->	MgM DCEK906593 Setear todas las Org.Vtas por
                              "default" (tabla Z) 04/10/2017
*    gs_deudor-datos_org_vtas[] = lt_knvv[].

    "recupero org.vtas/canal/sector Default que no tenga el cliente
    select *
      from zedu_c_org_vtas
        into corresponding fields of table gs_deudor-datos_org_vtas
          for all entries in lt_knvv
            where nro_org_vtas in lr_nro_org_vtas
              and vkorg = lv_vkorg
              and vtweg ne lt_knvv-vtweg
              and spart ne lt_knvv-spart.

    if sy-subrc ne 0.
      clear gs_deudor-datos_org_vtas.
    endif.

    append lines of lt_knvv
      to gs_deudor-datos_org_vtas.
*	End	  -->	MgM DCEK906593

  endif.

  select kunnr bukrs zuawa akont zterm fdgrv togru
    from knb1
    into table lt_knb1
    where kunnr = i_partner.
*      AND bukrs = i_bukrs.

  gs_deudor-partner      = i_partner.

  check sy-subrc is initial.
  gs_deudor-tipo_tramite = 'M'. " Modificar.

*** Verifica que existe el rol pero no para la sociedad.
  clear ls_knb1.
  read table lt_knb1 into ls_knb1 with key bukrs = i_bukrs.
  if sy-subrc is not initial.
    exit.
  endif.

  gs_deudor-datos_basicos-bukrs = ls_knb1-bukrs.
  gs_deudor-datos_basicos-zuawa = ls_knb1-zuawa.
  gs_deudor-datos_basicos-akont = ls_knb1-akont.
  gs_deudor-datos_basicos-zterm = ls_knb1-zterm.
  gs_deudor-datos_basicos-fdgrv = ls_knb1-fdgrv.
  gs_deudor-datos_basicos-togru = ls_knb1-togru.

endfunction.
