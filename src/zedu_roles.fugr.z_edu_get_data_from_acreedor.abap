FUNCTION z_edu_get_data_from_acreedor .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BU_PARTNER
*"     REFERENCE(I_BUKRS) TYPE  BUKRS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_lfa1,
           lifnr TYPE lfa1-lifnr,
           lnrza TYPE lfa1-lnrza,
           stcd2 TYPE lfa1-stcd2,
         END OF ty_lfa1,

         BEGIN OF ty_lfb1,
           lifnr TYPE lfb1-lifnr,
           bukrs TYPE lfb1-bukrs,
           pernr TYPE lfb1-pernr,
           zuawa TYPE lfb1-zuawa,
           akont TYPE lfb1-akont,
           zwels TYPE lfb1-zwels,
           zahls TYPE lfb1-zahls,
           zterm TYPE lfb1-zterm,
           fdgrv TYPE lfb1-fdgrv,
           reprf TYPE lfb1-reprf,
           togru TYPE lfb1-togru,
         END OF ty_lfb1,

         BEGIN OF ty_lfbw,
           lifnr     TYPE lfbw-lifnr,
           bukrs     TYPE lfbw-bukrs,
           witht     TYPE lfbw-witht,
           wt_subjct TYPE lfbw-wt_subjct,
           qsrec     TYPE lfbw-qsrec,
           wt_wtstcd TYPE lfbw-wt_wtstcd,
           wt_withcd TYPE lfbw-wt_withcd,
         END OF ty_lfbw,

         BEGIN OF ty_lfm1,
           lifnr TYPE lfm1-lifnr,
           ekorg TYPE lfm1-ekorg,
           sperm TYPE lfm1-sperm,
           loevm TYPE lfm1-loevm,
           waers TYPE lfm1-waers,
           zterm TYPE lfm1-zterm,
           webre TYPE lfm1-webre,
           kalsk TYPE lfm1-kalsk,
           meprf TYPE lfm1-meprf,
           nrgew TYPE lfm1-nrgew,
         END OF ty_lfm1.


  DATA: ls_lfb1 TYPE ty_lfb1,
        ls_lfa1 TYPE ty_lfa1,
        lt_lfb1 TYPE TABLE OF ty_lfb1,
        lt_lfbw TYPE TABLE OF ty_lfbw,
        lt_lfm1 TYPE TABLE OF ty_lfm1.

  gs_acreedor-tipo_tramite = 'C'. " Crear.

  gs_acreedor-partner             = i_partner.
  gs_acreedor-datos_basicos-lifnr = i_partner.
  gs_acreedor-datos_basicos-bukrs = i_bukrs.


  SELECT SINGLE lifnr lnrza stcd2
    FROM lfa1
    INTO ls_lfa1
    WHERE lifnr = i_partner.

  CHECK sy-subrc IS INITIAL.

  gs_acreedor-datos_basicos-stcd2 = ls_lfa1-stcd2.
  gs_acreedor-datos_basicos-lnrza = ls_lfa1-lnrza.

  SELECT lifnr bukrs pernr zuawa akont zwels
         zahls zterm fdgrv reprf togru
    FROM lfb1
    INTO TABLE lt_lfb1
    WHERE lifnr = i_partner.
*      AND bukrs = i_bukrs.

  CHECK sy-subrc IS INITIAL.
  gs_acreedor-tipo_tramite = 'M'. " Modificar.
  gs_acreedor-partner             = i_partner.

*** Verifica que existe el rol pero no para la sociedad.
  CLEAR ls_lfb1.
  READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY bukrs = i_bukrs.
  IF sy-subrc IS NOT INITIAL.
    EXIT.
  ENDIF.


  gs_acreedor-datos_basicos-lifnr = ls_lfb1-lifnr.
  gs_acreedor-datos_basicos-bukrs = ls_lfb1-bukrs.
  gs_acreedor-datos_basicos-pernr = ls_lfb1-pernr.
  gs_acreedor-datos_basicos-zuawa = ls_lfb1-zuawa.
  gs_acreedor-datos_basicos-akont = ls_lfb1-akont.
  gs_acreedor-datos_basicos-zwels = ls_lfb1-zwels.
  gs_acreedor-datos_basicos-zahls = ls_lfb1-zahls.
  gs_acreedor-datos_basicos-zterm = ls_lfb1-zterm.
  gs_acreedor-datos_basicos-fdgrv = ls_lfb1-fdgrv.
  gs_acreedor-datos_basicos-reprf = ls_lfb1-reprf.
  gs_acreedor-datos_basicos-togru = ls_lfb1-togru.

  SELECT lifnr bukrs witht wt_subjct qsrec wt_wtstcd wt_withcd
    FROM lfbw
    INTO TABLE lt_lfbw
    WHERE lifnr = i_partner
      AND bukrs = i_bukrs.

  IF sy-subrc IS NOT INITIAL.
    CLEAR lt_lfbw.
  ENDIF.

  SELECT lifnr ekorg sperm loevm waers zterm webre kalsk meprf nrgew
    FROM lfm1
    INTO TABLE lt_lfm1
    WHERE lifnr = i_partner.

  IF sy-subrc IS NOT INITIAL.
    CLEAR lt_lfm1.
  ENDIF.

  gs_acreedor-datos_retencion[] = lt_lfbw[].
  gs_acreedor-datos_logistica[] = lt_lfm1[].

ENDFUNCTION.
