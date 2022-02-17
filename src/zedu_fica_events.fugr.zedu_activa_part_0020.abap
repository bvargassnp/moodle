FUNCTION zedu_activa_part_0020.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_FKKKO) LIKE  FKKKO STRUCTURE  FKKKO
*"     VALUE(I_FKKKO_REV) LIKE  FKKKO STRUCTURE  FKKKO
*"     VALUE(I_AUGRD) LIKE  FKKOP-AUGRD
*"     VALUE(I_AUGVD) TYPE  AUGVD_KK OPTIONAL
*"     VALUE(I_PROCESS) TYPE  C OPTIONAL
*"     VALUE(I_PARTIAL_RESET) LIKE  BOOLE-BOOLE
*"     VALUE(I_ADD_DOC) LIKE  BOOLE-BOOLE OPTIONAL
*"     VALUE(I_LAST_OP_FROM_CALLER) TYPE  OPUPK_KK
*"     VALUE(I_LAST_OPK_FROM_CALLER) TYPE  OPUPK_KK
*"     VALUE(I_PYLOT_ADJUSTED) TYPE  FKKZP_KEY OPTIONAL
*"  TABLES
*"      T_FKKOP STRUCTURE  FKKOP
*"      T_FKKOPK STRUCTURE  FKKOPK
*"      T_FKKCL STRUCTURE  FKKCL
*"      T_FKKRAP STRUCTURE  DFKKRAPT
*"      T_FKKRAP_DETAIL STRUCTURE  FKKOP
*"      T_FKKOP_DP STRUCTURE  DFKKOP_DP OPTIONAL
*"----------------------------------------------------------------------
  DATA: ls_fkkcl_aux TYPE fkkcl.

  CONSTANTS: lc_augrd_05 TYPE fkkop-augrd VALUE '05',
             lc_augrd_04 TYPE fkkop-augrd VALUE '04',
             lc_augrd_14 TYPE fkkop-augrd VALUE '14'.

  "Continua solo si se tienen posiciones a compensar
  CHECK NOT t_fkkcl IS INITIAL.

  "Obtiene la primer posicion
  READ TABLE t_fkkcl INTO ls_fkkcl_aux INDEX 1.

*  CHECK i_augrd <> lc_augrd_05. "Salir si la compensación es por anulación

  "Si la compensación es por:
  IF ls_fkkcl_aux-augrd EQ lc_augrd_05 OR  "Anulacion
     ls_fkkcl_aux-augrd EQ lc_augrd_04 OR  "Cancelacion Individual
     ls_fkkcl_aux-augrd EQ lc_augrd_14.    "Cancelacion Masiva

    "Salir del evento
    EXIT.
  ENDIF.

  "Si no tiene compensacion y el evento es llamado por alguno de estos programas
  IF ls_fkkcl_aux-augrd IS INITIAL AND
    ( sy-cprog EQ 'ZDREDU_ANULAR_FACT_EVENTOS' OR
      sy-cprog EQ 'ZDRHCM_VERIF_PAGO'          OR
      sy-cprog EQ 'ZDRHCM_VERIF_PAGO_JOB' ).

    "Salir del evento
    EXIT.
  ENDIF.

  TYPES: BEGIN OF lty_event,
           event    TYPE t77refdoc-event,
           status   TYPE t77refdoc-status,
           document TYPE t77refdoc-document,
           plvar    TYPE t77refdoc-plvar,
           eotyp    TYPE t77refdoc-eotyp,
           eveid    TYPE t77refdoc-eveid,
           otype    TYPE t77refdoc-otype,
           sobid    TYPE t77refdoc-sobid,
           seqnr    TYPE t77refdoc-seqnr,
         END OF lty_event.

  TYPES: BEGIN OF lty_hrp9114.
           INCLUDE TYPE hripkey.
           TYPES: tabnr TYPE hrp9114-tabnr,
         END OF lty_hrp9114.

  TYPES:  BEGIN OF lty_pt9114.
            INCLUDE TYPE pt9114.
            TYPES:  tabnr TYPE hrp9114-tabnr,
          END OF lty_pt9114.

  DATA ls_hrp9114 TYPE lty_hrp9114.
  DATA lt_hrp9114 TYPE STANDARD TABLE OF lty_hrp9114 WITH KEY tabnr.
  DATA ls_iskey   TYPE hripkey.

  DATA ls_doc_ref TYPE lty_event.
  DATA ls_fkkcl TYPE fkkcl.

  DATA lv_objid TYPE hrobjid.

  DATA lt_pt9114 TYPE STANDARD TABLE OF lty_pt9114 WITH KEY evtid.
  DATA ls_pt9114 TYPE lty_pt9114.

  DATA ls_9114 TYPE pt9114.

*	Begin	-->	MgM DCEK901959 Marcar eventos Sec. también 16/12/2016
  DATA lr_eventos TYPE RANGE OF evtid.
*	End	  -->	MgM DCEK901959

  "Inicio lineas nuevas 26.11.2021 16084
  DATA: lr_blart TYPE RANGE OF blart_kk,
        ls_blart LIKE LINE OF lr_blart,
        lt_param TYPE STANDARD TABLE OF zedu_c_param,
        ls_param TYPE zedu_c_param,
        lt_mood  TYPE STANDARD TABLE OF zedu_int_moodle,
        ls_mood  TYPE zedu_int_moodle,
        lv_pag   TYPE betrw_kk,
        lv_tipop TYPE char2,
        lv_educ  TYPE c.    "Marca edudacion continua
  "Fin lineas nuevas 26.11.2021 16084

  SORT t_fkkcl BY blart
                  vktyp_ps.

  DELETE t_fkkcl WHERE xaktp <> 'X'.
  READ TABLE t_fkkcl
    INTO ls_fkkcl
      WITH KEY  blart     = 'FC'  "facturación continua
                vktyp_ps  = 'CO'  "tipo de cuenta educación continua
        BINARY SEARCH.

  CHECK sy-subrc EQ 0.
  CHECK ls_fkkcl-xblnr IS NOT INITIAL.

  "recuperamos el documento referenciado
  SELECT SINGLE event
                status
                document
                plvar
                eotyp
                eveid
                otype
                sobid
                seqnr
    INTO ls_doc_ref
      FROM t77refdoc
        WHERE event     = `F`  "Facturación
          AND document  = ls_fkkcl-xblnr+6(10).

  CHECK sy-subrc EQ 0.

  "recuperamos las referencias válidas
  SELECT  mandt                                         "#EC CI_NOFIRST
          plvar
          otype
          objid
          infty
          subty
          istat
*          priox
          begda
          endda
          varyf
          seqnr
          tabnr
    FROM hrp9114
      INTO CORRESPONDING FIELDS OF TABLE lt_hrp9114
      WHERE objid EQ ls_doc_ref-sobid
        AND otype EQ ls_doc_ref-otype
        AND begda LE sy-datum
        AND endda GT sy-datum.

  CHECK sy-subrc EQ 0.

  SORT lt_hrp9114 BY tabnr.

*	Begin	-->	MgM DCEK901959 Marcar eventos Sec. también 16/12/2016

  "Recuperamos evento principal y secundarios
  lr_eventos  = zedu_cl_help=>get_eventos( ls_doc_ref-eveid ). " Evento
*	End	  -->	MgM DCEK901959

  "recuperamos datos del evento referenciado
  SELECT evtid
         xfeld
         endda
         end_time
         evdat
         valorpend
         stext
         tabnr
    FROM hrt9114
      INTO TABLE lt_pt9114
        FOR ALL ENTRIES IN lt_hrp9114
          WHERE tabnr EQ lt_hrp9114-tabnr
*	Begin	-->	MgM DCEK901959 Marcar eventos Sec. también 16/12/2016
*            and evtid eq ls_doc_ref-eveid.
            AND evtid IN lr_eventos.
*	End	  -->	MgM DCEK901959

  IF sy-subrc EQ 0.

    SORT lt_pt9114 BY evtid.

*	Begin	-->	MgM DCEK901959 Marcar eventos Sec. también 16/12/2016
*    read table lt_pt9114
*      into ls_pt9114
*        with key evtid = ls_doc_ref-eveid binary search.
*
*    if sy-subrc eq 0.
    LOOP AT lt_pt9114
      INTO ls_pt9114.
*	End	  -->	MgM DCEK901959

      "movemos datos a estructura sin tabnr para MF Z
      MOVE ls_pt9114 TO ls_9114.

      "recuperamos estructura de la HRP9114 para el evento
      READ TABLE lt_hrp9114
        INTO ls_hrp9114
          WITH KEY tabnr = ls_pt9114-tabnr
            BINARY SEARCH.

      IF sy-subrc EQ 0.

        MOVE ls_hrp9114 TO ls_iskey.

        ls_iskey-begda  = sy-datum.

      ENDIF.

*    endif. "-->  MgM DCEK901959

      lv_objid  = ls_doc_ref-sobid.

      "Seteamos marca en estructura
      ls_9114-xfeld = cl_bp_const=>true.

      CALL FUNCTION 'ZMF_ACTUALIZAR_ITXXXX'
        EXPORTING
          is_xxxx       = ls_9114
          i_objid       = lv_objid
          i_infotype    = 9114
          is_key        = ls_iskey
          i_otype       = ls_doc_ref-otype
*         I_VTASK       = 'D'
          i_fcode       = 'INSE'
          iv_commit_flg = space
        EXCEPTIONS
          error         = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDLOOP.

  ENDIF.

  "Inicio lineas nuevas 26.11.2021 16084
  SELECT * FROM zedu_c_param INTO TABLE lt_param
    WHERE repid = 'ZEDU_ACTIVA_PART_0020'.

  LOOP AT lt_param INTO ls_param WHERE idparam = 'BLART'.
    ls_blart = 'IEQ'.
    ls_blart-low = ls_param-valor.
    APPEND ls_blart TO lr_blart.
  ENDLOOP.

  CLEAR: lv_pag, lv_educ.
  LOOP AT t_fkkcl INTO DATA(ls_fkkcl2) WHERE blart IN lr_blart.
    lv_educ = abap_true.
    lv_pag = lv_pag + ls_fkkcl2-betrw.

    CASE ls_fkkcl2-augrd.
      WHEN '01'.
        lv_tipop = i_fkkko-blart.
      WHEN '03'.
        lv_tipop = 'FN'. "financiación/acuerdo de pago
      WHEN '08'.
        LOOP AT t_fkkcl INTO DATA(ls_fkkcl3) WHERE blart NOT IN lr_blart.
          lv_tipop = ls_fkkcl3-blart.
        ENDLOOP.
        IF sy-subrc <> 0.
          lv_tipop = 'D1'. "Descuento 100%
        ENDIF.
    ENDCASE.

  ENDLOOP.
  "solo se inserta registro si se encontraron registros en el paso anterior
  IF lv_educ = abap_true.
    ls_mood-gpart     = ls_fkkcl2-gpart.
    ls_mood-evtid     = ls_doc_ref-eveid.
    ls_mood-opbel     = i_fkkko-opbel.
    ls_mood-tipo_pago = lv_tipop.
    ls_mood-betrw     = lv_pag.
    ls_mood-budat     = i_fkkko-budat.
    ls_mood-xblnr     = ls_fkkcl2-xblnr.
    ls_mood-waers     = ls_fkkcl2-waers.
    ls_mood-estado    = 'NP'.
    MODIFY zedu_int_moodle FROM ls_mood.
  ENDIF.
  "Fin lineas nuevas 26.11.2021 16084

ENDFUNCTION.                                             "#EC CI_VALPAR
