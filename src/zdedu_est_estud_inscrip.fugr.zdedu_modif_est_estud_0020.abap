FUNCTION ZDEDU_MODIF_EST_ESTUD_0020.
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

  TYPES: BEGIN OF t_dfkkop,
           opbel TYPE dfkkop-opbel,
           betrw TYPE dfkkop-betrw,
           xblnr TYPE dfkkop-xblnr,
         END OF t_dfkkop,
         BEGIN OF t_dfkkko,
           opbel TYPE dfkkko-opbel,
           cpudt TYPE dfkkko-cpudt,
           cputm TYPE dfkkko-cputm,
         END OF t_dfkkko.

  DATA: lt_dfkkko        TYPE TABLE OF t_dfkkko,
        ls_dfkkko        TYPE t_dfkkko,
        ls_fkkcl         TYPE fkkcl,
        lt_pre_admi_2    TYPE TABLE OF zpre_admi_2,
        lt_dfkkop        TYPE TABLE OF t_dfkkop,
        ls_dfkkop        TYPE t_dfkkop,
        lv_nr_formulario TYPE zpre_admi_2-nr_formulario.

  FIELD-SYMBOLS <l_pre_admi_2> TYPE zpre_admi_2.

*  IF sy-tcode EQ 'FP05' OR
*     sy-tcode EQ 'FP05CLE' OR
*     sy-tcode EQ 'FP06' OR
*     sy-tcode EQ 'FPCJ'.
  CHECK sy-tcode <> 'FP06'. "Evitar ejecución en compensación manunal
  IF t_fkkcl[] IS NOT INITIAL.

    SELECT *
      FROM zpre_admi_2
      INTO TABLE lt_pre_admi_2
      FOR ALL ENTRIES IN t_fkkcl
    WHERE nr_formulario = t_fkkcl-xblnr+6.

    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ELSE.
      SORT lt_pre_admi_2.
    ENDIF.

    SELECT opbel cpudt cputm
        INTO TABLE lt_dfkkko
        FROM dfkkko
        FOR ALL ENTRIES IN t_fkkcl
        WHERE opbel EQ t_fkkcl-opbel.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ELSE.
      SORT lt_dfkkko.
    ENDIF.

    SELECT opbel betrw xblnr
      INTO TABLE lt_dfkkop
      FROM dfkkop
      FOR ALL ENTRIES IN t_fkkcl
      WHERE opbel EQ t_fkkcl-opbel AND
            betrw > 0.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ELSE.
      SORT lt_dfkkop.
    ENDIF.

    LOOP AT t_fkkcl INTO ls_fkkcl WHERE ( psobtyp EQ 'A008' OR
                                          psobtyp EQ 'A007' ) AND
                                          stakz   EQ 'G'.

      READ TABLE lt_dfkkko INTO ls_dfkkko
        WITH KEY opbel = ls_fkkcl-opbel
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_dfkkop INTO ls_dfkkop
          WITH KEY opbel = ls_fkkcl-opbel
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = ls_fkkcl-xblnr
            IMPORTING
              output = lv_nr_formulario.

          READ TABLE lt_pre_admi_2 ASSIGNING <l_pre_admi_2>
            WITH KEY nr_formulario = lv_nr_formulario
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <l_pre_admi_2>-fecha_pago =  ls_dfkkko-cpudt.
            <l_pre_admi_2>-hora_pago =  ls_dfkkko-cputm.
            <l_pre_admi_2>-nr_comproban =  ls_dfkkop-xblnr.
            <l_pre_admi_2>-valor_pagado =  ls_dfkkop-betrw.
            <l_pre_admi_2>-sta2_solpago = 'P'.
            <l_pre_admi_2>-fecha_pago = i_fkkko-cpudt.
            <l_pre_admi_2>-hora_pago = i_fkkko-cputm.
            <l_pre_admi_2>-nr_comproban = i_fkkko-opbel.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    UPDATE zpre_admi_2 FROM TABLE lt_pre_admi_2.
    IF sy-subrc IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.


ENDFUNCTION.
