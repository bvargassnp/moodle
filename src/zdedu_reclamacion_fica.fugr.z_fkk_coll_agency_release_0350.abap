FUNCTION z_fkk_coll_agency_release_0350.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_TFK047L) LIKE  TFK047L STRUCTURE  TFK047L
*"  EXPORTING
*"     VALUE(C_FKKMAKO) LIKE  FKKMAKO STRUCTURE  FKKMAKO
*"     VALUE(C_FKKKO) LIKE  FKKKO STRUCTURE  FKKKO
*"  TABLES
*"      T_FKKMAZE STRUCTURE  FKKMAZE
*"      T_FKKMAKT STRUCTURE  FKKMAKT
*"      T_FKKOP STRUCTURE  FKKOP
*"      T_FKKOPK STRUCTURE  FKKOPK
*"      T_FIMSG STRUCTURE  FIMSG
*"----------------------------------------------------------------------


  DATA: ht_fkkop               LIKE fkkop OCCURS 0 WITH HEADER LINE,
        wt_fkkop               LIKE fkkop OCCURS 0 WITH HEADER LINE,
        ht_fkkop_not_submitted LIKE fkkop OCCURS 0 WITH HEADER LINE,
        h_submitted(7)         TYPE n,
        h_not_submitted(7)     TYPE n,
        h_update,
        h_field(50),
        h_xsimu                LIKE boole-boole,
        ht_fkkcoll             LIKE dfkkcoll OCCURS 0 WITH HEADER LINE,
        ht_dfkkcoll            LIKE dfkkcoll OCCURS 0 WITH HEADER LINE.

  DATA:   lv_aggrd               TYPE aggrd_kk.

* ------ get update flag from global memory ---------------------------*
  CALL FUNCTION 'FKK_DUNNING_PARAMETER_GET'
    IMPORTING
      e_update = h_update.
  IF h_update IS INITIAL.
    h_xsimu         = const_marked.
  ELSE.
    CLEAR h_xsimu.
  ENDIF.
* ------ Release each open item for collection agency -------*
  LOOP AT t_fkkmaze.
    CHECK t_fkkmaze-xinfo IS INITIAL AND
          t_fkkmaze-dalin IS INITIAL.
    CLEAR ht_fkkop.
    CALL FUNCTION 'FKK_BP_LINE_ITEM_SELECT_SINGLE'
      EXPORTING
        i_opbel = t_fkkmaze-opbel
        i_opupw = t_fkkmaze-opupw
        i_opupk = t_fkkmaze-opupk
        i_opupz = t_fkkmaze-opupz
      IMPORTING
        e_fkkop = ht_fkkop.
    IF ht_fkkop IS INITIAL AND t_fkkmaze-opupw NE '000'.
      CALL FUNCTION 'FKK_BP_LINE_ITEMS_SEL_LOGICAL'
        EXPORTING
          i_opbel     = t_fkkmaze-opbel
        TABLES
          pt_logfkkop = wt_fkkop.
      READ TABLE wt_fkkop INTO ht_fkkop WITH KEY
                                    opbel = t_fkkmaze-opbel
                                    opupw = t_fkkmaze-opupw
                                    opupk = t_fkkmaze-opupk
                                    opupz = t_fkkmaze-opupz.
      REFRESH wt_fkkop.
    ENDIF.
    IF NOT ht_fkkop IS INITIAL.

      CALL FUNCTION 'FKK_COLLECT_AGENCY_ITEM_SELECT'
        EXPORTING
          i_opbel        = ht_fkkop-opbel
          ix_opbel       = const_marked
          i_inkps        = ht_fkkop-inkps
          ix_inkps       = const_marked
        TABLES
          t_fkkcoll      = ht_fkkcoll
        EXCEPTIONS
          initial_values = 1
          not_found      = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        READ TABLE ht_fkkcoll INDEX 1.
        IF sy-subrc = 0.
          CLEAR h_field.
          CONCATENATE ht_fkkop-opbel ht_fkkop-opupw ht_fkkop-opupk
                      ht_fkkop-opupz INTO h_field SEPARATED BY '/'.
          IF ht_fkkcoll-agsta GE 01.                      "Note 905458
*          IF ht_fkkcoll-agsta = const_agsta_freigegeben. "Note 905458
            PERFORM message_append TABLES t_fimsg USING  '>3' 'I'
                                  '492' h_field ht_fkkcoll-inkgp
                                   space space.
* ------ For cross reference purpose only -----------------------------*
            SET EXTENDED CHECK OFF.
            IF 0 = 1. MESSAGE i492(>3). ENDIF.
            SET EXTENDED CHECK ON.
* ------ End of cross reference ---------------------------------------*
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ht_fkkop.
    ELSE.
      PERFORM message_append TABLES t_fimsg USING  '>3' 'E' '456'
        t_fkkmaze-opbel t_fkkmaze-opupw t_fkkmaze-opupk t_fkkmaze-opupz.
* ------ for cross reference purpose only -----------------------------*
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e456(>3). ENDIF.
      SET EXTENDED CHECK ON.
* ------ end of cross reference ---------------------------------------*
    ENDIF.
  ENDLOOP.

  READ TABLE ht_fkkop INDEX 1.
  IF sy-subrc EQ 0.

    IF i_tfk047l-ackey = '0009'.
      lv_aggrd = 'PJ'.
    ELSEIF i_tfk047l-ackey = '0010'.
      lv_aggrd = 'JR'.
    ELSE.
      lv_aggrd = const_aggrd_dunning.
    ENDIF.
    gv_ackey = i_tfk047l-ackey.
    SORT ht_fkkop BY opbel opupw opupk opupz.
    CALL FUNCTION 'FKK_RELEASE_FOR_COLLECT_AGENCY'
      EXPORTING
        i_aggrd               = lv_aggrd
        i_xsimu               = h_xsimu
      TABLES
        t_fkkop               = ht_fkkop
        t_fkkop_not_submitted = ht_fkkop_not_submitted
        t_fimsg               = t_fimsg
        t_dfkkcoll            = ht_dfkkcoll
      EXCEPTIONS
        error                 = 1
        OTHERS                = 2.
    IF sy-subrc NE 0.
      PERFORM message_append TABLES t_fimsg USING  '>3' 'E' '513'
                 'FKK_RELEASE_FOR_COLLECT_AGENCY' space space space.
* ------ for cross reference purpose only -----------------------------*
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e513(>3). ENDIF.
      SET EXTENDED CHECK ON.
* ------ end of cross reference ---------------------------------------*
    ELSE.
      LOOP AT ht_fkkop_not_submitted.
        CLEAR h_field.
        CONCATENATE ht_fkkop_not_submitted-opbel
                    ht_fkkop_not_submitted-opupw
                    ht_fkkop_not_submitted-opupk
                    ht_fkkop_not_submitted-opupz
                    INTO h_field SEPARATED BY '/'.
        PERFORM message_append TABLES t_fimsg USING  '>3' 'W' '461'
                        h_field const_agsta_freigegeben space space.
      ENDLOOP.
* ------ for cross reference purpose only -----------------------------*
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE w461(>3). ENDIF.
      SET EXTENDED CHECK ON.
* ------ end of cross reference ---------------------------------------*

** ------ Set flag in the header of the dunning history to indicate the
** ------ release for collection agencies -----------------------------*
      DESCRIBE TABLE ht_dfkkcoll.
      IF sy-tfill > 0.
        c_fkkmako-xcoll = const_marked.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH: ht_fkkop, ht_dfkkcoll, ht_fkkop_not_submitted.

* ------ Append fee and interest lines to interface table HT_FKKOP ----*
* To release the fee lines for collection agency it is necessary to
* set a collection position number for the fee and interest item.
  LOOP AT t_fkkmaze WHERE NOT dalin IS INITIAL.
    PERFORM append_fee_lines TABLES ht_fkkop
                              USING t_fkkmaze-opbel.
  ENDLOOP.

  SORT ht_fkkop BY opbel opupw opupk opupz.
  DELETE ADJACENT DUPLICATES FROM ht_fkkop.

  READ TABLE ht_fkkop INDEX 1.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'FKK_RELEASE_FOR_COLLECT_AGENCY'
      EXPORTING
        i_aggrd                 = const_aggrd_dunning
        i_xsimu                 = h_xsimu
        i_buffered_docs_allowed = const_marked
      TABLES
        t_fkkop                 = ht_fkkop
        t_fkkop_not_submitted   = ht_fkkop_not_submitted
        t_fimsg                 = t_fimsg
        t_dfkkcoll              = ht_dfkkcoll
      EXCEPTIONS
        error                   = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      PERFORM message_append TABLES t_fimsg USING  '>3' 'E' '513'
                 'FKK_RELEASE_FOR_COLLECT_AGENCY' space space space.
* ------ for cross reference purpose only -----------------------------*
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE e513(>3). ENDIF.
      SET EXTENDED CHECK ON.
* ------ end of cross reference ---------------------------------------*
    ELSE.
      LOOP AT ht_fkkop_not_submitted.
        CLEAR h_field.
        CONCATENATE ht_fkkop_not_submitted-opbel
                    ht_fkkop_not_submitted-opupw
                    ht_fkkop_not_submitted-opupk
                    ht_fkkop_not_submitted-opupz
                    INTO h_field SEPARATED BY '/'.
        PERFORM message_append TABLES t_fimsg USING  '>3' 'W' '461'
                        h_field const_agsta_freigegeben space space.
      ENDLOOP.
* ------ for cross reference purpose only -----------------------------*
      SET EXTENDED CHECK OFF.
      IF 1 = 2. MESSAGE w461(>3). ENDIF.
      SET EXTENDED CHECK ON.
* ------ end of cross reference ---------------------------------------*

** ------ Set flag in the header of the dunning history to indicate the
** ------ release for collection agencies -----------------------------*
      DESCRIBE TABLE ht_dfkkcoll.
      IF sy-tfill > 0.
        c_fkkmako-xcoll = const_marked.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFUNCTION.
