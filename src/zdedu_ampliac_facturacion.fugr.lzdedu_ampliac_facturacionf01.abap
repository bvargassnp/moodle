*----------------------------------------------------------------------*
***INCLUDE LZDEDU_AMPLIAC_FACTURACIONF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FKKKO  text
*----------------------------------------------------------------------*
FORM f_validar_campos  USING p_fkkko    TYPE fkkko
                             p_lt_fkkop TYPE fkkop_tab.

  DATA: lt_msgtab    TYPE TABLE OF fkk_msg,
        ls_t77refdoc TYPE tys_t77refdoc,
        ls_hrp9112   TYPE tys_hrp9112,
        ls_fkkop     TYPE fkkop,
        ls_msgtab    TYPE fkk_msg,
        lv_prctr     TYPE fkkop-prctr,
        lv_xblnr     TYPE fkkop-xblnr.

  IF p_fkkko-herkf <> co_10.

    LOOP AT p_lt_fkkop INTO ls_fkkop.

      IF ls_fkkop-prctr IS NOT INITIAL
      OR ls_fkkop-xblnr IS NOT INITIAL
      OR p_fkkko-xblnr IS INITIAL.

        ls_msgtab-msgty = co_e.
        ls_msgtab-msgid = co_fkkdm.
        ls_msgtab-msgno = co_000.
        ls_msgtab-msgv1 = text-004.
        ls_msgtab-msgv2 = text-005.
        APPEND ls_msgtab
        TO lt_msgtab.

        PERFORM f_generar_error
        USING lt_msgtab.

      ENDIF.

    ENDLOOP.

    SELECT SINGLE document
                  eotyp
                  eveid
      FROM t77refdoc
      INTO ls_t77refdoc
      WHERE document = p_fkkko-xblnr+6(10).
    IF sy-subrc = 0.

      SELECT SINGLE a~otype
                    a~objid
                    a~begda
                    a~endda
                    a~tabnr
                    b~ordinterna
        FROM hrp9112 AS a
        JOIN hrt9112 AS b
        ON a~tabnr = b~tabnr
        INTO ls_hrp9112
        WHERE a~otype EQ ls_t77refdoc-eotyp
          AND a~objid EQ ls_t77refdoc-eveid
          AND b~tabnr NE space.   "M9688 - HRESTREPO - 09/04/2019
      IF sy-subrc NE 0
        OR ls_hrp9112-begda IS INITIAL
        OR ls_hrp9112-endda IS INITIAL
        OR ls_hrp9112-ordinterna IS INITIAL.

        ls_msgtab-msgty = co_e.
        ls_msgtab-msgid = co_fkkdm.
        ls_msgtab-msgno = co_000.
        ls_msgtab-msgv1 = text-001.
        ls_msgtab-msgv2 = text-002.
        ls_msgtab-msgv3 = text-003.
        APPEND ls_msgtab
        TO lt_msgtab.

        PERFORM f_generar_error
        USING lt_msgtab.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GENERAR_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_MSGTAB  text
*----------------------------------------------------------------------*
FORM f_generar_error  USING p_lt_msgtab TYPE fkk_msg_t.

  DATA: ls_msgtab   TYPE fkk_msg,
        l_message   TYPE fkk_msg,
        l_ucomm     TYPE  sy-ucomm,
        ls_excluded TYPE fkk_ucomm,
        lt_excluded TYPE TABLE OF	fkk_ucomm.

  CALL FUNCTION 'FKK_MESSAGES_INIT'.
  LOOP AT p_lt_msgtab INTO ls_msgtab.
    CALL FUNCTION 'FKK_MESSAGES_APPEND'
      EXPORTING
        i_message = ls_msgtab.
  ENDLOOP.

  ls_excluded-ucomm = 'ENTR'.
  APPEND ls_excluded
  TO lt_excluded.

* show messages on tablecontrol popup
  CALL FUNCTION 'FKK_MESSAGES_SHOW'
    IMPORTING
      e_selected_message   = l_message
      e_user_command       = l_ucomm
    TABLES
      t_excluded_functions = lt_excluded.

  PERFORM okcode_lopk(saplfkpp).

ENDFORM.
