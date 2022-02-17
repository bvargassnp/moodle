*&---------------------------------------------------------------------*
*&  Include           LZDEDU_RECLAMACION_FICAF01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_APPEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_append TABLES pt_fimsg STRUCTURE fimsg
                    USING  p_msgid p_msgty p_msgno
                           p_msgv1 p_msgv2 p_msgv3 p_msgv4.
  CLEAR pt_fimsg.
  pt_fimsg-msgid = p_msgid.
  pt_fimsg-msgty = p_msgty.
  pt_fimsg-msgno = p_msgno.
  pt_fimsg-msgv1 = p_msgv1.
  pt_fimsg-msgv2 = p_msgv2.
  pt_fimsg-msgv3 = p_msgv3.
  pt_fimsg-msgv4 = p_msgv4.
  APPEND pt_fimsg.
ENDFORM.                               " MESSAGE_APPEND

*&---------------------------------------------------------------------*
*&      Form  APPEND_FEE_LINES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_FKKOP  text
*      -->P_C_FKKMAKO_MG1BL  text
*----------------------------------------------------------------------*
FORM append_fee_lines TABLES   pt_fkkop   STRUCTURE fkkop
                      USING    p_opbel.

  DATA: ht_fkkop LIKE fkkop OCCURS 0 WITH HEADER LINE.

  IF NOT p_opbel IS INITIAL.
    CLEAR pt_fkkop.

* fees and interest are not yet in the database when the dunning
* activity is called, therefore read the document buffer to get
* the document lines
    CALL FUNCTION 'FKK_READ_DOCUMENT_FROM_BUFFER'
      EXPORTING
        i_opbel   = p_opbel
      TABLES
        t_fkkop   = ht_fkkop
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      DELETE ht_fkkop WHERE augst = '9'.
      APPEND LINES OF ht_fkkop TO pt_fkkop.
    ENDIF.
  ENDIF.
ENDFORM.                               " APPEND_FEE_LINES
