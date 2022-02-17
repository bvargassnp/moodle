"Name: \FU:CMAC_FAID_REALDOC_ACCTINIT\SE:END\EI
ENHANCEMENT 0 ZEDU_PATRO_CENTRO.
*
  FIELD-SYMBOLS <fs-sponsor> TYPE fkkop.
  FIELD-SYMBOLS <fs-estudiante> TYPE fkkop.
  DATA: lv_centro type prctr.

  CALL FUNCTION 'ZEDU_MF_GET_PATRO_CENTRO'
   IMPORTING
     ev_centro       = lv_centro.

  LOOP AT et_bp_items_st ASSIGNING <fs-estudiante>.
    READ TABLE lt_fkkop_sp ASSIGNING <fs-sponsor> INDEX sy-tabix.
    IF sy-subrc IS INITIAL.
      <fs-estudiante>-prctr = <fs-sponsor>-prctr = lv_centro.
    ENDIF.
  ENDLOOP.
ENDENHANCEMENT.
