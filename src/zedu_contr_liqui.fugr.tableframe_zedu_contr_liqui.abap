*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_CONTR_LIQUI
*   generation date: 16.06.2017 at 09:18:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_CONTR_LIQUI   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
