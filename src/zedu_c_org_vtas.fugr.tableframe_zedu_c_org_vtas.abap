*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_C_ORG_VTAS
*   generation date: 18.01.2017 at 09:21:24
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_C_ORG_VTAS    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
