*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_C_CSTAXTYPE
*   generation date: 03.11.2016 at 10:17:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_C_CSTAXTYPE   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
