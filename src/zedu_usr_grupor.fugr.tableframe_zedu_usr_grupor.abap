*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_USR_GRUPOR
*   generation date: 05.09.2016 at 10:49:02
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_USR_GRUPOR    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
