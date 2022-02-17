*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_ESTUD_INFO
*   generation date: 12.07.2016 at 13:45:03
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_ESTUD_INFO    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
