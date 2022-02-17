*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_FILE_DIRLOG
*   generation date: 22.08.2016 at 14:28:43
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_FILE_DIRLOG   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
