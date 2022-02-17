*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_ROLES_TRAM
*   generation date: 06.06.2016 at 09:05:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_ROLES_TRAM    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
