*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_ENTIDAD
*   generation date: 20.10.2016 at 12:22:49
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_ENTIDAD       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
