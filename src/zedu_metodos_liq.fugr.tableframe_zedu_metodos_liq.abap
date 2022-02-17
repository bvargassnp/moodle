*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDU_METODOS_LIQ
*   generation date: 02.08.2016 at 10:07:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDU_METODOS_LIQ   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
