*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_C_CSTAXTYPE................................*
DATA:  BEGIN OF STATUS_ZEDU_C_CSTAXTYPE              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_C_CSTAXTYPE              .
CONTROLS: TCTRL_ZEDU_C_CSTAXTYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_C_CSTAXTYPE              .
TABLES: ZEDU_C_CSTAXTYPE               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
