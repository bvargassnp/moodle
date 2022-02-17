*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_CONTR_LIQUI................................*
DATA:  BEGIN OF STATUS_ZEDU_CONTR_LIQUI              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_CONTR_LIQUI              .
CONTROLS: TCTRL_ZEDU_CONTR_LIQUI
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_CONTR_LIQUI              .
TABLES: ZEDU_CONTR_LIQUI               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
