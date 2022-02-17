*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_C_PARAM....................................*
DATA:  BEGIN OF STATUS_ZEDU_C_PARAM                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_C_PARAM                  .
CONTROLS: TCTRL_ZEDU_C_PARAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_C_PARAM                  .
TABLES: ZEDU_C_PARAM                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
