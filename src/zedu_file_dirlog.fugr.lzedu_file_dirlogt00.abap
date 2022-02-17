*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_FILE_DIRLOG................................*
DATA:  BEGIN OF STATUS_ZEDU_FILE_DIRLOG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_FILE_DIRLOG              .
CONTROLS: TCTRL_ZEDU_FILE_DIRLOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_FILE_DIRLOG              .
TABLES: ZEDU_FILE_DIRLOG               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
