*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_USR_GRUPOR.................................*
DATA:  BEGIN OF STATUS_ZEDU_USR_GRUPOR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_USR_GRUPOR               .
CONTROLS: TCTRL_ZEDU_USR_GRUPOR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_USR_GRUPOR               .
TABLES: ZEDU_USR_GRUPOR                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
