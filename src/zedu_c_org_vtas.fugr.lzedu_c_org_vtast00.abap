*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_C_ORG_VTAS.................................*
DATA:  BEGIN OF STATUS_ZEDU_C_ORG_VTAS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_C_ORG_VTAS               .
CONTROLS: TCTRL_ZEDU_C_ORG_VTAS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_C_ORG_VTAS               .
TABLES: ZEDU_C_ORG_VTAS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
