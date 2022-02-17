*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_CERTIF.....................................*
DATA:  BEGIN OF STATUS_ZEDU_CERTIF                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_CERTIF                   .
CONTROLS: TCTRL_ZEDU_CERTIF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_CERTIF                   .
TABLES: ZEDU_CERTIF                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
