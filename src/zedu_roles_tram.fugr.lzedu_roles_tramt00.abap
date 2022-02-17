*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_ROLES_TRAM.................................*
DATA:  BEGIN OF STATUS_ZEDU_ROLES_TRAM               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_ROLES_TRAM               .
CONTROLS: TCTRL_ZEDU_ROLES_TRAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_ROLES_TRAM               .
TABLES: *ZEDU_ROLES_TRAMT              .
TABLES: ZEDU_ROLES_TRAM                .
TABLES: ZEDU_ROLES_TRAMT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
