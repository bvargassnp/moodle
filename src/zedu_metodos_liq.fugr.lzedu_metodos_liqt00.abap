*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_METODOS_LIQ................................*
DATA:  BEGIN OF STATUS_ZEDU_METODOS_LIQ              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_METODOS_LIQ              .
CONTROLS: TCTRL_ZEDU_METODOS_LIQ
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_METODOS_LIQ              .
TABLES: ZEDU_METODOS_LIQ               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
