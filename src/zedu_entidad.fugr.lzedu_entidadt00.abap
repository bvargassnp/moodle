*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_ENTIDAD....................................*
DATA:  BEGIN OF STATUS_ZEDU_ENTIDAD                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_ENTIDAD                  .
CONTROLS: TCTRL_ZEDU_ENTIDAD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDU_ENTIDAD                  .
TABLES: *ZEDU_ENTIDADT                 .
TABLES: ZEDU_ENTIDAD                   .
TABLES: ZEDU_ENTIDADT                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
