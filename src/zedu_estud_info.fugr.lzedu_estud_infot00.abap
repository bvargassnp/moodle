*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZEDU_DURTRAB....................................*
DATA:  BEGIN OF STATUS_ZEDU_DURTRAB                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_DURTRAB                  .
CONTROLS: TCTRL_ZEDU_DURTRAB
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZEDU_INGRESOS...................................*
DATA:  BEGIN OF STATUS_ZEDU_INGRESOS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_INGRESOS                 .
CONTROLS: TCTRL_ZEDU_INGRESOS
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZEDU_TIPOTRA....................................*
DATA:  BEGIN OF STATUS_ZEDU_TIPOTRA                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDU_TIPOTRA                  .
CONTROLS: TCTRL_ZEDU_TIPOTRA
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZEDU_DURTRAB                  .
TABLES: *ZEDU_INGRESOS                 .
TABLES: *ZEDU_TIPOTRA                  .
TABLES: ZEDU_DURTRAB                   .
TABLES: ZEDU_INGRESOS                  .
TABLES: ZEDU_TIPOTRA                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
