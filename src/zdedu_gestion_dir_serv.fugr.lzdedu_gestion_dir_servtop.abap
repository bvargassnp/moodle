FUNCTION-POOL ZDEDU_GESTION_DIR_SERV.       "MESSAGE-ID ..

CONSTANTS: co_sufijo_in(3)     TYPE c VALUE '_IN',
           co_sufijo_out(4)    TYPE c VALUE '_OUT',
           co_sufijo_err(4)    TYPE c VALUE '_ERR',
           co_sufijo_bkp_in(7) TYPE c VALUE '_BKP_IN',
           co_command          TYPE SXPGCOLIST-NAME VALUE 'Z_SUB_MOVE'.

* INCLUDE LZDEDU_GESTION_DIR_SERVD...        " Local class definition
