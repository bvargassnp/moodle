FUNCTION-POOL zdedu_tramites.               "MESSAGE-ID ..

DATA: gs_datos_personales     TYPE  zedu_s_datos_personales,
      gs_datos_personales_ini TYPE  zedu_s_datos_personales,
      gs_datos_direccion      TYPE  zedu_s_direccion,
      gs_datos_direccion_ini  TYPE  zedu_s_direccion,
      gt_datos_bancarios      TYPE  zedu_t_datos_bancarios,
      gt_datos_documento      TYPE  zedu_t_datos_documento,
*-- Inicio Modificación Adepcon 28.03.2017 -------------------------------------------
      gx_tramite_web          TYPE  xfeld.
*-- Fin Modificación Adepcon 28.03.2017 ----------------------------------------------


CONSTANTS: gc_ninguno TYPE ad_title1 VALUE '0015'. " Ninguno
* INCLUDE LZDEDU_TRAMITESD...                " Local class definition
