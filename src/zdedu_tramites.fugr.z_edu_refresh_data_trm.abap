FUNCTION z_edu_refresh_data_trm.
*"----------------------------------------------------------------------
*"*"Interfase local
*"----------------------------------------------------------------------

  REFRESH: gt_datos_bancarios[].
  CLEAR: gs_datos_personales, gs_datos_direccion.

ENDFUNCTION.
