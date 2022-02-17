function z_edu_set_data_trm.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IS_DATOS_PERSONALES) TYPE  ZEDU_S_DATOS_PERSONALES
*"       OPTIONAL
*"     REFERENCE(IS_DATOS_DIRECCION) TYPE  ZEDU_S_DIRECCION OPTIONAL
*"     REFERENCE(IT_DATOS_DOCUMENTO) TYPE  ZEDU_T_DATOS_DOCUMENTO
*"       OPTIONAL
*"  TABLES
*"      IT_DATOS_BANCARIOS TYPE  ZEDU_T_DATOS_BANCARIOS OPTIONAL
*"----------------------------------------------------------------------


*** Seteo los datos cargados en memoria durante el tramite.
*  IF is_datos_personales IS NOT INITIAL. "-->  MgM DCEK903539
  if is_datos_personales is supplied.     "-->  MgM DCEK903539
    gs_datos_personales   = is_datos_personales.
  endif.

*  IF is_datos_direccion IS NOT INITIAL.  "-->  MgM DCEK903539
  if is_datos_direccion is supplied.      "-->  MgM DCEK903539
    gs_datos_direccion    = is_datos_direccion.
  endif.

*	Begin	-->	MgM DCEK903539 Elimina cuenta bancaria 16/02/2017
*  IF it_datos_bancarios[] IS NOT INITIAL.
  if it_datos_bancarios is supplied.
*	End	  -->	MgM DCEK903539
    gt_datos_bancarios[]  = it_datos_bancarios[].
  endif.

*  IF it_datos_documento[] IS NOT INITIAL.  "-->  MgM DCEK903539
  if it_datos_documento is supplied.        "-->  MgM DCEK903539
    gt_datos_documento[]  = it_datos_documento[].
  endif.

endfunction.
