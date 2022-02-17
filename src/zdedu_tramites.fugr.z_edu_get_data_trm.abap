FUNCTION z_edu_get_data_trm.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     REFERENCE(ES_DATOS_PERSONALES) TYPE  ZEDU_S_DATOS_PERSONALES
*"     REFERENCE(ES_DATOS_DIRECCION) TYPE  ZEDU_S_DIRECCION
*"  TABLES
*"      ET_DATOS_BANCARIOS TYPE  ZEDU_T_DATOS_BANCARIOS OPTIONAL
*"----------------------------------------------------------------------


*** Traigo los datos cargados en memoria durante el tramite.

  es_datos_personales  = gs_datos_personales.
  es_datos_direccion   = gs_datos_direccion.
  et_datos_bancarios[] = gt_datos_bancarios[].

ENDFUNCTION.
