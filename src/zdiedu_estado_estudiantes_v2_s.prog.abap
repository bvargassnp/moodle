*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_ESTADO_ESTUDIANTES_V2_S
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*   Parametros de entrada
*&---------------------------------------------------------------------*
* Parametros Obligatorios
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS:
  so_fecha FOR gv_datum NO-EXTENSION OBLIGATORY.
PARAMETERS:
  pa_det TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b01.

* Parametros Adicionales
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
SELECT-OPTIONS:
  so_bp    FOR gv_partner,
  so_idty  FOR gv_idtype,
  so_idno  FOR gv_idnumber,
  so_st12  FOR gv_student12,
  so_fcat  FOR gv_fcat,
  so_sc    FOR gv_objid.
SELECTION-SCREEN END OF BLOCK b02.

* Parametros de Ejecución
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
PARAMETERS :
  p_vari   TYPE disvariant-variant,
  p_bloque TYPE int4 DEFAULT 5000 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b03.


*&---------------------------------------------------------------------*
*   Eventos de pantalla
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_sc-low.
  PERFORM f_sh_objid
    USING
      abap_false
      cl_hrpiq00const=>c_otype_sc
      'Ayuda de Búsqueda - Plan de Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_sc-high.
  PERFORM f_sh_objid
    USING
      abap_true
      cl_hrpiq00const=>c_otype_sc
      'Ayuda de Búsqueda - Plan de Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f_buscar_variante.
