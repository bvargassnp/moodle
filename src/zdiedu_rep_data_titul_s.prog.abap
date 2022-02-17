*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_REP_DATA_TITUL_S
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*   Parametros de entrada
*&---------------------------------------------------------------------*
* Parametros Obligatorios
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS:
  p_arch  TYPE string OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

* Parametros Adicionales
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
SELECT-OPTIONS:
  so_bp    FOR gv_partner,
  so_st    FOR gv_objid,
  so_mat   FOR gv_mat,
  so_id    FOR gv_id.
SELECTION-SCREEN END OF BLOCK b02.

* Parametros Adicionales
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
SELECT-OPTIONS:
  so_sc FOR gv_otjid,
  so_cs FOR gv_otjid.
SELECTION-SCREEN END OF BLOCK b03.

* Parametros de Ejecución
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.
PARAMETERS:
  p_vari   TYPE disvariant-variant,
  p_bloque TYPE int4 DEFAULT 5000 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b04.


*&---------------------------------------------------------------------*
*   Eventos de pantalla
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_sc-low.
  PERFORM f_sh_otjid
    USING
      abap_false
      cl_hrpiq00const=>c_otype_sc
      'Ayuda de Búsqueda - Plan de Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_sc-high.
  PERFORM f_sh_otjid
    USING
      abap_true
      cl_hrpiq00const=>c_otype_sc
      'Ayuda de Búsqueda - Plan de Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_st-low.
  PERFORM f_sh_otjid
    USING
      abap_false
      cl_hrpiq00const=>c_otype_st
      'Ayuda de Búsqueda - Estudiante'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_st-high.
  PERFORM f_sh_otjid
    USING
      abap_true
      cl_hrpiq00const=>c_otype_st
      'Ayuda de Búsqueda - Estudiante'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cs-low.
  PERFORM f_sh_otjid
    USING
      abap_false
      cl_hrpiq00const=>c_otype_cs
      'Ayuda de Búsqueda - Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_cs-high.
  PERFORM f_sh_otjid
    USING
      abap_true
      cl_hrpiq00const=>c_otype_cs
      'Ayuda de Búsqueda - Estudio'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_mat-low.
  PERFORM f_sh_mat
    USING abap_false.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_mat-high.
  PERFORM f_sh_mat
    USING abap_true.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f_buscar_variante.

AT SELECTION-SCREEN.
  PERFORM f_validar_campos.
