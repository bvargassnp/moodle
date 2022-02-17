*&---------------------------------------------------------------------*
*&  Include           ZDREDU_ESTADO_ESTUDIANTES_SEL
*&---------------------------------------------------------------------*

TABLES: t7piqyear,
        t7piqperiod,
        but0id,
        hrp1000.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS: p_peryr  TYPE t7piqyear-peryr,
            p_perid  TYPE t7piqperiod-perid,
            p_facul  like hrp1000-objid,
            p_tippro TYPE hrp1001-objid,
            p_tippre TYPE hrp1001-objid.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_plan FOR hrp1000-objid NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-003.
PARAMETERS:    p_tipest TYPE t7piqadmcategt-adm_categ,
               p_tipdoc TYPE tb039a-type.
SELECT-OPTIONS p_nrodoc FOR but0id-idnumber NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b03.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_facul.
  PERFORM f_cargar_facultad.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tippro.
  PERFORM f_cargar_tipo_programa.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tippre.
  PERFORM f_cargar_tipo_pregra_posgra.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_plan-low.
  PERFORM f_cargar_plan.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tipdoc.
  PERFORM f_cargar_tipo_doc.
