FUNCTION z_edu_extractor_graduacion.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      IT_NRO_IDENT TYPE  ZEDU_T_NRO_IDENT
*"      ET_DATOS_GRADUACION TYPE  ZEDU_T_DATOS_GRADUACION
*"  EXCEPTIONS
*"      SIN_DATOS
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_ident,
           objid     TYPE hrobjid,
           nro_ident TYPE bu_id_number,
         END OF ty_ident.

  DATA: lr_class   TYPE REF TO zcl_wd_general_ass,
        lt_ident   TYPE TABLE OF ty_ident,
        ls_ident   TYPE ty_ident,
        lv_stobjid TYPE hrobjid.

  FIELD-SYMBOLS: <fs_nro_ident>        TYPE bu_id_number,
                 <fs_datos_graduacion> TYPE zedu_s_datos_graduacion.

  CREATE OBJECT lr_class.

  LOOP AT it_nro_ident ASSIGNING <fs_nro_ident>. " Nros de identifiación

*** Busco el OBJID del estudiante
    lv_stobjid = lr_class->obtener_objid_st( EXPORTING iv_idnumber = <fs_nro_ident> ).

    ls_ident-nro_ident = <fs_nro_ident>.
    ls_ident-objid     = lv_stobjid.
    APPEND ls_ident TO lt_ident.

  ENDLOOP.

  CHECK lt_ident IS NOT INITIAL.
  SORT lt_ident.

  SELECT objid
         short
         plaest
         titulo
         acto
         acta
         libro
         folio
         diploma
         resolu
         fecact
         insc_per
         puesto_cohorte
         puesto_acto
         estud_coho_grad
         estudiantes_coho
         estudiantes_acto
         promed
         noasistio
    FROM hrp9117
    INTO CORRESPONDING FIELDS OF TABLE et_datos_graduacion
    FOR ALL ENTRIES IN lt_ident
    WHERE plvar = '01'
      AND otype = 'ST'
      AND objid = lt_ident-objid.
**      AND endda = '99991231'.

  IF sy-subrc IS NOT INITIAL.
    RAISE sin_datos.
  ENDIF.

  LOOP AT et_datos_graduacion ASSIGNING <fs_datos_graduacion>.
    CLEAR ls_ident.
    READ TABLE lt_ident INTO ls_ident WITH KEY objid = <fs_datos_graduacion>-objid.
    <fs_datos_graduacion>-nro_ident = ls_ident-nro_ident.
  ENDLOOP.


ENDFUNCTION.
