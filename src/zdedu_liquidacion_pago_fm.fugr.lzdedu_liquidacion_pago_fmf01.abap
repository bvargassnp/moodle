*----------------------------------------------------------------------*
***INCLUDE LZDEDU_LIQUIDACION_PAGO_FMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_OPBEL  text
*----------------------------------------------------------------------*
FORM f_obtener_datos  USING p_opbel
                            p_nr_formulario
                            p_id_liquidacion
                            p_fechas
                            p_docs_pagados
                            p_no_commit TYPE check
                            p_timelimit TYPE piqtimelimit
                            p_pospp     TYPE opupw_kk
                            p_mostrar_inter TYPE check
                   CHANGING p_lt_return TYPE wrf_bapireturn_tty
                            p_t_docs
                            p_valor_cero TYPE check
                            p_plan_pagos TYPE check.  "-->  MgM DCEK901536

  CLEAR gs_header.
  REFRESH gt_main.

  DATA: lt_opbel_persl  TYPE gty_t_opbel_persl,
        lv_liq_ok       TYPE c,
        lv_valor_cero   TYPE check,
        lv_financiacion TYPE check.

  IF p_opbel IS NOT INITIAL.
    PERFORM f_obtener_cabecera_opbel
      USING p_opbel
            p_docs_pagados
            p_id_liquidacion
            p_mostrar_inter
      CHANGING  p_t_docs
                p_lt_return
                lt_opbel_persl
                p_plan_pagos.


    IF p_plan_pagos IS INITIAL.
      PERFORM f_obtener_posiciones_opbel
      USING p_id_liquidacion
            p_t_docs
            lt_opbel_persl
            p_mostrar_inter "-->  MgM DCEK901536
            p_docs_pagados
      CHANGING p_lt_return
               lv_financiacion.
    ELSE.
      PERFORM f_obtener_posicion_plan_pagos
      USING p_t_docs
            p_pospp
      CHANGING lv_financiacion.
    ENDIF.
  ELSEIF p_nr_formulario IS NOT INITIAL.

    PERFORM f_obtener_cabecera_nr
    USING p_nr_formulario
    CHANGING p_t_docs       "-->  MgM DCEK901536
             p_lt_return
             lv_valor_cero
             lt_opbel_persl.   "-->  MgM DCEK901536
    p_valor_cero = lv_valor_cero.
    IF lv_valor_cero IS INITIAL.
      PERFORM f_obtener_posiciones_opbel
      USING p_id_liquidacion
            p_t_docs
            lt_opbel_persl
            p_mostrar_inter "-->  MgM DCEK901536
            p_docs_pagados
      CHANGING p_lt_return
               lv_financiacion.
    ENDIF.

  ENDIF.

  CHECK p_lt_return IS INITIAL AND lv_valor_cero IS INITIAL.

  PERFORM f_obtener_fechas
  USING gs_header-xblnr
        p_t_docs    "-->  MgM DCEK901536 16/11/2016
        p_id_liquidacion
        p_fechas
        lv_financiacion
        lt_opbel_persl
        p_no_commit
        p_timelimit
        p_mostrar_inter
        p_docs_pagados
  CHANGING p_lt_return.

* Begin Modif 13-12-2016
  IF lv_liq_ok = 'O'.
    CLEAR gs_header-extra1_value.
    CLEAR gs_header-extra2_value.
  ENDIF.
* End Modif 13-12-2016

*	Begin	-->	DCEK901748 DGAGLIARDI XBLNR sin 0 01/12/2016
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_header-xblnr
    IMPORTING
      output = gs_header-xblnr.
*	End	-->	DCEK901748 DGAGLIARDI XBLNR sin 0 01/12/2016

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CABECERA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_cabecera_opbel USING p_opbel
                                    p_docs_pagados TYPE check
                                    p_id_liquidacion
                                    p_mostrar_inter TYPE check
                              CHANGING pc_t_docs        TYPE fkkinv_opbel_tab
                                       pc_t_return      TYPE wrf_bapireturn_tty
                                       pc_t_opbel_persl TYPE gty_t_opbel_persl
                                       pc_plan_pagos    TYPE check.

  DATA: lv_objid       TYPE hrp1001-objid,
        lv_name_lst2   TYPE bu_namepl2,
        lv_namemiddle  TYPE bu_namemid,
        lv_xblnr       TYPE xblnr_kk,
        lv_prefijo_int TYPE zedu_prefijo_int,
        lt_form_blar   TYPE SORTED TABLE OF zedu_c_form_blar WITH UNIQUE KEY primary_key COMPONENTS id_liquidacion blart,
        lr_vktyp_patro TYPE RANGE OF vktyp_kk,
        ls_vktyp_patro LIKE LINE OF lr_vktyp_patro.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_opbel
    IMPORTING
      output = gs_header-opbel.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_opbel
    IMPORTING
      output = lv_xblnr.

  "Obtener clases de documentos excluidos
  SELECT mandt id_liquidacion blart mostrar_detalle
  INTO TABLE lt_form_blar
  FROM zedu_c_form_blar
  WHERE id_liquidacion = p_id_liquidacion.

  IF p_docs_pagados IS INITIAL.
    SELECT opbel gpart persl blart stakz vktyp_ps
    INTO TABLE pc_t_opbel_persl
    FROM dfkkop
    WHERE xblnr = lv_xblnr AND
          augst = ''.
  ELSE.
    SELECT opbel gpart persl blart stakz vktyp_ps
    INTO TABLE pc_t_opbel_persl
    FROM dfkkop
    WHERE xblnr = lv_xblnr AND
          augob = ''. "Documentos no anulados
  ENDIF.

  "Verificar si es documento de plan de pago a plazos
  READ TABLE pc_t_opbel_persl TRANSPORTING NO FIELDS WITH KEY stakz = 'R'.
  IF sy-subrc = 0.
    pc_plan_pagos = 'X'.
  ENDIF.

  "Si no existen documentos con esa referencia, buscar por la referencia del documento
  "del parámetro
  IF pc_t_opbel_persl IS INITIAL.
    SELECT SINGLE xblnr
     INTO @DATA(lv_xblnr_opbel)
     FROM dfkkko
     WHERE opbel = @gs_header-opbel.

    IF lv_xblnr_opbel IS NOT INITIAL.
      IF p_docs_pagados IS INITIAL.
        SELECT opbel gpart persl blart stakz vktyp_ps
        INTO TABLE pc_t_opbel_persl
        FROM dfkkop
        WHERE xblnr = lv_xblnr_opbel AND
              augst = ''.
      ELSE.
        SELECT opbel gpart persl blart stakz vktyp_ps
        INTO TABLE pc_t_opbel_persl
        FROM dfkkop
        WHERE xblnr = lv_xblnr_opbel AND
              augob = ''. "Documentos no anulados
      ENDIF.
      lv_xblnr = lv_xblnr_opbel.
    ENDIF.
  ENDIF.

  "Eliminar clases de documentos parametrizados
  LOOP AT pc_t_opbel_persl INTO DATA(ls_opbel_persl).
    READ TABLE lt_form_blar INTO DATA(ls_form_blar) WITH KEY primary_key COMPONENTS id_liquidacion = p_id_liquidacion blart = ls_opbel_persl-blart.
    CHECK sy-subrc = 0.
    IF p_mostrar_inter IS INITIAL OR ( p_mostrar_inter IS NOT INITIAL AND ls_form_blar-mostrar_detalle IS INITIAL ).
      DELETE pc_t_opbel_persl.
    ENDIF.
  ENDLOOP.

  "Excluir partidas de patrocinador (Se facturan por SD)
  SELECT *
   INTO TABLE @DATA(lt_edu_subv_ped)
   FROM zedu_c_subv_ped.

  ls_vktyp_patro-sign = 'I'.
  ls_vktyp_patro-option = 'EQ'.
  LOOP AT lt_edu_subv_ped INTO DATA(ls_edu_subv_ped).
    ls_vktyp_patro-low = ls_edu_subv_ped-vktyp.
    APPEND ls_vktyp_patro TO lr_vktyp_patro.
  ENDLOOP.

  IF lr_vktyp_patro IS NOT INITIAL.
    DELETE pc_t_opbel_persl WHERE vktyp IN lr_vktyp_patro.
  ENDIF.

  SORT pc_t_opbel_persl BY opbel gpart persl.
  DELETE ADJACENT DUPLICATES FROM pc_t_opbel_persl.

  "Obtener parametrizacion de Intersemestrales
  SELECT *
   INTO TABLE @DATA(lt_persl_int)
   FROM zedu_c_persl_int.

  "Obtener último documento de tasas tanto intersemestral como no intersemestral
  LOOP AT pc_t_opbel_persl ASSIGNING FIELD-SYMBOL(<fs_opbel_persl>).
    lv_prefijo_int = <fs_opbel_persl>-persl+0(2).
    READ TABLE lt_persl_int TRANSPORTING NO FIELDS WITH KEY prefijo_int_ant = lv_prefijo_int.
    IF sy-subrc  = 0. "Es documento de Intersemenstral
      <fs_opbel_persl>-inters = 'X'.
    ENDIF.
  ENDLOOP.

  SORT pc_t_opbel_persl BY inters ASCENDING.

  LOOP AT pc_t_opbel_persl INTO DATA(ls_pc_t_opbel_persl).
    INSERT ls_pc_t_opbel_persl-opbel INTO TABLE pc_t_docs.
  ENDLOOP.

  SORT pc_t_docs.
  DELETE ADJACENT DUPLICATES FROM pc_t_docs.

  CLEAR ls_opbel_persl.
  READ TABLE pc_t_opbel_persl INTO ls_opbel_persl INDEX 1.
  gs_header-gpart = ls_opbel_persl-gpart.
  gs_header-xblnr = lv_xblnr.

*Nombre del Estudiante
  SELECT SINGLE name_first
                namemiddle
                name_last
                name_lst2
    FROM but000
    INTO (gs_header-name_first,lv_namemiddle,
          gs_header-name_last,lv_name_lst2)
    WHERE partner = gs_header-gpart.
  IF sy-subrc NE 0.
    CLEAR: gs_header-name_first,
           gs_header-name_last.
  ENDIF.
  gs_header-name_first = gs_header-name_first && ` ` && lv_namemiddle.
  gs_header-name_last = gs_header-name_last && ` ` && lv_name_lst2.


*Documento de Identificación del estudiante
  SELECT SINGLE type
                idnumber
    FROM but0id
    INTO (gs_header-type,
          gs_header-idnumber)
    WHERE partner = gs_header-gpart.
  IF sy-subrc NE 0.
    CLEAR: gs_header-type,
           gs_header-idnumber.
  ENDIF.

*Fecha de generación de la liquidación de pago
*	Begin	-->	MgM DCEK901693  23/11/2016
*  gs_header-date = sy-datum.
  gs_header-fecha = sy-datum.
*	End	  -->	MgM DCEK901693

*Programa académico al cual está inscrito
*Periodo académico en el cual está ubicado
  SELECT SINGLE b~cmscid
                b~cmperyr                 "--> DGAGLINARDI DCEK901749
                b~cmperid
    FROM cmacdb_feefica AS a
    INNER JOIN cmacdb_feesc AS b
    ON a~docnr  = b~docnr
    INTO (gs_header-cmsmid,
          gs_header-cmperyr,              "--> DGAGLINARDI DCEK901749
          gs_header-cmperid)
    WHERE a~opbel = gs_header-opbel
      AND a~xnew  = co_x.
  IF sy-subrc NE 0.
    CLEAR : gs_header-cmsmid,
            gs_header-cmperid.

  ELSE.

    SELECT SINGLE stext
      FROM hrp1000
      INTO gs_header-cmsm_text
      WHERE plvar = co_01
        AND objid = gs_header-cmsmid
        AND otype = co_sc
        AND langu = sy-langu.
    IF sy-subrc NE 0.
      CLEAR gs_header-cmsm_text.
    ENDIF.

  ENDIF.

*Numero de Matricula del estudiante
  SELECT SINGLE student12
                stobjid
    FROM cmacbpst
    INTO (gs_header-student12,
          gs_header-objid_st)
    WHERE partner = gs_header-gpart.
  IF sy-subrc NE 0.
    CLEAR gs_header-student12.
  ENDIF.

*Semestre
  SELECT SINGLE b~objid
    FROM hrp1001 AS a
    INNER JOIN hrp1001 AS b
    ON a~sobid = b~objid
    INTO lv_objid
    WHERE a~otype = co_st
      AND a~objid = gs_header-objid_st
      AND a~rsign = co_a
      AND a~relat = co_517
      AND a~sclas = co_cs
      AND b~otype = co_cs
      AND b~sobid = gs_header-cmsmid
      AND b~sclas = co_sc.
  IF sy-subrc = 0.
    SELECT  plvar,otype,objid,subty,istat,begda,endda,varyf,seqnr,aclevel
      FROM hrp1771
      INTO TABLE @DATA(lt_hrp1771)
      WHERE plvar = @co_01 AND
            otype = @co_cs AND
            objid = @lv_objid.
    IF sy-subrc NE 0.
      CLEAR gs_header-aclevel.
    ELSE.
      SORT lt_hrp1771 BY endda DESCENDING.
      READ TABLE lt_hrp1771 INTO DATA(ls_hrp1771) INDEX 1.
      gs_header-aclevel = ls_hrp1771-aclevel.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_POSICIONES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_posiciones_opbel USING p_id_liquidacion
                                      p_t_docs          TYPE fkkinv_opbel_tab
                                      p_t_opbel_persl   TYPE gty_t_opbel_persl
                                      p_mostrar_inter   TYPE check
                                      p_docs_pagados    TYPE check
                                CHANGING p_lt_return    TYPE wrf_bapireturn_tty
                                         p_financiacion TYPE check.


  TYPES: BEGIN OF lty_hrp1000,
           plvar TYPE plvar,
           otype TYPE otype,
           objid TYPE hrobjid,
           istat TYPE istat_d,
           begda TYPE begdatum,
           endda TYPE enddatum,
           langu TYPE langu,
           seqnr TYPE seqnr,
           stext TYPE stext,
         END OF lty_hrp1000,

         BEGIN OF lty_cmacdb_feehd,
           docnr TYPE cmac_fee_docnr,
           persl TYPE persl_kk,
           sortf TYPE n LENGTH 3,
         END OF lty_cmacdb_feehd.

  TYPES lty_r_clase_cont TYPE RANGE OF psobtyp_ps.

  DATA: lt_contratos          TYPE tyt_contratos,
        lt_cmacdb_feefica     TYPE tyt_cmacdb_feefica,
        lt_cmacdb_feefica_all TYPE tyt_cmacdb_feefica,
        lt_cmacdb_feesm       TYPE tyt_cmacdb_feesm,
        lt_cmacdb_itemres     TYPE tyt_cmacdb_itemres.

  DATA: lt_contr_liqui         TYPE tyt_contr_liqui,
        lt_opbel               TYPE opbel_t,
        ls_opbel               TYPE opbel_s,
        lt_ref_adicional       TYPE zedu_t_ref_adicional_opbel,
        lt_edu_form_liq        TYPE SORTED TABLE OF zedu_c_form_liq WITH UNIQUE KEY primary_key COMPONENTS periodo_plan blart psobtyp,
        lt_hrp1000_feesm       TYPE SORTED TABLE OF lty_hrp1000   WITH UNIQUE KEY primary_key COMPONENTS plvar otype objid istat begda endda langu seqnr
                                                              WITH NON-UNIQUE SORTED KEY ix_objid COMPONENTS objid,
        lt_hrp1000_itemres     TYPE SORTED TABLE OF lty_hrp1000   WITH UNIQUE KEY primary_key COMPONENTS plvar otype objid istat begda endda langu seqnr
                                                              WITH NON-UNIQUE SORTED KEY ix_objid COMPONENTS otype objid,
        lv_prefijo_int         TYPE zedu_prefijo_int,
        lv_solo_intersemestral TYPE check,
        lra_clase_contrato     TYPE lty_r_clase_cont,
        lsra_clase_contrato    TYPE LINE OF lty_r_clase_cont,
        lt_opbel_feefica       TYPE STANDARD TABLE OF opbel_kk,
        lt_cmacdb_feehd        TYPE STANDARD TABLE OF lty_cmacdb_feehd,
        lv_per_year            TYPE c LENGTH 2,
        lv_per_sem             TYPE c LENGTH 1,
        lv_augst               TYPE augst_kk,
        lt_opbel_contratos     TYPE gty_t_opbel_contratos.


  REFRESH: lt_contratos,
           lt_cmacdb_feefica,
           lt_cmacdb_feesm,
           lt_cmacdb_itemres.

  CHECK p_t_docs IS NOT INITIAL.

  "Obtener Customizing restricciones Asignaturas/Conceptos FICA en el formulario
  SELECT mandt periodo_plan blart psobtyp elim_pos_fica elim_pos_acad
    INTO TABLE lt_edu_form_liq
    FROM zedu_c_form_liq.

  "Obtener DAtos Adicionales de los documentos FICA (Referencia Adicional VTRE2)
  LOOP AT p_t_docs INTO DATA(ls_docs).
    ls_opbel-opbel = ls_docs.
    APPEND ls_opbel TO lt_opbel.
  ENDLOOP.

  SORT lt_opbel BY opbel.
  DELETE ADJACENT DUPLICATES FROM lt_opbel.

  CALL FUNCTION 'Z_EDU_OBTENER_REF_ADICIONAL'
    EXPORTING
      i_opbel_t         = lt_opbel
    IMPORTING
      e_ref_adicional_t = lt_ref_adicional.

  SORT lt_ref_adicional BY opbel ASCENDING.

  "Obtener Tipos de PSOB permitidos según tipo de liquidación
  SELECT psobtyp
    FROM zedu_contr_liqui
    INTO TABLE lt_contr_liqui
    WHERE id_liquidacion = p_id_liquidacion.

  IF sy-subrc = 0.
    lsra_clase_contrato-option  = `EQ`.
    lsra_clase_contrato-sign    = `I`.

    LOOP AT lt_contr_liqui
      INTO lsra_clase_contrato-low.
      APPEND lsra_clase_contrato TO lra_clase_contrato.
    ENDLOOP.
  ENDIF.

  "Obtener parametrizacion de Intersemestrales
  SELECT *
   INTO TABLE @DATA(lt_persl_int)
   FROM zedu_c_persl_int.

*Programa y periodo académico al cual se encuentra inscrito
*Asignaturas inscritas con su número de créditos y valor total correspondiente.
  SELECT docnr
         itemnr
         opbel
   FROM cmacdb_feefica
   INTO TABLE lt_cmacdb_feefica
   FOR ALL ENTRIES IN p_t_docs
   WHERE opbel = p_t_docs-table_line.



  IF sy-subrc = 0.
    "Obtener Periodos de los documentos de tasas
    SELECT docnr persl
      INTO TABLE lt_cmacdb_feehd
      FROM cmacdb_feehd
      FOR ALL ENTRIES IN lt_cmacdb_feefica
      WHERE docnr = lt_cmacdb_feefica-docnr.

    LOOP AT lt_cmacdb_feehd ASSIGNING FIELD-SYMBOL(<fs_cmac_feehd>).
      lv_per_year = <fs_cmac_feehd>-persl+2(2).
      lv_per_sem = <fs_cmac_feehd>-persl+1(1).
      IF lv_per_year CO '0123456789' AND lv_per_sem CO '0123456789'.
        <fs_cmac_feehd>-sortf = lv_per_year && lv_per_sem.
      ENDIF.
    ENDLOOP.
    SORT lt_cmacdb_feehd BY docnr ASCENDING.

    LOOP AT lt_cmacdb_feefica ASSIGNING FIELD-SYMBOL(<fs_cmacdb_feefica>).
      READ TABLE lt_cmacdb_feehd INTO DATA(ls_cmacdb_feehd) WITH KEY docnr = <fs_cmacdb_feefica>-docnr BINARY SEARCH.
      CHECK sy-subrc = 0.
      <fs_cmacdb_feefica>-sortf = ls_cmacdb_feehd-sortf.
    ENDLOOP.

    SORT lt_cmacdb_feefica BY sortf DESCENDING docnr DESCENDING.

    "Obtener último documento de tasas tanto intersemestral como no intersemestral
    LOOP AT lt_cmacdb_feefica ASSIGNING <fs_cmacdb_feefica>.
      READ TABLE p_t_opbel_persl INTO DATA(ls_t_opbel_persl) WITH KEY opbel = <fs_cmacdb_feefica>-opbel.
      CHECK sy-subrc = 0.
      lv_prefijo_int = ls_t_opbel_persl-persl+0(2).
      READ TABLE lt_persl_int TRANSPORTING NO FIELDS WITH KEY prefijo_int_ant = lv_prefijo_int.
      IF sy-subrc  = 0. "Es documento de Intersemenstral
        <fs_cmacdb_feefica>-inters = 'X'.
        READ TABLE lt_cmacdb_feefica_all TRANSPORTING NO FIELDS WITH KEY inters = 'X'.
        IF sy-subrc <> 0.
          INSERT <fs_cmacdb_feefica> INTO TABLE lt_cmacdb_feefica_all.
        ENDIF.
      ELSE.
        READ TABLE lt_cmacdb_feefica_all TRANSPORTING NO FIELDS WITH KEY inters = ''.
        IF sy-subrc <> 0.
          INSERT <fs_cmacdb_feefica> INTO TABLE lt_cmacdb_feefica_all.
        ENDIF.
      ENDIF.
    ENDLOOP.


    "Obtener Asignaturas
    CHECK lt_cmacdb_feefica_all IS NOT INITIAL.
    SELECT docnr itemnr cmsmid cmcrhrs cmsmvalue
      FROM cmacdb_feesm
      INTO TABLE lt_cmacdb_feesm
      FOR ALL ENTRIES IN lt_cmacdb_feefica_all
      WHERE docnr = lt_cmacdb_feefica_all-docnr.

    LOOP AT lt_cmacdb_feesm ASSIGNING FIELD-SYMBOL(<fs_cmacdb_feesm>).
      <fs_cmacdb_feesm>-objid = <fs_cmacdb_feesm>-cmsmid.
      <fs_cmacdb_feesm>-objid_itemres = <fs_cmacdb_feesm>-cmsmid.
    ENDLOOP.

    "Si hay combinación de asignaturas intersemestral y semestral, borrar asignaturas intersemenstral, aparecen en FICA
    IF lines( lt_cmacdb_feefica_all ) > 1.
      LOOP AT lt_cmacdb_feesm ASSIGNING <fs_cmacdb_feesm>.
        READ TABLE lt_cmacdb_feefica_all INTO DATA(ls_cmacdb_feefica_all) WITH KEY docnr = <fs_cmacdb_feesm>-docnr.
        CHECK ls_cmacdb_feefica_all-inters = 'X'.
        DELETE lt_cmacdb_feesm.
      ENDLOOP.
    ENDIF.

    "Verificar si se deben eliminar asignaturas según configuración
    LOOP AT lt_cmacdb_feesm ASSIGNING <fs_cmacdb_feesm>.
      READ TABLE lt_cmacdb_feefica_all INTO ls_cmacdb_feefica_all WITH KEY docnr = <fs_cmacdb_feesm>-docnr.
      CHECK sy-subrc = 0.
      READ TABLE lt_ref_adicional INTO DATA(ls_ref_adicional) WITH KEY opbel = ls_cmacdb_feefica_all-opbel BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_edu_form_liq INTO DATA(ls_edu_form_liq) WITH KEY primary_key COMPONENTS
                 periodo_plan = ls_ref_adicional-periodo_plan
                 blart = ls_ref_adicional-blart
                 elim_pos_acad = 'X'.
      CHECK sy-subrc = 0.
      DELETE lt_cmacdb_feesm.
    ENDLOOP.

    "Obtener info de HRP1000 para nombre de asignatura
    IF lt_cmacdb_feesm IS NOT INITIAL.
      SELECT plvar otype objid istat begda endda langu seqnr stext
        INTO TABLE lt_hrp1000_feesm
        FROM hrp1000
        FOR ALL ENTRIES IN lt_cmacdb_feesm
        WHERE otype = cl_hrpiq00const=>c_otype_sm AND
              objid = lt_cmacdb_feesm-objid.

      LOOP AT lt_cmacdb_feesm ASSIGNING <fs_cmacdb_feesm>.
        READ TABLE lt_hrp1000_feesm INTO DATA(ls_hrp1000_feesm) WITH KEY ix_objid COMPONENTS objid = <fs_cmacdb_feesm>-objid endda = '99991231'.
        CHECK sy-subrc = 0.
        <fs_cmacdb_feesm>-stext = ls_hrp1000_feesm-stext.
      ENDLOOP.
    ENDIF.

    IF lt_cmacdb_feesm IS NOT INITIAL.
      SELECT docnr itemnr otype objid acc_key docamt
        INTO TABLE lt_cmacdb_itemres
        FROM cmacdb_itemres
        FOR ALL ENTRIES IN lt_cmacdb_feesm
        WHERE docnr   = lt_cmacdb_feesm-docnr
          AND objid   = lt_cmacdb_feesm-objid_itemres
          AND condtype IN lra_clase_contrato.

      LOOP AT lt_cmacdb_itemres ASSIGNING FIELD-SYMBOL(<fs_cmacdb_itemres>).
        <fs_cmacdb_itemres>-objid_hrp1000 = <fs_cmacdb_itemres>-objid.
      ENDLOOP.

      IF lt_cmacdb_itemres IS NOT INITIAL.
        SELECT plvar otype objid istat begda endda langu seqnr stext
          INTO TABLE lt_hrp1000_itemres
          FROM hrp1000
          FOR ALL ENTRIES IN lt_cmacdb_itemres
          WHERE otype = lt_cmacdb_itemres-otype AND
                objid = lt_cmacdb_itemres-objid_hrp1000.
      ENDIF.

      LOOP AT lt_cmacdb_itemres ASSIGNING <fs_cmacdb_itemres>.
        READ TABLE lt_hrp1000_itemres INTO DATA(ls_hrp1000_itemres) WITH KEY ix_objid COMPONENTS otype = <fs_cmacdb_itemres>-otype objid = <fs_cmacdb_itemres>-objid endda = '99991231'.
        CHECK sy-subrc = 0.
        <fs_cmacdb_itemres>-stext = ls_hrp1000_itemres-stext.
      ENDLOOP.
    ENDIF.

* Inicio agregado : Leonardo de Jesus Pavia ( ABAP_APO), del 04/03/2017
    LOOP AT lt_cmacdb_itemres INTO DATA(ls_cmacdb_itemres).
      CHECK ls_cmacdb_itemres-acc_key+0(1) <> 'A'.
      DELETE lt_cmacdb_itemres.
    ENDLOOP.
* Fin agregado : Leonardo de Jesus Pavia ( ABAP_ADP ), del 04/03/2017

    IF sy-subrc NE 0.
      CLEAR lt_cmacdb_itemres[].
    ENDIF.
  ENDIF.


  "Determinar si el documento es sólo de intersemestral
  READ TABLE lt_cmacdb_feefica_all TRANSPORTING NO FIELDS WITH KEY inters = 'X'.
  IF sy-subrc = 0.
    lv_solo_intersemestral = 'X'.
    READ TABLE lt_cmacdb_feefica_all TRANSPORTING NO FIELDS WITH KEY inters = ''.
    IF sy-subrc = 0.
      CLEAR lv_solo_intersemestral.
    ENDIF.
  ENDIF.

  IF lra_clase_contrato IS NOT INITIAL.
* End Modif 19-12-2016

*Otros conceptos cobrados
*Descuentos
*Financiacion
    IF p_docs_pagados IS INITIAL.
      SELECT a~opbel
             a~vtref
             a~blart
             a~hvorg
             a~psobtyp
             a~vktyp_ps "MOD 21-12-2016
             a~optxt    "MOD 21-12-2016
             a~betrh
             a~psgrant
             b~psobtypt
        FROM dfkkop AS a
        INNER JOIN tpsob001t AS b
        ON  a~psobtyp = b~psobtyp
        AND b~spras   = 'S'
        INTO TABLE lt_contratos
        FOR ALL ENTRIES IN p_t_docs
          WHERE a~opbel  = p_t_docs-table_line AND
                a~augst  = '' AND
               a~psobtyp IN lra_clase_contrato.
    ELSE.
      SELECT a~opbel
             a~vtref
             a~blart
             a~hvorg
             a~psobtyp
             a~vktyp_ps "MOD 21-12-2016
             a~optxt    "MOD 21-12-2016
             a~betrh
             a~psgrant
             b~psobtypt
        FROM dfkkop AS a
        INNER JOIN tpsob001t AS b
        ON  a~psobtyp = b~psobtyp
        AND b~spras   = 'S'
        INTO TABLE lt_contratos
        FOR ALL ENTRIES IN p_t_docs
          WHERE a~opbel  = p_t_docs-table_line AND
               a~psobtyp IN lra_clase_contrato.
    ENDIF.
    "Si hay posiciciones de Intersemestrales cambiar nombre y quitar tipo de PSOB
    LOOP AT lt_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos>).
      APPEND <fs_contratos>-opbel TO lt_opbel_contratos.
      READ TABLE p_t_opbel_persl INTO DATA(ls_opbel_persl) WITH KEY opbel = <fs_contratos>-opbel.
      CHECK ls_opbel_persl-inters = 'X' AND lv_solo_intersemestral IS INITIAL.
      CASE <fs_contratos>-psobtyp+0(1).
        WHEN 'D' OR 'B'.
          <fs_contratos>-optxt = <fs_contratos>-psobtypt = 'Descuento Intersemestral'.
          <fs_contratos>-psobtyp = '8888'.
        WHEN OTHERS.
          <fs_contratos>-optxt = <fs_contratos>-psobtypt = 'Matrícula Intersemestral'.
          <fs_contratos>-psobtyp = '9999'.
      ENDCASE.
    ENDLOOP.

    "Si hay posiciones de Patrocinios cambiar nombre por descrcipción del PSOB + Nombre Patrocinio
    SELECT *
     INTO TABLE @DATA(lt_edu_subv_ped)
     FROM zedu_c_subv_ped.

    SORT lt_edu_subv_ped BY psobtyp.
    LOOP AT lt_contratos ASSIGNING <fs_contratos>.
      READ TABLE lt_edu_subv_ped TRANSPORTING NO FIELDS WITH KEY psobtyp = <fs_contratos>-psobtyp BINARY SEARCH.
      CHECK sy-subrc = 0.
      <fs_contratos>-optxt = <fs_contratos>-psobtypt && ` ` && <fs_contratos>-psgrant.
    ENDLOOP.

    "Si hay posiciones de cobro extraordinario e intereses mostrar si parámetro está activo
    SELECT *
     INTO TABLE @DATA(lt_form_blar)
     FROM zedu_c_form_blar.

    SORT lt_form_blar BY blart.

    LOOP AT lt_contratos ASSIGNING <fs_contratos>.
      READ TABLE lt_form_blar INTO DATA(ls_form_blar) WITH KEY blart = <fs_contratos>-blart BINARY SEARCH.
      CHECK sy-subrc = 0.
      IF p_mostrar_inter = 'X'.
        IF ls_form_blar-mostrar_detalle IS INITIAL.
          DELETE lt_contratos.
        ELSE.
          "Cambiar Nombre de Objeto para Intereses y cambiar el tipo de psob para permitir agrupación
          <fs_contratos>-psobtypt = <fs_contratos>-optxt.
          <fs_contratos>-psobtyp = ls_form_blar-psobtyp_dummy.
        ENDIF.
      ELSE.
        DELETE lt_contratos.
      ENDIF.
    ENDLOOP.

*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 10/11/2016
    "Quitar conceptos financieros parametrizados (Ya aparecen como conceptos académicos en las asignaturas)
    "Solamente borrar las posiciones fica relacionadas con los documentos academicos
    LOOP AT lt_cmacdb_feefica_all INTO ls_cmacdb_feefica_all.
      LOOP AT lt_cmacdb_feefica INTO DATA(ls_cmacdb_feefica) WHERE docnr = ls_cmacdb_feefica_all-docnr.
        INSERT ls_cmacdb_feefica-opbel INTO TABLE lt_opbel_feefica.
      ENDLOOP.
    ENDLOOP.
    SORT lt_opbel_feefica BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_opbel_feefica.

    LOOP AT lt_contratos INTO DATA(ls_contratos).
      READ TABLE lt_ref_adicional INTO ls_ref_adicional WITH KEY opbel = ls_contratos-opbel BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_edu_form_liq INTO ls_edu_form_liq WITH KEY primary_key COMPONENTS
                 periodo_plan = ls_ref_adicional-periodo_plan
                 blart = ls_contratos-blart
                 psobtyp = ls_contratos-psobtyp
                 elim_pos_fica = 'X'.
      CHECK sy-subrc = 0.
      "Verificar que el documento no sea de Saldos a Favor
      READ TABLE lt_opbel_feefica TRANSPORTING NO FIELDS WITH KEY table_line = ls_contratos-opbel BINARY SEARCH.
      CHECK sy-subrc = 0.
      DELETE lt_contratos.
    ENDLOOP.

*	End	  -->	MgM DCEK901536

    "Inicio agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015
    "Eliminar conceptos ya incluidos en matricula parametrizados
    SELECT *
    INTO TABLE @DATA(lt_form_excl)
    FROM zedu_c_form_excl.

    LOOP AT lt_contratos INTO ls_contratos.
      READ TABLE lt_ref_adicional INTO ls_ref_adicional WITH KEY opbel = ls_contratos-opbel BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_form_excl INTO DATA(ls_form_excl) WITH KEY id_liquidacion = p_id_liquidacion periodo_plan = ls_ref_adicional-periodo_plan psobtyp = ls_contratos-psobtyp.
      CHECK sy-subrc = 0.
      DELETE lt_contratos.
    ENDLOOP.
    "Fin agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015

    "Inicio agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 26/02/2018
    "Multiplicador de conceptos en Recibos Odontologicos
    SELECT *
    INTO TABLE @DATA(lt_form_mult)
    FROM zedu_c_form_mult.

    LOOP AT lt_contratos ASSIGNING <fs_contratos>.
      READ TABLE lt_ref_adicional INTO ls_ref_adicional WITH KEY opbel = ls_contratos-opbel BINARY SEARCH.
      CHECK sy-subrc = 0.
      READ TABLE lt_form_mult INTO DATA(ls_form_mult) WITH KEY psobtyp = <fs_contratos>-psobtyp plan_odontologico = ls_ref_adicional-plan_odontologico.
      CHECK sy-subrc = 0.
      IF ls_form_mult-multiplicador <> 0.
        <fs_contratos>-betrw = <fs_contratos>-betrw * ls_form_mult-multiplicador.
      ENDIF.
    ENDLOOP.
    "Fin agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 26/02/2018

* Begin Modif 21-12-2016
    DELETE lt_contratos WHERE vktyp_ps EQ 'SP'.
* End Modif 21-12-2016
  ENDIF.

  "Verificar si documento tiene planes de pago y construir descripciones en el formulario
  SORT lt_opbel_contratos.
  DELETE ADJACENT DUPLICATES FROM lt_opbel_contratos.
  IF lt_opbel_contratos IS NOT INITIAL.
    SELECT opbel,id_operacion
      INTO TABLE @DATA(lt_opbel_fin)
      FROM zedu_financ_h
      FOR ALL ENTRIES IN @lt_opbel_contratos
      WHERE opbel = @lt_opbel_contratos-table_line.

    "Obtener estado
    IF lt_opbel_fin IS NOT INITIAL.
      SELECT *
        INTO TABLE @DATA(lt_financ_o)
        FROM zedu_financ_o
        FOR ALL ENTRIES IN @lt_opbel_fin
        WHERE id_operacion = @lt_opbel_fin-id_operacion.
    ENDIF.
  ENDIF.
  "Si para alguna de las posiciones existen documentos de financiación mostrar detalle del plan de pagos en la liq.
  READ TABLE lt_financ_o INTO DATA(ls_opbel_fin) WITH KEY estado = '1'.
  IF sy-subrc <> 0.
    PERFORM f_armar_posiciones_opbel
    USING p_id_liquidacion
          lt_contratos
          lt_cmacdb_feefica
          lt_cmacdb_feesm
          lt_cmacdb_itemres.
  ELSE.
    p_financiacion = 'X'.
    gv_fecha_cuota_ini = ls_opbel_fin-fecha_cuota_ini.
    PERFORM f_armar_posiciones_finan USING lt_opbel_contratos.
  ENDIF.
  IF gt_main IS INITIAL.
    PERFORM f_cargar_error
    USING co_e
          text-006
    CHANGING p_lt_return.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMIR_FORMULARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  f_imprimir_formulario USING p_email     TYPE c
                                  p_otf       TYPE c
                                  p_nr_formulario " DGAGLIARDI --> DCEK901536
                                  p_id_liquidacion " DGAGLIARDI -->  DCEK901748
                                  p_valor_cero TYPE check
                                  p_pos_plan_pagos TYPE opupw_kk
                                  p_plan_pagos TYPE check
                         CHANGING p_t_otf     TYPE tsfotf
                                  p_lt_return TYPE wrf_bapireturn_tty. " DGAGLIARDI --> DCEK901536

  DATA: lv_formname           TYPE tdsfname,
        lv_fm_name            TYPE rs38l_fnam,
        lv_control_parameters TYPE ssfctrlop,
        lv_output_options     TYPE ssfcompop,
        lv_job_output_info    TYPE ssfcrescl,
        lv_user_settings      TYPE tdbool VALUE 'X',
        lv_type               TYPE bapireturn-type,
        lv_msgv1              TYPE msgv1,
        lv_msgv2              TYPE msgv2,
        lv_message            TYPE bapireturn-message,
        lv_nro_liq            TYPE string,
        lv_xblnr              TYPE dfkkko-xblnr.

  CONSTANTS: c_liquidacion_pago_estud(30) VALUE 'ZDA_EDU_LIQUIDACION_PAGO_ESTUD',
             c_liquidacion_valor_cero(30) VALUE 'ZDA_EDU_LIQUIDACION_VALOR_CERO'.

  IF p_valor_cero IS INITIAL.
    lv_formname = c_liquidacion_pago_estud.
  ELSE.
    lv_formname = c_liquidacion_valor_cero.
  ENDIF.

  lv_control_parameters-getotf = co_x.
  lv_control_parameters-langu = 'S'.
  IF p_email IS NOT INITIAL
    OR p_otf IS NOT INITIAL.

    CLEAR lv_user_settings.
    lv_control_parameters-no_dialog = co_x.
    lv_output_options-tddest = co_pdf2.
    lv_output_options-tdnewid    = co_x.
    lv_control_parameters-getotf = co_x.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_formname
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc = 0.

    CALL FUNCTION lv_fm_name
      EXPORTING
        control_parameters = lv_control_parameters
        output_options     = lv_output_options
        user_settings      = lv_user_settings
        gs_header          = gs_header
        gt_main            = gt_main
        gv_id_liqui        = p_id_liquidacion
      IMPORTING
        job_output_info    = lv_job_output_info
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc = 0.
      lv_type = 'S'.
      lv_msgv1 = gs_header-xblnr.
      lv_msgv2 = gs_header-objid_st.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZDEDU_FACTURACION'
          msgnr               = '101'
          msgv1               = lv_msgv1
          msgv2               = lv_msgv2
        IMPORTING
          message_text_output = lv_message.
      IF 1 = 2. MESSAGE s101(zdedu_facturacion). ENDIF.

      PERFORM f_cargar_error USING lv_type
                                   lv_message
                          CHANGING p_lt_return.


*	Begin	-->	DCEK901536 DGAGLIARDI Guardar formulario en PDF 17/11/2016
      PERFORM f_guardar_pdf USING lv_job_output_info-otfdata
                                  p_nr_formulario
                                  p_pos_plan_pagos
                                  p_plan_pagos
                         CHANGING p_lt_return.
*	End	  -->	DCEK901536 DGAGLIARDI Guardar formulario en PDF 17/11/2016

      IF p_email IS INITIAL
        AND p_otf IS INITIAL.
        CALL FUNCTION 'HR_IT_DISPLAY_WITH_PDF'
          TABLES
            otf_table = lv_job_output_info-otfdata.
      ENDIF.

      IF  p_email IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = gs_header-xblnr
          IMPORTING
            output = lv_nro_liq.

        PERFORM f_enviar_mail
        USING lv_job_output_info-otfdata lv_nro_liq.
      ENDIF.

      IF p_otf IS NOT INITIAL.
        p_t_otf = lv_job_output_info-otfdata.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_POSICIONES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CONTRATOS  text
*----------------------------------------------------------------------*
FORM f_armar_posiciones_opbel
USING p_id_liquidacion    TYPE zedu_id_liquid
      p_lt_contratos      TYPE tyt_contratos
      p_lt_cmacdb_feefica TYPE tyt_cmacdb_feefica
      p_lt_cmacdb_feesm   TYPE tyt_cmacdb_feesm
      p_lt_cmacdb_itemres TYPE tyt_cmacdb_itemres.

  DATA: lv_moneda TYPE waers VALUE 'COP'.

  DATA: ls_contratos      TYPE tys_contratos,
        ls_cmacdb_feefica TYPE tys_cmacdb_feefica,
        ls_cmacdb_feesm   TYPE tys_cmacdb_feesm,
        ls_cmacdb_itemres TYPE tys_cmacdb_itemres,
        ls_main           TYPE zedu_s_liquidacion_pago_items.

  CLEAR: gv_total, gv_total_extr.

  LOOP AT p_lt_cmacdb_itemres INTO ls_cmacdb_itemres.
    READ TABLE p_lt_cmacdb_feesm INTO ls_cmacdb_feesm
    WITH KEY docnr  = ls_cmacdb_itemres-docnr
             cmsmid = ls_cmacdb_itemres-objid.
    IF sy-subrc = 0.
      READ TABLE p_lt_cmacdb_feefica INTO ls_cmacdb_feefica
      WITH KEY docnr  = ls_cmacdb_feesm-docnr.
      IF sy-subrc = 0.

        ls_main-opbel      = ls_cmacdb_feefica-opbel.
        ls_main-cmsmid     = ls_cmacdb_feesm-cmsmid.
        ls_main-psobtypt   = ls_cmacdb_itemres-stext.
        ls_main-cmcrhrs    = ls_cmacdb_feesm-cmcrhrs.

        gv_total = gv_total + ls_cmacdb_itemres-docamt.
        gv_total_extr = gv_total_extr + ls_cmacdb_itemres-docamt.

        WRITE ls_cmacdb_itemres-docamt TO ls_main-cmsmvalue CURRENCY lv_moneda.

        CONDENSE ls_main-cmsmvalue.

        APPEND ls_main
        TO gt_main.
        CLEAR ls_main.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "Inicio agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015
  "Totalizar Conceptos FICA y eliminar los que tienen resultado 0 de la totalización
  PERFORM f_totalizar_conceptos CHANGING p_lt_contratos.
  "Fin agregado : Leonardo de Jesus Pavia ( LEONARDOPL ), del 16/11/2015

  "Obtener tipos de Contrato exluidos del calculo de matricula extraordinaria
  SELECT *
   INTO TABLE @DATA(lt_form_extr)
   FROM zedu_c_form_extr.

  SORT lt_form_extr BY psobtyp.

  LOOP AT p_lt_contratos INTO ls_contratos.

    ls_main-opbel    = ls_contratos-opbel.
    ls_main-psobtyp  = ls_contratos-psobtyp.
    ls_main-hvorg    = ls_contratos-hvorg.

* Begin Modif 21-12-2016
    IF ls_contratos-psobtyp = 'SP01'.
      ls_main-psobtypt = ls_contratos-optxt.
    ELSE.
      ls_main-psobtypt = ls_contratos-psobtypt.
    ENDIF.
* End Modif 21-12-2016

    ls_main-cmcrhrs  = space.

    "Totalizar
    gv_total = gv_total + ls_contratos-betrw.

    "Totalizar sin conceptos excluidos de matricula extraordinara
    READ TABLE lt_form_extr TRANSPORTING NO FIELDS WITH KEY psobtyp = ls_contratos-psobtyp BINARY SEARCH.
    IF sy-subrc <> 0.
      gv_total_extr = gv_total_extr + ls_contratos-betrw.
    ENDIF.

    WRITE ls_contratos-betrw TO ls_main-cmsmvalue CURRENCY lv_moneda.

    APPEND ls_main TO gt_main.
    CLEAR ls_main.
  ENDLOOP.

  WRITE gv_total TO gs_header-total CURRENCY lv_moneda.

  CONDENSE gs_header-total.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_OPBEL  text
*      -->P_I_NR_FORMULARIO  text
*----------------------------------------------------------------------*
FORM f_validar_entrada  USING p_opbel
                              p_nr_formulario
                              p_id_liquidacion
                              p_fechas TYPE zedu_s_liquidacion_pago_fechas
                              p_mostrar_inter TYPE check
                        CHANGING p_lt_return TYPE wrf_bapireturn_tty.

  DATA: lv_type    TYPE bapireturn-type,
        lv_message TYPE bapireturn-message.

  IF ( p_opbel IS NOT INITIAL
  AND  p_nr_formulario IS NOT INITIAL )

  OR ( p_opbel IS INITIAL
  AND  p_nr_formulario IS INITIAL ).

    lv_type    = co_e.
    lv_message = text-001.

    PERFORM f_cargar_error
    USING lv_type
          lv_message
    CHANGING p_lt_return.

  ENDIF.

  CHECK p_lt_return IS INITIAL.

  IF p_id_liquidacion IS INITIAL.

    lv_type    = co_e.
    lv_message = text-002.

    PERFORM f_cargar_error
    USING lv_type
          lv_message
    CHANGING p_lt_return.

  ENDIF.

  CHECK p_lt_return IS INITIAL.

  IF  p_fechas-calendario IS INITIAL
  AND p_fechas-manual IS INITIAL.

    lv_type    = co_e.
    lv_message = text-003.

    PERFORM f_cargar_error
    USING lv_type
          lv_message
    CHANGING p_lt_return.
  ELSE.
    IF p_mostrar_inter IS INITIAL. "No validar fechas si se selecciona parametro de mostrar intereses y cobro extraord.
      PERFORM f_validar_fechas
      USING p_id_liquidacion
            p_fechas
      CHANGING lv_type
               lv_message.
    ENDIF.
    IF  lv_type IS NOT INITIAL
    AND lv_message IS NOT INITIAL  .
      PERFORM f_cargar_error
      USING lv_type
            lv_message
      CHANGING p_lt_return.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CABECERA_NR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NR_FORMULARIO  text
*----------------------------------------------------------------------*
FORM f_obtener_cabecera_nr  USING p_nr_formulario
                            CHANGING  pc_t_docs TYPE fkkinv_opbel_tab       "-->  MgM DCEK901536.
                                      pc_t_return TYPE wrf_bapireturn_tty
                                      p_valor_cero TYPE check
                                      pc_t_opbel_persl TYPE gty_t_opbel_persl.  "-->  MgM DCEK901536.

  DATA: ls_zpre_admi_1  TYPE tys_zpre_admi1,
        lv_xblnr        TYPE dfkkop-xblnr,
        lv_programa_1   TYPE zpre_admi_2-programa_1,
        lv_periodo_acad TYPE zpre_admi_2-periodo_acad, "--> DGAGLINARDI DCEK901749
        lv_objid        TYPE hrobjid,                  "--> DGAGLINARDI DCEK901749
        lv_val_pagado   TYPE betrw_kk,
        lv_val_pagado_c TYPE zpre_admi_2-valor_pagado.


* Inicio agregado : Leonardo de Jesus Pavia  21/03/2017
  "Si es Formulario de preinscripción con valor cero, salir.
  SELECT SINGLE valor_pagado
  INTO lv_val_pagado_c
  FROM zpre_admi_2
  WHERE nr_formulario = p_nr_formulario.

  lv_val_pagado = lv_val_pagado_c.
  IF lv_val_pagado = 0.
    p_valor_cero = 'X'.
    RETURN.
  ENDIF.
* Fin agregado : Leonardo de Jesus Pavia 21/03/2017

  SELECT SINGLE nombre
                sdo_nombre
                apellido
                sdo_apellido
                tipo_documen
                nro_documen
    INTO ls_zpre_admi_1
    FROM zpre_admi_1
    WHERE nr_formulario = p_nr_formulario.
  IF sy-subrc = 0.

    CONCATENATE ls_zpre_admi_1-nombre
                ls_zpre_admi_1-sdo_nombre
                INTO
                gs_header-name_first
                SEPARATED BY space.

    CONCATENATE ls_zpre_admi_1-apellido
                ls_zpre_admi_1-sdo_apellido
                INTO
                gs_header-name_last
                SEPARATED BY space.

    MOVE: ls_zpre_admi_1-tipo_documen
          TO gs_header-type,
          ls_zpre_admi_1-nro_documen
          TO gs_header-idnumber,
          sy-datum
*	Begin	-->	MgM DCEK901693  23/11/2016
*          to gs_header-date.
          TO gs_header-fecha.
*	End	  -->	MgM DCEK901693

    SELECT SINGLE partner
      FROM but0id
      INTO gs_header-gpart
      WHERE type     = ls_zpre_admi_1-tipo_documen
        AND idnumber = ls_zpre_admi_1-nro_documen.

    IF sy-subrc NE 0.
      CLEAR gs_header-gpart.
    ENDIF.

    SELECT SINGLE programa_1
                  periodo_acad                "--> DGAGLINARDI DCEK901749
      FROM zpre_admi_2
      INTO ( lv_programa_1, lv_periodo_acad ) "--> DGAGLINARDI DCEK901749
      WHERE nr_formulario = p_nr_formulario.

    IF sy-subrc NE 0.
      CLEAR lv_programa_1.
      CLEAR lv_periodo_acad.                   "--> DGAGLINARDI DCEK901749
    ELSE.
      gs_header-cmperyr = lv_periodo_acad(4).  "--> DGAGLINARDI DCEK901749
      gs_header-cmperid = lv_periodo_acad+4.   "--> DGAGLINARDI DCEK901749
    ENDIF.

    IF sy-subrc = 0.
      SELECT SINGLE stext
        FROM hrp1000
        INTO gs_header-cmsm_text
        WHERE objid = lv_programa_1
          AND otype = co_sc
          AND plvar = co_01
          AND langu = sy-langu.

      IF sy-subrc NE 0.
        CLEAR gs_header-cmsm_text.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_nr_formulario
      IMPORTING
        output = lv_xblnr.

*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 11/11/2016
*    select single opbel
*      into gs_header-opbel
*      from dfkkop
*      where xblnr = lv-_xblnr.
    SELECT opbel gpart persl
    INTO TABLE pc_t_opbel_persl
    FROM dfkkop
    WHERE xblnr = lv_xblnr AND
          augst = ''.


    SORT pc_t_opbel_persl BY opbel gpart persl.
    DELETE ADJACENT DUPLICATES FROM pc_t_opbel_persl.

    LOOP AT pc_t_opbel_persl INTO DATA(ls_pc_t_opbel_persl).
      INSERT ls_pc_t_opbel_persl-opbel INTO TABLE pc_t_docs.
    ENDLOOP.

    READ TABLE pc_t_docs INTO gs_header-opbel INDEX 1.

    IF sy-subrc EQ 0.
      MOVE lv_xblnr TO gs_header-xblnr.
    ENDIF.
  ENDIF.

*	Begin	-->	DCEK901749 DGAGLIARDI Agrego Nivel y Periodo 29/11/2016

  IF gs_header-opbel IS NOT INITIAL.

*Número de interlocutor comercial
    SELECT SINGLE gpart
                  xblnr       "	-->	MgM DCEK901693  23/11/2016
      FROM dfkkop
      INTO  (gs_header-gpart,
            gs_header-xblnr)  "	-->	MgM DCEK901693  23/11/2016
      WHERE opbel = gs_header-opbel.
    IF sy-subrc IS INITIAL.

*Numero de Matricula del estudiante
      SELECT SINGLE student12
                    stobjid
        FROM cmacbpst
        INTO (gs_header-student12,
              gs_header-objid_st)
        WHERE partner = gs_header-gpart.
      IF sy-subrc NE 0.
        CLEAR gs_header-student12.
      ENDIF.
      IF sy-subrc IS INITIAL.

*Semestre
        SELECT SINGLE b~objid
          FROM hrp1001 AS a
          INNER JOIN hrp1001 AS b
          ON a~sobid = b~objid
          INTO lv_objid
          WHERE a~otype = co_st
            AND a~objid = gs_header-objid_st
            AND a~rsign = co_a
            AND a~relat = co_517
            AND a~sclas = co_cs
            AND b~otype = co_cs
            AND b~sobid = lv_programa_1
            AND b~sclas = co_sc.
        IF sy-subrc = 0.
          SELECT SINGLE aclevel
            FROM hrp1771
            INTO gs_header-aclevel
            WHERE otype = co_cs
              AND objid = lv_objid
              AND plvar = co_01.
          IF sy-subrc NE 0.
            CLEAR gs_header-aclevel.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* End   --> DCEK901749 DGAGLIARDI Agrego Nivel y Periodo 29/11/2016
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_POSICIONES_NR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtener_posiciones_nr USING p_nr_formulario
                                   p_id_liquidacion
                             CHANGING p_lt_return TYPE wrf_bapireturn_tty.

  DATA: lt_conceptos   TYPE tyt_contratos,
        lt_contr_liqui TYPE tyt_contr_liqui.
*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 10/11/2016
  TYPES lty_r_clase_cont TYPE RANGE OF psobtyp_ps.
  DATA lra_clase_contrato TYPE lty_r_clase_cont.
  DATA lsra_clase_contrato TYPE LINE OF lty_r_clase_cont.
*	End	  -->	MgM DCEK901536

  IF p_id_liquidacion IS NOT INITIAL.

    SELECT psobtyp
      FROM zedu_contr_liqui
      INTO TABLE lt_contr_liqui
      WHERE id_liquidacion = p_id_liquidacion.
    IF sy-subrc = 0.

*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 10/11/2016
      lsra_clase_contrato-option  = `EQ`.
      lsra_clase_contrato-sign    = `I`.

      LOOP AT lt_contr_liqui
        INTO lsra_clase_contrato-low.
        APPEND lsra_clase_contrato TO lra_clase_contrato.
      ENDLOOP.
*	End	  -->	MgM DCEK901536

*Otros conceptos cobrados
      SELECT a~vtref
             a~blart
             a~hvorg
             a~psobtyp
             b~psobtypt
        FROM dfkkop AS a
        INNER JOIN tpsob001t AS b
        ON  a~psobtyp = b~psobtyp
        AND b~spras   = sy-langu
        INTO TABLE lt_conceptos
*        for all entries in lt_contr_liqui  "-->  MgM DCEK901536
        WHERE a~xblnr   = p_nr_formulario
*	Begin	-->	MgM DCEK901536 agrupa pagos XBLNR 10/11/2016
*          and a~psobtyp = lt_contr_liqui-psobtyp.
          AND a~psobtyp IN lra_clase_contrato.
*	End	  -->	MgM DCEK901536

      IF sy-subrc NE 0.
        REFRESH lt_conceptos.
      ENDIF.
    ENDIF.
  ELSE.

*Otros conceptos cobrados
    SELECT a~vtref
           a~blart
           a~hvorg
           a~psobtyp
           b~psobtypt
      FROM dfkkop AS a
      INNER JOIN tpsob001t AS b
      ON  a~psobtyp = b~psobtyp
      AND b~spras   = sy-langu
      INTO TABLE lt_conceptos
      WHERE a~xblnr = p_nr_formulario
        AND a~psobtyp LIKE co_o*.
    IF sy-subrc NE 0.
      REFRESH lt_conceptos.
    ENDIF.
  ENDIF.

  PERFORM f_armar_posiciones_nrfrom
  USING lt_conceptos.

  IF gt_main IS INITIAL.
    PERFORM f_cargar_error
    USING co_e
          text-006
    CHANGING p_lt_return.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ENVIAR_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_enviar_mail USING p_otf TYPE tsfotf p_nro_liq.

  DATA: ls_document_data TYPE sodocchgi1,
        lt_receivers     TYPE somlreci1_t,
        lt_contents_txt  TYPE srm_t_solisti1,
        lt_contents_bin  TYPE srm_t_solisti1,
        lt_packing_list  TYPE sopcklsti1_t.

  PERFORM f_crear_asunto
  USING p_nro_liq
  CHANGING ls_document_data.

  PERFORM f_obtener_destinatarios
  CHANGING lt_receivers.

  PERFORM f_crear_cuerpo
  CHANGING ls_document_data
           lt_contents_txt
           lt_packing_list.

  PERFORM f_crear_adjunto
  USING p_otf
  CHANGING lt_contents_bin
           lt_packing_list.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = ls_document_data
      put_in_outbox              = co_x
      commit_work                = co_x
    TABLES
      packing_list               = lt_packing_list
      contents_bin               = lt_contents_bin
      contents_txt               = lt_contents_txt
      receivers                  = lt_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
    REFRESH: lt_receivers,
             lt_packing_list,
             lt_contents_txt.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREAR_ASUNTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_DOCUMENT_DATA  text
*----------------------------------------------------------------------*
FORM f_crear_asunto USING p_nro_liq CHANGING p_document_data TYPE sodocchgi1.

  CONSTANTS: co_zliquidacion_pago_asunto TYPE char50
             VALUE 'ZLIQUIDACION_PAGO_ASUNTO'.

  DATA: lt_lines TYPE tline_t,
        ls_lines TYPE tline.

  PERFORM f_leer_texto
  USING co_st
        co_s
        co_zliquidacion_pago_asunto
        co_text
 CHANGING lt_lines.

  READ TABLE lt_lines INTO ls_lines
  INDEX 1.
  IF sy-subrc = 0.
    p_document_data-obj_descr = ls_lines-tdline && ` ` && p_nro_liq.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DESTINATARIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_RECEIVERS  text
*----------------------------------------------------------------------*
FORM f_obtener_destinatarios  CHANGING p_lt_receivers TYPE somlreci1_t.

  DATA: ls_receivers TYPE somlreci1,
        lv_email     TYPE adr6-smtp_addr.


  SELECT SINGLE addrnumber
   INTO @DATA(lv_addrnumber)
   FROM but021_fs
   WHERE partner = @gs_header-gpart AND
         adr_kind = 'XXDEFAULT'.

  SELECT SINGLE smtp_addr
  INTO lv_email
  FROM adr6
  WHERE addrnumber = lv_addrnumber.

  ls_receivers-rec_type = co_u.
  ls_receivers-express  = co_x.
  ls_receivers-receiver = lv_email.
  APPEND ls_receivers  TO p_lt_receivers.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREAR_CUERPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_CONTENTS_BIN  text
*----------------------------------------------------------------------*
FORM f_crear_cuerpo  CHANGING p_document_data   TYPE sodocchgi1
                              p_lt_contents_txt TYPE srm_t_solisti1
                              p_lt_packing_list TYPE sopcklsti1_t.

  CONSTANTS: co_zliquidacion_pago_cuerpo TYPE char50
             VALUE 'ZLIQUIDACION_PAGO_CUERPO'.

  DATA: ls_packing_list TYPE sopcklsti1,
        ls_contents_txt TYPE solisti1,
        lt_lines        TYPE tline_t,
        ls_lines        TYPE tline,
        lv_lines        TYPE i.

  PERFORM f_leer_texto
  USING co_st
        co_s
        co_zliquidacion_pago_cuerpo
        co_text
 CHANGING lt_lines.

  IF lt_lines IS NOT INITIAL.

    LOOP AT lt_lines INTO ls_lines.
      ls_contents_txt-line = ls_lines-tdline.
      APPEND ls_contents_txt
      TO p_lt_contents_txt.
      CLEAR ls_contents_txt.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE p_lt_contents_txt
  LINES lv_lines.

  IF lv_lines GT 0.
    p_document_data-doc_size  = lv_lines *
                                255.

    ls_packing_list-body_start = 1.
    ls_packing_list-body_num   = lv_lines.
    ls_packing_list-doc_type   = co_raw.
    APPEND ls_packing_list
    TO p_lt_packing_list.
    CLEAR ls_packing_list.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREAR_ADJUNTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_CONTENTS_BIN  text
*      <--P_LT_PACKING_LIST  text
*----------------------------------------------------------------------*
FORM f_crear_adjunto USING p_otf TYPE tsfotf
                     CHANGING p_lt_contents_bin TYPE srm_t_solisti1
                              p_lt_packing_list TYPE sopcklsti1_t.

  DATA: lt_docs         TYPE tyt_docs,
        lt_lines        TYPE tlinet,
        lt_pdf255       TYPE ibo_t_wf_txt_body,
        lv_bin_filesize TYPE i,
        ls_packing_list TYPE sopcklsti1.

  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    IMPORTING
      bin_filesize   = lv_bin_filesize
    TABLES
      otf            = p_otf
      doctab_archive = lt_docs
      lines          = lt_lines.

  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    EXPORTING
      transfer_bin                = space
    TABLES
      content_in                  = lt_lines
      content_out                 = lt_pdf255
    EXCEPTIONS
      err_line_width_src_too_long = 1
      err_line_width_dst_too_long = 2
      err_conv_failed             = 3
      OTHERS                      = 4.

  p_lt_contents_bin = lt_pdf255.

  ls_packing_list-transf_bin = co_x.
  ls_packing_list-body_start = 1.
  DESCRIBE TABLE p_lt_contents_bin
  LINES ls_packing_list-body_num.
  ls_packing_list-doc_type   = co_pdf.
  ls_packing_list-obj_name   = gs_header-opbel.
  ls_packing_list-obj_descr  = gs_header-opbel.
  ls_packing_list-doc_size   = ls_packing_list-body_num *
                               255.

  APPEND ls_packing_list
  TO p_lt_packing_list.
  CLEAR ls_packing_list.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_FECHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OPBEL  text
*      -->P_P_NR_FORMULARIO  text
*----------------------------------------------------------------------*
FORM f_obtener_fechas  USING p_xblnr TYPE xblnr_kk
                             p_t_docs
                             p_id_liquidacion
                             p_fechas        TYPE zedu_s_liquidacion_pago_fechas
                             p_financiacion  TYPE check
                             p_t_opbel_persl TYPE gty_t_opbel_persl
                             p_no_commit     TYPE check
                             p_timelimit TYPE piqtimelimit
                             p_mostrar_inter TYPE check
                             p_docs_pagados  TYPE check
                       CHANGING p_lt_return  TYPE wrf_bapireturn_tty.


  DATA: lt_fechas_pagos TYPE zfica_t_fechas_pagos_liquidaci,
        ls_fechas_pagos TYPE zfica_fechas_pagos_liquidacion,
        ls_cargos_extr  TYPE zedu_cargos_extr,
        lt_zfica_ppe    TYPE STANDARD TABLE OF zfica_ppe,
        ls_mensaje      TYPE bapireturn.

  DATA: ls_metodos_liq TYPE zedu_metodos_liq.

  DATA: lr_opbel_extr TYPE RANGE OF opbel_kk,
        ls_opbel_extr LIKE LINE OF lr_opbel_extr.

  DATA: lv_type         TYPE bapireturn-type,
        lv_message      TYPE bapireturn-message,
        lv_msgv1        TYPE msgv1,
        lv_error        TYPE check,
        lv_opbel_extra1 TYPE opbel_kk,
        lv_opbel_extra2 TYPE opbel_kk.

  SELECT SINGLE mandt
                id_liquidacion
                fecha_calendario
                fecha_manual
                fecha_ord
                fecha_extra1
                fecha_extra2
                descripcion_liq
    FROM zedu_metodos_liq
    INTO ls_metodos_liq
    WHERE id_liquidacion = p_id_liquidacion.

  SELECT mandt
         timelimit
         porc
    FROM zfica_ppe
    INTO TABLE lt_zfica_ppe.


  PERFORM f_obtener_pagos_fechas USING p_t_docs
                                       p_t_opbel_persl
                                       p_timelimit
                                       p_docs_pagados
                              CHANGING lt_fechas_pagos
                                       p_lt_return.

  IF p_fechas-manual = 'X'.
    LOOP AT lt_fechas_pagos ASSIGNING FIELD-SYMBOL(<fs_fechas_pagos>).
      CASE sy-tabix.
        WHEN 1.
          <fs_fechas_pagos>-endda = p_fechas-ord_date.
        WHEN 2.
          <fs_fechas_pagos>-endda = p_fechas-extra1_date.
        WHEN 3.
          <fs_fechas_pagos>-endda = p_fechas-extra2_date.
      ENDCASE.
    ENDLOOP.
  ENDIF.

  ls_cargos_extr-xblnr = p_xblnr.
  ls_cargos_extr-usuario = sy-uname.
  ls_cargos_extr-fecha = sy-datum.
  ls_cargos_extr-hora = sy-uzeit.
  ls_cargos_extr-tcode = sy-tcode.
  ls_cargos_extr-fecha_calendario = p_fechas-calendario.
  ls_cargos_extr-fecha_manual = p_fechas-manual.
  ls_cargos_extr-id_liquidacion = p_id_liquidacion.
  LOOP AT lt_fechas_pagos INTO ls_fechas_pagos.
    ls_cargos_extr-moneda = ls_fechas_pagos-waers.
    CASE sy-tabix.
      WHEN 1.
        IF ls_metodos_liq-fecha_ord IS NOT INITIAL.
          ls_cargos_extr-ord_date = ls_fechas_pagos-endda.
          ls_cargos_extr-ord_value = ls_fechas_pagos-betrw.
          IF p_financiacion IS NOT INITIAL.
            ls_cargos_extr-ord_value = gv_total.
            ls_cargos_extr-ord_date = gv_fecha_cuota_ini.
          ENDIF.
        ENDIF.
      WHEN 2.
        IF p_financiacion IS NOT INITIAL.
          CLEAR ls_fechas_pagos.
        ENDIF.
        IF ls_metodos_liq-fecha_extra1 IS NOT INITIAL.
          IF ls_fechas_pagos-endda IS NOT INITIAL.
            ls_cargos_extr-extra1_date = ls_fechas_pagos-endda.
            ls_cargos_extr-extra1_value = ls_fechas_pagos-betrw.
          ELSE.
            CLEAR: ls_cargos_extr-extra1_date, ls_cargos_extr-extra1_value.
          ENDIF.
        ENDIF.
      WHEN 3.
        IF p_financiacion IS NOT INITIAL.
          CLEAR ls_fechas_pagos.
        ENDIF.
        IF ls_metodos_liq-fecha_extra2 IS NOT INITIAL.
          IF ls_fechas_pagos-endda IS NOT INITIAL.
            ls_cargos_extr-extra2_date = ls_fechas_pagos-endda.
            ls_cargos_extr-extra2_value = ls_fechas_pagos-betrw.
          ELSE.
            CLEAR: ls_cargos_extr-extra2_date, ls_cargos_extr-extra2_value.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.


  "Verificar si la referencia tiene documentos de recargo, si están vigentes no generar nuevo registro de fechas
  IF p_mostrar_inter IS INITIAL.
    SELECT opbel
    INTO TABLE @DATA(lt_obpel)
    FROM dfkkko
    WHERE xblnr = @ls_cargos_extr-xblnr AND
          blart = 'CE' AND
          storb = ''.

    IF sy-subrc = 0. "La referencia tiene documentos vigentes de recargo, no generar formulario.
      lv_type = 'E'.
      lv_msgv1 = ls_cargos_extr-xblnr.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = 'ZDEDU_FACTURACION'
          msgnr               = '065'
          msgv1               = lv_msgv1
        IMPORTING
          message_text_output = lv_message.

      PERFORM f_cargar_error USING lv_type
                                   lv_message
                          CHANGING p_lt_return.
      IF 1 = 2. MESSAGE e065(zdedu_facturacion). ENDIF.
      lv_error = 'X'.
    ENDIF.
  ENDIF.

  IF lv_error IS INITIAL.
    "No modificar tabla de recargos si se imprime recibo con detalle de intereses y recargos en
    IF p_mostrar_inter IS INITIAL.
      MODIFY zedu_cargos_extr FROM ls_cargos_extr.
    ENDIF.
    gs_header-ord_date = ls_cargos_extr-ord_date.
    gs_header-ord_value = ls_cargos_extr-ord_value.
    gs_header-extra1_date = ls_cargos_extr-extra1_date.
    gs_header-extra1_value = ls_cargos_extr-extra1_value.
    gs_header-extra2_date = ls_cargos_extr-extra2_date.
    gs_header-extra2_value = ls_cargos_extr-extra2_value.

    IF p_fechas-manual = 'X'.
      PERFORM f_actualizar_fecha USING p_t_docs p_fechas-ord_date.
    ENDIF.

    IF p_no_commit IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TYPE  text
*      -->P_L_MESSAGE  text
*      <--P_P_RETURN  text
*----------------------------------------------------------------------*
FORM f_cargar_error  USING p_type
                           p_message
                     CHANGING p_lt_return TYPE wrf_bapireturn_tty.

  DATA: ls_return TYPE bapireturn.

  ls_return-type    = p_type.
  ls_return-message = p_message.

  APPEND ls_return
  TO p_lt_return.
  CLEAR ls_return.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_FECHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ID_LIQUIDACION  text
*      -->P_P_FECHAS  text
*      <--P_P_LT_RETURN  text
*----------------------------------------------------------------------*
FORM f_validar_fechas  USING p_id_liquidacion
                             p_fechas TYPE zedu_s_liquidacion_pago_fechas
                       CHANGING p_type
                                p_message.

  DATA: ls_metodos_liq TYPE zedu_metodos_liq.

  SELECT SINGLE mandt
                id_liquidacion
                fecha_calendario
                fecha_manual
                fecha_ord
                fecha_extra1
                fecha_extra2
                descripcion_liq
    FROM zedu_metodos_liq
    INTO ls_metodos_liq
    WHERE id_liquidacion = p_id_liquidacion.
  IF sy-subrc = 0.

    IF ( ls_metodos_liq-fecha_manual IS INITIAL
    AND  p_fechas-manual IS NOT INITIAL )

    OR ( ls_metodos_liq-fecha_calendario IS INITIAL
    AND  p_fechas-calendario IS NOT INITIAL ).

      p_type    = co_e.
      p_message = text-004.

      EXIT.
    ENDIF.

    IF p_fechas-manual IS NOT INITIAL.

      IF  p_fechas-ord_date IS NOT INITIAL
      AND ls_metodos_liq-fecha_ord IS INITIAL

      OR  p_fechas-extra1_date IS NOT INITIAL
      AND ls_metodos_liq-fecha_extra1 IS INITIAL

      OR  p_fechas-extra2_date IS NOT INITIAL
      AND ls_metodos_liq-fecha_extra2 IS INITIAL.

        p_type    = co_e.
        p_message = text-004.

        EXIT.
      ENDIF.

      IF  p_fechas-ord_date IS INITIAL
      AND ls_metodos_liq-fecha_ord IS NOT INITIAL

      OR  p_fechas-extra1_date IS INITIAL
      AND ls_metodos_liq-fecha_extra1 IS NOT INITIAL

      OR  p_fechas-extra2_date IS INITIAL
      AND ls_metodos_liq-fecha_extra2 IS NOT INITIAL.

        p_type    = co_e.
        p_message = text-004.

        EXIT.
      ENDIF.

    ENDIF.

  ELSE.

    p_type    = co_e.
    p_message = text-005.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LEER_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_ID  text
*      -->P_LV_LANGUAJE  text
*      -->P_LV_NAME  text
*      -->P_LV_OBJECT  text
*      <--P_LT_LINES  text
*----------------------------------------------------------------------*
FORM f_leer_texto  USING p_id
                         p_languaje
                         p_name
                         p_object
                   CHANGING p_lt_lines TYPE tline_t.

  DATA: lv_id       TYPE thead-tdid,
        lv_language TYPE thead-tdspras,
        lv_name     TYPE thead-tdname,
        lv_object   TYPE thead-tdobject.

  lv_id        = p_id.
  lv_language  = p_languaje.
  lv_name      = p_name.
  lv_object    = p_object.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = lv_id
      language                = lv_language
      name                    = lv_name
      object                  = lv_object
    TABLES
      lines                   = p_lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    REFRESH p_lt_lines.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ARMAR_POSICIONES_NRFROM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CONCEPTOS  text
*----------------------------------------------------------------------*
FORM f_armar_posiciones_nrfrom  USING p_lt_contratos TYPE tyt_contratos.

  DATA: ls_main      TYPE zedu_s_liquidacion_pago_items,
        ls_contratos TYPE tys_contratos.

  LOOP AT p_lt_contratos INTO ls_contratos.

    ls_main-opbel     = ls_contratos-opbel.
    ls_main-psobtyp   = ls_contratos-psobtyp.
    ls_main-psobtypt  = ls_contratos-psobtypt.
    ls_main-cmcrhrs   = space.
    ls_main-cmsmvalue = ls_contratos-betrw.

    APPEND ls_main
    TO gt_main.
    CLEAR ls_main.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PAGOS_FECHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OPBEL  text
*      <--P_LT_FECHAS_PAGOS  text
*----------------------------------------------------------------------*
FORM f_obtener_pagos_fechas  USING  p_t_docs  TYPE fkkinv_opbel_tab
                                    p_t_opbel_persl TYPE gty_t_opbel_persl
                                    p_timelimit TYPE piqtimelimit
                                    p_docs_pagados TYPE check
                             CHANGING p_lt_fechas_pagos TYPE zfica_t_fechas_pagos_liquidaci
                                      p_lt_return TYPE wrf_bapireturn_tty.

  DATA: lt_fechas_pagos       TYPE zfica_t_fechas_pagos_liquidaci,
        lt_valores_sin_fecha  TYPE zfica_t_fechas_pagos_liquidaci,
        lv_per_year           TYPE c LENGTH 2,
        lv_per_sem            TYPE c LENGTH 1,
        lv_docs_pagados       TYPE check,
        lt_opbel_ref_adic_in  TYPE opbel_t,
        lt_opbel_ref_adic_out TYPE zedu_t_ref_adicional_opbel,
        lv_plan_odontologico  TYPE char1.


  LOOP AT p_t_opbel_persl  ASSIGNING FIELD-SYMBOL(<fs_opbel_persl>).
    lv_per_year = <fs_opbel_persl>-persl+2(2).
    lv_per_sem = <fs_opbel_persl>-persl+1(1).
    IF lv_per_year CO '0123456789' AND lv_per_sem CO '0123456789'.
      <fs_opbel_persl>-sortf = lv_per_year && lv_per_sem.
    ENDIF.
    APPEND <fs_opbel_persl>-opbel TO lt_opbel_ref_adic_in.
  ENDLOOP.

  SORT p_t_opbel_persl BY sortf DESCENDING.


  "Obtener Referencia Adicional de los documentos
  CALL FUNCTION 'Z_EDU_OBTENER_REF_ADICIONAL'
    EXPORTING
      i_opbel_t         = lt_opbel_ref_adic_in
    IMPORTING
      e_ref_adicional_t = lt_opbel_ref_adic_out.
  SORT lt_opbel_ref_adic_out BY opbel.


  lv_docs_pagados = p_docs_pagados.
  LOOP AT p_t_opbel_persl INTO DATA(ls_opbel_persl).
    CLEAR lv_plan_odontologico.
    READ TABLE lt_opbel_ref_adic_out INTO DATA(ls_opbel_ref_adic_out) WITH KEY opbel = ls_opbel_persl-opbel BINARY SEARCH.
    IF sy-subrc = 0.
      IF ls_opbel_ref_adic_out-plan_odontologico IS NOT INITIAL.
        lv_plan_odontologico = 'X'.
      ENDIF.
    ENDIF.
    "Se llama a la función para obtener los valores de pagos
    CLEAR: lt_fechas_pagos.
    CALL FUNCTION 'ZFICA_FECHAS_PAGOS_LIQUIDACION'
      EXPORTING
        i_opbel                    = ls_opbel_persl-opbel
        i_timelimit                = p_timelimit
        i_docs_pagados             = lv_docs_pagados
        i_plan_odontologico        = lv_plan_odontologico
      IMPORTING
        et_fechas_pago_liquidacion = lt_fechas_pagos
      EXCEPTIONS
        no_existe_documento_fica   = 1
        no_existe_fechas_tasas     = 2
        no_existe_plan_estudios    = 3
        OTHERS                     = 4.

    LOOP AT lt_fechas_pagos INTO DATA(ls_fechas_pago).
      IF ls_fechas_pago-endda IS NOT INITIAL.
        READ TABLE p_lt_fechas_pagos ASSIGNING FIELD-SYMBOL(<fs_fechas_pagos>) WITH KEY timelimit = ls_fechas_pago-timelimit.  "endda = ls_fechas_pago-endda.
        IF sy-subrc = 0.
          <fs_fechas_pagos>-betrw = <fs_fechas_pagos>-betrw + ls_fechas_pago-betrw.
        ELSE.
          INSERT ls_fechas_pago INTO TABLE p_lt_fechas_pagos.
        ENDIF.
      ELSE.
        READ TABLE lt_valores_sin_fecha ASSIGNING FIELD-SYMBOL(<fs_valores_sin_fecha>) WITH KEY timelimit = ls_fechas_pago-timelimit.
        IF sy-subrc = 0.
          <fs_valores_sin_fecha>-betrw = <fs_valores_sin_fecha>-betrw + ls_fechas_pago-betrw.
        ELSE.
          INSERT ls_fechas_pago INTO TABLE lt_valores_sin_fecha.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  "Actualizar valores con valores sin fecha
  LOOP AT lt_valores_sin_fecha INTO DATA(ls_valores_sin_fecha).
    READ TABLE p_lt_fechas_pagos ASSIGNING FIELD-SYMBOL(<fs_p_fechas_pagos>) WITH KEY timelimit = ls_valores_sin_fecha-timelimit.
    IF sy-subrc = 0.
      <fs_p_fechas_pagos>-betrw = <fs_p_fechas_pagos>-betrw + ls_valores_sin_fecha-betrw.
    ELSE.
      INSERT ls_valores_sin_fecha INTO TABLE p_lt_fechas_pagos.
    ENDIF.
  ENDLOOP.
ENDFORM.
*	Begin	-->	DCEK901536 DGAGLIARDI Guardar formulario en PDF 17/11/2016
*&---------------------------------------------------------------------*
*&      Form  F_GUARDAR_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_JOB_OUTPUT_INFO_OTFDATA  text
*----------------------------------------------------------------------*
FORM f_guardar_pdf  USING p_otfdata   TYPE tsfotf
                          p_nr_formulario
                          p_pos_plan_pagos TYPE opupw_kk
                          p_plan_pagos TYPE check
                 CHANGING p_lt_return TYPE wrf_bapireturn_tty.

  CONSTANTS: lco_extension_pdf TYPE char4 VALUE '.pdf'.
  DATA:
    lt_lines                TYPE tlinet,
    lt_pdf255               TYPE ibo_t_wf_txt_body,
    ls_pdf255               TYPE so_text255,
    lt_docs                 TYPE TABLE OF docs,
    lv_xblnr                TYPE dfkkop-xblnr,
    lv_file_name_orig       TYPE string,
    lv_type                 TYPE bapireturn-type,
    lv_message              TYPE bapireturn-message,
    lv_bin_filesize         TYPE i,
    lv_valor_param          TYPE zedu_valor,
    lv_dir_ruta_recibo      TYPE eps2path,
    lv_timestamp            TYPE tzonref-tstamps,
    lv_file_name            TYPE eps2filnam,
    lv_file_date            TYPE sy-datum,
    lv_file_time_str        TYPE string,
    lv_file_name_dest       TYPE string,
    lv_dir_ruta_recibo_hist TYPE eps2path,
    lv_msgv1                TYPE msgv1,
    lv_msgv2                TYPE msgv2.

  "Obtener Ruta Recibos
  SELECT SINGLE valor
    INTO lv_valor_param
    FROM zedu_c_param
    WHERE repid = '' AND
          idparam = 'RUTARECIBO'.

  SELECT SINGLE dirname
    INTO lv_dir_ruta_recibo
    FROM user_dir
    WHERE aliass = lv_valor_param.

  IF sy-subrc = 0.
    lv_dir_ruta_recibo = lv_dir_ruta_recibo && '\'.
  ENDIF.

  "Obtener Ruta Recibos Historicos
  SELECT SINGLE valor
    INTO lv_valor_param
    FROM zedu_c_param
    WHERE repid = '' AND
          idparam = 'RUTARECHIS'.

  SELECT SINGLE dirname
    INTO lv_dir_ruta_recibo_hist
    FROM user_dir
    WHERE aliass = lv_valor_param.

  IF sy-subrc = 0.
    lv_dir_ruta_recibo_hist = lv_dir_ruta_recibo_hist && '\'.
  ENDIF.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_header-xblnr
    IMPORTING
      output = lv_xblnr.

  IF p_plan_pagos IS INITIAL.
    lv_file_name  = lv_xblnr && lco_extension_pdf.
  ELSE.
    lv_file_name  = lv_xblnr && '-' && p_pos_plan_pagos && lco_extension_pdf.
  ENDIF.
  lv_file_name_orig = lv_dir_ruta_recibo && lv_file_name.

  "Verificar si existe archivo y obtener atributo de fecha y hora para mover a ruta de Archiving
  CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
    EXPORTING
      iv_long_file_name      = lv_file_name
      iv_long_dir_name       = lv_dir_ruta_recibo
    IMPORTING
      file_mtime             = lv_timestamp
    EXCEPTIONS
      read_directory_failed  = 1
      read_attributes_failed = 2
      OTHERS                 = 3.

  IF sy-subrc = 0.  "Mover archivo existente
    "Obtener fecha y hora del archivo para adicionar al nombre archivo destino
    PERFORM p6_to_date_time_tz IN PROGRAM rstr0400 USING lv_timestamp
                                                         lv_file_time_str
                                                         lv_file_date.
    REPLACE ALL OCCURRENCES OF ':' IN lv_file_time_str WITH ''.

    IF p_plan_pagos IS INITIAL.
      lv_file_name_dest = lv_dir_ruta_recibo_hist && lv_xblnr && '_' && lv_file_date && '_' && lv_file_time_str && lco_extension_pdf.
    ELSE.
      lv_file_name_dest = lv_dir_ruta_recibo_hist && lv_xblnr && '-' && p_pos_plan_pagos && '_' && lv_file_date && '_' && lv_file_time_str && lco_extension_pdf.
    ENDIF.

    DATA: lv_xstring TYPE xstring.
    OPEN DATASET lv_file_name_orig FOR INPUT IN BINARY MODE.
    OPEN DATASET lv_file_name_dest FOR OUTPUT IN BINARY MODE.
    IF sy-subrc NE 0.
    ENDIF.
    DO.
      READ DATASET lv_file_name_orig INTO lv_xstring.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      TRANSFER lv_xstring TO lv_file_name_dest.
    ENDDO.
    CLOSE DATASET lv_file_name_orig.
    CLOSE DATASET lv_file_name_dest.
  ENDIF.


  "Crear Archivo
  CALL FUNCTION 'CONVERT_OTF_2_PDF'
    IMPORTING
      bin_filesize   = lv_bin_filesize
    TABLES
      otf            = p_otfdata
      doctab_archive = lt_docs
      lines          = lt_lines.

  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    EXPORTING
      transfer_bin                = space
    TABLES
      content_in                  = lt_lines
      content_out                 = lt_pdf255
    EXCEPTIONS
      err_line_width_src_too_long = 1
      err_line_width_dst_too_long = 2
      err_conv_failed             = 3
      OTHERS                      = 4.


  OPEN DATASET lv_file_name_orig FOR OUTPUT IN BINARY MODE.
  IF sy-subrc NE 0.
    "No se puede crear archivo
    lv_type = 'W'.
    lv_msgv1 = lv_xblnr.
    lv_msgv2 = lv_dir_ruta_recibo.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZDEDU_FACTURACION'
        msgnr               = '104'
        msgv1               = lv_msgv1
        msgv2               = lv_msgv2
      IMPORTING
        message_text_output = lv_message.

    PERFORM f_cargar_error USING lv_type
                                 lv_message
                        CHANGING p_lt_return.
    IF 1 = 2. MESSAGE w104(zdedu_facturacion). ENDIF.
    EXIT.
  ENDIF.

  LOOP AT lt_pdf255 INTO ls_pdf255.
    TRANSFER ls_pdf255 TO lv_file_name_orig.
  ENDLOOP.
  CLOSE DATASET lv_file_name_orig.

  IF sy-subrc IS INITIAL.
    "Se creó correctamente archivo en ruta....
    lv_type = 'S'.
    IF p_pos_plan_pagos IS INITIAL.
      lv_msgv1 = gs_header-xblnr.
    ELSE.
      lv_msgv1 = gs_header-xblnr && '-' && p_pos_plan_pagos.
    ENDIF.
    lv_msgv2 = lv_file_name_orig.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = 'ZDEDU_FACTURACION'
        msgnr               = '103'
        msgv1               = lv_msgv1
        msgv2               = lv_msgv2
      IMPORTING
        message_text_output = lv_message.

    PERFORM f_cargar_error USING lv_type
                                 lv_message
                        CHANGING p_lt_return.
    IF 1 = 2. MESSAGE s103(zdedu_facturacion). ENDIF.
  ENDIF.
ENDFORM.
*	End	-->	DCEK901536 DGAGLIARDI Guardar formulario en PDF 17/11/2016
* Begin Modif 22-12-2016
*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZAR_FECHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_actualizar_fecha USING pc_t_docs   TYPE fkkinv_opbel_tab
                              pa_ord_date TYPE sy-datum.

  DATA: lt_positions         TYPE STANDARD TABLE OF bapidfkkop,
        lt_partnerpositions  TYPE STANDARD TABLE OF bapidfkkopch,
        lt_partnerpositionsx TYPE STANDARD TABLE OF bapidfkkopchx,
        ls_positions         TYPE bapidfkkop,
        ls_partnerpositions  TYPE bapidfkkopch,
        ls_partnerpositionsx TYPE bapidfkkopchx,
        ls_documentheader    TYPE bapidfkkko,
        ls_return            TYPE bapiret2,
        ls_docs              TYPE LINE OF fkkinv_opbel_tab.

  DATA: lv_documentnumber TYPE bapidfkkko-doc_no.

  LOOP AT pc_t_docs INTO ls_docs.
    lv_documentnumber = ls_docs.

    REFRESH: lt_positions, lt_partnerpositions, lt_partnerpositionsx.

    CALL FUNCTION 'BAPI_CTRACDOCUMENT_GETDETAIL'
      EXPORTING
        documentnumber   = lv_documentnumber
        detailtype       = ' '
      IMPORTING
        documentheader   = ls_documentheader
        return           = ls_return
      TABLES
        partnerpositions = lt_positions.

    LOOP AT lt_positions INTO ls_positions.

      ls_partnerpositions-line_number = ls_partnerpositions-line_number + 1.

      MOVE-CORRESPONDING ls_positions TO ls_partnerpositions.
      ls_partnerpositions-net_date = pa_ord_date.
      APPEND ls_partnerpositions TO lt_partnerpositions.

      ls_partnerpositionsx-line_number = ls_partnerpositions-line_number.
      ls_partnerpositionsx-net_date    = 'X'.

      APPEND ls_partnerpositionsx TO lt_partnerpositionsx.

    ENDLOOP.

    CALL FUNCTION 'BAPI_CTRACDOCUMENT_CHANGE'
      EXPORTING
        documentnumber    = lv_documentnumber
*       DOCUMENTHEADER    =
*       DOCUMENTHEADERX   =
      IMPORTING
        return            = ls_return
      TABLES
        partnerpositions  = lt_partnerpositions
        partnerpositionsx = lt_partnerpositionsx.
  ENDLOOP.

ENDFORM.
* End Modif 22-12-2016

FORM f_agrupar_conceptos USING p_id_liquidacion TYPE zid_liquidacion.

  TYPES: BEGIN OF lty_grupo,
           id_grupo  TYPE zedu_id_grupo_f,
           cmsmvalue TYPE char20,
           betrw     TYPE betrw_kk,
         END OF lty_grupo.


  DATA: lt_c_form_grp     TYPE SORTED TABLE OF zedu_c_form_grp WITH UNIQUE KEY primary_key COMPONENTS id_liquidacion id_grupo psobtyp
                                                           WITH NON-UNIQUE SORTED KEY ix1 COMPONENTS psobtyp hvorg,
        lt_grupo          TYPE SORTED TABLE OF lty_grupo WITH UNIQUE KEY primary_key COMPONENTS id_grupo,
        ls_grupo          TYPE lty_grupo,
        lv_valor_concepto TYPE betrw_kk,
        lv_cmsmvalue      TYPE char20.

  SELECT mandt id_liquidacion id_grupo psobtyp hvorg mostrar_psobtyp
   INTO TABLE lt_c_form_grp
   FROM zedu_c_form_grp
   WHERE id_liquidacion = p_id_liquidacion.

  CHECK sy-subrc = 0.

  LOOP AT gt_main ASSIGNING FIELD-SYMBOL(<fs_main>).
    lv_cmsmvalue = <fs_main>-cmsmvalue.
    CONDENSE lv_cmsmvalue NO-GAPS.
    REPLACE ALL OCCURRENCES OF '.' IN lv_cmsmvalue WITH space.
    lv_valor_concepto = lv_cmsmvalue.
    READ TABLE lt_c_form_grp INTO DATA(ls_c_form_grp) WITH KEY ix1 COMPONENTS psobtyp = <fs_main>-psobtyp hvorg = <fs_main>-hvorg.
    CHECK sy-subrc = 0.
    READ TABLE lt_grupo ASSIGNING FIELD-SYMBOL(<fs_grupo>) WITH KEY primary_key COMPONENTS id_grupo = ls_c_form_grp-id_grupo.
    IF sy-subrc = 0.
      <fs_grupo>-betrw = <fs_grupo>-betrw + lv_valor_concepto / 100.
    ELSE.
      ls_grupo-id_grupo = ls_c_form_grp-id_grupo.
      ls_grupo-betrw = lv_valor_concepto / 100.
      INSERT ls_grupo INTO TABLE lt_grupo.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_main ASSIGNING <fs_main>.
    READ TABLE lt_c_form_grp INTO ls_c_form_grp WITH KEY ix1 COMPONENTS psobtyp = <fs_main>-psobtyp hvorg = <fs_main>-hvorg.
    .
    CHECK sy-subrc = 0.
    IF ls_c_form_grp-mostrar_psobtyp IS INITIAL.
      DELETE gt_main.
      CONTINUE.
    ELSE.
      READ TABLE lt_grupo INTO ls_grupo WITH KEY primary_key COMPONENTS id_grupo = ls_c_form_grp-id_grupo.
      IF sy-subrc = 0.
        WRITE ls_grupo-betrw TO <fs_main>-cmsmvalue CURRENCY 'COP' DECIMALS 0.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM f_mapear_nombres_conceptos USING p_id_liquidacion TYPE zid_liquidacion.
  DATA: lt_c_form_text TYPE SORTED TABLE OF zedu_c_form_text WITH UNIQUE KEY primary_key COMPONENTS psobtyp hvorg.


  SELECT mandt id_liquidacion psobtyp hvorg texto_concepto
    INTO TABLE lt_c_form_text
    FROM zedu_c_form_text
    WHERE id_liquidacion = p_id_liquidacion.

  CHECK lt_c_form_text IS NOT INITIAL.
  LOOP AT gt_main ASSIGNING FIELD-SYMBOL(<fs_main>).
    READ TABLE lt_c_form_text INTO DATA(ls_c_form_text) WITH KEY primary_key COMPONENTS psobtyp = <fs_main>-psobtyp hvorg = <fs_main>-hvorg.
    CHECK sy-subrc = 0.
    <fs_main>-psobtypt = ls_c_form_text-texto_concepto.
  ENDLOOP.
ENDFORM.

FORM f_totalizar_conceptos CHANGING pt_contratos TYPE tyt_contratos.
  DATA: lt_contratos_total TYPE SORTED TABLE OF tys_contratos  WITH UNIQUE KEY primary_key COMPONENTS opbel vtref blart hvorg psobtyp
                                                               WITH NON-UNIQUE SORTED KEY ix_psobtyp COMPONENTS hvorg psobtyp.
  "Agrupar y totalizar posiciones iguales HVORG PSOBTYP
  LOOP AT pt_contratos INTO DATA(ls_contratos).
    READ TABLE lt_contratos_total ASSIGNING FIELD-SYMBOL(<fs_contratos_total>) WITH KEY ix_psobtyp COMPONENTS hvorg = ls_contratos-hvorg psobtyp = ls_contratos-psobtyp.
    IF sy-subrc = 0.
      <fs_contratos_total>-betrw = <fs_contratos_total>-betrw + ls_contratos-betrw.
    ELSE.
      INSERT ls_contratos INTO TABLE lt_contratos_total.
    ENDIF.
  ENDLOOP.

  "Eliminar posiciones totalizadas en cero
  DELETE lt_contratos_total WHERE betrw = 0.


  REFRESH pt_contratos.
  INSERT LINES OF lt_contratos_total INTO TABLE pt_contratos.
ENDFORM.

FORM f_armar_posiciones_finan USING p_lt_contratos TYPE gty_t_opbel_contratos.
  DATA: lt_financ_h  TYPE SORTED TABLE OF zedu_financ_h WITH NON-UNIQUE KEY primary_key COMPONENTS opbel opupw opupk opupz,
        lt_financ_d  TYPE SORTED TABLE OF zedu_financ_d WITH UNIQUE KEY primary_key COMPONENTS id_operacion rptyp,
        ls_main      TYPE zedu_s_liquidacion_pago_items,
        lv_monto_str TYPE c LENGTH 20.

  CLEAR gv_total.
  CHECK p_lt_contratos IS NOT INITIAL.
  SELECT *
   INTO TABLE lt_financ_h
   FROM zedu_financ_h
   FOR ALL ENTRIES IN p_lt_contratos
   WHERE opbel = p_lt_contratos-table_line.

  "Obtener estado
  IF lt_financ_h IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(lt_financ_o)
      FROM zedu_financ_o
      FOR ALL ENTRIES IN @lt_financ_h
      WHERE id_operacion = @lt_financ_h-id_operacion.
  ENDIF.

  READ TABLE lt_financ_o INTO DATA(ls_financ_o) WITH KEY estado = '1'. "Estado grabado
  "Si tiene plan de pagos agregar descripcion de cuota inicial
  IF sy-subrc = 0.
    READ TABLE lt_financ_h INTO DATA(ls_financ_h) INDEX 1.
    WRITE ls_financ_o-cuota_inicial TO ls_main-cmsmvalue CURRENCY ls_financ_o-moneda.
    ls_main-psobtypt = 'Abono Inicial Plan de Pagos'.
    "WRITE ls_financ_h-cuota_inicial TO gs_header-total  CURRENCY ls_financ_h-moneda.
    "CONDENSE gs_header-total.
    gv_total = ls_financ_o-cuota_inicial.
    APPEND ls_main TO gt_main.
  ENDIF.

  "Agregar descripción de planes de pago
  SELECT *
    INTO TABLE lt_financ_d
    FROM zedu_financ_d
    WHERE id_operacion = ls_financ_o-id_operacion.

  SELECT *
    INTO TABLE @DATA(lt_tfk060t)
    FROM tfk060t
    FOR ALL ENTRIES IN @lt_financ_d
    WHERE spras = @sy-langu AND
          rptyp = @lt_financ_d-rptyp.

  LOOP AT lt_financ_d INTO DATA(ls_financ_d).
    CLEAR ls_main.
    READ TABLE lt_tfk060t INTO DATA(ls_tfk060t) WITH KEY rptyp = ls_financ_d-rptyp.
    WRITE ls_financ_d-total_tipo_plan TO lv_monto_str CURRENCY ls_financ_d-waers.
    CONDENSE lv_monto_str NO-GAPS.
    ls_main-psobtypt = ls_tfk060t-rptxt && ` ` && 'por' && ` ` && lv_monto_str && ` ` && ls_financ_d-waers.
    APPEND ls_main TO gt_main.
  ENDLOOP.
ENDFORM.
FORM f_mostrar_conceptos_pagados USING p_id_liquidacion TYPE zid_liquidacion
                                        pt_documentos TYPE fkkinv_opbel_tab.
  DATA: lt_psobtyp   TYPE STANDARD TABLE OF psobtyp_ps,
        ls_main      TYPE zedu_s_liquidacion_pago_items,
        lv_monto_str TYPE c LENGTH 20.

  "Buscar conceptos parametrizados para mostrar
  SELECT *
  INTO TABLE @DATA(lt_form_cver)
  FROM zedu_c_form_cver
  WHERE id_liquidacion = @p_id_liquidacion.

  "Verificar si el concepto a mostrar ya se está mostrando
  LOOP AT lt_form_cver INTO DATA(ls_formcver).
    READ TABLE gt_main TRANSPORTING NO FIELDS WITH KEY psobtyp = ls_formcver-psobtyp.
    IF sy-subrc <> 0. "Si el concepto a mostrar no está listado, guardar en tabla para buscarlo como concepto pagado
      APPEND ls_formcver-psobtyp TO lt_psobtyp.
    ENDIF.
  ENDLOOP.

  SORT lt_psobtyp.
  DELETE ADJACENT DUPLICATES FROM lt_psobtyp.


  IF lt_psobtyp IS NOT INITIAL AND pt_documentos IS NOT INITIAL.
    SELECT opbel,opupw,opupk,opupz,
           augst,betrh,waers,psobtyp
           INTO TABLE @DATA(lt_dfkkop)
           FROM dfkkop
           FOR ALL ENTRIES IN @pt_documentos
           WHERE opbel = @pt_documentos-table_line AND
                 augst = '9' AND "Compensados
                 augob = 'X'.    "No Anulados

    "Obtener Nombres de Tipo de Cuenta Contrato
    SELECT *
      INTO TABLE @DATA(lt_tpsob001t)
      FROM tpsob001t
      FOR ALL ENTRIES IN @lt_psobtyp
      WHERE spras = @sy-langu AND
            psobtyp = @lt_psobtyp-table_line.

    SORT lt_tpsob001t BY psobtyp.
  ENDIF.

  "Construir Tabla de salida GT_MAIN con los nuevos conceptos sólo de visualización, no debe sumar a ningún total
  LOOP AT lt_dfkkop INTO DATA(ls_dfkkop).
    READ TABLE lt_psobtyp TRANSPORTING NO FIELDS WITH KEY table_line = ls_dfkkop-psobtyp BINARY SEARCH.
    CHECK sy-subrc = 0. "Si el tipo de psob está configurado para mostrar aunque la partida esté compensada entonces mostrar
    ls_main-opbel = ls_dfkkop-opbel.
    ls_main-psobtyp  = ls_dfkkop-psobtyp.
    "Obtener nombre del concepto
    READ TABLE lt_tpsob001t INTO DATA(ls_tpsob001t) WITH KEY psobtyp = ls_dfkkop-psobtyp.
    IF sy-subrc = 0.
      WRITE ls_dfkkop-betrh TO lv_monto_str CURRENCY ls_dfkkop-waers DECIMALS 0.
      CONDENSE lv_monto_str.
      ls_main-psobtypt = ls_tpsob001t-psobtypt && ` ` && lv_monto_str.
    ENDIF.
    APPEND ls_main TO gt_main.
  ENDLOOP.
ENDFORM.

FORM  f_obtener_posicion_plan_pagos
     USING p_t_docs TYPE fkkinv_opbel_tab
           p_pospp TYPE opupw_kk
     CHANGING p_financiacion TYPE check.

  DATA: "it_installment_fkkop TYPE STANDARD TABLE OF bapifkkop,
    ls_return      TYPE bapireturn,
    lv_bapi_amount TYPE bapicurr_d,
    lv_sap_amount  TYPE bapicurr_d,
    ls_main        TYPE zedu_s_liquidacion_pago_items,
    lv_value       TYPE betrw_kk,
    lt_intereses   TYPE zedu_t_intereses_plan_pagos.


  READ TABLE p_t_docs INTO DATA(ls_t_docs) INDEX 1.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'Z_EDU_INTERESES_PLAN_PAGOS'
    EXPORTING
      i_opbel      = ls_t_docs
    IMPORTING
      et_intereses = lt_intereses.

  READ TABLE lt_intereses INTO DATA(ls_intereses) INDEX p_pospp.

  CHECK sy-subrc = 0.

  "Capital
  ls_main-opbel  = ls_intereses-opbel_plan_pago.
  ls_main-psobtypt = ls_intereses-optxt_plan_pago  && ` ` && '-' && ` ` && 'Cuota ' && ` ` && p_pospp.
  WRITE ls_intereses-betrw_plan_pago TO ls_main-cmsmvalue CURRENCY 'COP' DECIMALS 0.
  CONDENSE ls_main-cmsmvalue NO-GAPS.
  APPEND ls_main TO gt_main.

  "Intereses
  IF ls_intereses-opbel_intereses IS NOT INITIAL.
    ls_main-opbel  = ls_intereses-opbel_intereses.
    ls_main-psobtypt = ls_intereses-optxt_intereses.
    WRITE ls_intereses-betrw_intereses TO ls_main-cmsmvalue CURRENCY 'COP' DECIMALS 0.
    CONDENSE ls_main-cmsmvalue NO-GAPS.
    APPEND ls_main TO gt_main.
  ENDIF.

  "Variables Globales
  gv_total = ls_intereses-betrw_plan_pago + ls_intereses-betrw_intereses.
  gv_fecha_cuota_ini = ls_intereses-faedn.
  p_financiacion = 'X'.
ENDFORM.
