*&---------------------------------------------------------------------*
*&  Include           ZDREDU_ESTADO_ESTUDIANTES_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_plan.
  TYPES: BEGIN OF t_plan,
           objid TYPE hrp1000-objid,
           short TYPE hrp1000-short,
           stext TYPE hrp1000-stext,
         END OF t_plan.

  DATA: lt_value_tab  TYPE TABLE OF t_plan,
        lt_return_tab TYPE TABLE OF ddshretval,
        lr_plan       TYPE RANGE OF hrp1000-objid,
        ls_plan       LIKE LINE OF lr_plan,
        lt_plan       TYPE TABLE OF hrp1001-sobid,
        ls_plan2      TYPE hrp1001-sobid,
        lt_tipo_pre   TYPE TABLE OF dynpread,
        ls_tipo_pre   TYPE dynpread,
        lv_tipo_pre   TYPE hrp1001-objid,
        ls_return_tab TYPE ddshretval.

  ls_tipo_pre-fieldname = 'P_TIPPRE'.
  APPEND ls_tipo_pre TO lt_tipo_pre.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_tipo_pre.

  READ TABLE lt_tipo_pre INTO ls_tipo_pre INDEX 1.
  IF sy-subrc IS INITIAL.
    lv_tipo_pre = ls_tipo_pre-fieldvalue.
  ENDIF.


  IF lv_tipo_pre IS NOT INITIAL.
    SELECT sobid
      FROM hrp1001
      INTO TABLE lt_plan
      WHERE plvar EQ '01'
        AND otype EQ 'O'
        AND objid EQ lv_tipo_pre
        AND endda EQ '99991231'
        AND sclas EQ 'SC'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_plan INTO ls_plan2.
        ls_plan-sign   = 'I'.
        ls_plan-option = 'EQ'.
        ls_plan-low    = ls_plan2.
        APPEND ls_plan TO lr_plan.
      ENDLOOP.

      SELECT objid
             short
             stext
        INTO TABLE lt_value_tab
        FROM hrp1000
        WHERE plvar  EQ '01'
          AND otype  EQ 'SC'
          AND objid  IN lr_plan
          AND hrp1000~endda  EQ '99991231'.
      IF sy-subrc IS NOT INITIAL.
        REFRESH lt_value_tab.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc IS INITIAL.
    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.
    s_plan-low    = ls_return_tab-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_TIPO_PREGRA_POSGRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_tipo_pregra_posgra .

  TYPES: BEGIN OF t_tipo_pre,
           objid TYPE hrp1000-objid,
           short TYPE hrp1000-short,
           stext TYPE hrp1000-stext,
         END OF t_tipo_pre.

  DATA: lt_value_tab  TYPE TABLE OF t_tipo_pre,
        lt_return_tab TYPE TABLE OF ddshretval,
        lr_tipo_pre   TYPE RANGE OF hrp1000-objid,
        ls_tipo_pre   LIKE LINE OF lr_tipo_pre,
        lt_tipo_pre   TYPE TABLE OF hrp1001-sobid,
        ls_tipo_pre2  TYPE hrp1001-sobid,
        lt_tipo_prog  TYPE TABLE OF dynpread,
        ls_tipo_prog  TYPE dynpread,
        lv_tipo_prog  TYPE hrp1001-sobid,
        ls_return_tab TYPE ddshretval.


  ls_tipo_prog-fieldname = 'P_TIPPRO'.
  APPEND ls_tipo_prog TO lt_tipo_prog.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_tipo_prog.

  READ TABLE lt_tipo_prog INTO ls_tipo_prog INDEX 1.
  IF sy-subrc IS INITIAL.
    lv_tipo_prog = ls_tipo_prog-fieldvalue.
  ENDIF.


  IF lv_tipo_prog IS NOT INITIAL.
    SELECT sobid AS low
      FROM hrp1001
      INTO TABLE lt_tipo_pre
      WHERE plvar EQ '01'
        AND otype EQ 'O'
        AND objid EQ lv_tipo_prog
        AND endda EQ '99991231'
        AND sclas EQ 'O'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_tipo_pre INTO ls_tipo_pre2.
        ls_tipo_pre-sign   = 'I'.
        ls_tipo_pre-option = 'EQ'.
        ls_tipo_pre-low    = ls_tipo_pre2.
        APPEND ls_tipo_pre TO lr_tipo_pre.
      ENDLOOP.
    ENDIF.


    SELECT hrp1000~objid
           hrp1000~short
           hrp1000~stext
      INTO TABLE lt_value_tab
      FROM hrp1000
      INNER JOIN hrp1222
      ON hrp1000~objid EQ hrp1222~objid
      INNER JOIN hrt1222
      ON hrt1222~tabnr EQ hrp1222~tabnr
      WHERE hrp1000~plvar  EQ '01'
        AND hrp1000~otype  EQ 'O'
        AND hrp1000~objid  IN lr_tipo_pre
        AND hrp1000~endda  EQ '99991231'
        AND hrp1222~endda  EQ '99991231'
        AND hrp1222~plvar  EQ '01'
        AND hrp1222~otype  EQ 'O'
        AND hrp1222~subty  EQ '9000'
        AND hrp1222~endda  EQ '99991231'
        AND hrt1222~attrib EQ 'ZSLCM_O'
        AND hrt1222~low    IN ('003',
                               '004',
                               '006',
                               '007',
                               '008' ).
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_value_tab.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.
    p_tippre   = ls_return_tab-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_FACULTAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_facultad.
  TYPES: BEGIN OF t_facu,
           objid TYPE hrp1000-objid,
           short TYPE hrp1000-short,
           stext TYPE hrp1000-stext,
         END OF t_facu.

  DATA: lt_value_tab  TYPE TABLE OF t_facu,
        lt_return_tab TYPE TABLE OF ddshretval,
        ls_return_tab TYPE ddshretval.


  SELECT hrp1000~objid
         hrp1000~short
         hrp1000~stext
    FROM hrp1000
    INNER JOIN hrp1222
    ON hrp1000~objid EQ hrp1222~objid
    INNER JOIN hrt1222
    ON hrt1222~tabnr EQ hrp1222~tabnr
    INTO TABLE lt_value_tab
    WHERE hrp1000~otype  EQ 'O'          AND
          hrp1000~plvar  EQ '01'         AND
          hrp1000~endda  EQ '99991231'   AND
          hrp1222~subty  EQ '9000'       AND
          hrt1222~attrib EQ 'ZSLCM_O'    AND
*          hrt1222~low    EQ '001'.
**** Begin of Jhon Santana
          hrt1222~low   IN ( '001' , '009' , '010' ).
**** End of Jhon Santana
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_value_tab.
  ENDIF.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.
    p_facul    = ls_return_tab-fieldval.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_TIPO_PROGRAMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_tipo_programa.
  TYPES: BEGIN OF t_tip_programa,
           objid TYPE hrp1222-objid,
           short TYPE hrp1000-short,
           stext TYPE hrp1000-stext,
         END OF t_tip_programa.

  DATA: lt_value_tab  TYPE TABLE OF t_tip_programa,
        lt_return_tab TYPE TABLE OF ddshretval,
        ls_return_tab TYPE ddshretval,
        lv_facul      TYPE hrp1000-objid,
        lt_tipo_pro   TYPE TABLE OF hrp1001-sobid,
        ls_tipo_pro2  TYPE hrp1001-sobid,
        lr_tipo_pro   TYPE RANGE OF hrp1001-sobid,
        ls_tipo_pro   LIKE LINE OF lr_tipo_pro,
        lt_facultad   TYPE TABLE OF dynpread,
        ls_facultad   TYPE dynpread.

  ls_facultad-fieldname = 'P_FACUL'.
  APPEND ls_facultad TO lt_facultad.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_facultad.

  READ TABLE lt_facultad INTO ls_facultad INDEX 1.
  lv_facul = ls_facultad-fieldvalue.

  IF lv_facul IS NOT INITIAL.
    SELECT sobid AS low
      FROM hrp1001
      INTO TABLE lt_tipo_pro
      WHERE plvar EQ '01'
        AND otype EQ 'O'
        AND objid EQ lv_facul
        AND endda EQ '99991231'
        AND sclas EQ 'O'.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_tipo_pro INTO ls_tipo_pro2.
        ls_tipo_pro-sign   = 'I'.
        ls_tipo_pro-option = 'EQ'.
        ls_tipo_pro-low = ls_tipo_pro2.
        APPEND ls_tipo_pro TO lr_tipo_pro.
      ENDLOOP.
    ENDIF.


    SELECT hrp1000~objid
           hrp1000~short
           hrp1000~stext
      INTO TABLE lt_value_tab
      FROM hrp1000
      INNER JOIN hrp1222
      ON hrp1000~objid EQ hrp1222~objid
      INNER JOIN hrt1222
      ON hrt1222~tabnr EQ hrp1222~tabnr
      WHERE hrp1000~plvar  EQ '01'
        AND hrp1000~otype  EQ 'O'
        AND hrp1000~objid  IN lr_tipo_pro
        AND hrp1000~endda  EQ '99991231'
        AND hrp1222~plvar  EQ '01'
        AND hrp1222~otype  EQ 'O'
        AND hrp1222~subty  EQ '9000'
        AND hrp1222~endda  EQ '99991231'
        AND hrt1222~attrib EQ 'ZSLCM_O'
        AND hrt1222~low    IN ('002',
                               '005' ).
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_value_tab.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_value_tab
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc IS INITIAL.
    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.
    p_tippro    = ls_return_tab-fieldval.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data.
  TYPES: BEGIN OF t_tipo_inscripcion,
           objid  TYPE hrp1001-objid,
           sobid  TYPE hrp1001-sobid,
           objid2 TYPE hrp1001-objid,
         END OF t_tipo_inscripcion,

         BEGIN OF t_estado,
           objid     TYPE hrp1771-objid,
           pr_status TYPE hrp1771-pr_status,
           prs_state TYPE hrp1771-prs_state,
         END OF t_estado,

         BEGIN OF t_but000,
           partner    TYPE partner,
           name_first TYPE name_first,
           namemiddle TYPE namemiddle,
           name_last  TYPE name_last,
           name_lst2  TYPE but000-name_lst2,
         END OF t_but000,

         BEGIN OF t_matricula,
           stobjid   TYPE cmacbpst-stobjid,
           student12 TYPE cmacbpst-student12,
           partner   TYPE cmacbpst-partner,
         END OF t_matricula,

         BEGIN OF t_but0id,
           partner  TYPE but0id-partner,
           type     TYPE but0id-type,
           idnumber TYPE but0id-idnumber,
         END OF t_but0id,

         BEGIN OF t_tipo_aspirante,
           adatanr    TYPE hrpad530-adatanr,
           adm_categ  TYPE hrpad530-adm_categ,
           adm_categt TYPE t7piqadmcategt-adm_categt,
         END OF t_tipo_aspirante,

         BEGIN OF t_nivel,
           objid      TYPE hrp1737-objid,
           prcl       TYPE hrt1737-prcl,
           progclasst TYPE t7piqprogclasst-progclasst,
         END OF t_nivel,

         BEGIN OF t_creditos,
           objid   TYPE hrp1001-objid,
           sobid   TYPE hrp1001-sobid,
           adatanr TYPE hrp1001-adatanr,
         END OF t_creditos,

         BEGIN OF t_creditos_det,
           adatanr  TYPE hrpad506-adatanr,
           cpattemp TYPE hrpad506-cpattemp,
         END OF t_creditos_det,

         BEGIN OF t_bloqueos,
           objid TYPE hrp1728-objid,
           subty TYPE hrp1728-subty,
         END OF t_bloqueos,

         BEGIN OF t_dfkkop,
           gpart   TYPE dfkkop-gpart,
           opbel   TYPE dfkkop-opbel,
           opupk   TYPE dfkkop-opupk,
           psobtyp TYPE dfkkop-psobtyp,
           blart   TYPE dfkkop-blart,
           faedn   TYPE dfkkop-faedn,
           augst   TYPE dfkkop-augst,
           persl   TYPE dfkkop-persl,
           betrw   TYPE dfkkop-betrw,
           stakz   TYPE dfkkop-stakz,
           augrd   TYPE dfkkop-augrd,
         END OF t_dfkkop,

         BEGIN OF t_instpln_head,
           rpnum TYPE fkk_instpln_head-rpnum,
           rpcat TYPE fkk_instpln_head-rpcat,
         END OF t_instpln_head,

         BEGIN OF t_estudiantes,
           objid   TYPE hrp1001-objid,
           sobid   TYPE hrp1001-sobid,
           adatanr TYPE hrp1001-adatanr,
*	Begin	-->	MgM DCEK901255  19/10/2016
           sobid_n TYPE piqstudent,
*	End	  -->	MgM DCEK901255
         END OF t_estudiantes,

         BEGIN OF t_plan_descrip,
           objid TYPE hrp1000-objid,
           stext TYPE hrp1000-stext,
         END OF t_plan_descrip.

  DATA: lt_tipo_inscip    TYPE TABLE OF t_tipo_inscripcion,
        lt_estudiantes    TYPE TABLE OF t_estudiantes,
        lt_tipo_aspirante TYPE TABLE OF t_tipo_aspirante,
        lt_matricula      TYPE TABLE OF t_matricula,
        lt_creditos       TYPE TABLE OF t_creditos,
        lt_creditos_det   TYPE TABLE OF t_creditos_det,
        lt_plan_descrip   TYPE TABLE OF t_plan_descrip,
        lt_nivel          TYPE TABLE OF t_nivel,
        lt_but000         TYPE TABLE OF t_but000,
        lt_estado         TYPE TABLE OF t_estado,
        lt_dfkkop         TYPE TABLE OF t_dfkkop,
        lt_instpln_head   TYPE TABLE OF t_instpln_head,
        lt_tipo_bloqueos  TYPE TABLE OF v_t7piqhsstatush,
        lt_bloqueos       TYPE TABLE OF t_bloqueos,
        lt_tfk062at       TYPE TABLE OF tfk062at,
        lt_tpsob001t      TYPE TABLE OF tpsob001t,
        lt_but0id         TYPE TABLE OF t_but0id,
        ls_estudiantes    TYPE t_estudiantes,
        ls_tipo_aspirante TYPE t_tipo_aspirante,
        ls_matricula      TYPE t_matricula,
        ls_creditos       TYPE t_creditos,
        ls_creditos_det   TYPE t_creditos_det,
        ls_nivel          TYPE t_nivel,
        ls_but000         TYPE t_but000,
        ls_estado         TYPE t_estado,
        ls_plan_descrip   TYPE t_plan_descrip,
        ls_dfkkop         TYPE t_dfkkop,
        ls_instpln_head   TYPE t_instpln_head,
        ls_tipo_bloqueos  TYPE v_t7piqhsstatush,
        ls_bloqueos       TYPE t_bloqueos,
        ls_tfk062at       TYPE tfk062at,
        ls_tpsob001t      TYPE tpsob001t,
        ls_but0id         TYPE t_but0id,
        ls_tipo_inscip    TYPE t_tipo_inscripcion,
        ls_alv            TYPE zedu_s_rep_estado_estudi,
        lr_augrd          TYPE RANGE OF dfkkop-augrd,
        ls_augrd          LIKE LINE OF lr_augrd,
        ls_augst          TYPE RANGE OF dfkkop-augst,
        ls_xmanl          TYPE RANGE OF dfkkop-xmanl.

*	Begin	-->	MgM DCEK901255  19/10/2016
  FIELD-SYMBOLS <fs_est> TYPE t_estudiantes.
*	End	  -->	MgM DCEK901255

  SELECT objid sobid
    FROM hrp1001
    INTO TABLE lt_tipo_inscip
    WHERE plvar EQ '01' AND
          otype EQ 'SC' AND
          sclas EQ 'CS' AND
          objid IN s_plan.

  IF sy-subrc IS INITIAL.
    SORT lt_tipo_inscip.

    LOOP AT lt_tipo_inscip INTO ls_tipo_inscip.
      ls_tipo_inscip-objid2 = ls_tipo_inscip-sobid.
      MODIFY lt_tipo_inscip FROM ls_tipo_inscip INDEX sy-tabix.
    ENDLOOP.

    SELECT objid stext
        INTO TABLE lt_plan_descrip
        FROM hrp1000
      FOR ALL ENTRIES IN lt_tipo_inscip
        WHERE plvar  EQ '01'
          AND otype  EQ 'SC'
          AND objid  EQ lt_tipo_inscip-objid
          AND hrp1000~endda  EQ '99991231'.
    IF sy-subrc IS INITIAL.
      SORT lt_plan_descrip.
    ENDIF.


    SELECT objid pr_status prs_state
       FROM hrp1771
      INTO TABLE lt_estado
      FOR ALL ENTRIES IN lt_tipo_inscip
      WHERE plvar EQ '01' AND
            otype EQ 'CS' AND
*            BEGDA: Menor o igual a la fecha clave de consulta de los parámetros de entrada
*            ENDDA: Mayor o igual a la fecha clave de consulta de los parámetros de entrada
             objid EQ lt_tipo_inscip-objid2 AND
             ayear EQ p_peryr AND
             perid EQ p_perid.
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_estado.
    ELSE.

      SORT lt_estado.

      SELECT objid sobid adatanr
         FROM hrp1001
        INTO TABLE lt_estudiantes
        FOR ALL ENTRIES IN lt_estado
        WHERE plvar EQ '01' AND
              otype EQ 'CS' AND
              objid EQ lt_estado-objid AND
              sclas EQ 'ST' AND
              rsign EQ 'B'  AND
              relat EQ '530'.
      IF sy-subrc IS NOT INITIAL.
        REFRESH lt_estudiantes.
      ELSE.

*	Begin	-->	MgM DCEK901255  19/10/2016
        "movemos dato de sobid a numérico
        LOOP AT lt_estudiantes
          ASSIGNING <fs_est>.
          <fs_est>-sobid_n  = <fs_est>-sobid.
        ENDLOOP.
*	End	  -->	MgM DCEK901255

        SORT lt_estudiantes BY sobid_n.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK lt_estudiantes IS NOT INITIAL.

****  Número de matrícula:
  SELECT stobjid student12 partner
    FROM cmacbpst
    INTO TABLE lt_matricula
     FOR ALL ENTRIES IN lt_estudiantes
    WHERE stobjid EQ lt_estudiantes-sobid_n. " objid. "DCEK901255

  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_matricula.
  ELSE.
*** Nombres y apellidos
    SELECT partner name_first namemiddle name_last name_lst2
      INTO TABLE lt_but000
      FROM but000
      FOR ALL ENTRIES IN lt_matricula
      WHERE partner EQ lt_matricula-partner.
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_but000.
    ELSE.
      SORT lt_but000.
    ENDIF.

****  Número de documento:
    SELECT partner type idnumber
      FROM but0id
      INTO TABLE lt_but0id
      FOR ALL ENTRIES IN lt_matricula
       WHERE partner EQ lt_matricula-partner.
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_but0id.
    ELSE.
      SORT lt_but0id.
    ENDIF.
  ENDIF.

***   Tipo Aspirante
  SELECT hrpad530~adatanr hrpad530~adm_categ adm_categt
    INTO TABLE lt_tipo_aspirante
    FROM hrpad530
    INNER JOIN t7piqadmcategt
    ON  hrpad530~adm_categ EQ t7piqadmcategt~adm_categ
    FOR ALL ENTRIES IN lt_estudiantes
    WHERE adatanr EQ lt_estudiantes-adatanr.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_tipo_aspirante.
  ELSE.
    SORT lt_tipo_aspirante.
  ENDIF.

***Nivel
  SELECT hrp1737~objid hrt1737~prcl t7piqprogclasst~progclasst
    FROM hrp1737
    INNER JOIN hrt1737
    ON hrp1737~tabnr EQ hrt1737~tabnr
    INNER JOIN t7piqprogclasst
    ON t7piqprogclasst~progclass EQ hrt1737~prcl
    INTO TABLE lt_nivel
    FOR ALL ENTRIES IN lt_estudiantes
    WHERE hrp1737~prog_type EQ '4' AND
          hrp1737~otype     EQ 'ST' AND
          hrt1737~peryr     EQ p_peryr AND
          hrt1737~perid     EQ p_perid AND
          hrp1737~objid     EQ lt_estudiantes-sobid_n.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_nivel.
  ELSE.
    SORT lt_nivel.
  ENDIF.

******Nro. de Créditos Inscritos:
  SELECT objid sobid adatanr
    FROM hrp1001
    INTO TABLE lt_creditos
    FOR ALL ENTRIES IN lt_estudiantes
    WHERE plvar EQ '01' AND
          otype EQ 'ST' AND
          objid EQ lt_estudiantes-sobid_n AND
          sclas EQ 'SM'.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_creditos.
  ELSE.
    SORT lt_creditos.
  ENDIF.

  SELECT adatanr cpattemp
    FROM hrpad506
    INTO TABLE lt_creditos_det
    FOR ALL ENTRIES IN lt_creditos
    WHERE adatanr  EQ lt_creditos-adatanr AND
          peryr    EQ p_peryr AND
          perid    EQ p_perid AND
        ( smstatus EQ '01' OR
          smstatus EQ '02' OR
          smstatus EQ '03' ).
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_creditos_det.
  ELSE.
    SORT lt_creditos_det.
  ENDIF.

  CALL FUNCTION 'VIEW_GET_DATA'
    EXPORTING
      view_name = 'V_T7PIQHSSTATUSH'
    TABLES
      data      = lt_tipo_bloqueos.

  IF lt_tipo_inscip IS NOT INITIAL.
    SELECT objid subty
      FROM hrp1728
      INTO TABLE lt_bloqueos
      FOR ALL ENTRIES IN lt_tipo_inscip
      WHERE plvar EQ '01' AND
            otype EQ 'CS' AND
            objid EQ lt_tipo_inscip-objid2 AND
            endda >= sy-datum AND
            hs_state = 'A'.
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_bloqueos.
    ELSE.
      SORT lt_bloqueos.
    ENDIF.
  ENDIF.

  SELECT gpart opbel opupk psobtyp blart faedn augst persl betrw stakz augrd
    FROM dfkkop
    INTO TABLE lt_dfkkop
    FOR ALL ENTRIES IN lt_matricula
    WHERE augst IN ls_augst AND
          gpart EQ lt_matricula-partner AND
          bukrs EQ c_bukrs AND
          xmanl IN ls_xmanl.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_dfkkop.
  ELSE.
    SORT lt_dfkkop.
  ENDIF.


  ls_augrd-sign   = 'I'.
  ls_augrd-option = 'EQ'.
  ls_augrd-low    = '05'.
  APPEND ls_augrd TO lr_augrd.
  ls_augrd-low    = '06'.
  APPEND ls_augrd TO lr_augrd.
  ls_augrd-low    = '11'.
  APPEND ls_augrd TO lr_augrd.
  ls_augrd-low    = ''.
  APPEND ls_augrd TO lr_augrd.

*  DELETE lt_dfkkop WHERE persl NE p_peryr    OR
*                         stakz NE abap_false OR
*                         augrd IN lr_augrd.


  IF lt_dfkkop IS NOT INITIAL.

    SELECT rpnum rpcat
      FROM fkk_instpln_head
      INTO TABLE lt_instpln_head
      FOR ALL ENTRIES IN lt_dfkkop
      WHERE rpnum = lt_dfkkop-opbel.
    IF sy-subrc IS NOT INITIAL.
      REFRESH lt_instpln_head.
    ELSE.
      SORT lt_instpln_head.
    ENDIF.

  ENDIF.

  SELECT * FROM tfk062at
    INTO TABLE lt_tfk062at
    WHERE spras EQ sy-langu.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_tfk062at.
  ELSE.
    SORT lt_tfk062at.
  ENDIF.

  SELECT * FROM tpsob001t
    INTO TABLE lt_tpsob001t
    WHERE spras EQ sy-langu.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_tpsob001t.
  ELSE.
    SORT lt_tpsob001t.
  ENDIF.

  LOOP AT lt_matricula INTO ls_matricula.
    CLEAR ls_alv.
    READ TABLE lt_estudiantes INTO ls_estudiantes
      WITH KEY sobid_n = ls_matricula-stobjid
      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      ls_alv-student12 = ls_matricula-student12.
      READ TABLE lt_but0id INTO ls_but0id
        WITH KEY partner = ls_matricula-partner
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_alv-type     = ls_but0id-type.
        ls_alv-idnumber = ls_but0id-idnumber.
      ENDIF.
      READ TABLE lt_but000 INTO ls_but000
        WITH KEY partner = ls_matricula-partner
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        CONCATENATE ls_but000-name_first ls_but000-namemiddle INTO ls_alv-name_first SEPARATED BY space.
        CONCATENATE ls_but000-name_last ls_but000-name_lst2 INTO ls_alv-name_last SEPARATED BY space.
      ENDIF.

      ls_alv-peryr = p_peryr.
      ls_alv-perid = p_perid.

      READ TABLE lt_nivel INTO ls_nivel
        WITH KEY objid = ls_estudiantes-sobid_n
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_alv-prcl = ls_nivel-prcl.
      ENDIF.

      READ TABLE lt_tipo_aspirante INTO ls_tipo_aspirante
        WITH KEY adatanr = ls_estudiantes-adatanr
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_alv-adm_categ  = ls_tipo_aspirante-adm_categ.
        ls_alv-adm_categt = ls_tipo_aspirante-adm_categt.
      ENDIF.

      READ TABLE lt_creditos INTO ls_creditos
        WITH KEY objid = ls_estudiantes-sobid_n
                 BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_creditos_det INTO ls_creditos_det
          WITH KEY adatanr = ls_creditos-adatanr
          BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_alv-cpattemp = ls_creditos_det-cpattemp.
        ENDIF.
      ENDIF.

      LOOP AT lt_dfkkop INTO ls_dfkkop WHERE gpart = ls_matricula-partner.
        ls_alv-faedn = ls_dfkkop-faedn.

        IF ls_dfkkop-psobtyp EQ 'A010' AND
           ls_dfkkop-augst   EQ '9'.
          ADD ls_dfkkop-betrw TO ls_alv-val_matricula.
        ENDIF.

        IF ls_dfkkop-psobtyp EQ 'A011' AND
           ls_dfkkop-augst   EQ '9'.
          ADD ls_dfkkop-betrw TO ls_alv-val_recargo.
        ENDIF.

        IF ls_dfkkop-psobtyp CP 'D*' AND
           ls_dfkkop-augst   EQ '9'.
          ADD ls_dfkkop-betrw TO ls_alv-val_beneficio.
          READ TABLE lt_tpsob001t INTO ls_tpsob001t
            WITH KEY psobtyp = ls_dfkkop-psobtyp
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_alv-nombre_beneficio = ls_tpsob001t-psobtypt.
          ENDIF.
        ENDIF.

        IF ls_dfkkop-persl CP 'I1*' OR
           ls_dfkkop-persl CP 'I2*' AND
           ls_dfkkop-augst EQ '9'.
          ADD ls_dfkkop-betrw TO ls_alv-val_inter_sem.
        ENDIF.

        IF ls_dfkkop-augst = ' ' AND
           ls_dfkkop-betrw < 0.
          ADD ls_dfkkop-betrw TO ls_alv-val_saldo_favor.
        ENDIF.

        IF ls_dfkkop-blart EQ 'PC' OR
           ls_dfkkop-blart EQ 'PB' OR
           ls_dfkkop-blart EQ 'CD' OR
           ls_dfkkop-blart EQ 'CN' AND
           ls_dfkkop-augst EQ '9'.
          ADD ls_dfkkop-betrw TO ls_alv-val_real_pagado.
        ENDIF.
        IF ls_dfkkop-blart EQ 'PP'.
          ADD ls_dfkkop-betrw TO ls_alv-val_financiado.
          READ TABLE lt_instpln_head INTO ls_instpln_head
            WITH KEY rpnum = ls_dfkkop-opbel
            BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            READ TABLE lt_tfk062at INTO ls_tfk062at
              WITH KEY rpcat = ls_instpln_head-rpcat
              BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_alv-nombre_financia = ls_tfk062at-rpctxt.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ls_dfkkop-blart EQ 'IN'.
          ADD ls_dfkkop-betrw TO ls_alv-val_interes.
        ENDIF.

        IF ls_dfkkop-psobtyp  CP 'O*'.
          ADD ls_dfkkop-betrw TO ls_alv-val_otras_deudas.
        ENDIF.

        IF ls_dfkkop-augst = ' '.
          ADD ls_dfkkop-betrw TO ls_alv-val_saldo_actual.
        ENDIF.

      ENDLOOP.

      READ TABLE lt_estado INTO ls_estado
        WITH KEY objid = ls_estudiantes-objid
        BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF ls_estado-prs_state EQ 'A'.
          ls_alv-estado_matricula = 'Activos'.
        ELSEIF ls_estado-prs_state EQ 'I'.
          ls_alv-estado_matricula = 'Inactivos'.
        ENDIF.

        READ TABLE lt_tipo_inscip INTO ls_tipo_inscip
        WITH KEY objid2 = ls_estado-objid
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          READ TABLE lt_plan_descrip INTO ls_plan_descrip
                                                 WITH KEY objid = ls_tipo_inscip-objid
                                                 BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_alv-plan = ls_plan_descrip-stext.
          ENDIF.
          READ TABLE lt_bloqueos INTO ls_bloqueos
             WITH KEY objid = ls_tipo_inscip-objid2
             BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_alv-subty = ls_bloqueos-subty.
            READ TABLE lt_tipo_bloqueos INTO ls_tipo_bloqueos
              WITH KEY subty = ls_bloqueos-subty
              BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_alv-sutxt = ls_tipo_bloqueos-sutxt.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_alv TO gt_alv.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_alv .
  DATA: ls_layout TYPE slis_layout_alv.

  ls_layout-colwidth_optimize = abap_true.
  IF gt_alv IS NOT INITIAL .

    PERFORM f_get_catalog.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
        it_fieldcat   = gt_catalog
        is_layout     = ls_layout
      TABLES
        t_outtab      = gt_alv
      EXCEPTIONS
        program_error = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
    ENDIF.
  ELSE.
    MESSAGE 'No se encontraron datos' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_catalog.
  DATA lv_fieldname TYPE string.
  FIELD-SYMBOLS:
     <l_catalog> TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZEDU_S_REP_ESTADO_ESTUDI'
    CHANGING
      ct_fieldcat            = gt_catalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT gt_catalog ASSIGNING <l_catalog>.
    lv_fieldname = <l_catalog>-fieldname.

    CLEAR <l_catalog>.

    <l_catalog>-fieldname = lv_fieldname.
    CASE <l_catalog>-fieldname.
      WHEN c_student12.
        <l_catalog>-seltext_l = text-004.
      WHEN c_type.
        <l_catalog>-seltext_l = text-005.
      WHEN c_idnumber.
        <l_catalog>-seltext_l = text-006.
      WHEN c_name_last.
        <l_catalog>-seltext_l = text-007.
      WHEN c_name_first.
        <l_catalog>-seltext_l = text-008.
      WHEN c_plan.
        <l_catalog>-seltext_l = text-009.
      WHEN c_peryr.
        <l_catalog>-seltext_l = text-010.
      WHEN c_perid.
        <l_catalog>-seltext_l = text-011.
      WHEN c_prcl.
        <l_catalog>-seltext_l = text-012.
      WHEN c_adm_categ.
        <l_catalog>-seltext_l = text-013.
        <l_catalog>-no_out = 'X'.
      WHEN c_adm_categt.
        <l_catalog>-seltext_l = text-014.
      WHEN c_cpattemp.
        <l_catalog>-seltext_l = text-015.
      WHEN c_faedn.
        <l_catalog>-seltext_l = text-016.
      WHEN c_val_matricula.
        <l_catalog>-seltext_l = text-017.
      WHEN c_val_recargo.
        <l_catalog>-seltext_l = text-018.
      WHEN c_val_beneficio.
        <l_catalog>-seltext_l = text-019.
      WHEN c_nombre_beneficio.
        <l_catalog>-seltext_l = text-020.
      WHEN c_val_inter_sem.
        <l_catalog>-seltext_l = text-021.
      WHEN c_val_saldo_favor.
        <l_catalog>-seltext_l = text-022.
      WHEN c_val_real_pagado.
        <l_catalog>-seltext_l = text-023.
      WHEN c_val_financiado.
        <l_catalog>-seltext_l = text-024.
      WHEN c_nombre_financia.
        <l_catalog>-seltext_l = text-025.
      WHEN c_val_interes.
        <l_catalog>-seltext_l = text-026.
      WHEN c_val_otras_deudas.
        <l_catalog>-seltext_l = text-027.
      WHEN c_val_saldo_actual.
        <l_catalog>-seltext_l = text-028.
      WHEN c_subty.
        <l_catalog>-seltext_l = text-029.
      WHEN c_sutxt.
        <l_catalog>-seltext_l = text-030.
      WHEN c_estado_matricula.
        <l_catalog>-seltext_l = text-031.
      WHEN OTHERS.
        <l_catalog>-no_out = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CARGAR_TIPO_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_cargar_tipo_doc .
  TYPES: BEGIN OF t_tip_doc,
           type TYPE tb039a-type,
           text TYPE tb039b-text,
         END OF t_tip_doc.

  DATA: lt_tip_doc    TYPE TABLE OF t_tip_doc,
        lt_return_tab TYPE TABLE OF ddshretval,
        ls_return_tab TYPE ddshretval.

  SELECT tb039a~type tb039b~text
    INTO TABLE lt_tip_doc
    FROM tb039a
    INNER JOIN tb039b
    ON tb039a~type EQ tb039b~type
    WHERE tb039a~xperson EQ abap_true AND
          tb039b~spras   EQ sy-langu.
  IF sy-subrc IS NOT INITIAL.
    REFRESH lt_tip_doc.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TYPE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      value_org       = 'S'
    TABLES
      value_tab       = lt_tip_doc
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc IS INITIAL.
    READ TABLE lt_return_tab INTO ls_return_tab INDEX 1.
    p_tipdoc    = ls_return_tab-fieldval.
  ENDIF.
ENDFORM.
