class ZCL_WD_LISTA_CLASES_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  types:
    BEGIN OF ty_asig_help,
        objid  TYPE p1000-objid,
        objid2 TYPE p1000-objid,
        short  TYPE p1000-short,
        stext  TYPE p1000-stext,
        stgbeg TYPE piqstgbeg,
      END OF ty_asig_help .
  types:
    BEGIN OF ty_eve_help,
             objid  TYPE p1000-objid,
             objid2 TYPE  p1000-objid,
             short  TYPE p1000-short,
             stext  TYPE p1000-stext,
           END OF ty_eve_help .
  types:
    tt_asig_help TYPE TABLE OF ty_asig_help WITH DEFAULT KEY .
  types:
    tt_eve_help TYPE TABLE OF ty_eve_help WITH DEFAULT KEY .

  methods GET_DDBK_PRUEBAS
    importing
      !IT_PRUEBAS type ZEDU_T_PRADM
    returning
      value(RT_DDBK_PRUEBAS) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_DDBK_ANIO_PERIODO
    importing
      !IV_PROGRAMA type HROBJID optional
    returning
      value(RT_ATTR_VALUE) type ZEDU_T_CONTEXT_ATTR_VALUE .
  methods GET_NAME_USERS
    importing
      !IT_USERS type USMD_T_USER
    returning
      value(RT_USER_ADDR) type TABTYPE_USER_ADDR .
  methods SET_TEXT_COLUMN_ALV
    importing
      !IV_TEXT type WDR_TEXT_KEY
    changing
      !CS_COLUMN type SALV_WD_S_COLUMN_REF .
  methods FILL_EVENTS .
  methods FILL_DATA
    importing
      !IV_FACULTAD type HROBJID
      !IV_PROGRAMA type HROBJID
      !IV_PREGRADO type HROBJID
      !IV_ANIO type PIQPERYR
      !IV_PERIODO type PIQPERID
      !IR_ASIGNATURA type /ISDFPS/RT_HROBJID
      !IR_EVENTO type /ISDFPS/RT_HROBJID
      !IV_PLAN_ESTUDIO type HROBJID
    returning
      value(RV_DATOS) type BOOLE_D .
  methods GET_TABLE_ALV
    returning
      value(RT_SALIDA) type ZEDU_TT_LISTA_CLASES
    exceptions
      SIN_DATOS .
  methods GET_TABLE_PDF .
  methods GET_PDF_XSTRING
    returning
      value(RV_PDF_XSTRING) type XSTRING .
  methods GET_EVENTOS_OVS
    importing
      value(IR_ASIGNATURA) type /ISDFPS/RT_HROBJID optional
      !IV_ANIO type PIQPERYR optional
      !IV_PERIODO type PIQPERID optional
      !IV_PLAN_ESTUDIO type HROBJID optional
    returning
      value(RT_EVENTOS) type TT_EVE_HELP .
  methods GET_ASIGNATURAS_OVS
    importing
      !IV_PLAN_ESTUDIO type HROBJID optional
    returning
      value(RT_ASIGNATURAS) type TT_ASIG_HELP .
protected section.
private section.

*  types:
*    BEGIN OF ty_admi_1,
*      nr_formulario TYPE zpre_admi_1-nr_formulario,
*      tipo_documen  TYPE zpre_admi_1-tipo_documen,
*      nro_documen   TYPE zpre_admi_1-nro_documen,
*      nombre        TYPE zpre_admi_1-nombre,
*      sdo_nombre    TYPE zpre_admi_1-sdo_nombre,
*      apellido      TYPE zpre_admi_1-apellido,
*      sdo_apellido  TYPE zpre_admi_1-sdo_apellido,
*      fech_nacim    TYPE zpre_admi_1-fech_nacim,
*      direccion     TYPE zpre_admi_1-direccion,
*      pais          TYPE zpre_admi_1-pais,
*      departamento  TYPE zpre_admi_1-departamento,
*      municipio     TYPE zpre_admi_1-municipio,
*      estrato       TYPE zpre_admi_1-estrato,
*      tel_movil     TYPE zpre_admi_1-tel_movil,
*      tel_fijo      TYPE zpre_admi_1-tel_fijo,
*      email         TYPE zpre_admi_1-email,
*      sdo_email     TYPE zpre_admi_1-sdo_email,
*      genero        TYPE zpre_admi_1-genero,
*      estado_civil  TYPE zpre_admi_1-estado_civil,
*      eps           TYPE zpre_admi_1-eps,
**  Begin --> MgM DCEK901698 ajuste x renombre de campo ciudad_alter 23/11/2016
**      ciudad_alter  TYPE zpre_admi_1-ciudad_alter,
*      ciudad_alter  TYPE zpre_admi_1-ciudad_fuera,
**  End   --> MgM DCEK901698
*    END OF ty_admi_1 .
*  types:
*    BEGIN OF ty_admi_2,
*      nr_formulario  TYPE zpre_admi_2-nr_formulario,
*      facultad       TYPE zpre_admi_2-facultad,
*      tipo_program   TYPE zpre_admi_2-tipo_program,
*      t_posgrado     TYPE zpre_admi_2-t_posgrado,
*      sede           TYPE zpre_admi_2-sede,
*      periodo_acad   TYPE zpre_admi_2-periodo_acad,
*      tipo_aspirante TYPE zpre_admi_2-tipo_aspirante,
*      programa_1     TYPE zpre_admi_2-programa_1,
*      programa_2     TYPE zpre_admi_2-programa_2,
*      medio          TYPE zpre_admi_2-medio,
*    END OF ty_admi_2 .
*  types:
*    BEGIN OF ty_admi_3,
*      nr_formulario TYPE zpre_admi_3-nr_formulario,
*      registro_snp  TYPE zpre_admi_3-registro_snp,
*      colegio_proc  TYPE zpre_admi_3-colegio_proc,
*      ciudad_coleg  TYPE zpre_admi_3-ciudad_coleg,
*      titulo_coleg  TYPE zpre_admi_3-insti_idioma,
*      ano_grado     TYPE zpre_admi_3-ano_grado,
*      univ_procede  TYPE zpre_admi_3-univ_procede,
*      titulo_univ   TYPE zpre_admi_3-titulo_univ,
*      ciudad_univ   TYPE zpre_admi_3-ciudad_univ,
*      ano_gruni     TYPE zpre_admi_3-ano_gruni,
*      idioma        TYPE zpre_admi_3-idioma,
*      insti_idioma  TYPE zpre_admi_3-insti_idioma,
*      ciudad_idio   TYPE zpre_admi_3-ciudad_idio,
*      prueba        TYPE zpre_admi_3-prueba,
*      punta_idioma  TYPE zpre_admi_3-punta_idioma,
*      curso         TYPE zpre_admi_3-curso,
*      insti_curso   TYPE zpre_admi_3-insti_curso,
*      ciudad_curs   TYPE zpre_admi_3-ciudad_curs,
*      titulo_curs   TYPE zpre_admi_3-titulo_curs,
*    END OF ty_admi_3 .
*  types:
*    BEGIN OF ty_admi_4,
*      nr_formulario TYPE zpre_admi_4-nr_formulario,
*      vivecon       TYPE zpre_admi_4-vivecon,
*    END OF ty_admi_4 .
*  types:
*    BEGIN OF ty_admi_5,
*      nr_formulario TYPE zpre_admi_5-nr_formulario,
*      empresa       TYPE zpre_admi_5-empresa,
*      cargo         TYPE zpre_admi_5-cargo,
*      fech_inic     TYPE zpre_admi_5-fech_inic,
*      fech_fina     TYPE zpre_admi_5-fech_fina,
*      duracion      TYPE zpre_admi_5-duracion,
*      partic_inv    TYPE zpre_admi_5-partic_inv,
*      nomb_inv      TYPE zpre_admi_5-nomb_inv,
*      publicacion   TYPE zpre_admi_5-publicacion,
*      nomb_inv2     TYPE zpre_admi_5-nomb_inv2,
*    END OF ty_admi_5 .
  types:
    begin of ty_range_nr_formulario,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-nr_formulario,
      high   type zpre_admi_1-nr_formulario,
    end of ty_range_nr_formulario .
  types:
    begin of ty_range_programa,
      sign   type sign,
      option type option,
      low    type zpre_admi_2-tipo_program,
      high   type zpre_admi_2-tipo_program,
    end of ty_range_programa .
  types:
    begin of ty_range_facultad,
      sign   type sign,
      option type option,
      low    type zpre_admi_2-facultad,
      high   type zpre_admi_2-facultad,
    end of ty_range_facultad .
  types:
    begin of ty_range_periodo,
      sign   type sign,
      option type option,
      low    type zpre_admi_2-periodo_acad,
      high   type zpre_admi_2-periodo_acad,
    end of ty_range_periodo .
  types:
    begin of ty_range_plan_estudio,
      sign   type sign,
      option type option,
      low    type zpre_admi_2-programa_1,
      high   type zpre_admi_2-programa_1,
    end of ty_range_plan_estudio .
  types:
    begin of ty_range_preposgrado,
      sign   type sign,
      option type option,
      low    type zpre_admi_2-programa_1,
      high   type zpre_admi_2-programa_1,
    end of ty_range_preposgrado .
  types:
    begin of ty_range_nro_documen,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-nro_documen,
      high   type zpre_admi_1-nro_documen,
    end of ty_range_nro_documen .
  types:
    begin of ty_range_nombre,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-nombre,
      high   type zpre_admi_1-nombre,
    end of ty_range_nombre .
  types:
    begin of ty_range_sdo_nombre,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-sdo_nombre,
      high   type zpre_admi_1-sdo_nombre,
    end of ty_range_sdo_nombre .
  types:
    begin of ty_range_apellido,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-apellido,
      high   type zpre_admi_1-apellido,
    end of ty_range_apellido .
  types:
    begin of ty_range_sdo_apellido,
      sign   type sign,
      option type option,
      low    type zpre_admi_1-sdo_apellido,
      high   type zpre_admi_1-sdo_apellido,
    end of ty_range_sdo_apellido .
*  types:
*    tt_admi_1 TYPE TABLE OF ty_admi_1 .
*  types:
*    tt_admi_2 TYPE TABLE OF ty_admi_2 .
*  types:
*    tt_admi_3 TYPE TABLE OF ty_admi_3 .
*  types:
*    tt_admi_4 TYPE TABLE OF ty_admi_4 .
*  types:
*    tt_admi_5 TYPE TABLE OF ty_admi_5 .
  types:
    tt_range_programa type table of ty_range_programa .
  types:
    tt_range_facultad type table of ty_range_programa .
  types:
    tt_range_periodo type table of ty_range_periodo .
  types:
    tt_range_preposgrado type table of ty_range_programa .
  types:
    tt_range_plan_estudio type table of ty_range_plan_estudio .
  types:
    tt_range_nro_documen type table of ty_range_nro_documen .
  types:
    tt_range_nombre type table of ty_range_nombre .
  types:
    tt_range_sdo_nombre type table of ty_range_sdo_nombre .
  types:
    tt_range_apellido type table of ty_range_apellido .
  types:
    tt_range_sdo_apellido type table of ty_range_sdo_apellido .
  types:
    tt_range_nr_formulario type table of ty_range_nr_formulario .

  data ti_encabezado type ztty_encabezado .
  data ti_final type zedu_tt_lista_clases .

  methods fill_data_huma
    importing
      !iv_facultad    type hrobjid
      !iv_anio        type piqperyr
      !iv_periodo     type piqperid
      !ir_asignatura  type /isdfps/rt_hrobjid
      !ir_evento      type /isdfps/rt_hrobjid
    returning
      value(rv_datos) type boole_d .
*	Begin	-->	MgM DCEK903365 no se utiliza 08/02/2017
*  methods cargar_rangos
*    importing
*      !iv_seleccion     type zedu_s_sel_pru_admi
*      !it_admi_2        type tt_admi_2 optional
*    exporting
*      !er_nr_formulario type tt_range_nr_formulario
*      !er_programa      type tt_range_programa
*      !er_facultad      type tt_range_facultad
*      !er_periodo       type tt_range_periodo
*      !er_preposgrado   type tt_range_preposgrado
*      !er_plan_estudio  type tt_range_plan_estudio
*      !er_nro_documento type tt_range_nro_documen
*      !er_nombre        type tt_range_nombre
*      !er_sdo_nombre    type tt_range_sdo_nombre
*      !er_apellido      type tt_range_apellido
*      !er_sdo_apellido  type tt_range_sdo_apellido .
*	End	  -->	MgM DCEK903365
  methods fill_data_others
    importing
      !iv_facultad     type hrobjid
      !iv_programa     type hrobjid
      !iv_pregrado     type hrobjid
      !iv_anio         type piqperyr
      !iv_periodo      type piqperid
      !ir_asignatura   type /isdfps/rt_hrobjid
      !ir_evento       type /isdfps/rt_hrobjid
      !iv_plan_estudio type hrobjid
    returning
      value(rv_datos)  type boole_d .
  methods formateo_horario
    importing
      !is_horario type data
    changing
      !cv_horario type data .
  methods add_day
    importing
      !iv_day      type char3
      !iv_hora_ini type string
      !iv_hora_fin type string
    changing
      !cv_horario  type data .
ENDCLASS.



CLASS ZCL_WD_LISTA_CLASES_ASS IMPLEMENTATION.


  method add_day.
    data lv_day type string.

    lv_day = iv_day.

    if cv_horario is not initial.
      concatenate cv_horario
                  `/`
                  iv_day
        into  lv_day
          separated by space.
    endif.

    concatenate lv_day
                iv_hora_ini
                '-'
                iv_hora_fin
      into cv_horario
        separated by space.

  endmethod.


  method fill_data.
*    " Data para encabezado de smartform
*    data wa_encabezado    type zty_encabezado.
**  Begin --> MgM DCEK902983 Visualizar los no pagos 27/01/2017
**    data wa_final         type zty_final.
*    data wa_final         type zedu_s_lista_clases.
**  End   --> MgM DCEK902983
*    data lv_value_string  type string.
*
*    types: begin of ty_hrp1001,
*             sobid type hrp1001-sobid,
*             objid type hrp1001-objid,
*           end of ty_hrp1001.
*
*    clear ti_encabezado[].
*    clear ti_final[].
*
*    lv_value_string = iv_facultad.
*
*    read table get_dropdown_key( im_field  = c_field_facultades )
*      into data(ls_drop) with key value = iv_facultad.
*
*    if sy-subrc eq 0.
*      wa_encabezado-facultad  = ls_drop-text.
*    endif.
*
*    read table get_dropdown_key( im_field  = c_field_programas
*                                 im_value  = lv_value_string )
*      into ls_drop with key value = iv_programa.
*
*    if sy-subrc eq 0.
*      wa_encabezado-tipoprograma  = ls_drop-text.
*    else.
*      wa_encabezado-tipoprograma  = iv_programa.
*    endif.
*
*    read table get_dropdown_key( im_field  = 'PREPOSGRADO' )
*      into ls_drop with key value = iv_pregrado.
*
*    if sy-subrc eq 0.
*      wa_encabezado-tipopregrado  = ls_drop-text.
*    endif.
*
***ARMA RANGO DE PLANES DE ESTUDIO
**  DATA: R_PLANEST TYPE RANGE OF HRP1000-OBJID,
**        WA_R_PLAN LIKE LINE OF R_PLANEST,
**        WA_PR_PLES LIKE LINE OF pr_ples.
*
**  LOOP AT pr_ples INTO WA_PR_PLES.
**    WA_R_PLAN-SIGN = WA_PR_PLES-SIGN.
**    WA_R_PLAN-OPTION = WA_PR_PLES-OPTION.
**    PERFORM LLENAR_PLAN_OBJID USING WA_PR_PLES-LOW
**                           CHANGING WA_R_PLAN-LOW.
**
**    PERFORM LLENAR_PLAN_OBJID USING WA_PR_PLES-HIGH
**                           CHANGING WA_R_PLAN-HIGH.
**    APPEND WA_R_PLAN TO R_PLANEST.
**  ENDLOOP.
*
***PLAN DE ESTUDIOS
*    select  b~objid,
*            b~short,
*            b~stext
*      into table @data(ti_plest)
*        from hrp1001 as a
*          inner join hrp1000 as b on a~sobid  = b~objid
*            where a~plvar = '01'
*              and a~otype = 'O'
*              and a~objid = @iv_pregrado
*              and a~endda = '99991231'
*              and a~sclas = 'SC'
*              and b~plvar = '01'
*              and b~otype = 'SC'
*              and b~endda = '99991231'
**              and b~objid in r_planest.
*              and b~objid eq @iv_plan_estudio.
*
***ASIGNATURAS
***ARMA RANGO DE ASIGNATURAS
**    data: r_asig type range of hrp1000-objid.
**        WA_R_ASIG LIKE LINE OF R_ASIG,
**        WA_pr_asig LIKE LINE OF pr_asig.
*
**  LOOP AT pr_asig INTO data(WA_pr_asig).
**    WA_R_ASIG-SIGN = WA_pr_asig-SIGN.
**    WA_R_ASIG-OPTION = WA_pr_asig-OPTION.
**    IF NOT WA_pr_asig-LOW IS INITIAL.
**      PERFORM LLENAR_ASIG_OBJID USING WA_pr_asig-LOW
**                             CHANGING WA_R_ASIG-LOW.
**    ENDIF.
**
**    IF NOT WA_pr_asig-HIGH IS INITIAL.
**      PERFORM LLENAR_ASIG_OBJID USING WA_pr_asig-HIGH
**                             CHANGING WA_R_ASIG-HIGH.
**    ENDIF.
**    APPEND WA_R_ASIG TO R_ASIG.
**  ENDLOOP.
*
*    select  b~objid,
*            a~objid as objid2,
*            b~short,
*            b~stext,
*            c~stgbeg  into table @data(ti_asig)
*    from hrp1001 as a
*    inner join hrp1000 as b on a~sobid  = b~objid
*    inner join hrpad500 as c on a~adatanr = c~adatanr
***      FOR ALL ENTRIES IN ti_asig_objid
*    where a~plvar = '01'
*      and a~otype = 'SC'
**      AND a~objid IN R_PLANEST "ti_asig_objid-objid
*      and a~objid eq @iv_plan_estudio "ti_asig_objid-objid
*      and a~endda = '99991231'
*      and a~sclas = 'SM'
*      and b~plvar = '01'
*      and b~otype = 'SM'
*      and b~endda = '99991231'
*      and b~objid in @ir_asignatura.
*
***ESTUDIANTES
*    types: begin of ty_st,
*             stobjid    type piqstudent,
*             student12  type piqstudent12,
*             name_first type bu_namep_f,
*             namemiddle type bu_namemid,
*             name_last  type bu_namep_l,
*             name_lst2  type bu_namepl2,
*             idnumber   type bu_id_number,
*           end of ty_st.
*
*    types: begin of ty_st_e,
*             sobid    type hrp1001-sobid,
*             objid    type hrp1001-objid,
*             st_objid type hrp1001-objid,
*           end of ty_st_e.
*
*    data: ti_st_sm type table of ty_hrp1001,
*          wa_st_sm type ty_hrp1001,
*          ti_st    type table of ty_st,
*          wa_st    type ty_st,
*          ti_st_e  type table of ty_st_e,
*          wa_st_e  type ty_st_e.
*
***EVENTOS
*
*    types: begin of ty_hrp1026,
*             objid  type hrp1026-objid,
*             delet  type hrp1026-delet,
*             cancr  type hrp1026-cancr,
*             borrar type boole_d,
*           end of ty_hrp1026.
*
*    data: ti_hrp1026 type table of ty_hrp1026,
*          wa_hrp1026 type ty_hrp1026.
*
*    select  sobid,
*            objid
*      into table @data(ti_d)
*        from hrp1001
*        for all entries in @ti_asig
*        where plvar = '01'
*          and otype = 'SM'
*          and objid = @ti_asig-objid
**        AND endda = '99991231'
*          and sclas = 'D'.
*
*    if sy-subrc = 0.
**
*      select objid, sobid
*      into table @data(ti_e)
*      from hrp1001
*      for all entries in @ti_d
*      where plvar = '01'
*        and otype = 'E'
*        and sobid = @ti_d-sobid
**      AND endda >= SY-DATUM
*        and sclas = 'D'.
**
*      if sy-subrc = 0.
**
*        select objid
*        into table @data(ti_hrp1739)
*        from hrp1739
*        for all entries in @ti_e
*        where plvar = '01'
*          and otype = 'E'
*          and objid = @ti_e-objid
**  Begin --> MgM  DCEK902833 17/01/2017
*          and objid in @ir_evento
**  End   --> MgM
*          and peryr = @iv_anio
*          and perid = @iv_periodo.
**
*        if sy-subrc = 0.
*
*          select objid delet cancr
*            into table ti_hrp1026
*              from hrp1026
*                for all entries in ti_hrp1739
*                  where plvar = '01' and
*                        otype = 'E' and
*                        objid = ti_hrp1739-objid.
**
**
*          loop at ti_hrp1026 into wa_hrp1026.
*            if wa_hrp1026-delet = 'X' and not wa_hrp1026-cancr is initial.
*              wa_hrp1026-borrar = 'X'.
*            endif.
*            modify ti_hrp1026 from wa_hrp1026.
*          endloop.
**
*          delete ti_hrp1026 where borrar = 'X'.
*
*          if not ti_hrp1026[] is initial.
*
*            select objid, short, stext
*            into table @data(ti_eve)
*            from hrp1000
*            for all entries in @ti_hrp1026
*            where plvar = '01' and
*                  otype = 'E' and
*                  objid = @ti_hrp1026-objid and
*                  plvar = '01' and
*                  otype = 'E'.
*          endif.
*        endif.
*      endif.
*    endif.
**
**  TYPES: BEGIN OF TY_HORARIO,
**           OBJID      TYPE HRP1716-OBJID,
**           MONDAY     TYPE HRT1716-MONDAY,
**           TUESDAY    TYPE HRT1716-TUESDAY,
**           WEDNESDAY  TYPE HRT1716-WEDNESDAY,
**           THURSDAY   TYPE HRT1716-THURSDAY,
**           FRIDAY     TYPE HRT1716-FRIDAY,
**           SATURDAY   TYPE HRT1716-SATURDAY,
**           SUNDAY     TYPE HRT1716-SUNDAY,
**           BEGUZ      TYPE HRT1716-BEGUZ,
**           ENDUZ      TYPE HRT1716-ENDUZ,
**         END OF TY_HORARIO.
**
**  DATA: TI_HORARIO TYPE TABLE OF TY_HORARIO,
**        WA_HORARIO TYPE TY_HORARIO.
**
**  TYPES: BEGIN OF TY_ST_CS,
**             OBJID    TYPE HRP1001-OBJID,
**             SOBID    TYPE HRP1001-SOBID,
**             ADATANR  TYPE HRP1001-ADATANR,
**           END OF TY_ST_CS.
**
**    DATA: TI_ST_CS TYPE TABLE OF TY_ST_CS,
**          WA_ST_CS TYPE TY_ST_CS.
**
***HORARIOS
*    select t1~objid, t2~monday, t2~tuesday, t2~wednesday,
*           t2~thursday, t2~friday, t2~saturday, t2~sunday,
*           t2~beguz, t2~enduz
*    into table @data(ti_horario)
*    from hrp1716 as t1
*    join hrt1716 as t2
*      on t1~tabnr = t2~tabnr
*    for all entries in @ti_eve
*    where t1~plvar = '01' and
*          t1~otype = 'E' and
*          t1~objid = @ti_eve-objid.
**
*    select objid
*           sobid
*    into corresponding fields of table ti_st_e
*    from hrp1001
*      for all entries in ti_eve
*    where plvar = '01' and
*          otype = 'E' and
*          objid = ti_eve-objid and
**        ENDDA = '99991231' AND
*          sclas = 'ST' and
*          subty = 'A025'.
**
*    loop at ti_st_e
*      into wa_st_e.
*      wa_st_e-st_objid = wa_st_e-sobid.
*      modify ti_st_e from wa_st_e.
*    endloop.
*
*    if ti_st_e[] is not initial.
*      select  a~stobjid
*              a~student12
*              b~name_first
*              b~namemiddle
*              b~name_last
*              b~name_lst2
*              c~idnumber
*        into corresponding fields of table ti_st
*          from cmacbpst as a
*            inner join but000 as b on a~partner = b~partner
*            inner join but0id as c on b~partner = c~partner
*              for all entries in ti_st_e
*                where a~stobjid = ti_st_e-st_objid.
*
*      delete adjacent duplicates from ti_st.
*
*      if ti_st[] is not initial.
**  ***BUSCA EL TIPO DE ESTUDIANTE
*        select  objid,
*                sobid,
*                adatanr
*        into table @data(ti_st_cs)
*        from hrp1001
*          for all entries in @ti_st
*        where plvar = '01' and
*              otype = 'ST' and
*              objid = @ti_st-stobjid and
*              rsign = 'A' and
*              relat = '530' and
*              sclas = 'CS'.
*      endif.
*    endif.
*
*    loop at ti_eve
*      into data(wa_eve).
*
*      read table ti_e into data(wa_e) with key objid = wa_eve-objid.
*      if sy-subrc = 0.
*        read table ti_d into data(wa_d) with key sobid = wa_e-sobid.
*        if sy-subrc = 0.
*          read table ti_asig into data(wa_asig) with key objid = wa_d-objid.
*          if sy-subrc = 0.
*            wa_encabezado-asigobjid   = wa_asig-objid.
*            wa_encabezado-asignatura  = wa_asig-stext.
*            wa_encabezado-short       = wa_asig-short.
*            wa_encabezado-plobjid     = wa_asig-objid2.
*            wa_encabezado-nivel       = wa_asig-stgbeg.
*
*            read table ti_plest into data(wa_plest) with key objid = wa_asig-objid2.
*
*            if sy-subrc = 0.
*              wa_encabezado-planestudios = wa_plest-stext.
*              wa_encabezado-programa = wa_plest-stext.
*
*              wa_encabezado-evento = wa_eve-stext.
*              wa_encabezado-eveobjid = wa_eve-objid.
*
*************CONSULTA DOCENTE
*              data: ti_resources type table of piqrfc_resources,
*                    wa_resources type piqrfc_resources.
*
*              call function 'HRIQ_RFC_GET_EVENT_DETAILS'
*                exporting
*                  event_id                  = wa_eve-objid
*                  event_otype               = 'E'
*                  plvar                     = '01'
*                  read_resources            = 'X'
*                  read_resource_description = 'X'
*                tables
*                  resources                 = ti_resources.
*
*              read table ti_resources into wa_resources with key restp = 'P'.
*              if sy-subrc = 0.
*                wa_encabezado-docente = wa_resources-resxt.
*                clear wa_encabezado-docente_id.
*                select single perid
*                into wa_encabezado-docente_id
*                from pa0002
*                where pernr = wa_resources-resid.
*
*              endif.
*
*************AULA
*              data: w_objid_d type hrp1001-objid,
*                    w_sobid_g type hrp1001-sobid,
*                    w_objid_g type hrp1001-objid.
*
**            W_OBJID_D = WA_D-objid.
*              select single sobid
*              into w_sobid_g
*              from hrp1001
*              where plvar = '01' and
*                    otype = 'E' and
*                    objid = wa_eve-objid and
*                    sclas = 'G'.
*
*              if sy-subrc = 0.
*                w_objid_g = w_sobid_g.
*                select single stext
*                into wa_encabezado-aula
*                from hrp1000
*                where plvar = '01' and
*                      otype = 'G' and
*                      objid = w_objid_g.
*              endif.
*
*************HORARIO
*              read table ti_horario
*                into data(wa_horario)
*                  with key objid = wa_eve-objid.
*
*              if sy-subrc = 0.
*                clear wa_encabezado-horario.
*                if wa_horario-monday = 'X'.
*                  concatenate 'LUN' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-tuesday = 'X'.
*                  concatenate 'MAR' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-wednesday = 'X'.
*                  concatenate 'MIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-thursday = 'X'.
*                  concatenate 'JUE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-friday = 'X'.
*                  concatenate 'VIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-saturday = 'X'.
*                  concatenate 'SAB' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-sunday = 'X'.
*                  concatenate 'DOM' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                append wa_encabezado to ti_encabezado.
*              endif.
*
*************LLENA EL DETALLE
*              clear wa_final.
*              move-corresponding wa_encabezado to wa_final.
*              wa_final-anio = iv_anio.
*              wa_final-periodo = iv_periodo.
*
*              loop at ti_st_e
*                into wa_st_e
*                  where objid = wa_eve-objid.
*
*                read table ti_st
*                  into wa_st
*                    with key stobjid = wa_st_e-sobid.
*
*                if sy-subrc = 0.
*****************BUSCA EL TIPO DE ESTUDIANTE
*                  data: w_objid_sc type hrp1001-objid.
*
*                  loop at ti_st_cs
*                    into data(wa_st_cs)
*                      where objid = wa_st_e-sobid.
*
*                    select single objid
*                      into w_objid_sc
*                        from hrp1001
*                          where plvar = '01' and
*                                otype = 'SC' and
*                                objid = wa_asig-objid2 and
*                                sclas = 'CS' and
*                                sobid = wa_st_cs-sobid.
*
*                    if sy-subrc = 0.
*                      select single t2~adm_categt
*                        into wa_final-tipost
*                          from hrpad530 as t1 join t7piqadmcategt as t2
*                            on t1~adm_categ = t2~adm_categ
*                          where t1~adatanr = wa_st_cs-adatanr and
*                                t2~spras = sy-langu.
*
*                    endif.
*                  endloop.
**
*****************SE VALIDA EL PAGO
*                  data: w_adatanr type hrp1001-adatanr.
*
**  Begin --> MgM DCEK902983 determina pago por año/periodo 23/01/2017
**                  select single adatanr
**                    into w_adatanr
**                      from hrp1001
**                        where plvar = '01' and
**                              otype = 'ST' and
**                              objid = wa_st_e-sobid and
**                              sclas = 'SM' and
**                              sobid = wa_asig-objid.
**
**                  if sy-subrc = 0.
**
**                    select single pago
**                      into @data(w_pago)
**                        from hrpad506
**                        where adatanr = @w_adatanr and
**                              smstatus = '01'.
*
*                  select single h6~pago
*                    into @data(w_pago)
*                      from hrp1001 as h1
*                        inner join hrpad506 as h6
*                          on h1~adatanr eq h6~adatanr
*                            where h1~plvar = '01'
*                              and h1~otype = 'ST'
*                              and h1~objid = @wa_st_e-sobid
*                              and h1~sclas = 'SM'
*                              and h1~sobid = @wa_asig-objid
*                              and h6~smstatus = '01'.
**  End   --> MgM DCEK902983
*
*                    if sy-subrc eq 0.
**                      wa_final-pago = w_pago.
*                      wa_final-pago = cl_bp_const=>true.
*                    endif.
*
**  Begin --> MgM DCEK902983 Visualizar los no pagos 27/01/2017
**                    if w_pago is initial.
**                      continue.
**                    endif.
**  End   --> MgM DCEK902983
**                  endif.
*
*****************NIVEL DEL ST
*                  select single t2~prcl
*                  into wa_final-nivel
*                  from hrp1737 as t1 join hrt1737 as t2
*                    on t1~tabnr = t2~tabnr
*                  where t1~plvar = '01' and
*                        t1~otype = 'ST' and
*                        t1~objid = wa_st_e-sobid and
*                        t1~prog_type = '4' and
*                        t1~endda = '99991231' and
*                        t2~mc_valid_to = '99991231' and
*                        t2~result_status in ( '1' , '2' ).
*
*                  wa_final-codigo = wa_st-student12.
*                  wa_final-documento = wa_st-idnumber.
*                  wa_final-iddocente = wa_encabezado-docente_id.
*                  concatenate wa_st-name_first wa_st-name_last
*                      wa_st-name_lst2 wa_st-namemiddle into wa_final-nombre separated by space.
*                  append wa_final to ti_final.
*                endif.
*              endloop.
*            endif.
*          endif.
*        endif.
*      endif.
*    endloop.
*
*    if ti_final[] is not initial.
*      rv_datos = cl_bp_const=>true.
*    endif.

    if  iv_programa is initial and
        iv_pregrado is initial and
        iv_plan_estudio is initial. "Humanidades

      rv_datos = me->fill_data_huma(  iv_facultad   = iv_facultad     " ID objeto
                                      iv_anio       = iv_anio          " Año académico
                                      iv_periodo    = iv_periodo       " Período académico
                                      ir_asignatura = ir_asignatura
                                      ir_evento     = ir_evento     ).

    else.

      rv_datos = me->fill_data_others(
               iv_facultad     = iv_facultad
               iv_programa     = iv_programa
               iv_pregrado     = iv_pregrado
               iv_anio         = iv_anio
               iv_periodo      = iv_periodo
               ir_asignatura   = ir_asignatura
               ir_evento       = ir_evento
               iv_plan_estudio = iv_plan_estudio ).

    endif.

  endmethod.


  method fill_data_huma.
    " Data para encabezado de smartform
    data wa_encabezado    type zty_encabezado.
    data wa_final         type zedu_s_lista_clases.
    data lv_value_string  type string.

    types: begin of ty_hrp1001,
             sobid type hrp1001-sobid,
             objid type hrp1001-objid,
           end of ty_hrp1001.

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
    types:  begin of lty_ti_st_cs,
              objid   type hrp1001-objid,
              sobid   type hrp1001-sobid,
              adatanr type hrp1001-adatanr,
              sobidn  type hrp1001-objid,
            end of lty_ti_st_cs.
    data ti_st_cs type standard table of lty_ti_st_cs.
*  End    -->  MgM DCEK903211

    clear ti_encabezado[].
    clear ti_final[].

    lv_value_string = iv_facultad.

    read table get_dropdown_key( im_field  = c_field_facultades )
      into data(ls_drop) with key value = iv_facultad.

    if sy-subrc eq 0.
      wa_encabezado-facultad  = ls_drop-text.
    endif.

**ASIGNATURAS
    select  b~objid,
            a~objid as objid2,
            b~short,
            b~stext
      into table @data(ti_asig)
        from hrp1001 as a
          inner join hrp1000 as b
            on a~sobid  = b~objid
        where a~plvar = '01'
          and a~otype = 'O'
          and a~objid eq @iv_facultad
          and a~endda = '99991231'
          and a~sclas = 'SM'
          and b~plvar = '01'
          and b~otype = 'SM'
          and b~endda = '99991231'
          and b~objid in @ir_asignatura.

**ESTUDIANTES
    types: begin of ty_st,
             stobjid    type piqstudent,
             student12  type piqstudent12,
             name_first type bu_namep_f,
             namemiddle type bu_namemid,
             name_last  type bu_namep_l,
             name_lst2  type bu_namepl2,
             idnumber   type bu_id_number,
           end of ty_st.

    types: begin of ty_st_e,
             sobid    type hrp1001-sobid,
             objid    type hrp1001-objid,
             st_objid type hrp1001-objid,
           end of ty_st_e.

    data: ti_st_sm type table of ty_hrp1001,
          wa_st_sm type ty_hrp1001,
          ti_st    type table of ty_st,
          wa_st    type ty_st,
          ti_st_e  type table of ty_st_e,
          wa_st_e  type ty_st_e.

**EVENTOS

    types: begin of ty_hrp1026,
             objid  type hrp1026-objid,
             delet  type hrp1026-delet,
             cancr  type hrp1026-cancr,
             borrar type boole_d,
           end of ty_hrp1026.

    data: ti_hrp1026 type table of ty_hrp1026,
          wa_hrp1026 type ty_hrp1026.

    select  sobid,
            objid
      into table @data(ti_d)
        from hrp1001
        for all entries in @ti_asig
        where plvar = '01'
          and otype = 'SM'
          and objid = @ti_asig-objid
          and sclas = 'D'.

    if sy-subrc = 0.

      select objid, sobid
        into table @data(ti_e)
          from hrp1001
            for all entries in @ti_d
              where plvar = '01'
                and otype = 'E'
                and sobid = @ti_d-sobid
                and sclas = 'D'.

      if sy-subrc = 0.

        select objid
          into table @data(ti_hrp1739)
            from hrp1739
              for all entries in @ti_e
                where plvar = '01'
                  and otype = 'E'
                  and objid =   @ti_e-objid
                  and objid in  @ir_evento
                  and peryr =   @iv_anio
                  and perid =   @iv_periodo.
*
        if sy-subrc = 0.

          select objid delet cancr
            into table ti_hrp1026
              from hrp1026
                for all entries in ti_hrp1739
                  where plvar = '01' and
                        otype = 'E' and
                        objid = ti_hrp1739-objid.

          if sy-subrc eq 0.

            loop at ti_hrp1026 into wa_hrp1026.
              if wa_hrp1026-delet = 'X' and not wa_hrp1026-cancr is initial.
                wa_hrp1026-borrar = 'X'.
              endif.
              modify ti_hrp1026 from wa_hrp1026.
            endloop.

            delete ti_hrp1026 where borrar = 'X'.
          endif.

          if not ti_hrp1026[] is initial.

            select objid, short, stext
              into table @data(ti_eve)
                from hrp1000
                  for all entries in @ti_hrp1026
                    where plvar = '01'
                      and otype = 'E'
                      and objid = @ti_hrp1026-objid.

          endif.
        endif.
      endif.
    endif.

**HORARIOS
    select t1~objid, t2~monday, t2~tuesday, t2~wednesday,
           t2~thursday, t2~friday, t2~saturday, t2~sunday,
           t2~beguz, t2~enduz
      into table @data(ti_horario)
        from hrp1716 as t1
          join hrt1716 as t2
            on t1~tabnr = t2~tabnr
          for all entries in @ti_eve
            where t1~plvar = '01'
              and t1~otype = 'E'
              and t1~objid = @ti_eve-objid.
*
    select objid
           sobid
    into corresponding fields of table ti_st_e
      from hrp1001
        for all entries in ti_eve
          where plvar = '01'
            and otype = 'E'
            and objid = ti_eve-objid
            and sclas = 'ST'
            and subty = 'A025'.

    loop at ti_st_e
      into wa_st_e.
      wa_st_e-st_objid = wa_st_e-sobid.
      modify ti_st_e from wa_st_e.
    endloop.

    if ti_st_e[] is not initial.
      select  a~stobjid
              a~student12
              b~name_first
              b~namemiddle
              b~name_last
              b~name_lst2
              c~idnumber
        into corresponding fields of table ti_st
          from cmacbpst as a
            inner join but000 as b on a~partner = b~partner
            inner join but0id as c on b~partner = c~partner
              for all entries in ti_st_e
                where a~stobjid = ti_st_e-st_objid
                  and c~type    like `FS%`. "Identificaciones

      if ti_st[] is not initial.

**TIPO DE ESTUDIANTE
        select  objid
                sobid
                adatanr
        into table ti_st_cs
          from hrp1001
            for all entries in ti_st
              where plvar = '01'
                and otype = 'ST'
                and objid = ti_st-stobjid
                and rsign = 'A'
                and relat = '530'
                and sclas = 'CS'.

*  Begin  -->  MgM DCEK903211 recupera nivel 01/02/2017
        if sy-subrc eq 0.

          loop at ti_st_cs
            assigning field-symbol(<fs_st_cs>).
            move <fs_st_cs>-sobid to <fs_st_cs>-sobidn.
          endloop.

********************NIVEL DEL ESTUDIANTE
          select  objid,
                  aclevel
            into table @data(lt_level)
              from hrp1771
                for all entries in @ti_st_cs
                  where plvar = '01'
                    and otype = 'CS'
                    and objid = @ti_st_cs-sobidn
                    and ayear = @iv_anio
                    and perid = @iv_periodo.

          if sy-subrc eq 0.
            sort lt_level by objid.
          endif.

        endif.
*  End    -->  MgM DCEK903211

      endif.
    endif.

    loop at ti_eve
      into data(wa_eve).
*
      read table ti_e into
        data(wa_e)
          with key objid = wa_eve-objid.

      if sy-subrc = 0.

        read table ti_d
          into data(wa_d)
            with key sobid = wa_e-sobid.

        if sy-subrc = 0.

          read table ti_asig
            into data(wa_asig)
              with key objid = wa_d-objid.

          if sy-subrc = 0.
            wa_encabezado-asigobjid   = wa_asig-objid.
            wa_encabezado-asignatura  = wa_asig-stext.
            wa_encabezado-short       = wa_asig-short.
            wa_encabezado-plobjid     = wa_asig-objid2.
*            wa_encabezado-nivel       = wa_asig-stgbeg.
*
*            read table ti_plest into data(wa_plest) with key objid = wa_asig-objid2.
*
*            if sy-subrc = 0.
*              wa_encabezado-planestudios = wa_plest-stext.
*              wa_encabezado-programa = wa_plest-stext.
*
            wa_encabezado-evento = wa_eve-stext.
            wa_encabezado-eveobjid = wa_eve-objid.
*
************CONSULTA DOCENTE
            data: ti_resources type table of piqrfc_resources,
                  wa_resources type piqrfc_resources.

            call function 'HRIQ_RFC_GET_EVENT_DETAILS'
              exporting
                event_id                  = wa_eve-objid
                event_otype               = 'E'
                plvar                     = '01'
                read_resources            = 'X'
                read_resource_description = 'X'
              tables
                resources                 = ti_resources.

            read table ti_resources
              into wa_resources
                with key restp = 'P'.

            if sy-subrc = 0.
              wa_encabezado-docente = wa_resources-resxt.

              clear wa_encabezado-docente_id.

              select single perid
                into wa_encabezado-docente_id
                  from pa0002
                    where pernr = wa_resources-resid.

            endif.
*
************AULA
            data: w_objid_d type hrp1001-objid,
                  w_sobid_g type hrp1001-sobid,
                  w_objid_g type hrp1001-objid.

*            W_OBJID_D = WA_D-objid.
            select single sobid
            into w_sobid_g
            from hrp1001
            where plvar = '01' and
                  otype = 'E' and
                  objid = wa_eve-objid and
                  sclas = 'G'.

            if sy-subrc = 0.

              w_objid_g = w_sobid_g.

              select single stext
                into wa_encabezado-aula
                  from hrp1000
                    where plvar = '01'
                      and otype = 'G'
                      and objid = w_objid_g.

            endif.

************HORARIO
            read table ti_horario
              into data(wa_horario)
                with key objid = wa_eve-objid.

            if sy-subrc = 0.
              clear wa_encabezado-horario.

*  Begin  -->  MgM DCEK903211 Formatea horarios 01/02/2017
*              if wa_horario-monday = 'X'.
*                concatenate 'LUN' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-tuesday = 'X'.
*                concatenate 'MAR' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-wednesday = 'X'.
*                concatenate 'MIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-thursday = 'X'.
*                concatenate 'JUE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-friday = 'X'.
*                concatenate 'VIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-saturday = 'X'.
*                concatenate 'SAB' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.
*              if wa_horario-sunday = 'X'.
*                concatenate 'DOM' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*              endif.

              me->formateo_horario( exporting is_horario = wa_horario " Hora y días de un evento
                                    changing  cv_horario = wa_encabezado-horario ).
*  End    -->  MgM DCEK903211

              append wa_encabezado to ti_encabezado.
            endif.

************LLENA EL DETALLE
            clear wa_final.

            move-corresponding wa_encabezado to wa_final.

            wa_final-anio = iv_anio.
            wa_final-periodo = iv_periodo.

            loop at ti_st_e
              into wa_st_e
                where objid = wa_eve-objid.

              clear wa_final-nivel.

              read table ti_st
                into wa_st
                  with key stobjid = wa_st_e-sobid.

              if sy-subrc = 0.

****************BUSCA PROGRAMA DEL ESTUDIANTE
                select distinct t2~objid,
                                t2~stext
                 into table @data(ti_cs)
                   from hrp1001 as t1
                    join hrp1000 as t2
                      on t1~objid = t2~objid
                   where t1~plvar = '01' and
                         t1~otype = 'CS' and
                         t1~sclas = 'ST' and
                         t1~sobid = @wa_st_e-sobid.

                clear wa_final-programa.

                loop at ti_cs into data(wa_cs).
                  if wa_final-programa is initial.
                    wa_final-programa = wa_cs-stext.
                  else.
                    concatenate wa_final-programa
                                '-'
                                wa_cs-stext
                      into wa_final-programa
                        separated by space.
                  endif.
                endloop.

*****************BUSCA EL TIPO DE ESTUDIANTE
*                  data: w_objid_sc type hrp1001-objid.

                loop at ti_st_cs
                  into data(wa_st_cs)
                    where objid = wa_st_e-sobid.

                  select single t2~adm_categt
                    into wa_final-tipost
                      from hrpad530 as t1
                      join t7piqadmcategt as t2
                        on t1~adm_categ = t2~adm_categ
                        where t1~adatanr = wa_st_cs-adatanr
                          and t2~spras = sy-langu.

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
********************NIVEL DEL ESTUDIANTE
                  read table lt_level
                    into data(ls_level)
                      with key objid = wa_st_cs-sobid
                        binary search.

                  if sy-subrc eq 0.
                    move ls_level-aclevel to wa_final-nivel.
                  endif.
*  End    -->  MgM DCEK903211

                endloop.
*
*****************SE VALIDA EL PAGO

                select single h6~pago
                  into @data(w_pago)
                    from hrp1001 as h1
                      inner join hrpad506 as h6
                        on h1~adatanr eq h6~adatanr
                          where h1~plvar = '01'
                            and h1~otype = 'ST'
                            and h1~objid = @wa_st_e-sobid
                            and h1~sclas = 'SM'
                            and h1~sobid = @wa_asig-objid
                            and h6~smstatus = '01'
                            and h6~perid = @iv_periodo
                            and h6~peryr = @iv_anio.

                if  sy-subrc eq 0 and
                    w_pago eq `1`.  "Pago
*	Begin	-->	MgM DCEK903365 Pago para PDF 08/02/2017
*                  wa_final-pago = cl_bp_const=>true.
                  wa_final-pago_flag = cl_bp_const=>true.
                  wa_final-pago      = w_pago.
*	End	  -->	MgM DCEK903365
                else.
*	Begin	-->	MgM DCEK903365 Pago para PDF 08/02/2017
*                  wa_final-pago = cl_bp_const=>false.
                  wa_final-pago_flag = cl_bp_const=>false.
                  clear wa_final-pago.
*	End	  -->	MgM DCEK903365
                endif.

                "No se descarta para poder visualizar los no pagos también
*                  if w_pago is initial.
*                    continue.
*                  endif.
**                  endif.
*

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
*****************NIVEL DEL ST
*                select single t2~prcl
*                  into wa_final-nivel
*                    from hrp1737 as t1
*                      join hrt1737 as t2
*                        on t1~tabnr = t2~tabnr
*                      where t1~plvar        = '01'
*                        and t1~otype        = 'ST'
*                        and t1~objid        = wa_st_e-sobid
*                        and t1~prog_type    = '4'
*                        and t1~endda        = '99991231'
*                        and t2~mc_valid_to  = '99991231'
*                        and t2~result_status in ( '1' , '2' ).
*  End    -->  MgM DCEK903211

                wa_final-codigo     = wa_st-student12.
                wa_final-documento  = wa_st-idnumber.
                wa_final-iddocente  = wa_encabezado-docente_id.

                concatenate wa_st-name_first
                            wa_st-name_last
                            wa_st-name_lst2
                            wa_st-namemiddle
                  into wa_final-nombre
                    separated by space.

                append wa_final to ti_final.
                clear wa_final-programa.
              endif.
            endloop.
*            endif.
          endif.
        endif.
      endif.
    endloop.

    if ti_final[] is not initial.
      rv_datos = cl_bp_const=>true.
    endif.

  endmethod.


  method fill_data_others.
    " Data para encabezado de smartform
    data wa_encabezado    type zty_encabezado.
*  Begin  -->  MgM DCEK902983 Visualizar los no pagos 27/01/2017
*    data wa_final         type zty_final.
    data wa_final         type zedu_s_lista_clases.
*  End    -->  MgM DCEK902983
    data lv_value_string  type string.

    types: begin of ty_hrp1001,
             sobid type hrp1001-sobid,
             objid type hrp1001-objid,
           end of ty_hrp1001.

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
    types:  begin of lty_ti_st_cs,
              objid   type hrp1001-objid,
              sobid   type hrp1001-sobid,
              adatanr type hrp1001-adatanr,
              sobidn  type hrp1001-objid,
            end of lty_ti_st_cs.
    data ti_st_cs type standard table of lty_ti_st_cs.
*  End    -->  MgM DCEK903211

**ESTUDIANTES
    types: begin of ty_st,
             stobjid    type piqstudent,
             student12  type piqstudent12,
             name_first type bu_namep_f,
             namemiddle type bu_namemid,
             name_last  type bu_namep_l,
             name_lst2  type bu_namepl2,
             idnumber   type bu_id_number,
           end of ty_st.

    types: begin of ty_st_e,
             sobid    type hrp1001-sobid,
             objid    type hrp1001-objid,
             st_objid type hrp1001-objid,
           end of ty_st_e.

    data: ti_st_sm type table of ty_hrp1001,
          wa_st_sm type ty_hrp1001,
          ti_st    type table of ty_st,
          wa_st    type ty_st,
          ti_st_e  type table of ty_st_e,
          wa_st_e  type ty_st_e.

**EVENTOS

    types: begin of ty_hrp1026,
             objid  type hrp1026-objid,
             delet  type hrp1026-delet,
             cancr  type hrp1026-cancr,
             borrar type boole_d,
           end of ty_hrp1026.

    data: ti_hrp1026 type table of ty_hrp1026,
          wa_hrp1026 type ty_hrp1026.

    clear ti_encabezado[].
    clear ti_final[].

    lv_value_string = iv_facultad.

    read table get_dropdown_key( im_field  = c_field_facultades )
      into data(ls_drop) with key value = iv_facultad.

    if sy-subrc eq 0.
      wa_encabezado-facultad  = ls_drop-text.
    endif.

    read table get_dropdown_key( im_field  = c_field_programas
                                 im_value  = lv_value_string )
      into ls_drop with key value = iv_programa.

    if sy-subrc eq 0.
      wa_encabezado-tipoprograma  = ls_drop-text.
    else.
      wa_encabezado-tipoprograma  = iv_programa.
    endif.

    read table get_dropdown_key( im_field  = 'PREPOSGRADO' )
      into ls_drop with key value = iv_pregrado.

    if sy-subrc eq 0.
      wa_encabezado-tipopregrado  = ls_drop-text.
    endif.

**ARMA RANGO DE PLANES DE ESTUDIO
*  DATA: R_PLANEST TYPE RANGE OF HRP1000-OBJID,
*        WA_R_PLAN LIKE LINE OF R_PLANEST,
*        WA_PR_PLES LIKE LINE OF pr_ples.

*  LOOP AT pr_ples INTO WA_PR_PLES.
*    WA_R_PLAN-SIGN = WA_PR_PLES-SIGN.
*    WA_R_PLAN-OPTION = WA_PR_PLES-OPTION.
*    PERFORM LLENAR_PLAN_OBJID USING WA_PR_PLES-LOW
*                           CHANGING WA_R_PLAN-LOW.
*
*    PERFORM LLENAR_PLAN_OBJID USING WA_PR_PLES-HIGH
*                           CHANGING WA_R_PLAN-HIGH.
*    APPEND WA_R_PLAN TO R_PLANEST.
*  ENDLOOP.

**PLAN DE ESTUDIOS
    select  b~objid,
            b~short,
            b~stext
      into table @data(ti_plest)
        from hrp1001 as a
          inner join hrp1000 as b on a~sobid  = b~objid
            where a~plvar = '01'
              and a~otype = 'O'
              and a~objid = @iv_pregrado
              and a~endda = '99991231'
              and a~sclas = 'SC'
              and b~plvar = '01'
              and b~otype = 'SC'
              and b~endda = '99991231'
*              and b~objid in r_planest.
              and b~objid eq @iv_plan_estudio.

**ASIGNATURAS
**ARMA RANGO DE ASIGNATURAS
*    data: r_asig type range of hrp1000-objid.
*        WA_R_ASIG LIKE LINE OF R_ASIG,
*        WA_pr_asig LIKE LINE OF pr_asig.

*  LOOP AT pr_asig INTO data(WA_pr_asig).
*    WA_R_ASIG-SIGN = WA_pr_asig-SIGN.
*    WA_R_ASIG-OPTION = WA_pr_asig-OPTION.
*    IF NOT WA_pr_asig-LOW IS INITIAL.
*      PERFORM LLENAR_ASIG_OBJID USING WA_pr_asig-LOW
*                             CHANGING WA_R_ASIG-LOW.
*    ENDIF.
*
*    IF NOT WA_pr_asig-HIGH IS INITIAL.
*      PERFORM LLENAR_ASIG_OBJID USING WA_pr_asig-HIGH
*                             CHANGING WA_R_ASIG-HIGH.
*    ENDIF.
*    APPEND WA_R_ASIG TO R_ASIG.
*  ENDLOOP.

    select  b~objid,
            a~objid as objid2,
            b~short,
            b~stext,
            c~stgbeg  into table @data(ti_asig)
    from hrp1001 as a
    inner join hrp1000 as b on a~sobid  = b~objid
    inner join hrpad500 as c on a~adatanr = c~adatanr
**      FOR ALL ENTRIES IN ti_asig_objid
    where a~plvar = '01'
      and a~otype = 'SC'
*      AND a~objid IN R_PLANEST "ti_asig_objid-objid
      and a~objid eq @iv_plan_estudio "ti_asig_objid-objid
      and a~endda = '99991231'
      and a~sclas = 'SM'
      and b~plvar = '01'
      and b~otype = 'SM'
      and b~endda = '99991231'
      and b~objid in @ir_asignatura.

    if sy-subrc eq 0.                                       "DCEK903247

      select  sobid,
              objid
        into table @data(ti_d)
          from hrp1001
          for all entries in @ti_asig
          where plvar = '01'
            and otype = 'SM'
            and objid = @ti_asig-objid
*        AND endda = '99991231'
            and sclas = 'D'.

      if sy-subrc = 0.
*
        select  objid,
                sobid
          into table @data(ti_e)
            from hrp1001
              for all entries in @ti_d
                where plvar = '01'
                  and otype = 'E'
                  and sobid = @ti_d-sobid
*                 AND endda >= SY-DATUM
                  and sclas = 'D'.

        if sy-subrc = 0.

          select objid
            into table @data(ti_hrp1739)
              from hrp1739
                for all entries in @ti_e
                  where plvar = '01'
                    and otype = 'E'
                    and objid = @ti_e-objid
*  Begin  -->  MgM  DCEK902833 17/01/2017
                    and objid in @ir_evento
*  End    -->  MgM
                    and peryr = @iv_anio
                    and perid = @iv_periodo.
*
          if sy-subrc = 0.

            select objid delet cancr
              into table ti_hrp1026
                from hrp1026
                  for all entries in ti_hrp1739
                    where plvar = '01' and
                          otype = 'E' and
                          objid = ti_hrp1739-objid.

            loop at ti_hrp1026 into wa_hrp1026.
              if wa_hrp1026-delet = 'X' and not wa_hrp1026-cancr is initial.
                wa_hrp1026-borrar = 'X'.
              endif.
              modify ti_hrp1026 from wa_hrp1026.
            endloop.
*
            delete ti_hrp1026 where borrar = 'X'.

            if not ti_hrp1026[] is initial.

              select  objid,
                      short,
                      stext
                into table @data(ti_eve)
                  from hrp1000
                    for all entries in @ti_hrp1026
                      where plvar = '01'
                        and otype = 'E'
                        and objid = @ti_hrp1026-objid
                        and plvar = '01'
                        and otype = 'E'.
            endif.
          endif.
        endif.
      endif.
    endif.

    if ti_eve[] is not initial.                             "DCEK903247

**HORARIOS
      select t1~objid, t2~monday, t2~tuesday, t2~wednesday,
             t2~thursday, t2~friday, t2~saturday, t2~sunday,
             t2~beguz, t2~enduz
        into table @data(ti_horario)
          from hrp1716 as t1
          join hrt1716 as t2
            on t1~tabnr = t2~tabnr
          for all entries in @ti_eve
            where t1~plvar = '01'
              and t1~otype = 'E'
              and t1~objid = @ti_eve-objid.
*
      select objid
             sobid
        into corresponding fields of table ti_st_e
          from hrp1001
            for all entries in ti_eve
              where plvar = '01'
                and otype = 'E'
                and objid = ti_eve-objid
*                ENDDA = '99991231' AND
                and sclas = 'ST'
                and subty = 'A025'.
*
      loop at ti_st_e
        into wa_st_e.
        wa_st_e-st_objid = wa_st_e-sobid.
        modify ti_st_e from wa_st_e.
      endloop.

      if ti_st_e[] is not initial.
        select  a~stobjid
                a~student12
                b~name_first
                b~namemiddle
                b~name_last
                b~name_lst2
                c~idnumber
          into corresponding fields of table ti_st
            from cmacbpst as a
              inner join but000 as b on a~partner = b~partner
              inner join but0id as c on b~partner = c~partner
                for all entries in ti_st_e
                  where a~stobjid = ti_st_e-st_objid
                    and c~type    like `FS%`. "Identificaciones

        if ti_st[] is not initial.

*  ***BUSCA EL TIPO DE ESTUDIANTE
          select  objid
                  sobid
                  adatanr
            into table ti_st_cs
              from hrp1001
              for all entries in ti_st
                where plvar = '01'
                  and otype = 'ST'
                  and objid = ti_st-stobjid
                  and rsign = 'A'
                  and relat = '530'
                  and sclas = 'CS'.

*  Begin  -->  MgM DCEK903211 recupera nivel 01/02/2017
          if sy-subrc eq 0.

            loop at ti_st_cs
              assigning field-symbol(<fs_st_cs>).
              move <fs_st_cs>-sobid to <fs_st_cs>-sobidn.
            endloop.

********************NIVEL DEL ESTUDIANTE
            select  a~objid,
                    a~aclevel,
*	Begin	-->	MgM DCEK903257 Plan estudio de estudiante 03/02/2017
                    b~stext,
                    b~short
*	End	  -->	MgM DCEK903257
              into table @data(lt_level)
                from hrp1771  as a
                  inner join hrp1000 as b
                    on a~objid eq b~objid
                  for all entries in @ti_st_cs
                    where a~plvar = '01'
                      and a~otype = 'CS'
                      and a~objid = @ti_st_cs-sobidn
                      and a~ayear = @iv_anio
                      and a~perid = @iv_periodo
                      and b~langu = @sy-langu.

            if sy-subrc eq 0.
              sort lt_level by objid.
            endif.

*	Begin	-->	MgM DCEK903257 Plan estudio de estudiante 02/02/2017
            select  a~sobid,
                    a~objid,
                    b~short,
                    b~stext
              from hrp1001 as a
                inner join hrp1000 as b
                  on a~objid eq b~objid and
                     a~otype eq b~otype and                   "DCEK903280
                     a~plvar eq b~plvar                       "DCEK903416
                into table @data(lt_cs_st)
                for all entries in @ti_st_e
                  where a~otype eq @cl_hrpiq00const=>c_otype_cs
                    and a~plvar eq @cl_hrpiq00const=>c_plvar_active
                    and a~sobid eq @ti_st_e-sobid
                    and a~subty eq @cl_hrpiq00const=>c_relat_513b "DCEK903416
                    and a~sclas eq @cl_hrpiq00const=>c_otype_st
                    and b~langu eq @sy-langu.

            if sy-subrc eq 0.
              sort lt_cs_st by sobid.
            endif.
*	End	  -->	MgM DCEK903257

          endif.
*  End    -->  MgM DCEK903211
        endif.
      endif.
    endif.

    loop at ti_eve
      into data(wa_eve).

      read table ti_e into data(wa_e) with key objid = wa_eve-objid.
      if sy-subrc = 0.
        read table ti_d into data(wa_d) with key sobid = wa_e-sobid.
        if sy-subrc = 0.
          read table ti_asig into data(wa_asig) with key objid = wa_d-objid.
          if sy-subrc = 0.
            wa_encabezado-asigobjid   = wa_asig-objid.
            wa_encabezado-asignatura  = wa_asig-stext.
            wa_encabezado-short       = wa_asig-short.
            wa_encabezado-plobjid     = wa_asig-objid2.
            wa_encabezado-nivel       = wa_asig-stgbeg.

            read table ti_plest into data(wa_plest) with key objid = wa_asig-objid2.

            if sy-subrc = 0.
              wa_encabezado-planestudios = wa_plest-stext.
              wa_encabezado-programa = wa_plest-stext.

              wa_encabezado-evento = wa_eve-stext.
              wa_encabezado-eveobjid = wa_eve-objid.

************CONSULTA DOCENTE
              data: ti_resources type table of piqrfc_resources,
                    wa_resources type piqrfc_resources.

              call function 'HRIQ_RFC_GET_EVENT_DETAILS'
                exporting
                  event_id                  = wa_eve-objid
                  event_otype               = 'E'
                  plvar                     = '01'
                  read_resources            = 'X'
                  read_resource_description = 'X'
                tables
                  resources                 = ti_resources.

              read table ti_resources into wa_resources with key restp = 'P'.
              if sy-subrc = 0.
                wa_encabezado-docente = wa_resources-resxt.
                clear wa_encabezado-docente_id.
                select single perid
                into wa_encabezado-docente_id
                from pa0002
                where pernr = wa_resources-resid.

              endif.

************AULA
              data: w_objid_d type hrp1001-objid,
                    w_sobid_g type hrp1001-sobid,
                    w_objid_g type hrp1001-objid.

*            W_OBJID_D = WA_D-objid.
              select single sobid
              into w_sobid_g
              from hrp1001
              where plvar = '01' and
                    otype = 'E' and
                    objid = wa_eve-objid and
                    sclas = 'G'.

              if sy-subrc = 0.
                w_objid_g = w_sobid_g.
                select single stext
                into wa_encabezado-aula
                from hrp1000
                where plvar = '01' and
                      otype = 'G' and
                      objid = w_objid_g.
              endif.

************HORARIO
              read table ti_horario
                into data(wa_horario)
                  with key objid = wa_eve-objid.

              if sy-subrc = 0.
                clear wa_encabezado-horario.

*  Begin  -->  MgM DCEK903211 Formatea horarios 01/02/2017
*                if wa_horario-monday = 'X'.
*                  concatenate 'LUN' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-tuesday = 'X'.
*                  concatenate 'MAR' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-wednesday = 'X'.
*                  concatenate 'MIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-thursday = 'X'.
*                  concatenate 'JUE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-friday = 'X'.
*                  concatenate 'VIE' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-saturday = 'X'.
*                  concatenate 'SAB' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.
*                if wa_horario-sunday = 'X'.
*                  concatenate 'DOM' wa_horario-beguz '-' wa_horario-enduz into wa_encabezado-horario separated by space.
*                endif.

                me->formateo_horario( exporting is_horario = wa_horario " Hora y días de un evento
                                      changing  cv_horario = wa_encabezado-horario ).
*  End    -->  MgM DCEK903211

                append wa_encabezado to ti_encabezado.
              endif.

************LLENA EL DETALLE
              clear wa_final.
              move-corresponding wa_encabezado to wa_final.
              wa_final-anio     = iv_anio.
              wa_final-periodo  = iv_periodo.
              wa_final-programa_short = wa_plest-short. "-->  MgM DCEK903257

              loop at ti_st_e
                into wa_st_e
                  where objid = wa_eve-objid.

                clear wa_final-nivel. "-->  MgM DCEK903211

                read table ti_st
                  into wa_st
                    with key stobjid = wa_st_e-sobid.

                if sy-subrc = 0.
****************BUSCA EL TIPO DE ESTUDIANTE
                  data: w_objid_sc type hrp1001-objid.

                  loop at ti_st_cs
                    into data(wa_st_cs)
                      where objid = wa_st_e-sobid.

*                    select single objid
*                      into w_objid_sc
*                        from hrp1001
*                          where plvar = '01' and
*                                otype = 'SC' and
*                                objid = wa_asig-objid2 and
*                                sclas = 'CS' and
**                                sobid = wa_st_cs-sobid.
*
*                    if sy-subrc = 0.
                    select single t2~adm_categt
                      into wa_final-tipost
                        from hrpad530 as t1 join t7piqadmcategt as t2
                          on t1~adm_categ = t2~adm_categ
                        where t1~adatanr = wa_st_cs-adatanr and
                              t2~spras = sy-langu.

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
********************NIVEL DEL ESTUDIANTE
                    read table lt_level
                      into data(ls_level)
                        with key objid = wa_st_cs-sobid
                          binary search.

                    if sy-subrc eq 0.
                      move ls_level-aclevel to wa_final-nivel.
                    endif.
*  End    -->  MgM DCEK903211
*                    endif.

*	Begin	-->	MgM DCEK903257 Plan estudio de estudiante 02/02/2017
                    read table lt_cs_st
                      into data(ls_planest_st)
                        with key sobid  = wa_st_cs-objid
                          binary search.

                    if sy-subrc eq 0.
                      move ls_planest_st-stext to wa_final-programa.
                      move ls_planest_st-short to wa_final-programa_short.
                    endif.
*	End	  -->	MgM DCEK903257

                  endloop.
*
****************SE VALIDA EL PAGO
                  data: w_adatanr type hrp1001-adatanr.

*  Begin  -->  MgM DCEK902983 determina pago por año/periodo 23/01/2017
*                  select single adatanr
*                    into w_adatanr
*                      from hrp1001
*                        where plvar = '01' and
*                              otype = 'ST' and
*                              objid = wa_st_e-sobid and
*                              sclas = 'SM' and
*                              sobid = wa_asig-objid.
*
*                  if sy-subrc = 0.
*
*                    select single pago
*                      into @data(w_pago)
*                        from hrpad506
*                        where adatanr = @w_adatanr and
*                              smstatus = '01'.

                  select single h6~pago
                    into @data(w_pago)
                      from hrp1001 as h1
                        inner join hrpad506 as h6
                          on h1~adatanr eq h6~adatanr
                            where h1~plvar = '01'
                              and h1~otype = 'ST'
                              and h1~objid = @wa_st_e-sobid
                              and h1~sclas = 'SM'
                              and h1~sobid = @wa_asig-objid
                              and h6~smstatus = '01'.
*  End    -->  MgM DCEK902983

                  if  sy-subrc eq 0 and
                      w_pago eq `1`.  "Pago
*	Begin	-->	MgM DCEK903365 Pago para PDF 08/02/2017
*                  wa_final-pago = cl_bp_const=>true.
                    wa_final-pago_flag = cl_bp_const=>true.
                    wa_final-pago      = w_pago.
*	End	  -->	MgM DCEK903365
                  else.
*	Begin	-->	MgM DCEK903365 Pago para PDF 08/02/2017
*                  wa_final-pago = cl_bp_const=>false.
                    wa_final-pago_flag = cl_bp_const=>false.
                    clear wa_final-pago.
*	End	  -->	MgM DCEK903365
                  endif.

*  Begin  -->  MgM DCEK902983 Visualizar los no pagos 27/01/2017
*                    if w_pago is initial.
*                      continue.
*                    endif.
*  End    -->  MgM DCEK902983
*                  endif.

*  Begin  -->  MgM DCEK903211 Nivel estudiante 01/02/2017
*****************NIVEL DEL ST
*                  select single t2~prcl
*                  into wa_final-nivel
*                  from hrp1737 as t1 join hrt1737 as t2
*                    on t1~tabnr = t2~tabnr
*                  where t1~plvar = '01' and
*                        t1~otype = 'ST' and
*                        t1~objid = wa_st_e-sobid and
*                        t1~prog_type = '4' and
*                        t1~endda = '99991231' and
*                        t2~mc_valid_to = '99991231' and
*                        t2~result_status in ( '1' , '2' ).
*  End    -->  MgM DCEK903211

                  wa_final-codigo = wa_st-student12.
                  wa_final-documento = wa_st-idnumber.
                  wa_final-iddocente = wa_encabezado-docente_id.
                  concatenate wa_st-name_first wa_st-name_last
                      wa_st-name_lst2 wa_st-namemiddle into wa_final-nombre separated by space.
                  append wa_final to ti_final.
                endif.
              endloop.
            endif.
          endif.
        endif.
      endif.
    endloop.

    if ti_final[] is not initial.
      rv_datos = cl_bp_const=>true.
    endif.

  endmethod.


  method fill_events.

*    types: begin of ty_hrp1026,
*             objid  type hrp1026-objid,
*             delet  type hrp1026-delet,
*             cancr  type hrp1026-cancr,
*             borrar type boole_d,
*           end of ty_hrp1026.
*
**    data: "ti_d       type table of ty_hrp1001,
*            "wa_d     type ty_hrp1001,
**          ti_e       type table of ty_hrp1001,
*         " wa_e       type ty_hrp1001,
*          "ti_hrp1739 type table of ty_hrp1001,
**          ti_hrp1026 type table of ty_hrp1026,
**          wa_hrp1026 type ty_hrp1026.
*
*    select sobid, objid
**    into corresponding fields of table ti_d
*    into table @data(ti_d)
*    from hrp1001
*    for all entries in ti_asig
*    where plvar = '01'
*      and otype = 'SM'
*      and objid = ti_asig-objid
**    AND endda = '99991231'
*      and sclas = 'D'.
*
*    if sy-subrc = 0.
*
*      select objid sobid
*      into table @data(ti_e)
*      from hrp1001
*      for all entries in ti_d
*      where plvar = '01'
*        and otype = 'E'
*        and sobid = ti_d-sobid
**      AND endda >= SY-DATUM
*        and sclas = 'D'.
*
*      if sy-subrc = 0.
*
*        select objid
*        into table @data(ti_hrp1739)
*        from hrp1739
*        for all entries in ti_e
*        where plvar = '01'
*          and otype = 'E'
*          and objid = ti_e-objid
*          and peryr = pr_pery
*          and perid = pr_peri.
*
*        if sy-subrc = 0.
*          select objid delet cancr
*          into table @data(ti_hrp1026)
*          from hrp1026
*          for all entries in ti_hrp1739
*          where plvar = '01' and
*                otype = 'E' and
*                objid = ti_hrp1739-objid.
*
*
*          loop at ti_hrp1026 into data(wa_hrp1026).
*            if wa_hrp1026-delet = 'X' and not wa_hrp1026-cancr is initial.
*              wa_hrp1026-borrar = 'X'.
*            endif.
*            modify ti_hrp1026 from wa_hrp1026.
*          endloop.
*
*          delete ti_hrp1026 where borrar = 'X'.
*
*          if not ti_hrp1026[] is initial.
*
*            select objid short stext
*            into corresponding fields of table ti_eve
*            from hrp1000
*            for all entries in ti_hrp1026
*            where plvar = '01' and
*                  otype = 'E' and
*                  objid = ti_hrp1026-objid and
*                  plvar = '01' and
*                  otype = 'E'.
*          endif.
*        endif.
*      endif.
*    endif.

  endmethod.


  method formateo_horario.

    data ls_horario       type piqeventdates.

    move-corresponding is_horario to ls_horario.

    if ls_horario-beguz(1) = '0'.
      concatenate ls_horario-beguz+1(1)
                  ls_horario-beguz+2(2)
        into data(lv_horabeg) separated by cl_atra_util=>c_sep.
    else.
      concatenate ls_horario-beguz(2)
                  ls_horario-beguz+2(2)
        into lv_horabeg separated by cl_atra_util=>c_sep.
    endif.
    if ls_horario-enduz(1) = '0'.
      concatenate ls_horario-enduz+1(1)
                  ls_horario-enduz+2(2)
        into data(lv_horaend) separated by cl_atra_util=>c_sep.
    else.
      concatenate ls_horario-enduz(2)
                  ls_horario-enduz+2(2)
        into lv_horaend separated by cl_atra_util=>c_sep.
    endif.

    if ls_horario-monday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `LUN`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-tuesday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `MAR`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-wednesday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `MIE`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-thursday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `JUE`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-friday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `VIE`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-saturday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `SAB`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

    if ls_horario-sunday eq cl_bp_const=>true.
      add_day(  exporting iv_day      = `DOM`
                          iv_hora_ini = lv_horabeg
                          iv_hora_fin = lv_horaend
                changing  cv_horario  = cv_horario ).
    endif.

  endmethod.


  method get_asignaturas_ovs.

    data: lr_planest  type range of hrp1000-objid,
          lrs_planest like line of lr_planest.

    lrs_planest-sign   = 'I'.
    lrs_planest-option = 'EQ'.
    lrs_planest-low    = iv_plan_estudio.
    append lrs_planest to lr_planest.

    select b~objid a~objid as objid2 b~short b~stext c~stgbeg
      into corresponding fields of table rt_asignaturas
    from
      hrp1001 as a
      inner join hrp1000 as b on a~sobid  = b~objid
      inner join hrpad500 as c on a~adatanr = c~adatanr
**      FOR ALL ENTRIES IN ti_asig_objid
      where a~plvar = '01'
        and a~otype = 'SC'
        and a~objid in lr_planest
        and a~endda = '99991231'
        and a~sclas = 'SM'
        and b~plvar = '01'
        and b~otype = 'SM'
        and b~endda = '99991231'
      order by short.

  endmethod.


  method get_ddbk_anio_periodo.

    types: begin of ty_hrp1001,
             otype type hrp1001-otype,
             objid type hrp1001-objid,
             plvar type hrp1001-plvar,
             sclas type hrp1001-sclas,
             sobid type hrp1001-sobid,
           end of ty_hrp1001,

           begin of ty_hrp1750,
             plvar type hrp1750-plvar,
             otype type hrp1750-otype,
             objid type hrp1750-objid,
             peryr type hrp1750-peryr,
             tabnr type hrp1750-tabnr,
           end of ty_hrp1750,

           begin of ty_hrt1750,
             tabnr     type hrt1750-tabnr,
             tabseqnr  type hrt1750-tabseqnr,
             timelimit type hrt1750-timelimit,
             peryr     type hrt1750-peryr,
             perid     type hrt1750-perid,
           end of ty_hrt1750.

    data: lt_hrp1001     type table of ty_hrp1001,
          lt_hrp1750     type sorted table of ty_hrp1750
                              with non-unique key primary_key
                                components tabnr,
          lt_hrt1750     type sorted table of ty_hrt1750
                              with non-unique key primary_key
                                components peryr  perid,
          lr_objid       type range of hrobjid,
          lrs_objid      like line of lr_objid,
          lt_peri_descr  type zedu_t_context_attr_value,
          lt_anio_descr  type zedu_t_context_attr_value,
          ls_peri_descr  type wdr_context_attr_value,
          ls_anio_descr  type wdr_context_attr_value,
          ls_attr_value  type wdr_context_attr_value,
          lv_anio_act(4),
          lv_anio_sig(4).

    field-symbols: <fs_hrt1750> type ty_hrt1750,
                   <fs_hrp1001> type ty_hrp1001.

*	Begin	-->	MgM DCEK902709 sin dependencia de programa 12/01/2017
*    select otype objid plvar sclas sobid
*       from hrp1001
*       into table lt_hrp1001
*      where sobid  = iv_programa " Programa seleccionado
*         and otype = 'CA'
*         and sclas = 'O'.
*
*    delete lt_hrp1001 where sclas ne 'O'.
*
*    lrs_objid = 'IEQ'.
*    loop at lt_hrp1001 assigning <fs_hrp1001>.
*      lrs_objid-low = <fs_hrp1001>-objid.
*      append lrs_objid to lr_objid.
*    endloop.
*	End	  -->	MgM DCEK902709

    select plvar otype objid peryr tabnr
      from hrp1750
       into table lt_hrp1750
        where plvar = '01'
          and otype = 'CA'
          and objid in lr_objid
        order by tabnr.

*	Begin	-->	MgM DCEK902600 lista con duplicados 11/01/2017
    check lt_hrp1750[] is not initial.

    delete adjacent duplicates from lt_hrp1750
      comparing tabnr.
*	End	  -->	MgM DCEK902600

    select tabnr tabseqnr timelimit peryr perid
      from hrt1750
        into table lt_hrt1750
          for all entries in lt_hrp1750
            where tabnr = lt_hrp1750-tabnr.

*	Begin	-->	MgM DCEK902709 sin dependencia de programa 12/01/2017
*    delete lt_hrt1750 where timelimit ne 'ADMI'.
*	End	  -->	MgM DCEK902709

*	Begin	-->	MgM DCEK902600 lista con duplicados 11/01/2017
    delete adjacent duplicates from lt_hrt1750 comparing peryr
                                                         perid.
*	End	  -->	MgM DCEK902600

    select perid as value
           perit as text
      from t7piqperiodt
      into table lt_peri_descr
       where spras eq sy-langu
        order by perid.

    select peryr as value
           peryt as text
     from t7piqyeart
       into table lt_anio_descr
         where spras eq sy-langu
           order by peryr.

    loop at lt_hrt1750
      assigning <fs_hrt1750>.

      read table lt_anio_descr into ls_anio_descr
        with key value = <fs_hrt1750>-peryr binary search.

      check sy-subrc eq 0.  "-->  MgM DCEK902983 agregado humanidades 24/01/2017

      read table lt_peri_descr into ls_peri_descr
        with key value = <fs_hrt1750>-perid binary search.

      check sy-subrc eq 0.  "-->  MgM DCEK902983 agregado humanidades 24/01/2017

      concatenate <fs_hrt1750>-peryr
                  <fs_hrt1750>-perid
        into ls_attr_value-value.

      concatenate ls_anio_descr-text
                  ls_peri_descr-text
        into ls_attr_value-text
          separated by '-'.

      append ls_attr_value to rt_attr_value.

    endloop.

  endmethod.


  METHOD GET_DDBK_PRUEBAS.
    DATA: lt_ttype      TYPE TABLE OF t7piqtrtesttypet,
          ls_ttype      TYPE t7piqtrtesttypet,
          ls_attr_value TYPE wdr_context_attr_value.

    SELECT * FROM t7piqtrtesttypet
      INTO TABLE lt_ttype
      FOR ALL ENTRIES IN it_pruebas
      WHERE spras    EQ sy-langu AND
            testtype EQ it_pruebas-testtype.
    LOOP AT lt_ttype INTO ls_ttype.
      ls_attr_value-text  = ls_ttype-testtypetext.
      ls_attr_value-value = ls_ttype-testtype.
      APPEND ls_attr_value TO rt_ddbk_pruebas.
    ENDLOOP.

  ENDMETHOD.


  method get_eventos_ovs.


    types: begin of ty_hrp1001,
             sobid type hrp1001-sobid,
             objid type hrp1001-objid,
           end of ty_hrp1001.

    types: begin of ty_hrp1026,
             objid  type hrp1026-objid,
             delet  type hrp1026-delet,
             cancr  type hrp1026-cancr,
             borrar type char1,
           end of ty_hrp1026.

    data: lt_hrp1026 type table of ty_hrp1026,
          ls_hrp1026 type ty_hrp1026.

*	Begin	-->	MgM  DCEK902833 17/01/2017
    if ir_asignatura is initial.
      loop at get_asignaturas_ovs( iv_plan_estudio = iv_plan_estudio )
        into data(ls_asignaturas).
        append initial line to ir_asignatura assigning field-symbol(<fs_asigna>).
        <fs_asigna>-sign    = `I`.
        <fs_asigna>-option  = `EQ`.
        <fs_asigna>-low     = ls_asignaturas-objid.
      endloop.
    endif.
*	End	  -->	MgM

    select sobid
      into table @data(lt_d)
        from hrp1001
          where plvar = '01'
            and otype = 'SM'
            and objid in @ir_asignatura
*          AND endda = '99991231'
            and sclas = 'D'.

    if sy-subrc = 0.

      select objid
        into table @data(lt_e)
          from hrp1001
            for all entries in @lt_d
              where plvar = '01'
                and otype = 'E'
                and sobid = @lt_d-sobid
*              AND endda >= SY-DATUM
                and sclas = 'D'.

      if sy-subrc = 0.

        select objid
          into table @data(lt_hrp1739)
            from hrp1739
              for all entries in @lt_e
                where plvar = '01'
                  and otype = 'E'
                  and objid = @lt_e-objid
                  and peryr = @iv_anio
                  and perid = @iv_periodo.

        if sy-subrc = 0.

          select objid delet cancr
            into corresponding fields of table lt_hrp1026
              from hrp1026
                for all entries in lt_hrp1739
                  where plvar = '01' and
                        otype = 'E' and
                        objid = lt_hrp1739-objid.

          loop at lt_hrp1026 into ls_hrp1026.
            if ls_hrp1026-delet = 'X' and not ls_hrp1026-cancr is initial.
              ls_hrp1026-borrar = 'X'.
            endif.
            modify lt_hrp1026 from ls_hrp1026.
          endloop.

          delete lt_hrp1026 where borrar = 'X'.

          if not lt_hrp1026[] is initial.

            select objid short stext
              into corresponding fields of table rt_eventos
                from hrp1000
                  for all entries in lt_hrp1026
                    where plvar = '01' and
                          otype = 'E' and
                          objid = lt_hrp1026-objid and
                          plvar = '01' and
                          otype = 'E'.

            if sy-subrc eq 0.
              sort rt_eventos by short.
            endif.
          endif.
        endif.
      endif.
    endif.

  endmethod.


  method GET_NAME_USERS.

   SELECT *
    FROM user_addr
    INTO TABLE rt_user_addr
    FOR ALL ENTRIES IN it_users
    WHERE bname = it_users-table_line.

  endmethod.


  method get_pdf_xstring.

    data: lv_hora1(2) type c,
          lv_hora2(2) type c,
          lv_hora3(5) type c.
    data fm_name type  rs38l_fnam.
    " Data para el smartform
    data lv_control_parameters type ssfctrlop.
    data lv_output_options     type ssfcompop.
    data lv_ssf_output         type ssfcrescl.
    data lt_dummy   type standard table of tline.
    data lt_doctab_archive type standard table of docs.
    data lv_pdf_size type i.
*	Begin	-->	MgM DCEK903211 impresión pdf error 01/02/2017
    data lt_final type ztty_final.
*	End	  -->	MgM DCEK903211
    data ls_final type zty_final.

    if ti_final is not initial.

*	Begin	-->	MgM DCEK903365 Pago para PDF 08/02/2017
*      lt_final[] = ti_final[].  "-->  MgM DCEK903211
      loop at ti_final
        into data(ls_final_web).
        move-corresponding ls_final_web to ls_final.
        append ls_final to lt_final.
      endloop.
*	End	  -->	MgM DCEK903365

      call function 'SSF_FUNCTION_MODULE_NAME'
        exporting
          formname           = 'ZDR_EDU_LISTACLASE'
        importing
          fm_name            = fm_name
        exceptions
          no_form            = 1
          no_function_module = 2
          others             = 3.
      if sy-subrc = 0.
*   Colocamos los parametros de Control necesarios.
        lv_control_parameters-getotf = abap_true. "OTF output
        lv_control_parameters-no_dialog = abap_true. "No muestra dialogos de impresion
        lv_control_parameters-preview = space. "No previsualizacion
        lv_output_options-tdnewid = abap_true. "Print parameters,
        lv_output_options-tdimmed = abap_true. "Salida inmediata
        lv_output_options-tddelete = abap_true. "Borrar tras la Salida
        lv_output_options-tddest = 'LOCA'. " Impresora 'LOCAL'


*  formatear horarios
        loop at ti_encabezado assigning field-symbol(<lfs_encabezado>).
          lv_hora1 = <lfs_encabezado>-horario+4(2).
          lv_hora2 = <lfs_encabezado>-horario+6(2).
          concatenate lv_hora1 ':' lv_hora2 into  <lfs_encabezado>-horario+4(6).
          clear: lv_hora1, lv_hora2.

          lv_hora1 = <lfs_encabezado>-horario+13(2).
          lv_hora2 = <lfs_encabezado>-horario+15(2).
          concatenate lv_hora1 ':' lv_hora2 into  <lfs_encabezado>-horario+13(6).
        endloop.

        call function fm_name
          exporting
            control_parameters = lv_control_parameters
            output_options     = lv_output_options
            user_settings      = space
          importing
            job_output_info    = lv_ssf_output
          tables
*	Begin	-->	MgM DCEK903211 impresión pdf error 01/02/2017
*           tisf_final         = ti_final
            tisf_final         = lt_final
*	End	  -->	MgM DCEK903211
            tisf_encabezado    = ti_encabezado
          exceptions
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            others             = 5.
        if sy-subrc <> 0.
          message id sy-msgid type sy-msgty number sy-msgno
                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        else.

          clear lv_pdf_size.

          call function 'CONVERT_OTF'
            exporting
              format                = 'PDF'
            importing
              bin_file              = rv_pdf_xstring
              bin_filesize          = lv_pdf_size
            tables
              otf                   = lv_ssf_output-otfdata[]
              lines                 = lt_dummy
            exceptions
              err_max_linewidth     = 1
              err_format            = 2
              err_conv_not_possible = 3
              err_bad_otf           = 4
              others                = 5.

        endif.
      endif.
    endif.
  endmethod.


  method get_table_alv.

    rt_salida[] = me->ti_final[].

*	Begin	-->	MgM DCEK902579 mensaje sin datos 11/01/2017
    if rt_salida[] is initial.
      "No existen datos que se correspondan con los criterios de selección
      message e809(ewa)
        raising sin_datos.

    endif.
*	End	  -->	MgM DCEK902579

  endmethod.


  method GET_TABLE_PDF.
  endmethod.


  method set_text_column_alv.

    data: lr_column_header  type ref to cl_salv_wd_column_header .

    lr_column_header = cs_column-r_column->get_header( )  .
    lr_column_header->set_ddic_binding_field( if_salv_wd_c_column_settings=>ddic_bind_none )     .
    lr_column_header->set_text( me->if_wd_component_assistance~get_text( iv_text ) ).

  endmethod.
ENDCLASS.
