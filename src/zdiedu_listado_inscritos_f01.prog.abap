*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_LISTADO_INSCRITOS_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_LEER_DATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_leer_datos.

  types: begin of lty_prog,
           objid type hrobjid,
         end of lty_prog.

  data: lt_prog type standard table of lty_prog,
        ls_prog type lty_prog.

  field-symbols: <fs_pre> type gty_zpre_adm.

  select  a~nr_formulario
          a~tipo_documen
          a~nro_documen
          b~sta1_solpago
          b~sta2_solpago
          b~programa_1
          b~programa_2
          b~periodo_acad
          b~tipo_aspirante  "M7454 - HRESTREPO - 31/08/2018
          b~medio
          a~direccion
          a~tel_movil
          a~tel_fijo
          a~email
          a~nombre
          a~sdo_nombre
          a~apellido
          a~sdo_apellido
          a~genero
          a~pais_prcdnc
          a~depto_prcdnc
          c~ciudad_proc
          c~universidad
          c~otra_uni
          c~snp
*Inicio M7454 - HRESTREPO - 31/08/2018
          c~ano
          c~estudios_adic
          c~oportunidad
          c~veces_aspira
          c~progr_actual
          c~grupo_famili
          c~ingreso_hog
*Fin M7454 - HRESTREPO - 31/08/2018
"Alter info: Incluir Información de Colegios en el reporte
"Fecha: 06/05/2021
"Autor: Sebastián Espinosa Marin
         c~colegio
         c~otro_col
"Fil alter
     from zpre_admi_1 as a
     inner join zpre_admi_2 as b
       on a~nr_formulario = b~nr_formulario
     inner join zpre_admi_3 as c
       on a~nr_formulario = c~nr_formulario
     into table gt_zpre_adm
     where nro_documen in so_est and
*& ALTASER LAT 09-09-2021 Inicio: nuevos parámetros fechas y periodos de inscripción.
"          a~fech_registr in s_fechas and
          periodo_acad in s_period.
*& ALTASER LAT 09-09-2021 Final: nuevos parámetros fechas y periodos de inscripción.


  if so_estad[] is not initial.
    delete gt_zpre_adm where sta2_solpago not in so_estad.
  endif.

  if gt_zpre_adm[] is not initial.
    "Alter info: Consultar Textos de Colegios si vienen con código en el campo Colegio
    "Autor: Sebastian Espinosa
    "Fecha: 11/05/2021
*    Consultar la tabla ZEDU_COLEG según los datos que tenga lt_fuente
    select codigo, nombre_colegio from zedu_coleg into table @data(lt_datosmaestrocolegio)
    for all entries in @gt_zpre_adm
    where codigo = @gt_zpre_adm-colegio+0(12).
*    Ordernar tabla interna para posterior consulta
    sort lt_datosmaestrocolegio by codigo ascending.
    "Fin alter.
    loop at gt_zpre_adm assigning <fs_pre>.
      <fs_pre>-objid = <fs_pre>-programa_1.
      <fs_pre>-form  = <fs_pre>-nr_formulario.
      ls_prog-objid = <fs_pre>-programa_1.
      collect ls_prog into lt_prog.
      if <fs_pre>-programa_2 is not initial.
        ls_prog-objid = <fs_pre>-programa_2.
        collect ls_prog into lt_prog.
      endif.
      "Alter info: Consultar Textos de Colegios si vienen con código en el campo Colegio
      "Autor: Sebastian Espinosa
      "Fecha: 11/05/2021
      read table lt_datosmaestrocolegio assigning field-symbol(<fs_datosmaestrocolegio>)
      with key codigo = <fs_pre>-colegio
      binary search.
      if sy-subrc = 0.
        <fs_pre>-colegio = <fs_datosmaestrocolegio>-nombre_colegio.
      endif.
      "Fin alter
    endloop.

    select * from but0id into table gt_but0id
      for all entries in gt_zpre_adm
      where idnumber = gt_zpre_adm-nro_documen
      and   type     = gt_zpre_adm-tipo_documen.

    delete gt_but0id where valid_date_to <> '99991231'.

    if gt_but0id is not initial.

      select partner name_last name_first name_lst2 namemiddle
        from but000 into table gt_but000
        for all entries in gt_but0id
        where partner = gt_but0id-partner.

      select * from cmacbpst into table gt_cmacbpst
        for all entries in gt_but0id
        where partner = gt_but0id-partner.

      if gt_cmacbpst is not initial.
        select * from hrp1001 into table gt_hrp1001
          for all entries in gt_cmacbpst
          where otype = 'ST'
          and   objid = gt_cmacbpst-stobjid
          and   plvar = '01'
          and   rsign = 'A'
          and   relat = '530'
          and   begda in s_fechas "Fechas para optimizar
          and   sclas = cl_hrpiq00const=>c_otype_cs.
        "        delete gt_hrp1001 where sclas <> 'CS'.
        "        delete gt_hrp1001 where begda < '20170101'.

        if gt_hrp1001[] is not initial.
          select * from hrp1001 into table gt_planest
           for all entries in gt_hrp1001
           where otjid = gt_hrp1001-varyf
           and   subty = 'A514'
           and   plvar = '01'.
        endif.
      endif.

    endif.

    select objid short stext from hrp1000
      into table gt_prog
      for all entries in lt_prog
      where otype = 'SC'
      and   objid = lt_prog-objid.


    select * from zslcm_pin into table gt_pin
      for all entries in gt_zpre_adm
      where num_form = gt_zpre_adm-form.

    select * from t005t into table gt_land
      where spras = sy-langu.

    select * from t005h into table gt_city
      where spras = sy-langu.

    select * from zedu_univ into table gt_univ.

    select * from zedu_medios into table gt_medios.

    call function 'DD_DD07V_GET'
      exporting
        domain_name    = 'PIQADMSTATE'
        langu          = sy-langu
        withtext       = 'X'
      tables
        dd07v_tab      = gt_stat
      exceptions
        access_failure = 1
        others         = 2.

*Inicio M7454 - HRESTREPO - 31/08/2018
    "Obtiene las descripciones de las categorias de admision
    select adm_categ adm_categt
      into table gt_adm_categt
      from t7piqadmcategt
      where spras = sy-langu.

    "Ordena los registros
    sort gt_adm_categt by adm_categ.
* Fin M7454 - HRESTREPO - 31/08/2018

  endif.

endform.                    " F_LEER_DATOS

*&---------------------------------------------------------------------*
*&      Form  LEER_DATOS_INS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form leer_datos_ins .
  types: begin of lty_prog,
           objid type hrobjid,
         end of lty_prog.

  data: lt_prog type standard table of lty_prog,
        ls_prog type lty_prog.

  field-symbols: <fs_zpre> type gty_zpre_adm.

  "Encontrar rangos de fechas y periodos para zetas de admisiones
  perform mover_rangos_to_gen.
  perform convert_peryr_to_date tables  s_peryr
                                        s_perid
                                        gt_gen_fechas
                                        gt_gen_period.
  perform mover_gen_to_rangos.

  if lines( so_est ) > 0.
    perform convert_st_to_objid tables so_est
                                       r_objidst.
    if lines( r_objidst ) = 0.
      exit.
    endif.
  endif.

  select  a~nr_formulario
          a~tipo_documen
          a~nro_documen
          b~sta1_solpago
          b~sta2_solpago
          b~programa_1
          b~programa_2
          b~periodo_acad
          b~tipo_aspirante  "M7454 - HRESTREPO - 31/08/2018
          b~medio
          a~direccion
          a~tel_movil
          a~tel_fijo
          a~email
          a~nombre
          a~sdo_nombre
          a~apellido
          a~sdo_apellido
          a~genero
          a~pais_prcdnc
          a~depto_prcdnc
          c~ciudad_proc
          c~universidad
          c~otra_uni
          c~snp
*Inicio M7454 - HRESTREPO - 31/08/2018
          c~ano
          c~estudios_adic
          c~oportunidad
          c~veces_aspira
          c~progr_actual
          c~grupo_famili
          c~ingreso_hog
*Fin M7454 - HRESTREPO - 31/08/2018
"Alter info: Incluir Información de Colegios en el reporte
"Fecha: 06/05/2021
"Autor: Sebastián Espinosa Marin
          c~colegio
          c~otro_col
"Fil alter
     from zpre_admi_1 as a
     inner join zpre_admi_2 as b
     on a~nr_formulario = b~nr_formulario
    inner join zpre_admi_3 as c
     on a~nr_formulario = c~nr_formulario
     into table gt_zpre_adm
     where nro_documen in so_est and
*& ALTASER LAT 12-08-2021 Inicio: nuevos parámetros fechas y periodos de inscripción.
"          a~fech_registr in s_fechas and
          periodo_acad in s_period.
*& ALTASER LAT 12-08-2021 Final: nuevos parámetros fechas y periodos de inscripción.

  if so_estad[] is not initial.
    delete gt_zpre_adm where sta2_solpago not in so_estad.
  endif.

*-- Insertar la data del partner a partir de las Zetas
  if lines( gt_zpre_adm ) > 0.
    select * from but0id into table gt_but0id
        for all entries in gt_zpre_adm
      where type     = gt_zpre_adm-tipo_documen and
            idnumber = gt_zpre_adm-nro_documen.

    if lines( gt_but0id ) > 0.
      select * from cmacbpst into table gt_cmacbpst
           for all entries in gt_but0id
            where partner = gt_but0id-partner.
    endif.

    if lines( gt_cmacbpst ) > 0.
      select partner name_last name_first name_lst2 namemiddle xsexm xsexf
          from but000 into table gt_but000
          for all entries in gt_cmacbpst
          where partner = gt_cmacbpst-partner.
    endif.
  endif.

  "Alter info: Consultar Textos de Colegios si vienen con código en el campo Colegio
  "Autor: Sebastian Espinosa
  "Fecha: 11/05/2021
*    Consultar la tabla ZEDU_COLEG según los datos que tenga lt_fuente
  select codigo, nombre_colegio from zedu_coleg into table @data(lt_datosmaestrocolegio)
  for all entries in @gt_zpre_adm
  where codigo = @gt_zpre_adm-colegio+0(12).
*    Ordernar tabla interna para posterior consulta
  sort lt_datosmaestrocolegio by codigo ascending.
  "Fin alter.
  loop at gt_zpre_adm assigning <fs_zpre>.
    <fs_zpre>-objid = <fs_zpre>-programa_1.
    <fs_zpre>-form  = <fs_zpre>-nr_formulario.
    ls_prog-objid = <fs_zpre>-programa_1.
    collect ls_prog into lt_prog.
    if <fs_zpre>-programa_2 is not initial.
      ls_prog-objid = <fs_zpre>-programa_2.
      collect ls_prog into lt_prog.
    endif.
    "Alter info: Consultar Textos de Colegios si vienen con código en el campo Colegio
    "Autor: Sebastian Espinosa
    "Fecha: 11/05/2021
    read table lt_datosmaestrocolegio assigning field-symbol(<fs_datosmaestrocolegio>)
    with key codigo = <fs_zpre>-colegio
    binary search.
    if sy-subrc = 0.
      <fs_zpre>-colegio = <fs_datosmaestrocolegio>-nombre_colegio.
    endif.
    "Fin alter
  endloop.

  if so_est is initial.
*-- Cambiar el orden de lectura 1ero SC-CS y luego si CS-SC

*--------------------------------------------------------------------------------------------------------------------------------*
*-- ALTASER 26-01-2022 Inicio: Se ha solictado por Admisiones que la data mostrar puede o no tener segmentos en planes, por esta
*   Razón se tendran dos lecturas cuando los ST no fueron digitados en parámetros de entrada: una con matrículas y otra sin esta
*-- ALTASER 26-01-2022 Final: Dos opciones de lecturas para planes con o sin segmentos de estudio.
*--------------------------------------------------------------------------------------------------------------------------------*
    if p_matri = gc_matri_mat.
      select a~mandt a~otype a~objid a~plvar a~rsign a~relat a~istat a~priox a~begda a~endda a~varyf a~seqnr a~infty
             a~otjid a~subty a~aedtm a~uname a~reasn a~histo a~itxnr a~sclas a~sobid a~prozt a~adatanr
         into table gt_planest
         from hrp1001 as a
         inner join hrp1730 as b
            on a~sobid = b~objid
         inner join hrp1769 as c
            on c~objid = a~objid
         where a~otype = cl_hrpiq00const=>c_otype_cs        and
               a~plvar =  cl_hrpiq00const=>c_plvar_active   and
               a~rsign = 'A'                                and
               a~relat = '514'                              and
             ( c~begda in s_fechas or
               c~endda = cl_hrpiq00const=>c_date_highdate ) and
               a~sclas = cl_hrpiq00const=>c_otype_sc        and
               linea_edu = gc_lina_edu_f.
    endif.
    if p_matri = gc_matri_tod or p_matri = gc_matri_adm or p_matri = gc_matri_sin.
      select a~mandt a~otype a~objid a~plvar a~rsign a~relat a~istat a~priox a~begda a~endda a~varyf a~seqnr a~infty
             a~otjid a~subty a~aedtm a~uname a~reasn a~histo a~itxnr a~sclas a~sobid a~prozt a~adatanr
         into table gt_planest
         from hrp1001 as a
         inner join hrp1730 as b
            on a~sobid = b~objid
         where a~otype = cl_hrpiq00const=>c_otype_cs        and
               a~plvar =  cl_hrpiq00const=>c_plvar_active   and
               a~rsign = 'A'                                and
               a~relat = '514'                              and
               a~sclas = cl_hrpiq00const=>c_otype_sc        and
               linea_edu = gc_lina_edu_f.
    endif.

    if  gt_planest[] is not initial.
      select * from hrp1001 into table gt_hrp1001
        for all entries in gt_planest
        where otype = cl_hrpiq00const=>c_otype_st     and
              plvar = cl_hrpiq00const=>c_plvar_active and
              objid in r_objidst                      and
              rsign = 'A'                             and
              relat = '530'                           and
              begda in s_fechas                       and
              varyf = gt_planest-otjid.
      if gt_planest is initial.
        refresh gt_hrp1001. "No hubo data en el expediente.
      endif.

      "Hay línea en hrp1001 necesariamente.
      select * into table gt_pad530
        from hrpad530
          for all entries in gt_hrp1001
          where adatanr   = gt_hrp1001-adatanr and
                adm_ayear in s_peryr           and
                adm_perid in s_perid.
      "Primeras opciones de expediente únicamente -> SC
      perform rango_opciones_sc tables gr_opt_sc    "Tabla de rangos opcines de SC
                                using '01'.         "Opción segunda de SC
      "Segundas opciones de expediente únicamente -> SC
      perform rango_opciones_sc tables gr_opt_sc    "Tabla de rangos opcines de SC
                                using '02'.         "Opción segunda de SC
      call function 'ZSLCM_HRPAD530_TO_HRP1001_SC'
        exporting
          ir_choice     = gr_opt_sc
        tables
          t_hrpad530    = gt_pad530
        changing
          et_hrp1001_sc = gt_hrp1001_opt.
    endif.
  else. "Camino de lecturas a partir del estudiante convertido en Objid.
    select * from hrp1001 into table gt_hrp1001
     where otype = cl_hrpiq00const=>c_otype_st     and
           plvar = cl_hrpiq00const=>c_plvar_active and
           objid in r_objidst                      and
           rsign = 'A'                             and
           relat = '530'                           and
           begda in s_fechas.
    if gt_hrp1001[] is not initial.
      "Hay línea en hrp1001 necesariamente.
      select * into table gt_pad530
        from hrpad530
          for all entries in gt_hrp1001
          where adatanr   = gt_hrp1001-adatanr and
                adm_ayear in s_peryr           and
                adm_perid in s_perid.

*-- Puede darse que existan hrpad530 sin hrp1769 (el reporte debería mostrarlos si en los parámetros así se ha decidido
      "Primeras opciones de expediente únicamente -> SC
      perform rango_opciones_sc tables gr_opt_sc    "Tabla de rangos opcines de SC
                                using '01'.         "Opción segunda de SC

      "Segundas opciones de expediente únicamente -> SC
      perform rango_opciones_sc tables gr_opt_sc    "Tabla de rangos opcines de SC
                                using '02'.         "Opción segunda de SC

      call function 'ZSLCM_HRPAD530_TO_HRP1001_SC'
        exporting
          ir_choice     = gr_opt_sc
        tables
          t_hrpad530    = gt_pad530
        changing
          et_hrp1001_sc = gt_hrp1001_opt.

      if p_matri = gc_matri_mat or p_matri = gc_matri_sin.
        select a~mandt a~otype a~objid a~plvar a~rsign a~relat a~istat a~priox a~begda a~endda a~varyf a~seqnr a~infty
               a~otjid a~subty a~aedtm a~uname a~reasn a~histo a~itxnr a~sclas a~sobid a~prozt a~adatanr
           into table gt_planest
           from hrp1001 as a
           inner join hrp1730 as b
              on a~sobid = b~objid
           inner join hrp1769 as c
              on c~objid = a~objid
           for all entries in gt_hrp1001
           where a~otype   = cl_hrpiq00const=>c_otype_cs        and
                 a~plvar   =  cl_hrpiq00const=>c_plvar_active   and
                 a~rsign   = 'A'                                and
                 a~relat   = '514'                              and
               ( c~begda   in s_fechas or
                 c~endda   in s_fechas or
                 c~endda   = cl_hrpiq00const=>c_date_highdate ) and
                 a~sclas   = cl_hrpiq00const=>c_otype_sc        and
                 linea_edu = gc_lina_edu_f                      and
                 a~otjid   = gt_hrp1001-varyf.
      endif.

      if p_matri = gc_matri_tod or p_matri = gc_matri_adm or p_matri = gc_matri_sin.
        select a~mandt a~otype a~objid a~plvar a~rsign a~relat a~istat a~priox a~begda a~endda a~varyf a~seqnr a~infty
               a~otjid a~subty a~aedtm a~uname a~reasn a~histo a~itxnr a~sclas a~sobid a~prozt a~adatanr
          into table gt_planest
          from hrp1001 as a
          inner join hrp1730 as b
            on a~sobid = b~objid
        for all entries in gt_hrp1001
        where a~otype   = cl_hrpiq00const=>c_otype_cs    and
          a~plvar   =  cl_hrpiq00const=>c_plvar_active   and
          a~rsign   = 'A'                                and
          a~relat   = '514'                              and
          a~sclas   = cl_hrpiq00const=>c_otype_sc        and
          linea_edu = gc_lina_edu_f                      and
          a~otjid   = gt_hrp1001-varyf.
      endif.

    endif.
  endif.

*------------

  if lines( gt_planest ) > 0.
    "Nuevo para tener todas la inscripciones de SAP
    select * into table gt_hrp1769
      from hrp1769
      for all entries in gt_planest
      where plvar = cl_hrpiq00const=>c_plvar_active and
            otype = cl_hrpiq00const=>c_otype_cs     and
            objid = gt_planest-objid                and
            ( endda in s_fechas or
              endda = cl_hrpiq00const=>c_date_highdate ).

    "Reunir programas del periodo desde el estándar
    loop at gt_planest into data(gr_planest).
      ls_prog-objid = gr_planest-sobid.
      collect ls_prog into lt_prog.
    endloop.

    "Adicionar programas segundas opciones
    loop at gt_hrp1001_opt into gr_planest.
      ls_prog-objid = gr_planest-sobid.
      collect ls_prog into lt_prog.
    endloop.
  endif.

  if lines( gt_hrp1001 ) > 0.
    select * from cmacbpst appending table gt_cmacbpst
      for all entries in gt_hrp1001
      where stobjid = gt_hrp1001-objid.
  endif.

  if lines( gt_cmacbpst ) > 0.
    select partner name_last name_first name_lst2 namemiddle xsexm xsexf
      from but000 appending table gt_but000
      for all entries in gt_cmacbpst
      where partner = gt_cmacbpst-partner.
  endif.

  if lines( gt_but000 ) > 0.
    select * from but0id appending table gt_but0id
      for all entries in gt_but000
      where partner = gt_but000-partner and
            idnumber in so_est.
  endif.

  sort gt_but0id by partner type idnumber.
  delete adjacent duplicates from gt_but0id comparing partner type idnumber.

  select objid short stext from hrp1000
    into table gt_prog
    for all entries in lt_prog
    where otype = 'SC'
    and   objid = lt_prog-objid.

  select * from zslcm_pin into table gt_pin
    for all entries in gt_zpre_adm
    where num_form = gt_zpre_adm-form.

  select * from t005t into table gt_land
    where spras = sy-langu.

  select * from t005h into table gt_city
    where spras = sy-langu.

  select * from zedu_univ into table gt_univ.

  select * from zedu_medios into table gt_medios.

  call function 'DD_DD07V_GET'
    exporting
      domain_name    = 'PIQADMSTATE'
      langu          = sy-langu
      withtext       = 'X'
    tables
      dd07v_tab      = gt_stat
    exceptions
      access_failure = 1
      others         = 2.

*Inicio M7454 - HRESTREPO - 31/08/2018
  "Obtiene las descripciones de las categorias de admision
  select adm_categ adm_categt
    into table gt_adm_categt
    from t7piqadmcategt
    where spras = sy-langu.

  "Ordena los registros
  sort gt_adm_categt by adm_categ.
* Fin M7454 - HRESTREPO - 31/08/2018
endform.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_DATOS_INS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_procesar_datos_ins .

  types: begin of ty_inscscfin,
           sc1       type hrobjid, "Plan1 primera opción
           sc2       type hrobjid, "Plan2 segundo plan (para segunda opción).
           sc3       type hrobjid, "Plan cambiado (este era el sc viejo).
           stat1     type char20,  "Estatus primer programa
           stat2     type char20,  "Estatus segundo programa
           cambio_sc type c,       "Cambio de plan de estudio
           color     type c,       "Color 1-admitido 2-Inscrito
         end of ty_inscscfin.

  data: ls_planest      type hrp1001,                "Rec. hrp1001
        lt_530          type table of hrpad530,      "Tabla hrpad530 para agrupamientos de opcines 1 y 2
        lr_530_aux      type hrpad530,               "rec. hrpad530
        lv_adm_categ    type piqadm_categ,           "M7454 - HRESTREPO - 31/08/2018
        lv_cambio_sc    type c,                      "Cambio plan de estudio en expediente
        ls_data         type gty_data,               "Rec. para el alv final
        lv_contador     type int3,                   "Contador registros.
        lv_flag_error   type c,                      "Flag para errores
        lv_stat         type istat_d,                "Variable para búsqueda de estatus
        lv_cont_sc1001  type i,                      "Cantidad de SC de 1001 que no reemplazaron planes de zetas admisiones
        lv_stat_sc1_adm type c,                      "Existe un primer programa
        lv_stat_sc2_adm type c,                      "Existe un segundo programa
        lv_sc1_admi     type char10,                 "ID Plan de estudio 1 de admisiones
        lv_sc2_admi     type char10,                 "ID Plan de estudio 2 de admisiones
        lt_inscscfin    type table of ty_inscscfin,  "Tabla inscripciones finales para reporte
        lr_inscscfin    type ty_inscscfin,           "Rec. inscripciones finales para reporte
        lr_zpre         type gty_zpre_adm,           "Rec. Admisiones
        lv_sgda_opc     type i.                      "Contador segundas opciones.

  field-symbols:
    <fs_but0id>     type but0id,
    <fs_prog>       type gty_prog,
    <fs_but000>     type gty_but000,
    <fs_pin>        type zslcm_pin,
    <fs_cmacbpst>   type cmacbpst,
    <fs_hrp1001>    type hrp1001,
    <fs_inscscfin>  type ty_inscscfin,   "Rec. Tabla final de opciones de programas
    <fs_adm_categt> type gty_adm_categt. "M7454 - HRESTREPO - 31/08/2018

  constants:
    lc_stat_nul type char20 value 'Prim. Opc. Sin estatus',  "Estatus de estudiantes con pin pero sin diligenciar formulario
    lc_stat_sin type char20 value 'Sgda. Opc. Sin estatus'.  "Estatus de planes que no se inscribieron completamente en el expediente

  sort gt_cmacbpst by stobjid.
  delete adjacent duplicates from gt_cmacbpst comparing stobjid.

  gt_hrp1001_aux[] = gt_hrp1001[].
  sort gt_hrp1001 by objid.
  sort gt_hrp1001_aux by adatanr.
  gt_planest_aux[] = gt_planest[].
  sort gt_planest_aux by otjid.

*-- Lectura de los partner-st de todo el periodo, luego lectura de tablas zetas de admisión
  loop at gt_cmacbpst assigning <fs_cmacbpst>.
    clear: ls_data,
           lv_flag_error,
           lv_stat_sc1_adm,
           lv_stat_sc2_adm,
           lv_cont_sc1001,
           lv_cambio_sc,
           lv_sc2_admi.

    ls_data-contador = 1.
    ls_data-st        = <fs_cmacbpst>-stobjid.
    ls_data-matricula = <fs_cmacbpst>-student12.

    if <fs_cmacbpst>-stobjid = '10080004'.
      break ltafur.
    endif.

    "Read identification
    read table gt_but0id assigning <fs_but0id>
      with key partner  = <fs_cmacbpst>-partner.
    check sy-subrc = 0.

    ls_data-partner = <fs_but0id>-partner.
    ls_data-nro_documen = <fs_but0id>-idnumber.

    "Nombre a partir del partner
    read table gt_but000 assigning <fs_but000>
    with key partner = <fs_but0id>-partner.
    if sy-subrc eq 0.
      concatenate <fs_but000>-name_first <fs_but000>-namemiddle
      <fs_but000>-name_last <fs_but000>-name_lst2 into ls_data-nombre
      separated by space.
    endif.

    "Cruzar vs data de admisiones zetas x identificación
    read table gt_zpre_adm into lr_zpre
      with key tipo_documen = <fs_but0id>-type
               nro_documen  = <fs_but0id>-idnumber.

    if sy-subrc <> 0. "Registro creado en el estándar y no en la WEB.
      "bucar data de ST creado directamente en exediente.
      perform completar_st_expediente tables gt_planest
                                             gt_hrp1001
                                             gt_land
                                      using <fs_cmacbpst>
                                            <fs_but000>
                                            <fs_but0id>
                                      changing ls_data
                                               lv_flag_error.
      check lv_flag_error = cl_hrpiq00const=>c_unchecked.
      "      append ls_data to gt_data.
      "      continue.
    else.
      if ls_data-nombre is initial.
        clear ls_data-partner.
        concatenate lr_zpre-nombre lr_zpre-sdo_nombre lr_zpre-apellido
                    lr_zpre-sdo_apellido into ls_data-nombre separated by space.
      endif.
    endif.

    perform mover_campos_zadmi using lr_zpre ls_data.

    condense ls_data-cambio_plan.
    condense ls_data-prog_ant.
    condense ls_data-nombre.

*-- Agrupamientos de primeras opciones con cambio o sin cambio.
    refresh lt_530.
    read table gt_hrp1001 assigning <fs_hrp1001>
       with key objid = <fs_cmacbpst>-stobjid.
    loop at gt_hrp1001 assigning <fs_hrp1001> from sy-tabix.
      if <fs_hrp1001>-objid <> <fs_cmacbpst>-stobjid.
        exit.
      endif.
      read table gt_pad530 into data(lr_530)
        with key adatanr = <fs_hrp1001>-adatanr.
      check sy-subrc = 0.
      append lr_530 to lt_530.
    endloop.

    refresh lt_inscscfin.
    loop at lt_530 into lr_530.
      check lr_530-choice_no <> '02'. "no sea segundas opciones.
      if lr_530-choice_no <> '01'. "Posible error guardar en un log.
        continue.
      endif.

      if p_matri = gc_matri_sin.
        sy-subrc = 0.
        exit.
      endif.

      lr_530_aux = lr_530.

      read table gt_hrp1001_aux assigning <fs_hrp1001>
        with key adatanr =  lr_530-adatanr.

      clear lr_inscscfin.

      if p_matri = gc_matri_mat.
        read table gt_planest_aux into ls_planest
            with key otjid = <fs_hrp1001>-varyf.
        if sy-subrc <> 0.
          continue.
        endif.
        lr_inscscfin-color = '2'.
      endif.

      "Opción completa o solo admisiones
      if p_matri = gc_matri_tod or p_matri = gc_matri_adm.
        read table gt_planest into ls_planest
            with key otjid = <fs_hrp1001>-varyf.
        if sy-subrc <> 0.
          continue.
        endif.
        read table gt_hrp1769 with key objid = ls_planest-objid transporting no fields.
        if sy-subrc <> 0.
          lr_inscscfin-color = '1'.
        else.
          if p_matri = gc_matri_tod.
            lr_inscscfin-color = '2'.
          endif.
        endif.
      endif.

      "Agrupamiento por opciones sobre el expediente.
      at new choice_no.
        "Revisar si los planes de expedientes se sobreponen a los de admisiones zetas
        if ls_planest-sobid = lr_zpre-programa_1.
          read table gt_stat into gs_stat
            with key domvalue_l = <fs_hrp1001>-istat.
          if sy-subrc eq 0.
            ls_data-stat = gs_stat-ddtext.
          endif.
          "          lr_inscscfin-cambio_sc = cl_hrpiq00const=>c_checked. "Flag cambio de plan de estudio
          lr_inscscfin-sc1       = ls_planest-sobid.                "SC desde el expediente
          "          lr_inscscfin-sc3       = lr_zpre-programa_1.   "SC del cabio (plan viejo).
          lr_inscscfin-stat1     = ls_data-stat.                    "Estatus desde el expediente del estudiante
          append lr_inscscfin to lt_inscscfin.
        else. "--No hunbo coincidencia entre los SC de admisiones vs los de expediente (data nueva directamente de expediente).
          "Revisar si planes finales 1 y 2 están diligenciados o si los que vienen de la gt_hrp1001 entran en los
          "periodos de los parámetros de entrada (hasta aquí pueden cumplir pero con el parámetro fechas).
          if lr_zpre-programa_1 is initial and ls_planest-sobid is not initial. "Plan del expediente y nohabía nada en admisiones zetas
            lr_zpre-programa_1 = ls_planest-sobid.
            lv_stat_sc1_adm    = cl_hrpiq00const=>c_checked.
            read table gt_stat into gs_stat
              with key domvalue_l = <fs_hrp1001>-istat.
            if sy-subrc eq 0.
              ls_data-stat = gs_stat-ddtext.
            endif.
          else. "Segundo programa del expediente directamente
            if lr_zpre-programa_1 is not initial and ls_planest-sobid is not initial. "cambio de plan de estudio
              "Buscar estatus desde el plan que está inscrito en el expediente
              read table gt_planest into data(ls_planest_aux)
                with key sobid = ls_planest-sobid.
              if sy-subrc = 0.
                read table gt_hrp1001_aux into data(lr_hrp1001_aux)
                  with key sobid = ls_planest_aux-objid.
                read table gt_stat into gs_stat
                  with key domvalue_l = lr_hrp1001_aux-istat.
                if sy-subrc eq 0.
                  ls_data-stat = gs_stat-ddtext.
                endif.
              endif.
              lr_inscscfin-cambio_sc = cl_hrpiq00const=>c_checked. "Flag cambio de plan de estudio
              lr_inscscfin-sc1       = ls_planest-sobid.           "SC desde el expediente
              lr_inscscfin-sc3       = lr_zpre-programa_1.         "SC del cabio (plan viejo).
              lr_inscscfin-stat1     = ls_data-stat.               "Estatus desde el expediente del estudiante
              append lr_inscscfin to lt_inscscfin.
            endif.
          endif.
          add 1 to lv_cont_sc1001. "Cantidad de SC de 1001 que no han tenido un reemplazo en zetas admisiones.
        endif.
      endat.
    endloop.
    if sy-subrc <> 0. "Debe ser un estudiante con solo zetas y sin expediente (pagó pero no se ha inscrito).
      if p_matri = gc_matri_tod or p_matri = gc_matri_sin.
        clear lr_inscscfin.
        lr_inscscfin-sc1 = lr_zpre-programa_1.
        lr_inscscfin-stat1 = lc_stat_nul.
        lr_inscscfin-color = '5'.
        append lr_inscscfin to lt_inscscfin.
      endif.
    endif.
*-- Final Agrupamientos de primeras opciones con cambio o sin cambio.

*-- Revisar si hay segundas opciones desede el expediente
    clear lv_sgda_opc.
    loop at lt_530 into lr_530 where choice_no = '02'.
      read table gt_hrp1001_aux assigning <fs_hrp1001>
        with key adatanr =  lr_530-adatanr.

      read table gt_hrp1001_opt into data(lr_1001_sda)
       with key objid = <fs_hrp1001>-sobid.
      check sy-subrc = 0.

      clear lr_inscscfin.
      if lr_zpre-programa_2 is initial. "Modify sda opción
        loop at lt_inscscfin assigning <fs_inscscfin>.
          read table gt_stat into gs_stat
             with key domvalue_l = <fs_hrp1001>-istat.
          if sy-subrc eq 0.
            <fs_inscscfin>-stat2 = gs_stat-ddtext.
          endif.

          <fs_inscscfin>-sc2       = lr_1001_sda-sobid.        "SC desde tabla de expediente
          "          <fs_inscscfin>-stat2     = lc_stat_sin.              "Estatus desde el expediente del estudiante
          add 1 to lv_sgda_opc.
        endloop.
      endif.
    endloop.
    if lv_sgda_opc = 0.
      if p_matri = gc_matri_tod or p_matri = gc_matri_sin.
        if lr_zpre-programa_2 is not initial.
          loop at lt_inscscfin assigning <fs_inscscfin>.
            <fs_inscscfin>-sc2       = lr_zpre-programa_2.         "SC desde tablas zetas de admisiones
            <fs_inscscfin>-stat2     = lc_stat_sin.                "Estatus no se ha encontrado no hay expediente
          endloop.
        endif.
      endif.
    endif.
*-- Fin Revisar si hay segundas opciones desede el expediente

    loop at lt_inscscfin into lr_inscscfin.
      refresh ls_data-it_colors.
      clear: ls_data-prog, ls_data-prog2, ls_data-prog_ant.
      ls_data-cambio_plan = lr_inscscfin-cambio_sc.
      ls_data-stat        = lr_inscscfin-stat1.
      ls_data-stat2       = lr_inscscfin-stat2.

      read table gt_prog assigning <fs_prog>
      with key objid = lr_inscscfin-sc1.
      if sy-subrc eq 0.
        ls_data-prog = <fs_prog>-stext.
      endif.

      read table gt_prog assigning <fs_prog>
      with key objid = lr_inscscfin-sc2.
      if sy-subrc eq 0.
        ls_data-prog2 = <fs_prog>-stext.
      endif.

      "Programa anterior del cambio
      read table gt_prog assigning <fs_prog>
      with key objid = lr_inscscfin-sc3.
      if sy-subrc eq 0.
        ls_data-prog_ant = <fs_prog>-stext.
      endif.

      "colores y textos para SC admitido o inscrito
      case lr_inscscfin-color.
        when '1'.
          ls_data-sc_mat = gc_sc_admitido.
          gr_colors-fname = 'SC_MAT'.
          gr_colors-color-col = col_group.
          gr_colors-color-int = 1.
          append gr_colors to ls_data-it_colors.
        when '2'.
          ls_data-sc_mat = gc_sc_segmento.
          gr_colors-fname = 'SC_MAT'.
          gr_colors-color-col = col_positive.
          gr_colors-color-int = 1.
          append gr_colors to ls_data-it_colors.
        when '5'.
          ls_data-sc_mat = gc_sc_sinformu.
          gr_colors-fname = 'SC_MAT'.
          gr_colors-color-col = col_negative.
          gr_colors-color-int = 1.
          append gr_colors to ls_data-it_colors.
      endcase.

      read table gt_pin assigning <fs_pin>
      with key num_form = lr_zpre-form.
      if sy-subrc eq 0.
        ls_data-pin = <fs_pin>-pin.
      endif.

      read table gt_land into gs_land
      with key land1 = lr_zpre-pais_prcdnc.
      if sy-subrc eq 0.
        ls_data-pais = gs_land-landx.
      else.
        ls_data-pais = lr_zpre-pais_prcdnc.
      endif.

      read table gt_city into gs_city
      with key land1 = lr_zpre-pais_prcdnc
               regio = lr_zpre-depto_prcdnc
               cityc = lr_zpre-ciudad_proc.
      if sy-subrc eq 0.
        ls_data-ciudad_proc = gs_city-bezei.
      else.
        ls_data-ciudad_proc = lr_zpre-ciudad_proc.
      endif.

      read table gt_univ into gs_univ
      with key codigo = ls_data-universidad.
      if sy-subrc eq 0.
        ls_data-universidad = gs_univ-universidad.
      endif.

      read table gt_medios into gs_medio
      with key id = ls_data-cod_medio.
      if sy-subrc eq 0.
        ls_data-desc_medio = gs_medio-medio.
      endif.

*Inicio M7454 - HRESTREPO - 31/08/2018
      "Obtiene la descripcion de la categoria de admision
      lv_adm_categ = ls_data-tipo_aspirante.
      read table gt_adm_categt assigning <fs_adm_categt>
        with key adm_categ = lv_adm_categ
        binary search.
      "Si encuentra el registro
      if sy-subrc eq 0.
        "Asigna la descripcion del tipo de aspirante
        ls_data-t_asp_desc = <fs_adm_categt>-adm_categt.
      endif.
*Fin M7454 - HRESTREPO - 31/08/2018

      add 1 to lv_contador.

      append ls_data to gt_data.
    endloop. "Fin loop repetición por planes
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESAR_DATOS_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_procesar_datos_pre .

  data: lv_adm_categ type piqadm_categ.  "M7454 - HRESTREPO - 31/08/2018

  field-symbols: <fs_zpre>       type gty_zpre_adm,
                 <fs_but0id>     type but0id,
                 <fs_prog>       type gty_prog,
                 <fs_but000>     type gty_but000,
                 <fs_pin>        type zslcm_pin,
                 <fs_cmacbpst>   type cmacbpst,
                 <fs_adm_categt> type gty_adm_categt. "M7454 - HRESTREPO - 31/08/2018

  data: ls_data type gty_data.

  loop at gt_zpre_adm assigning <fs_zpre>.
    clear ls_data.
    ls_data-contador = 1.

    move-corresponding <fs_zpre> to ls_data.
    concatenate <fs_zpre>-nombre <fs_zpre>-sdo_nombre <fs_zpre>-apellido <fs_zpre>-sdo_apellido
           into ls_data-nombre separated by space.
    condense ls_data-nombre.

    read table gt_prog assigning <fs_prog>
    with key objid = <fs_zpre>-programa_1.
    if sy-subrc eq 0.
      ls_data-prog = <fs_prog>-stext.
    endif.

    read table gt_prog assigning <fs_prog>
    with key objid = <fs_zpre>-programa_2.
    if sy-subrc eq 0.
      ls_data-prog2 = <fs_prog>-stext.
    endif.

    read table gt_pin assigning <fs_pin>
    with key num_form = <fs_zpre>-form.
    if sy-subrc eq 0.
      ls_data-pin = <fs_pin>-pin.
    endif.

    read table gt_land into gs_land
    with key land1 = <fs_zpre>-pais_prcdnc.
    if sy-subrc eq 0.
      ls_data-pais = gs_land-landx.
    else.
      ls_data-pais = <fs_zpre>-pais_prcdnc.
    endif.

    read table gt_city into gs_city
      with key land1 = <fs_zpre>-pais_prcdnc
               regio = <fs_zpre>-depto_prcdnc
               cityc = <fs_zpre>-ciudad_proc.
    if sy-subrc eq 0.
      ls_data-ciudad_proc = gs_city-bezei.
    else.
      ls_data-ciudad_proc = <fs_zpre>-ciudad_proc.
    endif.

    read table gt_univ into gs_univ
    with key codigo = ls_data-universidad.
    if sy-subrc eq 0.
      ls_data-universidad = gs_univ-universidad.
    endif.

    read table gt_medios into gs_medio
    with key id = ls_data-cod_medio.
    if sy-subrc eq 0.
      ls_data-desc_medio = gs_medio-medio.
    endif.

*Inicio M7454 - HRESTREPO - 31/08/2018
    "Obtiene la descripcion de la categoria de admision
    lv_adm_categ = ls_data-tipo_aspirante.
    read table gt_adm_categt assigning <fs_adm_categt>
      with key adm_categ = lv_adm_categ
      binary search.
    "Si encuentra el registro
    if sy-subrc eq 0.
      "Asigna la descripcion del tipo de aspirante
      ls_data-t_asp_desc = <fs_adm_categt>-adm_categt.
    endif.
*Fin M7454 - HRESTREPO - 31/08/2018

    append ls_data to gt_data.
  endloop.

  sort gt_data by nro_documen prog nr_formulario descending .
  delete adjacent duplicates from gt_data comparing nro_documen prog.

endform.


*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mostrar_alv.

  data: lo_columns      type ref to cl_salv_columns_table,
        lo_display      type ref to cl_salv_display_settings,
        lo_layout       type ref to cl_salv_layout,
        lo_sorts        type ref to cl_salv_sorts,
        lo_filters      type ref to cl_salv_filters,
        lo_functions    type ref to cl_salv_functions,
        lo_events       type ref to cl_salv_events_table,
        lo_aggregations type ref to cl_salv_aggregations,
        wa_lay_key      type salv_s_layout_key,
        lv_tittle       type lvc_title.

  data: lr_events type ref to cl_salv_events_table.

  try.
      call method cl_salv_table=>factory
        importing
          r_salv_table = go_alv
        changing
          t_table      = gt_data.

      lo_columns = go_alv->get_columns( ).
      perform set_columns using lo_columns.

*     Configura opciones de visualizacion (zebra)
      lo_display = go_alv->get_display_settings( ).
      lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*      PERFORM set_top.

*     Activa el status estandar del ALV con todas sus funciones
      lo_functions = go_alv->get_functions( ).
*      lt_functions = lo_functions->get_functions( ).
      lo_functions->set_all( 'X' ).

      if rb_ins = cl_hrpiq00const=>c_checked.
        lo_display->set_list_header('Lista de inscritos').
      else.
        lo_display->set_list_header('Lista de preinscritos').
      endif.

*     Configura el ordenamiento del ALV
      lo_sorts = go_alv->get_sorts( ).
*      PERFORM set_sorts   USING lo_sorts.

*     Configura el Layout del ALV, si se tiene alguno por defecto se adopta este
      lo_layout = go_alv->get_layout( ).

      wa_lay_key-report = sy-repid.
      lo_layout->set_key( wa_lay_key ).
      lo_layout->set_save_restriction( 1 ). " 1 -Válido para todos los usuarios y específico de usuario
      lo_layout->set_default( 'X' ).

* visualizar botones especiales
      go_alv->set_screen_status( pfstatus = 'ZALV_STANDARD'
                                        report = sy-repid
                                        set_functions = go_alv->c_functions_all ).


* Logo ALV (Subir logo por transacción OAER)
      perform cabecera_alv1.

      lr_events = go_alv->get_event( ).

      create object gr_events.
      set handler gr_events->on_user_command for lr_events.
      set handler gr_events->on_link_click   for lr_events.

      go_alv->display( ).
    catch cx_salv_msg.                                  "#EC NO_HANDLER
  endtry.

endform.                    " MOSTRAR_ALV

*&---------------------------------------------------------------------*
*&      Form  set_columns
*&---------------------------------------------------------------------*
*&   Rutina que modifica los titulos de algunas columnas del ALV..
*&---------------------------------------------------------------------*
*&     -->PR_COLUMNS  Objeto que contiene las columnas del ALV.
*&---------------------------------------------------------------------*
form set_columns  using    pr_columns   type ref to cl_salv_columns_table.

  data: lr_column   type ref to cl_salv_column_table,
        ls_ddic_ref type salv_s_ddic_reference.

  pr_columns->set_key_fixation( abap_true ).

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'NRO_DOCUMEN' ).
      lr_column->set_long_text( 'Nro Id' ).
      lr_column->set_medium_text( 'Nro Id' ).
      lr_column->set_short_text( 'Nro Id' ).
      lr_column->set_output_length( 12 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'NOMBRE' ).
      lr_column->set_long_text( 'Nombre Estudiante' ).
      lr_column->set_medium_text( 'Nombre Estudiante' ).
*      lr_column->set_short_text( 'Asig Nue' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PROG' ).
      lr_column->set_long_text( 'Programa 1' ).
      lr_column->set_medium_text( 'Programa 1' ).
      lr_column->set_short_text( 'Prog 1' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'STAT' ).
      lr_column->set_long_text( 'Status Programa 1' ).
      lr_column->set_medium_text( 'Status Prg.1' ).
      lr_column->set_short_text( 'Status P1' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'SC_MAT' ).
      lr_column->set_long_text( 'Admi./Insc.' ).
      lr_column->set_medium_text( 'Plan Adm/insc' ).
      lr_column->set_short_text( 'Adm/insc' ).
      lr_column->set_output_length( 14 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PROG2' ).
      lr_column->set_long_text( 'Programa 2' ).
      lr_column->set_medium_text( 'Programa 2' ).
      lr_column->set_short_text( 'Prog 2' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'STAT2' ).
      lr_column->set_long_text( 'Status Programa 2' ).
      lr_column->set_medium_text( 'Status Prg.2' ).
      lr_column->set_short_text( 'Status P2' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'SDO_DIRECCION' ).
      lr_column->set_long_text( 'Direccion' ).
      lr_column->set_medium_text( 'Direccion' ).
      lr_column->set_short_text( 'Direccion' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PERIODO' ).
*      lr_column->set_long_text( 'Direccion' ).
      lr_column->set_medium_text( 'Periodo' ).
      lr_column->set_short_text( 'Periodo' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.
  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PIN' ).
      lr_column->set_long_text( 'Pin' ).
      lr_column->set_medium_text( 'Pin' ).
      lr_column->set_short_text( 'Pin' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'SNP' ).
      lr_column->set_long_text( 'SNP' ).
      lr_column->set_medium_text( 'SNP' ).
      lr_column->set_short_text( 'SNP' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.


  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PAIS' ).
      lr_column->set_long_text( 'Pais' ).
      lr_column->set_medium_text( 'Pais' ).
      lr_column->set_short_text( 'Pais' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'GENERO' ).
      lr_column->set_long_text( 'Sexo' ).
      lr_column->set_medium_text( 'Sexo' ).
      lr_column->set_short_text( 'Sexo' ).
      lr_column->set_output_length( 5 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'CIUDAD_PROC' ).
      lr_column->set_long_text( 'Ciudad proc.' ).
      lr_column->set_medium_text( 'Ciudad proc.' ).
      lr_column->set_short_text( 'Ciu.proc.' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'UNIVERSIDAD' ).
      lr_column->set_long_text( 'Universidad' ).
      lr_column->set_medium_text( 'Universidad' ).
      lr_column->set_short_text( 'Univ.' ).
      lr_column->set_output_length( 30 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'OTRA_UNI' ).
      lr_column->set_long_text( 'Universidad' ).
      lr_column->set_medium_text( 'Universidad' ).
      lr_column->set_short_text( 'Univ.' ).
      lr_column->set_output_length( 30 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'TEL_MOVIL' ).
      lr_column->set_long_text( 'Tel Movil' ).
      lr_column->set_medium_text( 'Tel Movil' ).
      lr_column->set_short_text( 'Tel Movil' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'TEL_FIJO' ).
      lr_column->set_long_text( 'Tel Fijo' ).
      lr_column->set_medium_text( 'Tel Fijo' ).
      lr_column->set_short_text( 'Tel Fijo' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'COD_MEDIO' ).
      lr_column->set_long_text( 'Código Medio' ).
      lr_column->set_medium_text( 'Código Medio' ).
      lr_column->set_short_text( 'Cod.Medio' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'DESC_MEDIO' ).
      lr_column->set_long_text( 'Medio por el que se enteró del prg' ).
      lr_column->set_medium_text( 'Medio' ).
      lr_column->set_short_text( 'Medio' ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'EMAIL' ).
      lr_column->set_long_text( 'Email' ).
      lr_column->set_medium_text( 'Email' ).
      lr_column->set_short_text( 'Email' ).
*      lr_column->set_visible( abap_false ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

*Inicio M7454 - HRESTREPO - 31/08/2018  CLEAR lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'TIPO_ASPIRANTE' ).
      lr_column->set_long_text( 'Cod. Categoría de admisión' ).
      lr_column->set_medium_text( 'Cod. Categ. admisión' ).
      lr_column->set_short_text( 'Cod.Categ.' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'T_ASP_DESC' ).
      lr_column->set_long_text( 'Categoría de admisión' ).
      lr_column->set_medium_text( 'Categoría admisión' ).
      lr_column->set_short_text( 'Categoría' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'ANO' ).
      lr_column->set_long_text( 'Año de graduación colegio' ).
      lr_column->set_medium_text( 'Año Grados colegio' ).
      lr_column->set_short_text( 'Año Grad.C' ).
      lr_column->set_output_length( 4 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'OPORTUNIDAD' ).
      lr_column->set_long_text( 'Presentado en oportunidad anterior' ).
      lr_column->set_medium_text( 'Pres.Oport. anterior' ).
      lr_column->set_short_text( 'Pres.Op.An' ).
      lr_column->set_output_length( 1 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'VECES_ASPIRA' ).
      lr_column->set_long_text( 'Número de veces que aspira a esta Beca' ).
      lr_column->set_medium_text( 'N. Veces aspira Beca' ).
      lr_column->set_short_text( 'Veces Asp.' ).
      lr_column->set_output_length( 3 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PROGR_ACTUAL' ).
      lr_column->set_long_text( 'Programa Actual' ).
      lr_column->set_medium_text( 'Programa Actual' ).
      lr_column->set_short_text( 'Prog.Act.' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'INGRESO_HOG' ).
      lr_column->set_long_text( 'Ingreso promedio hogar' ).
      lr_column->set_medium_text( 'Ingreso prome hogar' ).
      lr_column->set_short_text( 'IngP.Hogar' ).
      lr_column->set_output_length( 10 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'ESTUDIOS_ADIC' ).
      lr_column->set_long_text( 'Estudios Adicionales' ).
      lr_column->set_medium_text( 'Estudios Adicionales' ).
      lr_column->set_short_text( 'Estud.Adi.' ).
      lr_column->set_output_length( 250 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'GRUPO_FAMILI' ).
      lr_column->set_long_text( 'Grupo Familiar' ).
      lr_column->set_medium_text( 'Grupo Familiar' ).
      lr_column->set_short_text( 'Gr.Famil.' ).
      lr_column->set_output_length( 250 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.
*Fin M7454 - HRESTREPO - 31/08/2018

*-- ALTASER LTAFUR 07-09-2021 Inicio
  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'CAMBIO_PLAN' ).
      lr_column->set_long_text( 'Cambio Plan' ).
      lr_column->set_medium_text( 'Cambio Plan' ).
      lr_column->set_short_text( 'C.Plan.' ).
      lr_column->set_output_length( 1 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'PROG_ANT' ).
      lr_column->set_long_text( 'Plan Anterior' ).
      lr_column->set_medium_text( 'Plan Anterior' ).
      lr_column->set_short_text( 'Plan Ant.' ).
      lr_column->set_output_length( 20 ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'CONTADOR' ).
      lr_column->set_long_text( 'Número' ).
      lr_column->set_medium_text( 'Número' ).
      lr_column->set_short_text( 'Nro.' ).
    catch cx_salv_not_found.
  endtry.
*-- ALTASER LTAFUR 07-09-2021 Inicio

  if rb_pre eq 'X'.
    clear lr_column.
    try.
        lr_column ?= pr_columns->get_column( 'ST' ).
        lr_column->set_visible( abap_false ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    clear lr_column.
    try.
        lr_column ?= pr_columns->get_column( 'MATRICULA' ).
        lr_column->set_visible( abap_false ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    clear lr_column.
    try.
        lr_column ?= pr_columns->get_column( 'PARTNER' ).
        lr_column->set_visible( abap_false ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    clear lr_column.
    try.
        lr_column ?= pr_columns->get_column( 'STAT' ).
        lr_column->set_visible( abap_false ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.

    clear lr_column.
    try.
        lr_column ?= pr_columns->get_column( 'STAT2' ).
        lr_column->set_visible( abap_false ).
      catch cx_salv_not_found.                          "#EC NO_HANDLER
    endtry.
  endif.

  clear lr_column.
  try.
      lr_column ?= pr_columns->get_column( 'MANDT' ).
      lr_column->set_visible( abap_false ).
    catch cx_salv_not_found.                            "#EC NO_HANDLER
  endtry.

  try.
*-- Color por cada celda
      cl_columns ?= go_alv->get_columns( ).
      cl_columns->set_color_column( value = 'IT_COLORS' ).

    catch cx_salv_not_found.
  endtry.


endform.                    " set_columns

*&---------------------------------------------------------------------*
*&      Form  CONVERT_PERYR_TO_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_PERYR  text
*      <--P_R_FECHA  text
*----------------------------------------------------------------------*
form convert_peryr_to_date  tables  p_s_peryr
                                    p_s_perid
                                    p_r_fecha
                                    p_r_period.
  types: begin of ty_fechas,
           sign   type tvarv_sign,
           option type tvarv_opti,
           low    type dats,
           high   type dats,
         end of ty_fechas.

  "  data: lt_fechas type table of ty_fechas,
  "        lr_fechas type ty_fechas.

  data: lt_period     type table of rsis_s_range,               "Table periodos zetas admisiones
        lr_period     type rsis_s_range,                        "Rec. periodos zetas admisiones
        lt_fechas     type table of rsis_s_range,               "Table fechas
        lr_fechas     type rsis_s_range,                        "Rec. fechas
        lt_timelimits type table of piq_academic_calendar,  "Table time limits
        lt_mensaje    type table of piq_error_structure.    "Tabla de mensajes

  constants:
    lc_calces   type piqcaobjid   value '50000075',    "Calendario principal CES
    lc_time_100 type piqtimelimit value '0100'.        "Time limit 0100

  select * into table @data(lt_anos)
    from t7piqyear
    where peryr in @p_s_peryr.

  select * into table @data(lt_periodos)
    from t7piqperiod
    where perid in @p_s_perid.

  lr_fechas-option = 'BT'.
  lr_fechas-sign   = 'I0'.
  lr_period-option = 'EQ'.
  lr_period-sign   = 'I'.

  loop at lt_anos into data(lr_anos).
    loop at lt_periodos into data(lr_periodos).
      call function 'HRIQ_READ_TIMELIMITS_CA'
        exporting
          iv_peryr      = lr_anos-peryr
          iv_perid      = lr_periodos-perid
          iv_objid_ca   = lc_calces
        importing
          et_timelimits = lt_timelimits
          et_log_tab    = lt_mensaje.

      read table lt_timelimits into data(lr_timelimits)
         with key timelimit = lc_time_100.
      check sy-subrc = 0.
      lr_fechas-low  = lr_timelimits-begda. "Fecha inicial
      lr_fechas-high = lr_timelimits-endda. "Fecha final
      append lr_fechas to lt_fechas.

      lr_period-low = lr_anos-peryr && lr_periodos-perid.
      append lr_period to lt_period.
    endloop.
  endloop.

  p_r_fecha[]  = lt_fechas[].
  p_r_period[] = lt_period[].
endform.


*&---------------------------------------------------------------------*
*&      Form  CONSULTAR_PERIODO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_PERIOD_HIGH  text
*----------------------------------------------------------------------*
form consultar_periodo tables p_s_fechas p_s_period.

  data: lt_field type table of dfies,   "Interfase Dict: Campos de tabla para DDIF_FIELDINFO_GET
        lt_mark  type ddshmarks,         "Lista de líneas marcadas
        lr_mark  type line of ddshmarks, "Variable marca
        lv_value type shvalue_d.        "value

*& ALTASER LAT 12-08-2021 Inicio nuevos parámetros fechas y periodos de inscripción.
  types: begin of ty_fecha,
           sign   type tvarv_sign,
           option type tvarv_opti,
           low    type date,
           high   type date,
         end of ty_fecha,

         begin of ty_perid_so,
           sign   type tvarv_sign,
           option type tvarv_opti,
           low    type zedu_perioinsc,
           high   type zedu_perioinsc,
         end of ty_perid_so,

         begin of ty_periodos,
           periodo_acad type zedu_perioinsc,  "Año+periodo en entrevistas
           perit        type piqperit,        "Texto periodo
         end of ty_periodos.

  data: lt_fecha    type table of ty_fecha,    "Table fechas
        lt_periodos type table of ty_periodos, "Tabla periodos a ser escogidos
        lr_periodos type ty_periodos,          "Rec. periodos
        lt_retor    type table of ddshretval,  "Tabla retorno de ayuda
        lt_perid_so type table of ty_perid_so, "Tabla periodos auxiliares SO.
        lr_perid_so type ty_perid_so.          "Rec. periodos auxiliares SO.

  "Años
  select * from t7piqyearprd into table @data(lt_peryr)
  where peryr <> @space.

  select * from t7piqperiodt into table @data(lt_perid)
      where perid <> @space and
            spras = @sy-langu.

  lt_fecha[] = p_s_fechas[].
  read table lt_fecha into data(r_fechas) index 1.
  if sy-subrc = 0.
    "Llenar tabla periodos (año+periodo) a partir del año de la fecha
    loop at lt_peryr into data(lr_peryr) where peryr = r_fechas-low(4).
      read table lt_perid into data(lr_perid) with key perid = lr_peryr-perid.
      if sy-subrc = 0.
        lr_periodos-perit = lr_perid-perit.
      else.
        lr_periodos-perit = ''.
      endif.
      lr_periodos-periodo_acad = lr_peryr-peryr && lr_peryr-perid.
      append lr_periodos to lt_periodos.
    endloop.
  else.
    "Se consultan todos los años y periodos para consulta en la tabla
    loop at lt_peryr into lr_peryr.
      read table lt_perid into lr_perid with key perid = lr_peryr-perid.
      if sy-subrc = 0.
        lr_periodos-perit = lr_perid-perit.
      else.
        lr_periodos-perit = ''.
      endif.
      lr_periodos-periodo_acad = lr_peryr-peryr && lr_peryr-perid.
      append lr_periodos to lt_periodos.
    endloop.
  endif.

  refresh: lt_retor.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'PERIODO_ACAD'
      dynpprog        = sy-repid
      window_title    = 'Periodos en la inscripción'
      value_org       = 'S'
      multiple_choice = 'X'
      mark_tab        = lt_mark
    tables
      field_tab       = lt_field
      value_tab       = lt_periodos[]
      return_tab      = lt_retor
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  loop at lt_retor into  data(lr_retor).
    read table lt_periodos into lr_periodos
        with key periodo_acad = lr_retor-fieldval.
    if sy-subrc eq 0.
      lr_mark = sy-tabix.
      append lr_mark to lt_mark.
      clear  lr_mark.
    endif.

    lv_value = lr_retor-fieldval.
    at first.
      lr_perid_so-low    = lv_value.
      lr_perid_so-sign   = 'I'.
      lr_perid_so-option = 'EQ'.
      append lr_perid_so to lt_perid_so.
      clear lr_perid_so.
    endat.
    lr_perid_so-low    = lr_retor-fieldval.
    lr_perid_so-sign   = 'I'.
    lr_perid_so-option = 'EQ'.
    append lr_perid_so to lt_perid_so.
    clear lr_perid_so.
  endloop.

  if lines( lt_perid_so ) > 0.
    p_s_period[] = lt_perid_so[].
    call function 'SAPGUI_SET_FUNCTIONCODE'.
  endif.

*& ALTASER LAT 12-08-2021 Final nuevos parámetros fechas y periodos de inscripción.
endform.

*&---------------------------------------------------------------------*
*&      Form  COMPLETAR_ST_EXPEDIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_DATA  text
*----------------------------------------------------------------------*
form completar_st_expediente tables p_lt_planes   structure hrp1001    "CS-SC
                                    p_lt_hrp1001  structure hrp1001    "ST-CS
                                    p_lt_land     structure t005t      "Paises
                              using p_lr_cmacbpst structure cmacbpst   "cmacbpst
                                    p_lr_but000   type      gty_but000 "BP central
                                    p_lr_but0id   type      but0id     "IC: Números de identificación
                           changing p_ls_data     type gty_data
                                    p_flag_error  type c.

  data: lv_centraldata        type bapibus1006_central,        "Rec. central BP
        lv_centraldata_person type bapibus1006_central_person, "Rec. central person BP
        lt_teladdress         type table of bapiadtel,         "Tabla teléfono - direcciones
        lt_mailaddress        type table of bapiadsmtp,        "Tabla Dirección e-mail
        lt_return             type table of bapiret2,          "Tabla return
        lv_categadm_ok        type c.                          "Bandera categ. admisión

  constants:
    lc_nuevo_expediente type piqadm_categt value 'ST Creado directamente en SAP'.

  read table p_lt_hrp1001 into data(lr_hrp1000_stcs)
     with key objid = p_lr_cmacbpst-stobjid.

  "Categoría de admisión.
  read table gt_pad530 into gr_pad530
    with key adatanr   = lr_hrp1000_stcs-adatanr.
  if sy-subrc <> 0. "Fuera de periodos buscados
    p_flag_error = cl_hrpiq00const=>c_checked.
    exit.
  endif.

  p_ls_data-periodo = gr_pad530-adm_ayear && gr_pad530-adm_perid.

  if sy-subrc = 0.
    p_ls_data-tipo_aspirante = gr_pad530-adm_categ.      "Cod. categ. admisión
    read table gt_adm_categt into data(lr_adm_categt)
      with key adm_categ = gr_pad530-adm_categ
      binary search.
    if sy-subrc eq 0.
      p_ls_data-t_asp_desc     = lr_adm_categt-adm_categt. "Desc. categ. admisión.
    endif.
  endif.


  "Read objid SC
  read table p_lt_planes into data(lr_planes)
     with key objid = lr_hrp1000_stcs-sobid.

  check sy-subrc = 0.

  "Descripción SC
  read table gt_prog assigning field-symbol(<fs_prog>)
    with key objid = lr_planes-sobid.
  if sy-subrc eq 0.
    p_ls_data-prog = <fs_prog>-stext. "SC creado directamente en expediente
  endif.

  p_ls_data-desc_medio = lc_nuevo_expediente.

  "Estatus plan estudio
  read table gt_stat into gs_stat
    with key domvalue_l = lr_hrp1000_stcs-istat.
  if sy-subrc eq 0.
    p_ls_data-stat = gs_stat-ddtext.
  endif.

  "Select IC: Direcciones
  select single * into @data(lr_but020)
    from but020
    where partner =  @p_lr_cmacbpst-partner.

  "Select Direcciones (gestión central de direcciones)
  select single * into @data(lr_adrc)
    from adrc
    where addrnumber = @lr_but020-addrnumber.

  "Select Telèfonos
  select * into table @data(lt_tel)
    from adr2
    where  addrnumber = @lr_but020-addrnumber.

  "Select e-mail
  select single * into @data(lr_mail)
    from adr6
    where  addrnumber = @lr_but020-addrnumber.

  if p_lr_but000-xsexm = cl_hrpiq00const=>c_checked.
    p_ls_data-genero = 'M'.
  else.
    p_ls_data-genero = 'F'.
  endif.

  p_ls_data-ciudad_proc   = lr_adrc-city1.     "Ciudad
  p_ls_data-sdo_direccion = lr_adrc-street.    "Dirección
  p_ls_data-email         = lr_mail-smtp_addr. "E-mail

  "Teléfonos
  loop at lt_tel into data(lr_tel).
    if lr_tel-consnumber = '001'.
      p_ls_data-tel_movil = lr_tel-tel_number.
    elseif lr_tel-consnumber = '002'.
      p_ls_data-tel_fijo  = lr_tel-tel_number.
    endif.
  endloop.

  read table gt_land into gs_land
    with key land1 = p_lr_but0id-country.
  if sy-subrc eq 0.
    p_ls_data-pais = gs_land-landx.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  MOVER_CAMPOS_ZADMI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_lr_pre  text
*      -->P_LS_DATA  text
*----------------------------------------------------------------------*
form mover_campos_zadmi  using    p_zpre    type gty_zpre_adm
                                  p_ls_data type gty_data.
  "         nro_documen
  "         partner
  "         matricula
  "         st
  p_ls_data-nr_formulario = p_zpre-nr_formulario.
  "         nombre
  "         prog
  "         stat
  "         prog2
  "         stat2
  p_ls_data-periodo = p_zpre-periodo.
  p_ls_data-sdo_direccion = p_zpre-sdo_direccion.
  p_ls_data-tel_movil = p_zpre-tel_movil.
  p_ls_data-tel_fijo = p_zpre-tel_fijo.
  p_ls_data-email = p_zpre-email.
  "         p_ls_data-pin = p_zpre-p.
  p_ls_data-genero = p_zpre-genero.
  "         p_ls_data-pais = p_zpre-pais.
  p_ls_data-ciudad_proc = p_zpre-ciudad_proc.
  p_ls_data-universidad = p_zpre-universidad.
  p_ls_data-otra_uni = p_zpre-otra_uni.
  p_ls_data-cod_medio = p_zpre-cod_medio.
  "         p_ls_data-desc_medio = p_zpre-desc_medio.
  p_ls_data-snp = p_zpre-snp.
  p_ls_data-tipo_aspirante = p_zpre-tipo_aspirante.
  "         p_ls_data-t_asp_desc = p_zpre-t_asp_desc.
  p_ls_data-ano = p_zpre-ano.
  p_ls_data-estudios_adic = p_zpre-estudios_adic.
  p_ls_data-oportunidad = p_zpre-oportunidad.
  p_ls_data-veces_aspira = p_zpre-veces_aspira.
  p_ls_data-progr_actual = p_zpre-progr_actual.
  p_ls_data-grupo_famili = p_zpre-grupo_famili.
  p_ls_data-ingreso_hog = p_zpre-ingreso_hog.
  p_ls_data-colegio = p_zpre-colegio.
  p_ls_data-otro_colegio = p_zpre-otro_colegio.
  "         cambio_plan
  "         prog_ant
endform.

*&---------------------------------------------------------------------*
*&      Form  ARMAR_PERIODOZETA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form armar_periodozeta .
  "Años y periodos reales
  select * from t7piqyearprd into table @data(lt_peryr)
    where peryr in @s_peryr and
          perid in @s_perid.

  refresh s_period.
  s_period-sign   = 'I'.
  s_period-option = 'EQ'.
  loop at lt_peryr into data(lr_peryr).
    s_period-low = lr_peryr-peryr && lr_peryr-perid.
    append s_period.
  endloop.

  "  select * from t7piqperiodt into table @data(lt_perid)
  "    where perid in sperid
endform.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_ST_TO_OBJID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SO_EST  text
*      -->P_R_OBJIDST  text
*----------------------------------------------------------------------*
form convert_st_to_objid  tables   p_so_est structure so_est
                                   p_r_objidst structure r_objidst.

  types: begin of ty_objidst,
           sign   type tvarv_sign,
           option type tvarv_opti,
           low    type hrobjid,
           high   type hrobjid,
         end of ty_objidst.

  data: lr_objidst type ty_objidst. "Record Objid St

  constants:
    lc_type_fs0001 type  bu_id_type value 'FS0001'. "Tipo de identificación cédula

  select * from but0id into table @data(lt_but0id)
    where idnumber in @p_so_est.

  if sy-subrc <> 0.
    message id gc_men_inscritos type 'S' number 001
            with '(Identificación estudiantes)'.
    exit.
  endif.

  select * from cmacbpst into table @data(lt_cmacbpst)
    for all entries in @lt_but0id
    where partner = @lt_but0id-partner.

  if sy-subrc <> 0.
    "Mensaje de error
  endif.

  lr_objidst-sign   = 'I'.
  lr_objidst-option = 'EQ'.
  loop at lt_cmacbpst into data(lr_cmacbpst).
    lr_objidst-low = lr_cmacbpst-stobjid.
    append lr_objidst to p_r_objidst.
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  MOVER_RANGOS_TO_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mover_rangos_to_gen.
  data lr_gen like line of gt_gen_fechas.

  loop at s_fechas into data(lr_fechas).
    move-corresponding lr_fechas to lr_gen.
    append lr_gen to gt_gen_fechas.
  endloop.

  loop at s_period into data(lr_period).
    move-corresponding lr_period to lr_gen.
    append lr_gen to gt_gen_period.
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  MOVER_GEN_TO_RANGOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mover_gen_to_rangos.
  data: lr_gen    type rsis_s_range,
        lr_fechas like line of s_fechas,
        lr_period like line of s_period.

  refresh s_fechas.
  loop at gt_gen_fechas into lr_gen.
    move-corresponding lr_gen to lr_fechas.
    append lr_fechas to s_fechas.
  endloop.

  refresh s_period.
  loop at gt_gen_period into lr_gen.
    move-corresponding lr_gen to lr_period.
    append lr_period to s_period.
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  CABECERA_ALV1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cabecera_alv1 .
  data: lcl_grid  type ref to cl_salv_form_layout_grid,
        lcl_gridx type ref to cl_salv_form_layout_grid,
        lcl_flow  type ref to cl_salv_form_layout_flow,
        lcl_pict  type ref to cl_salv_form_picture,
        lcl_logo  type ref to cl_salv_form_layout_logo,
        lcl_label type ref to cl_salv_form_label,
        lcl_text  type ref to cl_salv_form_text.

  data: wf_name(20),
        wf_apellido(20),
        w_nombre(41),
        lv_titulo(60).


*&---------------------------------------------------------*
*       Recuperar Usuario que Ejecuto
*----------------------------------------------------------*

  select single t1~name_first t1~name_last
    into (wf_name, wf_apellido)
    from adrp as t1
   inner join usr21 as t2
      on t1~persnumber = t2~persnumber
   where t2~bname = sy-uname.

  if sy-subrc eq 0.
    concatenate wf_name wf_apellido into
    w_nombre separated by space.
  endif.

  create object lcl_grid.

  if rb_ins = cl_hrpiq00const=>c_checked.
    lv_titulo = 'Reporte de inscritos'.
  else.
    lv_titulo = 'Reporte de preinscritos'.
  endif.

  "  lv_titulo =  gr_zt001-name. "Nombre de la Institución
  "  lcl_grid->create_header_information( row = 1  column = 1 text = lv_titulo ).
  "lv_titulo =  gc_titulo_alv1. "Título enfermería
  lcl_grid->create_header_information( row = 2  column = 1 text = lv_titulo ).
  lcl_gridx = lcl_grid->create_grid( row = 3  column = 1  ).
  lcl_label = lcl_gridx->create_label( row = 3 column = 1  text = 'Programa:' ).
  lcl_text  = lcl_gridx->create_text(  row = 3 column = 4  text = sy-repid ).
  lcl_label = lcl_gridx->create_label( row = 4 column = 1  text = 'Fecha:' ).
  lcl_text  = lcl_gridx->create_text(  row = 4 column = 4  text = sy-datum ).
  lcl_label = lcl_gridx->create_label( row = 5 column = 1  text = 'Usuario:' ).
  lcl_text  = lcl_gridx->create_text(  row = 5 column = 4  text = w_nombre ).

* Create logo layout, set grid content on left and logo image on right
  create object lcl_logo.
  lcl_logo->set_left_content( lcl_grid ).
  lcl_logo->set_right_logo( 'ZLOGO_CES_OFICIAL' ). "Logo

* Set the element top_of_list
  go_alv->set_top_of_list( lcl_logo ).

endform.

*&---------------------------------------------------------------------*
*&      Form  RANGO_OPCIONES_SC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GR_OPT_SC  text
*      -->P_0907   text
*----------------------------------------------------------------------*
form rango_opciones_sc  tables   pr_opt_sc structure rsis_s_range
                        using    p_choice.
  data: lt_opt type table of  rsis_s_range, "Tabla de programas 1era y sgda opción
        lr_opt like line of lt_opt.         "REc. !era y sgda opción

  lt_opt[] = pr_opt_sc[].
  lr_opt-sign   = 'I'.
  lr_opt-option = 'EQ'.
  lr_opt-low    =  p_choice.
  append lr_opt to lt_opt.
  pr_opt_sc[] = lt_opt[].
endform.
