*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_LISTADO_INSCRITOS_TOP
*&---------------------------------------------------------------------*

tables: zpre_admi_1,
        zpre_admi_2,
        t7piqyearprd, "Años académicos
        t7piqperiodt. "Periodos académicos

*----------------------------------------------------------------------*
*     S T R U C T U R E S                                              *
*----------------------------------------------------------------------*
data: begin of gst_data, "Estructura campos reporte
        nro_documen    type bu_id_number,
        partner        type bu_partner,
        matricula      type piqstudent12,
        st             type piqstudent,
        nr_formulario  type zpre_admi_1-nr_formulario,
        nombre         type char120,
        prog           type stext,
        stat           type char20,
        sc_mat         type char12,                    "Estado del plan con respecto a segmento o solo admitido
        prog2          type stext,
        stat2          type char20,
        periodo        type char7,
        sdo_direccion  type char45,
        tel_movil      type numc10,
        tel_fijo       type numc10,
        email          type text60,
        pin            type zedu_pin,
        genero         type zdtsd015,
        pais           type landx,
        ciudad_proc    type bezei20,
        universidad    type char70,
        otra_uni       type zotra_uni,
        cod_medio      type zpre_admi_2-medio,
        desc_medio     type text50,
        snp            type char30,
*Inicio M7454 - HRESTREPO - 31/08/2018
        tipo_aspirante type zpre_admi_2-tipo_aspirante,
        t_asp_desc     type piqadm_categt,
        ano            type zpre_admi_3-ano,
        estudios_adic  type zpre_admi_3-estudios_adic,
        oportunidad    type zpre_admi_3-oportunidad,
        veces_aspira   type zpre_admi_3-veces_aspira,
        progr_actual   type zpre_admi_3-progr_actual,
        grupo_famili   type zpre_admi_3-grupo_famili,
        ingreso_hog    type zpre_admi_3-ingreso_hog,
*Fin M7454 - HRESTREPO - 31/08/2018
        "Alter info: Inclusión de Campos Colegio y Otro Colegio
        "Fecha: 06/05/2021
        "Autor: Sebastián Espinosa
        colegio        type zpre_admi_3-colegio,
        otro_colegio   type zpre_admi_3-otro_col,
        "Fin Alter
*lnicio ALTASER LTAFUR-07-09-2021
        cambio_plan    type char1,    "Flag cambio de plan de estudio expediente vs zetas
        prog_ant       type char20,   "Programa anterior en tablas zetas vs expediente
        contador       type integer,  "Contador registros
*Fin ALTASER LTAFUR-07-09-2021
      end of gst_data.

types: begin of gty_data.
         include structure gst_data.
         "internal table for cell color information
         types : it_colors type lvc_t_scol,
       end of gty_data.

types: begin of gty_zpre_adm,
         nr_formulario  type zpre_admi_1-nr_formulario,
         tipo_documen   type bu_id_type, "zpre_admi_1-tipo_documen,
         nro_documen    type char060, "zpre_admi_1-nro_documen,
         sta1_solpago   type zpre_admi_2-sta1_solpago,
         sta2_solpago   type zpre_admi_2-sta2_solpago,
         programa_1     type zpre_admi_2-programa_1,
         programa_2     type zpre_admi_2-programa_2,
         periodo        type zpre_admi_2-periodo_acad,
         tipo_aspirante type zpre_admi_2-tipo_aspirante, "M7454 - HRESTREPO - 31/08/2018
         cod_medio      type zpre_admi_2-medio,
         sdo_direccion  type char45,
         tel_movil      type numc10,
         tel_fijo       type numc10,
         email          type text60,
         nombre         type text45,
         sdo_nombre     type text45,
         apellido       type text45,
         sdo_apellido   type text45,
         genero         type zdtsd015,
         pais_prcdnc    type land1,
         depto_prcdnc   type regio,
         ciudad_proc    type cityc,
         universidad    type char70,
         otra_uni       type zotra_uni,
         snp            type char30,
*Inicio M7454 - HRESTREPO - 31/08/2018
         ano            type zpre_admi_3-ano,
         estudios_adic  type zpre_admi_3-estudios_adic,
         oportunidad    type zpre_admi_3-oportunidad,
         veces_aspira   type zpre_admi_3-veces_aspira,
         progr_actual   type zpre_admi_3-progr_actual,
         grupo_famili   type zpre_admi_3-grupo_famili,
         ingreso_hog    type zpre_admi_3-ingreso_hog,
*Fin M7454 - HRESTREPO - 31/08/2018

         "Alter info: Inclusión de Campos Colegio y Otro Colegio
         "Fecha: 06/05/2021
         "Autor: Sebastián Espinosa
         colegio        type zpre_admi_3-colegio,
         otro_colegio   type zpre_admi_3-otro_col,
         "Fin Alter
         objid          type hrobjid,
         form           type numc10,
*          cant_inscr    TYPE i,
       end of gty_zpre_adm.

"Inicio M7454 - HRESTREPO - 31/08/2018
types: begin of gty_adm_categt,
         adm_categ  type piqadm_categ,
         adm_categt type piqadm_categt,
       end of gty_adm_categt.
"Fin M7454 - HRESTREPO - 31/08/2018

types: begin of gty_prog,
         objid type hrobjid,
         short type short_d,
         stext type stext,
       end of gty_prog.

types: begin of gty_but000,
         partner    type bu_partner,
         name_last  type bu_namep_l,
         name_first type bu_namep_f,
         name_lst2  type bu_namepl2,
         namemiddle type bu_namemid,
         xsexm      type bu_xsexm,
         xsexf      type bu_xsexf,
       end of gty_but000.

types: begin of ty_periodos, "Periodos zeta admisiones
         anoper type  zedu_perioinsc, "Año periodo en zeta de admisiones
       end of ty_periodos.

ranges: r_peryr      for hrpad530-adm_ayear,     "Año académico
        r_perid      for hrpad530-adm_perid,     "Periodo académico
        r_objidst    for hrp1001-objid.          "ID de los estudiantes

data: gt_gen_fechas type table of rsis_s_range, "Rango de fechas general
      gt_gen_period type table of rsis_s_range, "Rango de periodos zetas admisión general
      gr_opt_sc     type table of rsis_s_range. "Opciones primera y segunda para SC

data: gt_data        type standard table of gty_data,       "Tabla para el reporte
      gt_adm_categt  type standard table of gty_adm_categt, "M7454 - HRESTREPO - 31/08/2018
      gt_zpre_adm    type standard table of gty_zpre_adm,   "Tabla data admisiones zetas
      gr_colors      type line of gty_data-it_colors,     "Rec. color of gt_report
      gt_but0id      type standard table of but0id,
      gt_prog        type standard table of gty_prog,
      gt_but000      type standard table of gty_but000,
      gt_pin         type standard table of zslcm_pin,
      gt_cmacbpst    type standard table of cmacbpst,
      gt_hrp1001     type standard table of hrp1001,
      gt_hrp1001_aux type standard table of hrp1001,
      gt_hrp1001_opt type standard table of hrp1001,       "tabla para planes solo primera y segunda opción
      gt_planest     type standard table of hrp1001,       "Planes con segmentos
      gt_planest_adm type standard table of hrp1001,       "Planes completos segmentos o solo admitidos
      gt_planest_aux type standard table of hrp1001,
      gt_land        type standard table of t005t,
      gt_city        type standard table of t005h,
      gt_univ        type standard table of zedu_univ,
      gt_medios      type standard table of zedu_medios,
      gt_pad530      type table of hrpad530,               "Tabla Detalle link 530
      gr_pad530      type hrpad530,                        "Rec. Detalle link 530
      gt_hrp1769     type table of hrp1769,                "Tabla hrp1769 segment.
      gr_hrp1769     type hrp1769,                         "Rec. hrp1769 segmen
      gs_univ        type zedu_univ,
      gs_medio       type zedu_medios,
      gt_stat        type standard table of dd07v,
      gs_stat        type dd07v,
      gs_city        type t005h,
      gs_land        type t005t.

*---------------------------------------------------------------------*
*         Declaración de objetos
*---------------------------------------------------------------------*
data: go_alv     type ref to cl_salv_table,
      cl_columns type ref to cl_salv_columns_table.

*---------------------------------------------------------------------*
*         Declaración de constantes
*---------------------------------------------------------------------*
constants:
  gc_lina_edu_f    type zedu_linea_e value 'F',              "Línea educativa
  gc_matri_tod     type char3        value 'TOD',            "Todo tipo de inscripción sobre planes con o sin segmentos
  gc_matri_mat     type char3        value 'MAT',            "Inscripción de planes con matrículas - segmentos de estudio
  gc_matri_adm     type char3        value 'ADM',            "Inscripción de planes con solo admisiones.
  gc_matri_sin     type char3        value 'SIN',            "Inscripción no se han llevado a cabo (pendiente diligenciar formulario).
  gc_sc_segmento   type char12       value 'INSCRITO',       "Posee segmento el ST en este plan
  gc_sc_admitido   type char12       value 'ADMITIDO',       "Posee solo admisión el ST en este plan
  gc_sc_sinformu   type char12       value 'SIN_FORMULAR',   "Posee solo admisión el ST en este plan
  gc_men_inscritos type char20       value 'ZEDU_INSCRITOS'. "Clase de mensajes reporte de inscritos
