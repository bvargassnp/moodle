*&---------------------------------------------------------------------*
*&  Include           ZDREDU_DIRECTORIO_INSCRI_TOP
*&---------------------------------------------------------------------*

TYPES:
  BEGIN OF tys_objid,
    objid        TYPE hrp1001-objid,
    sobid        TYPE hrp1001-sobid,
    otype        TYPE hrp1001-otype,
    sclas        TYPE hrp1001-sclas,
    rsign        TYPE hrp1001-rsign,
    relat        TYPE hrp1001-relat,
    subty        TYPE hrp1001-subty,
    adatnr       TYPE hrp1001-adatanr,
    istat        TYPE hrp1001-istat,
    adm_enrcateg TYPE hrpad530-adm_enrcateg,"Clase de Oyente
    adm_categ    TYPE hrpad530-adm_categ,
    choice_no    TYPE hrpad530-choice_no,   "Número de Opción (primera, segunda)
    adm_aclevel  TYPE hrpad530-adm_aclevel,
    adm_ayear    TYPE hrpad530-adm_ayear,
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
*    adm_perid    type hrpad530-adm_perid,
    adm_perid    TYPE t7piqperiodt-perit,
*	End	  -->	MgM DCEK903347
"Alter info: Sebastián Espinosa Marin - 05/07/2019 - Agregación de campos al reporte
    FechaInscripcion TYPE hrpad530-adm_recpt,
    EstadoEstParametro   TYPE stext, "Éste parámetro se llenará en f_filtrar_estudiantes
"Fin alter 05/07/2019
  END OF tys_objid,

  BEGIN OF tys_objid_aux,
    objid TYPE hrp1001-objid,
    sobid TYPE hrp1001-objid,
  END OF tys_objid_aux,

  BEGIN OF tys_swhactor_aux,
    otype TYPE hrp1001-otype,
    objid TYPE hrp1001-objid,
  END OF tys_swhactor_aux,

  BEGIN OF tys_f4_help,
    objid TYPE hrp1000-objid,
    short TYPE hrp1000-short,
    stext TYPE hrp1000-stext,
  END OF tys_f4_help,

  BEGIN OF tys_f4_help_period,
    peryr    TYPE t7piqyearprd-peryr,
    perid    TYPE t7piqyearprd-perid,
    persl    TYPE t7piqyearprd-persl,
    openfrom TYPE t7piqpkeyi-openfrom,
    opento   TYPE t7piqpkeyi-opento,
  END OF tys_f4_help_period,

  BEGIN OF tys_hrp1702,
    objid     TYPE hrp1702-objid,
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
    begda     TYPE begdatum,
    endda     TYPE enddatum,
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
    vorna     TYPE hrp1702-vorna,
    nachn     TYPE hrp1702-nachn,
    gbdat     TYPE hrp1702-gbdat,
    gbdep     TYPE bezei, "hrp1702-gbdep, "MgM DCEK903347
    gbort     TYPE hrp1702-gbort,
    gblnd     TYPE hrp1702-gblnd,
    gesch     TYPE hrp1702-gesch,
    dr_lic_no TYPE hrp1704-dr_lic_no,
"Alter info: Sebastián Espinosa Marin - 05/07/2019 - Agregación de campos al reporte
    NomPaisNacimiento TYPE bezei,
"Fin alter 05/07/2019
  END OF tys_hrp1702,

  BEGIN OF tys_hrp1721,
    objid      TYPE hrp1721-objid,
    testtotres TYPE hrp1721-testtotres,
  END OF tys_hrp1721,

  BEGIN OF tys_hrp9003,
    objid            TYPE hrp9003-objid,
    colegio          TYPE hrp9003-colegio,
    colegio_otro     TYPE hrp9003-otro_col,    "MgM DCEK903347
    titulo_c         TYPE hrp9003-titulo,      "MgM DCEK904012
    universidad      TYPE hrp9003-universidad,
    universidad_otra TYPE hrp9003-otra_uni,    "MgM DCEK903347
    titulo_u         TYPE hrp9003-titulo_u,
    saber_11         TYPE hrp9003-saber_11,  "MgM DCEK903347
"Alter info: Sebastián Espinosa Marin - 05/07/2019 - Agregación de campos al reporte
    Begda            TYPE begdatum,
    Endda            TYPE enddatum,
    AnnoTitulo       TYPE hrp9003-ano,
    CodPaisColegio   TYPE hrp9003-pais_proc,
    CodDptoColegio   TYPE hrp9003-dpto_proc,
    NomPaisColegio   TYPE bezei,
    NomDptoColegio   TYPE bezei,
    CiudadColegio    TYPE hrp9003-ciudad,
    CodPaisUniversidad TYPE hrp9003-pais_u,
    CodDptoUniversidad TYPE hrp9003-region_u,
    NomPaisUniversidad TYPE bezei,
    NomDptoUniversidad TYPE bezei,
    CiudadUniversidad TYPE hrp9003-ciudad_u,
"Fin alter 05/07/2019
  END OF tys_hrp9003,

*	Begin	-->	MgM DCEK903347 Descripción Colegio y univ 02/03/2017
  BEGIN OF ty_colegios,
    codigo         TYPE zedu_coleg-codigo,
    nombre_colegio TYPE zedu_coleg-nombre_colegio,
  END OF ty_colegios,

  BEGIN OF ty_universidad,
    codigo      TYPE zedu_univ-codigo,
    universidad TYPE zedu_univ-universidad,
  END OF ty_universidad,

  ty_t_colegios    TYPE STANDARD TABLE OF ty_colegios WITH KEY codigo,
  ty_t_universidad TYPE STANDARD TABLE OF ty_universidad WITH KEY codigo,
*	End	  -->	MgM DCEK903347

  BEGIN OF tys_partner_id,
    stobjid    TYPE cmacbpst-stobjid,
    partner    TYPE cmacbpst-partner,
    student12  TYPE cmacbpst-student12,
    idnumber   TYPE but0id-idnumber,
    type       TYPE tb039b-text,
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
**  Begin --> MgM DCEK904012 Procedencia 03/04/2017
*    pais_prcdnc    TYPE zedu_s_directorio_inscritos-pais_prcdnc,
*    depto_prcdnc   TYPE zedu_s_directorio_inscritos-depto_prcdnc,
*    ciudad_proc    TYPE zedu_s_directorio_inscritos-ciudad_proc,
*    primera_opcion TYPE zedu_s_directorio_inscritos-primera_opcion,
*    segunda_opcion TYPE zedu_s_directorio_inscritos-segunda_opcion,
**  End   --> MgM DCEK904012
    cant_inscr TYPE i,
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
  END OF tys_partner_id,

  BEGIN OF tys_partner_id_aux,
    stobjid TYPE hrp1001-objid,
    partner TYPE hrp1001-objid,
  END OF tys_partner_id_aux,

  BEGIN OF tys_programas,
    choice_no  TYPE t7piqchoicest-choice_no,
    spras      TYPE t7piqchoicest-spras,      "-->  MgM DCEK903347
    choice_not TYPE t7piqchoicest-choice_not,
  END OF tys_programas,

  BEGIN OF tys_zedu_prad,
    objid_st   TYPE zedu_pradm-objid_st,
    test_id    TYPE zedu_pradm-test_id,
    concepto   TYPE zedu_pradm-concepto,
    keyobs     TYPE zedu_pradm-keyobs,
    valoracion TYPE zedu_pradt-valoracion,
  END OF tys_zedu_prad,

  BEGIN OF tys_pers_addr_num,
    partner    TYPE but000-partner,
    persnumber TYPE but000-persnumber,
    addrnumber TYPE but020-addrnumber,
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
    name_first TYPE but000-name_first,
    namemiddle TYPE but000-namemiddle,
    name_last  TYPE but000-name_last,
    name_lst2  TYPE but000-name_lst2,
*	End	  -->	MgM DCEK903347
  END OF tys_pers_addr_num,

  BEGIN OF tys_direcciones,
    idnumber TYPE but0id-idnumber,
    pernr    TYPE pa0006-pernr,
    state    TYPE pa0006-state,
    ort01    TYPE pa0006-ort01,
    land1    TYPE pa0006-land1,
    stras    TYPE pa0006-stras,
  END OF tys_direcciones,

  BEGIN OF tys_procesos_ant,
    tipo_documen    TYPE zpre_admi_1-tipo_documen,
    nro_documen     TYPE zpre_admi_1-nro_documen,
    nr_formulario   TYPE zpre_admi_1-nr_formulario,
*    sta1_solpago  type zpre_admi_2-sta1_solpago,
    sta2_solpago    TYPE zpre_admi_2-sta2_solpago,
*	Begin	-->	MgM DCEK904012 Procedencia 03/04/2017
    pais_prcdnc     TYPE zpre_admi_1-pais_prcdnc,
    depto_prcdnc    TYPE zpre_admi_1-depto_prcdnc,
    ciudad_proc     TYPE zpre_admi_3-ciudad_proc,
    programa_1      TYPE zpre_admi_2-programa_1,
    primera_opcion  TYPE zedu_s_directorio_inscritos-primera_opcion,
    programa_2      TYPE zpre_admi_2-programa_2,
    segunda_opcion  TYPE zedu_s_directorio_inscritos-segunda_opcion,
*	End	  -->	MgM DCEK904012

"Alter info: Sebastián Espinosa Marin
"Fecha: 09/07/2019
"Incluir campo EPS, Facultad y Tipo de Programa
    EPS             TYPE zpre_admi_1-EPS,
    Facultad        TYPE HRP1000-MC_STEXT,
    TipoPrograma    TYPE HRP1000-MC_STEXT,
    Medio           TYPE zpre_admi_2-Medio,
    Pin             TYPE ZEDU_PIN,
* Inicio Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
*    cant_inscr     TYPE i,
    pais_prcdnc_tx  TYPE zedu_s_directorio_inscritos-pais_prcdnc,
    depto_prcdnc_tx TYPE zedu_s_directorio_inscritos-depto_prcdnc,
    ciudad_proc_tx  TYPE zedu_s_directorio_inscritos-ciudad_proc,
* Fin Modificado - 10/05/2017 - HRESTREPO - Ajustes Directorio Inscritos
    PeriodoAcademico TYPE zpre_admi_2-Periodo_acad,
  END OF tys_procesos_ant,

  BEGIN OF tys_entrevista,
    evalobj_id     TYPE piqdbcmprrecords-evalobj_id,
    objid          TYPE piqdbcmprrecords-objid,
    status2        TYPE piqdbcmprrecords-status2,
    examdate       TYPE piqevalobjinst-examdate,
    exambegtime    TYPE piqevalobjinst-exambegtime,
    location_objid TYPE piqevalobjinst-location_objid,
    concepto       TYPE zedu_pradm-concepto,
    valoracion     TYPE zedu_pradt-valoracion,
  END OF tys_entrevista,

  BEGIN OF tys_niveles,
    aclevel  TYPE t7piqlevelt-aclevel,
    aclevelt TYPE t7piqlevelt-aclevelt,
  END OF tys_niveles,

  BEGIN OF tys_categorias,
    enrcateg  TYPE t7piqenrcategt-enrcateg,
    enrcategt TYPE t7piqenrcategt-enrcategt,
  END OF tys_categorias,

  BEGIN OF tys_tipos,
    adm_categ  TYPE t7piqadmcategt-adm_categ,
    adm_categt TYPE t7piqadmcategt-adm_categt,
  END OF tys_tipos,

  BEGIN OF tys_t7piqyear,
    peryr TYPE t7piqyear-peryr,
  END OF tys_t7piqyear,

  BEGIN OF tys_t7piqyearprd,
    perid TYPE t7piqyearprd-perid,
  END OF tys_t7piqyearprd,

  BEGIN OF tys_hrp1028,
    objid TYPE hrp1028-objid,
    ort01 TYPE hrp1028-ort01,
  END OF tys_hrp1028,

  BEGIN OF tys_range_adm_cat,
    sign(1)   TYPE c,
    option(2) TYPE c,
    low       TYPE piqenrcateg,
    high      TYPE piqenrcateg,
  END OF tys_range_adm_cat,

  BEGIN OF tys_range_istat,
    sign(1)   TYPE c,
    option(2) TYPE c,
    low       TYPE istat_d,
    high      TYPE istat_d,
  END OF tys_range_istat,

  BEGIN OF estructuraEPS,
    CodEPS(4)  TYPE c,
    NombreEPS(100)  TYPE c,
  END OF estructuraEPS,

  BEGIN OF estructuraMedio,
    CodMedio(2) TYPE c,
    NombreMedio(50) TYPE c,
  END OF estructuraMedio,

  tyt_objid         TYPE STANDARD TABLE OF tys_objid,
  tyt_hrp1702       TYPE STANDARD TABLE OF tys_hrp1702,
  tyt_hrp1721       TYPE STANDARD TABLE OF tys_hrp1721,
  tyt_hrp9003       TYPE STANDARD TABLE OF tys_hrp9003,
  tyt_partner_id    TYPE STANDARD TABLE OF tys_partner_id,
  tyt_programas     TYPE STANDARD TABLE OF tys_programas,
  tyt_niveles       TYPE STANDARD TABLE OF tys_niveles,
  tyt_tipos         TYPE STANDARD TABLE OF tys_tipos,
  tyt_categorias    TYPE STANDARD TABLE OF tys_categorias,
  tyt_hrp1028       TYPE STANDARD TABLE OF tys_hrp1028,
  tyt_direcciones   TYPE STANDARD TABLE OF tys_direcciones,
  tyt_procesos_ant  TYPE STANDARD TABLE OF tys_procesos_ant,
  tyt_pers_addr_num TYPE STANDARD TABLE OF tys_pers_addr_num,
  tyt_entrevista    TYPE STANDARD TABLE OF tys_entrevista,
  tyt_t7piqyear     TYPE STANDARD TABLE OF tys_t7piqyear,
  tyt_t7piqyearprd  TYPE STANDARD TABLE OF tys_t7piqyearprd,
  tyt_zedu_prad     TYPE STANDARD TABLE OF tys_zedu_prad,
  tyt_objid_aux     TYPE STANDARD TABLE OF tys_objid_aux,
  tyt_swhactor_aux  TYPE STANDARD TABLE OF tys_swhactor_aux,
  tyt_range_adm_cat TYPE STANDARD TABLE OF tys_range_adm_cat,
  tyt_range_istat   TYPE STANDARD TABLE OF tys_range_istat,
  tablaEPS          TYPE STANDARD TABLE OF estructuraEPS,
  tablaMedio        TYPE STANDARD TABLE OF estructuraMedio.
*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
TYPES ty_ra_planes TYPE RANGE OF hrobjid.
TYPES ty_sra_planes TYPE LINE OF ty_ra_planes.

DATA gr_planes TYPE RANGE OF hrobjid.
*	End	  -->	MgM DCEK903347

CONSTANTS:
  c_coma(1)                     TYPE c VALUE ',',
  c_barra(1)                    TYPE c VALUE '/',
  c_dos_puntos(1)               TYPE c VALUE ':',
  c_1(1)                        TYPE c VALUE '1',
  c_2(1)                        TYPE c VALUE '2',
  c_o(1)                        TYPE c VALUE 'O',
  c_a(1)                        TYPE c VALUE 'A',
  c_b(1)                        TYPE c VALUE 'B',
  c_f(1)                        TYPE c VALUE 'F',
  c_s(1)                        TYPE c VALUE 'S',
  c_e(1)                        TYPE c VALUE 'E',
  c_x(1)                        TYPE c VALUE 'X',
  c_i(1)                        TYPE c VALUE 'I',
  c_p(1)                        TYPE c VALUE 'P',
  c_m(1)                        TYPE c VALUE 'M',
  c_01(2)                       TYPE c VALUE '01',
  c_02(2)                       TYPE c VALUE '02',
  c_05(2)                       TYPE c VALUE '05',
  c_06(2)                       TYPE c VALUE '06',
  c_07(2)                       TYPE c VALUE '07',
  c_08(2)                       TYPE c VALUE '08',
  c_cs(2)                       TYPE c VALUE 'CS',
  c_ce(2)                       TYPE c VALUE 'CE',
  c_sc(2)                       TYPE c VALUE 'SC',
  c_st(2)                       TYPE c VALUE 'ST',
  c_sm(2)                       TYPE c VALUE 'SM',
  c_eq(2)                       TYPE c VALUE 'EQ',
  c_en(2)                       TYPE c VALUE 'EN',
  c_ex(2)                       TYPE c VALUE 'EX',
  c_001(3)                      TYPE c VALUE '001',
  c_002(3)                      TYPE c VALUE '002',
  c_003(3)                      TYPE c VALUE '003',
  c_004(3)                      TYPE c VALUE '004',
  c_006(3)                      TYPE c VALUE '006',
  c_007(3)                      TYPE c VALUE '007',
  c_008(3)                      TYPE c VALUE '008',
  c_500(3)                      TYPE c VALUE '500',
  c_501(3)                      TYPE c VALUE '501',
  c_513(3)                      TYPE c VALUE '513',
  c_514(3)                      TYPE c VALUE '514',
  c_517(3)                      TYPE c VALUE '517',
  c_530(3)                      TYPE c VALUE '530',
  c_9000(4)                     TYPE c VALUE '9000',
  c_o-sc(4)                     TYPE c VALUE 'O-SC',
  c_b002(4)                     TYPE c VALUE 'B002',
  c_ra01(4)                     TYPE c VALUE 'RA01',
  c_sc-cs(5)                    TYPE c VALUE 'SC-CS',
  c_perid                       TYPE dfies-fieldname VALUE 'PERID',
  c_objid                       TYPE dfies-fieldname VALUE 'OBJID',
  c_zslcm_o(7)                  TYPE c VALUE 'ZSLCM_O',
  c_p_peryr(7)                  TYPE c VALUE 'P_PERYR',
  c_p_perid(7)                  TYPE c VALUE 'P_PERID',
  c_p_facul(7)                  TYPE c VALUE 'P_FACUL',
  c_p_tippro(8)                 TYPE c VALUE 'P_TIPPRO',
  c_p_planes(8)                 TYPE c VALUE 'P_PLANES',
  c_o-o_down(8)                 TYPE c VALUE 'O-O_DOWN',
  c_99991231(8)                 TYPE c VALUE '99991231',
  c_default_date_from           TYPE adrc-date_from VALUE '00010101',
  c_default_date_to             TYPE adrc-date_from VALUE '99991231',
  c_zedu_s_directorio_inscritos TYPE char30
                                VALUE 'ZEDU_S_DIRECTORIO_INSCRITOS'.

DATA: gt_output TYPE TABLE OF zedu_s_directorio_inscritos.

*	Begin	-->	MgM DCEK903347 Ajustes varios 07/02/2017
*DATA: wd_assist TYPE REF TO zwdc_pru_admi_ass.
DATA: wd_assist TYPE REF TO zcl_wd_lista_clases_ass .
*	End	  -->	MgM DCEK903347

DATA: gv_endda TYPE hrp1771-endda,
      gv_begda TYPE hrp1771-begda.
DATA gv_primer TYPE flag.

CREATE OBJECT wd_assist.
