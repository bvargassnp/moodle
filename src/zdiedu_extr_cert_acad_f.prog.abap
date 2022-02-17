*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_EXTR_CERT_ACAD_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
FORM f_obtener_datos
  CHANGING
    pvc_opbel       TYPE opbel_kk
    ptc_certacad_h  TYPE gtyt_certacad_h
    ptc_certacad_pe TYPE gtyt_certacad_pe
    ptc_certacad_pm TYPE gtyt_certacad_pm
    ptc_certacad_sm TYPE gtyt_certacad_sm
    ptc_certacad_na TYPE gtyt_certacad_na
    ptc_certacad_ps TYPE gtyt_certacad_ps
    ptc_certacad_ro TYPE gtyt_certacad_ro
    ptc_certacad_gr TYPE gtyt_certacad_gr.

  "Declaraciones
  DATA:
    lr_blart_fact TYPE gtyt_r_blart,
    lr_blart_ma   TYPE gtyt_r_blart,
    lr_psobtyp_ma TYPE gtyt_r_psobtyp,
    lr_augrd_pdir TYPE gtyt_r_augrd,
    lr_augrd_pcom TYPE gtyt_r_augrd,
    lr_augrd_fact TYPE gtyt_r_augrd,
    lr_hvorg      TYPE gtyt_r_hvorg,
    lr_tvorg_esta TYPE gtyt_r_tvorg,
    lr_tvorg_real TYPE gtyt_r_tvorg,
    lr_tvorg_fact TYPE gtyt_r_tvorg,
    ls_ctrlcert   TYPE gty_ctrlcert,
    ls_cmacbpst   TYPE gty_cmacbpst,
    lt_but0id     TYPE gtyt_but0id,
    lt_hrp1769    TYPE gtyt_hrp1769,
    lt_cs_st      TYPE gtyt_hrp1001,
    lt_hrpad530   TYPE gtyt_hrpad530,
    lt_hrp1771    TYPE gtyt_hrp1771,
    lt_cs_sc      TYPE gtyt_hrp1001,
    lt_hrp1730    TYPE gtyt_hrp1730,
    lt_sc_o       TYPE gtyt_hrp1001,
    lt_o_otp      TYPE gtyt_hrp1001,
    lt_otp_of     TYPE gtyt_hrp1001,
    lt_pensum     TYPE gtyt_pensum,
    lt_hrp1735    TYPE gtyt_hrp1735,
    lt_hrp1759    TYPE gtyt_hrp1759,
    lt_hrt1759    TYPE gtyt_hrt1759,
    lt_hrp1732    TYPE gtyt_hrp1732,
    lt_st_sm      TYPE gtyt_hrp1001,
    lt_hrpad506   TYPE gtyt_hrpad506,
    lt_hrp1724    TYPE gtyt_hrp1724,
    lt_bp_sc_fica TYPE gtyt_bp_sc_fica,
    lt_fac_anual  TYPE gtyt_fac_anual,
    lt_yearprd    TYPE gtyt_yearprd,
    lt_dfkkop     TYPE gtyt_dfkkop,
    lt_hrp1000    TYPE gtyt_hrp1000,
    lt_tb039t     TYPE gtyt_tb039t,
    lt_timeunitt  TYPE gtyt_timeunitt,
    lt_smstatt    TYPE gtyt_smstatt,
    lt_reasont    TYPE gtyt_reasont,
    lt_processt   TYPE gtyt_processt.


  "Obtiene el customizing
  PERFORM f_obtener_customizing
    CHANGING
      lr_blart_fact
      lr_blart_ma
      lr_psobtyp_ma
      lr_augrd_pdir
      lr_augrd_pcom
      lr_augrd_fact
      lr_hvorg
      lr_tvorg_esta
      lr_tvorg_real
      lr_tvorg_fact
      lt_tb039t
      lt_timeunitt
      lt_smstatt
      lt_reasont
      lt_processt.

  "Obtiene los registros de control
  PERFORM f_obtener_ctrlcert
    CHANGING
      pvc_opbel
      ls_ctrlcert.

  "Obtiene la relacion ST - BP - Nro. Matricula
  PERFORM f_obtener_cmacbpst
    USING
      ls_ctrlcert
    CHANGING
      ls_cmacbpst.

  "Obtiene los datos de identificacion del estudiante
  PERFORM f_obtener_but0id
    USING
      ls_cmacbpst
    CHANGING
      lt_but0id.

  "Obtiene los datos de la matricula al estudio
  PERFORM f_obtener_hrp1769
    USING
      ls_ctrlcert
    CHANGING
      lt_hrp1769.

  "Obtiene las relaciones estudio - estudiante
  PERFORM f_obtener_cs_st
    USING
      ls_ctrlcert
    CHANGING
      lt_cs_st.

  "Obtiene los datos de la admision
  PERFORM f_obtener_hrpad530
    USING
      lt_cs_st
    CHANGING
      lt_hrpad530.

  "Obtiene las matriculas del estudiante
  PERFORM f_obtener_hrp1771
    USING
      ls_ctrlcert
    CHANGING
      lt_hrp1771.

  "Obtiene la relacion de periodos academicos con financieros
  PERFORM f_obtener_periodos_fica
    USING
      lt_hrp1771
    CHANGING
      lt_fac_anual
      lt_yearprd.

  "Obtiene las relaciones estudio - plan de estudio
  PERFORM f_obtener_cs_sc
    USING
      ls_ctrlcert
    CHANGING
      lt_cs_sc.

  "Obtiene los datos del plan de estudios
  PERFORM f_obtener_datos_sc
    USING
      lt_cs_sc
    CHANGING
      lt_hrp1730
      lt_sc_o
      lt_o_otp
      lt_otp_of
      lt_pensum
      lt_hrp1735
      lt_hrp1759
      lt_hrt1759
      lt_hrp1732.

  "Obtiene la relacion Estudiante - Asignaturas
  PERFORM f_obtener_relaciones
    USING
      cl_hrpiq00const=>c_relat_506a
      cl_hrpiq00const=>c_otype_sm
      lt_cs_st
    CHANGING
      lt_st_sm.

  "Obtiene los datos adicionales SM
  PERFORM f_obtener_hrpad506
    USING
      lt_st_sm
    CHANGING
      lt_hrpad506.

  "Obtiene los datos de las asignaturas matriculadas
  PERFORM f_obtener_hrp1724
    USING
      lt_cs_st
    CHANGING
      lt_hrp1724.

  "Obtiene las descripciones de los diversos objetos
  PERFORM f_obtener_hrp1000
    USING
      lt_cs_sc
      lt_o_otp
      lt_otp_of
      lt_pensum
      lt_st_sm
    CHANGING
      lt_hrp1000.

  "Agrupa los datos
  PERFORM f_agrupar_datos
    USING
      ls_ctrlcert
      ls_cmacbpst
      lt_but0id
      lt_hrp1769
      lt_cs_st
      lt_hrpad530
      lt_hrp1771
      lt_cs_sc
      lt_hrp1730
      lt_sc_o
      lt_o_otp
      lt_otp_of
      lt_pensum
      lt_hrp1735
      lt_st_sm
      lt_hrpad506
      lt_hrp1724
      lt_hrp1759
      lt_hrt1759
      lt_hrp1732
      lt_fac_anual
      lt_yearprd
      lt_hrp1000
      lt_tb039t
      lt_timeunitt
      lt_smstatt
      lt_reasont
      lt_processt
    CHANGING
      ptc_certacad_h
      ptc_certacad_pe
      ptc_certacad_pm
      ptc_certacad_sm
      ptc_certacad_na
      ptc_certacad_ps
      ptc_certacad_ro
      ptc_certacad_gr
      lt_bp_sc_fica.

  "Obtiene los valores de FICA
  PERFORM f_obtener_fica
    USING
      lr_blart_fact
      lr_blart_ma
      lr_psobtyp_ma
      lr_augrd_fact
      lr_hvorg
      lr_tvorg_fact
    CHANGING
      lt_bp_sc_fica
      lt_dfkkop.

  "Asigna los datos de FICA
  PERFORM f_asignar_fica
    USING
      ptc_certacad_h
      lr_blart_fact
      lr_augrd_pdir
      lr_augrd_pcom
      lr_tvorg_esta
      lr_tvorg_real
      lr_tvorg_fact
      lt_bp_sc_fica
      lt_dfkkop
    CHANGING
      ptc_certacad_pm.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CUSTOMIZING
*&---------------------------------------------------------------------*
FORM f_obtener_customizing
  CHANGING
    ptc_r_blart_fact TYPE gtyt_r_blart
    ptc_r_blart_ma   TYPE gtyt_r_blart
    ptc_r_psobtyp_ma TYPE gtyt_r_psobtyp
    ptc_r_augrd_pdir TYPE gtyt_r_augrd
    ptc_r_augrd_pcom TYPE gtyt_r_augrd
    ptc_r_augrd_fact TYPE gtyt_r_augrd
    ptc_r_hvorg      TYPE gtyt_r_hvorg
    ptc_r_tvorg_esta TYPE gtyt_r_tvorg
    ptc_r_tvorg_real TYPE gtyt_r_tvorg
    ptc_r_tvorg_fact TYPE gtyt_r_tvorg
    ptc_tb039t       TYPE gtyt_tb039t
    ptc_timeunitt    TYPE gtyt_timeunitt
    ptc_smstatt      TYPE gtyt_smstatt
    ptc_reasont      TYPE gtyt_reasont
    ptc_processt     TYPE gtyt_processt.

  "Declaraciones
  DATA:
    ls_r_blart   TYPE gty_r_blart,
    ls_r_psobtyp TYPE gty_r_psobtyp,
    ls_r_augrd   TYPE gty_r_augrd,
    ls_r_hvorg   TYPE gty_r_hvorg,
    ls_r_tvorg   TYPE gty_r_tvorg,
    lt_c_param   TYPE gtyt_c_param.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_c_param> TYPE gty_c_param.


  "Inicializa retornos
  CLEAR:
    ptc_r_blart_fact,
    ptc_r_blart_ma,
    ptc_r_psobtyp_ma,
    ptc_r_augrd_pdir,
    ptc_r_augrd_pcom,
    ptc_r_augrd_fact,
    ptc_r_hvorg,
    ptc_r_tvorg_esta,
    ptc_r_tvorg_real,
    ptc_r_tvorg_fact,
    ptc_tb039t,
    ptc_timeunitt,
    ptc_smstatt,
    ptc_reasont,
    ptc_processt.

  "Obtiene el customizing de la extraccion
  SELECT idparam idparampos valor
    INTO TABLE lt_c_param
    FROM zies_c_param
    WHERE repid = 'ZDRIES_EXTRACCION'.

  "Indica que los registros son incluyentes
  MOVE 'I' TO:
    ls_r_blart-sign,
    ls_r_psobtyp-sign,
    ls_r_augrd-sign,
    ls_r_hvorg-sign,
    ls_r_tvorg-sign.

  MOVE 'EQ' TO:
    ls_r_blart-option,
    ls_r_psobtyp-option,
    ls_r_augrd-option,
    ls_r_hvorg-option,
    ls_r_tvorg-option.

  "Recorre los registros de customizing
  LOOP AT lt_c_param ASSIGNING <fs_c_param>.
    "Determina el parametro a diligenciar
    CASE <fs_c_param>-idparam.
      WHEN 'BLART_FACT'.  "Clase de documento FICA - Facturacion
        "Asigna el valor y crea el registro en el rango
        ls_r_blart-low = <fs_c_param>-valor.
        APPEND ls_r_blart TO ptc_r_blart_fact.

      WHEN 'BLART_MA'.    "Clase de documento FICA - Matricula Academica
        "Asigna el valor y crea el registro en el rango
        ls_r_blart-low = <fs_c_param>-valor.
        APPEND ls_r_blart TO ptc_r_blart_ma.

      WHEN 'PSOBTYP_MA'.  "Clase objeto contrato FICA - Matricula Academica
        "Asigna el valor y crea el registro en el rango
        ls_r_psobtyp-low = <fs_c_param>-valor.
        APPEND ls_r_psobtyp TO ptc_r_psobtyp_ma.

      WHEN 'AUGRD_PDIR'.  "Motivo de compensacion FICA - Pago directo
        "Asigna el valor y crea el registro en el rango
        ls_r_augrd-low = <fs_c_param>-valor.
        APPEND ls_r_augrd TO ptc_r_augrd_pdir.

      WHEN 'AUGRD_PCOM'.  "Motivo de compensacion FICA - Pago por compensacion
        "Asigna el valor y crea el registro en el rango
        ls_r_augrd-low = <fs_c_param>-valor.
        APPEND ls_r_augrd TO ptc_r_augrd_pcom.

      WHEN 'AUGRD_FACT'.  "Motivo de compensacion FICA - Facturacion
        "Asigna el valor y crea el registro en el rango
        ls_r_augrd-low = <fs_c_param>-valor.
        APPEND ls_r_augrd TO ptc_r_augrd_fact.

      WHEN 'HVORG'.       "Operacion Principal FICA
        "Asigna el valor y crea el registro en el rango
        ls_r_hvorg-low = <fs_c_param>-valor.
        APPEND ls_r_hvorg TO ptc_r_hvorg.

      WHEN 'TVORG_ESTA'.  "Operacion Parcial FICA - Estadistica
        "Asigna el valor y crea el registro en el rango
        ls_r_tvorg-low = <fs_c_param>-valor.
        APPEND ls_r_tvorg TO ptc_r_tvorg_esta.
        "Crea el registro tambien para operacion parcial FICA - Facturacion
        APPEND ls_r_tvorg TO ptc_r_tvorg_fact.

      WHEN 'TVORG_REAL'.  "Operacion Parcial FICA - Real
        "Asigna el valor y crea el registro en el rango
        ls_r_tvorg-low = <fs_c_param>-valor.
        APPEND ls_r_tvorg TO ptc_r_tvorg_real.
        "Crea el registro tambien para operacion parcial FICA - Facturacion
        APPEND ls_r_tvorg TO ptc_r_tvorg_fact.
    ENDCASE.
  ENDLOOP.

  "Obtiene las descripciones de los tipos de documento
  "Haciendo uso parcial de la clave primaria
  SELECT category text
    INTO TABLE ptc_tb039t
    FROM tb039t
    WHERE langu EQ 'S'.

  "Obtiene las descripciones de las unidades
  "Haciendo uso parcial de la clave primaria
  SELECT timeunit timeunitt
    INTO TABLE ptc_timeunitt
    FROM t7piqtimeunitt
    WHERE langu EQ 'S'.

  "Obtiene las descripciones de los estados de asignatura
  "Haciendo uso parcial de la clave primaria
  SELECT smstatus smstatust
    INTO TABLE ptc_smstatt
    FROM t7piqsmstatt
    WHERE spras EQ 'S'.

  "Obtiene las descripciones de los motivos de excedencia
  "Haciendo uso parcial de la clave primaria
  SELECT reason reasontext
    INTO TABLE ptc_reasont
    FROM t7piqreasont
    WHERE spras EQ 'S'.

  "Obtiene las descripciones de las actividades
  "Haciendo uso parcial de la clave primaria
  SELECT process processt
    INTO TABLE ptc_processt
    FROM t7piqprocesst
    WHERE spras EQ 'S'.

  "Ordena los registros
  SORT ptc_r_blart_fact BY low.
  SORT ptc_r_blart_ma   BY low.
  SORT ptc_r_psobtyp_ma BY low.
  SORT ptc_r_augrd_pdir BY low.
  SORT ptc_r_augrd_pcom BY low.
  SORT ptc_r_augrd_fact BY low.
  SORT ptc_r_hvorg      BY low.
  SORT ptc_r_tvorg_esta BY low.
  SORT ptc_r_tvorg_real BY low.
  SORT ptc_r_tvorg_fact BY low.
  SORT ptc_tb039t       BY category.
  SORT ptc_timeunitt    BY timeunit.
  SORT ptc_smstatt      BY smstatus.
  SORT ptc_reasont      BY reason.
  SORT ptc_processt     BY process.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CTRLCERT
*&---------------------------------------------------------------------*
FORM f_obtener_ctrlcert
  CHANGING
    pvc_opbel     TYPE opbel_kk
    psc_ctrlcert  TYPE gty_ctrlcert.


  "Inicializa retorno
  CLEAR:
    pvc_opbel,
    psc_ctrlcert.

  "Obtiene los registros de control del certificado
  "Haciendo uso de la clave primaria de la tabla
  "NOTA: Se usa SELECT * ya que se requieren todos los campos y son pocos
  SELECT SINGLE *
    INTO psc_ctrlcert
    FROM zedu_ctrlcert
    WHERE opbel EQ pa_opbel.

  "Asigna el numero de documento
  pvc_opbel = psc_ctrlcert-opbel.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CMACBPST
*&---------------------------------------------------------------------*
FORM f_obtener_cmacbpst
  USING
    psi_ctrlcert  TYPE gty_ctrlcert
  CHANGING
    psc_cmacbpst  TYPE gty_cmacbpst.


  "Inicializa retorno
  CLEAR:
    psc_cmacbpst.

  "Continua solo si se tienen datos
  CHECK NOT psi_ctrlcert-estudiante IS INITIAL
        AND psi_ctrlcert-estudiante NE '00000000'.

  "Obtiene los datos del estudiante
  "Haciendo uso del indice estandar 002 - CLIENT / STOBJID
  SELECT SINGLE partner student12 stobjid
    INTO psc_cmacbpst
    FROM cmacbpst
    WHERE stobjid EQ psi_ctrlcert-estudiante.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_BUT0ID
*&---------------------------------------------------------------------*
FORM f_obtener_but0id
  USING
    psi_cmacbpst  TYPE gty_cmacbpst
  CHANGING
    ptc_but0id    TYPE gtyt_but0id.


  "Inicializa retorno
  CLEAR:
    ptc_but0id.

  "Continua solo si se tienen datos
  CHECK NOT psi_cmacbpst-partner IS INITIAL
        AND psi_cmacbpst-partner NE '0000000000'.

  "Obtiene los datos de identificacion
  SELECT partner type idnumber institute entry_date
         valid_date_from valid_date_to
    INTO TABLE ptc_but0id
    FROM but0id
    WHERE partner EQ psi_cmacbpst-partner.

  "Ordena los registros por interlocutor dejando el mas reciente de primero
  SORT ptc_but0id BY partner         ASCENDING
                     valid_date_to   DESCENDING
                     entry_date      DESCENDING
                     valid_date_from DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1769
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1769
  USING
    psi_ctrlcert  TYPE gty_ctrlcert
  CHANGING
    ptc_hrp1769   TYPE gtyt_hrp1769.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1769.

  "Continua solo si se tienen datos
  CHECK NOT psi_ctrlcert-programa IS INITIAL
        AND psi_ctrlcert-programa NE '00000000'.

  "Obtiene los datos del estudiante
  "Haciendo uso parcial de la clave primaria
  SELECT plvar otype objid subty istat begda endda varyf seqnr otjid
         beg_process beg_reason beg_key_date end_process end_reason end_key_date
    INTO TABLE ptc_hrp1769
    FROM hrp1769
    WHERE plvar EQ '01'
      AND otype EQ cl_hrpiq00const=>c_otype_cs
      AND objid EQ psi_ctrlcert-programa.

  "Ordena los registros por estudio
  SORT ptc_hrp1769 BY objid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CS_ST
*&---------------------------------------------------------------------*
FORM f_obtener_cs_st
  USING
    psi_ctrlcert  TYPE gty_ctrlcert
  CHANGING
    ptc_cs_st     TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lv_otjid TYPE otjid,
    lv_varyf TYPE varyf.


  "Inicializa retorno
  CLEAR:
    ptc_cs_st.

  "Continua solo si los datos son validos
  CHECK NOT psi_ctrlcert-programa   IS INITIAL
    AND NOT psi_ctrlcert-estudiante IS INITIAL
    AND psi_ctrlcert-programa   NE '00000000'
    AND psi_ctrlcert-estudiante NE '00000000'.

  "Asigna el estudio
  lv_otjid+0(2) = cl_hrpiq00const=>c_otype_cs.
  lv_otjid+2(8) = psi_ctrlcert-programa.
  "Asigna el estudiante
  lv_varyf+0(2) = cl_hrpiq00const=>c_otype_st.
  lv_varyf+2(8) = psi_ctrlcert-estudiante.

  "Obtiene los datos de la relacion Estudio - Estudiante
  "Haciendo uso parcial del indice estandar 6 - OTJID/SUBTY/VARYF/PLVAR/ENDDA
  SELECT otype objid plvar rsign relat istat priox begda
         endda varyf seqnr otjid subty sclas sobid adatanr
    INTO TABLE ptc_cs_st
    FROM hrp1001
    WHERE otjid EQ lv_otjid
      AND subty EQ cl_hrpiq00const=>c_relat_530b
      AND varyf EQ lv_varyf
      AND plvar EQ '01'
      AND istat EQ '1'.

  "Elimina los registros invalidos
  DELETE ptc_cs_st
    WHERE adatanr IS INITIAL.

  "Ordena los registros por estudio
  SORT ptc_cs_st BY objid ASCENDING sobid ASCENDING endda DESCENDING begda DESCENDING .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRPAD530
*&---------------------------------------------------------------------*
FORM f_obtener_hrpad530
  USING
    pti_cs_st     TYPE gtyt_hrp1001
  CHANGING
    ptc_hrpad530  TYPE gtyt_hrpad530.

  "Declaraciones
  DATA:
    lt_hrp1001 TYPE gtyt_hrp1001.


  "Inicializa retorno
  CLEAR:
    ptc_hrpad530.

  "Crea una copia de los datos
  lt_hrp1001 = pti_cs_st.

  "Ordena y elimina los registros duplicados
  SORT lt_hrp1001 BY adatanr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1001
    COMPARING adatanr.

  "Continua solo si se tienen datos
  CHECK NOT lt_hrp1001 IS INITIAL.

  "Obtiene los datos de la admision
  "Haciendo uso de la clave primaria de la tabla
  SELECT adatanr adm_ayear adm_perid adm_aclevel
         adm_categ adm_enrcateg adm_recpt
    INTO TABLE ptc_hrpad530
    FROM hrpad530
    FOR ALL ENTRIES IN lt_hrp1001
    WHERE adatanr EQ lt_hrp1001-adatanr.

  "Ordena los registros
  SORT ptc_hrpad530 BY adatanr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1771
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1771
  USING
    psi_ctrlcert  TYPE gty_ctrlcert
  CHANGING
    ptc_hrp1771   TYPE gtyt_hrp1771.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1771.

  "Continua solo si se tienen datos
  CHECK NOT psi_ctrlcert-programa IS INITIAL
        AND psi_ctrlcert-programa NE '00000000'.

  "Obtiene los datos del estudiante
  "Haciendo uso parcial de la clave primaria
  SELECT plvar otype objid subty istat begda endda varyf seqnr otjid
         ayear perid pr_status prs_state reason aclevel regdate cancdate
    INTO TABLE ptc_hrp1771
    FROM hrp1771
    WHERE plvar EQ '01'
      AND otype EQ cl_hrpiq00const=>c_otype_cs
      AND objid EQ psi_ctrlcert-programa.

  "Elimina los registros no válidos
  DELETE ptc_hrp1771
    WHERE NOT cancdate IS INITIAL.

  "Ordena los registros por estudio y fechas
  SORT ptc_hrp1771 BY objid ASCENDING begda ASCENDING endda ASCENDING regdate ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_HRP1771
*&---------------------------------------------------------------------*
FORM f_asignar_hrp1771
  USING
    pvi_partner     TYPE bu_partner
    pvi_profit_ctr  TYPE prctr
    pvi_scotjid     TYPE otjid
    psi_ctrlcert    TYPE gty_ctrlcert
    pti_hrp1771     TYPE gtyt_hrp1771
    pti_hrp1732     TYPE gtyt_hrp1732
    pti_fac_anual   TYPE gtyt_fac_anual
    pti_yearprd     TYPE gtyt_yearprd
    pti_reasont     TYPE gtyt_reasont
  CHANGING
    pvc_ayear_ini   TYPE piqperyr
    pvc_perid_ini   TYPE piqperid
    pvc_perit_ini   TYPE piqperit
    pvc_ayear_ult   TYPE piqperyr
    pvc_perid_ult   TYPE piqperid
    pvc_perit_ult   TYPE piqperit
    pvc_aclevel_ult TYPE piqlevel
    ptc_certacad_pe TYPE gtyt_certacad_pe
    ptc_certacad_pm TYPE gtyt_certacad_pm
    ptc_bp_sc_fica  TYPE gtyt_bp_sc_fica.

  "Declaraciones
  DATA:
    lv_tabix       TYPE sy-tabix,
    lv_tabix_persl TYPE sy-tabix,
    lv_index       TYPE i,
    lv_string      TYPE string.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1771>     TYPE gty_hrp1771,
    <fs_hrp1732>     TYPE gty_hrp1732,
    <fs_fac_anual>   TYPE gty_fac_anual,
    <fs_yearprd>     TYPE gty_yearprd,
    <fs_certacad_pe> TYPE gty_certacad_pe,
    <fs_certacad_pm> TYPE gty_certacad_pm,
    <fs_bp_sc_fica>  TYPE gty_bp_sc_fica,
    <fs_reasont>     TYPE gty_reasont.


  "Inicializa retornos
  CLEAR:
    pvc_ayear_ini,
    pvc_perid_ini,
    pvc_perit_ini,
    pvc_ayear_ult,
    pvc_perid_ult,
    pvc_perit_ult,
    pvc_aclevel_ult,
    ptc_certacad_pe,
    ptc_certacad_pm.

  "Obtiene el primer registro de matricula
  READ TABLE pti_hrp1771 TRANSPORTING NO FIELDS
    WITH KEY objid = psi_ctrlcert-programa
    BINARY SEARCH.

  "Continua solo si encuentra un registro
  CHECK sy-subrc EQ 0.

  "Asigna el indice del registro
  lv_tabix = sy-tabix.

  "Recorre los registros de matricula
  LOOP AT pti_hrp1771 ASSIGNING <fs_hrp1771> FROM lv_tabix.
    "Aumenta el indice
    ADD 1 TO lv_index.

    "Si el registro no pertenece al estudio
    IF <fs_hrp1771>-objid NE psi_ctrlcert-programa.
      "Termina de recorrer los registros
      EXIT.
    ENDIF.

    "Si es el primer registro
    IF lv_index EQ 1.
      "Asigna el Año y Periodo de inicio
      pvc_ayear_ini = <fs_hrp1771>-ayear.
      pvc_perid_ini = <fs_hrp1771>-perid.

      "Obtiene la descripcion del periodo
      lv_string = pvc_perid_ini.
      CALL FUNCTION 'Z_IES_CONV_PERIODO'
        CHANGING
          cv_valor_campo = lv_string.
      pvc_perit_ini = lv_string.
    ENDIF.

    "Si la matricula se encuentra en estado de excedencia
    IF <fs_hrp1771>-pr_status EQ '2'.
      "Crea el registro con el periodo de excedencia
      APPEND INITIAL LINE TO ptc_certacad_pe ASSIGNING <fs_certacad_pe>.

      "Asigna los datos del periodo
      <fs_certacad_pe>-opbel  = psi_ctrlcert-opbel.
      <fs_certacad_pe>-begda  = <fs_hrp1771>-begda.
      <fs_certacad_pe>-endda  = <fs_hrp1771>-endda.
      <fs_certacad_pe>-ayear  = <fs_hrp1771>-ayear.
      <fs_certacad_pe>-perid  = <fs_hrp1771>-perid.
      <fs_certacad_pe>-reason = <fs_hrp1771>-reason.

      "Obtiene la descripcion del motivo de excedencia
      READ TABLE pti_reasont ASSIGNING <fs_reasont>
        WITH KEY reason = <fs_hrp1771>-reason
        BINARY SEARCH.
      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la descripcion del motivo de excedencia
        <fs_certacad_pe>-reasontext = <fs_reasont>-reasontext.
      ENDIF.

      "Obtiene la descripcion del periodo
      lv_string = <fs_certacad_pe>-perid.
      CALL FUNCTION 'Z_IES_CONV_PERIODO'
        CHANGING
          cv_valor_campo = lv_string.
      <fs_certacad_pe>-perit = lv_string.
    ENDIF.

    "Asigna el año, periodo y nivel como ultimo cursado
    "Nota: Se queda con los datos del ultimo registro
    pvc_ayear_ult   = <fs_hrp1771>-ayear.
    pvc_perid_ult   = <fs_hrp1771>-perid.
    pvc_aclevel_ult = <fs_hrp1771>-aclevel.

    "Obtiene la descripcion del periodo
    lv_string = pvc_perid_ult.
    CALL FUNCTION 'Z_IES_CONV_PERIODO'
      CHANGING
        cv_valor_campo = lv_string.
    pvc_perit_ult = lv_string.

    "Crea el registro con el periodo matriculado
    APPEND INITIAL LINE TO ptc_certacad_pm ASSIGNING <fs_certacad_pm>.

    "Asigna los datos basicos del periodo
    <fs_certacad_pm>-opbel       = psi_ctrlcert-opbel.
    <fs_certacad_pm>-mat_ayear   = <fs_hrp1771>-ayear.
    <fs_certacad_pm>-mat_perid   = <fs_hrp1771>-perid.
    <fs_certacad_pm>-mat_begda   = <fs_hrp1771>-begda.
    <fs_certacad_pm>-mat_endda   = <fs_hrp1771>-endda.
    <fs_certacad_pm>-mat_regdate = <fs_hrp1771>-regdate.

    "Obtiene la descripcion del periodo
    lv_string = <fs_certacad_pm>-mat_perid.
    CALL FUNCTION 'Z_IES_CONV_PERIODO'
      CHANGING
        cv_valor_campo = lv_string.
    <fs_certacad_pm>-mat_perit = lv_string.

    "Obtiene la categoria de tasas
    READ TABLE pti_hrp1732 ASSIGNING <fs_hrp1732>
      WITH KEY otjid = pvi_scotjid
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Obtiene el primer periodo fica
      READ TABLE pti_fac_anual TRANSPORTING NO FIELDS
        WITH KEY scfeecat = <fs_hrp1732>-scfeecat
                 peryr    = <fs_hrp1771>-ayear
                 perid    = <fs_hrp1771>-perid
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el indice del primer registro
        lv_tabix_persl = sy-tabix.

        "Recorre los registros de los periodos customizados
        LOOP AT pti_fac_anual ASSIGNING <fs_fac_anual>
          FROM lv_tabix_persl.

          "Si el registro no pertenece al estudiante
          IF <fs_fac_anual>-scfeecat NE <fs_hrp1732>-scfeecat OR
             <fs_fac_anual>-peryr    NE <fs_hrp1771>-ayear  OR
             <fs_fac_anual>-perid    NE <fs_hrp1771>-perid.
            "Deja de recorrer los registros
            EXIT.
          ENDIF.

          "Si ya se tiene el periodo
          IF NOT <fs_certacad_pm>-mat_persl IS INITIAL.
            "Deja de recorrer los registros
            EXIT.
          ENDIF.

          "Si la fecha inicial de la extraccion es valida
          IF <fs_hrp1771>-regdate BETWEEN <fs_fac_anual>-begda_fica AND <fs_fac_anual>-endda_fica.
            "Asigna el periodo FICA
            <fs_certacad_pm>-mat_persl = <fs_fac_anual>-persl.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "Si no encontro periodo anualizado
    IF <fs_certacad_pm>-mat_persl IS INITIAL.
      "Obtiene el periodo FICA
      READ TABLE pti_yearprd ASSIGNING <fs_yearprd>
        WITH KEY peryr = <fs_hrp1771>-ayear
                 perid = <fs_hrp1771>-perid
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el periodo FICA
        <fs_certacad_pm>-mat_persl = <fs_yearprd>-persl.
      ENDIF.
    ENDIF.

    "Crea el registro con la relacion GPART - PERSL
    APPEND INITIAL LINE TO ptc_bp_sc_fica ASSIGNING <fs_bp_sc_fica>.

    "Asigna los datos de la relacion GPART - PERSL
    <fs_bp_sc_fica>-gpart = pvi_partner.
    <fs_bp_sc_fica>-persl = <fs_certacad_pm>-mat_persl.
    <fs_bp_sc_fica>-prctr = pvi_profit_ctr.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_certacad_pm BY opbel mat_ayear mat_perid.
  SORT ptc_bp_sc_fica BY gpart persl prctr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_CS_SC
*&---------------------------------------------------------------------*
FORM f_obtener_cs_sc
  USING
    psi_ctrlcert  TYPE gty_ctrlcert
  CHANGING
    ptc_cs_sc     TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lv_otjid    TYPE otjid.


  "Inicializa retorno
  CLEAR:
    ptc_cs_sc.

  "Continua solo si se tienen datos
  CHECK NOT psi_ctrlcert-programa IS INITIAL
        AND psi_ctrlcert-programa NE '00000000'.

  "Asigna el estudio
  lv_otjid+0(2) = cl_hrpiq00const=>c_otype_cs.
  lv_otjid+2(8) = psi_ctrlcert-programa.

  "Obtiene los datos del estudiante
  "Haciendo uso parcial del indice estandar 4 - Índice OTJID/SUBTY/SCLAS/PLVAR/ENDDA
  SELECT otype objid plvar rsign relat istat priox begda
         endda varyf seqnr otjid subty sclas sobid adatanr
    INTO TABLE ptc_cs_sc
    FROM hrp1001
    WHERE otjid EQ lv_otjid
      AND subty EQ cl_hrpiq00const=>c_relat_514a
      AND sclas EQ cl_hrpiq00const=>c_otype_sc
      AND plvar EQ '01'.

  "Elimina los registros invalidos
  DELETE ptc_cs_sc
    WHERE sobid IS INITIAL
      OR  sobid EQ '00000000'.

  "Ordena los registros por estudio
  SORT ptc_cs_sc BY objid ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS_SC
*&---------------------------------------------------------------------*
FORM f_obtener_datos_sc
  USING
    pti_cs_sc   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1730 TYPE gtyt_hrp1730
    ptc_sc_o    TYPE gtyt_hrp1001
    ptc_o_otp   TYPE gtyt_hrp1001
    ptc_otp_of  TYPE gtyt_hrp1001
    ptc_pensum  TYPE gtyt_pensum
    ptc_hrp1735 TYPE gtyt_hrp1735
    ptc_hrp1759 TYPE gtyt_hrp1759
    ptc_hrt1759 TYPE gtyt_hrt1759
    ptc_hrp1732 TYPE gtyt_hrp1732.

  "Declaraciones
  DATA:
    lt_hrp1001 TYPE gtyt_hrp1001.


  "Crea una copia de datos
  lt_hrp1001 = pti_cs_sc.

  "Ordena los registros y elimina duplicados
  SORT lt_hrp1001 BY varyf.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1001
    COMPARING varyf.

  "Obtiene los registros de la HRP1730
  PERFORM f_otener_hrp1730
    USING
      lt_hrp1001
    CHANGING
      ptc_hrp1730.

  "Obtiene la relacion SC - O
  PERFORM f_obtener_relaciones
    USING
      cl_hrpiq00const=>c_relat_501b
      cl_hrpiq00const=>c_otype_o
      lt_hrp1001
    CHANGING
      ptc_sc_o.

  "Obtiene la relacion O - O (Tipo Programa)
  PERFORM f_obtener_relaciones
    USING
      cl_hrpiq00const=>c_relat_002a
      cl_hrpiq00const=>c_otype_o
      ptc_sc_o
    CHANGING
      ptc_o_otp.

  "Obtiene la relacion O (Tipo Programa) - O (Facultad)
  PERFORM f_obtener_relaciones
    USING
      cl_hrpiq00const=>c_relat_002a
      cl_hrpiq00const=>c_otype_o
      ptc_o_otp
    CHANGING
      ptc_otp_of.

  "Obtiene la relacion SC - SM
  PERFORM f_obtener_pensum
    USING
      lt_hrp1001
    CHANGING
      ptc_pensum.

  "Obtiene los creditos del plan de estudios
  PERFORM f_obtener_hrp1735
    USING
      lt_hrp1001
    CHANGING
      ptc_hrp1735.

  "Obtiene los Vinculos a los Centros de beneficio
  PERFORM f_obtener_hrp1759
    USING
      lt_hrp1001
    CHANGING
      ptc_hrp1759.

  "Obtiene los Centros de beneficio
  PERFORM f_obtener_hrt1759
    USING
      ptc_hrp1759
    CHANGING
      ptc_hrt1759.

  "Obtiene las categorias de tasa
  PERFORM f_obtener_hrp1732
    USING
     lt_hrp1001
    CHANGING
      ptc_hrp1732.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OTENER_HRP1730
*&---------------------------------------------------------------------*
FORM f_otener_hrp1730
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1730   TYPE gtyt_hrp1730.


  "Inicializa retorno
  CLEAR:
   ptc_hrp1730.

  "COntinua solo si se tienen datos
  CHECK NOT pti_hrp1001 IS INITIAL.

  "Establece la seleccion de datos del estudio
  "Haciendo uso del indice estandar 1 - Index PLVAR/OTJID
  SELECT plvar otype objid subty istat begda endda
         varyf seqnr otjid optlength timeunit snies
    INTO TABLE ptc_hrp1730
    FROM hrp1730
    FOR ALL ENTRIES IN pti_hrp1001
    WHERE plvar EQ '01'
      AND otjid EQ pti_hrp1001-varyf.

  "Ordena los registros
  SORT ptc_hrp1730 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_RELACIONES
*&---------------------------------------------------------------------*
FORM f_obtener_relaciones
  USING
    pvi_subty   TYPE subty
    pvi_sclas   TYPE sclas
    pti_hrp1001 TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1001 TYPE gtyt_hrp1001.


  "Inicializa retorno
  CLEAR:
   ptc_hrp1001.

  "COntinua solo si se tienen datos
  CHECK NOT pti_hrp1001 IS INITIAL.

  "Obtiene los datos de la relacion
  "Haciendo uso parcial del indice estandar 4 - Índice OTJID/SUBTY/SCLAS/PLVAR/ENDDA
  SELECT otype objid plvar rsign relat istat priox begda
         endda varyf seqnr otjid subty sclas sobid adatanr
    INTO TABLE ptc_hrp1001
    FROM hrp1001
    FOR ALL ENTRIES IN pti_hrp1001
    WHERE otjid EQ pti_hrp1001-varyf
      AND subty EQ pvi_subty
      AND sclas EQ pvi_sclas
      AND plvar EQ '01'.

  "Elimina los registros invalidos
  DELETE ptc_hrp1001
    WHERE sobid IS INITIAL
      OR  otjid EQ ''
      OR  sobid EQ '00000000'.

  "Ordena los registros
  SORT ptc_hrp1001 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_DATOS
*&---------------------------------------------------------------------*
FORM f_agrupar_datos
  USING
    psi_ctrlcert    TYPE gty_ctrlcert
    psi_cmacbpst    TYPE gty_cmacbpst
    pti_but0id      TYPE gtyt_but0id
    pti_hrp1769     TYPE gtyt_hrp1769
    pti_cs_st       TYPE gtyt_hrp1001
    pti_hrpad530    TYPE gtyt_hrpad530
    pti_hrp1771     TYPE gtyt_hrp1771
    pti_cs_sc       TYPE gtyt_hrp1001
    pti_hrp1730     TYPE gtyt_hrp1730
    pti_sc_o        TYPE gtyt_hrp1001
    pti_o_otp       TYPE gtyt_hrp1001
    pti_otp_of      TYPE gtyt_hrp1001
    pti_pensum      TYPE gtyt_pensum
    pti_hrp1735     TYPE gtyt_hrp1735
    pti_st_sm       TYPE gtyt_hrp1001
    pti_hrpad506    TYPE gtyt_hrpad506
    pti_hrp1724     TYPE gtyt_hrp1724
    pti_hrp1759     TYPE gtyt_hrp1759
    pti_hrt1759     TYPE gtyt_hrt1759
    pti_hrp1732     TYPE gtyt_hrp1732
    pti_fac_anual   TYPE gtyt_fac_anual
    pti_yearprd     TYPE gtyt_yearprd
    pti_hrp1000     TYPE gtyt_hrp1000
    pti_tb039t      TYPE gtyt_tb039t
    pti_timeunitt   TYPE gtyt_timeunitt
    pti_smstatt     TYPE gtyt_smstatt
    pti_reasont     TYPE gtyt_reasont
    pti_processt    TYPE gtyt_processt
  CHANGING
    ptc_certacad_h  TYPE gtyt_certacad_h
    ptc_certacad_pe TYPE gtyt_certacad_pe
    ptc_certacad_pm TYPE gtyt_certacad_pm
    ptc_certacad_sm TYPE gtyt_certacad_sm
    ptc_certacad_na TYPE gtyt_certacad_na
    ptc_certacad_ps TYPE gtyt_certacad_ps
    ptc_certacad_ro TYPE gtyt_certacad_ro
    ptc_certacad_gr TYPE gtyt_certacad_gr
    ptc_bp_sc_fica  TYPE gtyt_bp_sc_fica.

  "Declaraciones
  DATA:
    lv_sobid             TYPE sobid,
    lv_string            TYPE string,
    lv_seqnr             TYPE seqnr,
    lv_otjid             TYPE otjid,
    lv_objid             TYPE hrobjid,
    lt_nro_ident         TYPE zedu_t_nro_ident,
    lt_notas_academicas  TYPE zedu_t_notas_academicas,
    lt_promedio_semestre TYPE zedu_t_promedio_semestre_notas,
    lt_datos_rotaciones  TYPE zedu_t_datos_rotaciones,
    lt_datos_graduacion  TYPE zedu_t_datos_graduacion.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_certacad_h>        TYPE gty_certacad_h,
    <fs_but0id>            TYPE gty_but0id,
    <fs_hrp1769>           TYPE gty_hrp1769,
    <fs_cs_st>             TYPE gty_hrp1001,
    <fs_hrpad530>          TYPE gty_hrpad530,
    <fs_cs_sc>             TYPE gty_hrp1001,
    <fs_hrp1730>           TYPE gty_hrp1730,
    <fs_sc_o>              TYPE gty_hrp1001,
    <fs_o_otp>             TYPE gty_hrp1001,
    <fs_otp_of>            TYPE gty_hrp1001,
    <fs_hrp1735>           TYPE gty_hrp1735,
    <fs_hrp1759>           TYPE gty_hrp1759,
    <fs_hrt1759>           TYPE gty_hrt1759,
    <fs_tb039t>            TYPE gty_tb039t,
    <fs_timeunitt>         TYPE gty_timeunitt,
    <fs_certacad_na>       TYPE gty_certacad_na,
    <fs_certacad_ps>       TYPE gty_certacad_ps,
    <fs_certacad_ro>       TYPE gty_certacad_ro,
    <fs_certacad_gr>       TYPE gty_certacad_gr,
    <fs_notas_academicas>  TYPE zedu_s_notas_academicas,
    <fs_promedio_semestre> TYPE zedu_s_promedio_semestre_notas,
    <fs_datos_rotaciones>  TYPE zedu_s_datos_rotaciones,
    <fs_datos_graduacion>  TYPE zedu_s_datos_graduacion,
    <fs_pensum>            TYPE gty_pensum,
    <fs_reasont>           TYPE gty_reasont,
    <fs_processt>          TYPE gty_processt.


  "Inicializa el retorno
  CLEAR:
    ptc_certacad_h,
    ptc_certacad_na,
    ptc_certacad_ps,
    ptc_certacad_ro,
    ptc_certacad_gr.

  "Crea la linea de registro
  APPEND INITIAL LINE TO ptc_certacad_h ASSIGNING <fs_certacad_h>.

  "Asigna el numero de documento
  <fs_certacad_h>-opbel = psi_ctrlcert-opbel.

  "Asigna el codigo de estudiante ST
  <fs_certacad_h>-stotjid+0(2) = cl_hrpiq00const=>c_otype_st.
  <fs_certacad_h>-stotjid+2(8) = psi_ctrlcert-estudiante.

  "Asigna el codigo de estudio CS
  <fs_certacad_h>-csotjid+0(2) = cl_hrpiq00const=>c_otype_cs.
  <fs_certacad_h>-csotjid+2(8) = psi_ctrlcert-programa.

  "Asigna los datos del estudiante
  <fs_certacad_h>-partner   = psi_cmacbpst-partner.
  <fs_certacad_h>-student12 = psi_cmacbpst-student12.

  "Obtiene los datos de identificacion del estudiante
  READ TABLE pti_but0id ASSIGNING <fs_but0id>
    WITH KEY partner = psi_cmacbpst-partner
    BINARY SEARCH.

  "Si encuentra el registro
  IF sy-subrc EQ 0.
    "Asigna los datos de identificacion
    <fs_certacad_h>-type      = <fs_but0id>-type.
    <fs_certacad_h>-idnumber  = <fs_but0id>-idnumber.
    <fs_certacad_h>-institute = <fs_but0id>-institute.

    "Obtiene la descripcion del tipo de identificacion
    READ TABLE pti_tb039t ASSIGNING <fs_tb039t>
      WITH KEY category = <fs_certacad_h>-type
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion del tipo de identificacion
      <fs_certacad_h>-type_text = <fs_tb039t>-text.
    ENDIF.
  ENDIF.

  "Asigna el estudiante a una variable
  lv_sobid = psi_ctrlcert-estudiante.

  "Obtiene los datos de relacion entre el estudio y el estudiante
  READ TABLE pti_cs_st ASSIGNING <fs_cs_st>
    WITH KEY objid = psi_ctrlcert-programa
             sobid = lv_sobid
    BINARY SEARCH.

  "Si encuentra el registro
  IF sy-subrc EQ 0.
    "Asigna el estado
    <fs_certacad_h>-istat = <fs_cs_st>-istat.

    "Obtiene los datos de la admision
    READ TABLE pti_hrpad530 ASSIGNING <fs_hrpad530>
      WITH KEY adatanr = <fs_cs_st>-adatanr
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      <fs_certacad_h>-adm_ayear    = <fs_hrpad530>-adm_ayear.
      <fs_certacad_h>-adm_perid    = <fs_hrpad530>-adm_perid.
      <fs_certacad_h>-adm_aclevel  = <fs_hrpad530>-adm_aclevel.
      <fs_certacad_h>-adm_categ    = <fs_hrpad530>-adm_categ.
      <fs_certacad_h>-adm_enrcateg = <fs_hrpad530>-adm_enrcateg.
      <fs_certacad_h>-adm_recpt    = <fs_hrpad530>-adm_recpt.

      "Obtiene la descripcion del periodo
      lv_string = <fs_certacad_h>-adm_perid.
      CALL FUNCTION 'Z_IES_CONV_PERIODO'
        CHANGING
          cv_valor_campo = lv_string.
      <fs_certacad_h>-adm_perit    = lv_string.
    ENDIF.
  ENDIF.

  "Obtiene los datos de la matricula al estudio
  READ TABLE pti_hrp1769 ASSIGNING <fs_hrp1769>
    WITH KEY objid = psi_ctrlcert-programa
    BINARY SEARCH.

  "Si encuentra el registro
  IF sy-subrc EQ 0.
    "Asigna los datos de la matricula al estudio
    <fs_certacad_h>-beg_process  = <fs_hrp1769>-beg_process.
    <fs_certacad_h>-beg_reason   = <fs_hrp1769>-beg_reason.
    <fs_certacad_h>-beg_key_date = <fs_hrp1769>-beg_key_date.
    <fs_certacad_h>-end_process  = <fs_hrp1769>-end_process.
    <fs_certacad_h>-end_reason   = <fs_hrp1769>-end_reason.
    <fs_certacad_h>-end_key_date = <fs_hrp1769>-end_key_date.

    "Obtiene la descripcion de la actividad de inicio
    READ TABLE pti_processt ASSIGNING <fs_processt>
      WITH KEY process = <fs_hrp1769>-beg_process
      BINARY SEARCH.
    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion de la actividad de inicio
      <fs_certacad_h>-beg_processt = <fs_processt>-processt.
    ENDIF.

    "Obtiene la descripcion del motivo de inicio
    READ TABLE pti_reasont ASSIGNING <fs_reasont>
      WITH KEY reason = <fs_hrp1769>-beg_reason
      BINARY SEARCH.
    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion del motivo de inicio
      <fs_certacad_h>-beg_reasont = <fs_reasont>-reasontext.
    ENDIF.

    "Obtiene la descripcion de la actividad de fin
    READ TABLE pti_processt ASSIGNING <fs_processt>
      WITH KEY process = <fs_hrp1769>-end_process
      BINARY SEARCH.
    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion de la actividad de fin
      <fs_certacad_h>-end_processt = <fs_processt>-processt.
    ENDIF.

    "Obtiene la descripcion del motivo de fin
    READ TABLE pti_reasont ASSIGNING <fs_reasont>
      WITH KEY reason = <fs_hrp1769>-end_reason
      BINARY SEARCH.
    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion del motivo de fin
      <fs_certacad_h>-end_reasont = <fs_reasont>-reasontext.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Admitido
    IF <fs_hrp1769>-endda       EQ '99991231' AND
       <fs_hrp1769>-beg_process EQ 'RA01'     AND
       pti_hrp1771              IS INITIAL.
      "Indica que es un estudiante Admitido
      <fs_certacad_h>-admitido = abap_true.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Activo
    IF <fs_hrp1769>-endda       EQ '99991231' AND
       <fs_hrp1769>-beg_process EQ 'RA01'     AND
       NOT pti_hrp1771          IS INITIAL.
      "Indica que es un estudiante Activo
      <fs_certacad_h>-activo = abap_true.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Graduado
    IF ( <fs_hrp1769>-end_process EQ 'RV01' OR
         <fs_hrp1769>-end_process EQ 'RW01' ) AND
       <fs_hrp1769>-end_reason EQ '1018'.
      "Indica que es un estudiante graduado
      <fs_certacad_h>-graduado = abap_true.
    ENDIF.

    "Si se cumplen las condiciones de un estudiante Retirado
    IF ( <fs_hrp1769>-end_process EQ 'RV01'   OR
         <fs_hrp1769>-end_process EQ 'RW01' ) AND
       <fs_hrp1769>-end_reason  NE '1018'.
      "Indica que es un estudiante retirado
      <fs_certacad_h>-retirado = abap_true.
    ENDIF.
  ENDIF.

  "Obtiene los datos de relacion entre el estudio y el plan de estudios
  READ TABLE pti_cs_sc ASSIGNING <fs_cs_sc>
    WITH KEY objid = psi_ctrlcert-programa
    BINARY SEARCH.

  "Si encuentra el registro
  IF sy-subrc EQ 0.
    "Asigna el plan de estudios
    <fs_certacad_h>-scotjid = <fs_cs_sc>-varyf.

    "Asigna la descripcion del plan de estudios
    PERFORM f_asignar_hrp1000
      USING
        <fs_certacad_h>-scotjid
        pti_hrp1000
      CHANGING
        <fs_certacad_h>-scstext.

    "Obtiene los datos del plan de estudios
    READ TABLE pti_hrp1730 ASSIGNING <fs_hrp1730>
      WITH KEY otjid = <fs_cs_sc>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna los datos del plan de estudios
      <fs_certacad_h>-snies     = <fs_hrp1730>-snies.
      <fs_certacad_h>-optlength = <fs_hrp1730>-optlength.
      <fs_certacad_h>-timeunit  = <fs_hrp1730>-timeunit.

      "Obtiene la descripcion de la unidad
      READ TABLE pti_timeunitt ASSIGNING <fs_timeunitt>
        WITH KEY timeunit = <fs_certacad_h>-timeunit
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la descripcion de la unidad
        <fs_certacad_h>-timeunitt = <fs_timeunitt>-timeunitt.
      ENDIF.

    ENDIF.

    "Obtiene la relacion SC - O
    READ TABLE pti_sc_o ASSIGNING <fs_sc_o>
      WITH KEY otjid = <fs_cs_sc>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Obtiene la relacion O - O (Tipo Programa)
      READ TABLE pti_o_otp ASSIGNING <fs_o_otp>
        WITH KEY otjid = <fs_sc_o>-varyf
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el tipo de programa
        <fs_certacad_h>-ootjid_tp = <fs_o_otp>-varyf.

        "Asigna la descripcion del tipo de programa
        PERFORM f_asignar_hrp1000
          USING
            <fs_certacad_h>-ootjid_tp
            pti_hrp1000
          CHANGING
            <fs_certacad_h>-otp_stext.

        "Obtiene la relacion O (Tipo Programa) - O (Facultad)
        READ TABLE pti_otp_of ASSIGNING <fs_otp_of>
          WITH KEY otjid = <fs_o_otp>-varyf
          BINARY SEARCH.

        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Asigna la facultad
          <fs_certacad_h>-ootjid_fa = <fs_otp_of>-varyf.

          "Asigna la descripcion de la facultad
          PERFORM f_asignar_hrp1000
            USING
              <fs_certacad_h>-ootjid_fa
              pti_hrp1000
            CHANGING
              <fs_certacad_h>-ofa_stext.
        ENDIF.
      ENDIF.
    ENDIF.

    "Obtiene la cantidad de creditos del programa
    READ TABLE pti_hrp1735 ASSIGNING <fs_hrp1735>
      WITH KEY otjid = <fs_cs_sc>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la cantidad de creditos del programa
      <fs_certacad_h>-cpmin  = <fs_hrp1735>-cpmin.
      <fs_certacad_h>-cpmax  = <fs_hrp1735>-cpmax.
      <fs_certacad_h>-cpunit = <fs_hrp1735>-cpunit.
    ENDIF.

    "Obtiene la relacion con el centro de beneficio
    READ TABLE pti_hrp1759 ASSIGNING <fs_hrp1759>
      WITH KEY otjid = <fs_cs_sc>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Obtiene el centro de beneficio
      READ TABLE pti_hrt1759 ASSIGNING <fs_hrt1759>
        WITH KEY tabnr = <fs_hrp1759>-tabnr
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el centro de beneficio
        <fs_certacad_h>-profit_ctr = <fs_hrt1759>-profit_ctr.
      ENDIF.
    ENDIF.
  ENDIF.

  "Asigna el año y periodo indicado en la solicitud del certificado
  <fs_certacad_h>-ayearperid = psi_ctrlcert-peryrperid.

  "NOTA: Se comenta debido a que la solicitud pasa a ser un rango
  "      por lo tanto no da lugar a varias descripciones
**  "Obtiene la descripcion del periodo
**  lv_string = <fs_certacad_h>-perid.
**  CALL FUNCTION 'Z_IES_CONV_PERIODO'
**    CHANGING
**      cv_valor_campo = lv_string.
**  <fs_certacad_h>-perit    = lv_string.

  "Asigna los datos de matriculas
  PERFORM f_asignar_hrp1771
    USING
      <fs_certacad_h>-partner
      <fs_certacad_h>-profit_ctr
      <fs_certacad_h>-scotjid
      psi_ctrlcert
      pti_hrp1771
      pti_hrp1732
      pti_fac_anual
      pti_yearprd
      pti_reasont
    CHANGING
      <fs_certacad_h>-ayear_ini
      <fs_certacad_h>-perid_ini
      <fs_certacad_h>-perit_ini
      <fs_certacad_h>-ayear_ult
      <fs_certacad_h>-perid_ult
      <fs_certacad_h>-perit_ult
      <fs_certacad_h>-aclevel_ult
      ptc_certacad_pe
      ptc_certacad_pm
      ptc_bp_sc_fica.

  "Si el ultimo nivel cursado es menor al ultimo nivel del plan de estudios
  IF <fs_certacad_h>-aclevel_ult LT <fs_certacad_h>-optlength.
    "Asigna el siguiente nivel
    <fs_certacad_h>-aclevel_sig = <fs_certacad_h>-aclevel_ult + 1.
  ENDIF.

  "Asigna las asignaturas
  PERFORM f_asignar_asignaturas
    USING
      psi_ctrlcert-opbel
      <fs_certacad_h>-stotjid
      <fs_certacad_h>-csotjid
      <fs_certacad_h>-scotjid
      pti_pensum
      pti_hrp1735
      pti_st_sm
      pti_hrpad506
      pti_hrp1724
      pti_hrp1000
      pti_smstatt
    CHANGING
      <fs_certacad_h>-sm_cpattemp
      ptc_certacad_sm.

  "Si se tienen datos de identificacion
  IF NOT <fs_certacad_h>-idnumber IS INITIAL.
    "Crea el registro de consulta
    APPEND <fs_certacad_h>-idnumber TO lt_nro_ident.

    "Asigna el plan de estudio a una variable
    lv_objid = <fs_certacad_h>-scotjid+2(8).

    "Obtiene los datos de notas academicas
    CALL FUNCTION 'Z_EDU_EXTRACTOR_NOTAS_ACADEM'
      EXPORTING
        iv_plan_estudio      = lv_objid
      TABLES
        it_nro_ident         = lt_nro_ident
        et_notas_academicas  = lt_notas_academicas
        et_promedio_semestre = lt_promedio_semestre.

    "Inicializa la variable de posiciones
    CLEAR:
      lv_seqnr.

    "Recorre los registros obtenidos
    LOOP AT lt_notas_academicas ASSIGNING <fs_notas_academicas>.
      "Crea un registro en la tabla de retorno
      APPEND INITIAL LINE TO ptc_certacad_na ASSIGNING <fs_certacad_na>.

      "Aumenta la posicion
      ADD 1 TO lv_seqnr.

      "Asigna los datos obtenidos
      MOVE-CORRESPONDING <fs_notas_academicas> TO <fs_certacad_na>.
      "Asigna los datos clave del registro
      <fs_certacad_na>-opbel = psi_ctrlcert-opbel.
      <fs_certacad_na>-seqnr = lv_seqnr.
    ENDLOOP.

    "Inicializa la variable de posiciones
    CLEAR:
      lv_seqnr.

    "Recorre los registros obtenidos
    LOOP AT lt_promedio_semestre ASSIGNING <fs_promedio_semestre>.
      "Crea un registro en la tabla de retorno
      APPEND INITIAL LINE TO ptc_certacad_ps ASSIGNING <fs_certacad_ps>.

      "Aumenta la posicion
      ADD 1 TO lv_seqnr.

      "Asigna los datos obtenidos
      MOVE-CORRESPONDING <fs_promedio_semestre> TO <fs_certacad_ps>.
      "Asigna los datos clave del registro
      <fs_certacad_ps>-opbel = psi_ctrlcert-opbel.
      <fs_certacad_ps>-seqnr = lv_seqnr.
    ENDLOOP.

    "Obtiene el pensum del programa
    READ TABLE pti_pensum ASSIGNING <fs_pensum>
      WITH KEY sc_otjid = <fs_certacad_h>-scotjid
      BINARY SEARCH.

    "Si se tienen datos
    IF sy-subrc EQ 0.
      "Obtiene los datos de rotaciones
      CALL FUNCTION 'Z_EDU_EXTRACTOR_ROTACIONES'
        TABLES
          it_nro_ident        = lt_nro_ident
          et_datos_rotaciones = lt_datos_rotaciones
        EXCEPTIONS
          sin_datos           = 1
          OTHERS              = 2.

      "Inicializa la variable de posiciones
      CLEAR:
        lv_seqnr.

      "Recorre los registros obtenidos
      LOOP AT lt_datos_rotaciones ASSIGNING <fs_datos_rotaciones>.
        "Arma la identificacion de la asignatura
        lv_otjid+0(2) = cl_hrpiq00const=>c_otype_sm.
        lv_otjid+2(8) = <fs_datos_rotaciones>-objid_sm.

        "Continua solo si la asignatura hace parte del pensum
        CHECK lv_otjid IN <fs_pensum>-sm_r_otjid.

        "Crea un registro en la tabla de retorno
        APPEND INITIAL LINE TO ptc_certacad_ro ASSIGNING <fs_certacad_ro>.

        "Aumenta la posicion
        ADD 1 TO lv_seqnr.

        "Asigna los datos obtenidos
        MOVE-CORRESPONDING <fs_datos_rotaciones> TO <fs_certacad_ro>.
        "Asigna los datos clave del registro
        <fs_certacad_ro>-opbel = psi_ctrlcert-opbel.
        <fs_certacad_ro>-seqnr = lv_seqnr.
      ENDLOOP.
    ENDIF.

    "Obtiene los datos de Graduación
    CALL FUNCTION 'Z_EDU_EXTRACTOR_GRADUACION'
      TABLES
        it_nro_ident        = lt_nro_ident
        et_datos_graduacion = lt_datos_graduacion
      EXCEPTIONS
        sin_datos           = 1
        OTHERS              = 2.

    "Inicializa la variable de posiciones
    CLEAR:
      lv_seqnr.

    "Recorre los registros obtenidos
    LOOP AT lt_datos_graduacion ASSIGNING <fs_datos_graduacion>.
      "Continua solo si el registro de grado pertenece al plan de estudio
      CHECK <fs_datos_graduacion>-plaest EQ <fs_certacad_h>-scotjid+2(8).

      "Crea un registro en la tabla de retorno
      APPEND INITIAL LINE TO ptc_certacad_gr ASSIGNING <fs_certacad_gr>.

      "Aumenta la posicion
      ADD 1 TO lv_seqnr.

      "Asigna los datos obtenidos
      MOVE-CORRESPONDING <fs_datos_graduacion> TO <fs_certacad_gr>.
      "Asigna los datos clave del registro
      <fs_certacad_gr>-opbel = psi_ctrlcert-opbel.
      <fs_certacad_gr>-seqnr = lv_seqnr.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GUARDAR_DATOS
*&---------------------------------------------------------------------*
FORM f_guardar_datos
  USING
    pvi_opbel       TYPE opbel_kk
    pti_certacad_h  TYPE gtyt_certacad_h
    pti_certacad_pe TYPE gtyt_certacad_pe
    pti_certacad_pm TYPE gtyt_certacad_pm
    pti_certacad_sm TYPE gtyt_certacad_sm
    pti_certacad_na TYPE gtyt_certacad_na
    pti_certacad_ps TYPE gtyt_certacad_ps
    pti_certacad_ro TYPE gtyt_certacad_ro
    pti_certacad_gr TYPE gtyt_certacad_gr.


  "Elimina los registros que puedan existir en los repositorios
  DELETE FROM zedu_certacad_h  WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_pe WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_pm WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_sm WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_na WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_ps WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_ro WHERE opbel EQ pvi_opbel.
  DELETE FROM zedu_certacad_gr WHERE opbel EQ pvi_opbel.

  "Establece los cambios
  COMMIT WORK AND WAIT.

  "Si se tienen datos
  IF NOT pti_certacad_h IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_h FROM TABLE pti_certacad_h.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_pe IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_pe FROM TABLE pti_certacad_pe.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_pm IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_pm FROM TABLE pti_certacad_pm.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_sm IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_sm FROM TABLE pti_certacad_sm.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_na IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_na FROM TABLE pti_certacad_na.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_ps IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_ps FROM TABLE pti_certacad_ps.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_ro IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_ro FROM TABLE pti_certacad_ro.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  "Si se tienen datos
  IF NOT pti_certacad_gr IS INITIAL.
    "Guarda los registros en la tabla
    "Inserta los registros en la tabla de repositorio
    MODIFY zedu_certacad_gr FROM TABLE pti_certacad_gr.

    "Si termina correctamente
    IF sy-subrc EQ 0.
      "Establece los cambios
      COMMIT WORK AND WAIT.

      "Si no termina correctamente
    ELSE.
      "Deshace los cambios
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  MESSAGE 'Termino el proceso de extracción' TYPE 'S'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PENSUM
*&---------------------------------------------------------------------*
FORM f_obtener_pensum
  USING
    pti_hrp1001 TYPE gtyt_hrp1001
  CHANGING
    ptc_pensum  TYPE gtyt_pensum.

  "Declaraciones
  DATA:
    lt_hrp1001_sm TYPE gtyt_hrp1001,
    lt_hrp1764    TYPE gtyt_hrp1764,
    lt_hrt1764    TYPE gtyt_hrt1764.


  "Obtiene la relacion SC - SM de las asignaturas principales
  PERFORM f_obtener_relaciones
    USING
      cl_hrpiq00const=>c_relat_500a
      cl_hrpiq00const=>c_otype_sm
      pti_hrp1001
    CHANGING
      lt_hrp1001_sm.

  "Obtiene las referencias de las asignaturas relacionadas
  PERFORM f_obtener_hrp1764
    USING
      lt_hrp1001_sm
    CHANGING
      lt_hrp1764.

  "Obtiene las asignaturas relacionadas
  PERFORM f_obtener_hrt1764
    USING
      lt_hrp1764
    CHANGING
      lt_hrt1764.

  "Agrupa las SM con sus respectivos SC
  PERFORM f_agrupar_pensum
    USING
      lt_hrp1001_sm
      lt_hrp1764
      lt_hrt1764
    CHANGING
      ptc_pensum.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1764
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1764
  USING
    pti_hrp1001_sm  TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1764     TYPE gtyt_hrp1764.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_otjid       TYPE gty_otjid,
    lt_hrp1001_sm  TYPE gtyt_hrp1001,
    lt_otjid       TYPE gtyt_otjid,
    lt_hrp1764_blq TYPE gtyt_hrp1764.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001_sm> TYPE gty_hrp1001.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1764.

  "Crea una copia de los datos
  lt_hrp1001_sm = pti_hrp1001_sm.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1001_sm BY varyf.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1001_sm
    COMPARING varyf.

  "Recorre los registros
  LOOP AT lt_hrp1001_sm ASSIGNING <fs_hrp1001_sm>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna los datos y crea el registro
    ls_otjid-otjid = <fs_hrp1001_sm>-varyf.
    APPEND ls_otjid TO lt_otjid.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE pa_block OR lv_ultimo = abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos de la tabla HRP1764 haciendo uso
      "parcial del indice estandar 4 - OTJID/SUBTY/SCLAS/PLVAR/ENDDA
      SELECT plvar otype objid subty istat begda
             endda varyf seqnr otjid tabnr
        FROM hrp1764
        FOR ALL ENTRIES IN lt_otjid
        WHERE otjid EQ lt_otjid-otjid
          AND plvar EQ '01'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp1764_blq
          PACKAGE SIZE pa_block.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros no vigentes
        DELETE lt_hrp1764_blq
          WHERE begda GT sy-datum
            OR  endda LT sy-datum
            OR  tabnr IS INITIAL
            OR  tabnr EQ ''.

        "Crea los registros en el retorno
        APPEND LINES OF lt_hrp1764_blq TO ptc_hrp1764.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_otjid.
    ENDIF.
  ENDLOOP.

  "Ordena los registros de retorno
  SORT ptc_hrp1764 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRT1764
*&---------------------------------------------------------------------*
FORM f_obtener_hrt1764
  USING
    pti_hrp1764   TYPE gtyt_hrp1764
  CHANGING
    ptc_hrt1764   TYPE gtyt_hrt1764.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_tabnr       TYPE gty_tabnr,
    lt_hrp1764     TYPE gtyt_hrp1764,
    lt_tabnr       TYPE gtyt_tabnr,
    lt_hrt1764_blq TYPE gtyt_hrt1764.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1764> TYPE gty_hrp1764.


  "Inicializa retorno
  CLEAR:
    ptc_hrt1764.

  "Crea una copia de los datos
  lt_hrp1764 = pti_hrp1764.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1764 BY tabnr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1764
    COMPARING tabnr.

  "Recorre los registros
  LOOP AT lt_hrp1764 ASSIGNING <fs_hrp1764>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna los datos y crea el registro
    ls_tabnr-tabnr = <fs_hrp1764>-tabnr.
    APPEND ls_tabnr TO lt_tabnr.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE pa_block OR lv_ultimo = abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos de la tabla HRT1764
      "haciendo uso parcial de la clave primaria de la tabla
      SELECT tabnr tabseqnr pt_otype pt_objid
        FROM hrt1764
        FOR ALL ENTRIES IN lt_tabnr
        WHERE tabnr EQ lt_tabnr-tabnr.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrt1764_blq
          PACKAGE SIZE pa_block.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros no vigentes
        DELETE lt_hrt1764_blq
          WHERE pt_otype NE cl_hrpiq00const=>c_otype_sm.

        "Crea los registros en el retorno
        APPEND LINES OF lt_hrt1764_blq TO ptc_hrt1764.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_tabnr.
    ENDIF.
  ENDLOOP.

  "Ordena los registros de retorno
  SORT ptc_hrt1764 BY tabnr tabseqnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_PENSUM
*&---------------------------------------------------------------------*
FORM f_agrupar_pensum
  USING
    pti_hrp1001_sm  TYPE gtyt_hrp1001
    pti_hrp1764     TYPE gtyt_hrp1764
    pti_hrt1764     TYPE gtyt_hrt1764
  CHANGING
    ptc_pensum      TYPE gtyt_pensum.

  "Declaraciones
  DATA:
    lv_ultimo_sc  TYPE abap_bool,
    lv_tabix_p    TYPE sy-tabix,
    lv_tabix_t    TYPE sy-tabix,
    ls_pensum     TYPE gty_pensum,
    ls_sm_r_otjid TYPE gty_r_otjid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001_sm> TYPE gty_hrp1001,
    <fs_hrp1764>    TYPE gty_hrp1764,
    <fs_hrt1764>    TYPE gty_hrt1764.


  "Recorre las asignaturas principales
  LOOP AT pti_hrp1001_sm ASSIGNING <fs_hrp1001_sm>.
    "Si es el primer registro del plan de estudio
    AT NEW objid.
      "Inicializa la estructura de datos
      CLEAR:
        lv_ultimo_sc,
        ls_pensum,
        ls_pensum-sm_r_otjid,
        ls_sm_r_otjid.

      "Asigna el plan de estudios
      ls_pensum-sc_otjid = <fs_hrp1001_sm>-otjid.

      "Indica que la asignatura es incluyente
      ls_sm_r_otjid-sign   = 'I'.
      ls_sm_r_otjid-option = 'EQ'.
    ENDAT.

    "Si es el ultimo registro del plan de estudio
    AT END OF objid.
      "Indica que es el ultimo registro
      lv_ultimo_sc = abap_true.
    ENDAT.

    "Asigna la asignatura y crea el registro
    ls_sm_r_otjid-low    = <fs_hrp1001_sm>-varyf.
    APPEND ls_sm_r_otjid TO ls_pensum-sm_r_otjid.

    "Obtiene la primer referencia de las asignaturas relacionadas
    READ TABLE pti_hrp1764 TRANSPORTING NO FIELDS
      WITH KEY otjid = <fs_hrp1001_sm>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Almacena el indice de la referencia
      lv_tabix_p = sy-tabix.

      "Recorre los registros de las referencias
      LOOP AT pti_hrp1764 ASSIGNING <fs_hrp1764> FROM lv_tabix_p.
        "Si la asignatura referente no es la consultada
        IF <fs_hrp1764>-otjid NE <fs_hrp1001_sm>-varyf.
          "Termina el recorrido
          EXIT.
        ENDIF.

        "Obtiene la primer asignatura relacionada
        READ TABLE pti_hrt1764 TRANSPORTING NO FIELDS
          WITH KEY tabnr = <fs_hrp1764>-tabnr
          BINARY SEARCH.

        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Almacena el indice de la referencia
          lv_tabix_t = sy-tabix.

          "Recorre los registros de las asignaturas relacionadas
          LOOP AT pti_hrt1764 ASSIGNING <fs_hrt1764> FROM lv_tabix_t.
            "Si la asignatura no es la consultada
            IF <fs_hrt1764>-tabnr NE <fs_hrp1764>-tabnr.
              "Termina el recorrido
              EXIT.
            ENDIF.

            "Asigna la asignatura y crea el registro
            ls_sm_r_otjid-low+0(2) = <fs_hrt1764>-pt_otype.
            ls_sm_r_otjid-low+2(8) = <fs_hrt1764>-pt_objid.
            APPEND ls_sm_r_otjid TO ls_pensum-sm_r_otjid.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.

    "Si es el ultimo registro del plan de estudio
    IF lv_ultimo_sc EQ abap_true.
      "Ordena y elimina los registros duplicados
      SORT ls_pensum-sm_r_otjid BY low.
      DELETE ADJACENT DUPLICATES FROM ls_pensum-sm_r_otjid
        COMPARING low.
      "Crea el registro en la tabla de retorno
      APPEND ls_pensum TO ptc_pensum.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_pensum BY sc_otjid.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1735
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1735
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1735   TYPE gtyt_hrp1735.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1735.

  "Continua solo si se tienen datos
  CHECK NOT pti_hrp1001 IS INITIAL.

  "Establece la seleccion de datos del estudio
  "Haciendo uso del indice estandar 1 - Index PLVAR/OTJID
  SELECT plvar otype objid subty istat begda endda
         varyf seqnr otjid cpmin cpmax cpunit
    INTO TABLE ptc_hrp1735
    FROM hrp1735
    FOR ALL ENTRIES IN pti_hrp1001
    WHERE plvar EQ '01'
      AND otjid EQ pti_hrp1001-varyf.

  "Ordena los registros
  SORT ptc_hrp1735 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRPAD506
*&---------------------------------------------------------------------*
FORM f_obtener_hrpad506
  USING
    pti_st_sm     TYPE gtyt_hrp1001
  CHANGING
    ptc_hrpad506  TYPE gtyt_hrpad506.

  "Declaraciones
  DATA:
    lv_cantidad TYPE i,
    lv_ultimo   TYPE abap_bool,
    ls_adatanr  TYPE gty_adatanr,
    lt_adatanr  TYPE gtyt_adatanr.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_st_sm> TYPE gty_hrp1001.


  "Inicializa retorno
  CLEAR:
    ptc_hrpad506.

  "Recorre los registros
  LOOP AT pti_st_sm ASSIGNING <fs_st_sm>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Si el registro tiene datos de la asignatura
    IF NOT <fs_st_sm>-adatanr IS INITIAL.
      "Inicializa declaraciones
      CLEAR:
        ls_adatanr.

      "Aumenta la cantidad de registros
      ADD 1 TO lv_cantidad.

      "Asigna los datos y crea el registro
      ls_adatanr-adatanr = <fs_st_sm>-adatanr.
      APPEND ls_adatanr TO lt_adatanr.
    ENDIF.

    "Si se tienen datos y
    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF NOT lt_adatanr IS INITIAL AND ( lv_cantidad GE pa_block OR lv_ultimo = abap_true ).
      "Obtiene los registros de la tabla HRPAD506
      "NOTA: No se maneja cursor ya que se entra por PK y en bloques
      SELECT adatanr smstatus cpattemp cpgained
             cpunit perid peryr alt_scaleid id pago
        APPENDING TABLE ptc_hrpad506
        FROM hrpad506
        FOR ALL ENTRIES IN lt_adatanr
        WHERE adatanr EQ lt_adatanr-adatanr.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_adatanr.
    ENDIF.
  ENDLOOP.

  "Ordena los registros de retorno
  SORT ptc_hrpad506 BY adatanr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_ASIGNATURAS
*&---------------------------------------------------------------------*
FORM f_asignar_asignaturas
  USING
    pvi_opbel         TYPE opbel_kk
    pvi_stotjid       TYPE otjid
    pvi_csotjid       TYPE otjid
    pvi_scotjid       TYPE otjid
    pti_pensum        TYPE gtyt_pensum
    pti_hrp1735       TYPE gtyt_hrp1735
    pti_st_sm         TYPE gtyt_hrp1001
    pti_hrpad506      TYPE gtyt_hrpad506
    pti_hrp1724       TYPE gtyt_hrp1724
    pti_hrp1000       TYPE gtyt_hrp1000
    pti_smstatt       TYPE gtyt_smstatt
  CHANGING
    pvc_sm_cpattemp   TYPE piqcpattemp
    ptc_certacad_sm   TYPE gtyt_certacad_sm.

  "Declaraciones
  DATA:
    lv_tabix       TYPE sy-tabix,
    lv_string      TYPE string,
    lt_certacad_sm TYPE gtyt_certacad_sm.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_st_sm>       TYPE gty_hrp1001,
    <fs_hrpad506>    TYPE gty_hrpad506,
    <fs_certacad_sm> TYPE gty_certacad_sm,
    <fs_pensum>      TYPE gty_pensum,
    <fs_sm_r_otjid>  TYPE gty_r_otjid,
    <fs_smstatt>     TYPE gty_smstatt.


  "Inicializa el retorno
  CLEAR:
    pvc_sm_cpattemp,
    ptc_certacad_sm.

  "Obtiene el primer registro de asignaturas
  READ TABLE pti_st_sm TRANSPORTING NO FIELDS
    WITH KEY otjid = pvi_stotjid
    BINARY SEARCH.

  "Continua solo si encuentra registro
  CHECK sy-subrc EQ 0.

  "Almacena el indice del registro
  lv_tabix = sy-tabix.

  "Recorre las asignaturas de la relacion ST - SC
  LOOP AT pti_st_sm ASSIGNING <fs_st_sm> FROM lv_tabix.
    "Si el registro pertenece a otro Estudiante
    IF <fs_st_sm>-otjid NE pvi_stotjid .
      "Deja de contar registros y sale
      EXIT.
    ENDIF.

    "Obtiene los datos adicionales de la asignatura
    READ TABLE pti_hrpad506 ASSIGNING <fs_hrpad506>
      WITH KEY adatanr = <fs_st_sm>-adatanr
      BINARY SEARCH.

    "Continua solo si encuentra registro
    CHECK sy-subrc EQ 0.

    "Valida si la asignatura esta matriculada en el estudio
    READ TABLE pti_hrp1724 TRANSPORTING NO FIELDS
      WITH KEY otjid     = pvi_csotjid
               modreg_id = <fs_hrpad506>-id
      BINARY SEARCH.

    "Continua solo si encuentra el registro
    CHECK sy-subrc EQ 0.

    "Crea un registro en la tabla de retorno
    APPEND INITIAL LINE TO ptc_certacad_sm ASSIGNING <fs_certacad_sm>.

    "Asigna los datos.
    <fs_certacad_sm>-opbel    = pvi_opbel.
    <fs_certacad_sm>-ayear    = <fs_hrpad506>-peryr.
    <fs_certacad_sm>-perid    = <fs_hrpad506>-perid.
    <fs_certacad_sm>-smotjid  = <fs_st_sm>-varyf.
    <fs_certacad_sm>-seqnr    = <fs_st_sm>-seqnr.
    <fs_certacad_sm>-smstatus = <fs_hrpad506>-smstatus.
    <fs_certacad_sm>-pago     = <fs_hrpad506>-pago.

    "Obtiene la descripcion del periodo
    lv_string = <fs_certacad_sm>-perid.
    CALL FUNCTION 'Z_IES_CONV_PERIODO'
      CHANGING
        cv_valor_campo = lv_string.
    <fs_certacad_sm>-perit = lv_string.

    "Asigna la descripcion de la asignatura
    PERFORM f_asignar_hrp1000
      USING
        <fs_certacad_sm>-smotjid
        pti_hrp1000
      CHANGING
        <fs_certacad_sm>-smstext.

    "Si el estado de la asignatura es:
    IF <fs_hrpad506>-smstatus EQ '01' OR  "Inscrito
       <fs_hrpad506>-smstatus EQ '02' OR  "Aprobado
       <fs_hrpad506>-smstatus EQ '03'.    "Perdido
      "Adiciona la cantidad de creditos cursados
      ADD <fs_hrpad506>-cpattemp TO pvc_sm_cpattemp.
    ENDIF.

    "Obtiene la descripcion del estado
    READ TABLE pti_smstatt ASSIGNING <fs_smstatt>
      WITH KEY smstatus = <fs_certacad_sm>-smstatus
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ '0'.
      "Asigna la descripcion del estado
      <fs_certacad_sm>-smstatust =  <fs_smstatt>-smstatust.
    ENDIF.
  ENDLOOP.

  "Crea una copia de los registros creados
  lt_certacad_sm = ptc_certacad_sm.

  "Elimina los registros de las asignaturas no inscritas y no ganadas
  DELETE lt_certacad_sm
    WHERE smstatus NE '01'
      AND smstatus NE '02'.

  "Ordena los registros
  SORT lt_certacad_sm BY smotjid.

  "Obtiene el pensum del programa
  READ TABLE pti_pensum ASSIGNING <fs_pensum>
    WITH KEY sc_otjid = pvi_scotjid
    BINARY SEARCH.

  "Continua solo si se tienen datos
  CHECK sy-subrc EQ 0.

  "Recorre los registros del pensum
  LOOP AT <fs_pensum>-sm_r_otjid ASSIGNING <fs_sm_r_otjid>.
    "Valida si la asignatura ya fue cursada
    READ TABLE lt_certacad_sm TRANSPORTING NO FIELDS
      WITH KEY smotjid = <fs_sm_r_otjid>-low
      BINARY SEARCH.

    "Si no encuentra el registro
    IF sy-subrc NE 0.
      "Crea un registro en la tabla de retorno
      APPEND INITIAL LINE TO ptc_certacad_sm ASSIGNING <fs_certacad_sm>.

      "Asigna los datos.
      <fs_certacad_sm>-opbel    = pvi_opbel.
      <fs_certacad_sm>-ayear    = ''.
      <fs_certacad_sm>-perid    = ''.
      <fs_certacad_sm>-smotjid  = <fs_sm_r_otjid>-low.
      <fs_certacad_sm>-seqnr    = ''.
      <fs_certacad_sm>-smstatus = ''.
      <fs_certacad_sm>-pago     = ''.

      "Asigna la descripcion de la asignatura
      PERFORM f_asignar_hrp1000
        USING
          <fs_certacad_sm>-smotjid
          pti_hrp1000
        CHANGING
          <fs_certacad_sm>-smstext.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1724
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1724
  USING
    pti_cs_st      TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1724    TYPE gtyt_hrp1724.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_otjid       TYPE gty_otjid,
    lt_otjid       TYPE gtyt_otjid,
    lt_hrp1724_blq TYPE gtyt_hrp1724.


  "Inicializa retornos
  CLEAR:
    ptc_hrp1724.

  "Establece la seleccion de datos de la tabla HRP1724
  "Haciendo uso del indice estadar 1 - Index PLVAR/OTJID
  SELECT plvar otype objid subty istat begda
         endda varyf seqnr otjid modreg_id
    INTO TABLE ptc_hrp1724
    FROM hrp1724
    FOR ALL ENTRIES IN pti_cs_st
    WHERE otjid EQ pti_cs_st-otjid
      AND plvar EQ '01'.

  "Elimina los registros no validos
  DELETE ptc_hrp1724
    WHERE modreg_id IS INITIAL
      OR  modreg_id EQ '00000000000000000000000000000000'.

  "Ordena los registros
  SORT ptc_hrp1724 BY otjid ASCENDING modreg_id ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1759
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1759
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1759   TYPE gtyt_hrp1759.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1759.

  "Continua solo si se tienen datos
  CHECK NOT pti_hrp1001 IS INITIAL.

  "Establece la seleccion de datos del estudio
  "Haciendo uso del indice estandar 1 - Index PLVAR/OTJID
  SELECT plvar otype objid subty istat begda
         endda varyf seqnr infty otjid tabnr
    INTO TABLE ptc_hrp1759
    FROM hrp1759
    FOR ALL ENTRIES IN pti_hrp1001
    WHERE plvar EQ '01'
      AND otjid EQ pti_hrp1001-varyf.

  "Elimina los registros invalidos
  DELETE ptc_hrp1759
    WHERE tabnr IS INITIAL
      OR  tabnr EQ ''.

  "Ordena los registros
  SORT ptc_hrp1759 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRT1759
*&---------------------------------------------------------------------*
FORM f_obtener_hrt1759
  USING
    pti_hrp1759   TYPE gtyt_hrp1759
  CHANGING
    ptc_hrt1759   TYPE gtyt_hrt1759.

  "Declaraciones
  DATA:
    lt_hrp1759  TYPE gtyt_hrp1759.


  "Inicializa retorno
  CLEAR:
    ptc_hrt1759.

  "Crea una copia de los datos
  lt_hrp1759 = pti_hrp1759.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1759 BY tabnr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1759
    COMPARING tabnr.

  "Continua solo si se tienen datos
  CHECK NOT lt_hrp1759 IS INITIAL.

  "Establece la seleccion de datos de la tabla HRT1759
  "haciendo uso parcial de la clave primaria de la tabla
  SELECT tabnr tabseqnr profit_ctr
    INTO TABLE ptc_hrt1759
    FROM hrt1759
    FOR ALL ENTRIES IN lt_hrp1759
    WHERE tabnr EQ lt_hrp1759-tabnr.

  "Ordena los registros de retorno
  SORT ptc_hrt1759 BY tabnr tabseqnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1732
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1732
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1732   TYPE gtyt_hrp1732.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1732.

  "Continua solo si se tienen datos
  CHECK NOT pti_hrp1001 IS INITIAL.

  "Define la seleccion de datos
  "Haciendo uso del indice estandar 1
  SELECT plvar otype objid subty istat begda
         endda varyf seqnr otjid scfeecat
    INTO TABLE ptc_hrp1732
    FROM hrp1732
    FOR ALL ENTRIES IN pti_hrp1001
    WHERE otjid EQ pti_hrp1001-otjid
      AND plvar EQ '01'.

  "Ordena los registros
  SORT ptc_hrp1732 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PERIODOS_FICA
*&---------------------------------------------------------------------*
FORM f_obtener_periodos_fica
  USING
    pti_hrp1771   TYPE gtyt_hrp1771
  CHANGING
    ptc_fac_anual TYPE gtyt_fac_anual
    ptc_yearprd   TYPE gtyt_yearprd.

  "Declaraciones
  DATA:
      lt_hrp1771   TYPE gtyt_hrp1771.


  "Inicializa retornos
  CLEAR:
    ptc_fac_anual,
    ptc_yearprd.

  "Crea una copia de los registros
  lt_hrp1771 = pti_hrp1771.

  "Elimina los registros invalidos
  DELETE lt_hrp1771
    WHERE ayear IS INITIAL
      OR  ayear EQ '0000'
      OR  perid IS INITIAL
      OR  perid EQ '000'.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1771 BY ayear perid.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1771
    COMPARING ayear perid.

  "Valida que se tengan periodos
  CHECK NOT lt_hrp1771 IS INITIAL.

  "Obtiene los périodos fica de los anualizados
  "Nota: Se traen la totalidad de campos ya que se requieren
  SELECT *
    INTO TABLE ptc_fac_anual
    FROM zedu_fac_anual
    FOR ALL ENTRIES IN lt_hrp1771
    WHERE peryr EQ lt_hrp1771-ayear
      AND perid EQ lt_hrp1771-perid.

  "Obtiene los periodod fica de los demas periodos academicos
  "Haciendo uso de la clave primaria de la tabla
  SELECT *
    INTO TABLE ptc_yearprd
    FROM t7piqyearprd
    FOR ALL ENTRIES IN lt_hrp1771
    WHERE peryr EQ lt_hrp1771-ayear
      AND perid EQ lt_hrp1771-perid.

  "Ordena los registros
  SORT ptc_fac_anual BY scfeecat peryr perid begda_fica endda_fica.
  SORT ptc_yearprd   BY peryr perid.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_FICA
*&---------------------------------------------------------------------*
FORM f_obtener_fica
  USING
    pti_r_blart_fact    TYPE gtyt_r_blart
    pti_r_blart_ma      TYPE gtyt_r_blart
    pti_r_psobtyp_ma    TYPE gtyt_r_psobtyp
    pti_r_augrd_fact    TYPE gtyt_r_augrd
    pti_r_hvorg         TYPE gtyt_r_hvorg
    pti_r_tvorg_fact    TYPE gtyt_r_tvorg
  CHANGING
    ptc_bp_sc_fica      TYPE gtyt_bp_sc_fica
    ptc_dfkkop          TYPE gtyt_dfkkop.


  "Obtiene las partidas de los interlocutores
  PERFORM f_obtener_dfkkop
    USING
      pti_r_blart_fact
      pti_r_augrd_fact
      pti_r_hvorg
      pti_r_tvorg_fact
      ptc_bp_sc_fica
    CHANGING
      ptc_dfkkop.

  "Asigna la referencia a la relacion BP SC FICA
  PERFORM f_asignar_xblnr
    USING
      pti_r_blart_ma
      pti_r_psobtyp_ma
      ptc_dfkkop
    CHANGING
      ptc_bp_sc_fica.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DFKKOP
*&---------------------------------------------------------------------*
FORM f_obtener_dfkkop
  USING
    pti_r_blart     TYPE gtyt_r_blart
    pti_r_augrd     TYPE gtyt_r_augrd
    pti_r_hvorg     TYPE gtyt_r_hvorg
    pti_r_tvorg     TYPE gtyt_r_tvorg
    pti_bp_sc_fica  TYPE gtyt_bp_sc_fica
  CHANGING
    ptc_dfkkop      TYPE gtyt_dfkkop.

  "Declaraciones
  DATA:
    lc_cursor         TYPE cursor,
    lv_cantidad       TYPE i,
    lv_ultimo         TYPE abap_bool,
    lt_dfkkop_blq     TYPE gtyt_dfkkop,
    lt_bp_sc_fica_tmp TYPE gtyt_bp_sc_fica,
    lt_bp_sc_fica_blq TYPE gtyt_bp_sc_fica.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_bp_sc_fica> TYPE gty_bp_sc_fica.


  "Inicializa retorno
  CLEAR:
    ptc_dfkkop.

  "Crea una copia de los datos
  lt_bp_sc_fica_tmp = pti_bp_sc_fica.

  "Ordena y elimina los registros duplicados por BP y Periodo
  SORT lt_bp_sc_fica_tmp BY gpart persl.
  DELETE ADJACENT DUPLICATES FROM lt_bp_sc_fica_tmp
    COMPARING gpart persl.

  "Recorre los registros
  LOOP AT lt_bp_sc_fica_tmp ASSIGNING <fs_bp_sc_fica>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "crea el registro
    APPEND <fs_bp_sc_fica> TO lt_bp_sc_fica_blq.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE pa_block OR lv_ultimo = abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos de la tabla DFKKOP haciendo uso
      "del indice Z03 - GPART/PERSL/BLART/HVORG/TVORG
      SELECT opbel opupw opupk opupz prctr augst
             gpart abwtp hvorg tvorg stakz faedn
             betrh persl augrd blart xblnr psobtyp
        FROM dfkkop
        FOR ALL ENTRIES IN lt_bp_sc_fica_blq
        WHERE gpart EQ lt_bp_sc_fica_blq-gpart
          AND persl EQ lt_bp_sc_fica_blq-persl
          AND blart IN pti_r_blart
          AND hvorg IN pti_r_hvorg
          AND tvorg IN pti_r_tvorg.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_dfkkop_blq
          PACKAGE SIZE pa_block.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros invalidos
        DELETE lt_dfkkop_blq
          WHERE augrd IN pti_r_augrd.

        "Asigna los registros validos al retorno
        APPEND LINES OF lt_dfkkop_blq TO ptc_dfkkop.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_bp_sc_fica_blq.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_dfkkop BY gpart persl xblnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_XBLNR
*&---------------------------------------------------------------------*
FORM f_asignar_xblnr
  USING
    pti_r_blart     TYPE gtyt_r_blart
    pti_r_psobtyp   TYPE gtyt_r_psobtyp
    pti_dfkkop      TYPE gtyt_dfkkop
  CHANGING
    ptc_bp_sc_fica  TYPE gtyt_bp_sc_fica.

  "Declaraciones
  DATA:
    lv_tabix   TYPE sy-tabix,
    ls_r_xblnr TYPE gty_r_xblnr,
    lt_dfkkop  TYPE gtyt_dfkkop.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_bp_sc_fica> TYPE gty_bp_sc_fica,
    <fs_dfkkop>     TYPE gty_dfkkop.


  "Crea una copia de los datos
  lt_dfkkop = pti_dfkkop.

  "Elimina los registros que no aplican para el concepto de matricula academica
  DELETE lt_dfkkop
    WHERE NOT blart IN pti_r_blart
      OR  xblnr IS INITIAL
      OR  NOT psobtyp IN pti_r_psobtyp.

  "Ordena los registros validos
  SORT lt_dfkkop BY gpart prctr persl.

  "Indica que los registros son incluyentes
  ls_r_xblnr-sign = 'I'.
  ls_r_xblnr-option = 'EQ'.

  "Recorre los registros de relacion BP SC FICA
  LOOP AT ptc_bp_sc_fica ASSIGNING <fs_bp_sc_fica>.
    "Obtiene la primer referencia de la facturacion
    READ TABLE lt_dfkkop TRANSPORTING NO FIELDS
      WITH KEY gpart = <fs_bp_sc_fica>-gpart
               prctr = <fs_bp_sc_fica>-prctr
               persl = <fs_bp_sc_fica>-persl
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Almacena el indice del registro
      lv_tabix = sy-tabix.

      "Recorre los registros desde la primer referencia
      LOOP AT lt_dfkkop ASSIGNING <fs_dfkkop>
        FROM lv_tabix.

        "Si el BP, Periodo o Centro de Beneficio es diferente
        IF <fs_dfkkop>-gpart NE <fs_bp_sc_fica>-gpart OR
           <fs_dfkkop>-prctr NE <fs_bp_sc_fica>-prctr OR
           <fs_dfkkop>-persl NE <fs_bp_sc_fica>-persl.
          "Deja de recorrer los registros
          EXIT.
        ENDIF.

        "Asigna la referencia y crea el registro
        ls_r_xblnr-low = <fs_dfkkop>-xblnr.
        APPEND ls_r_xblnr TO <fs_bp_sc_fica>-r_xblnr.
      ENDLOOP.

      "Ordena los registros
      SORT <fs_bp_sc_fica>-r_xblnr BY low.

      "Elimina duplicados
      DELETE ADJACENT DUPLICATES FROM <fs_bp_sc_fica>-r_xblnr
        COMPARING low.

      "Elimina los registros invalidos
      DELETE <fs_bp_sc_fica>-r_xblnr
        WHERE low IS INITIAL
          OR  low EQ '0000000000000000'.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_FICA
*&---------------------------------------------------------------------*
FORM f_asignar_fica
  USING
    pti_certacad_h      TYPE gtyt_certacad_h
    pti_r_blart_fact    TYPE gtyt_r_blart
    pti_r_augrd_pdir    TYPE gtyt_r_augrd
    pti_r_augrd_pcom    TYPE gtyt_r_augrd
    pti_r_tvorg_esta    TYPE gtyt_r_tvorg
    pti_r_tvorg_real    TYPE gtyt_r_tvorg
    pti_r_tvorg_fact    TYPE gtyt_r_tvorg
    pti_bp_sc_fica      TYPE gtyt_bp_sc_fica
    pti_dfkkop          TYPE gtyt_dfkkop
  CHANGING
    ptc_certacad_pm      TYPE gtyt_certacad_pm.

  "Declaraciones
  DATA:
    lv_tabix_pm TYPE sy-tabix,
    lv_tabix    TYPE sy-tabix,
    lv_partida  TYPE abap_bool.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_certacad_h>  TYPE gty_certacad_h,
    <fs_certacad_pm> TYPE gty_certacad_pm,
    <fs_bp_sc_fica>  TYPE gty_bp_sc_fica,
    <fs_dfkkop>      TYPE gty_dfkkop.


  "Recorre los registros de cabecera
  LOOP AT pti_certacad_h ASSIGNING <fs_certacad_h>.
    "Obtiene el primer periodo matriculado
    READ TABLE ptc_certacad_pm TRANSPORTING NO FIELDS
      WITH KEY opbel = <fs_certacad_h>-opbel
      BINARY SEARCH.

    "Continua solo si encuentra el registro
    CHECK sy-subrc EQ 0.

    "Asigna el indice del primer registro periodo matriculado
    lv_tabix_pm = sy-tabix.

    "Recorre los registros de los periodos matriculados
    LOOP AT ptc_certacad_pm ASSIGNING <fs_certacad_pm>
      FROM lv_tabix_pm.

      "Si el registro no pertenece al estudio
      IF <fs_certacad_h>-opbel NE <fs_certacad_pm>-opbel.
        "Deja de recorrer los registros
        EXIT.
      ENDIF.

      "Inicializa declaraciones
      CLEAR:
        lv_partida.

      "Obtiene la referencia para las partidas
      READ TABLE pti_bp_sc_fica ASSIGNING <fs_bp_sc_fica>
        WITH KEY gpart = <fs_certacad_h>-partner
                 persl = <fs_certacad_pm>-mat_persl
                 prctr = <fs_certacad_h>-profit_ctr
        BINARY SEARCH.

      "Continua solo si encuentra el registro
      CHECK sy-subrc EQ 0.

      "Continua solo si tiene referencias
      CHECK NOT <fs_bp_sc_fica>-r_xblnr IS INITIAL.

      "Obtiene el primer registro de la referencia para el periodo
      READ TABLE pti_dfkkop TRANSPORTING NO FIELDS
        WITH KEY gpart = <fs_certacad_h>-partner
                 persl = <fs_certacad_pm>-mat_persl
        BINARY SEARCH.

      "Continua solo si encuentra el registro
      CHECK sy-subrc EQ 0.

      "Almacena el indice del primer registro
      lv_tabix = sy-tabix.

      "Recorre las partidas de la referencia para el periodo
      LOOP AT pti_dfkkop ASSIGNING <fs_dfkkop>
        FROM lv_tabix.

        "Si el registro no pertenece al BP o periodo
        IF <fs_dfkkop>-gpart NE <fs_certacad_h>-partner        OR
           <fs_dfkkop>-persl NE <fs_certacad_pm>-mat_persl.
          "Deja de recorrer los registros
          EXIT.
        ENDIF.

        "Continua solo si la referencia hace parte de la facturacion
        CHECK <fs_dfkkop>-xblnr IN <fs_bp_sc_fica>-r_xblnr.

        "Valida si la partida es un Pago Directo Estadistico
        IF <fs_dfkkop>-augst EQ '9'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_esta AND
           <fs_dfkkop>-augrd IN pti_r_augrd_pdir AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida al Pago Directo Estadistico
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_de.
          "Adiciona el valor de la partida al Pago Total
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_total.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es un Pago Directo Real
        IF <fs_dfkkop>-augst EQ '9'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_real AND
           <fs_dfkkop>-augrd IN pti_r_augrd_pdir AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida al Pago Directo Real
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_dr.
          "Adiciona el valor de la partida al Pago Total
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_total.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es un Pago por Compensacion Estadistica
        IF <fs_dfkkop>-augst EQ '9'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_esta AND
           <fs_dfkkop>-augrd IN pti_r_augrd_pcom AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida al Pago por Compensacion Estadistica
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_ce.
          "Adiciona el valor de la partida al Pago Total
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_total.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es un Pago por Compensacion Real
        IF <fs_dfkkop>-augst EQ '9'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_real AND
           <fs_dfkkop>-augrd IN pti_r_augrd_pcom AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida al Pago por Compensacion Real
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_cr.
          "Adiciona el valor de la partida al Pago Total
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-pago_total.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida hace parte de la Facturacion Total
        IF <fs_dfkkop>-tvorg     IN pti_r_tvorg_fact AND
           <fs_dfkkop>-blart     IN pti_r_blart_fact.

          "Adiciona el valor de la partida a la Facturacion Total
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-total_facturado.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es una Deuda Abierta Real
        IF <fs_dfkkop>-augst NE '9'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_fact AND
           <fs_dfkkop>-stakz EQ ''               AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida a la Deuda Abierta Real
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-deuda_ar.
          "Adiciona el valor de la partida a la Deuda Abierta Real + Estadistica
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-deuda_a_re.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es una Deuda Abierta Estadistica
        IF <fs_dfkkop>-augst NE '9'              AND
           <fs_dfkkop>-abwtp EQ 'R'              AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_fact AND
           <fs_dfkkop>-stakz EQ 'G'              AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida a la Deuda Abierta Estadistica
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-deuda_ae.
          "Adiciona el valor de la partida a la Deuda Abierta Real + Estadistica
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-deuda_a_re.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.

        "Valida si la partida es una Deuda Abierta Otros
        IF <fs_dfkkop>-augst NE '9'              AND
           <fs_dfkkop>-abwtp EQ ''               AND
           <fs_dfkkop>-tvorg IN pti_r_tvorg_fact AND
           <fs_dfkkop>-stakz EQ 'G'              AND
           <fs_dfkkop>-blart IN pti_r_blart_fact.

          "Adiciona el valor de la partida a la Deuda Abierta Otros
          ADD <fs_dfkkop>-betrh TO <fs_certacad_pm>-deuda_ao.

          "Indica que encontro una partida valida
          lv_partida = abap_true.
        ENDIF.
      ENDLOOP.

      "Continua solo si encontro alguna partida valida
      CHECK lv_partida EQ abap_true.

      "Calcula el total de la deuda
      <fs_certacad_pm>-total_deuda = <fs_certacad_pm>-total_facturado - <fs_certacad_pm>-pago_total.

      "Si el total de la deuda es menor o igual a 0 "cero"
      IF <fs_certacad_pm>-total_deuda LE 0.
        "Se indica que el estudiante se considera matriculado financieramente
        <fs_certacad_pm>-mat_financiera = abap_true.

        "En caso contrario
      ELSE.
        "Si el total de la deuda es menor o igual al valor financiado o en plan de pagos
        IF <fs_certacad_pm>-total_deuda LE <fs_certacad_pm>-deuda_a_re.
          "Se indica que el estudiante se considera matriculado financieramente
          <fs_certacad_pm>-mat_financiera = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1000
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1000
  USING
    pti_cs_sc   TYPE gtyt_hrp1001
    pti_o_otp   TYPE gtyt_hrp1001
    pti_otp_of  TYPE gtyt_hrp1001
    pti_pensum  TYPE gtyt_pensum
    pti_st_sm   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1000 TYPE gtyt_hrp1000.

  "Declaraciones
  DATA:
    ls_otjid   TYPE gty_otjid,
    lt_hrp1001 TYPE gtyt_hrp1001,
    lt_otjid   TYPE gtyt_otjid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001> TYPE gty_hrp1001,
    <fs_pensum>  TYPE gty_pensum,
    <fs_r_otjid> TYPE gty_r_otjid.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1000.

  "Crea una copia de los datos
  APPEND LINES OF pti_cs_sc  TO lt_hrp1001.
  APPEND LINES OF pti_o_otp  TO lt_hrp1001.
  APPEND LINES OF pti_otp_of TO lt_hrp1001.
  APPEND LINES OF pti_st_sm  TO lt_hrp1001.

  "Recorre los registros
  LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001>.
    "Asigna y crea el registro con el objeto origen
    ls_otjid-otjid = <fs_hrp1001>-otjid.
    APPEND ls_otjid TO lt_otjid.
    "Asigna y crea el registro con el objeto relacionado
    ls_otjid-otjid = <fs_hrp1001>-varyf.
    APPEND ls_otjid TO lt_otjid.
  ENDLOOP.

  "Recorre los pensum
  LOOP AT pti_pensum ASSIGNING <fs_pensum>.
    "Recorre las asignaturas del pensum
    LOOP AT <fs_pensum>-sm_r_otjid ASSIGNING <fs_r_otjid>.
      "Asigna y crea el registro con la asignatura
      ls_otjid-otjid = <fs_r_otjid>-low.
      APPEND ls_otjid TO lt_otjid.
    ENDLOOP.
  ENDLOOP.

  "Ordena los registros y elimina los duplicados
  SORT lt_otjid BY otjid.
  DELETE ADJACENT DUPLICATES FROM lt_otjid
    COMPARING otjid.

  "Elimina los registros invalidos
  DELETE lt_otjid
    WHERE otjid IS INITIAL
      OR  otjid EQ ''.

  "Continua solo si se tienen datos
  CHECK NOT lt_otjid IS INITIAL.

  "Obtiene las descripciones de los objetos
  "Haciendo uso del indice 1 - Índice OTJID/PLVAR
  SELECT plvar otype objid istat begda
         endda langu seqnr otjid stext
    INTO TABLE ptc_hrp1000
    FROM hrp1000
    FOR ALL ENTRIES IN lt_otjid
    WHERE otjid EQ lt_otjid-otjid
      AND plvar EQ '01'.

  "Elimina los registros que no pertenecen al idioma
  DELETE ptc_hrp1000
    WHERE langu NE 'S'.

  "Ordena los registros
  SORT ptc_hrp1000 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_HRP1000
*&---------------------------------------------------------------------*
FORM f_asignar_hrp1000
  USING
    pvi_otjid   TYPE otjid
    pti_hrp1000 TYPE gtyt_hrp1000
  CHANGING
    pvc_stext   TYPE stext.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1000> TYPE gty_hrp1000.


  "Inicializa retorno
  CLEAR:
    pvc_stext.

  "Valida que se tengan datos
  CHECK NOT pvi_otjid IS INITIAL.

  "Obtiene la descripcion del objeto
  READ TABLE pti_hrp1000 ASSIGNING <fs_hrp1000>
    WITH KEY otjid = pvi_otjid
    BINARY SEARCH.

  "Valida que se encuentre un registro
  CHECK sy-subrc EQ 0.

  "Asigna la descripcion
  pvc_stext = <fs_hrp1000>-stext.

ENDFORM.
