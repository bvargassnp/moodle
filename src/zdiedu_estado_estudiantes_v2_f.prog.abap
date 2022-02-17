*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_ESTADO_ESTUDIANTES_V2_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SH_OTJID
*&---------------------------------------------------------------------*
FORM f_sh_objid
  USING
    pvi_high    TYPE abap_bool
    pvi_otype   TYPE otype
    pvi_title   TYPE ddtext.

  "Declaraciones
  DATA:
    ls_hrp1000_sel TYPE ddshretval,
    ls_dynpfields  TYPE dynpread,
    lt_hrp1000_sh  TYPE gtyt_hrp1000_sh,
    lt_hrp1000_sel TYPE comt_ddshretval_tab,
    lt_dynpfields  TYPE dynpread_t.


  "Obtiene los registros para la ayuda de busqueda
  SELECT objid short stext
    INTO CORRESPONDING FIELDS OF TABLE lt_hrp1000_sh
      FROM hrp1000
      WHERE plvar EQ '01'
        AND otype EQ pvi_otype
        AND endda EQ cl_hrpiq00const=>c_date_highdate
        AND langu EQ sy-langu.

  "Genera el recuadro con la ayuda de busqueda
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OBJID'
      window_title    = pvi_title
      value_org       = 'S'
    TABLES
      value_tab       = lt_hrp1000_sh
      return_tab      = lt_hrp1000_sel
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  "Si ocurre algun error
  IF sy-subrc NE 0.
    "Genera un mensaje informando el error
    sy-msgty = 'S'.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    "Si no ocurre ningun error
  ELSE.
    "Obtiene el valor retornado
    READ TABLE lt_hrp1000_sel INTO ls_hrp1000_sel INDEX 1.

    "Continua solo si tiene el dato
    CHECK sy-subrc EQ 0.

    "Inicializa la estructura de datos
    CLEAR ls_dynpfields.

    "Si el valor es un valor superior
    IF pvi_high EQ abap_true.
      "Determina el campo a modificar segun la pantalla
      CASE sy-dynnr.
        WHEN '1000'.
          ls_dynpfields-fieldname = 'SO_' && pvi_otype && '-HIGH'.
        WHEN '3020'.
          ls_dynpfields-fieldname = 'RSCSEL_255-IHIGH_I'.
        WHEN '3040'.
          ls_dynpfields-fieldname = 'RSCSEL_255-IHIGH_E'.
      ENDCASE.

      "Si el valor es un valor inferior
    ELSE.
      "Determina el campo a modificar segun la pantalla
      CASE sy-dynnr.
        WHEN '1000'.
          ls_dynpfields-fieldname = 'SO_' && pvi_otype && '-LOW'.
        WHEN '3010'.
          ls_dynpfields-fieldname = 'RSCSEL_255-SLOW_I'.
        WHEN '3020'.
          ls_dynpfields-fieldname = 'RSCSEL_255-ILOW_I'.
        WHEN '3030'.
          ls_dynpfields-fieldname = 'RSCSEL_255-SLOW_E'.
        WHEN '3040'.
          ls_dynpfields-fieldname = 'RSCSEL_255-ILOW_E'.
      ENDCASE.
    ENDIF.

    "Asigna el valor seleccionado
    IF NOT ls_hrp1000_sel-fieldval IS INITIAL.
      ls_dynpfields-fieldvalue = ls_hrp1000_sel-fieldval.
    ENDIF.

    "Crea el registro del campo a cambiar
    APPEND ls_dynpfields TO lt_dynpfields.

    "Modifica el valor del campo
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = lt_dynpfields.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_VARIANTE
*&---------------------------------------------------------------------*
FORM f_buscar_variante .

  "Declaraciones
  DATA:
    lv_exit,
    ls_variant TYPE disvariant.


  "Asigna el nombre del programa
  ls_variant-report = sy-repid.

  "Obtiene las variantes grabadas para el programa
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = 'X'
    IMPORTING
      e_exit     = lv_exit
      es_variant = ls_variant.

  "Si encontro variantes
  IF sy-subrc EQ 0.
    "Si selecciono alguna variante
    IF lv_exit = space.
      "Asigna el nombre de la variante al parametro de entrada
      p_vari = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BUSCAR_VARIANTE

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
FORM f_obtener_datos
  CHANGING
    ptc_informe       TYPE gtyt_informe.

  "Declaraciones
  DATA:
    lt_progclasst TYPE gtyt_progclasst,
    lt_eps        TYPE gtyt_eps,
    lt_marstt     TYPE gtyt_marstt,
    lt_idtypet    TYPE gtyt_idtypet,
    lt_prog_acstt TYPE gtyt_prog_acstt,
    lt_smstatust  TYPE gtyt_smstatust,
    lt_slcm       TYPE zies_slcm_tab,
    lt_slcm_mat   TYPE zies_slcm_mat_tab,
    lt_slcm_sm    TYPE zies_slcm_sm_tab,
    lt_hrp1737    TYPE gtyt_hrp1737,
    lt_hrt1737_4  TYPE gtyt_hrt1737,
    lt_hrt1737_2  TYPE gtyt_hrt1737,
    lt_hsstatush  TYPE gtyt_hsstatush,
    lt_hrp1728    TYPE gtyt_hrp1728,
    lt_hrp9121    TYPE gtyt_hrp9121.

  "Obtiene las descripciones de la progresion
  PERFORM f_obtener_progclasst
    CHANGING
      lt_progclasst.

  "Obtiene las descripciones de las EPS
  PERFORM f_obtener_eps
    CHANGING
      lt_eps.

  "Obtiene las descripciones de los estados civiles
  PERFORM f_obtener_marstt
    CHANGING
      lt_marstt.

  "Obtiene las descripciones de los tipos de identificacion
  PERFORM f_obtener_idtypet
    CHANGING
      lt_idtypet.

  "Obtiene las descripciones de las categorias de trabajo academico
  PERFORM f_obtener_prog_acstt
    CHANGING
      lt_prog_acstt.

  "Obtiene las descripciones de los estados de las asignaturas
  PERFORM f_obtener_smstatust
    CHANGING
      lt_smstatust.

  "Filtra las categorias de tasas
  PERFORM f_filtrar_fcat.

  "Extrae los datos
  PERFORM f_extraer_datos
    CHANGING
      lt_slcm
      lt_slcm_mat
      lt_slcm_sm.

  "Obtiene los datos de progresion
  PERFORM f_obtener_hrp1737
    USING
      lt_slcm
    CHANGING
      lt_hrp1737.

  "Obtiene los datos adicionales de progresion
  PERFORM f_obtener_hrt1737
    USING
      lt_hrp1737
    CHANGING
      lt_hrt1737_4
      lt_hrt1737_2.

  "Obtiene las clases indicadoras de bloqueo y sus descripciones
  PERFORM f_obtener_hsstatush
    CHANGING
      lt_hsstatush.

  "Obtiene los bloqueos de los estudios
  PERFORM f_obtener_hrp1728
    USING
      lt_slcm
      lt_hsstatush
    CHANGING
      lt_hrp1728.

  "Obtiene los datos de EPS
  PERFORM f_obtener_hrp9121
    USING
      lt_slcm
    CHANGING
      lt_hrp9121.

  "Agrupa los datos
  PERFORM f_agrupar_datos
    USING
      lt_slcm
      lt_slcm_mat
      lt_hrp1737
      lt_hrt1737_4
      lt_hrt1737_2
      lt_progclasst
      lt_marstt
      lt_idtypet
      lt_prog_acstt
      lt_smstatust
      lt_slcm_sm
      lt_hsstatush
      lt_hrp1728
      lt_hrp9121
      lt_eps
    CHANGING
      ptc_informe.

*!Begin Of Soporte.
*! Incluir Ultimo correo Parametrizado
*  PERFORM get_last_mail
*    CHANGING
*      ptc_informe.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_LAST_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PTC_INFORME  text
*----------------------------------------------------------------------*
FORM get_last_mail    CHANGING
    ptc_informe    TYPE gtyt_informe.
*!Begin Of Soporte.
*! Incluir Ultimo correo Parametrizado
  TYPES: BEGIN OF ty_cmacbpst,
           partner TYPE bu_partner,
           stobjid TYPE piqstudent,
         END OF   ty_cmacbpst,
         BEGIN OF ty_but000,
           partner    TYPE bu_partner,
           persnumber TYPE ad_persnum,
         END OF ty_but000,
         BEGIN OF ty_adr6,
           persnumber TYPE ad_persnum,
           addrnumber TYPE ad_addrnum,
           consnumber TYPE ad_consnum,
           flgdefault	TYPE ad_flgdfad,
           flg_nouse  TYPE ad_flnouse,
           home_flag  TYPE ad_flghome,
           smtp_addr  TYPE ad_smtpadr,
         END OF ty_adr6.

  DATA:     it_cmacbpst TYPE STANDARD TABLE OF ty_cmacbpst,
            it_but000   TYPE STANDARD TABLE OF ty_but000,
            it_adr6     TYPE STANDARD TABLE OF ty_adr6,
            it_adr6_aux TYPE STANDARD TABLE OF ty_adr6,
            is_cmacbpst TYPE ty_cmacbpst,
            is_but000   TYPE ty_but000,
            is_adr6     TYPE ty_adr6.

  FIELD-SYMBOLS: <fs_ptc_informe> LIKE LINE OF ptc_informe.

  CHECK ptc_informe[] IS NOT INITIAL.

  SELECT partner stobjid
    FROM cmacbpst
    INTO CORRESPONDING FIELDS OF TABLE it_cmacbpst
    FOR ALL ENTRIES IN ptc_informe
    WHERE stobjid = ptc_informe-stobjid.

  CHECK it_cmacbpst[] IS NOT INITIAL.

  SELECT partner persnumber
    INTO CORRESPONDING FIELDS OF TABLE it_but000
    FROM but000
    FOR ALL ENTRIES IN it_cmacbpst
    WHERE partner = it_cmacbpst-partner.

  CHECK it_but000[] IS NOT INITIAL.

  SELECT persnumber addrnumber consnumber flgdefault flg_nouse home_flag smtp_addr
    INTO CORRESPONDING FIELDS OF TABLE it_adr6
    FROM adr6
    FOR ALL ENTRIES IN it_but000
    WHERE persnumber = it_but000-persnumber.

  SORT it_adr6 BY persnumber addrnumber DESCENDING .

  LOOP AT ptc_informe ASSIGNING <fs_ptc_informe>.
    it_adr6_aux[] = it_adr6[].
    READ TABLE it_cmacbpst INTO is_cmacbpst WITH KEY stobjid = <fs_ptc_informe>-stobjid.
    IF sy-subrc = 0.
      READ TABLE it_but000 INTO is_but000 WITH KEY partner = is_cmacbpst-partner.
      IF  sy-subrc = 0.
        DELETE it_adr6_aux[] WHERE  persnumber NE is_but000-persnumber.
        SORT it_adr6 BY persnumber addrnumber DESCENDING .
        READ TABLE it_adr6_aux[] INTO is_adr6 WITH KEY persnumber = is_but000-persnumber
                                                       home_flag  = 'X'.
        IF sy-subrc = 0.
          <fs_ptc_informe>-i05_smtp_addr = is_adr6-smtp_addr.
          DELETE it_adr6_aux[] WHERE  addrnumber NE is_adr6-addrnumber." AND
*          addrnumber = is_adr6-addrnumber AND
*          flgdefault = is_adr6-flgdefault AND
*          flg_nouse  = is_adr6-flg_nouse AND
*          home_flag  = is_adr6-home_flag AND
*          smtp_addr  = is_adr6-smtp_addr.
          SORT it_adr6_aux[] BY  consnumber ASCENDING .
          READ TABLE it_adr6_aux[] INTO is_adr6 WITH KEY persnumber = is_but000-persnumber
                                                         home_flag  = ''.
          IF sy-subrc = 0.
            <fs_ptc_informe>-i05_smtp_addr_aux = is_adr6-smtp_addr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PROGCLASST
*&---------------------------------------------------------------------*
FORM f_obtener_progclasst
  CHANGING
    ptc_progclasst  TYPE gtyt_progclasst.


  "Inicializa retorno
  CLEAR:
    ptc_progclasst.

  "Obtiene la totalidad de descripciones
  "Haciendo uso parcial de la clave primaria
  SELECT progclass progclasst
    INTO TABLE ptc_progclasst
    FROM t7piqprogclasst
    WHERE spras EQ sy-langu.

  "Ordena los registros
  SORT ptc_progclasst BY progclass.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_EPS
*&---------------------------------------------------------------------*
FORM f_obtener_eps
  CHANGING
    ptc_eps  TYPE gtyt_eps.


  "Inicializa retorno
  CLEAR:
    ptc_eps.

  "Obtiene la totalidad de descripciones
  SELECT codigo descripcion
    INTO TABLE ptc_eps
    FROM zedu_eps.

  "Ordena los registros
  SORT ptc_eps BY codigo.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_MARSTT
*&---------------------------------------------------------------------*
FORM f_obtener_marstt
  CHANGING
    ptc_marstt  TYPE gtyt_marstt.


  "Inicializa retorno
  CLEAR:
    ptc_marstt.

  "Obtiene la totalidad de descripciones
  "Haciendo uso parcial de la clave primaria
  SELECT marst bez20
    INTO TABLE ptc_marstt
    FROM tb027t
    WHERE spras EQ sy-langu.

  "Ordena los registros
  SORT ptc_marstt BY marst.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_IDTYPET
*&---------------------------------------------------------------------*
FORM f_obtener_idtypet
  CHANGING
    ptc_idtypet  TYPE gtyt_idtypet.


  "Inicializa retorno
  CLEAR:
    ptc_idtypet.

  "Obtiene la totalidad de descripciones
  "Haciendo uso parcial de la clave primaria
  SELECT category text
    INTO TABLE ptc_idtypet
    FROM tb039t
    WHERE langu EQ sy-langu.

  "Ordena los registros
  SORT ptc_idtypet BY category.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_PROG_ACSTT
*&---------------------------------------------------------------------*
FORM f_obtener_prog_acstt
  CHANGING
    ptc_prog_acstt  TYPE gtyt_prog_acstt.


  "Inicializa retorno
  CLEAR:
    ptc_prog_acstt.

  "Obtiene la totalidad de descripciones
  "Haciendo uso parcial de la clave primaria
  SELECT progc_var acst acst_txt
    INTO TABLE ptc_prog_acstt
    FROM t7piqprog_acstt
    WHERE spras EQ sy-langu.

  "Ordena los registros
  SORT ptc_prog_acstt BY progc_var acst.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_SMSTATUST
*&---------------------------------------------------------------------*
FORM f_obtener_smstatust
  CHANGING
    ptc_smstatust   TYPE gtyt_smstatust.


  "Inicializa retorno
  CLEAR:
    ptc_smstatust.

  "Obtiene la totalidad de descripciones
  "Haciendo uso parcial de la clave primaria
  SELECT smstatus smstatust
    INTO TABLE ptc_smstatust
    FROM t7piqsmstatt
    WHERE spras EQ sy-langu.

  "Ordena los registros
  SORT ptc_smstatust BY smstatus.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_EXTRAER_DATOS
*&---------------------------------------------------------------------*
FORM f_extraer_datos
  CHANGING
    ptc_slcm      TYPE zies_slcm_tab
    ptc_slcm_mat  TYPE zies_slcm_mat_tab
    ptc_slcm_sm   TYPE zies_slcm_sm_tab.

  "Declaraciones
  DATA:
    lv_begda     TYPE begda,
    lv_endda     TYPE endda,
    lv_condicion TYPE string.


  "Inicializa los retornos
  CLEAR:
    ptc_slcm,
    ptc_slcm_mat,
    ptc_slcm_sm.

  "Asigna la fecha inicial
  lv_begda = so_fecha-low.

  "Si no se tiene fecha final
  IF so_fecha-high IS INITIAL OR so_fecha-high EQ '00000000'.
    "Asigna la fecha inicial, como fecha final
    lv_endda = lv_begda.

    "Si se tiene fecha final
  ELSE.
    "Asigna la fecha final
    lv_endda = so_fecha-high.
  ENDIF.

  "Obiene los datos de la extraccion SLCM
  CALL FUNCTION 'Z_IES_EXTRACCION_SLCM'
    EXPORTING
      iv_bloque   = p_bloque
      iv_spool    = ' '
      iv_formal   = ' '
      iv_begda    = lv_begda
      iv_endda    = lv_endda
      ir_objid_sc = so_sc[]
    IMPORTING
      et_slcm     = ptc_slcm
      et_slcm_mat = ptc_slcm_mat
      et_slcm_sm  = ptc_slcm_sm.

  "Adiciona el filtro de matriculados unicamente
  lv_condicion = 'matriculado NE abap_true'.

  "Si se tiene filtro por BP
  IF NOT so_bp IS INITIAL.
    "Adiciona el filtro por BP
    CONCATENATE lv_condicion
                'OR NOT partner IN so_bp'
      INTO lv_condicion SEPARATED BY space.
  ENDIF.

  "Si se tiene filtro por numero de matricula
  IF NOT so_st12 IS INITIAL.
    "Adiciona el filtro por numero de matricula
    CONCATENATE lv_condicion
                'OR NOT student12 IN so_st12'
      INTO lv_condicion SEPARATED BY space.
  ENDIF.

  "Si se tiene filtro por tipo de identificacion
  IF NOT so_idty IS INITIAL.
    "Adiciona el filtro por tipo de identificacion
    CONCATENATE lv_condicion
                'OR NOT i01_type IN so_idty'
      INTO lv_condicion SEPARATED BY space.
  ENDIF.

  "Si se tiene filtro por numero de identificacion
  IF NOT so_idno IS INITIAL.
    "Adiciona el filtro por numero de identificacion
    CONCATENATE lv_condicion
                'OR NOT i01_idnumber IN so_idno'
      INTO lv_condicion SEPARATED BY space.
  ENDIF.

  "Elimina los registros que no hacen parte de la seleccion
  DELETE ptc_slcm
    WHERE (lv_condicion).

  "Ordena los registros
  SORT ptc_slcm BY stotjid   ASCENDING
                   csotjid   ASCENDING
                   scotjid   ASCENDING.

  SORT ptc_slcm_mat BY stotjid   ASCENDING
                       csotjid   ASCENDING
                       scotjid   ASCENDING
                       mat_ayear ASCENDING
                       mat_perid ASCENDING.

  SORT ptc_slcm_sm BY stotjid   ASCENDING
                      csotjid   ASCENDING
                      scotjid   ASCENDING
                      mat_ayear ASCENDING
                      mat_perid ASCENDING
                      smotjid   ASCENDING
                      seqnr     ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1737
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1737
  USING
    pti_slcm    TYPE zies_slcm_tab
  CHANGING
    ptc_hrp1737 TYPE gtyt_hrp1737.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_hrp1737_idx TYPE gty_hrp1737_idx,
    lt_hrp1737_idx TYPE gtyt_hrp1737_idx.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_slcm> TYPE zies_slcm.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp1737.

  "Recorre los registros
  LOOP AT pti_slcm ASSIGNING <fs_slcm>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Inicializa declaraciones
    CLEAR:
      ls_hrp1737_idx.

    "Aumenta la cantidad de registros
    ADD 2 TO lv_cantidad.

    "Asigna los datos y crea el registro para Clasificación del progreso
    ls_hrp1737_idx-prog_type = '4'.
    ls_hrp1737_idx-otype     = <fs_slcm>-stotjid+0(2).
    ls_hrp1737_idx-progc_var = <fs_slcm>-i10_progcvar.
    ls_hrp1737_idx-objid     = <fs_slcm>-stotjid+2(8).
    APPEND ls_hrp1737_idx TO lt_hrp1737_idx.

    "Asigna los datos y crea el registro para Categoría de trabajo académico
    ls_hrp1737_idx-prog_type = '2'.
    ls_hrp1737_idx-otype     = <fs_slcm>-stotjid+0(2).
    ls_hrp1737_idx-progc_var = <fs_slcm>-i10_progcvar.
    ls_hrp1737_idx-objid     = <fs_slcm>-stotjid+2(8).
    APPEND ls_hrp1737_idx TO lt_hrp1737_idx.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo = abap_true.
      "Intenta realizar la siguiente consulta
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos del estudio
      "Haciendo uso del indice estandar 3 - Indice para Hoja de vida del estudiante
      SELECT plvar otype objid subty istat begda endda
             varyf seqnr otjid progc_var prog_type tabnr
        FROM hrp1737
        FOR ALL ENTRIES IN lt_hrp1737_idx
        WHERE prog_type EQ lt_hrp1737_idx-prog_type
          AND otype     EQ lt_hrp1737_idx-otype
          AND progc_var EQ lt_hrp1737_idx-progc_var
          AND objid     EQ lt_hrp1737_idx-objid.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor APPENDING TABLE ptc_hrp1737
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_hrp1737_idx.
    ENDIF.
  ENDLOOP.

  "Elimina los registros invalidos
  DELETE ptc_hrp1737
    WHERE tabnr IS INITIAL.

  "Ordena los registros
  SORT ptc_hrp1737 BY otjid     ASCENDING
                      progc_var ASCENDING
                      prog_type ASCENDING
                      endda     DESCENDING
                      begda     DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRT1737
*&---------------------------------------------------------------------*
FORM f_obtener_hrt1737
  USING
    pti_hrp1737   TYPE gtyt_hrp1737
  CHANGING
    ptc_hrt1737_4 TYPE gtyt_hrt1737
    ptc_hrt1737_2 TYPE gtyt_hrt1737.

  "Declaraciones
  DATA:
    lc_cursor   TYPE cursor,
    lv_cantidad TYPE i,
    lv_ultimo   TYPE abap_bool,
    ls_tabnr    TYPE gty_tabnr,
    lt_hrp1737  TYPE gtyt_hrp1737,
    lt_tabnr    TYPE gtyt_tabnr.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1737> TYPE gty_hrp1737.


  "Inicializa el retorno
  CLEAR:
    ptc_hrt1737_4,
    ptc_hrt1737_2.

  "Crea una copia de los registros
  lt_hrp1737 = pti_hrp1737.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1737 BY tabnr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1737
    COMPARING tabnr.

  "Recorre los registros
  LOOP AT lt_hrp1737 ASSIGNING <fs_hrp1737>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Inicializa declaraciones
    CLEAR:
      ls_tabnr.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna los datos y crea el registro
    ls_tabnr-tabnr = <fs_hrp1737>-tabnr.
    APPEND ls_tabnr TO lt_tabnr.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo = abap_true.
      "Intenta realizar la siguiente consulta
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos del estudio
      "Haciendo uso parcial de la clave primaria de la tabla
      SELECT tabnr tabseqnr progc_var prog_type acst prcl peryr perid
        FROM hrt1737
        FOR ALL ENTRIES IN lt_tabnr
        WHERE tabnr EQ lt_tabnr-tabnr.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor APPENDING TABLE ptc_hrt1737_4
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_tabnr.
    ENDIF.
  ENDLOOP.

  "Crea una copia de los datos obtenidos
  ptc_hrt1737_2 = ptc_hrt1737_4.

  "Elimina los registros no validos
  DELETE ptc_hrt1737_4
    WHERE prog_type NE '4'.

  DELETE ptc_hrt1737_2
    WHERE prog_type NE '2'.

  "Ordena los registros
  SORT ptc_hrt1737_4 BY tabnr ASCENDING progc_var ASCENDING peryr ASCENDING perid ASCENDING.
  SORT ptc_hrt1737_2 BY tabnr ASCENDING progc_var ASCENDING tabseqnr DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HSSTATUSH
*&---------------------------------------------------------------------*
FORM f_obtener_hsstatush
  CHANGING
    ptc_hsstatush TYPE gtyt_hsstatush.


  "Inicializa retorno
  CLEAR:
    ptc_hsstatush.

  "Obtiene la totalidad de bloqueos posibles
  SELECT subty sutxt hs_otype
    INTO TABLE ptc_hsstatush
    FROM z_t7piqhsstatush
    WHERE hs_otype EQ cl_hrpiq00const=>c_otype_cs.

  "Ordena los registros
  SORT ptc_hsstatush BY subty.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1728
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1728
  USING
    pti_slcm      TYPE zies_slcm_tab
    pti_hsstatush TYPE gtyt_hsstatush
  CHANGING
    ptc_hrp1728   TYPE gtyt_hrp1728.

  "Declaraciones
  DATA:
    lc_cursor   TYPE cursor,
    lv_cantidad TYPE i,
    lv_ultimo   TYPE abap_bool,
    lr_subty    TYPE RANGE OF subtyp,
    lr_istat    TYPE RANGE OF istat_d,
    lr_varyf    TYPE RANGE OF varyf,
    lr_seqnr    TYPE RANGE OF seqnr,
    ls_r_subty  LIKE LINE OF lr_subty,
    ls_objid    TYPE gty_objid,
    lt_objid    TYPE gtyt_objid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hsstatush> TYPE gty_hsstatush,
    <fs_slcm>      TYPE zies_slcm.


  "Inicializa retorno
  CLEAR:
    ptc_hrp1728.

  "Continua solo si se tienen bloqueos
  CHECK NOT pti_hsstatush IS INITIAL.

  "Indica que los registros son incluyentes
  ls_r_subty-sign   = 'I'.
  ls_r_subty-option = 'EQ'.

  "Recorre los posibles bloqueos y arma un rango de los mismos
  LOOP AT pti_hsstatush ASSIGNING <fs_hsstatush>.
    "Asigna el bloqueo y crea el registro
    ls_r_subty-low = <fs_hsstatush>-subty.
    APPEND ls_r_subty TO lr_subty.
  ENDLOOP.

  "Recorre los registros
  LOOP AT pti_slcm ASSIGNING <fs_slcm>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna y crea el registro de consulta
    ls_objid-objid = <fs_slcm>-csotjid+2(8).
    APPEND ls_objid TO lt_objid.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo = abap_true.
      "Intenta realizar la siguiente consulta
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos de bloqueo del estudio
      "Haciendo uso de la clave primaria de la tabla mas un campo adicional
      SELECT plvar otype objid subty istat begda
             endda varyf seqnr otjid hs_state
        FROM hrp1728
        FOR ALL ENTRIES IN lt_objid
        WHERE plvar    EQ '01'
          AND otype    EQ cl_hrpiq00const=>c_otype_cs
          AND objid    EQ lt_objid-objid
          AND subty    IN lr_subty
          AND istat    IN lr_istat
          AND begda    LE sy-datum
          AND endda    GE sy-datum
          AND varyf    IN lr_varyf
          AND seqnr    IN lr_seqnr
          AND hs_state EQ 'A'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor APPENDING TABLE ptc_hrp1728
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_objid.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp1728 BY otjid ASCENDING endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_DATOS
*&---------------------------------------------------------------------*
FORM f_agrupar_datos
  USING
    pti_slcm       TYPE zies_slcm_tab
    pti_slcm_mat   TYPE zies_slcm_mat_tab
    pti_hrp1737    TYPE gtyt_hrp1737
    pti_hrt1737_4  TYPE gtyt_hrt1737
    pti_hrt1737_2  TYPE gtyt_hrt1737
    pti_progclasst TYPE gtyt_progclasst
    pti_marstt     TYPE gtyt_marstt
    pti_idtypet    TYPE gtyt_idtypet
    pti_prog_acstt TYPE gtyt_prog_acstt
    pti_smstatust  TYPE gtyt_smstatust
    pti_slcm_sm    TYPE zies_slcm_sm_tab
    pti_hsstatush  TYPE gtyt_hsstatush
    pti_hrp1728    TYPE gtyt_hrp1728
    pti_hrp9121    TYPE gtyt_hrp9121
    pti_eps        TYPE gtyt_eps
  CHANGING
    ptc_informe    TYPE gtyt_informe.

  "Declaraciones
  DATA:
    lv_valor_campo TYPE string,
    ls_informe     TYPE gty_informe.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_slcm>    TYPE zies_slcm,
    <fs_marstt>  TYPE gty_marstt,
    <fs_idtypet> TYPE gty_idtypet,
    <fs_hrp9121> TYPE gty_hrp9121,
    <fs_eps>     TYPE gty_eps.


  "Inicializa retorno
  CLEAR:
    ptc_informe.

  "Recorre los registros obtenidos
  LOOP AT pti_slcm ASSIGNING <fs_slcm>.
    "Inicializa la estructura de datos
    CLEAR:
      ls_informe.

    "Asigna los valores de los campos con el mismo nombre
    MOVE-CORRESPONDING <fs_slcm> TO ls_informe.

    "Mapea los valores que no poseen el mismo nombre o son calculados
    ls_informe-stobjid = <fs_slcm>-stotjid+2(8).  "ST
    ls_informe-csobjid = <fs_slcm>-csotjid+2(8).  "CS
    ls_informe-scobjid = <fs_slcm>-scotjid+2(8).  "SC

    "Obtiene la Descripcion Categoría de admisión
    CALL FUNCTION 'Z_IES_SLCM_ADM_CATEG_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Categoría de admisión
    ls_informe-i00_adm_categt = lv_valor_campo.

    "Obtiene la Descripcion Clase de oyente
    CALL FUNCTION 'Z_IES_SLCM_ENR_CATEG_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Clase de oyente
    ls_informe-i00_enrcategt = lv_valor_campo.

    "Determina el genero de la persona
    CASE abap_true.
      WHEN <fs_slcm>-i02_xsexf. "Mujer
        ls_informe-i02_genero = 'Mujer'.
      WHEN <fs_slcm>-i02_xsexm. "Hombre
        ls_informe-i02_genero = 'Hombre'.
    ENDCASE.

    "Obtiene la Descripcion Actividad inscripción
    CALL FUNCTION 'Z_IES_SLCM_BEG_PROCESS_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Actividad inscripción
    ls_informe-i11_beg_processt = lv_valor_campo.

    "Obtiene la Descripcion Motivo actividad inscripción
    CALL FUNCTION 'Z_IES_SLCM_BEG_REASON_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Motivo actividad inscripción
    ls_informe-i11_beg_reasontext = lv_valor_campo.

    "Obtiene la Descripcion Actividad Retiro
    CALL FUNCTION 'Z_IES_SLCM_END_PROCESS_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Actividad Retiro
    ls_informe-i11_end_processt = lv_valor_campo.

    "Obtiene la Descripcion Motivo actividad Retiro
    CALL FUNCTION 'Z_IES_SLCM_END_REASON_DESC'
      EXPORTING
        is_datos_h     = <fs_slcm>
      IMPORTING
        ev_valor_campo = lv_valor_campo.
    "Asigna la Descripcion Motivo actividad inscripción
    ls_informe-i11_end_reasontext = lv_valor_campo.

    "Asigna los bloqueos
    PERFORM f_asignar_bloqueos
      USING
        <fs_slcm>-csotjid
        pti_hrp1728
        pti_hsstatush
      CHANGING
        ls_informe-cs_bloqueos.

    "Asigna el codigo del pais del colegio
    lv_valor_campo = ls_informe-i09_pais_proc.
    "Obtiene el nombre del pais del colegio
    CALL FUNCTION 'Z_IES_CONV_PAIS'
      CHANGING
        cv_valor_campo = lv_valor_campo.
    "Asigna el nombre del pais del colegio
    ls_informe-i09_pais_proct = lv_valor_campo.

    "Obtiene el nombre del departamento del colegio
    CALL FUNCTION 'Z_IES_CONV_DPTO'
      EXPORTING
        iv_pais         = ls_informe-i09_pais_proc
        iv_departamento = ls_informe-i09_dpto_proc
      CHANGING
        cv_valor_campo  = lv_valor_campo.
    "Asigna el nombre del departamento del colegio
    ls_informe-i09_dpto_proct = lv_valor_campo.

    "Obtiene el nombre de la ciudad del colegio
    CALL FUNCTION 'Z_IES_CONV_CIUDAD'
      EXPORTING
        iv_pais         = ls_informe-i09_pais_proc
        iv_departamento = ls_informe-i09_dpto_proc
        iv_ciudad       = ls_informe-i09_ciudad
      CHANGING
        cv_valor_campo  = lv_valor_campo.
    "Asigna el nombre de la ciudad del colegio
    ls_informe-i09_ciudadt = lv_valor_campo.

    "Asigna el codigo del pais de la universidad
    lv_valor_campo = ls_informe-i09_pais_u.
    "Obtiene el nombre del pais de la universidad
    CALL FUNCTION 'Z_IES_CONV_PAIS'
      CHANGING
        cv_valor_campo = lv_valor_campo.
    "Asigna el nombre del pais de la universidad
    ls_informe-i09_pais_ut = lv_valor_campo.

    "Obtiene el nombre del departamento de la universidad
    CALL FUNCTION 'Z_IES_CONV_DPTO'
      EXPORTING
        iv_pais         = ls_informe-i09_pais_u
        iv_departamento = ls_informe-i09_region_u
      CHANGING
        cv_valor_campo  = lv_valor_campo.
    "Asigna el nombre del departamento de la universidad
    ls_informe-i09_region_ut = lv_valor_campo.

    "Obtiene el nombre de la ciudad de la universidad
    CALL FUNCTION 'Z_IES_CONV_CIUDAD'
      EXPORTING
        iv_pais         = ls_informe-i09_pais_u
        iv_departamento = ls_informe-i09_region_u
        iv_ciudad       = ls_informe-i09_ciudad_u
      CHANGING
        cv_valor_campo  = lv_valor_campo.
    "Asigna el nombre de la ciudad de la universidad
    ls_informe-i09_ciudad_ut = lv_valor_campo.

    "Obtiene la descripcion del estado civil
    READ TABLE pti_marstt ASSIGNING <fs_marstt>
      WITH KEY marst = ls_informe-i02_marst
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion del estado civil
      ls_informe-i02_marstt = <fs_marstt>-bez20.
    ENDIF.

    "Obtiene la descripcion del tipo de identificacion
    READ TABLE pti_idtypet ASSIGNING <fs_idtypet>
      WITH KEY category = ls_informe-i01_type
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna la descripcion del tipo de identificacion
      ls_informe-i01_typet = <fs_idtypet>-text.
    ENDIF.

    "Obtiene la EPS
    READ TABLE pti_hrp9121 ASSIGNING <fs_hrp9121>
      WITH KEY otjid = <fs_slcm>-stotjid
      BINARY SEARCH.

    "Si se encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna el codigo de la EPS.
      ls_informe-epscod = <fs_hrp9121>-epscod.

      "Obtiene el nombre de la eps
      READ TABLE pti_eps ASSIGNING <fs_eps>
        WITH KEY codigo = <fs_hrp9121>-epscod
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna el nombre de la EPS
        ls_informe-epstext = <fs_eps>-descripcion.
      ENDIF.
    ENDIF.

    "Recorre las matriculas del estudio CS
    PERFORM f_recorrer_matriculas
      USING
        <fs_slcm>-stotjid
        <fs_slcm>-csotjid
        <fs_slcm>-scotjid
        <fs_slcm>-i10_progcvar
        pti_slcm_mat
        pti_hrp1737
        pti_hrt1737_4
        pti_hrt1737_2
        pti_progclasst
        pti_prog_acstt
        pti_smstatust
        pti_slcm_sm
      CHANGING
        ls_informe
        ptc_informe.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_BLOQUEOS
*&---------------------------------------------------------------------*
FORM f_asignar_bloqueos
  USING
    pvi_csotjid     TYPE otjid
    pti_hrp1728     TYPE gtyt_hrp1728
    pti_hsstatush   TYPE gtyt_hsstatush
  CHANGING
    pvc_cs_bloqueos TYPE zies_csbloqueos_cod.

  "Declaraciones
  DATA:
    lv_tabix TYPE sy-tabix,
    lv_texto TYPE zies_csbloqueos_cod.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1728>   TYPE gty_hrp1728,
    <fs_hsstatush> TYPE gty_hsstatush.


  "Inicializa el retorno
  CLEAR:
    pvc_cs_bloqueos.

  "Valida si el estudio tiene algun bloqueo
  READ TABLE pti_hrp1728 TRANSPORTING NO FIELDS
    WITH KEY otjid = pvi_csotjid
    BINARY SEARCH.

  "Continua solo si encuentra el registro
  CHECK sy-subrc EQ 0.

  "Almacena el indice del regisrto
  lv_tabix = sy-tabix.

  "Recorre los bloqueos
  LOOP AT pti_hrp1728 ASSIGNING <fs_hrp1728> FROM lv_tabix.
    "Valida que el estudio sea el mismo consultado
    IF <fs_hrp1728>-otjid NE pvi_csotjid.
      "Deja de recorrer los bloqueos
      EXIT.
    ENDIF.

    "Inicializa declaracion
    CLEAR:
      lv_texto.

    "Obtiene la descripcion del bloqueo
    READ TABLE pti_hsstatush ASSIGNING <fs_hsstatush>
      WITH KEY subty = <fs_hrp1728>-subty
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna el codigo de bloqueo junto con su descripcion
      CONCATENATE <fs_hrp1728>-subty
                  <fs_hsstatush>-sutxt
        INTO lv_texto
        SEPARATED BY space.

      "Si no encuentra el registro
    ELSE.
      "Asigna unicamente el codigo del bloqueo
      lv_texto = <fs_hrp1728>-subty.

    ENDIF.

    "Si no se tienen bloqueos
    IF pvc_cs_bloqueos IS INITIAL.
      "Asigna el bloqueo
      pvc_cs_bloqueos = lv_texto.

      "Si ya se tienen bloqueos
    ELSE.
      "Adiciona el bloqueo al listado
      CONCATENATE pvc_cs_bloqueos
                  lv_texto
        INTO pvc_cs_bloqueos
        SEPARATED BY ' / '.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_RECORRER_MATRICULAS
*&---------------------------------------------------------------------*
FORM f_recorrer_matriculas
  USING
    pvi_stotjid    TYPE otjid
    pvi_csotjid    TYPE otjid
    pvi_scotjid    TYPE otjid
    pvi_progcvar   TYPE piqprogc_var
    pti_slcm_mat   TYPE zies_slcm_mat_tab
    pti_hrp1737    TYPE gtyt_hrp1737
    pti_hrt1737_4  TYPE gtyt_hrt1737
    pti_hrt1737_2  TYPE gtyt_hrt1737
    pti_progclasst TYPE gtyt_progclasst
    pti_prog_acstt TYPE gtyt_prog_acstt
    pti_smstatust  TYPE gtyt_smstatust
    pti_slcm_sm    TYPE zies_slcm_sm_tab
  CHANGING
    psc_informe    TYPE gty_informe
    ptc_informe    TYPE gtyt_informe.

  "Declaraciones
  DATA:
    lv_tabix    TYPE sy-tabix,
    lv_creditos TYPE i.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_slcm_mat>   TYPE zies_slcm_mat,
    <fs_hrp1737>    TYPE gty_hrp1737,
    <fs_hrt1737>    TYPE gty_hrt1737,
    <fs_progclasst> TYPE gty_progclasst,
    <fs_prog_acstt> TYPE gty_prog_acstt.


  "Obtiene el primer registro de matricula
  READ TABLE pti_slcm_mat TRANSPORTING NO FIELDS
    WITH KEY stotjid = pvi_stotjid
             csotjid = pvi_csotjid
             scotjid = pvi_scotjid
    BINARY SEARCH.

  "Continua solo si encuentra registro
  CHECK sy-subrc EQ 0.

  "Almacena el indice del registro encontrado
  lv_tabix = sy-tabix.

  "Recorre las matriculas encontradas
  LOOP AT pti_slcm_mat ASSIGNING <fs_slcm_mat> FROM lv_tabix.
    "Si el registro no pertenece al estudio consultado
    IF <fs_slcm_mat>-stotjid NE pvi_stotjid OR
       <fs_slcm_mat>-csotjid NE pvi_csotjid OR
       <fs_slcm_mat>-scotjid NE pvi_scotjid.
      "Deja de recorrer los registros
      EXIT.
    ENDIF.

    "Inicializa datos relativos a la matricula
    CLEAR:
      psc_informe-mat_ayear,
      psc_informe-mat_perid,
      psc_informe-acst,
      psc_informe-acst_txt,
      psc_informe-prcl,
      psc_informe-progclasst,
      psc_informe-mat_financiera_n,
      psc_informe-mat_persl,
      psc_informe-pago_de,
      psc_informe-pago_dr,
      psc_informe-pago_ce,
      psc_informe-pago_cr,
      psc_informe-pago_total,
      psc_informe-total_facturado,
      psc_informe-total_deuda,
      psc_informe-deuda_ar,
      psc_informe-deuda_ae,
      psc_informe-deuda_a_re,
      psc_informe-deuda_ao,
      psc_informe-recargo,
      psc_informe-descuento,
      psc_informe-descuento_cod,
      psc_informe-pp_finan,
      psc_informe-pp_finan_cod,
      psc_informe-subvencion,
      psc_informe-subvencion_cod,
      psc_informe-intereses,
      psc_informe-mat_prs_state,
      psc_informe-smobjid,
      psc_informe-smstext,
      psc_informe-seqnr,
      psc_informe-smstatus,
      psc_informe-smstatust,
      psc_informe-smpago.

    "Asigna los valores de los campos con el mismo nombre
    MOVE-CORRESPONDING <fs_slcm_mat> TO psc_informe.

    "Si tiene marca de pago financiero
    IF <fs_slcm_mat>-mat_financiera EQ abap_true.
      "Asigna 1 al campo marca financiera
      psc_informe-mat_financiera_n = '1'.

      "Si no tiene marga de pago financiero
    ELSE.
      "Asigna 0 al campo marca financiera
      psc_informe-mat_financiera_n = '0'.
    ENDIF.

    "Asigna la cantidad de creditos a una variable entera
    lv_creditos = <fs_slcm_mat>-sm_cpattemp.
    "Asigna el valor entero a la estructura de datos
    psc_informe-sm_cpattemp = lv_creditos.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pago_de
      IMPORTING
        bapi_amount = psc_informe-pago_de.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pago_dr
      IMPORTING
        bapi_amount = psc_informe-pago_dr.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pago_ce
      IMPORTING
        bapi_amount = psc_informe-pago_ce.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pago_cr
      IMPORTING
        bapi_amount = psc_informe-pago_cr.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pago_total
      IMPORTING
        bapi_amount = psc_informe-pago_total.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-total_facturado
      IMPORTING
        bapi_amount = psc_informe-total_facturado.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-total_deuda
      IMPORTING
        bapi_amount = psc_informe-total_deuda.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-deuda_ar
      IMPORTING
        bapi_amount = psc_informe-deuda_ar.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-deuda_ae
      IMPORTING
        bapi_amount = psc_informe-deuda_ae.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-deuda_a_re
      IMPORTING
        bapi_amount = psc_informe-deuda_a_re.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-deuda_ao
      IMPORTING
        bapi_amount = psc_informe-deuda_ao.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-recargo
      IMPORTING
        bapi_amount = psc_informe-recargo.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-descuento
      IMPORTING
        bapi_amount = psc_informe-descuento.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-pp_finan
      IMPORTING
        bapi_amount = psc_informe-pp_finan.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-subvencion
      IMPORTING
        bapi_amount = psc_informe-subvencion.

    "Convierte el valor segun la moneda COP
    CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_BAPI'
      EXPORTING
        currency    = 'COP'
        sap_amount  = psc_informe-intereses
      IMPORTING
        bapi_amount = psc_informe-intereses.

    "Obtiene el registro de progresión
    READ TABLE pti_hrp1737 ASSIGNING <fs_hrp1737>
      WITH KEY otjid = pvi_stotjid
               progc_var = pvi_progcvar
               prog_type = '4'
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Obtiene el registro de datos adicionales de progresión
      READ TABLE pti_hrt1737_4 ASSIGNING <fs_hrt1737>
        WITH KEY tabnr     = <fs_hrp1737>-tabnr
                 progc_var = pvi_progcvar
                 peryr     = <fs_slcm_mat>-mat_ayear
                 perid     = <fs_slcm_mat>-mat_perid
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la progresion
        psc_informe-prcl = <fs_hrt1737>-prcl.

        "Obtiene la descripcion de la progresion
        READ TABLE pti_progclasst ASSIGNING <fs_progclasst>
          WITH KEY progclass = <fs_hrt1737>-prcl
          BINARY SEARCH.

        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Asigna la descripcion de la progresion
          psc_informe-progclasst = <fs_progclasst>-progclasst.
        ENDIF.
      ENDIF.
    ENDIF.

    "Obtiene el registro de Categoría de trabajo académico
    READ TABLE pti_hrp1737 ASSIGNING <fs_hrp1737>
      WITH KEY otjid = pvi_stotjid
               progc_var = pvi_progcvar
               prog_type = '2'
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Obtiene el registro de datos adicionales de progresión
      READ TABLE pti_hrt1737_2 ASSIGNING <fs_hrt1737>
        WITH KEY tabnr     = <fs_hrp1737>-tabnr
                 progc_var = pvi_progcvar
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la progresion
        psc_informe-acst = <fs_hrt1737>-acst.

        "Obtiene la descripcion de la categoria de trabajo academico
        READ TABLE pti_prog_acstt ASSIGNING <fs_prog_acstt>
          WITH KEY progc_var = <fs_hrt1737>-progc_var
                   acst      = <fs_hrt1737>-acst
          BINARY SEARCH.

        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Asigna la descripcion de la progresion
          psc_informe-acst_txt = <fs_prog_acstt>-acst_txt.
        ENDIF.
      ENDIF.
    ENDIF.

    "Si se solicita detalle de asignaturas
    IF pa_det EQ abap_true.
      "Recorre las asignaturas
      PERFORM f_recorrer_asignaturas
        USING
          pvi_stotjid
          pvi_csotjid
          pvi_scotjid
          <fs_slcm_mat>-mat_ayear
          <fs_slcm_mat>-mat_perid
          pti_smstatust
          pti_slcm_sm
        CHANGING
          psc_informe
          ptc_informe.

      "Si no se requiere detalle de asignaturas
    ELSE.
      "Crea el registro para el reporte
      APPEND psc_informe TO ptc_informe.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_RECORRER_ASIGNATURAS
*&---------------------------------------------------------------------*
FORM f_recorrer_asignaturas
  USING
    pvi_stotjid   TYPE otjid
    pvi_csotjid   TYPE otjid
    pvi_scotjid   TYPE otjid
    pvi_mat_ayear TYPE piqperyr
    pvi_mat_perid TYPE piqperid
    pti_smstatust TYPE gtyt_smstatust
    pti_slcm_sm   TYPE zies_slcm_sm_tab
  CHANGING
    psc_informe   TYPE gty_informe
    ptc_informe   TYPE gtyt_informe.

  "Declaraciones
  DATA:
    lv_tabix TYPE sy-tabix.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_slcm_sm>   TYPE zies_slcm_sm,
    <fs_smstatust> TYPE gty_smstatust.


  "Obtiene el primer registro de matricula
  READ TABLE pti_slcm_sm TRANSPORTING NO FIELDS
    WITH KEY stotjid   = pvi_stotjid
             csotjid   = pvi_csotjid
             scotjid   = pvi_scotjid
             mat_ayear = pvi_mat_ayear
             mat_perid = pvi_mat_perid
    BINARY SEARCH.

  "Si encuentra el registro
  IF sy-subrc EQ 0.
    "Almacena el indice del registro encontrado
    lv_tabix = sy-tabix.

    "Recorre las matriculas encontradas
    LOOP AT pti_slcm_sm ASSIGNING <fs_slcm_sm> FROM lv_tabix.
      "Si el registro no pertenece al estudio consultado
      IF <fs_slcm_sm>-stotjid   NE pvi_stotjid   OR
         <fs_slcm_sm>-csotjid   NE pvi_csotjid   OR
         <fs_slcm_sm>-scotjid   NE pvi_scotjid   OR
         <fs_slcm_sm>-mat_ayear NE pvi_mat_ayear OR
         <fs_slcm_sm>-mat_perid NE pvi_mat_perid.
        "Deja de recorrer los registros
        EXIT.
      ENDIF.

      "Inicializa datos relativos a la asignatura
      CLEAR:
        psc_informe-smobjid,
        psc_informe-seqnr,
        psc_informe-smstatus,
        psc_informe-smstatust,
        psc_informe-smstext,
        psc_informe-smpago.

      "Asigna los valores de los campos con el mismo nombre
      MOVE-CORRESPONDING <fs_slcm_sm> TO psc_informe.

      "Mapea los valores que no poseen el mismo nombre o son calculados
      psc_informe-smobjid = <fs_slcm_sm>-smotjid+2(8).  "SM
      psc_informe-smpago  = <fs_slcm_sm>-pago.          "Indicador de pago SM

      "Obtiene la descripcion del estado de la asignatura
      READ TABLE pti_smstatust ASSIGNING <fs_smstatust>
        WITH KEY smstatus = <fs_slcm_sm>-smstatus
        BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la descripcion del estado de la asignatura
        psc_informe-smstatust = <fs_smstatust>-smstatust.
      ENDIF.

      "Crea el registro para el reporte
      APPEND psc_informe TO ptc_informe.
    ENDLOOP.

    "Si no encuentra el registro
  ELSE.
    "Inicializa datos relativos a la asignatura
    CLEAR:
      psc_informe-smobjid,
      psc_informe-seqnr,
      psc_informe-smstatus,
      psc_informe-smstatust,
      psc_informe-smstext,
      psc_informe-smpago.

    "Crea el registro para el reporte
    APPEND psc_informe TO ptc_informe.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MOSTRAR_REPORTE
*&---------------------------------------------------------------------*
FORM f_mostrar_reporte
  USING
    pti_informe   TYPE gtyt_informe.


  "Establece la configuracion Visual del Reporte ALV
  PERFORM f_visual_alv
    USING
      pti_informe.

  "Establece la configuracion de Eventos del Reporte ALV
  PERFORM f_evento_alv.

* Llama el ALV
  gr_table->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VISUAL_ALV
*&---------------------------------------------------------------------*
FORM f_visual_alv
  USING
    pti_informe    TYPE gtyt_informe.

* Declaracion de field-symbols
  FIELD-SYMBOLS <tabla> TYPE ANY TABLE .   " Field-symbol para contener la tabla interna con los datos para el ALV

* Asigna la tabla interna con los datos al field-symbol que utiliza el ALV
  ASSIGN pti_informe[] TO <tabla>[].

  TRY.
      "Importa el objeto de referencia dentro de GR_TABLE y pasa la tabla interna con los datos para el ALV
      cl_salv_table=>factory( IMPORTING r_salv_table = gr_table
                              CHANGING  t_table = <tabla>[] ).
    CATCH cx_salv_msg.
  ENDTRY.

  "Configura la parte visual y de funcion del ALV en general
  PERFORM f_configurar_alv.

  "Configura los nombres de las columnas del ALV.
  PERFORM f_configurar_columnas.

  "Configura la parte visual de la parte superior del ALV
  PERFORM f_top_page.

ENDFORM.                    "F_VISUAL_ALV

*&---------------------------------------------------------------------*
*&      Form  F_CONFIGURAR_ALV
*&---------------------------------------------------------------------*
FORM f_configurar_alv .

  "Declaracion
  DATA:
    lv_titulo TYPE lvc_title.


  "Asigna el titulo
  lv_titulo = 'Reporte Estado de Estudiantes'(t01).

  "Establece todas las funciones estandar para el ALV toolbar
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).

  "Obtiene todas las columnas del ALV
  gr_columns = gr_table->get_columns( ).
  "Hace que las columnas sean del ancho optimo.
  gr_columns->set_optimize( 'X' ).

  "Habilita el manejo de Layouts en el ALV
  gr_layout = gr_table->get_layout( ).
  gs_key-report = sy-repid.
  gr_layout->set_key( gs_key ).
  gr_layout->set_default( 'X' ).
  gr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
  "Habilita el uso de un layout seleccionado por el usuario.
  IF p_vari IS NOT INITIAL.
    gr_layout->set_initial_layout( p_vari ).
  ENDIF.

  "Establece el titulo del programa
  gr_display = gr_table->get_display_settings( ).
  gr_display->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_display->set_list_header( lv_titulo ).

ENDFORM.                    " F_CONFIGURAR_ALV

*&---------------------------------------------------------------------*
*&      Form  F_CONFIGURAR_COLUMNAS
*&---------------------------------------------------------------------*
FORM f_configurar_columnas .

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'MAT_FINANCIERA_N' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Matrícula Financiera' ).
  gr_column->set_medium_text( 'Mat.Financiera' ).
  gr_column->set_short_text( 'Mat.Financ' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I02_GENERO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Sexo' ).
  gr_column->set_medium_text( 'Sexo' ).
  gr_column->set_short_text( 'Sexo' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I07_SOCIAL' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Estrato' ).
  gr_column->set_medium_text( 'Estrato' ).
  gr_column->set_short_text( 'Estrato' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'EPSTEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'EPS' ).
  gr_column->set_medium_text( 'EPS' ).
  gr_column->set_short_text( 'EPS' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_SNP' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Registro SNP' ).
  gr_column->set_medium_text( 'Registro SNP' ).
  gr_column->set_short_text( 'Reg. SNP' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_COLEGIO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Colegio' ).
  gr_column->set_medium_text( 'Colegio' ).
  gr_column->set_short_text( 'Colegio' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_TITULO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Título Colegio' ).
  gr_column->set_medium_text( 'Título Colegio' ).
  gr_column->set_short_text( 'Título Col' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_ANO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año Colegio' ).
  gr_column->set_medium_text( 'Año Colegio' ).
  gr_column->set_short_text( 'Año Col.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_PAIS_PROC' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Pais Colegio' ).
  gr_column->set_medium_text( 'Cod. Pais Colegio' ).
  gr_column->set_short_text( 'C.Pais Col' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_PAIS_PROCT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Pais Colegio' ).
  gr_column->set_medium_text( 'Pais Colegio' ).
  gr_column->set_short_text( 'Pais Col.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_DPTO_PROC' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Dpto. Colegio' ).
  gr_column->set_medium_text( 'Cod. Dpto. Colegio' ).
  gr_column->set_short_text( 'C.Dpto.Col' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_DPTO_PROCT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Dpto Colegio' ).
  gr_column->set_medium_text( 'Dpto Colegio' ).
  gr_column->set_short_text( 'Dpto Col.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_CIUDAD' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Ciudad Colegio' ).
  gr_column->set_medium_text( 'Cod. Ciudad Colegio' ).
  gr_column->set_short_text( 'C.Ciud.Col' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_CIUDADT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Ciudad Colegio' ).
  gr_column->set_medium_text( 'Ciudad Colegio' ).
  gr_column->set_short_text( 'Ciudad Col' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_UNIVERSIDAD' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Universidad' ).
  gr_column->set_medium_text( 'Universidad' ).
  gr_column->set_short_text( 'Univer.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_TITULO_U' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Título Universidad' ).
  gr_column->set_medium_text( 'Título Universidad' ).
  gr_column->set_short_text( 'Título U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_ANO_U' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año Universidad' ).
  gr_column->set_medium_text( 'Año Universidad' ).
  gr_column->set_short_text( 'Año U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_PAIS_U' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Pais Universidad' ).
  gr_column->set_medium_text( 'Cod.Pais Universidad' ).
  gr_column->set_short_text( 'C.Pais U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_PAIS_UT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Pais Universidad' ).
  gr_column->set_medium_text( 'Pais Universidad' ).
  gr_column->set_short_text( 'Pais U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_REGION_U' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Dpto. Universidad' ).
  gr_column->set_medium_text( 'Cod.Dpto.Universidad' ).
  gr_column->set_short_text( 'C.Dpto.U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_REGION_UT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Dpto Universidad' ).
  gr_column->set_medium_text( 'Dpto Universidad' ).
  gr_column->set_short_text( 'Dpto U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_CIUDAD_U' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Ciudad Universidad' ).
  gr_column->set_medium_text( 'Cod.Ciudad Univer.' ).
  gr_column->set_short_text( 'C.Ciud.U.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'I09_CIUDAD_UT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Ciudad Universidad' ).
  gr_column->set_medium_text( 'Ciudad Universidad' ).
  gr_column->set_short_text( 'Ciudad U.' ).

  "Si la ejecucion es sin detalle de asignaturas
  IF pa_det IS INITIAL.
    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SMOBJID' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).

    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SMSTEXT' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).

    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SEQNR' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).

    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SMSTATUS' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).

    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SMSTATUST' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).

    "Obtiene la referencia de la columna
    gr_column ?= gr_columns->get_column( 'SMPAGO' ).
    "Establece la columna como no visible
    gr_column->set_visible( abap_false ).
  ENDIF.

ENDFORM.                    " F_CONFIGURAR_COLUMNAS

*&---------------------------------------------------------------------*
*&      Form  F_TOP_PAGE
*&---------------------------------------------------------------------*
FORM f_top_page .

  "Declaracion
  DATA:
    lv_titulo TYPE lvc_title.


  "Asigna el titulo
  lv_titulo = 'Reporte Estado de Estudiantes'(t01).

  "Habilita el titulo del reporte ALV (top of list)
  CREATE OBJECT gr_grid.
  gr_grid->create_header_information(
    row = 1
    column = 1
    text = lv_titulo
    tooltip = lv_titulo ).

  "llama el titulo del reporte ALV (top of list)
  gr_table->set_top_of_list( gr_grid ).

ENDFORM.                    " F_TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F_EVENTO_ALV
*&---------------------------------------------------------------------*
FORM f_evento_alv .

* Asigna un menú al reporte ALV
* Previo a esto es necesario ir al grupo de funciones "SALV_METADATA_STATUS" y copiar
* el gui status "SALV_TABLE_STANDARD" al programa que se esta creando.
  gr_table->set_screen_status(
    pfstatus = 'SALV_TABLE_STANDARD'
    report = sy-repid
    set_functions = gr_table->c_functions_all ).

* Habilita el manejo de eventos del reporte ALV
  gr_events = gr_table->get_event( ).
  CREATE OBJECT event_handler.
* Habilita el manejo del evento user_command
  SET HANDLER event_handler->on_user_command FOR gr_events.

* Habilita el manejo del evento doble clic
  SET HANDLER event_handler->on_double_click FOR gr_events.

* Obtiene el tipo de seleccion de registros en el reporte ALV
  gr_selections = gr_table->get_selections( ).
* Establece el tipo de seleccion de registros en el reporte ALV
  gr_selections->set_selection_mode( 3 ). " 1=Simple(toda la fila), 2=Multiple(toda la fila), 3=Simple(casilla individual + Columa seleccion),

ENDFORM.                    "F_EVENTO_ALV

*----------------------------------------------------------------------*
* CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

* Metodo para los comandos de usuario
  METHOD on_user_command.
*   Variable propia del metodo:
*     e_salv_function = Codigo del comando ejecutado.

*   Declaracion de variables y objetos del metodo
    DATA: lr_selections TYPE REF TO cl_salv_selections.
    DATA: lt_rows TYPE salv_t_row.
    DATA: ls_rows TYPE i.
    DATA: message TYPE string.

*   Verifica el comando que ha ejecutado el usuario
    CASE e_salv_function.
*     Ejecuta la nueva funcionalidad adicionada en el menu
      WHEN 'MIFUNCION'.
***        lr_selections = gr_table->get_selections( ).        " Obtiene la referencia de la linea seleccionada en el ALV
***        lt_rows = lr_selections->get_selected_rows( ).      " Obtiene la fila seleccionada en el ALV
****       Recorre los registros seleccionados en el ALV
***        LOOP AT lt_rows INTO ls_rows.
***          read table gt_datos into gs_datos index ls_rows.  " Obtiene los datos del registro seleccionado en el ALV
***        ENDLOOP.
    ENDCASE.
  ENDMETHOD. "on_user_command

* Metodo para el doble clic
  METHOD on_double_click.
*   Variables propias del metodo:
*     row     = Numero de fila en la cual se ha hecho doble clic
*     column  = Nombre de la columna sobre la cual se ha hecho doble clic

    "Define el tipo de accion segun el campo del doble clic
    CASE column.
        "Para el campo Resultado Reversion
      WHEN 'CAMPO'.
***        "Obtiene el Registro sobre el cual se hizo doble click
***        READ TABLE gt_informe INTO gs_informe INDEX row.
    ENDCASE.

    "Refresca el contenido en el ALV
    PERFORM f_refrescar_alv.

  ENDMETHOD.                    "on_double_click

ENDCLASS. "lcl_handle_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  F_REFRESCAR_ALV
*&---------------------------------------------------------------------*
FORM f_refrescar_alv.

  gr_table->refresh( ).

ENDFORM.                    "F_REFFRESCAR_ALV

*&---------------------------------------------------------------------*
*&      Form  F_FILTRAR_FCAT
*&---------------------------------------------------------------------*
FORM f_filtrar_fcat.

  "Declaraciones
  DATA:
    lv_objid TYPE hrobjid,
    lt_objid TYPE TABLE OF hrobjid.


  "Continua solo si se tienen datos
  CHECK NOT so_fcat IS INITIAL.

  "Borra el filtro que se tenga por programas
  CLEAR:
    so_sc.

  REFRESH:
    so_sc.

  "Obtiene los programas asociados a la categoria de tasas
  SELECT objid
    INTO TABLE lt_objid
    FROM hrp1732
    WHERE scfeecat IN so_fcat.

  "Ordena los registros
  SORT lt_objid.

  "Recorre los registros
  LOOP AT lt_objid INTO lv_objid.
    "Asigna los datos del plan de estudio
    so_sc-sign   = 'I'.
    so_sc-option = 'EQ'.
    so_sc-low    = lv_objid.
    "Crea el registro con el plan de estudios
    APPEND so_sc.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP9121
*&---------------------------------------------------------------------*
FORM f_obtener_hrp9121
  USING
    pti_slcm    TYPE zies_slcm_tab
  CHANGING
    ptc_hrp9121 TYPE gtyt_hrp9121.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    lt_objid       TYPE gtyt_objid,
    lt_hrp9121_blq TYPE gtyt_hrp9121,
    ls_objid       TYPE gty_objid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_slcm> TYPE zies_slcm.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp9121.

  "Recorre los registros
  LOOP AT pti_slcm ASSIGNING <fs_slcm>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Inicializa declaraciones
    CLEAR:
      ls_objid.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna los datos y crea el registro para la consulta
    ls_objid-objid = <fs_slcm>-stotjid+2(8).
    APPEND ls_objid TO lt_objid.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo = abap_true.
      "Intenta realizar la siguiente consulta
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos
      "Haciendo uso parcial de la clave primaria
      SELECT plvar otype objid subty istat begda endda
             varyf seqnr infty otjid epscod
        FROM hrp9121
        FOR ALL ENTRIES IN lt_objid
        WHERE plvar EQ '01'
          AND otype     EQ 'ST'
          AND objid     EQ lt_objid-objid.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp9121_blq
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros no validos
        DELETE lt_hrp9121_blq
          WHERE infty NE '9121'.

        "Adiciona los registros al retorno
        APPEND LINES OF lt_hrp9121_blq TO ptc_hrp9121.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_objid.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp9121 BY otjid     ASCENDING
                      endda     DESCENDING
                      begda     DESCENDING.

ENDFORM.
