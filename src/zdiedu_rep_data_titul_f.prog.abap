*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_REP_DATA_TITUL_F
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_SH_OTJID
*&---------------------------------------------------------------------*
FORM f_sh_otjid
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
        AND endda EQ cl_hrpiq00const=>c_date_highdate.

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
      "Si la busqueda es estudiante
      IF pvi_otype EQ cl_hrpiq00const=>c_otype_st.
        ls_dynpfields-fieldvalue = ls_hrp1000_sel-fieldval.

        "En caso contrario
      ELSE.
        CONCATENATE pvi_otype
                    ls_hrp1000_sel-fieldval
          INTO ls_dynpfields-fieldvalue
          RESPECTING BLANKS.
      ENDIF.
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
*&      Form  F_SH_MAT
*&---------------------------------------------------------------------*
FORM f_sh_mat
  USING
    pvi_high    TYPE abap_bool.

  "Declaraciones
  DATA:
    ls_objec      LIKE objec,
    ls_dynpfields TYPE dynpread,
    lt_dynpfields TYPE dynpread_t.


  "Inicializa declaraciones
  CLEAR:
    ls_objec.

  "Ejecuta la ayuda de busqueda
  CALL FUNCTION 'HRIQ_OBJID_REQUEST'
    EXPORTING
      plvar           = '01'
      otype           = cl_hrpiq00const=>c_otype_st
    IMPORTING
      sel_object      = ls_objec
    EXCEPTIONS
      cancelled       = 1
      wrong_condition = 2
      nothing_found   = 3
      illegal_mode    = 4
      internal_error  = 5
      OTHERS          = 6.

  "Si ocurre algun error
  IF sy-subrc NE 0.
    "Genera un mensaje informando el error
    sy-msgty = 'S'.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    "Si no ocurre ningun error
  ELSE.
    "Inicializa la estructura de datos
    CLEAR ls_dynpfields.

    "Si el valor es un valor superior
    IF pvi_high EQ abap_true.
      "Determina el campo a modificar segun la pantalla
      CASE sy-dynnr.
        WHEN '1000'.
          ls_dynpfields-fieldname = 'SO_MAT-HIGH'.
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
          ls_dynpfields-fieldname = 'SO_MAT-LOW'.
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
    IF NOT ls_objec-short IS INITIAL.
      "Si la busqueda es plan de estudio
      ls_dynpfields-fieldvalue = ls_objec-short.
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
*&      Form  F_VALIDAR_CAMPOS
*&---------------------------------------------------------------------*
FORM f_validar_campos .

  "Si todos los parametros de estudiante no tienen datos
  IF so_bp    IS INITIAL AND
     so_st    IS INITIAL AND
     so_mat   IS INITIAL AND
     so_id    IS INITIAL.

    "Genera mensaje de error
    MESSAGE 'Se debe ingresar por lo menos un parámetro de estudiante' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
FORM f_obtener_datos
  CHANGING
    ptc_informe       TYPE gtyt_informe.

  "Declaraciones
  DATA:
    lt_tipo_doc     TYPE gtyt_tipo_doc,
    lt_data_general TYPE gtyt_data_general,
    lt_st_cs        TYPE gtyt_hrp1001,
    lt_hrpad530     TYPE gtyt_hrpad530,
    lt_cs_sc        TYPE gtyt_hrp1001,
    lt_hrp1000      TYPE gtyt_hrp1000,
    lt_hrp1002      TYPE gtyt_hrp1002,
    lt_hrt1002      TYPE gtyt_hrt1002.


  "Obtiene los datos generales
  PERFORM f_obtener_data_general
    CHANGING
      lt_tipo_doc
      lt_data_general.

  "Obtiene las relaciones ST - CS
  PERFORM f_obtener_hrp1001_ini
    USING
      cl_hrpiq00const=>c_relat_530a
      lt_data_general
      so_cs[]
    CHANGING
      lt_st_cs.

  "Obtiene los registros de los segmentos
  PERFORM f_obtener_hrpad530
    USING
      lt_st_cs
    CHANGING
      lt_hrpad530.

  "Filtra los segmentos
  PERFORM f_filtrar_hrpad530
    USING
      lt_hrpad530
    CHANGING
      lt_st_cs.

  "Obtiene las relaciones CS - SC
  PERFORM f_obtener_hrp1001_sec_rev
    USING
      cl_hrpiq00const=>c_relat_514a
      lt_st_cs
      so_sc[]
    CHANGING
      lt_cs_sc.

  "Filtra los registros de la relacion ST - CS
  PERFORM f_filtrar_hrp1001
    USING
      lt_cs_sc
    CHANGING
      lt_st_cs.

  "Obtiene las descripciones de los diversos objetos
  PERFORM f_obtener_hrp1000
    USING
      lt_cs_sc
    CHANGING
      lt_hrp1000.

  "Obtiene los enlaces a los nombres de los titulos
  PERFORM f_obtener_hrp1002
    USING
      lt_cs_sc
    CHANGING
      lt_hrp1002.

  "Obtiene los nombres de los titulos
  PERFORM f_obtener_hrt1002
    USING
      lt_hrp1002
    CHANGING
      lt_hrt1002.

  "Ordena los datos consultados
  PERFORM f_ordenar_datos
    CHANGING
      lt_st_cs
      lt_hrpad530
      lt_cs_sc
      lt_tipo_doc
      lt_data_general
      lt_hrp1000
      lt_hrp1002
      lt_hrt1002.

  "Agrupa los datos
  PERFORM f_agrupar_datos
    USING
      lt_st_cs
      lt_hrpad530
      lt_cs_sc
      lt_tipo_doc
      lt_data_general
      lt_hrp1000
      lt_hrp1002
      lt_hrt1002
    CHANGING
      ptc_informe.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRPAD530
*&---------------------------------------------------------------------*
FORM f_obtener_hrpad530
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrpad530  TYPE gtyt_hrpad530.

  "Declaraciones
  DATA:
    lc_cursor   TYPE cursor,
    lv_cantidad TYPE i,
    lv_ultimo   TYPE abap_bool,
    ls_adatanr  TYPE gty_adatanr,
    lt_hrp1001  TYPE gtyt_hrp1001,
    lt_adatanr  TYPE gtyt_adatanr.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001>  TYPE gty_hrp1001.


  "Inicializa el retorno
  CLEAR:
    ptc_hrpad530.

  "Crea una copia de los datos
  lt_hrp1001 = pti_hrp1001.

  "Ordena y elimina los registros duplicados
  SORT lt_hrp1001 BY adatanr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1001
    COMPARING adatanr.

  "Recorre los registros
  LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Asigna los datos a consultar y crea el registro
    ls_adatanr-adatanr = <fs_hrp1001>-adatanr.
    APPEND ls_adatanr TO lt_adatanr.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Si la cantidad de registros llega al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo EQ abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Define la seleccion de datos
      "Haciendo uso de la clave primaria
      SELECT adatanr adm_ayear adm_perid
        FROM hrpad530
        FOR ALL ENTRIES IN lt_adatanr
        WHERE adatanr EQ lt_adatanr-adatanr.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor APPENDING TABLE ptc_hrpad530
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
        lt_adatanr.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrpad530 BY adatanr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILTRAR_HRPAD530
*&---------------------------------------------------------------------*
FORM f_filtrar_hrpad530
  USING
    pti_hrpad530  TYPE gtyt_hrpad530
  CHANGING
    ptc_hrp1001   TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lv_tabix  TYPE sy-tabix.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001>  TYPE gty_hrp1001.


  "Recorre los registros
  LOOP AT ptc_hrp1001 ASSIGNING <fs_hrp1001>.
    "Almacena el indice del registro
    lv_tabix = sy-tabix.

    "Valida si tiene registro en la tabla HRPAD506
    READ TABLE pti_hrpad530 TRANSPORTING NO FIELDS
      WITH KEY adatanr = <fs_hrp1001>-adatanr
      BINARY SEARCH.

    "Si no encuentra el registro
    IF sy-subrc NE 0.
      "Elimina el registro
      DELETE ptc_hrp1001 INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1001_INI
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1001_ini
  USING
    pvi_subty         TYPE subtyp
    pti_data_general  TYPE gtyt_data_general
    pri_varif         TYPE gtyt_r_otjid
  CHANGING
    ptc_hrp1001 TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lc_cursor       TYPE cursor,
    lv_cantidad     TYPE i,
    lv_ultimo       TYPE abap_bool,
    ls_otjid        TYPE gty_otjid,
    lt_data_general TYPE gtyt_data_general,
    lt_hrp1001_blq  TYPE gtyt_hrp1001,
    lt_otjid        TYPE gtyt_otjid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_data_general>  TYPE gty_data_general.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp1001.

  "Crea una copia de los datos
  lt_data_general = pti_data_general.

  "Ordena y elimina los registros duplicados
  SORT lt_data_general BY stobjid.
  DELETE ADJACENT DUPLICATES FROM lt_data_general
    COMPARING stobjid.

  "Recorre los registros
  LOOP AT lt_data_general ASSIGNING <fs_data_general>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Asigna los datos a consultar y crea el registro
    ls_otjid-otjid+0(2) = cl_hrpiq00const=>c_otype_st.
    ls_otjid-otjid+2(8) = <fs_data_general>-stobjid.
    APPEND ls_otjid TO lt_otjid.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Si la cantidad de registros llega al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo EQ abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Define la seleccion de datos
      "Haciendo uso parcial del indice estandar 6 - OTJID/SUBTY/VARYF/PLVAR/ENDDA
      SELECT otype objid plvar rsign relat istat priox begda endda
             varyf seqnr infty otjid subty sclas sobid adatanr
        FROM hrp1001
        FOR ALL ENTRIES IN lt_otjid
        WHERE otjid EQ lt_otjid-otjid
          AND subty EQ pvi_subty
          AND varyf IN pri_varif
          AND plvar EQ '01'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp1001_blq
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros invalidos
        DELETE lt_hrp1001_blq
          WHERE sobid IS INITIAL
            OR  sobid EQ ''
            OR  sobid EQ '00000000'.

        "Adiciona los registros validos al retorno
        APPEND LINES OF lt_hrp1001_blq TO ptc_hrp1001.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_otjid.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp1001 BY otjid varyf.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1001_SEC_REV
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1001_sec_rev
  USING
    pvi_subty   TYPE subtyp
    pti_hrp1001 TYPE gtyt_hrp1001
    pri_varif   TYPE gtyt_r_otjid
  CHANGING
    ptc_hrp1001 TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_otjid       TYPE gty_otjid,
    lt_hrp1001     TYPE gtyt_hrp1001,
    lt_hrp1001_blq TYPE gtyt_hrp1001,
    lt_otjid       TYPE gtyt_otjid.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001>  TYPE gty_hrp1001.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp1001.

  "Crea una copia de los datos
  lt_hrp1001 = pti_hrp1001.

  "Ordena y elimina los registros duplicados
  SORT lt_hrp1001 BY varyf.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1001
    COMPARING varyf.

  "Recorre los registros
  LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Asigna los datos a consultar y crea el registro
    ls_otjid-otjid = <fs_hrp1001>-varyf.
    APPEND ls_otjid TO lt_otjid.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Si la cantidad de registros llega al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo EQ abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Define la seleccion de datos
      "Haciendo uso parcial del indice estandar 6 - OTJID/SUBTY/VARYF/PLVAR/ENDDA
      SELECT otype objid plvar rsign relat istat priox begda endda
             varyf seqnr infty otjid subty sclas sobid adatanr
        FROM hrp1001
        FOR ALL ENTRIES IN lt_otjid
        WHERE otjid EQ lt_otjid-otjid
          AND subty EQ pvi_subty
          AND varyf IN pri_varif
          AND plvar EQ '01'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp1001_blq
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros invalidos
        DELETE lt_hrp1001_blq
          WHERE sobid IS INITIAL
            OR  sobid EQ ''
            OR  sobid EQ '00000000'.

        "Adiciona los registros validos al retorno
        APPEND LINES OF lt_hrp1001_blq TO ptc_hrp1001.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_otjid.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp1001 BY otjid varyf.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FILTRAR_HRP1001
*&---------------------------------------------------------------------*
FORM f_filtrar_hrp1001
  USING
    pti_hrp1001   TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1001   TYPE gtyt_hrp1001.

  "Declaraciones
  DATA:
    lv_tabix TYPE sy-tabix.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001> TYPE gty_hrp1001.


  "Recorre los registros
  LOOP AT ptc_hrp1001 ASSIGNING <fs_hrp1001>.
    "Almacena el indice del registro
    lv_tabix = sy-tabix.

    "Valida si el registro va a ser tratado
    READ TABLE pti_hrp1001 TRANSPORTING NO FIELDS
      WITH KEY otjid = <fs_hrp1001>-varyf
      BINARY SEARCH.

    "Si no encuentra registro
    IF sy-subrc NE 0.
      "Elimina el registro
      DELETE ptc_hrp1001 INDEX lv_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATA_GENERAL
*&---------------------------------------------------------------------*
FORM f_obtener_data_general
  CHANGING
    ptc_tipo_doc      TYPE gtyt_tipo_doc
    ptc_data_general  TYPE gtyt_data_general.

  "Declaraciones
  DATA:
    lc_cursor       TYPE cursor,
    lr_tipoid       TYPE RANGE OF bu_id_type,
    ls_tb039a       TYPE gty_tb039a,
    ls_tipoid       LIKE LINE OF lr_tipoid,
    lt_tb039a       TYPE gtyt_tb039a,
    lt_data_general TYPE gtyt_data_general.


  "Obtiene todos los tipos de identificacion validos
  SELECT type
    INTO TABLE lt_tb039a
    FROM tb039a
    WHERE type LIKE 'F%'.

  "Indica que los registros son incluyentes
  ls_tipoid-sign   = 'I'.
  ls_tipoid-option = 'EQ'.

  "Recorre los registros
  LOOP AT lt_tb039a INTO ls_tb039a.
    ls_tipoid-low = ls_tb039a-type.
    APPEND ls_tipoid TO lr_tipoid.
  ENDLOOP.

  "Obtiene las descripciones de los tipos de identificacion validos
  SELECT category text
    INTO TABLE ptc_tipo_doc
    FROM tb039t
    WHERE langu EQ 'S'
      AND category IN lr_tipoid.

  "Abre el cursor
  OPEN CURSOR @lc_cursor FOR

"Define la busqueda para obtiene los datos generales
  SELECT bu~type, bu~idnumber, bu~institute, bu~partner, cm~stobjid, cm~student12,
         bt~name_last, bt~name_first, bt~name_lst2, bt~namemiddle
    FROM but0id AS bu
      LEFT OUTER JOIN but000 AS bt ON bu~partner EQ bt~partner
      LEFT OUTER JOIN cmacbpst AS cm ON bt~partner EQ cm~partner
    WHERE bu~idnumber      IN @so_id
      AND cm~student12     IN @so_mat
      AND bu~partner       IN @so_bp
      AND cm~stobjid       IN @so_st
      AND bu~type          IN @lr_tipoid
      AND bu~valid_date_to EQ '99991231'.

  "Mientras se tengan datos
  DO.
    "Obtiene el siguiente bloque de datos
    FETCH NEXT CURSOR @lc_cursor INTO @lt_data_general
      PACKAGE SIZE @p_bloque.

    "Si se tienen datos
    IF sy-subrc EQ 0.
      "Elimina los registros que no tengan
      DELETE lt_data_general
        WHERE partner   IS INITIAL
          OR  partner   EQ '0000000000'
          OR  stobjid   IS INITIAL
          OR  stobjid   EQ '00000000'
          OR  student12 IS INITIAL
          OR  student12 EQ '000000000000'.

      "Asigna los registros validos al retorno
      APPEND LINES OF lt_data_general TO ptc_data_general.

      "Si no se tienen datos
    ELSE.
      "Termina la busqueda
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1000
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1000
  USING
    pti_cs_sc     TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1000   TYPE gtyt_hrp1000.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_otjid       TYPE gty_otjid,
    lt_otjid_tmp   TYPE gtyt_otjid,
    lt_otjid_blq   TYPE gtyt_otjid,
    lt_hrp1000_blq TYPE gtyt_hrp1000.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001> TYPE gty_hrp1001,
    <fs_otjid>   TYPE gty_otjid.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp1000.

  "Recorre las relaciones CS - SC
  LOOP AT pti_cs_sc ASSIGNING <fs_hrp1001>.
    "Asigna los datos del SC "Plan de estudio" y crea el registro
    ls_otjid-otjid = <fs_hrp1001>-varyf.
    APPEND ls_otjid TO lt_otjid_tmp.
  ENDLOOP.

  "Ordena los registros y elimina los duplicados
  SORT lt_otjid_tmp BY otjid.
  DELETE ADJACENT DUPLICATES FROM lt_otjid_tmp
    COMPARING otjid.

  "Recorre los registros
  LOOP AT lt_otjid_tmp ASSIGNING <fs_otjid>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Crea el registro a consultar
    APPEND <fs_otjid> TO lt_otjid_blq.

    "Si la cantidad de registros llega al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo EQ abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Define la seleccion de datos
      "Haciendo uso parcial del indice estandar 1 - OTJID/PLVAR/MANDT
      SELECT plvar otype objid istat begda
             endda langu seqnr otjid stext
        FROM hrp1000
        FOR ALL ENTRIES IN lt_otjid_blq
        WHERE otjid EQ lt_otjid_blq-otjid
          AND plvar EQ '01'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp1000_blq
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros invalidos
        DELETE lt_hrp1000_blq
          WHERE langu NE sy-langu.

        "Adiciona los registros validos al retorno
        APPEND LINES OF lt_hrp1000_blq TO ptc_hrp1000.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_otjid_blq.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp1000 BY otjid endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRP1002
*&---------------------------------------------------------------------*
FORM f_obtener_hrp1002
  USING
    pti_cs_sc     TYPE gtyt_hrp1001
  CHANGING
    ptc_hrp1002   TYPE gtyt_hrp1002.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_objid       TYPE gty_objid,
    lt_objid_tmp   TYPE gtyt_objid,
    lt_objid_blq   TYPE gtyt_objid,
    lt_hrp1002_blq TYPE gtyt_hrp1002.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1001> TYPE gty_hrp1001,
    <fs_objid>   TYPE gty_objid.


  "Inicializa el retorno
  CLEAR:
    ptc_hrp1002.

  "Recorre las relaciones CS - SC
  LOOP AT pti_cs_sc ASSIGNING <fs_hrp1001>.
    "Asigna los datos del SC "Plan de estudio" y crea el registro
    ls_objid-objid = <fs_hrp1001>-sobid.
    APPEND ls_objid TO lt_objid_tmp.
  ENDLOOP.

  "Ordena los registros y elimina los duplicados
  SORT lt_objid_tmp BY objid.
  DELETE ADJACENT DUPLICATES FROM lt_objid_tmp
    COMPARING objid.

  "Recorre los registros
  LOOP AT lt_objid_tmp ASSIGNING <fs_objid>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Crea el registro a consultar
    APPEND <fs_objid> TO lt_objid_blq.

    "Si la cantidad de registros llega al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo EQ abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Define la seleccion de datos
      "Haciendo uso parcial de la clave primaria
      SELECT plvar otype objid subty istat begda endda
             langu filler seqnr otjid tabnr
        FROM hrp1002
        FOR ALL ENTRIES IN lt_objid_blq
        WHERE plvar EQ '01'
          AND otype EQ cl_hrpiq00const=>c_otype_sc
          AND objid EQ lt_objid_blq-objid
          AND subty EQ '0003'.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor INTO TABLE lt_hrp1002_blq
          PACKAGE SIZE p_bloque.

        "Si no se tienen mas bloques
        IF sy-subrc NE 0.
          "Cierra el cursor y sale
          CLOSE CURSOR lc_cursor.
          EXIT.
        ENDIF.

        "Elimina los registros invalidos
        DELETE lt_hrp1002_blq
          WHERE langu NE 'S'.

        "Adiciona los registros validos al retorno
        APPEND LINES OF lt_hrp1002_blq TO ptc_hrp1002.
      ENDDO.

      "Inicializa declaraciones
      CLEAR:
        lv_cantidad,
        lt_objid_blq.
    ENDIF.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_hrp1002 BY otjid endda DESCENDING begda DESCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_HRT1002
*&---------------------------------------------------------------------*
FORM f_obtener_hrt1002
  USING
    pti_hrp1002   TYPE gtyt_hrp1002
  CHANGING
    ptc_hrt1002   TYPE gtyt_hrt1002.

  "Declaraciones
  DATA:
    lc_cursor      TYPE cursor,
    lv_cantidad    TYPE i,
    lv_ultimo      TYPE abap_bool,
    ls_tabnr       TYPE gty_tabnr,
    lt_hrp1002_tmp TYPE gtyt_hrp1002,
    lt_tabnr       TYPE gtyt_tabnr.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1002> TYPE gty_hrp1002.


  "Inicializa retorno
  CLEAR:
    ptc_hrt1002.

  "Crea una copia de los datos
  lt_hrp1002_tmp = pti_hrp1002.

  "Ordena los registros y elimina los duplicados
  SORT lt_hrp1002_tmp BY tabnr.
  DELETE ADJACENT DUPLICATES FROM lt_hrp1002_tmp
    COMPARING tabnr.

  "Recorre los registros
  LOOP AT lt_hrp1002_tmp ASSIGNING <fs_hrp1002>.
    "Si es el ultimo registro
    AT LAST.
      "Indica que es el ultimo registro
      lv_ultimo = abap_true.
    ENDAT.

    "Aumenta la cantidad de registros
    ADD 1 TO lv_cantidad.

    "Asigna los datos y crea el registro
    ls_tabnr-tabnr = <fs_hrp1002>-tabnr.
    APPEND ls_tabnr TO lt_tabnr.

    "Si la cantidad de registros es mayor o igual al bloque o es el ultimo registro
    IF lv_cantidad GE p_bloque OR lv_ultimo = abap_true.
      "Abre el cursor
      OPEN CURSOR lc_cursor FOR

      "Establece la seleccion de datos de la tabla HRP1001
      "haciendo uso parcial de la clave primaria de la tabla
      SELECT tabnr tabseqnr tline
        FROM hrt1002
        FOR ALL ENTRIES IN lt_tabnr
        WHERE tabnr EQ lt_tabnr-tabnr.

      "Mientras se tengan datos
      DO.
        "Obtiene el bloque de datos
        FETCH NEXT CURSOR lc_cursor APPENDING TABLE ptc_hrt1002
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

  "Ordena los registros de retorno
  SORT ptc_hrt1002 BY tabnr tabseqnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ORDENAR_DATOS
*&---------------------------------------------------------------------*
FORM f_ordenar_datos
  CHANGING
    ptc_st_cs        TYPE gtyt_hrp1001
    ptc_hrpad530     TYPE gtyt_hrpad530
    ptc_cs_sc        TYPE gtyt_hrp1001
    ptc_tipo_doc     TYPE gtyt_tipo_doc
    ptc_data_general TYPE gtyt_data_general
    ptc_hrp1000      TYPE gtyt_hrp1000
    ptc_hrp1002      TYPE gtyt_hrp1002
    ptc_hrt1002      TYPE gtyt_hrt1002.


  "Realiza el ordenamiento de los registros para su posterior agrupacion
  SORT ptc_st_cs        BY objid varyf.
  SORT ptc_hrpad530     BY adatanr.
  SORT ptc_cs_sc        BY objid varyf.
  SORT ptc_tipo_doc     BY category.
  SORT ptc_data_general BY stobjid.
  SORT ptc_hrp1000      BY otjid.
  SORT ptc_hrp1002      BY otjid endda DESCENDING begda DESCENDING.
  SORT ptc_hrt1002      BY tabnr tabseqnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_DATOS
*&---------------------------------------------------------------------*
FORM f_agrupar_datos
  USING
    pti_st_cs         TYPE gtyt_hrp1001
    pti_hrpad530      TYPE gtyt_hrpad530
    pti_cs_sc         TYPE gtyt_hrp1001
    pti_tipo_doc      TYPE gtyt_tipo_doc
    pti_data_general  TYPE gtyt_data_general
    pti_hrp1000       TYPE gtyt_hrp1000
    pti_hrp1002       TYPE gtyt_hrp1002
    pti_hrt1002       TYPE gtyt_hrt1002
  CHANGING
    ptc_informe       TYPE gtyt_informe.

  "Declaraciones
  DATA:
    ls_informe      TYPE gty_informe.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_st_cs>        TYPE gty_hrp1001,
    <fs_hrpad530>     TYPE gty_hrpad530,
    <fs_hrp1001>      TYPE gty_hrp1001,
    <fs_tipo_doc>     TYPE gty_tipo_doc,
    <fs_data_general> TYPE gty_data_general,
    <fs_hrp1002>      TYPE gty_hrp1002,
    <fs_hrt1002>      TYPE gty_hrt1002.


  "Inicializa retorno
  CLEAR:
    ptc_informe.

  "Recorre los registros de los estudios a procesar
  LOOP AT pti_st_cs ASSIGNING <fs_st_cs>.
    "Inicializa las declaraciones
    CLEAR:
      ls_informe.

    "Asigna el código del estudio y del estudiante
    ls_informe-csobjid = <fs_st_cs>-sobid.
    ls_informe-stobjid = <fs_st_cs>-objid.

    "Obtiene los datos del segmento
    READ TABLE pti_hrpad530 ASSIGNING <fs_hrpad530>
      WITH KEY adatanr = <fs_st_cs>-adatanr
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Arma el periodo
      CONCATENATE <fs_hrpad530>-adm_ayear
                  <fs_hrpad530>-adm_perid+1(2)
        INTO ls_informe-insc_per.
    ENDIF.

    "Obtiene el Plan de Estudio
    READ TABLE pti_cs_sc ASSIGNING <fs_hrp1001>
      WITH KEY otjid = <fs_st_cs>-varyf
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna el codigo del Plan de Estudios
      ls_informe-scobjid = <fs_hrp1001>-sobid.
      "Asigna la descripcion
      PERFORM f_asignar_hrp1000
        USING
          <fs_hrp1001>-varyf
          pti_hrp1000
        CHANGING
          ls_informe-sc_stext.

      "Asigna el nombre del titulo
      PERFORM f_asignar_nombre_titulo
        USING
          <fs_hrp1001>-varyf
          pti_hrp1002
          pti_hrt1002
        CHANGING
          ls_informe-titulo.
    ENDIF.

    "Obtiene los datos generales del estudiante
    READ TABLE pti_data_general ASSIGNING <fs_data_general>
      WITH KEY stobjid = ls_informe-stobjid
      BINARY SEARCH.

    "Si encuentra el registro
    IF sy-subrc EQ 0.
      "Asigna los datos generales del estudiante
      ls_informe-student12  = <fs_data_general>-student12.
      ls_informe-partner    = <fs_data_general>-partner.
      ls_informe-taxtype    = <fs_data_general>-taxtype.
      ls_informe-idnumber   = <fs_data_general>-idnumber.
      ls_informe-lug_exp    = <fs_data_general>-institute.

      "Arma el nombre
      CONCATENATE <fs_data_general>-name_first
                  <fs_data_general>-namemiddle
        INTO ls_informe-nombre_comp
        SEPARATED BY space.
      "Elimina espacios al inicio y al final
      CONDENSE ls_informe-nombre_comp.
      "Adiciona los apellidos
      CONCATENATE ls_informe-nombre_comp
                  <fs_data_general>-name_last
                  <fs_data_general>-name_lst2
        INTO ls_informe-nombre_comp
        SEPARATED BY space.
      "Elimina espacios al inicio y al final
      CONDENSE ls_informe-nombre_comp.

      "Obtiene la descripcion del tipo de documento
      READ TABLE pti_tipo_doc ASSIGNING <fs_tipo_doc>
      WITH KEY category = <fs_data_general>-taxtype
      BINARY SEARCH.

      "Si encuentra el registro
      IF sy-subrc EQ 0.
        "Asigna la descripcion del tipo de documento
        ls_informe-taxtype_text = <fs_tipo_doc>-text.
      ENDIF.
    ENDIF.

    "Crea el registro para la anulacion de asignatura
    APPEND ls_informe TO ptc_informe.
  ENDLOOP.

  "Ordena los registros
  SORT ptc_informe BY stobjid scobjid.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_HRP1000
*&---------------------------------------------------------------------*
FORM f_asignar_hrp1000
  USING
    pvi_otjid     TYPE otjid
    pti_hrp1000   TYPE gtyt_hrp1000
  CHANGING
    pvc_stext     TYPE stext.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1000> TYPE gty_hrp1000.


  "Valida que se consulte un dato valido
  CHECK NOT pvi_otjid IS INITIAL.

  "Consulta la descripcion
  READ TABLE pti_hrp1000 ASSIGNING <fs_hrp1000>
    WITH KEY otjid = pvi_otjid
    BINARY SEARCH.

  "Continua solo si encuentra el registro
  CHECK sy-subrc EQ 0.

  "Asigna la descripcion encontrada
  pvc_stext = <fs_hrp1000>-stext.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ASIGNAR_NOMBRE_TITULO
*&---------------------------------------------------------------------*
FORM f_asignar_nombre_titulo
  USING
    pvi_otjid     TYPE otjid
    pti_hrp1002   TYPE gtyt_hrp1002
    pti_hrt1002   TYPE gtyt_hrt1002
  CHANGING
    pvc_titulo    TYPE ztitulo.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_hrp1002> TYPE gty_hrp1002,
    <fs_hrt1002> TYPE gty_hrt1002.


  "Valida que se consulte un dato valido
  CHECK NOT pvi_otjid IS INITIAL.

  "Consulta el enlace con el nombre del titulo
  READ TABLE pti_hrp1002 ASSIGNING <fs_hrp1002>
    WITH KEY otjid = pvi_otjid
    BINARY SEARCH.

  "Continua solo si encuentra el registro
  CHECK sy-subrc EQ 0.

  "Consulta el nombre del titulo
  READ TABLE pti_hrt1002 ASSIGNING <fs_hrt1002>
    WITH KEY tabnr = <fs_hrp1002>-tabnr
    BINARY SEARCH.

  "Continua solo si encuentra el registro
  CHECK sy-subrc EQ 0.

  "Asigna el nombre del titulo
  pvc_titulo = <fs_hrt1002>-tline.

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
  lv_titulo = 'Reporte - Generación data para desatrazo titulaciones'(t01).

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
  gr_column ?= gr_columns->get_column( 'STOBJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Estudiante' ).
  gr_column->set_medium_text( 'Código Estudiante' ).
  gr_column->set_short_text( 'Código ST' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'CSOBJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Estudio' ).
  gr_column->set_medium_text( 'Código Estudio' ).
  gr_column->set_short_text( 'Código CS' ).
  "Establece la columna como hotspot
  gr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'STUDENT12' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Matrícula' ).
  gr_column->set_medium_text( 'Matrícula' ).
  gr_column->set_short_text( 'Matrícula' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TAXTYPE' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Clase ID' ).
  gr_column->set_medium_text( 'Cod. Clase ID' ).
  gr_column->set_short_text( 'C.Clase ID' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TAXTYPE_TEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Clase ID' ).
  gr_column->set_medium_text( 'Clase ID' ).
  gr_column->set_short_text( 'Clase ID' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'IDNUMBER' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Nº identificación' ).
  gr_column->set_medium_text( 'Nº identificación' ).
  gr_column->set_short_text( 'Número ID' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'LUG_EXP' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Lugar de emisión ID' ).
  gr_column->set_medium_text( 'Lugar de emisión ID' ).
  gr_column->set_short_text( 'Lugar E.ID' ).


  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PARTNER' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Interlocutor Comercial' ).
  gr_column->set_medium_text( 'Interloc.cial.' ).
  gr_column->set_short_text( 'BP' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'NOMBRE_COMP' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Nombre Completo' ).
  gr_column->set_medium_text( 'Nombre Completo' ).
  gr_column->set_short_text( 'Nombre C.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SCOBJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Plan Estudio' ).
  gr_column->set_medium_text( 'Código Plan Estudio' ).
  gr_column->set_short_text( 'Código SC' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SC_STEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Nombre Plan Estudio' ).
  gr_column->set_medium_text( 'Nombre Plan Estudio' ).
  gr_column->set_short_text( 'Nombre SC' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TITULO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Titulo' ).
  gr_column->set_medium_text( 'Titulo' ).
  gr_column->set_short_text( 'Titulo' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'INSC_PER' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Periodo Inscripción' ).
  gr_column->set_medium_text( 'Periodo Inscripción' ).
  gr_column->set_short_text( 'Per.Inscri' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PROCESAR' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Procesar' ).
  gr_column->set_medium_text( 'Procesar' ).
  gr_column->set_short_text( 'Procesar' ).
  "Establece la columna como checkbox hotspot
  gr_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'RESULT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Resultado' ).
  gr_column->set_medium_text( 'Resultado' ).
  gr_column->set_short_text( 'Resultado' ).
  "Establece la columna como icono
  gr_column->set_icon( abap_true ).

ENDFORM.                    " F_CONFIGURAR_COLUMNAS

*&---------------------------------------------------------------------*
*&      Form  F_TOP_PAGE
*&---------------------------------------------------------------------*
FORM f_top_page .

  "Declaracion
  DATA:
    lv_titulo TYPE lvc_title.


  "Asigna el titulo
  lv_titulo = 'Reporte - Generación data para desatrazo titulaciones'(t01).

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

* Habilita el manejo del evento link clic
  SET HANDLER event_handler->on_link_click   FOR gr_events.


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

***        lr_selections = gr_table->get_selections( ).        " Obtiene la referencia de la linea seleccionada en el ALV
***        lt_rows = lr_selections->get_selected_rows( ).      " Obtiene la fila seleccionada en el ALV
****       Recorre los registros seleccionados en el ALV
***        LOOP AT lt_rows INTO ls_rows.
***          read table gt_datos into gs_datos index ls_rows.  " Obtiene los datos del registro seleccionado en el ALV
***        ENDLOOP.

*   Verifica el comando que ha ejecutado el usuario
    CASE e_salv_function.
      WHEN '&ZALL'. "Marcar para procesar
        "Marca todos los registros no procesados
        PERFORM f_marcar_desmarcar
          USING
            abap_true.

      WHEN '&ZSAL'. "Desmarcar de procesar
        "Marca todos los registros no procesados
        PERFORM f_marcar_desmarcar
          USING
            abap_false.

      WHEN '&ZTXT'. "Generar archivo plano
        "Genera el archivo plano
        PERFORM f_generar_txt.
    ENDCASE.

    "Refresca el contenido en el ALV
    PERFORM f_refrescar_alv.

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

* Metodo para el link clic
  METHOD on_link_click.
*   Variables propias del metodo:
*     row     = Numero de fila en la cual se ha hecho doble clic
*     column  = Nombre de la columna sobre la cual se ha hecho doble clic

    "Define el tipo de accion segun el campo del doble clic
    CASE column.
      WHEN 'PROCESAR'. "Para el campo Procesar
        "Obtiene el Registro sobre el cual se hizo doble click
        READ TABLE gt_informe ASSIGNING <gfs_informe> INDEX row.
        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Si esta marcado para procesar
          IF <gfs_informe>-procesar EQ abap_true.
            "Retira la marca
            CLEAR <gfs_informe>-procesar .

            "En caso contrario
          ELSE.
            "Asigna la marca
            <gfs_informe>-procesar = abap_true.
          ENDIF.
        ENDIF.

      WHEN 'CSOBJID'. "Para el campo Estudio
        "Obtiene el Registro sobre el cual se hizo doble click
        READ TABLE gt_informe ASSIGNING <gfs_informe> INDEX row.
        "Si encuentra el registro
        IF sy-subrc EQ 0.
          "Asigna los valores del parameter id para las funcionalidades estandar
          SET PARAMETER ID 'STUDENT'  FIELD <gfs_informe>-stobjid.
          SET PARAMETER ID 'PON'      FIELD <gfs_informe>-scobjid.

          "Asigna los valores del EXPORT TO MEMORY ID para el enhacement
          "NOTA: Enhacement en include LHRPIQ00STUDENTENTRYF01 - subrutina fill_listbox
          "      Se hico el enhacement en ese punto ya que hacerlo en la subrutina display_st_prog_data
          "      implicaria modificar el valor de una variable que esta como "USING"
          "      Nombre de la Implementación ampliación: ZHRIQ_STUDENT_ENTRY
          gs_save_st00-st_objid = <gfs_informe>-stobjid.
          gs_save_st00-sc_objid = <gfs_informe>-scobjid.
          EXPORT gs_save_st00-st_objid FROM gs_save_st00-st_objid TO MEMORY ID 'ZSTUDENT2'.
          EXPORT gs_save_st00-sc_objid FROM gs_save_st00-sc_objid TO MEMORY ID 'ZPON2'.

          "Lanza la visualizacion del expediente para el programa seleccionado
          "en la pestaña "Inscripción" y en modo visualización
          CALL FUNCTION 'HRIQ_STUDENT_ENTRY'
            EXPORTING
              plvar              = '01'
              stobjid            = <gfs_informe>-stobjid
              iv_extended_mode   = ''
              iv_default_program = <gfs_informe>-scobjid
              iv_default_tab     = 'REGIST'.
        ENDIF.
    ENDCASE.

    "Refresca el contenido en el ALV
    PERFORM f_refrescar_alv.

  ENDMETHOD.                    "on_link_click

ENDCLASS. "lcl_handle_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  F_REFRESCAR_ALV
*&---------------------------------------------------------------------*
FORM f_refrescar_alv.

  gr_table->refresh( ).

ENDFORM.                    "F_REFFRESCAR_ALV

*&---------------------------------------------------------------------*
*&      Form  F_MARCAR_DESMARCAR
*&---------------------------------------------------------------------*
FORM f_marcar_desmarcar
  USING
    pvi_procesar  TYPE abap_bool.


  "Recorre los registros
  LOOP AT gt_informe ASSIGNING <gfs_informe>.
    "Asigna o borra la marca para procesar
    <gfs_informe>-procesar = pvi_procesar.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GENERAR_TXT
*&---------------------------------------------------------------------*
FORM f_generar_txt .

  "Declaraciones
  DATA:
    lv_procesado TYPE abap_bool,
    lv_linea     TYPE string,
    lr_ref       TYPE REF TO cx_root.


  "Abre el archivo de retorno
  OPEN DATASET p_arch FOR OUTPUT IN LEGACY TEXT MODE.

  "Si no puede abrir el archivo
  IF sy-subrc NE 0.
    "Genera mensaje y sale
    MESSAGE 'Error al crear el archivo' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "Crea el registro de encabezado
  CONCATENATE 'ID_OBJ_ESTUDIANTE'
              'ID_ESTUDIO'
              'ID_PLAN_ESTUDIO'
              'NUMERO_MATRICULA'
              'NOMBRE_COMPLETO'
              'TIPO_DOC_IDENTIDAD'
              'NUMERO_DOC_IDENTIDAD'
              'LUGAR_EXPEDICION'
              'APODERADO'
              'FECHA_FIN_SEGMENTO'
              'TITULO'
              'ACTO'
              'ACTA'
              'LIBRO'
              'FOLIO'
              'DIPLOMA'
              'RESOLUCION'
              'FECHA_ACTO'
              'PERIODO_INSCRIPCION'
              'PUESTO_COHORTE'
              'PUESTO_ACTO'
              'ESTUD_COHO_GRAD'
              'ESTUDIANTES_COHO'
              'ESTUDIANTES_ACTO'
              'PROMED'
              'NOASISTIO'
    INTO lv_linea
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  "Crea el registro en el retorno
  TRANSFER lv_linea TO p_arch.

  "Recorre los registros
  LOOP AT gt_informe ASSIGNING <gfs_informe>.
    "Inicializa declaraciones
    CLEAR:
      <gfs_informe>-result,
      lv_linea.

    "Valida que este marcado para procesar
    CHECK <gfs_informe>-procesar EQ abap_true.

    "Genera la linea del archivo
    CONCATENATE <gfs_informe>-stobjid
                <gfs_informe>-csobjid
                <gfs_informe>-scobjid
                <gfs_informe>-student12
                <gfs_informe>-nombre_comp
                <gfs_informe>-taxtype_text
                <gfs_informe>-idnumber
                <gfs_informe>-lug_exp
                '' "apoderado
                '' "fecha fin segmento
                <gfs_informe>-titulo
                '' "acto
                '' "acta
                '' "libro
                '' "folio
                '' "diploma
                '' "resolucion
                '' "fecha acto
                <gfs_informe>-insc_per
                '' "puesto_cohorte
                '' "puesto_acto
                '' "estud_coho_grad
                '' "estudiantes_coho
                '' "estudiantes_acto
                '' "promed
                '' "noasistio
      INTO lv_linea
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    "Se intenta generar la linea en el archivo
    TRY .
        "Crea el registro en el retorno
        TRANSFER lv_linea TO p_arch.

        "Indica que se proceso el registro
        <gfs_informe>-result = gc_icon_gl.
        lv_procesado = abap_true.

        "Obtiene la excepcion generada
      CATCH cx_root INTO lr_ref.
        "Indica que ocurrio un error
        <gfs_informe>-result = gc_icon_rl.
    ENDTRY.
  ENDLOOP.

  "Cierra el archivo de retorno
  CLOSE DATASET p_arch.

  "Si se proceso algun registro
  IF lv_procesado EQ abap_true.
    "Genera mensaje
    MESSAGE 'Se generó el archivo' TYPE 'I' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.
