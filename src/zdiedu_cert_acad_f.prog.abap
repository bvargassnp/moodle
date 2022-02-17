*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CERT_ACAD_F
*&---------------------------------------------------------------------*

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
      pa_vari = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BUSCAR_VARIANTE

*&---------------------------------------------------------------------*
*&      Form  F_OBTENER_DATOS
*&---------------------------------------------------------------------*
FORM f_obtener_datos
  CHANGING
    ptc_informe     TYPE gtyt_informe
    ptc_informe_pe  TYPE gtyt_informe_pe
    ptc_informe_pm  TYPE gtyt_informe_pm
    ptc_informe_sm  TYPE gtyt_informe_sm
    ptc_informe_na  TYPE gtyt_informe_na
    ptc_informe_ps  TYPE gtyt_informe_ps
    ptc_informe_ro  TYPE gtyt_informe_ro
    ptc_informe_gr  TYPE gtyt_informe_gr.

  "Declaraciones
  DATA:
    lt_informe_ro TYPE gtyt_informe_ro,
    lt_hrp1000    TYPE gtyt_hrp1000.

  "Field-Symbols
  FIELD-SYMBOLS:
    <fs_informe_ro> TYPE gty_informe_ro,
    <fs_hrp1000>    TYPE gty_hrp1000.


  "Inicializa retornos
  CLEAR:
    ptc_informe,
    ptc_informe_pe,
    ptc_informe_pm,
    ptc_informe_sm,
    ptc_informe_na,
    ptc_informe_ps,
    ptc_informe_ro,
    ptc_informe_gr.

  "Obtiene los datos de los reportes consultados
  "Haciendo uso de la clave primaria
  SELECT *
    INTO TABLE ptc_informe
    FROM zedu_certacad_h
    WHERE opbel IN so_opbel.

  "Obtiene los datos de los periodos excedencia
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_pe
    FROM zedu_certacad_pe
    WHERE opbel IN so_opbel.

  "Obtiene los datos de los periodos matriculados
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_pm
    FROM zedu_certacad_pm
    WHERE opbel IN so_opbel.

  "Obtiene los datos de las asignaturas
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_sm
    FROM zedu_certacad_sm
    WHERE opbel IN so_opbel.

  "Obtiene los datos de las notas academicas
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_na
    FROM zedu_certacad_na
    WHERE opbel IN so_opbel.

  "Obtiene los datos de los promedios semestre
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_ps
    FROM zedu_certacad_ps
    WHERE opbel IN so_opbel.

  "Obtiene los datos de las rotaciones
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_ro
    FROM zedu_certacad_ro
    WHERE opbel IN so_opbel.

  "Obtiene los datos de los datos de graduacion
  "Haciendo uso parcial de la clave primaria
  SELECT *
    INTO TABLE ptc_informe_gr
    FROM zedu_certacad_gr
    WHERE opbel IN so_opbel.

  "Crea una copia de los datos de rotacion
  lt_informe_ro = ptc_informe_ro.

  "Ordena los registros por recurso y elimina los duplicados
  SORT lt_informe_ro BY objid_rec.
  DELETE ADJACENT DUPLICATES FROM lt_informe_ro
    COMPARING objid_rec.

  "Elimina los registros invalidos
  DELETE lt_informe_ro
     WHERE objid_rec IS INITIAL
       OR  objid_rec EQ '00000000'
       OR  objid_rec EQ ''.

  "Si se tienen recursos
  IF NOT lt_informe_ro IS INITIAL.
    "Obtiene las descripciones de los recursos.
    "Haciendo uso parcial de la clave primaria de la tabla
    SELECT plvar otype objid istat begda
           endda langu seqnr stext
      INTO TABLE lt_hrp1000
      FROM hrp1000
      FOR ALL ENTRIES IN lt_informe_ro
      WHERE plvar EQ '01'
        AND otype EQ cl_hrpiq00const=>c_otype_r
        AND objid EQ lt_informe_ro-objid_rec.

    "Elimina las descripciones de otro idioma
    DELETE lt_hrp1000
      WHERE langu NE sy-langu.

    "Ordena los registros
    SORT lt_hrp1000 BY objid ASCENDING endda DESCENDING begda DESCENDING.
  ENDIF.

  "Si se tienen descripciones de recursos
  IF NOT lt_hrp1000 IS INITIAL.
    "Recorre los registros de rotacion
    LOOP AT ptc_informe_ro ASSIGNING <fs_informe_ro>.
      "Obtiene la descripcion del recurso
      READ TABLE lt_hrp1000 ASSIGNING <fs_hrp1000>
        WITH KEY objid = <fs_informe_ro>-objid_rec
        BINARY SEARCH.

      "Continua solo si encuentra el registro
      CHECK sy-subrc EQ 0.

      "Asigna la descripcion del recurso
      <fs_informe_ro>-stext_rec = <fs_hrp1000>-stext.
    ENDLOOP.
  ENDIF.

  "Ordena los registros
  SORT ptc_informe    BY opbel ASCENDING.

  SORT ptc_informe_pe BY opbel ASCENDING
                         endda DESCENDING
                         begda DESCENDING.

  SORT ptc_informe_pm BY opbel       ASCENDING
                         mat_endda   DESCENDING
                         mat_begda   DESCENDING
                         mat_regdate DESCENDING.

  SORT ptc_informe_sm BY opbel   ASCENDING
                         ayear   DESCENDING
                         perid   DESCENDING
                         smotjid ASCENDING
                         seqnr   DESCENDING.

  SORT ptc_informe_na BY opbel ASCENDING
                         seqnr ASCENDING.

  SORT ptc_informe_ps BY opbel ASCENDING
                         seqnr ASCENDING.

  SORT ptc_informe_ro BY opbel ASCENDING
                         seqnr ASCENDING.

  SORT ptc_informe_gr BY opbel ASCENDING
                         seqnr ASCENDING.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MOSTRAR_REPORTE
*&---------------------------------------------------------------------*
FORM f_mostrar_reporte.

  "Establece la configuracion Visual del Reporte ALV
  PERFORM f_visual_alv.

  "Establece la configuracion de Eventos del Reporte ALV
  PERFORM f_evento_alv.

* Llama el ALV
  gr_table->display( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_VISUAL_ALV
*&---------------------------------------------------------------------*
FORM f_visual_alv.

* Declaracion de field-symbols
  FIELD-SYMBOLS <tabla> TYPE ANY TABLE .   " Field-symbol para contener la tabla interna con los datos para el ALV

* Asigna la tabla interna con los datos al field-symbol que utiliza el ALV
  ASSIGN gt_informe[] TO <tabla>[].

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
  lv_titulo = 'Reporte Certificados Académicos'(t01).

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
  IF pa_vari IS NOT INITIAL.
    gr_layout->set_initial_layout( pa_vari ).
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
  gr_column ?= gr_columns->get_column( 'MANDT' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OPBEL' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Número de documento' ).
  gr_column->set_medium_text( 'Nº documento' ).
  gr_column->set_short_text( 'Nº doc.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'STOTJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Estudiante' ).
  gr_column->set_medium_text( 'Cód. Estudiante' ).
  gr_column->set_short_text( 'Cód. ST.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'CSOTJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Estudio' ).
  gr_column->set_medium_text( 'Cód. Estudio' ).
  gr_column->set_short_text( 'Cód. CS.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PARTNER' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Int.comercial' ).
  gr_column->set_medium_text( 'Interloc.cial.' ).
  gr_column->set_short_text( 'Int.cial.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'STUDENT12' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Número de matrícula' ).
  gr_column->set_medium_text( 'Nº matrícula' ).
  gr_column->set_short_text( 'Nº mat.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADMITIDO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Admitido' ).
  gr_column->set_medium_text( 'Admitido' ).
  gr_column->set_short_text( 'Admitido' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ACTIVO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Activo' ).
  gr_column->set_medium_text( 'Activo' ).
  gr_column->set_short_text( 'Activo' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'GRADUADO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Graduado' ).
  gr_column->set_medium_text( 'Graduado' ).
  gr_column->set_short_text( 'Graduado' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'RETIRADO' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Retirado' ).
  gr_column->set_medium_text( 'Retirado' ).
  gr_column->set_short_text( 'Retirado' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ISTAT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Status de la solicitud de admisión' ).
  gr_column->set_medium_text( 'Status sol.admisión' ).
  gr_column->set_short_text( 'Status' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_AYEAR' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año Académico Admisión' ).
  gr_column->set_medium_text( 'Año Académico Adm.' ).
  gr_column->set_short_text( 'Año.Ac.Adm' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_PERID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Periodo Académico Admisión' ).
  gr_column->set_medium_text( 'Cod.Per.Acad.Adm.' ).
  gr_column->set_short_text( 'C.P.Ac.Adm' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_PERIT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Periodo Académico Admisión' ).
  gr_column->set_medium_text( 'Per. Académico Adm.' ).
  gr_column->set_short_text( 'Per.Ac.Adm' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_ACLEVEL' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Ciclo Académico Admision' ).
  gr_column->set_medium_text( 'Ciclo Académico Adm.' ).
  gr_column->set_short_text( 'Cic.Ac.Adm' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_CATEG' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Categoría de admisión' ).
  gr_column->set_medium_text( 'Categoría admisión' ).
  gr_column->set_short_text( 'Categoría' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_ENRCATEG' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Clase de oyente' ).
  gr_column->set_medium_text( 'Clase de oyente' ).
  gr_column->set_short_text( 'Cl.oyente' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ADM_RECPT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Fe.entr.(sol.adm.)' ).
  gr_column->set_medium_text( 'Fecha entrada' ).
  gr_column->set_short_text( 'Fe.entrada' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'BEG_PROCESS' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Actividad inscrip.' ).
  gr_column->set_medium_text( 'Cod.Activ.inscrip.' ).
  gr_column->set_short_text( 'Cod.Inscri' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'BEG_PROCESST' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Actividad inscrip.' ).
  gr_column->set_medium_text( 'Activ.inscrip.' ).
  gr_column->set_short_text( 'Inscrip.' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'BEG_REASON' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Mot.actividad' ).
  gr_column->set_medium_text( 'Cod.Mot.actividad' ).
  gr_column->set_short_text( 'Cod.Motivo' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'BEG_REASONT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Mot.actividad' ).
  gr_column->set_medium_text( 'Mot.actividad' ).
  gr_column->set_short_text( 'Motivo' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'BEG_KEY_DATE' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'FeClvActivInSegmEst' ).
  gr_column->set_medium_text( 'FeClvActSegmEst' ).
  gr_column->set_short_text( 'Fe.clave' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'END_PROCESS' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Actividad anul.matr.' ).
  gr_column->set_medium_text( 'Cod.ActivAnulMatríc' ).
  gr_column->set_short_text( 'Cod.Activi' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'END_PROCESST' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Actividad anul.matr.' ).
  gr_column->set_medium_text( 'ActivAnulMatríc' ).
  gr_column->set_short_text( 'Actividad' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'END_REASON' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Motivo anul.matríc.' ).
  gr_column->set_medium_text( 'Cod.Mot.anul.matr.' ).
  gr_column->set_short_text( 'C.Mot.Anu' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'END_REASONT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Motivo anul.matríc.' ).
  gr_column->set_medium_text( 'Mot.anul.matr.' ).
  gr_column->set_short_text( 'Motivo Anu' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'END_KEY_DATE' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Efectuado el' ).
  gr_column->set_medium_text( 'Efectuado el' ).
  gr_column->set_short_text( 'Efect.el' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TYPE' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Clase identificación' ).
  gr_column->set_medium_text( 'Cod. Clase ID' ).
  gr_column->set_short_text( 'C.Clase ID' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TYPE_TEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Clase identificación' ).
  gr_column->set_medium_text( 'Clase ID' ).
  gr_column->set_short_text( 'Clase ID' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'IDNUMBER' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Nº identificación' ).
  gr_column->set_medium_text( 'Número ID' ).
  gr_column->set_short_text( 'Número ID' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'INSTITUTE' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Lugar Expedición' ).
  gr_column->set_medium_text( 'Lugar Expedición' ).
  gr_column->set_short_text( 'Expedición' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OOTJID_FA' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'O Facultad' ).
  gr_column->set_medium_text( 'O Facultad' ).
  gr_column->set_short_text( 'O Facultad' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OFA_STEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Facultad' ).
  gr_column->set_medium_text( 'Facultad' ).
  gr_column->set_short_text( 'Facultad' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OOTJID_TP' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'O Tipo Programa' ).
  gr_column->set_medium_text( 'O Tipo Programa' ).
  gr_column->set_short_text( 'O.T.Prog.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OTP_STEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Tipo Programa' ).
  gr_column->set_medium_text( 'Tipo Programa' ).
  gr_column->set_short_text( 'T.Programa' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SCOTJID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código Plan Estudio' ).
  gr_column->set_medium_text( 'Cód. Plan Estudio' ).
  gr_column->set_short_text( 'Cód. SC.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SCSTEXT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Plan Estudio' ).
  gr_column->set_medium_text( 'Plan Estudio' ).
  gr_column->set_short_text( 'SC' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SNIES' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Código de SNIES' ).
  gr_column->set_medium_text( 'Cód. SNIES' ).
  gr_column->set_short_text( 'SNIES' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'OPTLENGTH' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Duración de estudios' ).
  gr_column->set_medium_text( 'Dur.estudios' ).
  gr_column->set_short_text( 'Duración' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TIMEUNIT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Un.tiempo' ).
  gr_column->set_medium_text( 'Cod.Un.tiempo' ).
  gr_column->set_short_text( 'C.U.tiempo' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'TIMEUNITT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Un.tiempo' ).
  gr_column->set_medium_text( 'Un.tiempo' ).
  gr_column->set_short_text( 'Un.tiempo' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'AYEARPERID' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año y periodo académico consultado' ).
  gr_column->set_medium_text( 'Año.Per acad. consul' ).
  gr_column->set_short_text( 'A.Per.con' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'AYEAR_INI' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año académico inicio' ).
  gr_column->set_medium_text( 'Año académico inicio' ).
  gr_column->set_short_text( 'Año ac.ini' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PERID_INI' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod. Período académico inicio' ).
  gr_column->set_medium_text( 'Cod.Per.acad.inic.' ).
  gr_column->set_short_text( 'C.Per.ini.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PERIT_INI' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Período académico inicio' ).
  gr_column->set_medium_text( 'Per.académico inic.' ).
  gr_column->set_short_text( 'Per.inicio' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'AYEAR_ULT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Año académico actual' ).
  gr_column->set_medium_text( 'Año académico actual' ).
  gr_column->set_short_text( 'Año ac.act' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PERID_ULT' ).
  "Establece los valores de descripcion de la columna
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Cod.Período académico actual' ).
  gr_column->set_medium_text( 'Cod.Per.acad.actu.' ).
  gr_column->set_short_text( 'C.Per.act.' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PERIT_ULT' ).
  "Establece los valores de descripcion de la columna
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Período académico actual' ).
  gr_column->set_medium_text( 'Per.académico actu.' ).
  gr_column->set_short_text( 'Per.actual' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ACLEVEL_ULT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Ciclo Académico Actual' ).
  gr_column->set_medium_text( 'Ciclo Académico Act.' ).
  gr_column->set_short_text( 'Cic.Ac.Act' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'ACLEVEL_SIG' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Ciclo Académico Siguiente' ).
  gr_column->set_medium_text( 'Ciclo Académico Sig.' ).
  gr_column->set_short_text( 'Cic.Ac.Sig' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'CPMIN' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Créditos mínimos' ).
  gr_column->set_medium_text( 'Créditos mín.' ).
  gr_column->set_short_text( 'Créd.mín.' ).
  "Apunta el campo de la unidad de medida
  gr_column->set_quantity_column( 'CPUNIT' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'CPMAX' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Créditos máximos' ).
  gr_column->set_medium_text( 'Créditos máx.' ).
  gr_column->set_short_text( 'Créd.máx.' ).
  "Apunta el campo de la unidad de medida
  gr_column->set_quantity_column( 'CPUNIT' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'CPUNIT' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Tp.crédito' ).
  gr_column->set_medium_text( 'Tp.crédito' ).
  gr_column->set_short_text( 'Tp.crédito' ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'PROFIT_CTR' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Centro de beneficio' ).
  gr_column->set_medium_text( 'CeBe' ).
  gr_column->set_short_text( 'CeBe' ).
  "Establece la columna como no visible
  gr_column->set_visible( abap_false ).

  "Obtiene la referencia de la columna
  gr_column ?= gr_columns->get_column( 'SM_CPATTEMP' ).
  "Establece los valores de descripcion de la columna
  gr_column->set_long_text( 'Créditos Cursados' ).
  gr_column->set_medium_text( 'Crédicos Cursados' ).
  gr_column->set_short_text( 'Créd.Curs' ).
  "Apunta el campo de la unidad de medida
  gr_column->set_quantity_column( 'CPUNIT' ).

ENDFORM.                    " F_CONFIGURAR_COLUMNAS

*&---------------------------------------------------------------------*
*&      Form  F_TOP_PAGE
*&---------------------------------------------------------------------*
FORM f_top_page .

  "Declaracion
  DATA:
    lv_titulo TYPE lvc_title.


  "Asigna el titulo
  lv_titulo = 'Reporte Certificados Académicos'(t01).

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
  gr_selections->set_selection_mode( if_salv_c_selection_mode=>single ). " 1=Simple(toda la fila), 2=Multiple(toda la fila), 3=Simple(casilla individual + Columa seleccion),

ENDFORM.                    "F_EVENTO_ALV

*----------------------------------------------------------------------*
* CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

* Metodo para los comandos de usuario
  METHOD on_user_command.
*   Variable propia del metodo:
*     e_salv_function = Codigo del comando ejecutado.

    "Declaracion de variables y objetos del metodo
    DATA:
      lr_selections TYPE REF TO cl_salv_selections,
      lr_alv_p      TYPE REF TO cl_salv_table,
      lr_functions  TYPE REF TO cl_salv_functions_list,
      lr_columns    TYPE REF TO cl_salv_columns_table,
      lr_column     TYPE REF TO cl_salv_column_table,
      lr_layout     TYPE REF TO cl_salv_layout,
      lt_rows       TYPE salv_t_row,
      lt_informe_pe TYPE gtyt_informe_pe,
      lt_informe_pm TYPE gtyt_informe_pm,
      lt_informe_sm TYPE gtyt_informe_sm,
      lt_informe_na TYPE gtyt_informe_na,
      lt_informe_ps TYPE gtyt_informe_ps,
      lt_informe_ro TYPE gtyt_informe_ro,
      lt_informe_gr TYPE gtyt_informe_gr,
      ls_key        TYPE salv_s_layout_key,
      ls_rows       TYPE i,
      ls_informe    TYPE gty_informe,
      message       TYPE string.

    "Field-symbols
    FIELD-SYMBOLS:
      <tabla> TYPE ANY TABLE .


    lr_selections = gr_table->get_selections( ).        " Obtiene la referencia de la linea seleccionada en el ALV
    lt_rows = lr_selections->get_selected_rows( ).      " Obtiene la fila seleccionada en el ALV

    "Obtiene la seleccion
    READ TABLE lt_rows INTO ls_rows INDEX 1.

    "Continua solo si encuentra el registro
    CHECK sy-subrc EQ 0.

    "Obtiene el registro seleccionado
    READ TABLE gt_informe INTO ls_informe INDEX ls_rows.

    "Continua solo si encuentra el registro
    CHECK sy-subrc EQ 0.

    "Verifica el comando que ha ejecutado el usuario
    CASE e_salv_function.
      WHEN '&PE'. "Periodos Excedencia
        "Crea una copia de los datos
        lt_informe_pe = gt_informe_pe.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_pe
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_pe[] TO <tabla>[].

      WHEN '&PM'. "Periodos Matriculads
        "Crea una copia de los datos
        lt_informe_pm = gt_informe_pm.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_pm
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_pm[] TO <tabla>[].

      WHEN '&SM'. "Asignaturas
        "Crea una copia de los datos
        lt_informe_sm = gt_informe_sm.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_sm
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_sm[] TO <tabla>[].

      WHEN '&NA'. "Notas Académicas
        "Crea una copia de los datos
        lt_informe_na = gt_informe_na.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_na
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_na[] TO <tabla>[].

      WHEN '&PS'. "Promedio Acumulado
        "Crea una copia de los datos
        lt_informe_ps = gt_informe_ps.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_ps
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_ps[] TO <tabla>[].

      WHEN '&RO'. "Rotaciones
        "Crea una copia de los datos
        lt_informe_ro = gt_informe_ro.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_ro
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_ro[] TO <tabla>[].

      WHEN '&GR'. "Datos Graduación
        "Crea una copia de los datos
        lt_informe_gr = gt_informe_gr.
        "Elimina los registro que no pertenecen a la seleccion
        DELETE lt_informe_gr
          WHERE opbel NE ls_informe-opbel.
        "Asigna los datos a la tabla
        ASSIGN lt_informe_gr[] TO <tabla>[].
    ENDCASE.

    "Continua solo si se tienen datos
    CHECK NOT <tabla>[] IS INITIAL.

    TRY.
        "Importa el objeto de referencia dentro de GR_TABLE y pasa la tabla interna con los datos para el ALV
        cl_salv_table=>factory( IMPORTING r_salv_table = lr_alv_p
                                CHANGING  t_table = <tabla>[] ).
      CATCH cx_salv_msg.
    ENDTRY.

    "Obtiene las funciones y las asigna todas
    lr_functions = lr_alv_p->get_functions( ).
    lr_functions->set_all( 'X' ).
    "Obtiene todas las columnas del ALV
    lr_columns = lr_alv_p->get_columns( ).
    "Hace que las columnas sean del ancho optimo.
    lr_columns->set_optimize( 'X' ).

    "Habilita el manejo de Layouts en el ALV
    lr_layout = lr_alv_p->get_layout( ).
    ls_key-report = sy-repid && e_salv_function.
    lr_layout->set_key( ls_key ).
    lr_layout->set_default( 'X' ).
    lr_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

    "Si el comando fue "Datos Graduación.
    IF e_salv_function EQ '&GR'.
      "Obtiene la referencia de la columna
      lr_column ?= lr_columns->get_column( 'NOASISTIO' ).
      "Establece la columna como checkbox
      lr_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
    ENDIF.

    "Si el contenido fue "Rotaciones"
    IF e_salv_function EQ '&RO'.
      "Obtiene la referencia de la columna
      lr_column ?= lr_columns->get_column( 'STEXT_REC' ).
      "Establece los valores de descripcion de la columna
      lr_column->set_long_text( 'Descripción Recurso' ).
      lr_column->set_medium_text( 'Descripción Recurso' ).
      lr_column->set_short_text( 'Des Recurs' ).
    ENDIF.

    "Establece el ALV tipo popup
    lr_alv_p->set_screen_popup( start_column = 5
                                end_column   = 220
                                start_line   = 3
                                end_line     = 20 ).

    "Genera el popup
    lr_alv_p->display( ).

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
