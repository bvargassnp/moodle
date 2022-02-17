class ZEDU_CL_HELP definition
  public
  final
  create public .

public section.

  types:
    ty_ra_evento type range of evtid .
  types:
    ty_rs_evento type line of ty_ra_evento .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_EVENTOS
    importing
      !IV_EVENTO type EVTID
    returning
      value(RR_EVENTOS) type TY_RA_EVENTO .
  class-methods CHECK_EMAIL
    importing
      !IV_MAIL type DATA
    exceptions
      MAIL_INVALIDO .
  class-methods CHECK_ALPHABET
    importing
      value(IV_TEXTO) type DATA
    exceptions
      NOT_ALPHA .
  class-methods CHECK_NRO_TELEF
    importing
      value(IV_NRO_TELEF) type DATA
    exceptions
      NOT_NRO_TELEF .
  class-methods GET_TEXT_ST
    importing
      !IV_TEXT_NAME type STRING
      value(IV_CONVERT_ASCII) type BOOLE_D default CL_BP_CONST=>FALSE
    returning
      value(RV_TEXTO) type STRING .
  class-methods CHECK_FACU_AUTO
    importing
      !IV_WITH_ERROR type BOOLE_D default CL_BP_CONST=>FALSE
      !IV_VALIDA_SUPER type BOOLE_D default CL_BP_CONST=>FALSE
    changing
      !CT_FACULTADES type WDY_KEY_VALUE_TABLE optional
    exceptions
      SIN_AUTORIZACION
      NO_SUPERUSUARIO .
  class-methods CHECK_IDENTIFICACION
    importing
      !IV_NUM_DOC type BU_ID_NUMBER
    returning
      value(RT_MESSAGES) type BAPIRETTAB
    exceptions
      NOT_FOUND .
  class-methods GET_TVARVC
    importing
      value(IV_NAME) type RVARI_VNAM
      !IV_RANGE type BOOLE_D default CL_BP_CONST=>FALSE
    changing
      !CV_DATO type DATA optional .
  class-methods CONVERT_TIPO_DOC
    importing
      !IV_TIPO_HCM type DATA optional
      !IV_TIPO_BP type DATA optional
    changing
      !CV_TIPO type DATA .
  class-methods CONVERT_SPECIAL_CHARACTERS
    changing
      !CH_STRING type DATA .
  class-methods CONVERT_URL_TO_LINK
    changing
      !CV_TEXTO type STRING .
protected section.
private section.

  types:  begin of ty_tipo_doc,
            tipo_bp   type string,
            tipo_hcm  type string,
          end of ty_tipo_doc.

  class-data gt_tipo_doc type standard table of ty_tipo_doc with default key.
ENDCLASS.



CLASS ZEDU_CL_HELP IMPLEMENTATION.


  method check_alphabet.

    constants lc_letras type string value ` ABCDEFGHIJKLMNÑOPQRSTUVWXYZÁÉÍÓÚÜ`.

    translate iv_texto to upper case.

    if iv_texto cn lc_letras.

      "Solo se permiten Letras a-z A-Z
      message e067(zedu_wd_message)
        raising not_alpha.

    endif.

  endmethod.


  method check_email.
* Local Data
    data:  lo_regex   type ref to cl_abap_regex,       " Regex Object
           lo_matcher type ref to cl_abap_matcher,     " Matcher Object
           lv_match   type c length 1.                 " Match ?

* Instntiate Regex
    create object lo_regex
      exporting
        pattern     = '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9._-]+\.[a-zA-Z0-9]{2,4}'
        ignore_case = abap_true.

* Create the Matcher
    lo_matcher = lo_regex->create_matcher( text = iv_mail ).

* Match not found, invalid
    if lo_matcher->match( ) is initial.

      "Dirección de correo electrónico errónea
      message e553(sproject)
        raising mail_invalido.

    endif.

  endmethod.


  method check_facu_auto.

    types:  begin of lty_seguridad,
              uname         type zedu_pd_segurida-uname,
              nivel_permiso type zedu_pd_segurida-nivel_permiso,
              facultad      type zedu_pd_segurida-facultad,
            end of lty_seguridad.
    types:  begin of lty_facultades,
              facultad type zedu_pd_segurida-facultad,
              text     type string,
            end of lty_facultades.

    data lt_seguridad type standard table of lty_seguridad.
    data lt_facultades type standard table of lty_facultades.
    data ls_facultad type wdy_key_value.
* Se comenta codigo de autorización, ya no se ocupara
* Según indicación FUNCIONAL
**    "Recupero facultades con permisos para el usuario
**    select uname
**           nivel_permiso
**           facultad
**      from zedu_pd_segurida
**      into table lt_seguridad
**        where uname eq sy-uname.
**
**    if sy-subrc eq 0.
**
**      "validamos si es superusuario
**      read table lt_seguridad
**        transporting no fields
**          with key nivel_permiso = `0`.
**
**      if  sy-subrc        eq 0.
**        "no elimina ninguna facultad
**
**      elseif iv_valida_super eq cl_bp_const=>true.
**
**        "Debe seleccionar al menos 1 Facultad
**        message e071(zedu_wd_message)
**          raising no_superusuario.
**
**      else. "no es superUsuario
**
**        "solo deja las facultades autorizadas
**        loop at ct_facultades
**          into ls_facultad.
**
**          read table lt_seguridad
**            with key facultad = ls_facultad-key
**              transporting no fields.
**
**          if sy-subrc ne 0.
**            delete ct_facultades where key eq ls_facultad-key.
**          endif.
**        endloop.
**
**      endif.
**
**    else.
**
**      if iv_with_error eq cl_bp_const=>true.
**        "Usuario sin autorización para las facultades seleccionadas (ZEDU_PD_SEGURIDA)
**        message e070(zedu_wd_message)
**          raising sin_autorizacion.
**
**      else.
**        "no está cargado en la tabla el usuario por lo tanto
**        " no tiene autorización a ninguna facultad
**        clear ct_facultades.
**
**      endif.
**
**    endif.

  endmethod.


  method check_identificacion.

    data lo_assist type ref to zcl_wd_general_ass.

    select  id~partner,
            id~type,
            b0~mc_name1,
            b0~mc_name2
      from but0id as id
        left outer join but000 as b0
          on id~partner eq b0~partner
      into table @data(lt_partners)
      where idnumber         = @iv_num_doc
        and valid_date_from <= @sy-datum
        and valid_date_to   >= @sy-datum.

    if sy-subrc eq 0.

      create object lo_assist.

      loop at lt_partners
        into data(ls_partner).

        read table lo_assist->get_dropdown_key( 'TIPO_DOC' )
          with key value  = ls_partner-type
            into data(ls_type).

        "Nro. de identificación &1 existe como &2 perteneciente a &3
        append initial line to rt_messages  assigning field-symbol(<fs_mess>).

        <fs_mess>-id  = `ZEDU_WD_MESSAGE`.
        <fs_mess>-type = `W`.
        <fs_mess>-number  = 074.
        <fs_mess>-message_v1  = iv_num_doc.
        <fs_mess>-message_v2  = ls_type-text.
        <fs_mess>-message_v3  = ls_partner-mc_name2.
        <fs_mess>-message_v4  = ls_partner-mc_name1.

      endloop.

    else.
      "No existe
      message e006(64) with iv_num_doc
        raising not_found.

    endif.

  endmethod.


  method check_nro_telef.

    constants lc_nros type string value ` 1234567890-`.

    if iv_nro_telef cn lc_nros.

      "Solo se permiten números, espacios y separador -
      message e068(zedu_wd_message)
        raising not_nro_telef.

    endif.

  endmethod.


  method class_constructor.

    append initial line to gt_tipo_doc assigning field-symbol(<fs_tipo>).
    <fs_tipo>-tipo_hcm  = `02`.  "Cédula de ciudadania
    <fs_tipo>-tipo_bp   = `FS0001`.

    append initial line to gt_tipo_doc assigning <fs_tipo>.
    <fs_tipo>-tipo_hcm  = `03`.  "Cédula de Extranjería
    <fs_tipo>-tipo_bp   = `FS0003`.

    append initial line to gt_tipo_doc assigning <fs_tipo>.
    <fs_tipo>-tipo_hcm  = `04`.  "Tarjeta de Identidad
    <fs_tipo>-tipo_bp   = `FS0005`.

    append initial line to gt_tipo_doc assigning <fs_tipo>.
    <fs_tipo>-tipo_hcm  = `05`.  "Pasaporte
    <fs_tipo>-tipo_bp   = `FS0002`.

    append initial line to gt_tipo_doc assigning <fs_tipo>.
    <fs_tipo>-tipo_hcm  = `07`.  "Registro Civil de Nacimiento
    <fs_tipo>-tipo_bp   = `FS0006`.

  endmethod.


  method convert_special_characters.

    call function 'SCP_REPLACE_STRANGE_CHARS'
      exporting
        intext            = ch_string
        replacement       = 124  "vertical bar
      importing
        outtext           = ch_string
      exceptions
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        others            = 6.

    if sy-subrc eq 0.
      replace all occurrences of /iwbep/cl_mgw_remote_handler=>co_seperator_entry
        in ch_string with ` `.
    endif.

  endmethod.


  method convert_tipo_doc.

    if iv_tipo_hcm  is not initial.

      read table gt_tipo_doc
        with key tipo_hcm = iv_tipo_hcm
          into data(ls_tipo_doc).

      if sy-subrc eq 0.
        cv_tipo = ls_tipo_doc-tipo_bp.
      endif.

    elseif iv_tipo_bp is not initial.

      read table gt_tipo_doc
        with key tipo_bp = iv_tipo_bp
          into ls_tipo_doc.

      if sy-subrc eq 0.
        cv_tipo = ls_tipo_doc-tipo_hcm.
      endif.

    endif.

  endmethod.


  method convert_url_to_link.

    data  lo_regex            type ref to cl_abap_regex.
    data  lo_match            type ref to cl_abap_matcher.
    data  lt_match_result_tab type match_result_tab.
    data  lo_form_text        type ref to cl_wd_formatted_text.
    data  lv_link             type string.

    create object lo_regex exporting pattern = `([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?`.
    create object lo_match
      exporting
        regex = lo_regex
        text  = cv_texto.

    "Recupero todas las direcciones web
    lt_match_result_tab = lo_match->find_all( ).

    "Reemplazo de atrás hacia adelante para no alterar los offset
    sort lt_match_result_tab by offset  descending.

    loop at lt_match_result_tab
      into data(ls_match).

      move cv_texto+ls_match-offset(ls_match-length) to lv_link.

      concatenate `http://`
        lv_link
          into data(lv_reference).

      "Genero hipervinculo
      lo_form_text = cl_wd_formatted_text=>make_a_tag(
                        href        = lv_reference
                        target      = '_blank'
                        inner_text  = lv_link  ).

      "reemplaza texto por hipervinculo
      replace lv_link
        in cv_texto
          with lo_form_text->m_xml_text.

    endloop.

  endmethod.


  method get_eventos.
    field-symbols <fs_ev> type ty_rs_evento.

    "busca el evento princial
    select hrt9112~idevento as low
      from hrt9112
      inner join hrp9112
        on hrt9112~tabnr eq hrp9112~tabnr
          into corresponding fields of table rr_eventos
            where
                  "Recupera evento principal
                  ( hrp9112~objid    eq iv_evento )
*                  "y secundarios
*                 or hrt9112~idevento eq iv_evento )
              and
                  hrt9112~venta    eq space
              and hrt9112~tabnr    ne space
              and hrt9112~idevento ne 0.

    if sy-subrc eq 0. "si era secundario

      "en rr_eventos está solo el evento principal
      select hrp9112~objid as low
        from hrt9112
        inner join hrp9112
          on hrt9112~tabnr eq hrp9112~tabnr
            into corresponding fields of table rr_eventos
              for all entries in rr_eventos
              where
                    "Recupera evento principal
                    ( hrp9112~objid    eq rr_eventos-low
                    "y secundarios
                   or hrt9112~idevento eq rr_eventos-low )
                and
                    hrt9112~venta    eq space
                and hrt9112~tabnr    ne space.

      if sy-subrc ne 0.
        clear rr_eventos[].
      endif.

    else. "Si no era secundario

      "iv_evento es principal
      select hrp9112~objid as low
        from hrt9112
        inner join hrp9112
          on hrt9112~tabnr eq hrp9112~tabnr
            into corresponding fields of table rr_eventos
              where
                    "Recupera evento principal
                    ( hrp9112~objid    eq iv_evento
                    "y secundarios
                   or hrt9112~idevento eq iv_evento )
                and
                    hrt9112~venta    eq space
                and hrt9112~tabnr    ne space.

      if sy-subrc ne 0.
        clear rr_eventos[].
      endif.

    endif.

    if rr_eventos[] is not initial.

      delete rr_eventos
        where low is initial.

      sort rr_eventos
        by low.

      delete adjacent duplicates from rr_eventos.

      loop at rr_eventos
        assigning <fs_ev>.

        <fs_ev>-option  = `EQ`.
        <fs_ev>-sign    = `I`.

      endloop.

    else.

      "Agregamos el evento de entrada
      append initial line to rr_eventos assigning <fs_ev>.
      <fs_ev>-option  = `EQ`.
      <fs_ev>-sign    = `I`.
      <fs_ev>-low     = iv_evento.

    endif.

  endmethod.


  method get_text_st.
    data lt_tline     type table of tline.
    data ls_tline     type tline.
    data lv_name      type thead-tdname.
    data lt_datatab   type tdtab_c132.

    lv_name     = iv_text_name.

    call function 'READ_TEXT'
      exporting
        id                      = 'ST'
        language                = sy-langu
        name                    = lv_name
        object                  = 'TEXT'
      tables
        lines                   = lt_tline
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.
    if sy-subrc <> 0.
    endif.

*	Begin	-->	MgM  DCEK903551 quita caracteres <(> 21/02/2017
    if iv_convert_ascii eq cl_bp_const=>true.

      call function 'CONVERT_ITF_TO_ASCII'
        exporting
          formatwidth       = 132
          language          = sy-langu
          tabletype         = 'ASC'
        importing
          c_datatab         = lt_datatab
        tables
          itf_lines         = lt_tline
        exceptions
          invalid_tabletype = 1
          others            = 2.

      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      loop at lt_datatab
        into data(ls_data).

        concatenate rv_texto
                    ls_data
               into rv_texto
           separated by space.

      endloop.

    else.
*	End	  -->	MgM  DCEK903551
      loop at lt_tline
        into ls_tline.

        concatenate rv_texto
                    ls_tline-tdline
               into rv_texto
               separated by space.
      endloop.

    endif.

  endmethod.


  method get_tvarvc.

    translate iv_name to upper case.

    if iv_range eq cl_bp_const=>true.

      select  sign,
              opti,
              low,
              high
        into table @data(lt_datos)
          from tvarvc
            where name eq @iv_name
              and type eq @if_ism_sd_object_set=>con_kind_range_tab.

      if sy-subrc ne 0.

        "agregamos línea sin valor para que posibles "IN" no tomen todos los valores
        append initial line to lt_datos
          assigning field-symbol(<fs_tvarv>).

        <fs_tvarv>-sign = `I`.
        <fs_tvarv>-opti = `EQ`.

      endif.

      cv_dato = lt_datos.

    else.

      select single low
        into @data(lv_valor)
          from tvarvc
            where name eq @iv_name
              and type eq @if_ism_sd_object_set=>con_kind_parameter.

      if sy-subrc eq 0.

        move lv_valor to cv_dato.

      endif.

    endif.

  endmethod.
ENDCLASS.
