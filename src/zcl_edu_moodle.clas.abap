CLASS zcl_edu_moodle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: gt_c_param    TYPE SORTED TABLE OF zedu_c_param WITH UNIQUE KEY primary_key COMPONENTS repid idparam idparampos.
    METHODS:
      constructor,
      payment_report_product
        IMPORTING
          i_input  TYPE zstedu_paymentreportprod_in
        EXPORTING
          e_output TYPE zstedu_paymentreportprod_out
        RAISING
          zcx_edu_paymentreportprod.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA lo_http_client TYPE REF TO if_http_client .
    CONSTANTS gc_content_type TYPE string VALUE 'Content-type' ##NO_TEXT.
    CONSTANTS gc_json_content TYPE string VALUE 'application/json;charset=UTF-8' ##NO_TEXT.
    CONSTANTS gc_repid_pp TYPE char40 VALUE 'ZCL_EDU_MOODLE' ##NO_TEXT.
    CONSTANTS gc_rfc_dest TYPE char8 VALUE 'RFC_DEST' ##NO_TEXT.
    CONSTANTS gc_clave TYPE char5 VALUE 'CLAVE' ##NO_TEXT.
    CONSTANTS gc_wstoken TYPE char7 VALUE 'WSTOKEN' ##NO_TEXT.
    CONSTANTS gc_wsfunction TYPE char10 VALUE 'WSFUNCTION' ##NO_TEXT.
    CONSTANTS gc_format TYPE char4 VALUE 'json' ##NO_TEXT.
    CONSTANTS gc_ex1 TYPE string VALUE 'invalid_parameter_exception' ##NO_TEXT.
    CONSTANTS gc_ex2 TYPE string VALUE 'moodle_exception' ##NO_TEXT.



    METHODS http_client
      IMPORTING                                       "DATA "TYPE REF TO data
        !i_in          TYPE any
        !i_destination TYPE c OPTIONAL
        !i_request_id  TYPE string OPTIONAL
      EXPORTING
        !e_out         TYPE any                               "REF TO data
        !e_status      TYPE i
      RAISING
        zcx_edu_paymentreportprod .
    METHODS get_param_customizing .
    METHODS get_auth
      IMPORTING
        !i_data        TYPE string
      EXPORTING
        !e_hash_string TYPE string
      RAISING
        cx_abap_message_digest .
    METHODS set_request_uri
      IMPORTING
        !i_input           TYPE zstedu_paymentreportprod_in
      RETURNING
        VALUE(r_url_param) TYPE string .
ENDCLASS.



CLASS zcl_edu_moodle IMPLEMENTATION.


  METHOD constructor.
    me->get_param_customizing( ).
  ENDMETHOD.


  METHOD get_auth.
    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm = 'MD5'
        if_data = i_data
      IMPORTING
        ef_hashstring = e_hash_string
    ).
    TRANSLATE e_hash_string TO LOWER CASE.
  ENDMETHOD.


  METHOD get_param_customizing.
    SELECT mandt repid idparam idparampos valor
      INTO TABLE me->gt_c_param
      FROM zedu_c_param
      WHERE repid = gc_repid_pp.
  ENDMETHOD.


  METHOD http_client.

    DATA: ls_paymentrepprod_out_ex1 TYPE zstedu_paymentrepprod_out_ex1,
          ls_paymentrepprod_out_ex2 TYPE zstedu_paymentrepprod_out_ex2.

    cl_http_client=>create_by_destination(
      EXPORTING
        destination              = i_destination
      IMPORTING
        client                   = lo_http_client
      EXCEPTIONS
        argument_not_found       = 1
        destination_not_found    = 2
        destination_no_authority = 3
        plugin_not_active        = 4
        internal_error           = 5
        OTHERS                   = 6
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
        EXPORTING
          type       = sy-msgty
          id         = sy-msgid
          number     = sy-msgno
          message_v1 = sy-msgv1
          message_v2 = sy-msgv2
          message_v3 = sy-msgv3
          message_v4 = sy-msgv4.
    ENDIF.

    IF lo_http_client IS BOUND.


      lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).
      lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_http_client->request->set_content_type( gc_json_content ).

      cl_http_utility=>set_request_uri(
         EXPORTING
           request = lo_http_client->request
           uri     = me->set_request_uri( i_input = i_in )
           ).



      lo_http_client->send(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5
      ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = sy-msgty
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4.
      ENDIF.

      lo_http_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4
      ).

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = sy-msgty
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4.
      ENDIF.

      DATA(lv_cdata) = lo_http_client->response->get_cdata( ).

      lo_http_client->close( ).

      lo_http_client->response->get_status( IMPORTING code = e_status ).

      FIND REGEX gc_ex1 IN lv_cdata.
      IF sy-subrc EQ 0.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_cdata pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = ls_paymentrepprod_out_ex1 ).
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = 'E'
            id         = '00'
            number     = '001'
            message    = CONV bapi_msg( ls_paymentrepprod_out_ex1-message )
            message_v1 = CONV symsgv( ls_paymentrepprod_out_ex1-exception )
            message_v2 = CONV symsgv( ls_paymentrepprod_out_ex1-errorcode )
            message_v3 = CONV symsgv( ls_paymentrepprod_out_ex1-message )
            message_v4 = CONV symsgv( ls_paymentrepprod_out_ex1-debuginfo ).
      ENDIF.

      FIND REGEX gc_ex2 IN lv_cdata.
      IF sy-subrc EQ 0.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_cdata pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = ls_paymentrepprod_out_ex2 ).
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = 'E'
            id         = '00'
            number     = '001'
            message    = CONV bapi_msg( ls_paymentrepprod_out_ex1-message )
            message_v1 = CONV symsgv( ls_paymentrepprod_out_ex2-exception )
            message_v2 = CONV symsgv( ls_paymentrepprod_out_ex2-errorcode )
            message_v3 = CONV symsgv( ls_paymentrepprod_out_ex2-message ).
      ENDIF.

      /ui2/cl_json=>deserialize( EXPORTING json = lv_cdata pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = e_out ).
    ENDIF.
  ENDMETHOD.


  METHOD payment_report_product.

    DATA: ls_in TYPE zstedu_paymentreportprod_in.

    TRY.
        DATA(lv_data) = |{ me->gt_c_param[ repid = gc_repid_pp idparam = gc_clave ]-valor }{ i_input-code }{ i_input-remoteid }{ i_input-identification }{ i_input-email }|.

        ls_in = CORRESPONDING #( i_input ).
        ls_in-wstoken = me->gt_c_param[ repid = gc_repid_pp idparam = gc_wstoken ]-valor.
        ls_in-wsfunction = me->gt_c_param[ repid = gc_repid_pp idparam = gc_wsfunction ]-valor.
        ls_in-moodlewsrestformat = me->gc_format.

        me->get_auth(
          EXPORTING
            i_data                 = lv_data
          IMPORTING
            e_hash_string          = ls_in-token
        ).

        me->http_client(
          EXPORTING
            i_in                   = ls_in
            i_destination          = me->gt_c_param[ repid = gc_repid_pp idparam = gc_rfc_dest ]-valor
          IMPORTING
            e_out                  = e_output
            e_status               = DATA(lv_status_rest)
        ).

      CATCH cx_abap_message_digest.
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = sy-msgty
            id         = sy-msgid
            number     = sy-msgno
            message_v1 = sy-msgv1
            message_v2 = sy-msgv2
            message_v3 = sy-msgv3
            message_v4 = sy-msgv4.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_err).
        RAISE EXCEPTION TYPE zcx_edu_paymentreportprod
          EXPORTING
            type       = 'E'
            id         = '00'
            number     = '000'
            message_v1 = CONV symsgv( lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD set_request_uri.

    DATA: l_descr_ref TYPE REF TO cl_abap_structdescr,
          gt_comp     TYPE abap_component_tab,
          gs_comp     TYPE abap_componentdescr.

    FIELD-SYMBOLS: <fs_table> TYPE ANY TABLE.

    l_descr_ref ?= cl_abap_typedescr=>describe_by_data( i_input ).

    gt_comp = l_descr_ref->get_components( ).

    ASSIGN i_input TO FIELD-SYMBOL(<f1>).
    IF <f1> IS ASSIGNED.
      LOOP AT CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( i_input ) )->get_components( )
                                    INTO DATA(wa_structure_field).
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <f1> TO FIELD-SYMBOL(<f2>).
        IF <f2> IS ASSIGNED.
          CASE wa_structure_field-type->kind.
            WHEN cl_abap_typedescr=>kind_elem.
              lo_http_client->append_field_url(
                EXPORTING
                  name  =  to_lower( wa_structure_field-name )
                  value =  CONV string( <f2> )
                CHANGING
                  url   = r_url_param
              ).
            WHEN cl_abap_typedescr=>kind_table.
              ASSIGN <f2>  TO <fs_table> .
              IF <fs_table>  IS ASSIGNED.
                  DATA(lv_count) = 0.
                LOOP AT  <fs_table>  ASSIGNING FIELD-SYMBOL(<fs_line>).
                  ASSIGN COMPONENT 'NAME' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_name>).
                  ASSIGN COMPONENT 'VALUE' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_value>).
                  IF <fs_name> IS ASSIGNED AND <fs_value> IS ASSIGNED.
                    lo_http_client->append_field_url(
                    EXPORTING
                        name  =  to_lower( |{ wa_structure_field-name }[{ lv_count }][name]| )
                        value =  to_lower( <fs_name> )
                    CHANGING
                        url   = r_url_param
                ).
                    lo_http_client->append_field_url(
                            EXPORTING
                                name  =  to_lower( |{ wa_structure_field-name }[{ lv_count }][value]| )
                                value =  CONV string( <fs_value> )
                            CHANGING
                                url   = r_url_param
                        ).
                        add 1 TO lv_count.
                  ENDIF.
                ENDLOOP.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
