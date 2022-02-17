class ZWDC_INSCRIP_EVENTO_ASS definition
  public
  inheriting from ZCL_WD_GENERAL_ASS
  create public .

public section.

  types:
    BEGIN OF tys_eventos,
      evtid     TYPE hrt9114-evtid,
      evtid_sec TYPE hrt9114-evtid,
    END OF tys_eventos .
  types:
    TYS_PARAMETROS TYPE TABLE OF zedu_c_param .

  data GT_PRECIOS type ZEDU_WD_T_PRECIOS_EVENTO .
  data GT_TIP_EVENTOS type ZEDU_WD_T_TIP_EVENTOS .
  data GT_BUT100 type BUP_T_BUT100 .
  data GO_MESSAGE_MAN type ref to IF_WD_MESSAGE_MANAGER .
  data GT_FECHAS type ZEDU_WD_T_FECHAS_EVENTO .
  data:
    GT_eventos type TABLE OF tys_eventos .
  data GT_PARAMETROS type TYS_PARAMETROS .
  data GT_VALORES_EVENTOS type ZHCM_T_EVENTO_VALORES .
  constants C_PLVAR type PLVAR value '01' ##NO_TEXT.
  constants C_EVENTO type OTYPE value 'E' ##NO_TEXT.
  constants C_MONEDA_DEFAULT type WAERS value 'COP' ##NO_TEXT.
  constants C_FILENAME type STRING value 'Factura.pdf' ##NO_TEXT.
  constants C_MIMETYPE type STRING value 'application/pdf' ##NO_TEXT.
  constants C_EXTENSION_PDF type STRING value '.pdf' ##NO_TEXT.
  constants C_DDBK_EMPRESA type STRING value 'Emp.' ##NO_TEXT.
  constants C_FILENAME_ZIP type STRING value 'Facturas.zip' ##NO_TEXT.
  data GT_TIPO_DESC type ZEDU_T_TIPO_DESC .
  data GV_HABEAS_DATA type XFELD .

  methods GET_PARTNER_FROM_DOC
    importing
      !IV_TYPE type BU_ID_TYPE
      !IV_IDNUMBER type BU_ID_NUMBER
    returning
      value(RT_PARTNER) type BU_PARTNER .
  methods INSCRIBIR_EVENTO
    importing
      !I_EVENTO type HROBJID
      !I_ID_PART type HRPARID
      !I_TYP_PART type PRTPE
      !I_BP type BU_PARTNER
      !I_GENERAR_DOC type XFELD
      !I_VAL_TOT_EVENT type ZEDU_VAL_TOT_EVENTO optional
    returning
      value(R_ERROR) type XFELD .
  methods GET_PRECIO
    importing
      !I_EVENTO type EVTID
    returning
      value(R_PRECIO) type EXKOS .
  methods CREATE_PARTNER
    importing
      !IS_DATOS_PERSONALES type ZEDU_S_DATOS_PERSONALES optional
      !IS_DATOS_DIRECCION type ZEDU_S_DIRECCION optional
      !IT_DATOS_DOCUMENTO type ZEDU_T_DATOS_DOCUMENTO optional
      !IV_ID_PART type HRPARID optional
      !IV_BP type BU_PARTNER optional
    exporting
      !EV_MESSAGE type STRING
    returning
      value(R_BP) type BU_PARTNER .
  methods GET_ROL_BP
    importing
      !I_BP type BU_PARTNER
      !I_ROL type BU_PARTNERROLE
    returning
      value(R_ERROR) type XFELD .
  methods GET_DESCUENTO
    importing
      value(I_TYP_PART) type PRTPE
      value(I_ID_PART) type HRPARID
      !I_NRO_DOC type STRING
      !I_EVENTO type HROBJID
    exporting
      !E_PORCENTAJE type Z_PORCEDESC
      !E_TIP_DESC type Z_IDTIPDESC
      !E_VALOR type Z_VALORDESC
      !E_DESCRIP type STRING .
  methods GET_DESCUENTO_X_DOC
    importing
      !I_EVENTO type EVTID
      !I_NRO_DOC type BPTAXNUM
    exporting
      !E_POR_DESC type Z_PORCEDESC
      !E_TIP_DESC type Z_IDTIPDESC
      !E_VAL_DESC type Z_VALORDESC .
  methods SET_DATOS_PAGOS
    importing
      !IV_EVENTO type HROBJID
      !I_ID_PART type HRPARID
      !I_TYP_PART type PRTPE
      !IV_EMPRESA type XFELD optional
      !IV_TOTAL type Z_IPRICE optional .
  methods ACTUALIZAR_VALOR_EVENTO
    importing
      !IS_VALOR_EVENTO type ZHCM_EVTO_VALORE optional
      !IT_VALORES_EVENTOS type ZHCM_T_EVENTO_VALORES optional
    returning
      value(RV_ERROR) type XFELD .
  methods ACTUALIZAR_VALOR_EVENTO_KU
    importing
      !IT_VALORES_EVENTOS type ZHCM_T_EVENTO_VALORES
    returning
      value(RV_ERROR) type XFELD .
  methods BUSCAR_RELACION_TIPDOC
    importing
      !I_TIP_DOC type BU_ID_TYPE optional
      !I_TIP_DOC_ARCH type CHAR20 optional
    exporting
      !E_TIP_DOC_BP type BU_ID_TYPE
      !E_TIP_DOC_IT type ICTYP .
  methods GET_CORREO
    importing
      !I_PARTICIPANTE type SOBID
      !I_TIPO_PARTICIPANTE type SCLAS
    returning
      value(R_CORREO) type SMTPAD_AVIS .
  methods BUSCAR_PARTICIPANTE
    importing
      !IV_NUMBERID type DATA
      !IV_PERS_EXT type XFELD optional
      !IV_TYPEID type DATA
    exporting
      !EV_ID_PART type HRPARID
      !EV_TIP_PART type PRTPE
      !EV_BP type BU_PARTNER
    returning
      value(R_ERROR) type XFELD .
  methods BUSCAR_DATA_PERSONAL
    importing
      !I_TIP_PART type PRTPE
      !I_ID_PART type HRPARID
    returning
      value(R_DATOS_PERSONALES) type ZSHCMPEXT .
  methods VALIDAR_EVENTO
    importing
      !IV_EVTID type HROBJID
      value(IV_ID_PART) type HRPARID
      !IV_TIP_PART type PRTPE
    returning
      value(R_ERROR) type XFELD .
  methods INFORMAR_FACTURA
    importing
      !IV_EVTID type HROBJID
      !IV_ID_PART type HRPARID
      !IV_TIP_PART type PRTPE
    returning
      value(RV_FACTURA) type STRING .
  methods GET_EVENTO_PPAL
    importing
      !IV_EVENTO_SEC type HROBJID
    exporting
      !EV_EVENTO_PPA type HROBJID .
  methods SET_EVENTOS_MASIVO
    importing
      !IT_EVENTOS type TIBR_OBJID .
  methods CREATE_CC
    importing
      !I_BP type BU_PARTNER
    returning
      value(R_ERROR) type STRING .
  methods LEER_PARAMETROS .
  methods VALIDAR_REGION
    importing
      !IV_LAND1 type LAND1
      !IV_REGIO type REGIO
    returning
      value(RV_ERROR) type XFELD .
  methods HAY_CUPO
    importing
      !IV_EVENTO type HROBJID
    returning
      value(RV_HAY_CUPO) type XFELD .
  methods GET_OTJID_TABLE
    importing
      !IV_OTYPE type OTYPE
      !IT_OBJID type TIBR_OBJID
    returning
      value(RT_OTJID) type ZEDU_T_OTJID .
  methods GET_OTJID
    importing
      !IV_OTYPE type OTYPE
      !IV_OBJID type HROBJID
    returning
      value(RV_OTJID) type HROTJID .
  methods VALIDAR_HABEAS_DATA
    importing
      !IV_TYPE type BU_ID_TYPE
      !IV_ID_NUMBER type BU_ID_NUMBER
    returning
      value(R_EXISTE) type XFELD .
  methods CREATE_HABEAS_DATA
    importing
      !IS_HABEAS_DATA type ZHCM_HABEAS_DATA .
  methods GET_PAGADOR_REF
    importing
      !IV_EVENTO type EVTID
      !IV_PARTICIPANTE type HRPARID
      !IV_PARTICIPANTE_TIPO type PRTPE
    changing
      value(CV_PAGADOR) type KUNRG
      !CV_REFERENCIA type XBLNR_KK
    exceptions
      FACTURA_NOT_FOUND .
  methods VALIDAR_KNVV
    importing
      !I_BP type BU_PARTNER
    returning
      value(RV_ERROR) type STRING .
  methods MODIFICAR_KNVV
    importing
      !I_BP type BU_PARTNER .
  methods FILL_DATA_PERSONAL_FROM_BP
    importing
      value(IV_BP) type BU_PARTNER
    changing
      value(CS_DATOS_PERSONALES) type ZSHCMPEXT .

  methods GET_DROPDOWN_KEY
    redefinition .
protected section.
private section.

  types:
    BEGIN OF tys_9114,
      otype TYPE hrp9114-otype,
      objid TYPE hrp9114-objid,
      seqnr TYPE hrp9114-seqnr,
      evtid TYPE hrt9114-evtid,
    END OF tys_9114 .
  types:
*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
    begin of ty_participante,
            type type prtpe,
            id   type hrparid,
          end of ty_participante .

  data:
    begin of gs_participante,
          hr_9113 type ty_participante,
          pa_0185 type ty_participante,
          bp_st   type ty_participante,
        end of gs_participante .
*  data GS_IDENTIFICACION type STRING .
*	End	  -->	MgM DCEK903285
  constants C_REL025 type RELAT value '025' ##NO_TEXT.

  methods CREATE_DEUDOR
    importing
      !I_BP type BU_PARTNER
    returning
      value(R_ERROR) type STRING .
  methods CREATE_MKK
    importing
      !I_BP type BU_PARTNER
    returning
      value(R_ERROR) type STRING .
  methods VALIDAR_CREACION_ROL
    importing
      !I_BP type BU_PARTNER
    returning
      value(R_ERROR) type STRING .
ENDCLASS.



CLASS ZWDC_INSCRIP_EVENTO_ASS IMPLEMENTATION.


  METHOD actualizar_valor_evento.

    IF is_valor_evento IS NOT INITIAL.
      MODIFY zhcm_evto_valore FROM is_valor_evento.
      IF sy-subrc IS NOT INITIAL.
        rv_error = abap_true.
      ENDIF.
    ENDIF.
    IF it_valores_eventos IS NOT INITIAL.
      MODIFY zhcm_evto_valore FROM TABLE it_valores_eventos.
      IF sy-subrc IS NOT INITIAL.
        rv_error = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method actualizar_valor_evento_ku.

    data lt_val_ku type standard table of zhcm_evto_val_ku.
    data lv_descuento type z_total.
    data lv_ident type bptaxnum.
    data lv_secuencia type seqnr.
    data:  begin of ls_desc_doc,
             porcent type zhcm_tipo_desc-porcdesc,
             valor   type zhcm_tipo_desc-valor,
             tipo    type zhcm_tipo_desc-codtipdesc,
           end of ls_desc_doc.

    "movemos datos a tabla de KU
    loop at it_valores_eventos
      assigning field-symbol(<fs_val>).

      at first.

        "recuperamos identificación del KU
        select single stcd1
          from kna1
            into lv_ident
              where kunnr eq <fs_val>-participante.

        if sy-subrc eq 0.

          "Recuperamos descuento para el KU (Empresa)
          me->get_descuento_x_doc(
            exporting
              i_evento   = <fs_val>-idevento
              i_nro_doc  = lv_ident
            importing
              e_por_desc = ls_desc_doc-porcent
              e_val_desc = ls_desc_doc-valor
              e_tip_desc = ls_desc_doc-tipo   ).

        endif.

        "Recuperamos última secuencia (similar a hrvpad25)
        select max( seqnr )
          from hrp1001
            into lv_secuencia
              where otype = c_evento
                and objid = <fs_val>-idevento
                and relat = cl_hrpiq00const=>c_relat_025
                and sobid = <fs_val>-participante
                and sclas = <fs_val>-otype.

        if sy-subrc ne 0.
          clear lv_secuencia.
        endif.

      endat.

      append initial line to lt_val_ku
        assigning field-symbol(<fs_val_ku>).

      if sy-subrc eq 0.
        move-corresponding <fs_val> to <fs_val_ku>.
        move lv_secuencia to <fs_val_ku>-seqnr.

        move ls_desc_doc-tipo     to <fs_val_ku>-codigo.
        move ls_desc_doc-valor    to <fs_val_ku>-valor.
        move ls_desc_doc-porcent  to <fs_val_ku>-porcentaje.

        "Calculamos montos con descuentos
        if <fs_val_ku>-porcentaje is not initial.

          lv_descuento  = <fs_val_ku>-neto
                        * ls_desc_doc-porcent
                        / 100.

          <fs_val_ku>-total = <fs_val_ku>-neto
                            - lv_descuento.

        else.

          <fs_val_ku>-total = <fs_val_ku>-neto
                            - ls_desc_doc-valor.

        endif.

      endif.

    endloop.

    if lt_val_ku[] is not initial.
      "Agregamos registro para su futura facturación
      modify zhcm_evto_val_ku
        from table lt_val_ku.

      if sy-subrc is not initial.
        rv_error = abap_true.
      endif.

    endif.

  endmethod.


  method buscar_data_personal.

    types: begin of tys_1034,
             objid type hrp1034-objid,
             anred type hrp1034-anred,
           end of tys_1034,

           begin of tys_1028,
             cname type hrp1028-cname,
             stras type hrp1028-stras,
             ort01 type hrp1028-ort01,
             pstlz type hrp1028-pstlz,
             land1 type hrp1028-land1,
           end of tys_1028,
           begin of tys_0002,
             vorna type pa0002-vorna,
             cname type pa0002-cname,
             anred type pa0002-anred,
           end of tys_0002,

           begin of tys_0006,
             land1 type pa0006-land1,
             ort01 type pa0006-ort01,
             pstlz type pa0006-pstlz,
             state type pa0006-state,
             stras type pa0006-stras,
             telnr type pa0006-telnr,
           end of tys_0006,

           begin of tys_9113,
             vorna       type hrt9113-vorna,
             cname       type hrt9113-cname,
             stras       type hrt9113-stras,
             bland       type hrt9113-bland,
             tel_number  type hrt9113-tel_number,
             mob_number  type hrt9113-mob_number,
             smtp_addr   type hrt9113-smtp_addr,
             procedencia type hrt9113-procedencia,
             gbort       type hrt9113-gbort,
           end of tys_9113,

           begin of tys_0185,
             iscot type pa0185-iscot,
             isspl type pa0185-isspl,
           end of tys_0185.
    data:
      ls_hrt9113 type tys_9113,
      ls_hrp1028 type tys_1028,
      ls_hrp1034 type tys_1034,
      ls_pa0002  type tys_0002,
      ls_pa0006  type tys_0006,
      ls_pa0105  type pa0105-usrid,
      ls_pa0185  type tys_0185,
      lv_otjid   type hrotjid,
      lv_id_part type hrobjid,
      lv_tabnr   type hrtabnr.


    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = i_id_part
      importing
        output = lv_id_part.


    case i_tip_part.
      when 'H' or 'PT'.
        select single tabnr
          from hrp9113
          into lv_tabnr
        where plvar eq me->c_plvar
          and objid eq lv_id_part.

        if sy-subrc is initial.

          select single vorna
                        cname
                        stras
                        bland
                        tel_number
                        mob_number
                        smtp_addr
                        procedencia
                        gbort
            from hrt9113
            into ls_hrt9113
          where tabnr eq lv_tabnr.
          if sy-subrc is not initial.
            clear ls_hrt9113.
          endif.

          concatenate i_tip_part
                      lv_id_part
                      into lv_otjid
                      respecting blanks.

          select single cname stras ort01 pstlz land1
            from hrp1028
            into ls_hrp1028
            where plvar eq me->c_plvar
              and otjid eq lv_otjid.
          if sy-subrc is not initial.
            clear ls_hrp1028.
          endif.

          select single objid anred
            from hrp1034
            into ls_hrp1034
            where plvar eq me->c_plvar
              and otjid eq lv_otjid.
          if sy-subrc is not initial.
            clear ls_hrp1034.
          endif.

          r_datos_personales-nombre          = ls_hrt9113-vorna.
          r_datos_personales-apellido        = ls_hrt9113-cname.
          r_datos_personales-direccion       = ls_hrt9113-stras.
          r_datos_personales-compl_direccion = ls_hrp1028-cname.
          r_datos_personales-calle2          = ls_hrp1028-stras.
          r_datos_personales-poblacion       = ls_hrp1028-ort01.
          r_datos_personales-cod_postal      = ls_hrp1028-pstlz.
          r_datos_personales-nacionalidad    =
          r_datos_personales-pais            = ls_hrp1028-land1.
          r_datos_personales-tratamiento     = ls_hrp1034-anred.
          r_datos_personales-region          = ls_hrt9113-bland.
          r_datos_personales-telefono        = ls_hrt9113-tel_number.
          r_datos_personales-fono_movil      = ls_hrt9113-mob_number.
          r_datos_personales-correo          = ls_hrt9113-smtp_addr.
          r_datos_personales-procedencia     = ls_hrt9113-procedencia.
          r_datos_personales-lugar_emision   = ls_hrt9113-gbort.
*r_datos_personales-FAX
*r_datos_personales-TITULO = ls_hrt9113-
*r_datos_personales-FECH_NAC = ls_hrt9113-

        endif.
      when 'P'.
        select single vorna
                      cname
                      anred
          from pa0002 "Datos personales
          into ls_pa0002
        where pernr eq lv_id_part.
        if sy-subrc is not initial.
          clear ls_pa0002.
        endif.

        select single land1
                      ort01
                      pstlz
                      state
                      stras
                      telnr
          from pa0006 "Datos direccion
          into ls_pa0006
        where pernr eq lv_id_part.
        if sy-subrc is not initial.
          clear ls_pa0006.
        endif.

        select single iscot
                      isspl
          from pa0185 "Identificacion
          into ls_pa0185
        where pernr eq lv_id_part.
        if sy-subrc is not initial.
          clear ls_pa0185.
        endif.

        select single usrid
          from pa0105 "Correo electronico
          into ls_pa0105
        where pernr eq lv_id_part
          and usrty eq '0010'.
        if sy-subrc is not initial.
          clear ls_pa0105.
        endif.


        r_datos_personales-nombre          = ls_pa0002-vorna.
        r_datos_personales-apellido        = ls_pa0002-cname.
        r_datos_personales-tratamiento     = ls_pa0002-anred.

        r_datos_personales-calle2          =
        r_datos_personales-direccion       = ls_pa0006-stras.
        r_datos_personales-cod_postal      = ls_pa0006-pstlz.
        r_datos_personales-poblacion       = ls_pa0006-ort01.
        r_datos_personales-nacionalidad    =
        r_datos_personales-pais            = ls_pa0006-land1.
        r_datos_personales-region          = ls_pa0006-state.
        r_datos_personales-telefono        = ls_pa0006-telnr.
*        r_datos_personales-compl_direccion = ls_pa0006-cname.
*        r_datos_personales-fono_movil      = ls_pa0006-.

        r_datos_personales-procedencia     = ls_pa0185-iscot.
        r_datos_personales-lugar_emision   = ls_pa0185-isspl.

        r_datos_personales-correo          = ls_pa0105.

*	Begin	-->	MgM DCEK903784 data de BP 14/03/2017
      when `ST`.

        "Recuperamos el código de BP
        select single partner
          from cmacbpst
            into @data(lv_bp)
              where stobjid eq @i_id_part.

          if sy-subrc eq 0.

            me->fill_data_personal_from_bp(
              exporting
                iv_bp               = lv_bp " Número de interlocutor comercial
              changing
                cs_datos_personales = r_datos_personales ).
          endif.

*	End	  -->	MgM DCEK903784
    endcase.

  endmethod.


  method buscar_participante.
    "lv_numberid TYPE BU_ID_NUMBER
    "lv_typeid  TYPE BU_ID_TYPE
    types: begin of tys_9113,
             otype type hrp9113-otype,
             objid type hrp9113-objid,
           end of tys_9113.

    data: lv_icnum   type psg_idnum,
          lv_stcd1   type stcd1,
          lt_9113    type table of tys_9113,
          ls_9113    type tys_9113,
          lv_partner type bu_partner,
          lv_otype   type otype,
*          ls_cmacbpst TYPE cmacbpst,
          lv_tip_doc type ictyp.

*	Begin	-->	MgM DCEK903938 validación 20/03/2017
    data lv_numberid  type bu_id_number.
    data lv_typeid    type bu_id_type .
*	End	  -->	MgM DCEK903938

    if iv_typeid   is not initial and
       iv_numberid is not initial.

*	Begin	-->	MgM DCEK903938 validación 20/03/2017
      lv_numberid = iv_numberid.
      lv_typeid   = iv_typeid.
*	End	  -->	MgM DCEK903938

*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
      clear me->gs_participante.
*	End	  -->	MgM DCEK903285

*    Busco al participante en el infotipo 9113
*    Persona de contacto
*    Persona Externa
      me->buscar_relacion_tipdoc(
        exporting
          i_tip_doc = lv_typeid
        importing
          e_tip_doc_it = lv_tip_doc
      ).

      if iv_pers_ext is initial.
        lv_otype = 'H'. "Persona Externa
      else.
        lv_otype = 'PT'. "Persona de Contacto
      endif.

      select hrp9113~otype
             hrp9113~objid
        from hrt9113
        inner join hrp9113
        on hrt9113~tabnr eq hrp9113~tabnr
        into table lt_9113
      where hrt9113~permo eq lv_tip_doc
        and hrt9113~perid eq lv_numberid.

      if sy-subrc is initial.

        clear ls_9113.
        loop at lt_9113 into ls_9113 where otype eq lv_otype. "#EC CI_STDSEQ
          exit.
        endloop.
        if sy-subrc is initial.
          ev_tip_part = ls_9113-otype.
          ev_id_part  = ls_9113-objid.
*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
          me->gs_participante-hr_9113-type  = ls_9113-otype.
          me->gs_participante-hr_9113-id    = ls_9113-objid.
*	End	  -->	MgM DCEK903285
        endif.
      endif.

*    Busco al participante como Empleado
*      if ev_id_part is initial.  "priorizar cuándo sea empleado "DCEK903285
      lv_icnum = lv_numberid.
      select single pernr
        from pa0185
        into ev_id_part
        where icnum eq lv_icnum.
      if sy-subrc is initial.
        ev_tip_part = 'P'.
*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
        me->gs_participante-pa_0185-type  = ev_tip_part.
        me->gs_participante-pa_0185-id    = ev_id_part.
*	End	  -->	MgM DCEK903285
      endif.
*      endif.

*    Busco al participante como PARTNER
*	Begin	-->	MgM DCEK902511 devuelvo BP siempre que haya 06/01/2017
*      IF ev_id_part IS INITIAL.
*	End	  -->	MgM DCEK902511
      me->get_partner_from_doc(
        exporting
          iv_type     = lv_typeid     " Clase de identificación
          iv_idnumber = lv_numberid   " Número de identificación
        receiving
          rt_partner  = lv_partner ). " Número de interlocutor comercial

      if lv_partner is not initial.

*	Begin	-->	MgM DCEK902511 devuelvo BP siempre que haya 06/01/2017
        ev_bp = lv_partner.
*	End	  -->	MgM DCEK902511

        select single stobjid
          from cmacbpst
          into ev_id_part
         where partner eq lv_partner.
        if sy-subrc is initial.
*            ev_id_part = ls_cmacbpst-stobjid.
          ev_tip_part = 'ST'.
*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
          me->gs_participante-bp_st-type  = ev_tip_part.
          me->gs_participante-bp_st-id  = ev_id_part.
*	End	  -->	MgM DCEK903285
        endif.
      endif.
*      ENDIF.

*    Busco al participante como Cliente
*      IF ev_id_part IS INITIAL.
*        lv_stcd1 = lv_numberid.
*
*        SELECT SINGLE kunnr
*          FROM kna1
*          INTO ev_id_part
*        WHERE stcd1 EQ lv_stcd1.
*        IF sy-subrc IS INITIAL.
*          ev_tip_part = 'KU'.
*        ENDIF.
*      ENDIF.

    endif.

*	Begin	-->	MgM DCEK903285 priorizar cuándo sea empleado 10/02/2017
    if gs_participante-pa_0185  is not initial.
      move gs_participante-pa_0185-type to ev_tip_part.
      move gs_participante-pa_0185-id   to ev_id_part.
    endif.
*	End	  -->	MgM DCEK903285

    if ev_id_part is initial.
      r_error = 'X'.
    endif.

  endmethod.


  METHOD buscar_relacion_tipdoc.
    IF i_tip_doc IS NOT INITIAL.
      CASE i_tip_doc.
        WHEN 'FS0001'. "'Cédula de ciudadanía'.
          e_tip_doc_it = '02'.
        WHEN 'FS0002'. "'Pasaporte'.
          e_tip_doc_it = '05'.
        WHEN 'FS0003'. "'Cédula de Extranjería'.
          e_tip_doc_it = '03'.
        WHEN 'FS0005'. "'Tarjeta de Identidad'.
          e_tip_doc_it = '04'.
        WHEN 'FS0006'. "'Registro Civil de Nacimiento'.
          e_tip_doc_it = '07'.
*    WHEN 'FS0007'. "'Numero Unico de Identificacion'.
*      e_tip_doc = ''.
*    WHEN 'FS0008'. "'Tarjeta de Extranjeria'.
*      e_tip_doc = ''.
        WHEN OTHERS.
          e_tip_doc_it = 'FS0001'.
      ENDCASE.
    ENDIF.
    IF i_tip_doc_arch IS NOT INITIAL.
      CASE i_tip_doc_arch.
        WHEN 'Cédula de ciudadanía'.
          e_tip_doc_bp = 'FS0001'.
        WHEN 'Pasaporte'.
          e_tip_doc_bp = 'FS0002'.
        WHEN 'Cédula de Extranjería'.
          e_tip_doc_bp = 'FS0003'.
        WHEN 'Tarjeta de Identidad'.
          e_tip_doc_bp = 'FS0005'.
        WHEN 'Registro Civil de Nacimiento'.
          e_tip_doc_bp = 'FS0006'.
        WHEN 'Numero Unico de Identificacion'.
          e_tip_doc_bp = 'FS0007'.
        WHEN 'Tarjeta de Extranjeria'.
          e_tip_doc_bp = 'FS0008'.
        WHEN OTHERS.
          e_tip_doc_bp = 'FS0001'.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD create_cc.
    DATA:
      ls_cc_info       TYPE bapifkkvkci,
      ls_cc_detail     TYPE bapifkkvki,
      lv_cont_acct     TYPE bapifkkvk-cont_acct,
      lt_partnerdetail TYPE TABLE OF bapifkkvkpi1,
      ls_partnerdetail TYPE bapifkkvkpi1,
      lv_gpart         TYPE fkkvkp-gpart,
      lt_return        TYPE TABLE OF bapiret2,
      ls_return        TYPE bapiret2.

*    SELECT SINGLE gpart
*      FROM fkkvkp
*      INTO lv_gpart
*    WHERE gpart EQ i_bp.
*
*    IF sy-subrc IS NOT INITIAL.

      ls_partnerdetail-buspartner =
      ls_cc_info-buspartner = i_bp.
      ls_cc_info-acct_cat = 'CO'.
      ls_cc_detail-acct_name = ''. "Denominacion

      APPEND ls_partnerdetail TO lt_partnerdetail.

      CALL FUNCTION 'BAPI_CTRACCOUNT_EASYCREATE'
        EXPORTING
          ctraccreateinfo    = ls_cc_info
          ctracdetail        = ls_cc_detail
        IMPORTING
          contractaccount    = lv_cont_acct
        TABLES
          ctracpartnerdetail = lt_partnerdetail
          return             = lt_return.

      SORT lt_return.
      READ TABLE lt_return INTO ls_return WITH KEY type = cl_hcp_global_constants=>c_error BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        r_error = ls_return-message.
      ELSE.
        CALL FUNCTION 'BAPI_CTRACPSOBJECT_EASYCREATE'
          EXPORTING
            psobjecttype = 'CO01'
            partner      = i_bp
            ctraccount   = lv_cont_acct
          TABLES
            return       = lt_return.
        SORT lt_return.
        READ TABLE lt_return INTO ls_return WITH KEY type = cl_hcp_global_constants=>c_error BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          r_error = ls_return-message.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
      ENDIF.
*    ELSE.
*      r_error = me->validar_knvv( i_bp = i_bp ).
*    ENDIF.

  ENDMETHOD.


  method create_deudor.
    data:
      ls_datos_deudor type zedu_s_deudor_cvi,
      lt_return       type bapiret2_t,
      ls_return       type bapiret2,
      ls_but100       type but100,
      lv_kunnr        type kunnr,
*      ls_knb1         type knb1, "-->  MgM DCEK903181
*      ls_knvv         type knvv, "-->  MgM DCEK903181
      ls_deudor       type zedu_s_datos_deudor_det.

    if me->get_rol_bp(
          exporting
            i_bp    = i_bp    " Número de interlocutor comercial
            i_rol   = 'FLCU00' " Función IC
        ) is not initial.

      ls_datos_deudor-partner = i_bp.
      ls_datos_deudor-tipo_tramite = 'C'.

      ls_datos_deudor-datos_basicos-akont = '1305050100'.
      ls_datos_deudor-datos_basicos-togru =
      ls_datos_deudor-datos_basicos-bukrs = 'UCES'.
      ls_datos_deudor-datos_basicos-zuawa = '001'.
      ls_datos_deudor-datos_basicos-fdgrv = 'D01'.
      ls_datos_deudor-datos_basicos-zterm = 'D00'.
*	Begin	-->	MgM DCEK902866 Datos default en tabla Z 18/01/2017
      select single *
        from zedu_c_org_vtas
          into corresponding fields of ls_deudor
            where vkorg eq `1000`
              and vtweg eq `16`
              and spart eq `16`.

      if sy-subrc ne 0.
*	End	  -->	MgM DCEK902866
        ls_deudor-vkorg   = '1000'.
        ls_deudor-vtweg   = '16'.
        ls_deudor-spart   = '16'.
        ls_deudor-bzirk   = '000001'.
        ls_deudor-konda   = '02'.
        ls_deudor-awahr   = '100'.
        ls_deudor-waers   = 'COP'.
        ls_deudor-ktgrd   = '01'.
        ls_deudor-zterm_2 = 'D00'.
        ls_deudor-vkgrp   = '005'.
        ls_deudor-vkbur   = 'P001'.

      endif.  "-->  MgM DCEK902866

      append ls_deudor to ls_datos_deudor-datos_org_vtas.

      call function 'Z_EDU_SET_DEUDOR'
        exporting
          is_deudor = ls_datos_deudor.

      call function 'Z_EDU_SAVE_ROL'
        exporting
          i_partner  = i_bp
          i_tipo_rol = 'C' " Cliente / deudor
        tables
          et_return  = lt_return.
      sort lt_return.
      read table lt_return into ls_return
          with key type = cl_hcp_global_constants=>c_error
          binary search.
      if sy-subrc is initial.
        r_error = abap_true.
        me->go_message_man->report_message(
          exporting
            message_text              = ls_return-message
        ).
      else.
        ls_but100-partner = i_bp.
        ls_but100-rltyp   = 'FLCU00'.
        append ls_but100 to me->gt_but100.
      endif.
    else.

      select single kunnr
        into lv_kunnr
        from knvv
      where kunnr eq i_bp
        and vkorg eq '1000'
*	Begin	-->	MgM DCEK902505 esquema cliente estandard 06/01/2017
        and kalks eq `1`
        and vtweg eq '16'
        and spart eq '16'.
*	End	  -->	MgM DCEK902505

      if sy-subrc is not initial.
*        ls_knb1-kunnr = i_bp.  "-->  MgM DCEK903181
        me->modificar_knvv( i_bp = i_bp ).

      endif.
*      r_error = me->validar_creacion_rol( i_bp = i_bp ).
    endif.
  endmethod.


  METHOD create_habeas_data.

    INSERT zhcm_habeas_data FROM is_habeas_data.

  ENDMETHOD.


  METHOD create_mkk.

    DATA:
      ls_datos_deudor  TYPE zedu_s_deudor_cvi,
      lt_return        TYPE bapiret2_t,
      ls_return        TYPE bapiret2,
      ls_but100        TYPE but100,
      lv_cont_acct     TYPE bapifkkvk-cont_acct.
      "ls_deudor        TYPE zedu_s_datos_deudor_det. "No utilizado DCEK902866

    IF me->get_rol_bp(
          EXPORTING
            i_bp    = i_bp    " Número de interlocutor comercial
            i_rol   = 'MKK' " Función IC
        ) IS NOT INITIAL.

      ls_datos_deudor-partner = i_bp.
      ls_datos_deudor-tipo_tramite = 'C'.

      CALL FUNCTION 'Z_EDU_SET_DEUDOR'
        EXPORTING
          is_deudor = ls_datos_deudor.

      CALL FUNCTION 'Z_EDU_SAVE_ROL'
        EXPORTING
          i_partner  = i_bp
          i_tipo_rol = 'M' "MKK
        TABLES
          et_return  = lt_return.
      sort lt_return.
      READ TABLE lt_return INTO ls_return
            WITH KEY type = cl_hcp_global_constants=>c_error
            BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        r_error = abap_true.
        me->go_message_man->report_message(
          EXPORTING
            message_text              = ls_return-message
        ).
      ELSE.

        ls_but100-partner = i_bp.
        ls_but100-rltyp   = 'MKK'.
        APPEND ls_but100 TO me->gt_but100.
      ENDIF.
*    ELSE.
*      r_error = me->validar_creacion_rol( i_bp = i_bp ).
    ENDIF.
  ENDMETHOD.


  METHOD create_partner.
    TYPES: BEGIN OF tys_but0id,
             partner TYPE but0id-partner,
             type    TYPE but0id-type,
           END OF tys_but0id.

    DATA:
      lv_bp              TYPE bu_partner,
      lt_but0id          TYPE TABLE OF tys_but0id,
      ls_but0id          TYPE tys_but0id,
      lt_but100          TYPE TABLE OF but100,
      ls_doc             TYPE zedu_s_datos_documento,
      ls_identification	 TYPE bapibus1006_identification,
      lt_return          TYPE bapiret2_t,
      ls_return          TYPE bapiret2,
      lt_datos_documento TYPE zedu_t_datos_documento. "HIRS 18-07-2017 - Incidente Educacion Continua


    IF it_datos_documento IS NOT INITIAL.
      "Crea una copia de los datos a consultar
      lt_datos_documento = it_datos_documento.  "HIRS 18-07-2017 - Incidente Educacion Continua
      "Elimina el tipo de documento ZPART
      DELETE lt_datos_documento                 "HIRS 18-07-2017 - Incidente Educacion Continua
        WHERE type = 'ZPART'.
      "Si aun quedan datos de documentos
      IF NOT lt_datos_documento IS INITIAL.     "HIRS 18-07-2017 - Incidente Educacion Continua
        SELECT partner type
          FROM but0id
          INTO TABLE lt_but0id
          FOR ALL ENTRIES IN lt_datos_documento
          WHERE type     EQ lt_datos_documento-type AND   "HIRS 18-07-2017 - Incidente Educacion Continua
                idnumber EQ lt_datos_documento-idnumber.  "HIRS 18-07-2017 - Incidente Educacion Continua

        IF sy-subrc IS NOT INITIAL.

          CALL FUNCTION 'Z_EDU_SET_DATA_TRM'
            EXPORTING
              is_datos_personales = is_datos_personales
              is_datos_direccion  = is_datos_direccion
              it_datos_documento  = it_datos_documento.

          CALL FUNCTION 'Z_EDU_SAVE_ALTA'
            IMPORTING
              ev_partner = lv_bp
            TABLES
              et_return  = lt_return.

          SORT lt_return.

          READ TABLE lt_return INTO ls_return WITH KEY type = cl_hcp_global_constants=>c_error BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ev_message = ls_return-message.
            EXIT.
          ENDIF.

        ELSE.

          READ TABLE lt_but0id INTO ls_but0id INDEX 1.  "#EC CI_NOORDER
          IF sy-subrc IS INITIAL.
            lv_bp = ls_but0id-partner.

            "Obtiene todos los tipos de documentos del BP y reemplaza el resultado anterior
            SELECT partner type       "HIRS 18-07-2017 - Incidente Educacion Continua
              FROM but0id             "HIRS 18-07-2017 - Incidente Educacion Continua
              INTO TABLE lt_but0id    "HIRS 18-07-2017 - Incidente Educacion Continua
              WHERE partner EQ lv_bp. "HIRS 18-07-2017 - Incidente Educacion Continua

            READ TABLE lt_but0id WITH KEY type = 'ZPART' TRANSPORTING NO FIELDS. "#EC CI_STDSEQ
            IF sy-subrc IS NOT INITIAL.
              READ TABLE it_datos_documento INTO ls_doc WITH KEY type = 'ZPART'. "#EC CI_STDSEQ
              IF sy-subrc IS INITIAL.

                ls_identification-identrydate     = sy-datum.
                ls_identification-idvalidfromdate = sy-datum.
                ls_identification-idvalidtodate   = '99991231'.
                ls_identification-country         = 'CO'.
                ls_identification-region          = '05'.

                CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
                  EXPORTING
                    businesspartner        = lv_bp
                    identificationcategory = ls_doc-type
                    identificationnumber   = ls_doc-idnumber
                    identification         = ls_identification
*	Begin	-->	MgM DCEK904116 crea primero BP 30/03/2017
                  TABLES
                    return                 = lt_return.

                READ TABLE lt_return
                  INTO ls_return
                    WITH KEY type = cl_hcp_global_constants=>c_error.

                IF sy-subrc IS INITIAL.
                  ev_message = ls_return-message.
                  EXIT.
                ELSE.
                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = cl_bp_const=>true.
                ENDIF.
*	End	  -->	MgM DCEK904116

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*	Begin	-->	MgM DCEK904116 crea cliente 30/03/2017
    ENDIF.

    IF iv_bp IS SUPPLIED.
      IF lv_bp IS INITIAL.
        lv_bp = iv_bp.
      ENDIF.
    ENDIF.

    CHECK lv_bp IS NOT INITIAL.
*	End	  -->	MgM DCEK904116

    ev_message = me->create_deudor( i_bp = lv_bp ).
    IF ev_message IS NOT INITIAL.
      EXIT.
    ENDIF.

    ev_message = me->create_mkk( i_bp = lv_bp ).
    IF ev_message IS NOT INITIAL.
      EXIT.
    ENDIF.

*      ev_message = me->create_cc( i_bp = lv_bp ).
    ev_message = validar_knvv( i_bp = lv_bp ).
    IF ev_message IS NOT INITIAL.
      EXIT.
    ENDIF.

    r_bp = lv_bp.
*    endif.

  ENDMETHOD.


  method fill_data_personal_from_bp.

    check iv_bp is not initial.

    select single name_first,
                  namemiddle,
                  name_lst2,
                  name_last2,
                  name_last,
                  a~title,
                  birthdt,
                  b~addrnumber,
                  c~city1,
                  c~post_code1,
                  c~street,
                  c~country,
                  c~region,
                  d~smtp_addr
      from but000 as a
        left outer join but021_fs as b
          on a~partner eq b~partner
        left outer join adrc as c
          on b~addrnumber eq c~addrnumber
        left outer join adr6 as d
          on b~addrnumber eq d~addrnumber and
             d~flgdefault eq @cl_bp_const=>true
          into @data(ls_datos_personales_bp)
            where a~partner eq @iv_bp
              and c~addr_group eq `BP`.

    if sy-subrc eq 0.

      select  r3_user,
              telnr_long
        from adr2
          into table @data(lt_telef)
            where addrnumber  eq @ls_datos_personales_bp-addrnumber.

      if sy-subrc eq 0.
        sort lt_telef by r3_user.
      endif.

      concatenate ls_datos_personales_bp-name_first
                  ls_datos_personales_bp-namemiddle
        into cs_datos_personales-nombre
          separated by space.

      concatenate ls_datos_personales_bp-name_last
                  ls_datos_personales_bp-name_lst2
        into cs_datos_personales-apellido
          separated by space.

      move ls_datos_personales_bp-street      to cs_datos_personales-direccion.
      move ls_datos_personales_bp-city1       to cs_datos_personales-poblacion.
      move ls_datos_personales_bp-post_code1  to cs_datos_personales-cod_postal.
      if cs_datos_personales-cod_postal is initial.
         cs_datos_personales-cod_postal = me->get_param(
                                            iv_repid      = 'ZWDC_INSCRIP_EVENTO'
                                            iv_idparam    = 'COD_POSTAL'
                                            iv_idparampos = '1' ).
      endif.
      move ls_datos_personales_bp-country     to cs_datos_personales-pais.
      move ls_datos_personales_bp-country     to cs_datos_personales-nacionalidad.
      move ls_datos_personales_bp-region      to cs_datos_personales-region.

      loop at lt_telef
        into data(ls_telef).

        case ls_telef-r3_user.
          when `1`.
            move ls_telef-telnr_long  to cs_datos_personales-telefono.
          when `3`.
            move ls_telef-telnr_long  to cs_datos_personales-fono_movil.
          when others.
        endcase.

      endloop.

      case ls_datos_personales_bp-title.
        when '0001'.
          cs_datos_personales-tratamiento = 'Sr.'.
        when '0002'.
          cs_datos_personales-tratamiento = 'Sra.'.
        when '0004'.
          cs_datos_personales-tratamiento = 'Dr.'.
        when '0005'.
          cs_datos_personales-tratamiento = 'Dra.'.
        when others.
      endcase.

      move ls_datos_personales_bp-smtp_addr to cs_datos_personales-correo.
      move ls_datos_personales_bp-birthdt   to cs_datos_personales-fech_nac.

    endif.

*PROCEDENCIA                                      1
*LUGAR_EMISION                                      Lugar emisión*

*	Begin	-->	MgM DCEK904177 fecha vacía en BP 31/03/2017
  if cs_datos_personales-fech_nac is initial.
    "colocamos fecha default para continuar con proceso de creación
    " de participante
    cs_datos_personales-fech_nac = `19000101`.
  endif.
*	End	  -->	MgM DCEK904177

  endmethod.


  METHOD get_correo.
    DATA:
      lv_partner    TYPE bu_partner,
      lv_addrnumber TYPE but020-addrnumber,
      lv_message    TYPE string.
*      lv_persnumber TYPE but020-persnumber.

    SELECT SINGLE partner
      FROM but0id
      INTO lv_partner
    WHERE type     EQ 'ZPART'
      AND idnumber EQ i_participante.
    IF sy-subrc is not INITIAL.
      CLEAR lv_partner.
    ENDIF.

    SELECT SINGLE addrnumber
      FROM but020
      INTO lv_addrnumber ", lv_persnumber)
    WHERE partner = lv_partner.
      IF sy-subrc is not INITIAL.
      CLEAR lv_addrnumber.
    ENDIF.

    SELECT SINGLE smtp_addr
      FROM adr6
      INTO r_correo
    WHERE addrnumber EQ lv_addrnumber.
*      AND persnumber EQ lv_persnumber.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e050(ZEDU_WD_MESSAGE) INTO lv_message.
      me->go_message_man->report_message(
        EXPORTING
          message_text              = lv_message ).
    ENDIF.



*  CASE i_tipo_participante.
*    WHEN 'H'. "Persona Externa
*      SELECT SINGLE hrt9113~smtp_addr
*        FROM hrp9113
*        INNER JOIN hrt9113
*        ON hrp9113~tabnr EQ hrt9113~tabnr
*        INTO r_correo
*      WHERE hrp9113~objid EQ i_participante.
*    WHEN 'P'.
*    WHEN OTHERS.
*  ENDCASE.

  ENDMETHOD.


  method get_descuento.

    data:
      lv_stat2      type piqhs_state,
      lv_tip_desc   type z_idtipdesc,
      lv_tip_desc_2 type z_idtipdesc,
      lv_perks      type persk,
      ls_parametros type zedu_c_param,
      lv_porc_typ   type z_porcedesc,
      lv_porc_id    type z_porcedesc,
      lv_valor_typ  type z_valordesc,
      lv_valor_id   type z_valordesc,
      lv_sobid      type hrp1001-sobid,
      lt_objid      type table of hrp1001-objid,
      lv_objid      type hrp1001-objid,
      lv_ident      type bptaxnum,
      ls_tipo_desc  type zhcm_tipo_desc_t,
      lt_sobid      type table of sobid,
      lt_objid_1769 type table of hrobjid,
      lt_objid_1222 type table of hrp1001-objid,
*      lt_tabnr      TYPE TABLE OF hrp1222-tabnr,
      lt_low        type table of om_attrval,
      lt_result_tab type table of swhactor,
      ls_result_tab type swhactor.

*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
    data  lv_valor_may    type z_valordesc.
    data  lv_porc_may     type z_porcedesc.
    data  lv_tip_desc_may type z_idtipdesc.                 "DCEK903500

*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
    types:  begin of lty_desc,
              porcent type zhcm_tipo_desc-porcdesc,
              valor   type zhcm_tipo_desc-valor,
              tipo    type zhcm_tipo_desc-codtipdesc,
            end of lty_desc.

    data ls_desc_doc type lty_desc.
    data lt_desc type standard table of lty_desc .

    types lty_ra_tip_desc type range of z_idtipdesc.
    data  lr_tip_desc     type range of z_idtipdesc.
    data  lr_s_tip_desc   type line of lty_ra_tip_desc.

    lr_s_tip_desc-option  = `EQ`.
    lr_s_tip_desc-sign    = `I`.
*	End	  -->	MgM DCEK903621

*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
    clear lr_tip_desc[].
*	End	  -->	MgM DCEK903621

    do 3 times. "en Do se llena rango lr_tip_desc con todos los descuentos

      case sy-index.
        when 1.
          i_typ_part = me->gs_participante-hr_9113-type.
          i_id_part  = me->gs_participante-hr_9113-id.
        when 2.
          i_typ_part = me->gs_participante-pa_0185-type.
          i_id_part  = me->gs_participante-pa_0185-id.
        when 3.
          i_typ_part = me->gs_participante-bp_st-type.
          i_id_part  = me->gs_participante-bp_st-id.
      endcase.
*	End	  -->	MgM DCEK903285

* Valido si tiene un descuento por tipo de participante
      case i_typ_part.
        when 'P'. "Persona (empleado)
*Valido si es un empleado Activo
          select single stat2
            from pa0000
              into lv_stat2
                where pernr eq i_id_part
                  and endda eq '99991231'
                  and stat2 eq '3'. "Activo

          if sy-subrc is initial.
*Obtengo el tipo de empleado
            select single persk
              from pa0001
                into lv_perks
                  where pernr eq i_id_part
                    and endda ge sy-datum.  "--> MgM DCEK903784

            if sy-subrc is initial.
              read table gt_parametros
                into ls_parametros
                  with key valor = lv_perks.             "#EC CI_STDSEQ

              if sy-subrc is initial.
                lv_tip_desc = ls_parametros-idparam.
*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
                lr_s_tip_desc-low = ls_parametros-idparam..
                append lr_s_tip_desc to lr_tip_desc.
*	End	  -->	MgM DCEK903621

              endif.
*            CASE lv_perks.
*              WHEN 'AT' "Adscritos son ad honoren.
*                OR 'DA' "Son administrativos y docentes.
*                OR 'DC' "Docente de cátedra.
*                OR 'MT' "Docentes de medio tiempo
*                OR 'TC' "Docente tiempo completo
*                OR 'TP'."Docente T. Parcial
*                lv_tip_desc = 'D018'.
*              WHEN 'HM' "Honorarios
*                OR 'IV'."Investigadores
*                lv_tip_desc = 'D020'.
*            ENDCASE.
            endif.
          endif.

        when 'ST'. "Estudiante

*------> DESCUENTO EGRESADOS
          select distinct sobid
            from hrp1001
              into table lt_sobid
                where otype eq 'ST'
                  and objid eq i_id_part.

          if sy-subrc is initial.

            loop at lt_sobid into lv_sobid.
              lv_objid = lv_sobid.
              append lv_objid to lt_objid_1769.
            endloop.

*	Begin	-->	MgM DCEK902600 recupera descuento 10/01/2017
            sort lt_objid_1769 by table_line.
            delete adjacent duplicates from lt_objid_1769.
*	End	  -->	MgM DCEK902600

            select objid
              from hrp1769
                into table lt_objid
                  for all entries in lt_objid_1769
                    where objid       eq lt_objid_1769-table_line
                      and end_process eq 'RV01'
                      and end_reason  eq '1018'.

          endif.

          if sy-subrc is initial.
            lv_tip_desc = 'D021'.
*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
            lr_s_tip_desc-low = 'D021'.
            append lr_s_tip_desc to lr_tip_desc.
*	End	  -->	MgM DCEK903621
          endif.
*          else.

*------> DESCUENTO ESTUDIANTES ACTIVOS ( posgrado y pregrado)
          select distinct sobid
            from hrp1001
*	Begin	-->	MgM DCEK904116 posibilidad de más de 1 estudio 29/03/2017
*              into lv_sobid
              into table lt_sobid
*	End	  -->	MgM DCEK904116
                where otype eq 'ST'
                  and objid eq i_id_part
                  and rsign eq 'A'
                  and relat eq '513'
                  and endda gt sy-datum.                    "DCEK903987

          if sy-subrc is initial.

*	Begin	-->	MgM DCEK903987 Valida si es egresado 22/03/2017
            "Consultamos si se encuentra activo el estudiante
*            select single objid
*              from hrp1771
*                into @data(lv_activo)
*                  where objid in (  select objid
*                                      from hrp1769
*                                        where objid       eq @lv_sobid
*                                          and beg_process eq 'RA01'
*                                          and endda       gt @sy-datum
*                                          and begda       le @sy-datum )
*                  and endda   gt @sy-datum
*                  and begda   le @sy-datum.
*            check sy-subrc eq 0 and
*                  lv_activo is not initial.
*
*            lv_objid = lv_sobid.

            clear lt_objid_1769[].

            loop at lt_sobid into lv_sobid.
              lv_objid = lv_sobid.
              append lv_objid to lt_objid_1769.
            endloop.

            select distinct h1~objid
              from hrp1771 as h1
                inner join hrp1769 as h9
                  on h1~objid eq h9~objid and
                     h9~beg_process eq `RA01` and
                     h9~endda       gt @sy-datum and
                     h9~begda       le @sy-datum
                into table @data(lt_activos)
                  for all entries in @lt_objid_1769
                    where h1~objid eq @lt_objid_1769-table_line
                      and h1~endda   gt @sy-datum
                      and h1~begda   le @sy-datum.

            check sy-subrc eq 0 and
                  lt_activos[] is not initial.

            "reviso las marcas de las O por cada activo
            loop at lt_activos
              into lv_objid.
*	End	  -->	MgM DCEK903987

              select single sobid
                from hrp1001
                  into lv_sobid
                    where otype eq 'CS'
                      and objid eq lv_objid
                      and rsign eq 'A'
                      and relat eq '514'.

              if sy-subrc is initial.
                lv_objid = lv_sobid.

                call function 'HRIQ_STRUC_GET'
                  exporting
                    act_otype      = 'SC'
                    act_objid      = lv_objid
                    act_wegid      = 'SC-O'
                    act_plvar      = '01'
                  tables
                    result_tab     = lt_result_tab
                  exceptions
                    no_plvar_found = 1
                    no_entry_found = 2
                    internal_error = 3
                    others         = 4.

                if sy-subrc is initial.
                  loop at lt_result_tab into ls_result_tab.
                    lv_objid = ls_result_tab-objid.
                    append lv_objid to lt_objid_1222.
                  endloop.

                  select hrt1222~low
                    from hrp1222
                      inner join hrt1222
                        on hrp1222~tabnr eq hrt1222~tabnr
                      into table lt_low
                        for all entries in lt_objid_1222
                          where hrp1222~plvar  eq me->c_plvar
                            and hrp1222~otype  eq 'O'
                            and hrp1222~objid  eq lt_objid_1222-table_line
                            and hrp1222~subty  eq '9000'
                            and hrt1222~attrib eq 'ZSLCM_O'.

                  if sy-subrc is initial.
                    sort lt_low.

                    read table lt_low
                      with key table_line = '002'
                        transporting no fields binary search.

                    if sy-subrc is initial.
                      lv_tip_desc = 'D016'.
*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
                      lr_s_tip_desc-low = 'D016'.
                      append lr_s_tip_desc to lr_tip_desc.
*	End	  -->	MgM DCEK903621
                    endif.

                    read table lt_low
                      with key table_line = '005'
                        transporting no fields binary search.

                    if sy-subrc is initial.
                      lv_tip_desc = 'D017'.
*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
                      lr_s_tip_desc-low = 'D017'.
                      append lr_s_tip_desc to lr_tip_desc.
*	End	  -->	MgM DCEK903621
                    endif.
                  endif.
                endif.
              endif.
            endloop.  "-->  MgM DCEK904116
          endif.
*          endif.
        when 'KU'. "Cliente
*      Sin descuento por Tipo
        when 'PT'. "Persona de Contacto
*      Sin descuento por Tipo
        when 'H'.  "Persona externa
*      Sin descuento por Tipo
        when others.
          continue.
      endcase.

    enddo.

*	Begin	-->	MgM DCEK903621 descuentos 01/03/2017
*      select single porcent valor
*           from zhcm_tipo_desc
*          into (lv_porc_typ, lv_valor_typ)
*          where tipo eq lv_tip_desc.
    if lr_tip_desc[] is not initial.

      select  porcdesc
              valor
              codtipdesc
        from zhcm_tipo_desc
          into table lt_desc
            where codtipdesc in lr_tip_desc.
*              order by  porcdesc descending
*                        valor    descending.

    endif.

*	End	  -->	MgM DCEK903621

*	Begin	-->	MgM DCEK903285 mayor descuento 03/02/2017
*    if sy-subrc is not initial.
*      clear: lv_porc_typ, lv_valor_typ.
*    endif.
*      if sy-subrc eq 0.
*
*        read table lt_desc
*          into data(ls_desc) index 1.

*          if sy-subrc eq 0.
*            move ls_desc-valor      to lv_valor_typ.
*            move ls_desc-porcent   to lv_porc_typ.
*            move ls_desc-tipo to lv_tip_desc.
*
*            if lv_porc_typ >= lv_porc_may.
*              lv_valor_may    = lv_valor_typ.
*              lv_porc_may     = lv_porc_typ.
*              lv_tip_desc_may = lv_tip_desc.                "DCEK903500
*            endif.
*          endif.
*      endif.

*    enddo.

*    lv_valor_typ = lv_valor_may.
*    lv_porc_typ  = lv_porc_may.
*    lv_tip_desc  = lv_tip_desc_may.                         "DCEK903500
*	End	  -->	MgM DCEK903285

    lv_ident = i_nro_doc.

    me->get_descuento_x_doc(
      exporting
        i_evento   = i_evento
        i_nro_doc  = lv_ident
      importing
        e_por_desc = ls_desc_doc-porcent
        e_val_desc = ls_desc_doc-valor
        e_tip_desc = ls_desc_doc-tipo
    ).

    if ls_desc_doc is not initial.
      append ls_desc_doc to lt_desc.
    endif.

    "ordenamos por mayor descuento
    sort lt_desc by porcent descending
                    valor   descending.

    "tomamos el mayor
    read table lt_desc
      into data(ls_desc) index 1.

    if sy-subrc eq 0.
      e_porcentaje   = ls_desc-porcent.
      e_valor        = ls_desc-valor.
      e_tip_desc     = ls_desc-tipo.
    endif.
**********************************************************************

*    if ls_desc_doc-porcent is initial and ls_desc-porcent is initial.
*      if ls_desc_doc-valor > ls_desc-valor.
*        e_porcentaje   = ls_desc_doc-porcent.
*        e_valor        = ls_desc_doc-valor.
*        e_tip_desc     = ls_desc_doc-tipo.
*      else.
*        e_porcentaje   = ls_desc-porcent.
*        e_valor        = ls_desc-valor.
*        e_tip_desc     = ls_desc-tipo.
*      endif.
*    else.
*      if ls_desc_doc-porcent > ls_desc-porcent.
*        e_porcentaje   = ls_desc_doc-porcent.
*        e_valor        = ls_desc_doc-valor.
*        e_tip_desc     = ls_desc_doc-tipo.
*      else.
*        e_porcentaje   = ls_desc-porcent.
*        e_valor        = ls_desc-valor.
*        e_tip_desc     = ls_desc-tipo.
*      endif.
*    endif.

    if me->gt_tipo_desc is initial.
      select *
        from zhcm_tipo_desc_t
        into table me->gt_tipo_desc.
      if sy-subrc is initial.
        sort me->gt_tipo_desc.
      endif.
    endif.

    read table me->gt_tipo_desc
      into ls_tipo_desc
        with key codtipdesc = e_tip_desc binary search.

    if sy-subrc is initial.
      e_descrip = ls_tipo_desc-denominacion.
    else.
      e_descrip = me->if_wd_component_assistance~get_text( '037' ).
    endif.

  endmethod.


  METHOD get_descuento_x_doc.

    SELECT SINGLE zhcm_tipo_desc~codtipdesc porcdesc valor
       FROM zhcm_descuentos
       INNER JOIN zhcm_tipo_desc
      ON zhcm_tipo_desc~codtipdesc EQ zhcm_descuentos~codtipdesc
      INTO (e_tip_desc, e_por_desc, e_val_desc)
      WHERE idevento  EQ i_evento AND
            numid     EQ i_nro_doc
        AND fecinicio <= sy-datum
        AND fecfinal  >= sy-datum.
    IF sy-subrc IS NOT INITIAL.
      CLEAR: e_tip_desc, e_por_desc, e_val_desc.
    ENDIF.


  ENDMETHOD.


  method get_dropdown_key.

    case im_field.

      when `EMPRESAS`.

        select  kunnr as value
                name1 as text
          from kna1
            into table rt_attr_value
              where ktokd eq 'ZORG'.

          if sy-subrc eq 0.
            sort rt_attr_value by text.
          endif.

      when others.

        call method super->get_dropdown_key
          exporting
            im_field      = im_field
            im_value      = im_value
            im_value_2    = im_value_2
            im_null_value = im_null_value
          receiving
            rt_attr_value = rt_attr_value.

    endcase.


  endmethod.


  method get_evento_ppal.
    data: ls_eventos type tys_eventos.

    read table gt_eventos
      into ls_eventos
        with key evtid_sec = iv_evento_sec
          binary search.

    if sy-subrc is initial.
      ev_evento_ppa = ls_eventos-evtid.
    else.
      select hrt9112~idevento   as evtid
             hrp9112~objid      as evtid_sec
        from hrp9112
          inner join hrt9112
            on hrp9112~tabnr eq hrt9112~tabnr
              appending table gt_eventos
                where hrp9112~objid eq iv_evento_sec
                  and hrp9112~tabnr ne space. "DCEK901922

      if sy-subrc is initial.
        sort gt_eventos by evtid_sec.

        read table gt_eventos
          into ls_eventos
            with key evtid_sec = iv_evento_sec
              binary search.

        if sy-subrc is initial.
          ev_evento_ppa = ls_eventos-evtid.
        endif.
      endif.
    endif.
  endmethod.


  METHOD get_otjid.
    DATA:
      lv_objid TYPE hrobjid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_objid
      IMPORTING
        output = lv_objid.

    CONCATENATE iv_otype
                lv_objid
                INTO rv_otjid
                RESPECTING BLANKS.

  ENDMETHOD.


  METHOD get_otjid_table.
    DATA:
      lv_objid TYPE hrobjid,
      lv_otjid TYPE hrotjid.
    LOOP AT it_objid INTO lv_objid.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_objid
        IMPORTING
          output = lv_objid.

      CONCATENATE iv_otype
                  lv_objid
                  INTO lv_otjid
                  RESPECTING BLANKS.
      APPEND lv_otjid to rt_otjid.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_pagador_ref.
    CONSTANTS: lco_hrv_status_4 TYPE hrv_status VALUE '4', "Transferencia confirmada
               lco_hrv_event_f  TYPE hrv_event VALUE 'F'.
*    SELECT SINGLE document
*                  payer
*      FROM t77refdoc
*        INTO (cv_referencia,cv_pagador)
*          WHERE eotyp = `E`
*            AND eveid = iv_evento
*            AND otype = iv_participante_tipo
*            AND sobid = iv_participante.

    "Obtener referencias para evento y participante
    SELECT refdocno,refdocitem,refdocseqnr,docdate,postingdate,document,event,status,payer
      INTO TABLE @DATA(lt_t77refdoc)
      FROM t77refdoc
      WHERE eotyp = `E`
        AND eveid = @iv_evento
        AND otype = @iv_participante_tipo
        AND sobid = @iv_participante.

    IF sy-subrc NE 0.
      MESSAGE e040(zedu_wd_message)
        RAISING factura_not_found.
    ENDIF.

    "Eliminar documentos que no sean de transferencia confirmada
    DELETE lt_t77refdoc WHERE status <> lco_hrv_status_4.

    "Elimininar documentos que nos sean de facturación del evento
    DELETE lt_t77refdoc WHERE event <> lco_hrv_event_f.

    "Ordener por fecha
    SORT lt_t77refdoc BY postingdate DESCENDING.

    READ TABLE lt_t77refdoc INTO DATA(ls_t77refdoc) INDEX 1.

    cv_pagador = ls_t77refdoc-payer.
    cv_referencia = ls_t77refdoc-document.
  ENDMETHOD.


  METHOD get_partner_from_doc.

    SELECT SINGLE partner
      FROM but0id
      INTO rt_partner
    WHERE type     EQ iv_type
      AND idnumber EQ iv_idnumber.
    IF sy-subrc is not INITIAL.
      CLEAR rt_partner.
    ENDIF.

  ENDMETHOD.


  METHOD get_precio.
    DATA: ls_precio TYPE zedu_wd_precios_evento,
          lv_otjid  TYPE hrotjid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = i_evento
      IMPORTING
        output = ls_precio-evento.

    READ TABLE me->gt_precios INTO ls_precio WITH KEY evento = ls_precio-evento BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      r_precio = ls_precio-precio.
    ELSE.

      CONCATENATE me->c_evento
                  ls_precio-evento
                  INTO lv_otjid
                  RESPECTING BLANKS.

      SELECT SINGLE ekost
         FROM hrp1021
         INTO r_precio
        WHERE plvar EQ me->c_plvar
          AND otjid EQ lv_otjid.
*        objid EQ ls_precio-evento.
      IF sy-subrc IS INITIAL.
        ls_precio-precio = r_precio.
        APPEND ls_precio TO  me->gt_precios.
        SORT me->gt_precios.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_rol_bp.

    READ TABLE gt_but100 WITH KEY partner = i_bp TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      SELECT * FROM but100
      INTO TABLE gt_but100
      WHERE partner EQ i_bp.
      IF sy-subrc IS INITIAL.
        SORT gt_but100.
      ENDIF.
    ENDIF.

    READ TABLE gt_but100 WITH KEY rltyp = i_rol TRANSPORTING NO FIELDS. "#EC CI_STDSEQ
    IF sy-subrc IS NOT INITIAL.
      r_error = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD hay_cupo.
    TYPES: BEGIN OF tys_1024,
             begda TYPE hrp1024-begda,
             endda TYPE hrp1024-endda,
             kapz3 TYPE hrp1024-kapz3,
           END OF tys_1024.
    TYPES: BEGIN OF tys_1000,
             objid TYPE hrp1001-objid,
             begda TYPE hrp1001-begda,
             endda TYPE hrp1001-endda,
             relat TYPE hrp1001-relat,
           END OF tys_1000.

    DATA:
      lt_inscriptos TYPE TABLE OF tys_1000,
      ls_inscriptos TYPE tys_1000,
      lt_1024       TYPE TABLE OF tys_1024,
      ls_1024       TYPE tys_1024,
      lv_otjid      TYPE hrotjid,
      lv_evento     TYPE hrobjid,
      lv_lineas     TYPE i.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_evento
      IMPORTING
        output = lv_evento.

    CONCATENATE me->c_evento
                lv_evento
                INTO lv_otjid
                RESPECTING BLANKS.

    SELECT objid
           begda
           endda
           relat
      FROM hrp1001
      INTO TABLE lt_inscriptos
    WHERE plvar EQ me->c_plvar
      AND otjid EQ lv_otjid.

    IF sy-subrc IS INITIAL.
      LOOP AT lt_inscriptos INTO ls_inscriptos WHERE relat NE me->c_rel025. "#EC CI_STDSEQ
        DELETE lt_inscriptos INDEX sy-tabix.
      ENDLOOP.

      DESCRIBE TABLE lt_inscriptos LINES lv_lineas.

      SELECT begda endda kapz3
        FROM hrp1024
        INTO TABLE lt_1024
      WHERE plvar EQ me->c_plvar
        AND objid EQ lv_otjid.
      IF sy-subrc IS INITIAL.
        LOOP AT lt_1024 INTO ls_1024 WHERE begda <= sy-datum
                                       AND endda >= sy-datum. "#EC CI_STDSEQ
          EXIT.
        ENDLOOP.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE lt_1024 INTO ls_1024 INDEX 1. "#EC CI_NOORDER
        ENDIF.
        IF sy-subrc IS INITIAL AND ls_1024-kapz3 > lv_lineas.
          rv_hay_cupo = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      rv_hay_cupo = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD informar_factura.
    TYPES: BEGIN OF tys_factura,
             document TYPE t77refdoc-document,
             status   TYPE t77refdoc-status,
           END OF tys_factura.
    DATA:
      lt_document TYPE TABLE OF tys_factura,
      ls_document TYPE tys_factura,
      lv_message  TYPE string.

    SELECT document status
      FROM t77refdoc
      INTO TABLE lt_document                  "Status - Anulados
    WHERE eotyp EQ me->c_evento               "5 Anulación reservada
      AND eveid EQ iv_evtid                   "6 Anulación con errores
      AND otype EQ iv_tip_part                "7 Anulación efectuada
      AND sobid EQ iv_id_part                 "8 Anulación confirmada
      AND NOT ( status BETWEEN 5 AND 8 ).     "#EC CI_NOFIRST
    IF sy-subrc IS INITIAL.
      READ TABLE lt_document INTO ls_document INDEX 1. "#EC CI_NOORDER
      if sy-subrc is initial.
*        IF ls_document-status EQ '4'.
*          CONCATENATE 'Factura'
*                      ls_document-document
*                      'pagada'
*                      INTO lv_message
*                      SEPARATED BY space.
*        ELSE.
*          CONCATENATE 'Factura'
*                      ls_document-document
*                      'pendiente de pago'
*                    INTO lv_message
*                    SEPARATED BY space.
*        ENDIF.

*          CONCATENATE 'Factura Nro'
*                      ls_document-document
*                      INTO lv_message
*                      SEPARATED BY space.
*
*        me->go_message_man->report_message(
*          EXPORTING
*            message_text              = lv_message
*        ).
        rv_factura = ls_document-document.

      ENDIF.
    ENDIF.
  ENDMETHOD.


method inscribir_evento.

  data:
    lt_n1001   type table of p1001,
    ls_n1001   type p1001,
    ls_nad25   type pad25,
    lt_ilfcod  type table of ilfcod,
    ls_ilfcod  type ilfcod,
    lv_flag    type c,
    lv_message type string,
    lv_otjid   type hrotjid,
    ls_fechas  type zedu_wd_s_fechas_evento,
    lt_refdoc  type table of  t77refdoc,
    lv_obj10   type char10,
    lv_event   type hrobjid,
    ls_refdoc  type t77refdoc.

  if i_bp is initial.
    r_error = abap_true.
    exit.
  endif.

  clear ls_ilfcod.
  refresh lt_ilfcod.
  if i_typ_part ne 'KU'.
    ls_ilfcod-tabix = 1.
  endif.
  ls_ilfcod-lfcod = 'BOKP'.
  append ls_ilfcod to lt_ilfcod.

  ls_nad25-manzl = '1'.
  ls_nad25-intex = me->c_evento.

  ls_nad25-budat = sy-datum.
  ls_nad25-event = 'F'.

  if i_generar_doc is not initial.

    call function 'RH_REFDOC_NUMBER_GET_NEXT'
      importing
        out_refdocno          = ls_refdoc-refdocno
      exceptions
        number_get_next_error = 1
        others                = 2.

    ls_refdoc-refdocitem     = '000'.
    ls_refdoc-refdocseqnr    = '001'.
    ls_refdoc-event          = 'F'.
    ls_refdoc-sold_to        = i_bp.
    ls_refdoc-payer          = i_bp.
    ls_refdoc-bill_to        = i_bp.
    ls_refdoc-ship_to        = i_bp.
*	Begin	-->	MgM DCEK904078 asigna Id estudiante 27/03/2017
    ls_refdoc-otype          = i_typ_part.
    ls_refdoc-sobid          = i_id_part.
*	End	  -->	MgM DCEK904072
    append ls_refdoc to lt_refdoc.

    call function 'RH_REFDOC_INSERT'
      exporting
        in_vtask              = 'D'
      tables
        in_refdoctab          = lt_refdoc
      exceptions
        invalid_vtask         = 1
        no_data_to_process    = 2
        key_data_initial      = 3
        admin_data_initial    = 4
        refdoc_already_exists = 5
        refdoc_insert_error   = 6
        others                = 7.

    if sy-subrc is initial.
      ls_nad25-refdocno = ls_refdoc-refdocno.
    endif.

    read table me->gt_fechas
      into ls_fechas
        with key evento = i_evento
          binary search.

    if sy-subrc is initial.
      ls_nad25-kwaer = ls_fechas-moneda.
    else.
      ls_nad25-kwaer = me->c_moneda_default.
    endif.

    if i_val_tot_event is initial.
      ls_nad25-kkost = me->get_precio( i_evento = i_evento )."Precio
    else.
      ls_nad25-kkost = i_val_tot_event.
    endif.

  endif.

  call method cl_hr_adata_type_cast=>padnn_to_adata "YSBUNI
    exporting
      padnn = ls_nad25
    importing
      adata = ls_n1001-adata.

  ls_n1001-plvar = me->c_plvar.
  ls_n1001-relat = me->c_rel025.
  ls_n1001-infty = '1001'.
  ls_n1001-rsign = 'A'.
  ls_n1001-priox = '50'.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = i_evento
    importing
      output = lv_event.

  concatenate me->c_evento
              lv_event
              into lv_otjid
              respecting blanks.

  select single begda
                endda
    from hrp1000
      into (ls_n1001-begda,
            ls_n1001-endda )
        where plvar eq me->c_plvar
          and otjid eq lv_otjid.

  if sy-subrc is not initial.
    clear:
      ls_n1001-begda,
      ls_n1001-endda.
  endif.

  ls_n1001-istat = '2'.
  ls_n1001-otype = me->c_evento.
  ls_n1001-objid = lv_event.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = ls_n1001-objid
    importing
      output = ls_n1001-objid.

  ls_n1001-sclas = i_typ_part.

  if i_typ_part eq 'PT'.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = i_id_part
      importing
        output = lv_obj10.
    ls_n1001-sobid = lv_obj10.
  else.
    ls_n1001-sobid = i_id_part.
  endif.

  concatenate ls_n1001-sclas
              ls_n1001-sobid
    into ls_n1001-varyf.

  append ls_n1001 to lt_n1001.

  call function 'ENQUEUE_EZ_ENQUE_1001'
    exceptions
      foreign_lock   = 1
      system_failure = 2
      others         = 3.

  if sy-subrc is not initial.

    do 5 times.

      wait up to 1 seconds.

      call function 'ENQUEUE_EZ_ENQUE_1001'
        exceptions
          foreign_lock   = 1
          system_failure = 2
          others         = 3.

      if sy-subrc is initial.
        lv_flag = abap_true.
        exit.
      endif.
    enddo.

    if lv_flag is initial.
      message e039(zedu_wd_message)
        into lv_message.

      me->go_message_man->report_message(
        exporting
          message_text = lv_message ).
    endif.
  endif.

*  calcular inscripcion HRP1024
  if me->hay_cupo( iv_evento = i_evento ) is not initial or
     i_typ_part eq 'KU'.

    call function 'RH_INSERT_INFTY'
      exporting
        fcode               = 'INSE'
        vtask               = 'D'
        workf_actv          = 'X'
      tables
        innnn               = lt_n1001
        ilfcode             = lt_ilfcod
      exceptions
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        others              = 6.

*	Begin	-->	MgM DCEK904116 issues carga masiva 29/03/2017
    if sy-subrc eq 0.

      "&1 &2 Inscrito OK
      message e080(zedu_wd_message)
       with i_typ_part
            i_id_part
         into lv_message.

      me->go_message_man->report_message(
        exporting message_type  = if_wd_message_manager=>co_type_standard
                  message_text  = lv_message ).

    endif.
*	End	  -->	MgM DCEK904116

  else.
    message e038(zedu_wd_message) with i_evento into lv_message.
    me->go_message_man->report_message(
        exporting
          message_text = lv_message ).
    r_error = abap_true.
  endif.
  call function 'DEQUEUE_EZ_ENQUE_1001'.

endmethod.


  METHOD leer_parametros.
    SELECT *
      FROM zedu_c_param
      INTO TABLE gt_parametros
    WHERE repid EQ 'ZWDC_INSCRIP_EVENTO'.
    IF sy-subrc IS NOT INITIAL.
      REFRESH gt_parametros.
    ENDIF.
  ENDMETHOD.


  method modificar_knvv.
    data:
      lv_kunnr type kunnr,
      ls_knvv  type knvv,
      ls_knb1  type knb1.

    select single kunnr
        into lv_kunnr
        from knvv
      where kunnr eq i_bp
        and vkorg eq '1000'
        and kalks eq `1`
        and vtweg eq '16'
        and spart eq '16'.

    if sy-subrc is not initial.
*	Begin	-->	MgM DCEK903181 datos clientes 31/01/2017
*      ls_knb1-kunnr = i_bp.
*      ls_knb1-bukrs =
*      ls_knb1-togru = 'UCES'.
*      ls_knb1-zuawa = '001'.
*      ls_knb1-akont = '1305050100'.
*      ls_knb1-zterm = 'D00'.
*      ls_knb1-fdgrv = 'D01'.
*
*      MODIFY knb1 FROM ls_knb1.
*
*      ls_knvv-kunnr = i_bp.
*      ls_knvv-kalks = '1'.
*      ls_knvv-vkorg = '1000'.
*      ls_knvv-vtweg = '16'.
*      ls_knvv-spart = '16'.
*      ls_knvv-bzirk = '000001'.
*      ls_knvv-konda = '02'.
*      ls_knvv-awahr = '100'.
*      ls_knvv-waers = 'COP'.
*      ls_knvv-ktgrd = '01'.
*      ls_knvv-zterm = 'D00'.
*      ls_knvv-vkgrp = '005'.
*      ls_knvv-vkbur = 'P001'.
*      ls_knvv-lprio = '02'.
*      ls_knvv-vsbed = '01'.

      "Recuperamos datos de tabla z de customizing
      select single *
        from zedu_c_org_vtas
          into corresponding fields of ls_knvv
            where vkorg = `1000`
              and vtweg = `16`
              and spart = `16`.

      if sy-subrc eq 0.

        ls_knvv-kunnr = i_bp.
        ls_knvv-ernam = sy-uname.
        ls_knvv-erdat = sy-datum.
        modify knvv from ls_knvv.

      endif.

    endif.

    select single *
      from knb1
        into ls_knb1
          where kunnr eq i_bp
            and bukrs eq `UCES`.

    if sy-subrc ne 0.
      "solo si no existe el dato cargamos valores y cuentas básicas
      ls_knb1-kunnr = i_bp.
      ls_knb1-bukrs =
      ls_knb1-togru = 'UCES'.
      ls_knb1-zuawa = '001'.
      ls_knb1-akont = '1305050100'.
      ls_knb1-zterm = 'D00'.
      ls_knb1-fdgrv = 'D01'.

      insert knb1 from ls_knb1.

    endif.
*	End	  -->	MgM DCEK903181

  endmethod.


  method set_datos_pagos.

    data:
      ls_pt9114 type pt9114,
      lv_otype  type otype,
      ls_fechas type zedu_wd_s_fechas_evento,
      lv_evtid  type hrt9114-evtid,
      ls_9114   type tys_9114,
      ls_key    type hripkey,
      lv_objid  type hrobjid.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = iv_evento
      importing
        output = ls_pt9114-evtid.

    if iv_empresa is initial.
      ls_pt9114-xfeld     = abap_false.
      if iv_total is initial.
        ls_pt9114-valorpend = me->get_precio( i_evento = iv_evento ).
      else.
        ls_pt9114-valorpend = iv_total.
      endif.
    else.
      ls_pt9114-xfeld     = abap_true.
      ls_pt9114-valorpend = 0.
    endif.
*	Begin	-->	MgM DCEK902759 marca pagado en eventos gratuitos 13/01/2017
    if ls_pt9114-valorpend is initial.
      ls_pt9114-xfeld     = abap_true.
    endif.
*	End	  -->	MgM DCEK902759
    read table me->gt_fechas into ls_fechas with key evento = iv_evento binary search.
    if sy-subrc is initial.
      ls_pt9114-end_time  = ls_fechas-hora.
      ls_pt9114-evdat = ls_fechas-fecha_inicio.
      ls_pt9114-endda = ls_fechas-fecha.
    endif.
    lv_objid = i_id_part.
    lv_otype = i_typ_part.

    ls_key-plvar = '01'.
    ls_key-otype = i_typ_part.
    ls_key-objid = lv_objid.
    ls_key-infty = '9114'.
    ls_key-istat = '1'.
    ls_key-begda = sy-datum.
    ls_key-endda = '99991231'.
    concatenate me->c_evento
                iv_evento
                into ls_key-varyf
                separated by space.

    call function 'ZMF_ACTUALIZAR_ITXXXX'
      exporting
        is_xxxx    = ls_pt9114
        i_objid    = lv_objid
        i_infotype = '9114'
        is_key     = ls_key
        i_otype    = i_typ_part
      exceptions
        error      = 1
        others     = 2.
    if sy-subrc <> 0.
    endif.

  endmethod.


  method set_eventos_masivo.
    types: begin of tys_vencimiento,
             evento      type hrp9112-objid,
             ordinterna  type hrt9112-ordinterna,
             vencimiento type hrt9112-vencimiento,
           end of tys_vencimiento,
           begin of tys_1000,
             objid type hrp1000-objid,
             begda type hrp1000-begda,
           end of tys_1000,
           begin of tys_1021,
             objid type hrp1021-objid,
             ewaer type hrp1021-ewaer,
           end of tys_1021.


    data:
      lt_vencimiento type table of tys_vencimiento,
      ls_vencimiento type tys_vencimiento,
      lt_1000        type table of tys_1000,
      ls_1000        type tys_1000,
      lt_1021        type table of tys_1021,
      ls_1021        type tys_1021,
      lt_otjid       type table of otjid.

    field-symbols: <fs_fechas> type zedu_wd_s_fechas_evento.

    if it_eventos is not initial.

      select hrp9112~objid as evento
             hrt9112~ordinterna
             hrt9112~vencimiento
        from hrt9112
        inner join hrp9112
      on hrt9112~tabnr eq hrp9112~tabnr
        into table lt_vencimiento
      for all entries in it_eventos
      where hrp9112~objid eq it_eventos-table_line
        and hrp9112~tabnr ne space.                         "DCEK903531

      if sy-subrc is initial.

        me->get_otjid_table(
          exporting
            iv_otype = me->c_evento
            it_objid = it_eventos
          receiving
            rt_otjid = lt_otjid ).
        if lt_otjid is not initial.
          select objid
                 ewaer
            from hrp1021
            into table lt_1021
            for all entries in lt_otjid
          where plvar eq me->c_plvar
            and otjid eq lt_otjid-table_line.
          if sy-subrc is not initial.
            refresh lt_1021.
          else.
            sort lt_1021.
          endif.


          select objid
                 begda
            from hrp1000
            into table lt_1000
            for all entries in lt_otjid
          where plvar eq me->c_plvar
            and otjid eq lt_otjid-table_line.
          if sy-subrc is not initial.
            refresh lt_1000.
          else.
            sort lt_1000.
          endif.
        endif.

        loop at lt_vencimiento into ls_vencimiento.
*	Begin	-->	MgM DCEK903531 Fechas vacías 16/02/2017
          at first.
            "limpio tabla de fechas de evento
            clear me->gt_fechas[].
          endat.
*	End	  -->	MgM DCEK903531
          append initial line to me->gt_fechas assigning <fs_fechas>.
          <fs_fechas>-evento = ls_vencimiento-evento.
          <fs_fechas>-fecha  = ls_vencimiento-vencimiento.
          <fs_fechas>-hora   = ls_vencimiento-vencimiento+8.

          read table lt_1000 into ls_1000 with key objid = <fs_fechas>-evento binary search.
          if sy-subrc is initial.
            <fs_fechas>-fecha_inicio = ls_1000-begda.
          endif.
          read table lt_1021 into ls_1021 with key objid = <fs_fechas>-evento binary search.
          if sy-subrc is initial.
            if ls_1021-ewaer is initial.
              ls_1021-ewaer = me->c_moneda_default.
            endif.
            <fs_fechas>-moneda = ls_1021-ewaer.
          else.
            <fs_fechas>-moneda = me->c_moneda_default.
          endif.
        endloop.
      endif.
    endif.

    sort me->gt_fechas.

  endmethod.


  METHOD validar_creacion_rol.

    types: BEGIN OF tys_adrc,
      country TYPE adrc-country,
      region  TYPE adrc-region,
      END OF tys_adrc.

    DATA:
      lv_addrnumber TYPE but020-addrnumber,
      lv_kunnr      TYPE kunnr,
      ls_adrc       TYPE tys_adrc,
      lv_message    TYPE string.

    SELECT SINGLE addrnumber
      FROM but020
      INTO lv_addrnumber
      WHERE partner EQ i_bp.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE country region
        FROM adrc
        INTO ls_adrc
        WHERE addrnumber EQ lv_addrnumber.

      IF sy-subrc IS INITIAL.
        IF ls_adrc-region IS INITIAL.
          MESSAGE e035(zedu_wd_message) INTO lv_message.
          me->go_message_man->report_message(
            EXPORTING
              message_text = lv_message ).
          r_error = abap_true.
        ENDIF.
        IF ls_adrc-country IS INITIAL.
          MESSAGE e036(zedu_wd_message) INTO lv_message.
          me->go_message_man->report_message(
            EXPORTING
              message_text = lv_message ).
          r_error = abap_true.
        ENDIF.
*        IF ls_adrc-post_code1 IS INITIAL.
*          MESSAGE e037(zedu_wd_message) INTO lv_message.
*          me->go_message_man->report_message(
*            EXPORTING
*              message_text = lv_message ).
*          r_error = abap_true.
*        ENDIF.
      ENDIF.
    ENDIF.

    SELECT SINGLE kunnr
      FROM knvv
      INTO lv_kunnr
    WHERE kunnr EQ i_bp.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e034(zedu_wd_message) INTO lv_message.
      me->go_message_man->report_message(
            EXPORTING
              message_text = lv_message ).
      r_error = abap_true.
    ENDIF.

    IF r_error IS NOT INITIAL.
      MESSAGE e033(zedu_wd_message) INTO r_error.
    ENDIF.
  ENDMETHOD.


  METHOD validar_evento.

    DATA lv_objid       TYPE hrobjid.
    DATA lv_message     TYPE string.
    DATA lv_otjid       TYPE hrotjid.
    DATA lv_id_part_n10 TYPE n LENGTH 10.

    CONCATENATE me->c_evento
                iv_evtid
                INTO lv_otjid
                SEPARATED BY space.

    SELECT SINGLE  objid
      INTO lv_objid
      FROM hrp1000
    WHERE plvar EQ me->c_plvar
      AND otjid EQ lv_otjid.

    IF sy-subrc IS NOT INITIAL.
      r_error = abap_true.
      MESSAGE e032(zedu_wd_message)
              WITH iv_evtid
              INTO lv_message.
      me->go_message_man->report_message(
          EXPORTING
            message_text              = lv_message ).
    ENDIF.

    IF NOT iv_id_part IS INITIAL AND NOT iv_tip_part IS INITIAL.
*	Begin	-->	MgM DCEK902600 valida empresa 10/01/2017
      IF iv_tip_part EQ `PT`.
        lv_id_part_n10  = iv_id_part.
        iv_id_part      = lv_id_part_n10.
      ENDIF.
*	End	  -->	MgM DCEK902600

      SELECT SINGLE objid
        INTO lv_objid
        FROM hrp1001
      WHERE otype EQ me->c_evento
        AND objid EQ iv_evtid
        AND sclas EQ iv_tip_part
        AND sobid EQ iv_id_part
        AND relat EQ me->c_rel025.

      IF sy-subrc IS INITIAL.
        r_error = abap_true.
        MESSAGE e031(zedu_wd_message)
                WITH iv_tip_part
                     iv_id_part
                     iv_evtid
                INTO lv_message.
        me->go_message_man->report_message(
            EXPORTING
              message_text              = lv_message ).
      ENDIF.
    ENDIF.

*	Begin	-->	MgM DCEK903531 Fechas vacías 16/02/2017
    "Validamos que el evento tenga fecha de vencimiento
    SELECT vencimiento
      FROM hrp9112 AS p
        INNER JOIN hrt9112 AS t
          ON p~tabnr EQ t~tabnr
        INTO TABLE @DATA(lt_venc)
          WHERE otype         EQ @c_evento
            AND objid         EQ @iv_evtid
            AND p~tabnr       NE @space.

    IF sy-subrc EQ 0.

*	Begin	-->	MgM DCEK903535 check fecha 16/02/2017
      LOOP AT lt_venc
        INTO DATA(ls_venc).

        CALL FUNCTION 'RP_CHECK_DATE'
          EXPORTING
            date         = ls_venc
          EXCEPTIONS
            date_invalid = 1
            OTHERS       = 2.

        IF sy-subrc <> 0.
*	End	  -->	MgM DCEK903535

          r_error = abap_true.

          "El evento & no tiene fecha de vencimiento (9112)
          MESSAGE e075(zedu_wd_message)
            WITH iv_evtid
              INTO lv_message.

          me->go_message_man->report_message(
              EXPORTING
                message_text  = lv_message ).

        ENDIF.

      ENDLOOP.

    ENDIF.
*	End	  -->	MgM DCEK903531

  ENDMETHOD.


  METHOD validar_habeas_data.

    SELECT SINGLE hab_dat
      FROM zhcm_habeas_data
      INTO r_existe
    WHERE type      EQ iv_type
      AND id_number EQ iv_id_number.

  ENDMETHOD.


  METHOD validar_knvv.
    DATA:
      lt_vkont TYPE TABLE OF fkkvkp-vkont,
      lt_vktyp TYPE TABLE OF fkkvk-vktyp.

    SELECT vkont
      FROM fkkvkp
      INTO TABLE lt_vkont
    WHERE gpart EQ i_bp.

    IF sy-subrc IS INITIAL.

      SELECT vktyp
      FROM fkkvk
      INTO TABLE lt_vktyp
      FOR ALL ENTRIES IN lt_vkont
    WHERE vkont EQ lt_vkont-table_line
      AND vktyp EQ 'CO'.

      IF sy-subrc IS INITIAL.
*	Begin	-->	MgM DCEK902505 valida esquema cliente Estandar 06/01/2017
        DATA lv_cliente TYPE knvv-kunnr.
        SELECT SINGLE kunnr
          FROM knvv
            INTO lv_cliente
              WHERE kunnr EQ i_bp
                AND vkorg EQ `1000`
                AND kalks EQ `1`  "Estándar
                AND vtweg EQ '16'
                AND spart EQ '16'.
        IF sy-subrc EQ 0.
*	End	  -->	MgM DCEK902505
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF lt_vktyp IS INITIAL.
    IF lv_cliente IS INITIAL.
      rv_error = me->create_cc( i_bp = i_bp ).
    ENDIF.

  ENDMETHOD.


  METHOD validar_region.
    data: lv_regio TYPE regio.

    SELECT SINGLE bland
      INTO lv_regio
      FROM t005u
    WHERE spras eq sy-langu
      AND land1 eq iv_land1
      AND bland eq iv_regio.
    IF sy-subrc IS NOT INITIAL.
      rv_error = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
