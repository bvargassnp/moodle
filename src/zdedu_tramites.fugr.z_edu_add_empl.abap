function z_edu_add_empl.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_BP) TYPE  BU_PARTNER
*"     REFERENCE(IV_NRO_EMPLE) TYPE  BU_ID_NUMBER
*"     REFERENCE(IV_SOCIEDAD) TYPE  BUKRS
*"     REFERENCE(IV_CTRIND) TYPE  PCO_SUBES
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------


  data: ls_identification	type bapibus1006_identification,
        ls_return         type bapiret2,
        ls_knb1           type knb1,
        ls_lfb1           type lfb1,
        ls_but0id         type but0id,
        lv_category       type bu_id_category,
        lv_CtrInd         Type PCO_SubES.     "20190313RBP


*** Agrego el nro de empleado al BP.
*  ls_identification-identrydate     = sy-datum.
*  ls_identification-idvalidfromdate = sy-datum.
*  ls_identification-idvalidtodate   = '99991231'.

*20190313 RBP Begin ----------------------*
*Se agrega indicador de Contratista
  lv_CtrInd = IV_CtrInd.
  If lv_CtrIND = ' '.
     lv_CtrIND = 'E'.
  EndIf.
  Concatenate 'Z' lv_CtrIND iv_Sociedad Into lv_Category.
*  Case iv_Sociedad.
*    When 'UCES'.
*         If IV_CtrIND = 'X'.
*            lv_Category = 'ZEUCES'.
*         Else.
*            lv_Category = 'ZXUCES'.
*         EndIf.
*    When 'ICMT'.
*         If IV_CtrIND = 'X'.
*            lv_Category = 'ZEICMT'.
*         Else.
*            lv_Category = 'ZXICMT'.
*         EndIf.
*  EndCase.
*20190313 RBP End   ----------------------*

*  CALL FUNCTION 'BAPI_IDENTIFICATION_ADD'
*    EXPORTING
*      businesspartner        = iv_bp
*      identificationcategory = lv_category
*      identificationnumber   = iv_nro_emple
*      identification         = ls_identification
*    TABLES
*      return                 = et_return.
*
*  READ TABLE et_return INTO ls_return WITH KEY type = 'E'.
*  IF sy-subrc IS INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    EXIT.
*  ELSE.
**   Se actualizó el número de empleado en el & correctamente.
*    MESSAGE s064(zedu_wd_message) WITH 'interlocutor'.
*    MOVE sy-msgid TO ls_return-id.
*    MOVE sy-msgno TO ls_return-number.
*    MOVE sy-msgty TO ls_return-type.
*    MOVE sy-msgv1 TO ls_return-message_v1.
*    APPEND ls_return TO et_return.
*  ENDIF.

*	Begin	-->	MgM DCEK903181 evita borrado nro. persona 31/01/2017
  check lv_category is not initial. "que la soc. haya determinado cat
  check iv_nro_emple is not initial. "que se ingrese nro. persona a setear
*	End	  -->	MgM DCEK903181

  ls_but0id-partner  = iv_bp.
  ls_but0id-type     = lv_category.
  ls_but0id-idnumber = iv_nro_emple.
  modify but0id from ls_but0id.

*** Agrego el nro de empleado al BP.
  if sy-subrc is initial.
*   Se actualizó el número de empleado en el & correctamente.
    message s064(zedu_wd_message) with 'interlocutor'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
  else.
*   No se actualizó el número de empleado en el &.
    message e063(zedu_wd_message) with 'interlocutor'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
    exit.
  endif.

*** Agrego el nro de empleado al cliente.
  select single *
    from knb1
    into ls_knb1
    where kunnr = iv_bp
      and bukrs = iv_sociedad.

  if sy-subrc is initial.
    ls_knb1-pernr = iv_nro_emple.
    modify knb1 from ls_knb1.
*   Se actualizó el número de empleado en el & correctamente.
    message s064(zedu_wd_message) with 'cliente'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
  else.
*   No se actualizó el número de empleado en el &.
    message w063(zedu_wd_message) with 'cliente'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
  endif.

*** Agrego el nro de empleado al proveedor.
  select single *
    from lfb1
    into ls_lfb1
    where lifnr = iv_bp
      and bukrs = iv_sociedad.

  if sy-subrc is initial.
    ls_lfb1-pernr = iv_nro_emple.
    modify lfb1 from ls_lfb1.
*   Se actualizó el número de empleado en el & correctamente.
    message s064(zedu_wd_message) with 'proveedor'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
  else.
*   No se actualizó el número de empleado en el &.
    message w063(zedu_wd_message) with 'proveedor'.
    move sy-msgid to ls_return-id.
    move sy-msgno to ls_return-number.
    move sy-msgty to ls_return-type.
    move sy-msgv1 to ls_return-message_v1.
    append ls_return to et_return.
  endif.

  read table et_return into ls_return with key type = 'E'.
  if sy-subrc is not initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    exit.
  endif.

endfunction.
