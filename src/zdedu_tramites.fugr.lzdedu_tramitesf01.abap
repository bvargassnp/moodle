*----------------------------------------------------------------------*
***INCLUDE LZDEDU_TRAMITESF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MODIF_DATOS_SENSIBLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_EXIT  text
*----------------------------------------------------------------------*
form f_modif_datos_sensibles        Using    p_Exit
                                    Changing p_Return Type BapiRet2_T.
  include <cntain>.
  data: l_object_key  type sweinstcou-objkey.
  data: ls_zedu_datos_pers  type zedu_datos_pers,
        ls_zedu_datos_dir   type zedu_datos_dir,
        lt_zedu_datos_banco type table of zedu_datos_banco,
        ls_zedu_datos_banco type zedu_datos_banco,
        ls_Return                   Type BapiRet2.

  data  lv_estudiante       type boole_d.    "-->	MgM DCEK902658

  field-symbols: <fs_datos_bancarios> type zedu_s_datos_bancarios.

  if gs_datos_personales-type       ne gs_datos_personales_ini-type
  or gs_datos_personales-idnumber   ne gs_datos_personales_ini-idnumber
  or gs_datos_personales-name_last  ne gs_datos_personales_ini-name_last
  or gs_datos_personales-name_first ne gs_datos_personales_ini-name_first
  or gs_datos_personales-namemiddle ne gs_datos_personales_ini-namemiddle
  or gs_datos_personales-name_lst2  ne gs_datos_personales_ini-name_lst2
  or gs_datos_personales-name_org1  ne gs_datos_personales_ini-name_org1
  or gs_datos_personales-name_org2  ne gs_datos_personales_ini-name_org2.

*20200302 RBP Begin --------------------------------------------------*
*Se valida que el usuario esté registrado en la tabla de autorizaciones
    Select Single * From      ZEdu_Usr_GrupoR
                              Into ls_zEdu_Datos_Pers
                              Where BName = Sy-Uname.
    If Sy-SubRC <> 0.
       Message S081(zEdu_wd_Message) With Sy-Uname.
       Move Sy-MsgId          To ls_Return-Id.
       Move Sy-MsgNo          To ls_Return-Number.
       Move 'E'               To ls_Return-Type.
       Move Sy-MsgV1          To ls_Return-message_v1.
       Append ls_Return       To p_Return.
       p_Exit = cl_bp_Const=>True.
       Check cl_bp_Const=>True NE cl_bp_Const=>True.
    EndIf.
*20200302 RBP End   --------------------------------------------------*

*	Begin	-->	MgM DCEK902658 wf solo para estudiantes 11/01/2017
    perform f_check_is_student  using gs_datos_personales-partner
                                changing lv_estudiante.

    check lv_estudiante eq cl_bp_const=>true.
*	End	  -->	MgM DCEK902658

*** Disparo WF

*** Guardo en temporal
    ls_zedu_datos_pers-mandt = sy-mandt.
    move-corresponding gs_datos_personales to ls_zedu_datos_pers.
    modify zedu_datos_pers from ls_zedu_datos_pers.

    ls_zedu_datos_dir-mandt   = sy-mandt.
    ls_zedu_datos_dir-partner = ls_zedu_datos_pers-partner.
    move-corresponding gs_datos_direccion to ls_zedu_datos_dir.
    modify zedu_datos_dir from ls_zedu_datos_dir.

    loop at gt_datos_bancarios assigning <fs_datos_bancarios>.
      ls_zedu_datos_banco-mandt   = sy-mandt.
      ls_zedu_datos_banco-partner = ls_zedu_datos_pers-partner.
      move-corresponding <fs_datos_bancarios> to ls_zedu_datos_banco.
      append ls_zedu_datos_banco to lt_zedu_datos_banco.
    endloop.

    if lt_zedu_datos_banco[] is not initial.
      modify zedu_datos_banco from table lt_zedu_datos_banco.
    endif.

    p_exit = abap_true.

*Container
    swc_container container.          " Se declara el contenedor
    swc_clear_container container.    " Se inicializa el contenedor
    swc_create_container container.   " Se crea el contenedor

    "La clave del objeto es el BP
    l_object_key = ls_zedu_datos_pers-partner.

    call function 'SWE_EVENT_CREATE'
      exporting
        objtype           = 'ZDWFBO_CAM'
        objkey            = l_object_key
        event             = 'CAMBIOBP'
      tables
        event_container   = container
      exceptions
        objtype_not_found = 1
        others            = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    else.
      commit work.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_IS_STUDENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_EXIT  text
*----------------------------------------------------------------------*
form f_check_is_student using    pv_partner
                        changing pv_st.
*20200302 RBP Begin --------------------------------------------------*
  Data: lcEstudent                  Type PIQStudent12,
        lcObjEstud                  Type ObjEktID,
        ltRegistrations             Type PIQStReg_t,
        lsRegistrations             Type PIQStReg.
*20200302 RBP End   --------------------------------------------------*

  select single partner,
                rltyp
    from but100
      into @data(ls_estudiante)
      where partner eq @pv_partner
        and rltyp   eq @cl_hrpiq00bpconst=>c_bpview_st. "Estudiante

    if sy-subrc eq 0.
      pv_st = cl_bp_const=>true.
    endif.

    Check cl_bp_Const=>True NE cl_bp_Const=>True. "No Continúa

    Check Pv_St EQ 'X'.
*20200302 RBP Begin --------------------------------------------------*
*Se agrega la validación de la vigencia como estudiante, se recomienda
*que solo si es estudiante se solicite la aprobación de Admisiones
    Call Function 'HRIQ_STUDENT_NUMBERS_GET' "Trae el Número de estudiate
      Exporting
        IV_PlVar                       = '01'
        IV_Partner                     = pv_Partner
        IV_XRead_Buffer                = 'X'
      Importing
        EV_Student12                   = lcEstudent
        EV_ObjID                       = lcObjEstud
     Exceptions
        NO_Number                      = 1
        NO_PlVar                       = 2
        NO_Student12_For_ObjID         = 3
        NO_ObjID_For_Partner           = 4
        NO_ObjID_For_Student12         = 5
        NO_Partner_For_ObjID           = 6
        NO_Student12_For_Partner       = 7
        Others                         = 8.

    If Sy-SubRC <> 0.
       Pv_St    = cl_bp_Const=>False.
    EndIf.
    Call Function 'HRIQ_STUDENT_REGIST_READ' "Historial de Matrículas
      Exporting
        IV_PlVar                      = '01'
        IV_St_ObjID                   = lcObjEstud
        IV_Status_Attend              = 'X'
        IV_Status_Leave               = 'X'
        IV_Base_Auth                  = 'DISP'
        IV_With_Stru_Auth             = 'X'
      Importing
        ET_Registrations              = ltRegistrations.

    Sort ltRegistrations By EndDA Descending BegDA.
    Loop At ltRegistrations           Into lsRegistrations
                                      Where EndDA GE Sy-DatUM.
    EndLoop.
    If Sy-SubRC <> 0.
       Pv_St    = cl_bp_Const=>False.
    EndIf.
*20200302 RBP End   --------------------------------------------------*

endform.
