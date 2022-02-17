*----------------------------------------------------------------------*
***INCLUDE LZDEDU_ACTUALIZAR_ESTADO_ESF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZAR_ESTADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DFKKOP  text
*----------------------------------------------------------------------*
FORM f_actualizar_estados  USING p_lt_fkkcl TYPE fkkcl_t.

  DATA: lt_dfkkko TYPE tyt_dfkkko,
        ls_fkkcl  TYPE fkkcl.

  SELECT opbel
         cpudt
         cputm
    FROM dfkkko
    INTO TABLE lt_dfkkko
    FOR ALL ENTRIES IN p_lt_fkkcl
    WHERE opbel = p_lt_fkkcl-opbel.
  IF sy-subrc = 0.

    READ TABLE p_lt_fkkcl INTO ls_fkkcl
    WITH KEY "opbel =
             psobtyp = c_a010.
             "stakz   = space.
    IF sy-subrc = 0.

      CALL FUNCTION 'Z_EDU_MARCADO_ASIGNATURAS'
        EXPORTING
          i_gpart        = ls_fkkcl-gpart
          i_opbel        = ls_fkkcl-opbel
          i_pago         = c_1
        EXCEPTIONS
          no_estudiante  = 1
          no_opbel       = 2
          no_hrp1001     = 3
          no_begda_endda = 4
          OTHERS         = 5.

    ENDIF.

    READ TABLE p_lt_fkkcl INTO ls_fkkcl
    WITH KEY "opbel =
             hvorg = c_3000
             stakz = c_a.
    IF sy-subrc = 0.
      PERFORM f_actualizar_reserva_cupo
      USING ls_fkkcl.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ACTUALIZAR_RESERVA_CUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FKKCL  text
*----------------------------------------------------------------------*
FORM f_actualizar_reserva_cupo  USING p_fkkcl TYPE fkkcl.

  DATA: lt_admissions TYPE piqst_adm_t,
        ls_stadm      TYPE piqst_adm,
        lv_result     TYPE sysubrc,
        lv_plvar      TYPE plvar,
        lv_st_objid   TYPE piqstudent,
        lt_return     TYPE bapiret2_t.

  lv_plvar    = c_01.
  lv_st_objid = p_fkkcl-gpart.

  CALL FUNCTION 'HRIQ_STUDENT_ADMIS_READ'
    EXPORTING
      iv_plvar        = lv_plvar
      iv_st_objid     = lv_st_objid
    IMPORTING
      et_admissions   = lt_admissions
    EXCEPTIONS
      wrong_import    = 1
      technical_error = 2
      OTHERS          = 3.
  IF sy-subrc = 0.

    DELETE lt_admissions
    WHERE adm_enrcateg NE c_08.

    LOOP AT lt_admissions INTO ls_stadm.

      ls_stadm-adm_enrcateg = c_07.

      CALL FUNCTION 'HRIQ_STUDENT_ADMIS_CHANGE_DB'
        EXPORTING
          is_stadm         = ls_stadm
        IMPORTING
          ev_result        = lv_result
          et_return        = lt_return
        EXCEPTIONS
          internal_error   = 1
          no_authorisation = 2
          corr_exit        = 3
          cancelled        = 4
          OTHERS           = 5.

    ENDLOOP.

  ENDIF.

ENDFORM.
