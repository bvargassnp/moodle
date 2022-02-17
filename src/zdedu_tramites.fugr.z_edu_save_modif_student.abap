FUNCTION Z_EDU_SAVE_MODIF_STUDENT.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_PARTNER) TYPE  BAPIBUS1006_HEAD-BPARTNER
*"  TABLES
*"      ET_RETURN TYPE  BAPIRET2_T OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_objidst              TYPE hrobjid,
        ls_studentpersonaldata  TYPE bapistudent_personal,
        ls_studentpersonaldatax TYPE bapistudent_personalx.

  "Verificar si BP existe como Estudiante
  SELECT SINGLE stobjid
    FROM cmacbpst
    INTO lv_objidst
    WHERE partner = i_partner.

  "Salir si el BP no es estudiante
  CHECK sy-subrc = 0.

  "Modificar Nombres de estudiante
  ls_studentpersonaldata-first_name = gs_datos_personales-name_first.
  ls_studentpersonaldata-last_name =  gs_datos_personales-name_last.
  ls_studentpersonaldata-middle_name = gs_datos_personales-namemiddle.
  ls_studentpersonaldata-name_at_birth = gs_datos_personales-name_lst2.
  ls_studentpersonaldata-persidno = gs_datos_personales-idnumber.

  ls_studentpersonaldatax-first_name = 'X'.
  ls_studentpersonaldatax-last_name = 'X'.
  ls_studentpersonaldatax-middle_name = 'X'.
  ls_studentpersonaldatax-name_at_birth ='X'.
  ls_studentpersonaldatax-persidno = 'X'.

  CALL FUNCTION 'BAPI_STUDENT_CHANGE3'
    EXPORTING
      objectid             = lv_objidst
      validitybegin        = cl_hrpiq00const=>c_date_lowdate
      validityend          = cl_hrpiq00const=>c_date_highdate
      studentpersonaldata  = ls_studentpersonaldata
      studentpersonaldatax = ls_studentpersonaldatax
    TABLES
      return               = et_return.
ENDFUNCTION.
