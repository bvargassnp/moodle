FUNCTION z_edu_get_autoespe.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_OBJID_ST) TYPE  HROBJID
*"     REFERENCE(I_OBJID_SM) TYPE  HROBJID
*"     REFERENCE(I_PERYR) TYPE  PIQPERYR
*"     REFERENCE(I_PERID) TYPE  PIQPERID
*"  EXPORTING
*"     REFERENCE(E_PERMISO) TYPE  ZEDU_PERMISO_E
*"----------------------------------------------------------------------
*LAT : Se leen los permisos de la asignatura.
*    : 16-06-2017.
*-----------------------------------------------------------------------
  DATA:    lt_1764   TYPE TABLE OF p1764,                    "Tabla Inf. para Categ. por SC
           lr_1764   TYPE p1764,                             "Rec. Inf. para Categ. por SC
           lt_t1764  TYPE TABLE OF hrt1764,                  "Tabla Actualiz.atrib.general Adic.
           lr_t1764  TYPE hrt1764,                           "Rec. Actualiz.atrib.general Adic.
           lt_object TYPE TABLE OF hrobject,                 "Tabla hrobject
           lr_object TYPE hrobject.                          "Rec. hrobject

  CONSTANTS: lc_subty_8010 TYPE subtyp VALUE '8010'.         "Subtipo AE.

  lr_object-otype = cl_hrpiq00const=>c_otype_st.
  lr_object-plvar = cl_hrpiq00const=>c_plvar_active.
  lr_object-objid = i_objid_st.
  APPEND lr_object TO lt_object.

  CALL FUNCTION 'HRIQ_READ_INFTY_NNNN'
    EXPORTING
      infty                 = cl_hrpiq00const=>c_infty_1764
      subty                 = lc_subty_8010
      istat                 = cl_hrpiq00const=>c_istat_active
    TABLES
      innnn                 = lt_1764
      objects               = lt_object
    EXCEPTIONS
      nothing_found         = 1
      wrong_condition       = 2
      infotyp_not_supported = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_1764.
  IF sy-tfill = 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'HRIQ_READ_INFTY_TABDATA'
    EXPORTING
      infty          = cl_hrpiq00const=>c_infty_1764
    TABLES
      innnn          = lt_1764
      hrtnnnn        = lt_t1764
    EXCEPTIONS
      no_table_infty = 1
      innnn_empty    = 2
      nothing_found  = 3
      OTHERS         = 4.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  READ TABLE lt_t1764 INTO lr_t1764 WITH KEY pt_otype = cl_hrpiq00const=>c_otype_sm
                                             pt_objid = i_objid_sm
                                             peryr    = i_peryr
                                             perid    = i_perid.
  IF sy-subrc = 0.
    e_permiso = 'X'.
  ENDIF.
ENDFUNCTION.
