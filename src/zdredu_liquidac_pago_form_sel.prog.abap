*&---------------------------------------------------------------------*
*&  Include           ZDREDU_LIQUIDAC_PAGO_FORM_SEL
*&---------------------------------------------------------------------*
*Parámetros para método de selección y variante de selección
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-001.
PARAMETERS: p_selmet TYPE t7piqselmethods-selmeth NO-DISPLAY MODIF ID sel,
            p_selvar TYPE piqselvari NO-DISPLAY MODIF ID sel.
SELECTION-SCREEN END OF BLOCK s1.

SELECTION-SCREEN BEGIN OF TABBED BLOCK rep_subscr FOR 4 LINES.
SELECTION-SCREEN END OF BLOCK rep_subscr.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_deuda AS CHECKBOX.
SELECTION-SCREEN COMMENT 3(54) text-008.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_inter as checkbox.
SELECTION-SCREEN COMMENT 3(66) text-019.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-003.
PARAMETERS: p_selliq TYPE zedu_metodos_liq-id_liquidacion
            AS LISTBOX VISIBLE LENGTH 50
            USER-COMMAND upd
            MODIF ID ml.
SELECTION-SCREEN END OF BLOCK s2.

SELECTION-SCREEN BEGIN OF BLOCK nf WITH FRAME TITLE text-012.
PARAMETERS: p_nrform TYPE zpre_admi_1-nr_formulario MODIF ID nf.
SELECTION-SCREEN END OF BLOCK nf.

SELECTION-SCREEN BEGIN OF BLOCK f1 WITH FRAME TITLE text-002.
PARAMETER:  p_calen  RADIOBUTTON GROUP fs USER-COMMAND ok DEFAULT 'X' MODIF ID fc,
            p_manual RADIOBUTTON GROUP fs MODIF ID fm.
*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-016 MODIF ID fo.
PARAMETER:  p_ordina TYPE datum DEFAULT sy-datum MODIF ID fo.
SELECTION-SCREEN COMMENT 45(5) text-015 MODIF ID fov.
PARAMETERS  p_ordval TYPE betrw_kk MODIF ID fov.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-021 MODIF ID fo.
PARAMETER:  p_persl TYPE persl_kk MODIF ID fo.
SELECTION-SCREEN END OF LINE.
PARAMETER:  "p_ordina TYPE datum DEFAULT sy-datum MODIF ID fo,
            p_extra1 TYPE datum MODIF ID fe1,
            p_extra2 TYPE datum MODIF ID fe1.
SELECTION-SCREEN END OF BLOCK f1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-009.
PARAMETER: p_email AS CHECKBOX,
           p_nodial AS CHECKBOX,
           p_cero  TYPE check AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.

* Ejemplo de un método de seleción
  CREATE OBJECT gr_selappif TYPE lcl_selappif.
* Ejemplo de una interface
  CREATE OBJECT g_selmethods_ref.
* Inicialización
  CALL METHOD g_selmethods_ref->init
    EXPORTING
      im_applicationif_ref = gr_selappif
      im_selscen           = 'FEEC'     "selection method group(custom.)
    EXCEPTIONS
      selscen_not_found    = 1
      internal_error       = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IMPORT gv_cursor
  FROM MEMORY ID co_cursor.
  SET CURSOR FIELD gv_cursor.
  FREE MEMORY ID co_cursor.

AT SELECTION-SCREEN OUTPUT.

  CALL METHOD g_selmethods_ref->before_pbo.

  PERFORM f_llenar_listbox.

  PERFORM f_ocultar_campos.

AT SELECTION-SCREEN.
*date is set date
  gs_selperiod-begda = sy-datum.
  gs_selperiod-endda = sy-datum.

  CALL METHOD g_selmethods_ref->after_pai
    EXPORTING
      im_period = gs_selperiod.
