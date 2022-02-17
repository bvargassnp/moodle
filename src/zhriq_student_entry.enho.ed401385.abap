"Name: \PR:SAPLHRPIQ00STUDENTENTRY\FO:FILL_LISTBOX\SE:BEGIN\EI
ENHANCEMENT 0 ZHRIQ_STUDENT_ENTRY.
 "Si quien invoca la funcionalidad es el reporte de desatrazo de titulaciones
  IF sy-tcode EQ 'ZEDU_REP_DATA_TITUL'.
    IMPORT gs_save_st00-st_objid TO gs_save_st00-st_objid FROM MEMORY ID 'ZSTUDENT2'.
    IMPORT gs_save_st00-sc_objid TO gs_save_st00-sc_objid FROM MEMORY ID 'ZPON2'.
  ENDIF.
ENDENHANCEMENT.
