FUNCTION z_edu_sh_zwels.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_t042z,
           zlsch TYPE t042z-zlsch,
           text1 TYPE t042z-text1,
         END OF ty_t042z.


  DATA: lt_t042z TYPE TABLE OF ty_t042z.

  CHECK callcontrol-step = 'DISP'.


  SELECT zlsch text1
    FROM t042z
    INTO TABLE lt_t042z
    WHERE land1 = 'CO'.

  record_tab[] = lt_t042z[].



ENDFUNCTION.
