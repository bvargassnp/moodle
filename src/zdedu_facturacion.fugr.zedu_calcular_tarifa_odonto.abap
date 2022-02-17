FUNCTION zedu_calcular_tarifa_odonto.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_DIV_CRH) TYPE  ZEDU_DIVCRH
*"     REFERENCE(I_KSCHL) TYPE  KSCHA
*"     REFERENCE(I_CMSCCAT) TYPE  PIQSCFEECAT
*"     REFERENCE(I_PERYR) TYPE  PIQPERYR
*"     REFERENCE(I_PERID) TYPE  PIQPERID
*"  EXPORTING
*"     REFERENCE(E_KEBTR) TYPE  KBETR_KOND
*"  TABLES
*"      T_RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      NO_ENCONTRADA_TARIFA_ODON
*"----------------------------------------------------------------------
  DATA: lv_fact_key_sc TYPE zedu_fact_key_sc,        "Reg zedu_fac_key_sc
        lr_bapiret2    TYPE bapiret2.             "Rec. mensajes

  CONSTANTS:
    lc_symsgid TYPE symsgid          VALUE 'ZEDU_FACTURACION', "Clase de mensaje
    lc_msgty_i TYPE bapi_mtype       VALUE 'I',                "Mensaje Informativo
    lc_msgty_e TYPE bapi_mtype       VALUE 'E',                "Mensaje Error
    lc_msgty_a TYPE bapi_mtype       VALUE 'A'.                "Mensaje Abortar

"  CALL FUNCTION 'Z_EDU_GET_FACT_KEY_SC'
"    IMPORTING
"      e_fact_key_sc = lv_fact_key_sc.

  SELECT * INTO TABLE @DATA(lt_a534)
    FROM a534
    WHERE kappl     = @gc_kappl      AND
          kschl     = @i_kschl       AND
          cmperyr   = @I_PERYR       AND
          cmperid   = @I_PERid       AND
          cmsccat   = @I_CMSCCAT     AND
          zzdiv_crh = @i_div_crh.
  IF sy-subrc = 0.
    SELECT * INTO TABLE @DATA(lt_konp)
      FROM konp FOR ALL ENTRIES IN @lt_a534
      WHERE knumh    = @lt_a534-knumh AND
            loevm_ko = @cl_hrpiq00const=>c_unchecked. "No borrado
  ENDIF.

  LOOP AT lt_konp INTO DATA(lr_konp).
    E_KEBTR = lr_konp-kbetr.
  ENDLOOP.
  IF sy-subrc <> 0.
    lr_bapiret2-id         = lc_symsgid.
    lr_bapiret2-log_msg_no = 32.
    lr_bapiret2-type       = lc_msgty_a.
    APPEND lr_bapiret2 TO t_return.
  ENDIF.
ENDFUNCTION.
