FUNCTION Z_EDU_OBTENER_REF_ADICIONAL.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_OPBEL_T) TYPE  OPBEL_T
*"  EXPORTING
*"     REFERENCE(E_REF_ADICIONAL_T) TYPE  ZEDU_T_REF_ADICIONAL_OPBEL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF lty_dfkkop,
           opbel TYPE opbel_kk,
           blart TYPE blart_kk,
           vtre2 TYPE vtre2_kk,
         END OF lty_dfkkop.

  DATA: lt_dfkkop        TYPE SORTED TABLE OF lty_dfkkop WITH NON-UNIQUE KEY primary_key COMPONENTS opbel,
        ls_ref_adicional TYPE zedu_ref_adicional_opbel.

  CHECK i_opbel_t IS NOT INITIAL.

  SELECT opbel blart vtre2
    INTO TABLE lt_dfkkop
    FROM dfkkop
    FOR ALL ENTRIES IN i_opbel_t
    WHERE opbel = i_opbel_t-opbel.


  DELETE ADJACENT DUPLICATES FROM lt_dfkkop COMPARING opbel.

  LOOP AT lt_dfkkop INTO DATA(ls_dfkkop).
    SPLIT ls_dfkkop-vtre2 AT '|' INTO ls_ref_adicional-plan_estudio
                                      ls_ref_adicional-linea_educativa
                                      ls_ref_adicional-clase_plan
                                      ls_ref_adicional-nivel_plan
                                      ls_ref_adicional-periodo_plan
                                      ls_ref_adicional-plan_odontologico.
    ls_ref_adicional-opbel = ls_dfkkop-opbel.
    ls_ref_adicional-blart = ls_dfkkop-blart.
    APPEND ls_ref_adicional TO e_ref_adicional_t.
  ENDLOOP.



ENDFUNCTION.
