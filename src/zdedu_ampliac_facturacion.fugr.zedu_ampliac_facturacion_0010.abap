FUNCTION zedu_ampliac_facturacion_0010 .
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_FKKKO) LIKE  FKKKO STRUCTURE  FKKKO
*"     VALUE(I_ADD_DOC) LIKE  BOOLE-BOOLE
*"     VALUE(I_LAST_OP_FROM_CALLER) TYPE  OPUPK_KK
*"     VALUE(I_LAST_OPK_FROM_CALLER) TYPE  OPUPK_KK
*"  TABLES
*"      T_FKKOP STRUCTURE  FKKOP
*"      T_FKKOPK STRUCTURE  FKKOPK
*"      T_FKKOPW STRUCTURE  FKKOPW
*"      T_FKKOP_DP STRUCTURE  DFKKOP_DP OPTIONAL
*"--------------------------------------------------------------------

  PERFORM f_validar_campos
  USING i_fkkko
        t_fkkop[].

ENDFUNCTION.
