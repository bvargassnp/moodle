FUNCTION zedu_fkk_sample_5059.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_FKKOP) LIKE  FKKOP STRUCTURE  FKKOP
*"  TABLES
*"      T_FKKOP_NOT_SUBMITTED STRUCTURE  FKKOP
*"  CHANGING
*"     VALUE(I_REL_INSTPLN) TYPE  BOOLE-BOOLE OPTIONAL
*"----------------------------------------------------------------------

* Check: only interest, charges, budget billing plan positions and
* collectiove bills can be released to a collection agency.
  IF NOT i_fkkop-stakz IS INITIAL.
    CASE i_fkkop-stakz.
**INICIO - Comentario para valor G --> Para que no genere Actividades de cobro Juridico y Pre-Juridico
*     WHEN 'G'.                       " Interest and charges
*       OK
**FIN - Comentario
      WHEN 'I'.                       " down payment request from invoicing
*       OK
      WHEN 'P'.                       " budget billing position
*       OK
*      WHEN 'S'.                       " collective bill
*       OK
      WHEN 'R'.                       " Installment plan items
*       OK
        i_rel_instpln = const_marked. " release of InstPlan possible
      WHEN OTHERS.                    " block for release
        t_fkkop_not_submitted = i_fkkop.
        APPEND t_fkkop_not_submitted.
    ENDCASE.
  ENDIF.

* no submission for items that are in an installment plan
  IF i_fkkop-abwtp = 'R'.
    t_fkkop_not_submitted = i_fkkop.
    APPEND t_fkkop_not_submitted.
  ENDIF.

* no submission for items that are in a collective bill
*  IF i_fkkop-abwtp = 'S'.
*    t_fkkop_not_submitted = i_fkkop.
*    APPEND t_fkkop_not_submitted.
*  ENDIF.





ENDFUNCTION.
