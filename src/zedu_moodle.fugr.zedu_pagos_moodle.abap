FUNCTION ZEDU_PAGOS_MOODLE.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(I_ESTRUCTURA_MOODLE) TYPE  ZSTEDU_PAYMENTREPORTPROD_IN
*"  EXPORTING
*"     REFERENCE(E_IDENTIFICADOR) TYPE  STRING
*"     REFERENCE(ET_MENSAJES) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------
  DATA(lo_edu_pago_moodle) = NEW zcl_edu_moodle(  ).

  TRY.
      lo_edu_pago_moodle->payment_report_product(
        EXPORTING
          i_input                   = i_estructura_moodle
        IMPORTING
          e_output                  = DATA(ls_out)
      ).

      e_identificador = ls_out-value.

    CATCH zcx_edu_paymentreportprod INTO DATA(lx_payment).
      et_mensajes = lx_payment->gt_mensajes.
  ENDTRY.

ENDFUNCTION.
