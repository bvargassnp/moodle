*&---------------------------------------------------------------------*
*& Report  ZDREDU_EXTR_ENVIO_PAGO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zdredu_extr_envio_pago.

*&--------------------------------------------------------------------&*
*&                          INCLUDES                                  &*
*&--------------------------------------------------------------------&*

INCLUDE zdiedu_extr_envio_pago_top.
INCLUDE zdiedu_extr_envio_pago_f01.

*&--------------------------------------------------------------------&*
*&                         START-OF-SELECTION                         &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.

PERFORM f_leer_datos.

*&--------------------------------------------------------------------&*
*&                         END-OF-SELECTION                         &*
*&--------------------------------------------------------------------&*
END-OF-SELECTION.

PERFORM f_procesar_reg.
