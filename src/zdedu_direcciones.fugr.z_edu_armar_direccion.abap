FUNCTION z_edu_armar_direccion.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  CHANGING
*"     REFERENCE(CH_DIRECCION) TYPE  ZEDU_S_DIRECCION
*"----------------------------------------------------------------------

data: lv_numero_1(10),
      lv_numero_2(10).

    CONCATENATE ch_direccion-numero
                ch_direccion-letra
                ch_direccion-orientacion
                into lv_numero_1.

    CONCATENATE '#'
                ch_direccion-numero_2
                ch_direccion-letra_2
                ch_direccion-orientacion_2
                '-'
                ch_direccion-numero_3
                into lv_numero_2.

    CONCATENATE ch_direccion-t_via
                lv_numero_1
                lv_numero_2
                ch_direccion-urbanizacion
                INTO ch_direccion-street SEPARATED BY space.

ENDFUNCTION.
