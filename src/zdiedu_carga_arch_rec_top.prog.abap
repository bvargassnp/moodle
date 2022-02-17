*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_CARGA_ARCH_REC_TOP
*&---------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&                           TIPOS GLOBALES                           &*
*&--------------------------------------------------------------------&*
* Estructura con los campos necesarios para calcular los totales de
* control del archivo financiero.
TYPES: BEGIN OF ty_totales,
         tot_registro  TYPE zedu_recaudo_f1-tot_reg,
         tot_recaudado TYPE zedu_recaudo_f1-valor,
         tot_lotes(6)  TYPE n,
       END OF ty_totales.

TYPES: ty_mensajes TYPE STANDARD TABLE OF bapiret2,
       ty_recau_d  TYPE STANDARD TABLE OF zedu_recaudo_d.

*&--------------------------------------------------------------------&*
*&                           DATOS GLOBALES                           &*
*&--------------------------------------------------------------------&*
DATA: gt_mensajes           TYPE STANDARD TABLE OF bapiret2.

DATA:
**   Tabla local para almacenamiento de registros del archivo sin parsear
  gt_archivo_str TYPE STANDARD TABLE OF bsstring,
  gs_archivo_str TYPE bsstring,
  gt_param       TYPE STANDARD TABLE OF zedu_c_param,
  gs_param       TYPE zedu_c_param,
  gt_cuentas     TYPE STANDARD TABLE OF zedu_c_cta_comp.

DATA:
  gv_campo(20)   TYPE c,
  gv_archivo     TYPE zedu_nom_archivo,
  gv_archivo_out TYPE file_table-filename,
  gv_archivo_err TYPE file_table-filename,
  gv_archivo_bkp TYPE file_table-filename,
  gv_archivo_in  TYPE file_table-filename,
  gv_blart       TYPE blart_kk,
  gv_waers       TYPE waers,
  gv_bukrs       TYPE bukrs,
  gv_main        TYPE hvorg_kk,
  gv_sub         TYPE tvorg_kk,
  gv_fikey_lote  TYPE dfkksumc-fikey,
  gv_error_param TYPE c,
  gv_id_archivo  TYPE zedu_id_archivo.

DATA: gv_dir_in  TYPE pathextern,
      gv_dir_out TYPE pathextern,
      gv_dir_err TYPE pathextern,
      gv_dir_bkp TYPE pathextern.

DATA:
  gv_nro_registro(10) TYPE n,
  gv_tipo_registro(2) TYPE c.

CONSTANTS: gc_msg_type_e TYPE bapi_mtype VALUE 'E',
           gc_msg_type_w TYPE bapi_mtype VALUE 'W',         "#EC NEEDED
           gc_msg_type_i TYPE bapi_mtype VALUE 'I',
           gc_msg_id     TYPE symsgid VALUE 'ZDEDU_RECAUDACION'.

** Status posibles del archivo según errores
CONSTANTS: gc_status_es TYPE zedu_r_ctrl_arch-status VALUE 'ES', "Errores de Sintaxis
           gc_status_ev TYPE zedu_r_ctrl_arch-status VALUE 'EV', "Errores de Validaciones de Negocio
           gc_status_ok TYPE zedu_r_ctrl_arch-status VALUE 'OK'. "Ingresados Ok
***************************************************************
CONSTANTS: gc_object    TYPE bal_s_log-object VALUE 'ZEDU_RECAUDACION',
           gc_subobject TYPE bal_s_log-subobject VALUE 'ZEDU_CARGA',
           gc_blart(5)  TYPE c VALUE 'BLART',
           gc_waers(5)  TYPE c VALUE 'WAERS',
           gc_augrd(5)  TYPE c VALUE 'AUGRD',
           gc_selt1(5)  TYPE c VALUE 'SELT1',
           gc_selt2(5)  TYPE c VALUE 'SELT2',
           gc_bukrs(10) TYPE c VALUE 'ZCRE_SOCIE'.
DATA: gv_log TYPE REF TO zcl_edu_log.
*&--------------------------------------------------------------------&*
*&                       PANTALLA DE SELECCION                        &*
*&--------------------------------------------------------------------&*
PARAMETERS: p_arch   TYPE file_table-filename,
            p_correc AS CHECKBOX.
