*&---------------------------------------------------------------------*
*&  Include           ZDREDU_ESTADO_ESTUDIANTES_TOP
*&---------------------------------------------------------------------*

DATA: gt_alv     TYPE TABLE OF zedu_s_rep_estado_estudi,
      gt_catalog TYPE slis_t_fieldcat_alv.

CONSTANTS:
  c_student12(15)        TYPE c VALUE 'STUDENT12',
  c_type(15)             TYPE c VALUE 'TYPE',
  c_idnumber(15)         TYPE c VALUE 'IDNUMBER',
  c_name_last(15)        TYPE c VALUE 'NAME_LAST',
  c_name_first(15)       TYPE c VALUE 'NAME_FIRST',
  c_plan(15)             TYPE c VALUE 'PLAN',
  c_peryr(15)            TYPE c VALUE 'PERYR',
  c_perid(15)            TYPE c VALUE 'PERID',
  c_prcl(15)             TYPE c VALUE 'PRCL',
  c_adm_categ(15)        TYPE c VALUE 'ADM_CATEG',
  c_adm_categt(15)       TYPE c VALUE 'ADM_CATEGT',
  c_cpattemp(15)         TYPE c VALUE 'CPATTEMP',
  c_faedn(15)            TYPE c VALUE 'FAEDN',
  c_val_matricula(15)    TYPE c VALUE 'VAL_MATRICULA',
  c_val_recargo(15)      TYPE c VALUE 'VAL_RECARGO',
  c_val_beneficio(15)    TYPE c VALUE 'VAL_BENEFICIO',
  c_nombre_beneficio(16) TYPE c VALUE 'NOMBRE_BENEFICIO',
  c_val_inter_sem(15)    TYPE c VALUE 'VAL_INTER_SEM',
  c_val_saldo_favor(15)  TYPE c VALUE 'VAL_SALDO_FAVOR',
  c_val_real_pagado(15)  TYPE c VALUE 'VAL_REAL_PAGADO',
  c_val_financiado(15)   TYPE c VALUE 'VAL_FINANCIADO',
  c_nombre_financia(15)  TYPE c VALUE 'NOMBRE_FINANCIA',
  c_val_interes(15)      TYPE c VALUE 'VAL_INTERES',
  c_val_otras_deudas(16) TYPE c VALUE 'VAL_OTRAS_DEUDAS',
  c_val_saldo_actual(16) TYPE c VALUE 'VAL_SALDO_ACTUAL',
  c_subty(15)            TYPE c VALUE 'SUBTY',
  c_sutxt(15)            TYPE c VALUE 'SUTXT',
  c_estado_matricula(16) TYPE c VALUE 'ESTADO_MATRICULA',
  c_bukrs(4)             TYPE c VALUE 'UCES' .
