*&---------------------------------------------------------------------*
*&  Include           ZDIEDU_EXTR_ENVIO_PAGO_TOP
*&---------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&                               TYPES                                &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF gty_but0id,
         partner       TYPE bu_partner,
         type          TYPE bu_id_type,
         idnumber      TYPE  bu_id_number,
         valid_date_to TYPE	bu_id_valid_date_to,
       END OF gty_but0id.

TYPES: BEGIN OF gty_but000,
         partner    TYPE bu_partner,
         name_last  LIKE but000-name_last,
         name_lst2  LIKE but000-name_lst2,
         name_first LIKE but000-name_first,
         namemiddle LIKE but000-namemiddle,
       END OF gty_but000.

TYPES: BEGIN OF gty_but021,
         partner    TYPE bu_partner,
         addrnumber TYPE ad_addrnum,
       END OF gty_but021.

TYPES: BEGIN OF gty_adr6,
         addrnumber TYPE ad_addrnum,
         smtp_addr  TYPE ad_smtpadr,
         flgdefault	TYPE ad_flgdfad,
         home_flag  TYPE ad_flghome,
       END OF gty_adr6.

*&--------------------------------------------------------------------&*
*&                           DATOS GLOBALES                           &*
*&--------------------------------------------------------------------&*

DATA: gt_moodle TYPE STANDARD TABLE OF zedu_int_moodle,
      gt_but0id TYPE STANDARD TABLE OF gty_but0id,
      gt_but000 TYPE STANDARD TABLE OF gty_but000,
      gt_but021 TYPE STANDARD TABLE OF gty_but021,
      gt_adr6   TYPE STANDARD TABLE OF gty_adr6,
      gt_hrp    TYPE STANDARD TABLE OF hrp1000.


*----------------------------------------------------------------------*
* Definición de Parámetros y Select-Options
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: pa_est TYPE zedu_est_mod DEFAULT 'NP' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
