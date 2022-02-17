FUNCTION ZEDU_LIQUIDACION_PAGO_FORM_RFC.
*"----------------------------------------------------------------------
*"*"Módulo funciones actualiz.
*"
*"*"Interfase local
*"  IMPORTING
*"     VALUE(I_XBLNR) TYPE  XBLNR_KK
*"     VALUE(I_BLART) TYPE  BLART_KK
*"     VALUE(I_OPBEL) TYPE  OPBEL_KK OPTIONAL
*"     VALUE(I_NO_COMMIT) TYPE  CHECK OPTIONAL
*"----------------------------------------------------------------------
***********************************************************************
* Cambiado por : Leonardo de Jesus Pavia ( LEONARDOPL ).
* Cambiado el  : 29/06/2017 .
* Orden de Transporte  :
* Frente  :
* ID de desarrollo  :
* Descripción  : Función para generación automática en fondo del
*                formulario de liquidación cuando se crea un documento
*                contable y luego que se asigna el documento de referencia original.
*                Se llama desde evento 0010 Z_EDU_FKK_ASIG_DOC_ORIG_0010
***********************************************************************
* ID    | Fecha      | Orden      | Responsable | Descripción          *
***********************************************************************
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*

  CONSTANTS: lco_intentos TYPE i VALUE 3,
             lco_segundos TYPE i VALUE 5.

  DATA: ls_fechas         TYPE zedu_s_liquidacion_pago_fechas,
        lv_id_liquidacion TYPE zid_liquidacion,
        lv_opbel          TYPE opbel_kk,
        lt_blart          TYPE SORTED TABLE OF blart_kk WITH NON-UNIQUE KEY primary_key COMPONENTS table_line,
        lv_blart          TYPE blart_kk,
        lt_psobtyp        TYPE SORTED TABLE OF psobtyp_ps WITH NON-UNIQUE KEY primary_key COMPONENTS table_line,
        lt_id_liq         TYPE SORTED TABLE OF zid_liquidacion WITH NON-UNIQUE KEY primary_key COMPONENTS table_line.



  "Esperar n intentos de x segundos hasta que el documento opbel sea creado y se pueda imprimir el recibo.
  "Esta función es llamada en UPDATE TASK desde el evento Z_EDU_FKK_ASIG_DOC_ORIG_0010
  IF i_opbel IS NOT INITIAL.
    DO lco_intentos TIMES.
      SELECT SINGLE opbel
        INTO @DATA(lv_opbel_wait)
        FROM dfkkko
        WHERE opbel = @i_opbel.

      IF lv_opbel_wait IS INITIAL.
        WAIT UP TO lco_segundos SECONDS.
      ENDIF.
    ENDDO.
  ENDIF.

  "Obtener Customizng de tipos de documento permitidos para Generación de Liquidación Automática
  SELECT *
    INTO TABLE @DATA(lt_c_param)
    FROM zedu_c_param
    WHERE repid = @sy-repid.

  CHECK sy-subrc = 0.

  LOOP AT lt_c_param INTO DATA(ls_c_param).
    CHECK ls_c_param-idparam = 'BLARTLIQAU'.
    lv_blart = ls_c_param-valor.
    INSERT lv_blart INTO TABLE lt_blart.
  ENDLOOP.

  READ TABLE lt_blart TRANSPORTING NO FIELDS WITH KEY primary_key COMPONENTS table_line = i_blart.

  CHECK sy-subrc = 0.

  "Determinar automáticamente tipo de Liquidación a generar de acuerdo al tipo de contrato
  SELECT psobtyp
    INTO TABLE lt_psobtyp
    FROM dfkkop
    WHERE xblnr = i_xblnr.

  DELETE ADJACENT DUPLICATES FROM lt_psobtyp.

  "Obtener customizing de Tipos de Liquidación según Tipo de PSOB
  IF lt_psobtyp IS NOT INITIAL.
    SELECT id_liquidacion
     INTO TABLE lt_id_liq
     FROM zedu_contr_liqui
     FOR ALL ENTRIES IN lt_psobtyp
     WHERE psobtyp = lt_psobtyp-table_line.

    "Obtener tipo de liquidación
    DELETE ADJACENT DUPLICATES FROM lt_id_liq.

    READ TABLE lt_id_liq INTO DATA(ls_id_liq) INDEX 1.
    IF sy-subrc = 0.
      lv_id_liquidacion = ls_id_liq.
    ENDIF.
  ENDIF.

  IF lv_id_liquidacion IS INITIAL.
    lv_id_liquidacion = '001'.
  ENDIF.

  ls_fechas-calendario = 'X'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = i_xblnr
    IMPORTING
      output = lv_opbel.

  CALL FUNCTION 'ZEDU_LIQUIDACION_PAGO_FORM'
    EXPORTING
      i_opbel          = lv_opbel
      i_id_liquidacion = lv_id_liquidacion
      i_fechas         = ls_fechas
      i_otf            = 'X'
      i_no_commit      = i_no_commit.
ENDFUNCTION.
