*&------------------------------------------------------------------------*
*& Nombre Transaccion :                                                   *
*& Nombre  Prog       : ZDREDU_ESTADO_ESTUDIANTES                         *
*& ID Requerimiento   : EF030-SLCM-3-EDU                                  *
*& Modulo             : SLCM                                              *
*& Tipo Programa      : Reporte                                           *
*& Descripción        : Reporte de Estado de Estudiantes                  *
*&------------------------------------------------------------------------*
*& Compañía            : UNIVERSIDAD CES                                  *
*& Autor(es)           : Diego Gagliardi                                  *
*& Fecha               : 14-06-2016                                       *
*&------------------------------------------------------------------------*
*&------------------------------------------------------------------------*
*&                L O G.   D E   M O D I F I C A C I O N E S              *
*&------------------------------------------------------------------------*
*&  FECHA       REQ #      PROGRAMADOR             DESCRIPCIÓN            *
*&------------------------------------------------------------------------*
*& 2016-06-14   ZSLCM    Gagliardi Diego          DESARROLLO INICIAL      *
*&------------------------------------------------------------------------*
REPORT zdredu_estado_estudiantes.

INCLUDE zdredu_estado_estudiantes_top.
INCLUDE zdredu_estado_estudiantes_sel.
INCLUDE zdredu_estado_estudiantes_f01.


START-OF-SELECTION.
  PERFORM f_get_data.
end-of-SELECTION.
  PERFORM f_show_alv.
