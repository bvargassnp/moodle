*----------------------------------------------------------------------*
***INCLUDE LZDEDU_GESTION_DIR_SERVF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_OBTIENE_DIRECTORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FILE_DIRLOG_PREFIJO  text
*      -->P_CO_SUFIJO_IN  text
*      <--P_E_DIR_IN  text
*----------------------------------------------------------------------*
FORM f_obtiene_directorio  USING    p_prefijo TYPE zedu_prefijo_directorio
                                    p_sufijo  TYPE csequence
                           CHANGING p_dir     TYPE pathextern.

  DATA: lv_fileintern TYPE fileintern.

  CONCATENATE p_prefijo p_sufijo INTO lv_fileintern.
  TRANSLATE lv_fileintern TO UPPER CASE.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
*     CLIENT           = SY-MANDT
      logical_filename = lv_fileintern
*     OPERATING_SYSTEM = SY-OPSYS
*     PARAMETER_1      = ' '
*     PARAMETER_2      = ' '
*     PARAMETER_3      = ' '
*     USE_PRESENTATION_SERVER       = ' '
*     WITH_FILE_EXTENSION           = ' '
*     USE_BUFFER       = ' '
*     ELEMINATE_BLANKS = 'X'
    IMPORTING
*     EMERGENCY_FLAG   =
*     FILE_FORMAT      =
      file_name        = p_dir
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " F_OBTIENE_DIRECTORIO
