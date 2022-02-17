FUNCTION z_edu_wfencontrar_usuario .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"      AC_CONTAINER STRUCTURE  SWCONT
*"----------------------------------------------------------------------

  INCLUDE <cntn01>.

  DATA :

    lt_holders  TYPE STANDARD TABLE OF swhactor,
    lwa_holders TYPE swhactor,
    lwa_users   TYPE STANDARD TABLE OF hrp1001,
    wa_users    TYPE hrp1001,
    num_lines   TYPE i,
    lt_usr      TYPE usmd_t_user,
    ls_usr      LIKE LINE OF lt_usr.

*Read values assigned to the rule criteria
*  swc_get_element ac_container 'CLAVE' clave.
*  swc_get_element ac_container 'NIVEL' nivel.

  SELECT b~bname INTO TABLE lt_usr
    FROM zedu_roles_tram AS a
    INNER JOIN zedu_usr_grupor AS b
    ON a~grp_resp = b~grp_resp
    WHERE a~aprobacion = 'X'.

  IF NOT lt_usr IS INITIAL.
    REFRESH lt_holders[].
    LOOP AT lt_usr INTO ls_usr.
      CONDENSE ls_usr.
      lwa_holders-otype = 'US'.
      lwa_holders-objid = ls_usr.
      APPEND lwa_holders TO lt_holders.
      APPEND LINES OF lt_holders TO actor_tab.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE actor_tab LINES num_lines.
  IF num_lines IS INITIAL.
    RAISE noagent_found.
  ENDIF.

ENDFUNCTION.
