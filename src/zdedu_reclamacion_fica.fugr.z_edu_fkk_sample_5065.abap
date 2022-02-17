FUNCTION z_edu_fkk_sample_5065.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      T_DFKKCOLL STRUCTURE  DFKKCOLL
*"      T_ALL_COLL STRUCTURE  DFKKCOLL
*"      T_RECALL_COLL STRUCTURE  DFKKCOLL
*"      T_REASSIGN_COLL STRUCTURE  DFKKCOLL
*"  CHANGING
*"     VALUE(DO_RECALL) LIKE  BOOLE-BOOLE
*"  EXCEPTIONS
*"      ERROR_FOUND
*"----------------------------------------------------------------------

  FIELD-SYMBOLS: <fs_reassing> TYPE dfkkcoll.

  CHECK gv_ackey <> '0009'.

  do_recall = 'X'.
  t_recall_coll[]   = t_dfkkcoll[].
  t_reassign_coll[] = t_dfkkcoll[].

  LOOP AT t_reassign_coll ASSIGNING <fs_reassing>.
    <fs_reassing>-aggrd = 'JR'.
  ENDLOOP.


ENDFUNCTION.
