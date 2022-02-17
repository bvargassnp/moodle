FUNCTION-POOL zdedu_reclamacion_fica.       "MESSAGE-ID ..

* INCLUDE LZDEDU_RECLAMACION_FICAD...        " Local class definition

DATA: gv_ackey  TYPE ackey_kk.

CONSTANTS: const_marked            VALUE 'X',
           const_aggrd_dunning     LIKE dfkkcoll-aggrd VALUE '04',
           const_agsta_freigegeben LIKE dfkkcoll-agsta VALUE '01'.
