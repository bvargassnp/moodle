FUNCTION-POOL zdedu_get_fechas_admi.        "MESSAGE-ID ..

* INCLUDE LZDEDU_GET_FECHAS_ADMID...         " Local class definition

TYPES:     BEGIN OF t_objid,
             objid        TYPE hrp1001-objid,
             sobid        TYPE hrp1001-sobid,
             otype        TYPE hrp1001-otype,
             sclas        TYPE hrp1001-sclas,
             rsign        TYPE hrp1001-rsign,
             relat        TYPE hrp1001-relat,
             subty        TYPE hrp1001-subty,
             adatnr       TYPE hrp1001-adatanr,
             istat        TYPE hrp1001-istat,
             adm_enrcateg TYPE hrpad530-adm_enrcateg,
             choice_no    TYPE hrpad530-choice_no,
             adm_aclevel  TYPE hrpad530-adm_aclevel,
             adm_ayear    TYPE hrpad530-adm_ayear,
             adm_perid    TYPE hrpad530-adm_perid,
           END OF t_objid,

           BEGIN OF t_objid_aux,
             objid TYPE hrp1001-objid,
             sobid TYPE hrp1001-objid,
           END OF t_objid_aux,

           BEGIN OF t_zedu_prad,
             objid_st   TYPE zedu_pradm-objid_st,
             test_id    TYPE zedu_pradm-test_id,
             concepto   TYPE zedu_pradm-concepto,
             keyobs     TYPE zedu_pradm-keyobs,
             valoracion TYPE zedu_pradt-valoracion,
           END OF t_zedu_prad,

           tyt_objid     TYPE TABLE OF t_objid,
           tyt_objid_aux TYPE TABLE OF t_objid_aux.

CONSTANTS: c_a(1)   TYPE c VALUE 'A',
           c_01(2)  TYPE c VALUE '01',
           c_ce(2)  TYPE c VALUE 'CE',
           c_sc(2)  TYPE c VALUE 'SC',
           c_sm(2)  TYPE c VALUE 'SM',
           c_500(3) TYPE c VALUE '500'.
