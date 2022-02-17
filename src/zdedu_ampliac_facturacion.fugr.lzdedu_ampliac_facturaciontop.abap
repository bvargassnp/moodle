FUNCTION-POOL zdedu_ampliac_facturacion.    "MESSAGE-ID ..

* INCLUDE LZDEDU_AMPLIAC_FACTURACIOND...     " Local class definition

CONSTANTS:
  co_e(1)     TYPE c VALUE 'E',
  co_10(2)    TYPE c VALUE '10',
  co_000(3)   TYPE c VALUE '000',
  co_fkkdm(5) TYPE c VALUE 'FKKDM'.

TYPES:

  BEGIN OF tys_t77refdoc,
    document TYPE t77refdoc-document,
    eotyp    TYPE t77refdoc-eotyp,
    eveid    TYPE t77refdoc-eveid,
  END OF tys_t77refdoc,

  BEGIN OF tys_hrp9112,
    otype      TYPE hrp9112-otype,
    objid      TYPE hrp9112-objid,
    begda      TYPE hrp9112-begda,
    endda      TYPE hrp9112-endda,
    tabnr      TYPE hrp9112-tabnr,
    ordinterna TYPE hrt9112-tabnr,
  END OF tys_hrp9112.
