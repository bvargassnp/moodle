FUNCTION-POOL zdedu_actualizar_estado_es.   "MESSAGE-ID ..

* INCLUDE LZDEDU_ACTUALIZAR_ESTADO_ESD...    " Local class definition

TYPES:

  BEGIN OF tys_dfkkko,
    opbel TYPE dfkkko-opbel,
    cpudt TYPE dfkkko-cpudt,
    cputm TYPE dfkkko-cputm,
  END OF tys_dfkkko,

  tyt_dfkkko TYPE STANDARD TABLE OF tys_dfkkko.

CONSTANTS:

  c_a(1)    TYPE c VALUE 'A',
  c_1(1)    TYPE c VALUE '1',
  c_01(2)   TYPE c VALUE '01',
  c_07(2)   TYPE c VALUE '07',
  c_08(2)   TYPE c VALUE '08',
  c_fa(2)   TYPE c VALUE 'FA',
  c_a010(4) TYPE c VALUE 'A010',
  c_3000(4) TYPE c VALUE '3000'.
