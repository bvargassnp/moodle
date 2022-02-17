FUNCTION-POOL zdedu_facturacion.            "MESSAGE-ID .
DATA: gt_konh TYPE TABLE OF konh,     "Tabla cabecera condiciones
      gr_konh TYPE konh,              "Rec. cabecera condiciones
      gt_konp TYPE TABLE OF konp,     "Tabla posiciones de condiciones
      gr_konp TYPE konp.              "Rec. posiciones de condiciones

 CONSTANTS:
   gc_KAPPL type KAPPL value 'CM'.   "Aplicación CM

* INCLUDE LZDEDU_FACTURACIOND...             " Local class definition
