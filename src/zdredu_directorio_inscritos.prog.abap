report zdredu_directorio_inscritos.

include zdredu_directorio_inscri_top.
include zdredu_directorio_inscri_sel.
include zdredu_directorio_inscri_f01.

start-of-selection.

  perform f_obtener_datos.

end-of-selection.

  perform f_mostrar_datos.
