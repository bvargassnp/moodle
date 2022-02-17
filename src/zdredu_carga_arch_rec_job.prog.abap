REPORT zdredu_carga_arch_rec_job.
***********************************************************************
* Cambiado por : Leonardo de Jesus Pavia (  ).
* Cambiado el  : 03/02/2017 .
* Orden de Transporte  :
* Frente  :
* ID de desarrollo  :
* Descripción  : Job para leer ruta de archivos de asobancaria y procesar
*                automáticamente los archivos allí encontrados.
*                Por cada archivo se realiza llamado a programa zdredu_carga_arch_rec
***********************************************************************
* ID    | Fecha      | Orden      | Responsable | Descripción          *
***********************************************************************
* M0001 | DD.MM.AAAA | DSRK9XXXXX | Usuario SAP | Objetivo de la modif.*


INCLUDE zdredu_carga_arch_rec_job_f01.

START-OF-SELECTION.

  "Leer ruta de archivos de asobancaria y procesar archivos
  PERFORM f_leer_ruta.
