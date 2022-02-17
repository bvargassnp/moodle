FUNCTION z_edu_get_fecha_examen_entrev.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_PLAN_ESTUDIO) TYPE  HROBJID
*"     REFERENCE(IV_ANIO) TYPE  PIQPERYR
*"     REFERENCE(IV_PERIODO) TYPE  PIQPERID
*"     REFERENCE(IV_TIPO_FECHA) TYPE  PIQEVOBTYPE
*"  TABLES
*"      ET_FECHAS TYPE  ZEDU_T_FEC_EXAM_ENTREVISTA
*"----------------------------------------------------------------------

  DATA: lt_hrp1001     TYPE TABLE OF t_objid,
        lt_zedu_prad   TYPE TABLE OF t_zedu_prad,
        lt_hrp1001_aux TYPE TABLE OF t_objid_aux,
        ls_zedu_prad   TYPE t_zedu_prad,
        ls_fechas      TYPE zedu_s_fec_exam_entrevista.

  SELECT a~objid
         a~sobid
         a~otype
         a~sclas
         a~rsign
         a~relat
         a~subty
         a~adatanr
         a~istat
     FROM hrp1001 AS a
     INNER JOIN hrp1746 AS b
     ON a~sobid = b~objid
     INTO TABLE lt_hrp1001
     WHERE a~otype    = c_sc
       AND a~objid    = iv_plan_estudio
       AND a~sclas    = c_sm
       AND a~rsign    = c_a
       AND a~relat    = c_500
       AND b~plvar    = c_01
       AND b~severity = 5.
  IF sy-subrc = 0.

    PERFORM f_completar_auxiliar
    USING lt_hrp1001
    CHANGING lt_hrp1001_aux.

    CHECK lt_hrp1001_aux IS NOT INITIAL.

    SELECT a~objid
           a~sobid
           a~otype
           a~sclas
           a~rsign
           a~relat
           a~subty
           a~adatanr
           a~istat
       FROM hrp1001 AS a
*       INNER JOIN hrp1746 AS b
*       ON a~sobid = b~objid
       INNER JOIN hrp1766 AS c
       ON a~sobid = c~objid
      AND a~sclas = c~otype
       INTO TABLE lt_hrp1001
       FOR ALL ENTRIES IN lt_hrp1001_aux
       WHERE a~otype    = c_sm
         AND a~objid    = lt_hrp1001_aux-sobid
         AND a~sclas    = c_ce
*         AND a~rsign    = c_a
*         AND a~relat    = c_500
*         AND b~plvar    = c_01
*         AND b~severity = 5
         AND c~evobtype = iv_tipo_fecha.
    IF sy-subrc = 0.

      PERFORM f_completar_auxiliar
      USING lt_hrp1001
      CHANGING lt_hrp1001_aux.

      CHECK lt_hrp1001_aux IS NOT INITIAL.

      SELECT objid
         FROM hrp1739
         INTO TABLE lt_hrp1001
         FOR ALL ENTRIES IN lt_hrp1001_aux
         WHERE otype = c_ce
           AND objid = lt_hrp1001_aux-sobid
           AND peryr = iv_anio
           AND perid = iv_periodo
           AND plvar = c_01.
      IF sy-subrc IS NOT INITIAL.
        REFRESH lt_hrp1001.
      ELSE.
        SELECT a~evalobj_id
               a~objid
               a~status2
               b~examdate
               b~exambegtime
               b~location_objid
*	Begin	-->	MgM DCEK903737 texto de locatión 06/03/2017
               c~stext
*	End	  -->	MgM DCEK903737
          FROM piqdbcmprrecords AS a
          JOIN piqdbevoboffer3 AS b
          ON  a~evalobj_id = b~evob_objid
          and a~peryr      = b~peryr  "HIRS 02/10/2017 "Solucion Issues directorio de Inscritos
          and a~perid      = b~perid  "HIRS 02/10/2017 "Solucion Issues directorio de Inscritos
          and a~offerno    = b~offerno
*	Begin	-->	MgM DCEK903737 texto de locatión 06/03/2017
            left OUTER JOIN hrp1000 as c
              on b~location_objid = c~objid and
                 c~otype eq cl_hrpiq00const=>c_otype_f
*	End	  -->	MgM DCEK903737
          INTO TABLE et_fechas
          FOR ALL ENTRIES IN lt_hrp1001
          WHERE a~evalobj_id = lt_hrp1001-objid
            AND a~peryr      = iv_anio
            AND a~perid      = iv_periodo.
        IF sy-subrc IS NOT INITIAL.
          REFRESH et_fechas.
        ELSE.
          SELECT a~objid_st
                 a~test_id
                 a~concepto
                 a~keyobs
                 b~valoracion
             FROM zedu_pradm AS a
             JOIN zedu_pradt AS b
             ON a~keyobs = b~keyobs
             INTO TABLE lt_zedu_prad
             FOR ALL ENTRIES IN et_fechas
             WHERE objid_st = et_fechas-objid.
          IF sy-subrc IS NOT INITIAL.
            REFRESH  lt_zedu_prad.
          ELSE.
            SORT lt_zedu_prad.

            LOOP AT et_fechas INTO ls_fechas.
              READ TABLE lt_zedu_prad INTO ls_zedu_prad
                WITH KEY objid_st = ls_fechas-objid
                BINARY SEARCH.
              IF sy-subrc = 0.
                ls_fechas-concepto   = ls_zedu_prad-concepto.
                ls_fechas-valoracion = ls_zedu_prad-valoracion.

                MODIFY et_fechas
                FROM ls_fechas
                TRANSPORTING concepto
                             valoracion.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF..




ENDFUNCTION.
