*&---------------------------------------------------------------------*
*&  Include           ZDREDU_DIRECTORIO_INSCRI_SEL
*&---------------------------------------------------------------------*

data: ls_hrp1000  type hrp1000,
      ls_hrp1001  type hrp1001,
      ls_cmacbpst type cmacbpst,
      ls_hrp1702  type hrp1702,
      ls_piqstadm type piqstadm.

selection-screen begin of block b01 with frame title text-001.
parameters:  p_facul  type hrp1000-objid
             as listbox visible length 35
             user-command facul,

             p_tippro type hrp1001-objid
             as listbox visible length 35
             user-command tippro,

             p_peryr  type t7piqyear-peryr
             as listbox visible length 8
             user-command anio,

             p_perid  type t7piqperiod-perid
             as listbox visible length 20
             user-command period.

*	Begin	-->	MgM DCEK903347 Ajustes varios 07/02/2017
*             p_planes TYPE hrp1000-objid
*             AS LISTBOX VISIBLE LENGTH 35
*             USER-COMMAND planes.
*	End	  -->	MgM DCEK903347

selection-screen end of block b01.

selection-screen begin of block b02 with frame title text-002.
*	Begin	-->	MgM DCEK903347 Ajustes varios 07/02/2017
parameters  p_planes type hrp1000-objid
             as listbox visible length 35
             user-command planes.
*	End	  -->	MgM DCEK903347
select-options: s_nromat for ls_cmacbpst-matrikel  no intervals,
                s_nroid  for ls_hrp1702-prdni      no intervals.
*	Begin	-->	MgM DCEK903347 Ajustes varios 24/02/2017
*                s_tiasp  for ls_piqstadm-adm_categ no intervals.
*	End	  -->	MgM DCEK903347
selection-screen end of block b02.

selection-screen begin of block b03 with frame title text-003.
parameters: p_inscr  as checkbox default 'X',
            p_admit  as checkbox, " default 'X', "-->	MgM DCEK903347
            p_matr_a as checkbox, " default 'X', "-->	MgM DCEK903347
            p_matr_n as checkbox, " default 'X'. "-->	MgM DCEK903347
*	Begin	--> FROSENZVAIG - DCEK906683 - 10/10/2017
            p_rech   AS CHECKBOX.
*	End	--> FROSENZVAIG - DCEK906683 - 10/10/2017
selection-screen end of block b03.

selection-screen begin of block b04 with frame title text-004.
parameters: p_layout type salv_s_layout_key.
selection-screen end of block b04.

at selection-screen on value-request for p_layout.
  perform f_cargar_layouts.

at selection-screen on p_facul.
  perform f_llenar_tipos_programa.

at selection-screen on p_tippro.
  perform f_llenar_anio.
  perform f_llenar_planes_estudio.

at selection-screen on p_peryr.
  perform f_llenar_periodos.

*	Begin	-->	MgM DCEK903347 Ajustes varios 09/02/2017
at selection-screen on p_perid.
  perform f_llenar_planes_estudio.
*	End	  -->	MgM DCEK903347

at selection-screen.
  perform f_vaciar_campos.

at selection-screen output.
  perform f_llenar_facultad.
