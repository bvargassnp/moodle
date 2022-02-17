* modulpool infotype 9002
PROGRAM MP911900 MESSAGE-ID 5A.
INCLUDE MPH5ATOP.                      "header
Include rhiqst_nf_tabdefinitions.
Include rhiqst_nf_tabgeneral.
TABLES: WPLOG,
        PPPAR, PPHDR, PPHDX, PPSEL, PPENQ,
        T777O, T777P, T777S, T777T,
        P1000, P1001, P9119, pt9119.
DATA: gv_first_time TYPE char1.
DATA: gs_hrt9119 TYPE hrt9119.
INCLUDE MPHCOM00.                      "common areas
INCLUDE FHVTAB00.                      "update tables
INCLUDE FHVIEW00.                      "USER-VIEW
INCLUDE MPHFCOD0.                      "function codes
INCLUDE MPHDAT00.                      "general data
INCLUDE MPHPBO00.                      "PBO modules
INCLUDE MPHPAI00.                      "PAI modules
INCLUDE MP900220.                      "specific PAI/PBO modules
INCLUDE MP911920.                      "specific PAI/PBO modules
*include mpxxxxbi.                      "Batch-Input von der WPLOG

INCLUDE MP911900_INIT_0300_EARLYO01.
*INCLUDE mp900200_init_0300_earlyo01.

INCLUDE MP911900_INIT_0300_EARLYO02.
*INCLUDE mp900200_init_0300_earlyo02.

INCLUDE mp9119i0.

INCLUDE mp9119f0.
