@AbapCatalog.sqlViewName: 'ZCDS_H1035'
@AbapCatalog.compiler.CompareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Días Evento'
define view ZCDS_V_H1035 as 
    select from hrp1035 as hp
        inner join hrt1035 as ht on ht.tabnr = hp.tabnr
{

    hp.plvar,
    hp.otype,
    hp.objid,
    hp.subty,
    hp.istat,
    hp.begda,
    hp.endda,
    hp.varyf,
    hp.seqnr,
    ht.tabnr,
    ht.tabseqnr,
    ht.evdat,
    ht.beguz,
    ht.enduz
    
}
