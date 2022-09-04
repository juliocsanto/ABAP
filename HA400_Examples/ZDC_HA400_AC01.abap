@EndUserText.label: 'Role for SBOOK'
@MappingRole: true
define role ZDC_HA400_AC01 {
    grant select
          on ZDD_HA400_D09_Exposed_Asso
       where ( carrid ) = aspect pfcg_auth ( 
            S_CARRID,       --Auth. object
            CARRID,         --Matched auth.
                            --object field
            ACTVT = '03' ); --Fixed auth
                            --object field                        
}
