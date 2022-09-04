*&---------------------------------------------------------------------*
*& Report zjntst
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_select_using_associations_views.

SELECT FROM ZDD_HA400_D09_Exposed_Asso
       FIELDS carrid, connid,
              SUM( loccuram ) as total_amount, currcode,
              \_Customer-postcode,
              \_Customer-city,
              \_Customer-street

  WHERE carrid EQ 'LH'
    AND \_Customer-country EQ 'US'
  GROUP BY carrid, connid, currcode,
           \_Customer-postcode,
           \_Customer-city,
           \_Customer-street
  INTO TABLE @DATA(LT_CUSTOMERS).