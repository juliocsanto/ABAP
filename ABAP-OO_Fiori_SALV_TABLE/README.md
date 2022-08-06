# ABAP OO and Fiori Example for work with CL_SALV_TABLE class

Transaction ZTST_JN_SALV_TABLE created on S/4HANA with ABAP OO was fiorized.

Helped me:
 - https://blogs.sap.com/2019/03/15/sap-fiori-catalog-business-group-role-creation/

Steps needed:
 - Create Transaction Code on tcode SE93 for developed program 
 - Create Semantic Object on tcode /UI2/SEMOBJ
 - Include portuguese translation to semantic object created (Goto -> Translation -> Select the idioms needed)
 - Create catalog and group on fiori lauchpad using the semantic object created, accessed through tcode /ui2/flpd_cust (see link above)
 - Create role for this catalog and this group on tcode PFCG using catalog and group created (see link above) 

## View on S/4HANA

![view-on-s4](view-on-s4-1.png?raw=true)
![view-on-s4](view-on-s4-2.png?raw=true)
![view-on-s4](view-on-s4-3.png?raw=true)

## View on Fiori Launchpad

![view-on-lp](view-on-lp1.png?raw=true)
![view-on-lp](view-on-lp2.png?raw=true)
![view-on-lp](view-on-lp3.png?raw=true)
![view-on-lp](view-on-lp4.png?raw=true)
