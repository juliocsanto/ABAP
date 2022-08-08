*&---------------------------------------------------------------------*
*& Include          ZTST_JN_SALV_TABLE_SCR
*&---------------------------------------------------------------------*
TABLES: spfli.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_carid FOR spfli-carrid,
                  so_conid FOR spfli-connid,
                  so_afrom FOR spfli-airpfrom,
                  so_ato   FOR spfli-airpto.
SELECTION-SCREEN END OF BLOCK b1.
