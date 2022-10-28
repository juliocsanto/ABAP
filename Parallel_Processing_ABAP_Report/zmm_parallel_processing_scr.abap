*&---------------------------------------------------------------------*
*& Include          zmm_parallel_processing_scr
*&---------------------------------------------------------------------*
************************************************************************
* Screen selection
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr.                "material number
PARAMETERS:     p_werks TYPE werks_d OBLIGATORY.       "plant
SELECT-OPTIONS: s_dispo FOR marc-dispo OBLIGATORY.
SELECT-OPTIONS: s_dismm FOR marc-dismm.                 "mrp type
SELECTION-SCREEN END OF BLOCK b1.