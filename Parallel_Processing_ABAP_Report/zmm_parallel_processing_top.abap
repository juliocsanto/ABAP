*&---------------------------------------------------------------------*
*& Include          zmm_parallel_processing_top
*&---------------------------------------------------------------------*
TABLES: mara, marc.

*Class definition for handling double click
CLASS gcl_handle_events DEFINITION DEFERRED.

TYPES: BEGIN OF ty_matnr,
         matnr TYPE matnr,
       END OF ty_matnr,
       tty_matnr TYPE STANDARD TABLE OF ty_matnr WITH EMPTY KEY.

DATA: g_okcode TYPE sy-ucomm.
DATA lv_matnr TYPE matnr.
DATA ls_mrp_list TYPE bapi_mrp_list.
DATA ls_mrp_control_param TYPE bapi_mrp_control_param.
DATA ls_mrp_stock_detail TYPE bapi_mrp_stock_detail.
DATA ls_return TYPE bapiret2.
DATA lt_mrp_items TYPE TABLE OF bapi_mrp_items.
DATA ls_mrp_items TYPE bapi_mrp_items.
DATA lt_mrp_ind_lines TYPE TABLE OF bapi_mrp_ind_lines.
DATA ls_mrp_ind_lines TYPE bapi_mrp_ind_lines.
DATA lt_mrp_total_lines TYPE TABLE OF bapi_mrp_total_lines.
DATA ls_mrp_total_lines TYPE bapi_mrp_total_lines.
DATA g_container        TYPE scrfname VALUE 'ZMM_SHORTAGE_0100_CONT1'.
DATA grid1              TYPE REF TO cl_gui_alv_grid.
DATA g_custom_container TYPE REF TO cl_gui_custom_container.
DATA gs_layout          TYPE lvc_s_layo.
DATA ok_code            LIKE sy-ucomm.
DATA lt_fcat            TYPE lvc_t_fcat.
DATA gt_matnr TYPE TABLE OF ty_matnr.
DATA gt_shortage TYPE TABLE OF zmm_shortage.
DATA ls_shortage TYPE zmm_shortage.
DATA lv_prev_date TYPE zavail_date.
DATA c_alv1 TYPE REF TO cl_gui_alv_grid.
DATA c_cont1 TYPE REF TO cl_gui_custom_container.
DATA event_receiver TYPE REF TO gcl_handle_events.

DATA: gr_table TYPE REF TO cl_salv_table.

DATA: gv_processed   TYPE i.

************************************************************************
* Class event definition
************************************************************************
CLASS gcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS: handle_double_click FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.

ENDCLASS.

************************************************************************
* Class event implementation
************************************************************************
CLASS gcl_handle_events IMPLEMENTATION.

  METHOD handle_double_click.
*Reading the selected data into a variable
    READ TABLE gt_shortage INDEX row INTO DATA(ls_shortage).
    IF sy-subrc = 0.
      SET PARAMETER ID 'MAT' FIELD ls_shortage-matnr.
      SET PARAMETER ID 'WRK' FIELD ls_shortage-werks.
      CALL TRANSACTION 'MD04' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.