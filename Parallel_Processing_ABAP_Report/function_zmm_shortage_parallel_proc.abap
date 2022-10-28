FUNCTION zmm_shortage_parallel_proc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WERKS) TYPE  WERKS_D
*"  TABLES
*"      T_MARA STRUCTURE  MARA
*"      T_SHORTAGE STRUCTURE  ZMM_SHORTAGE
*"      T_SUPPLIER_DETAILS STRUCTURE  ZMMS_SHORTAGE_SUPP_DETAILS
*"----------------------------------------------------------------------
  TYPES: tty_bapi_mrp_ind_lines TYPE STANDARD TABLE OF bapi_mrp_ind_lines.

  DATA: lt_mrp_ind_lines     TYPE TABLE OF bapi_mrp_ind_lines,
        lt_mrp_items         TYPE TABLE OF bapi_mrp_items,
        lt_mrp_total_lines   TYPE TABLE OF bapi_mrp_total_lines,
        ls_mrp_total_lines   TYPE bapi_mrp_total_lines,
        ls_mrp_list          TYPE bapi_mrp_list,
        ls_mrp_control_param TYPE bapi_mrp_control_param,
        ls_mrp_stock_detail  TYPE bapi_mrp_stock_detail,
        ls_return            TYPE bapiret2.

  DATA: lv_stock TYPE vrfmg.

  LOOP AT t_mara INTO DATA(ls_matnr).
    REFRESH lt_mrp_ind_lines.
    CALL FUNCTION 'BAPI_MATERIAL_STOCK_REQ_LIST'
      EXPORTING
*       MATERIAL          = lv_matnr
        plant             = werks
        get_ind_lines     = 'X'
        material_long     = ls_matnr-matnr
      IMPORTING
        mrp_list          = ls_mrp_list
        mrp_control_param = ls_mrp_control_param
        mrp_stock_detail  = ls_mrp_stock_detail
        return            = ls_return
      TABLES
        mrp_items         = lt_mrp_items
        mrp_ind_lines     = lt_mrp_ind_lines
        mrp_total_lines   = lt_mrp_total_lines.

    IF lt_mrp_ind_lines[] IS NOT INITIAL.      "If requirement exists
      DELETE lt_mrp_ind_lines WHERE mrp_element_ind = 'SH'    "safety stock
                                 OR mrp_element_ind = 'BA'    "purchase req
                                 OR mrp_element_ind = 'SB'    "dependent requirement
                                 OR mrp_element_ind = 'U2'.   "STO purchase req

      "recalculation of the availability
      CLEAR lv_stock.
*      lv_stock = REDUCE vrfmg(
*                    INIT val TYPE vrfmg
*                         index = 0
*                    FOR ls_ind_lines IN lt_mrp_ind_lines
*                      NEXT val = COND vrfmg(
*                                   WHEN index EQ 1
*                                    THEN ls_ind_lines-avail_qty1
*                                   ELSE val + ls_ind_lines-rec_reqd_qty
*                                 )
*                           index = index + 1
*                 ).

      LOOP AT lt_mrp_ind_lines ASSIGNING FIELD-SYMBOL(<mrp_lines>).
        IF sy-tabix = 1.       "initial stock count with stock value
          lv_stock = <mrp_lines>-avail_qty1.
        ELSE.
          <mrp_lines>-avail_qty1 = lv_stock + <mrp_lines>-rec_reqd_qty.
          lv_stock = <mrp_lines>-avail_qty1.
        ENDIF.
      ENDLOOP.

      IF lv_stock LT 0.    "append table only if stock will be negative
        " delete stock line since we don't need it anymore
        DELETE lt_mrp_ind_lines WHERE mrp_element_ind = 'WB'.  "stock line

        CHECK lt_mrp_ind_lines[] IS NOT INITIAL.
        SORT lt_mrp_ind_lines BY avail_date ASCENDING.   "sort table to get the soonest shortage

        LOOP AT lt_mrp_ind_lines INTO DATA(ls_mrp_ind_lines).
          DATA(wa_mrp_lines) = ls_mrp_ind_lines.

          AT END OF avail_date.
            "available stock + safety stock + physical stock
            IF wa_mrp_lines-avail_qty1 LT 0.       "means shortage
              APPEND INITIAL LINE TO t_shortage ASSIGNING FIELD-SYMBOL(<fs_shortage>).
              <fs_shortage>-werks      = werks.
              <fs_shortage>-matnr      = ls_matnr-matnr.
              <fs_shortage>-avail_date = wa_mrp_lines-avail_date.
              <fs_shortage>-avail_qty1 = wa_mrp_lines-avail_qty1.

              <fs_shortage>-lifnr = VALUE #( t_supplier_details[ matnr = ls_matnr-matnr
                                                                 werks = werks ]-lifnr OPTIONAL ) .

              <fs_shortage>-name1 = VALUE #( t_supplier_details[ matnr = ls_matnr-matnr
                                                                 werks = werks ]-name1 OPTIONAL ) .

              EXIT.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDAT.
        ENDLOOP.
      ENDIF.      "lv_stock < 0
    ENDIF.

  ENDLOOP.
ENDFUNCTION.