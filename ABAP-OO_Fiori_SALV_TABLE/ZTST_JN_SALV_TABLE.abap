*&---------------------------------------------------------------------*
*& Report ZTST_JN_SALV_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztst_jn_salv_table.

*--------------------------- INCLUDES ---------------------------------*
INCLUDE ztst_jn_salv_table_scr.
INCLUDE ztst_jn_salv_table_cls.
INCLUDE ztst_jn_salv_table_top.
INCLUDE ztst_jn_salv_table_f01.

*---------------------- START-OF-SELECTION ----------------------------*
START-OF-SELECTION.
  "Initiate main data
  SELECT * INTO TABLE @DATA(t_tab)
    FROM spfli
*    FROM sflight
   WHERE carrid   IN @so_carid
     AND connid   IN @so_conid
     AND airpfrom IN @so_afrom
     AND airpto   IN @so_ato.

  IF sy-subrc NE 0.
    MESSAGE 'No data found.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  alv = NEW lcl_alv( t_main_tab = t_tab
                     pfstatus   = 'SALV_TABLE_STANDARD' ).

  alv->set_event_handlers( NEW lcl_handle_events( ) ).
  alv->set_alv_header( sy-title ).

  DATA(t_column_visibilities) =
    VALUE alv->tty_column_visibilities(
        ( columnname = 'MANDT' visibility = abap_false )
       ).

  alv->change_column_visibilities( t_column_visibilities ).

  DATA(t_aggrs) =
    VALUE alv->tty_aggregations(
        ( columnname = 'DISTANCE' )
        ( columnname = 'FLTIME' )
        ( columnname = 'PRICE' )
       ).

  alv->add_aggregations( t_aggrs ).

  DATA(t_sorts) =
    VALUE alv->tty_sorts(
        ( columnname = 'CARRID'    position = 1 subtotal = abap_false sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'CONNID'    position = 2 subtotal = abap_false sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'DISTID'    position = 1 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'COUNTRYFR' position = 2 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'CURRENCY'  position = 3 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )
       ).

  alv->add_sorts( t_sorts ).

*----------------------- END-OF-SELECTION ------------------------------*
END-OF-SELECTION.
  alv->show_alv( ).
