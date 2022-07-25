*&---------------------------------------------------------------------*
*& Report ZTST_SALV_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztst_salv_table.

CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor .
    CLASS-METHODS:
      main,
      show_alv,
      send_alv_in_email_attach,
      handle_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,
      added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

    CLASS-DATA: r_table  TYPE REF TO cl_salv_table,
                t_outtab TYPE STANDARD TABLE OF spfli,
                r_event  TYPE REF TO cl_salv_events_table.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD constructor.
    "Initiate main data
    SELECT * FROM spfli UP TO 100 ROWS
      INTO CORRESPONDING FIELDS OF TABLE t_outtab.

    TRY .
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = r_table
          CHANGING
            t_table      = t_outtab ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    main( ).

  ENDMETHOD.

  METHOD main.

    TRY .
        r_table->get_functions( )->set_all( abap_true ).
        r_table->get_columns( )->set_optimize( abap_true ).
        r_table->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

        r_event ?= r_table->get_event( ).
        SET HANDLER lcl_report=>handle_click   FOR r_event.
        SET HANDLER lcl_report=>added_function FOR r_event.

        r_table->get_columns( )->get_column( 'MANDT' )->set_visible( abap_false ).

        DATA(lr_aggr) = r_table->get_aggregations( ).
        lr_aggr->add_aggregation( columnname = 'DISTANCE' ).
        lr_aggr->add_aggregation( columnname = 'FLTIME' ).

        DATA(lr_sorts) = r_table->get_sorts( ).

        lr_sorts->add_sort( columnname = 'DISTID'
                            position   = 1
                            subtotal   = abap_true
                            sequence   = if_salv_c_sort=>sort_up ).

        lr_sorts->add_sort( columnname = 'COUNTRYFR'
                            position   = 2
                            subtotal   = abap_true
                            sequence   = if_salv_c_sort=>sort_up ).

        lr_sorts->add_sort( columnname = 'CARRID'
                            position   = 3
*                            subtotal   = abap_true
                            sequence   = if_salv_c_sort=>sort_up ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD show_alv.
    r_table->display( ).
  ENDMETHOD.

  METHOD send_alv_in_email_attach.
    TRY .
        DATA(msg) = NEW cl_bcs_message( ).
        msg->set_subject( 'This is a subject' ).
        msg->set_main_doc( 'This is a <b>body</b>' ).
        msg->add_recipient( 'julio.nascimento@numenit.com' ).

        DATA(v_xstring) = r_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

        msg->add_attachment(
          EXPORTING
            iv_doctype      = 'EXT'
            iv_filename     = 'Report.xlsx'
            iv_contents_bin = v_xstring ).

        msg->send( ).
        MESSAGE 'ALV sent by e-mail. Check SOST.' TYPE 'S'.
      CATCH cx_root.
        MESSAGE 'Error at sending e-mail.' TYPE 'E' DISPLAY LIKE 'S'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_click.
    DATA(lv_data) = |Clicked row { row } Clicked column { column }|.

    DATA(s_outtab) = t_outtab[ row ].
    lv_data = |City from for flight = { s_outtab-cityfrom }|.

    MESSAGE lv_data TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.

  METHOD added_function.
    MESSAGE e_salv_function TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(lcl_report) = NEW lcl_report( ).

START-OF-SELECTION.
*  lcl_report=>send_alv_in_email_attach( ).
  lcl_report=>show_alv( ).
