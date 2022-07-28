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
      set_alv_header,
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
    set_alv_header( ).

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

  METHOD set_alv_header.
    DATA(lr_lay_grid) = NEW cl_salv_form_layout_grid( ).
    DATA(lr_label)    = lr_lay_grid->create_label( row = 1 column = 1 ).

    lr_label->set_text( sy-title ).
    r_table->set_top_of_list( lr_lay_grid ).
    r_table->set_top_of_list_print( lr_lay_grid ).

    lr_label = lr_lay_grid->create_label( row = 2 column = 1 ).
*    lr_label->set_text( |{ sy-title } 2| ).
    r_table->set_top_of_list( lr_lay_grid ).
    r_table->set_top_of_list_print( lr_lay_grid ).


    lr_label = lr_lay_grid->create_label( row = 2 column = 3 ).
    WRITE sy-datum TO sy-msgv1.
    lr_label->set_text( |{ sy-msgv1 }| ).
    r_table->set_top_of_list( lr_lay_grid ).
    r_table->set_top_of_list_print( lr_lay_grid ).
  ENDMETHOD.

  METHOD send_alv_in_email_attach.
    DATA: lv_content TYPE string,
          lv_address TYPE bcs_address,
          lv_subject TYPE bcs_subject,
          v_xstring  TYPE xstring.

    lv_subject = 'This is a subject'.
    lv_content = 'This is a <b>e-mail body</b> content!'.
    lv_address = 'julio.nascimento@numenit.com'.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_content
      IMPORTING
        buffer = v_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    TRY .
        DATA(msg) = NEW cl_bcs_message( ).

        msg->set_subject( lv_subject ).

        msg->set_main_doc(
          EXPORTING
            iv_contents_bin = v_xstring
            iv_doctype      = 'HTM'
            iv_codepage     = 4110 ). "UTF-8"

        msg->add_recipient( lv_address ).

        v_xstring = r_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

        msg->add_attachment(
          EXPORTING
            iv_doctype      = 'EXT'
            iv_filename     = 'Report.xlsx'
            iv_contents_bin = v_xstring ).

        msg->set_send_immediately( abap_true ).

        msg->send( ).
        MESSAGE 'ALV sent by e-mail with attachment. Check SOST.' TYPE 'S'.
      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_click.
*    DATA(lv_data) = |Clicked row { row } Clicked column { column }|.

    DATA(s_outtab) = t_outtab[ row ].

    SELECT * FROM sflight INTO TABLE @DATA(lt_sflight)
      WHERE carrid EQ @s_outtab-carrid.

    TRY .
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lr_table)
          CHANGING
            t_table      = lt_sflight ).

        lr_table->get_columns( )->get_column( 'MANDT' )->set_visible( abap_false ).
        lr_table->get_aggregations( )->add_aggregation( columnname = 'PAYMENTSUM' ).
        lr_table->get_aggregations( )->add_aggregation( columnname = 'PRICE' ).

        lr_table->get_sorts( )->add_sort( columnname = 'PLANETYPE'
                                          position   = 1
                                          subtotal   = abap_true
                                          sequence   = if_salv_c_sort=>sort_up ).

        lr_table->get_sorts( )->add_sort( columnname = 'FLDATE'
                                          position   = 2
*                                          subtotal   = abap_true
                                          sequence   = if_salv_c_sort=>sort_up ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    lr_table->get_functions( )->set_all( ).
    lr_table->get_columns( )->set_optimize( abap_true ).

    SELECT SINGLE carrname INTO @DATA(lv_carrname)
      FROM scarr
     WHERE carrid EQ @s_outtab-carrid.

    TRANSLATE lv_carrname TO UPPER CASE.

    DATA(lv_title) = |Vôos da companhia aérea { lv_carrname }|.

    lr_table->get_display_settings( )->set_list_header( CONV lvc_title( lv_title ) ).

    DATA(lr_lay_grid) = NEW cl_salv_form_layout_grid( ).
    DATA(lr_label)    = lr_lay_grid->create_label( row = 1 column = 1 ).

    lr_label->set_text( lv_title ).
    lr_table->set_top_of_list( lr_lay_grid ).
    lr_table->set_top_of_list_print( lr_lay_grid ).

    lr_label = lr_lay_grid->create_label( row = 2 column = 1 ).
    WRITE sy-datum TO sy-msgv1.
    lr_label->set_text( |{ sy-msgv1 }| ).
    lr_table->set_top_of_list( lr_lay_grid ).
    lr_table->set_top_of_list_print( lr_lay_grid ).

    lr_table->display( ).

  ENDMETHOD.

  METHOD added_function.
    MESSAGE e_salv_function TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(lcl_report) = NEW lcl_report( ).

START-OF-SELECTION.
  lcl_report=>send_alv_in_email_attach( ).
  lcl_report=>show_alv( ).
