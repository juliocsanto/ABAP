*&---------------------------------------------------------------------*
*& Include          ZTST_JN_SALV_TABLE_CLS
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       INTERFACE lcl_if_handle_events
*---------------------------------------------------------------------*
INTERFACE lcl_if_handle_events.
  METHODS:
    on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function,

    on_double_click FOR EVENT double_click   OF cl_salv_events_table
      IMPORTING row column,

    on_link_click   FOR EVENT link_click     OF cl_salv_events_table
      IMPORTING row column.
ENDINTERFACE.

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES: lcl_if_handle_events.
ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION .
  METHOD lcl_if_handle_events~on_user_command.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.                    "on_user_command

  METHOD lcl_if_handle_events~on_double_click.
    PERFORM handle_double_click USING row column.
  ENDMETHOD.                    "on_double_click

  METHOD lcl_if_handle_events~on_link_click.
*    PERFORM show_cell_info USING 0 row column TEXT-i06.
  ENDMETHOD.                    "on_single_click
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION

CLASS lcl_alv DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_column_visibilities,
             columnname TYPE dd03t-fieldname,
             visibility TYPE abap_bool,
           END OF ty_column_visibilities,
           tty_column_visibilities TYPE STANDARD TABLE OF ty_column_visibilities
               WITH KEY columnname,

           BEGIN OF ty_aggregations,
             columnname TYPE dd03t-fieldname,
           END OF ty_aggregations,
           tty_aggregations TYPE STANDARD TABLE OF ty_aggregations
                              WITH KEY columnname,

           BEGIN OF ty_sorts,
             columnname TYPE dd03t-fieldname,
             position   TYPE i,
             subtotal   TYPE abap_bool,
             sequence   TYPE salv_de_sort_sequence,
           END OF ty_sorts,
           tty_sorts TYPE STANDARD TABLE OF ty_sorts
             WITH KEY columnname,

           BEGIN OF ty_address,
             address TYPE bcs_address,
           END OF ty_address,
           tty_address TYPE STANDARD TABLE OF ty_address WITH KEY address,

           BEGIN OF ty_column_texts,
             columnname TYPE dd03t-fieldname,
             text       TYPE dd03t-ddtext,
           END OF ty_column_texts,
           tty_column_texts TYPE STANDARD TABLE OF ty_column_texts
                              WITH KEY columnname.

    METHODS:
      constructor                IMPORTING t_main_tab            TYPE ANY TABLE
                                           alv_title             TYPE lvc_title                   OPTIONAL
                                           t_column_visibilities TYPE tty_column_visibilities     OPTIONAL
                                           t_aggregations        TYPE tty_aggregations            OPTIONAL
                                           t_sorts               TYPE tty_sorts                   OPTIONAL
                                           t_column_texts        TYPE tty_column_texts            OPTIONAL
                                           r_events              TYPE REF TO lcl_if_handle_events OPTIONAL
                                           pfstatus              TYPE sypfkey                     OPTIONAL,

      set_alv_header             IMPORTING title                 TYPE lvc_title,
      change_column_visibilities IMPORTING t_column_visibilities TYPE tty_column_visibilities,
      add_aggregations           IMPORTING t_aggregations        TYPE tty_aggregations,
      add_sorts                  IMPORTING t_sorts               TYPE tty_sorts,
      change_column_texts        IMPORTING t_column_texts        TYPE tty_column_texts,

      send_alv_in_email_attach   IMPORTING subject      TYPE bcs_subject
                                           body_content TYPE string
                                           t_address    TYPE tty_address
                                           attach_name  TYPE bcs_filename
                                 EXPORTING success      TYPE abap_bool,

      set_screen_popup           IMPORTING start_column TYPE i DEFAULT 40
                                           end_column   TYPE i DEFAULT 100
                                           start_line   TYPE i DEFAULT 5
                                           end_line     TYPE i DEFAULT 25,

      show_alv,
      set_event_handlers         IMPORTING r_events              TYPE REF TO lcl_if_handle_events,
      set_own_status             IMPORTING pfstatus              TYPE sypfkey,
      get_line                   IMPORTING row  TYPE i
                                 CHANGING  line TYPE any.

  PRIVATE SECTION.
    METHODS:
      main.

    DATA: r_table      TYPE REF TO cl_salv_table,
          r_data_table TYPE REF TO data.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD constructor.
    SET COUNTRY 'BR'.

    TRY .
        CREATE DATA r_data_table LIKE t_main_tab.
        ASSIGN r_data_table->* TO FIELD-SYMBOL(<fs_table>).

        <fs_table> = t_main_tab.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = r_table
          CHANGING
            t_table      = <fs_table> ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    main( ).

    IF r_events IS BOUND.
      set_event_handlers( r_events ).
    ENDIF.

    IF NOT pfstatus IS INITIAL.
      set_own_status( pfstatus ).
    ENDIF.

    IF NOT alv_title IS INITIAL.
      set_alv_header( alv_title ).
    ENDIF.

    IF NOT t_aggregations IS INITIAL.
      add_aggregations( t_aggregations ).
    ENDIF.

    IF NOT t_sorts IS INITIAL.
      add_sorts( t_sorts ).
    ENDIF.

    IF NOT t_column_visibilities IS INITIAL.
      change_column_visibilities( t_column_visibilities ).
    ENDIF.

    IF NOT t_column_texts IS INITIAL.
      change_column_texts( t_column_texts ).
    ENDIF.
  ENDMETHOD.

  METHOD set_own_status.
    CHECK r_table IS BOUND.
    TRY .
        r_table->set_screen_status(
            pfstatus      = pfstatus
            report        = sy-repid
            set_functions = r_table->c_functions_all ).

        r_table->get_functions( )->set_all( abap_true ).
      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD main.
    TRY .
        r_table->get_functions( )->set_all( abap_true ).
        r_table->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).
        r_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        r_table->get_columns( )->set_optimize( abap_true ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_line.
    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

    CHECK r_table IS BOUND.

    ASSIGN r_data_table->* TO <fs_table>.
    READ TABLE <fs_table> ASSIGNING FIELD-SYMBOL(<fs_wa>) INDEX row.

    CHECK sy-subrc IS INITIAL.
    MOVE-CORRESPONDING <fs_wa> TO line.
  ENDMETHOD.

  METHOD set_event_handlers.
    CHECK r_table IS BOUND.

    DATA(lr_events) = r_table->get_event( ).
    SET HANDLER r_events->on_double_click FOR lr_events.
    SET HANDLER r_events->on_user_command FOR lr_events.
    SET HANDLER r_events->on_link_click   FOR lr_events.
  ENDMETHOD.

  METHOD set_alv_header.
    TRY .
        DATA(lr_lay_grid) = NEW cl_salv_form_layout_grid( ).
        DATA(lr_label)    = lr_lay_grid->create_label( row = 1 column = 1 ).

        lr_label->set_text( title ).
        r_table->set_top_of_list( lr_lay_grid ).
        r_table->set_top_of_list_print( lr_lay_grid ).

        lr_label = lr_lay_grid->create_label( row = 2 column = 1 ).
        WRITE sy-datum TO sy-msgv1.
        lr_label->set_text( |Data do Relatório: { sy-msgv1 }| ).
        r_table->set_top_of_list( lr_lay_grid ).
        r_table->set_top_of_list_print( lr_lay_grid ).

        r_table->get_display_settings( )->set_list_header( title ).

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD add_sorts.
    CHECK NOT t_sorts IS INITIAL.
    DATA(lr_sorts) = r_table->get_sorts( ).

    LOOP AT t_sorts INTO DATA(s_sort).
      TRANSLATE s_sort-columnname TO UPPER CASE.
      TRY .
          lr_sorts->add_sort( columnname = s_sort-columnname
                              position   = s_sort-position
                              subtotal   = s_sort-subtotal
                              sequence   = s_sort-sequence ).
        CATCH cx_root INTO DATA(lr_xroot).
*          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_aggregations.
    CHECK NOT t_aggregations IS INITIAL.

    DATA(lr_aggr) = r_table->get_aggregations( ).

    LOOP AT t_aggregations INTO DATA(s_aggregation).
      TRANSLATE s_aggregation-columnname TO UPPER CASE.
      TRY .
          lr_aggr->add_aggregation( columnname = s_aggregation-columnname ).
        CATCH cx_root INTO DATA(lr_xroot).
*          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD change_column_visibilities.
    CHECK NOT t_column_visibilities IS INITIAL.

    DATA(lr_columns) = r_table->get_columns( ).

    LOOP AT t_column_visibilities INTO DATA(s_column_visibility).
      TRANSLATE s_column_visibility-columnname TO UPPER CASE.
      TRY .
          lr_columns->get_column( s_column_visibility-columnname )->set_visible( s_column_visibility-visibility ).
        CATCH cx_root INTO DATA(lr_xroot).
*          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD change_column_texts.
    CHECK NOT t_column_texts IS INITIAL.

    DATA(lr_columns) = r_table->get_columns( ).

    LOOP AT t_column_texts INTO DATA(s_column_text).
      TRANSLATE s_column_text-columnname TO UPPER CASE.
      TRY .
          lr_columns->get_column( s_column_text-columnname )->set_long_text( CONV #( s_column_text-text ) ).
          lr_columns->get_column( s_column_text-columnname )->set_medium_text( CONV #( s_column_text-text ) ).
          lr_columns->get_column( s_column_text-columnname )->set_short_text( CONV #( s_column_text-text ) ).
        CATCH cx_root INTO DATA(lr_xroot).
*          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_screen_popup.
    TRY .
        r_table->set_screen_popup(
              EXPORTING
                start_column = start_column
                end_column   = end_column
                start_line   = start_line
                end_line     = end_line ).
      CATCH cx_root INTO DATA(lr_xroot).
        MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD show_alv.
    CHECK r_table IS BOUND.
    r_table->display( ).
  ENDMETHOD.

  METHOD send_alv_in_email_attach.
    DATA: v_xstring  TYPE xstring.

    CHECK r_table IS BOUND AND NOT t_address IS INITIAL.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = body_content
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

        msg->set_subject( subject ).

        msg->set_main_doc(
          EXPORTING
            iv_contents_bin = v_xstring
            iv_doctype      = 'HTM'
            iv_codepage     = 4110 ). "UTF-8"

        LOOP AT t_address INTO DATA(s_address).
          msg->add_recipient( s_address-address ).
        ENDLOOP.

        v_xstring = r_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

        msg->add_attachment(
          EXPORTING
            iv_doctype      = 'EXT'
            iv_filename     = |{ attach_name }.xlsx|
            iv_contents_bin = v_xstring ).

        msg->set_send_immediately( abap_true ).

        msg->send( ).
        success = abap_true.

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
