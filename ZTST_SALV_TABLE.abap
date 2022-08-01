*&---------------------------------------------------------------------*
*& Report ZTST_SALV_TABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztst_salv_table.

TABLES: spfli.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_carid FOR spfli-carrid,
                  so_conid FOR spfli-connid,
                  so_afrom FOR spfli-airpfrom,
                  so_ato   FOR spfli-airpto.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_alv DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_field_visibilities,
             field_name TYPE dd03t-fieldname,
             visibility TYPE abap_bool,
           END OF ty_field_visibilities,
           tty_field_visibilities TYPE STANDARD TABLE OF ty_field_visibilities
                                  WITH KEY field_name,

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
           tty_address TYPE STANDARD TABLE OF ty_address WITH KEY address.

    METHODS:
      constructor               IMPORTING t_main_tab           TYPE ANY TABLE,
      set_alv_header            IMPORTING title                TYPE syst_title,
      change_field_visibilities IMPORTING t_field_visibilities TYPE tty_field_visibilities,
      add_aggregations          IMPORTING t_aggregations       TYPE tty_aggregations,
      add_sorts                 IMPORTING t_sorts              TYPE tty_sorts,
      send_alv_in_email_attach  IMPORTING subject      TYPE bcs_subject
                                          body_content TYPE string
                                          t_address    TYPE tty_address
                                          attach_name  TYPE bcs_filename
                                EXPORTING success      TYPE abap_bool,
      show_alv.

  PRIVATE SECTION.
    METHODS:
      main,
      handle_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column,
      added_function FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function.

    DATA: r_table       TYPE REF TO cl_salv_table,
          gr_data_table TYPE REF TO data,
          r_event       TYPE REF TO cl_salv_events_table.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD constructor.
    SET COUNTRY 'BR'.

    TRY .
        CREATE DATA gr_data_table LIKE STANDARD TABLE OF t_main_tab.
        ASSIGN gr_data_table->* TO FIELD-SYMBOL(<fs_table>).

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

      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD main.
    TRY .
        r_table->get_functions( )->set_all( abap_true ).
        r_table->get_columns( )->set_optimize( abap_true ).
        r_table->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).

        r_event ?= r_table->get_event( ).
        SET HANDLER handle_click   FOR r_event.
        SET HANDLER added_function FOR r_event.

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
          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
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
          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD change_field_visibilities.
    CHECK NOT t_field_visibilities IS INITIAL.

    DATA(lr_columns) = r_table->get_columns( ).

    LOOP AT t_field_visibilities INTO DATA(s_fd_visibility).
      TRANSLATE s_fd_visibility-field_name TO UPPER CASE.
      TRY .
          lr_columns->get_column( s_fd_visibility-field_name )->set_visible( s_fd_visibility-visibility ).
        CATCH cx_root INTO DATA(lr_xroot).
          MESSAGE lr_xroot->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDLOOP.
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
*        MESSAGE 'ALV sent by e-mail with attachment. Check SOST.' TYPE 'S'.
      CATCH cx_root INTO DATA(lcx).
        MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

  METHOD handle_click.
*    DATA(lv_data) = |Clicked row { row } Clicked column { column }|.
    TYPES: BEGIN OF ty_totals,
             connid   TYPE sflight-connid,
             year     TYPE char4,
             price    TYPE sflight-price,
             currency TYPE sflight-currency,
           END OF ty_totals,
           tty_totals TYPE STANDARD TABLE OF ty_totals WITH KEY connid year,
           tty_spfli  TYPE STANDARD TABLE OF spfli.

    DATA: lt_sflight TYPE STANDARD TABLE OF sflight
                          WITH NON-UNIQUE SORTED KEY key_connid
                               COMPONENTS carrid connid fldate.

    FIELD-SYMBOLS: <fs_table> TYPE tty_spfli.

    IF row IS INITIAL.
      MESSAGE 'Click on a valid line.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    ASSIGN gr_data_table->* TO <fs_table>.
    DATA(s_outtab) = <fs_table>[ row ].

    SELECT * FROM sflight INTO TABLE lt_sflight
      WHERE carrid EQ s_outtab-carrid.

    DATA(lt_sflight_uniques) = lt_sflight.

    SORT lt_sflight_uniques BY connid.
    DELETE ADJACENT DUPLICATES FROM lt_sflight_uniques COMPARING connid.

    DATA(lt_total) = VALUE tty_totals(
                        FOR ls_sflight IN lt_sflight_uniques
                         ( REDUCE ty_totals(
                            INIT ls_tot = VALUE ty_totals( connid   = ls_sflight-connid
                                                           year     = ls_sflight-fldate(4)
                                                           currency = ls_sflight-currency )
                            FOR sflight_filtered IN
                              FILTER #( lt_sflight
                                        USING KEY key_connid
                                        WHERE connid EQ ls_sflight-connid
                                          AND carrid EQ ls_sflight-carrid
                                          AND fldate EQ ls_sflight-fldate )
                            NEXT ls_tot-price = ls_tot-price + ls_sflight-price
                                           )
                         )
                       ).

    TRY .
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lr_table)
          CHANGING
            t_table      = lt_total ).

        lr_table->get_aggregations( )->add_aggregation( columnname = 'PRICE' ).

        lr_table->get_columns( )->get_column( 'YEAR' )->set_medium_text( CONV #('Ano') ).
        lr_table->get_columns( )->get_column( 'YEAR' )->set_long_text( CONV #('Ano') ).
        lr_table->get_columns( )->get_column( 'YEAR' )->set_short_text( CONV #('Ano') ).

        lr_table->get_sorts( )->add_sort( columnname = 'YEAR'
                                          position   = 1
                                          subtotal   = abap_true
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

    DATA(lv_title) = |Preço Anual por Vôos - Companhia Aérea: { lv_carrname }|.

    lr_table->get_display_settings( )->set_list_header( CONV lvc_title( lv_title ) ).

    DATA(lr_lay_grid) = NEW cl_salv_form_layout_grid( ).
    DATA(lr_label)    = lr_lay_grid->create_label( row = 1 column = 1 ).

    lr_label->set_text( lv_title ).
    lr_table->set_top_of_list( lr_lay_grid ).
    lr_table->set_top_of_list_print( lr_lay_grid ).

    lr_label = lr_lay_grid->create_label( row = 2 column = 1 ).

    WRITE sy-datum TO sy-msgv1.
    lr_label->set_text( |Data do Relatório: { sy-msgv1 }| ).
    lr_table->set_top_of_list( lr_lay_grid ).
    lr_table->set_top_of_list_print( lr_lay_grid ).

    lr_table->set_screen_popup( start_column = 40
                                end_column   = 100
                                start_line   = 5
                                end_line     = 25 ).
    lr_table->display( ).

  ENDMETHOD.

  METHOD added_function.
    MESSAGE e_salv_function TYPE 'I' DISPLAY LIKE 'S'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  "Initiate main data
  SELECT * INTO TABLE @DATA(t_tab)
  FROM spfli
*  FROM sflight
 WHERE carrid   IN @so_carid
   AND connid   IN @so_conid
   AND airpfrom IN @so_afrom
   AND airpto   IN @so_ato.

  IF sy-subrc NE 0.
    MESSAGE 'No data found.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(lcl) = NEW lcl_alv( t_tab ).
  lcl->set_alv_header( sy-title ).

  DATA(t_field_visibilities) =
    VALUE lcl->tty_field_visibilities(
        ( field_name = 'MANDT' visibility = abap_false )
       ).

  lcl->change_field_visibilities( t_field_visibilities ).

  DATA(t_aggrs) =
    VALUE lcl->tty_aggregations(
        ( columnname = 'DISTANCE' )
        ( columnname = 'FLTIME' )
        ( columnname = 'PRICE' )
       ).

  lcl->add_aggregations( t_aggrs ).

  DATA(t_sorts) =
    VALUE lcl->tty_sorts(
        ( columnname = 'CARRID'    position = 1 subtotal = abap_false sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'CONNID'    position = 2 subtotal = abap_false sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'DISTID'    position = 1 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'COUNTRYFR' position = 2 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )
        ( columnname = 'CURRENCY'  position = 3 subtotal = abap_true  sequence = if_salv_c_sort=>sort_up )

      ).

  lcl->add_sorts( t_sorts ).

END-OF-SELECTION.

  DATA(lv_subject) = 'ALV sent in e-mail attachment'.
  DATA(lv_content) = 'This is a <b>e-mail body</b> content! See XSLX attachment!'.

  WRITE sy-datum TO sy-msgv1.
  REPLACE ALL OCCURRENCES OF '.' IN sy-msgv1 WITH '-'.
  DATA(lv_attach_name) = |ALV_{ sy-msgv1 }|.

  DATA(t_addresses) =
    VALUE lcl->tty_address(
        ( address = 'julio.nascimento@numenit.com' )
       ).

  lcl->send_alv_in_email_attach(
                  EXPORTING
                    attach_name  = lv_attach_name
                    body_content = CONV #( lv_content )
                    subject      = CONV #( lv_subject )
                    t_address    = t_addresses
                  IMPORTING
                    success      = DATA(is_ok) ).

  IF NOT is_ok IS INITIAL.
    MESSAGE 'E-mail sent successfully. Check SOST!' TYPE 'S'.
  ENDIF.

  lcl->show_alv( ).
