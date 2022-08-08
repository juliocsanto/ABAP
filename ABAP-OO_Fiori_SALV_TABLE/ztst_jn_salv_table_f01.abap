*&---------------------------------------------------------------------*
*& Include          ZTST_JN_SALV_TABLE_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_SALV_FUNCTION TYPE salv_de_function
*&---------------------------------------------------------------------*
FORM handle_user_command  USING i_ucomm TYPE salv_de_function.
  CASE i_ucomm.
    WHEN '&EMAIL'.
      CHECK alv IS BOUND.

      DATA(lv_subject) = 'ALV sent in e-mail attachment'.
      DATA(lv_content) = 'This is a <b>e-mail body</b> content! See XSLX attachment!'.

      WRITE sy-datum TO sy-msgv1.
      REPLACE ALL OCCURRENCES OF '.' IN sy-msgv1 WITH '-'.
      DATA(lv_attach_name) = |ALV_{ sy-msgv1 }|.

      DATA(t_addresses) =
        VALUE alv->tty_address(
            ( address = 'julio.nascimento@numenit.com' )
           ).

      alv->send_alv_in_email_attach(
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
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_double_click
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ROW
*&      --> COLUMN
*&---------------------------------------------------------------------*
FORM handle_double_click  USING i_row    TYPE i
                                i_column TYPE lvc_fname.
*    DATA(lv_data) = |Clicked row { row } Clicked column { column }|.
  TYPES: BEGIN OF ty_totals,
           connid   TYPE sflight-connid,
           year     TYPE char4,
           price    TYPE sflight-price,
           currency TYPE sflight-currency,
         END OF ty_totals,
         tty_totals TYPE STANDARD TABLE OF ty_totals WITH KEY connid year.

  DATA: lt_sflight TYPE STANDARD TABLE OF sflight
                        WITH NON-UNIQUE SORTED KEY key_connid
                             COMPONENTS carrid connid fldate.

  DATA: ls_spfli TYPE spfli.

  CHECK NOT i_row IS INITIAL AND alv IS BOUND.

  alv->get_line(
      EXPORTING
        row = i_row
      CHANGING
        line = ls_spfli ).

  SELECT * FROM sflight INTO TABLE lt_sflight
    WHERE carrid EQ ls_spfli-carrid.

  DATA(lt_sflight_uniques) = lt_sflight.

  SORT lt_sflight_uniques BY connid.
  DELETE ADJACENT DUPLICATES FROM lt_sflight_uniques COMPARING connid currency fldate(4).

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
                          NEXT ls_tot-price = ls_tot-price + sflight_filtered-price
                              )
                        )
                      ).

  SELECT SINGLE carrname INTO @DATA(lv_carrname)
    FROM scarr
   WHERE carrid EQ @ls_spfli-carrid.

  TRANSLATE lv_carrname TO UPPER CASE.

  DATA(lv_title) = CONV lvc_title( |Preço Anual por Vôos - Companhia Aérea: { lv_carrname }| ).

  DATA(t_aggrs) =
    VALUE lcl_alv=>tty_aggregations(
        ( columnname = 'PRICE' )
       ).

  DATA(t_sorts) =
    VALUE lcl_alv=>tty_sorts(
        ( columnname = 'YEAR' position = 1 subtotal = abap_true sequence = if_salv_c_sort=>sort_up )
       ).

  DATA(t_column_texts) =
    VALUE lcl_alv=>tty_column_texts(
        ( columnname = 'YEAR' text = 'Ano' )
       ).

  DATA(sec_alv) = NEW lcl_alv( t_main_tab     = lt_total
                               t_aggregations = t_aggrs
                               t_sorts        = t_sorts
                               t_column_texts = t_column_texts
                               alv_title      = lv_title ).

  sec_alv->set_screen_popup( ).

  sec_alv->show_alv( ).

ENDFORM.
