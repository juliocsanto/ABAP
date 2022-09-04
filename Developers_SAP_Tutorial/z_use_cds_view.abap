*&---------------------------------------------------------------------*
*& Report z_invoice_items_euro_jn
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_use_cds_view.

class lcl_main definition create private.

  public section.
    CLASS-METHODS create
      RETURNING
        value(r_result) TYPE REF TO lcl_main.
    METHODS: run.
  protected section.
  private section.

endclass.

class lcl_main implementation.

  method create.

    r_result = new #( ).

  endmethod.

  method run.
    cl_salv_gui_table_ida=>create_for_cds_view( 'ZDD_INVOICE_ITEMS' )->fullscreen( )->display( ).
  endmethod.

endclass.

START-OF-SELECTION.
    lcl_main=>create( )->run( ).