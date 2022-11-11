CLASS zcl_amdp_sample_jn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_result_line,
        airline           TYPE s_carrname,
        flight_connection TYPE s_conn_id,
        old_price         TYPE s_price,
        old_currency      TYPE s_currcode,
        new_price         TYPE s_price,
        new_currency      TYPE s_currcode,
      END OF ty_result_line,

      BEGIN OF ty_flights_line,
        airline           TYPE s_carrname,
        flight_connection TYPE s_conn_id,
        price             TYPE s_price,
        currency          TYPE s_currcode,
      END OF ty_flights_line,

      ty_result_table  TYPE STANDARD TABLE OF ty_result_line WITH EMPTY KEY,
      ty_flights_table TYPE STANDARD TABLE OF ty_flights_line WITH EMPTY KEY,
      ty_flights       TYPE STANDARD TABLE OF sflight.

    INTERFACES: if_amdp_marker_hdb, if_oo_adt_classrun.

    METHODS:
      get_flights
        EXPORTING
                  VALUE(result) TYPE ty_result_table
        RAISING   cx_amdp_execution_error,

      convert_currency
        IMPORTING
                  VALUE(flights) TYPE ty_flights_table
        EXPORTING
                  VALUE(result)  TYPE ty_result_table
        RAISING   cx_amdp_execution_error.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_amdp_sample_jn IMPLEMENTATION.

  METHOD convert_currency BY DATABASE PROCEDURE
                          FOR HDB
                          LANGUAGE SQLSCRIPT
                          OPTIONS READ-ONLY.

    declare today date;
    declare new_currency nvarchar(3);

    select current_date into today from dummy;
    new_currency := 'EUR';

    result = select distinct
                      airline,
                      flight_connection,
                      price    as old_price,
                      currency as old_currency,
                      convert_currency(
                        "AMOUNT"          => price,
                        "SOURCE_UNIT"     => currency,
                        "TARGET_UNIT"     => :new_currency,
                        "REFERENCE_DATE"  => :today,
                        "CLIENT"          => session_context( 'CLIENT' ),
                        "ERROR_HANDLING"  => 'set to null',
                        "SCHEMA"          => current_schema
                      ) as new_price,
                      :new_currency as new_currency
               from :flights;

  ENDMETHOD.

  METHOD get_flights BY DATABASE PROCEDURE
                      FOR HDB
                      LANGUAGE SQLSCRIPT
                      OPTIONS READ-ONLY
                      USING
                        sflight
                        scarr
                        zcl_amdp_sample_jn=>convert_currency.

    flights = select distinct
                      c.carrname as airline,
                      f.connid as flight_connection,
                      f.price    as price,
                      f.currency as currency
                from sflight  as f
                     inner join scarr as c
                             on f.carrid = c.carrid;

    call "ZCL_AMDP_SAMPLE_JN=>CONVERT_CURRENCY"( :flights, result );

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    TRY.
        me->get_flights(
          IMPORTING
            result = DATA(lt_result) ).

      CATCH cx_amdp_execution_error INTO DATA(lx_amdp).
        out->write( lx_amdp->get_longtext( ) ).
    ENDTRY.

    out->write( lt_result ).

  ENDMETHOD.

ENDCLASS.