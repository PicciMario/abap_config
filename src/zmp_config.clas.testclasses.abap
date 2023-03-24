*"* use this source file for your ABAP unit test classes
CLASS ltc_zmp_config DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

    METHODS:
      _01_test FOR TESTING.

  PRIVATE SECTION.

    CLASS-DATA: environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown.

    DATA:
      mo_cut       TYPE REF TO zmp_config,
      lt_zmp_param TYPE STANDARD TABLE OF zmp_param.

ENDCLASS.



CLASS ltc_zmp_config IMPLEMENTATION.


  METHOD class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'ZMP_PARAM' ) ) ).
  ENDMETHOD.


  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.


  METHOD setup.

    lt_zmp_param = VALUE #(
        ( function = 'TEST01' config_key = 'key1'                               config_value = 'key1' )
        ( function = 'TEST01' config_key = 'key1' company = 'AAA'               config_value = 'key1 AAA' )
        ( function = 'TEST01' config_key = 'key1' company = 'AAA' plant = 'P01' config_value = 'key1 AAA P01' )
        ( function = 'TEST01' config_key = 'key1' company = 'AAA' plant = 'P02' config_value = 'key1 AAA P02' )
    ).

    environment->insert_test_data( lt_zmp_param ).

  ENDMETHOD.


  METHOD teardown.
    environment->clear_doubles( ).
  ENDMETHOD.


  METHOD _01_test.

    DATA(lo_config) = zmp_config=>create( iv_function = 'TEST01' ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_config->get_key(
          iv_key = 'key1'
          iv_company = ''
          iv_plant = ''
          iv_default = 'DEFAULT'
        )
        exp = 'key1'
    ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_config->get_key(
          iv_key = 'key1'
          iv_company = 'AAA'
          iv_plant = ''
          iv_default = 'DEFAULT'
        )
        exp = 'key1 AAA'
    ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_config->get_key(
          iv_key = 'key1'
          iv_company = 'AAA'
          iv_plant = 'P01'
          iv_default = 'DEFAULT'
        )
        exp = 'key1 AAA P01'
    ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_config->get_key(
          iv_key = 'key1'
          iv_company = 'AAA'
          iv_plant = 'XXX'
          iv_default = 'DEFAULT'
        )
        exp = 'key1 AAA'
    ).

    cl_abap_unit_assert=>assert_equals(
        act = lo_config->get_key(
          iv_key = 'key1'
          iv_company = 'XXX'
          iv_plant = 'XXX'
          iv_default = 'DEFAULT'
        )
        exp = 'key1'
    ).

  ENDMETHOD.


ENDCLASS.
