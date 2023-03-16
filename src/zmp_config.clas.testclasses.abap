*"* use this source file for your ABAP unit test classes
CLASS ltc_zmp_config DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

    METHODS _01_test FOR TESTING.

  PRIVATE SECTION.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown.

    DATA:
      mo_cut TYPE REF TO zmp_config.

ENDCLASS.



CLASS ltc_zmp_config IMPLEMENTATION.


  METHOD class_setup.

    DELETE FROM zmp_param.

    INSERT zmp_param FROM TABLE @( VALUE #(
        ( function = 'CIAO' config_key = 'key1'                               config_value = 'key1' )
        ( function = 'CIAO' config_key = 'key1' company = 'XXX'               config_value = 'key1 XXX' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA'               config_value = 'key1 AAA' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P01' config_value = 'key1 AAA P01' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P02' config_value = 'key1 AAA P01' )
        ( function = 'CIAO' config_key = 'key2'                               config_value = 'key2' )
    ) ).

  ENDMETHOD.


  METHOD class_teardown.

  ENDMETHOD.


  METHOD setup.

  ENDMETHOD.


  METHOD teardown.

  ENDMETHOD.


  METHOD _01_test.

    DATA(lo_config) = zmp_config=>create( iv_function = 'CIAO' ).

    DATA: lv_key     TYPE zmp_param-config_key VALUE 'key1',
          lv_company TYPE zmp_param-company VALUE 'AAA',
          lv_plant   TYPE zmp_param-plant VALUE ''.

    DATA(lv_result) = lo_config->get_key(
        iv_key = lv_key
        iv_company = lv_company
        iv_plant = lv_plant
        iv_default = 'DEFAULT'
    ).

    cl_abap_unit_assert=>assert_equals( act = lo_config->get_key( iv_key = 'key1' ) exp = 'key1' ).


  ENDMETHOD.


ENDCLASS.
