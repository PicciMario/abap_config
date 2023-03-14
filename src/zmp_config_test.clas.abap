CLASS zmp_config_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zmp_config_test IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DELETE FROM zmp_param.

    INSERT zmp_param FROM TABLE @( VALUE #(
        ( function = 'CIAO' config_key = 'key1' config_value = 'prima chiave' )
        ( function = 'CIAO' config_key = 'key2' config_value = 'seconda chiave' )
    ) ).

    DATA: lo_config TYPE REF TO zmp_config.
    lo_config = zmp_config=>create( iv_function = 'CIAO' ).

    lo_config->get_key(
        EXPORTING iv_key = 'key1'
        IMPORTING ov_value = DATA(lv_key)
    ).

    out->write( lv_key ).
    out->write( lo_config->gt_param ).

  ENDMETHOD.

ENDCLASS.
