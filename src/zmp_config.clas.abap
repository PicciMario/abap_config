CLASS zmp_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    METHODS:
      get_key
        IMPORTING
          iv_key   TYPE zmp_param-config_key
        EXPORTING
          ov_value TYPE zmp_param-config_value.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CLASS-METHODS:
      create
        RETURNING VALUE(lo_instance) TYPE REF TO zmp_config.

ENDCLASS.



CLASS zmp_config IMPLEMENTATION.


  METHOD create.

    DELETE FROM zmp_param.

    INSERT zmp_param FROM TABLE @( VALUE #(
        ( function = 'CIAO' config_key = 'key1' config_value = 'prima chiave' )
        ( function = 'CIAO' config_key = 'key2' config_value = 'seconda chiave' )
    ) ).

    lo_instance = NEW zmp_config(  ).

  ENDMETHOD.


  METHOD get_key.
    ov_value = |ottimismo!|.
  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(lo_config) = zmp_config=>create( ).
    lo_config->get_key(
        EXPORTING iv_key = 'key1'
        IMPORTING ov_value = DATA(lo_key)
    ).
    out->write( lo_key ).

  ENDMETHOD.


ENDCLASS.
