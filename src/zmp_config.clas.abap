CLASS zmp_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    TYPES: BEGIN OF ty_param,
             function     TYPE zmp_param-function,
             config_key   TYPE zmp_param-config_key,
             company      TYPE zmp_param-company,
             plant        TYPE zmp_param-plant,
             config_value TYPE zmp_param-config_value,
           END OF ty_param.

    " Curioso modo per creare un tipo composito.
    TYPES: BEGIN OF ty_param_eval.
             INCLUDE TYPE ty_param.
    TYPES:   score TYPE i,
           END OF ty_param_eval.

    DATA: gv_function TYPE zmp_param-function,
          gt_param    TYPE TABLE OF ty_param.

    CLASS-METHODS:
      create
        IMPORTING
          iv_function        TYPE zmp_param-function
        RETURNING
          VALUE(lo_instance) TYPE REF TO zmp_config.

    METHODS:
      constructor
        IMPORTING
          iv_func TYPE zmp_param-function OPTIONAL,
      get_key
        IMPORTING
          iv_key          TYPE zmp_param-config_key
          iv_company      TYPE zmp_param-company OPTIONAL
          iv_plant        TYPE zmp_param-plant OPTIONAL
          iv_default      TYPE zmp_param-config_value OPTIONAL
        RETURNING
          VALUE(ov_value) TYPE zmp_param-config_value.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: lv_param_eval TYPE ty_param_eval,
          lt_param_eval TYPE TABLE OF ty_param_eval.

ENDCLASS.



CLASS zmp_config IMPLEMENTATION.


  METHOD create.
    lo_instance = NEW zmp_config( iv_func = iv_function  ).
  ENDMETHOD.


  METHOD constructor.

    gv_function = iv_func.

    SELECT
      function,
      config_key,
      company,
      plant,
      config_value
    FROM zmp_param
    WHERE function = @iv_func
    APPENDING CORRESPONDING FIELDS OF TABLE @gt_param.

  ENDMETHOD.


  METHOD get_key.

    CLEAR lt_param_eval.
    LOOP AT gt_param INTO DATA(lv_param) WHERE config_key = iv_key.

      APPEND INITIAL LINE TO lt_param_eval ASSIGNING FIELD-SYMBOL(<wa_param_eval>).
      MOVE-CORRESPONDING lv_param TO <wa_param_eval>.

      IF ( <wa_param_eval>-company = iv_company ).
        <wa_param_eval>-score += 1.
      ELSE.
        IF ( <wa_param_eval>-company IS NOT INITIAL ).
          <wa_param_eval>-score = 0.
        ENDIF.
        CONTINUE.
      ENDIF.

      IF ( <wa_param_eval>-plant = iv_plant ).
        <wa_param_eval>-score += 1.
      ELSE.
        IF ( <wa_param_eval>-plant IS NOT INITIAL ).
          <wa_param_eval>-score = 0.
        ENDIF.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    ov_value = iv_default.
    IF ( lines( lt_param_eval ) > 0 ).
      SORT lt_param_eval BY score DESCENDING.
      IF ( lt_param_eval[ 1 ]-score > 0 ).
        ov_value = lt_param_eval[ 1 ]-config_value.
      ENDIF.
    ENDIF.

    "debug
    SORT lt_param_eval BY config_key company plant.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DELETE FROM zmp_param.

    INSERT zmp_param FROM TABLE @( VALUE #(
*        ( function = 'CIAO' config_key = 'key1' config_value = 'prima chiave' )
        ( function = 'CIAO' config_key = 'key1' company = 'XXX' config_value = 'prima chiave XXX' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' config_value = 'prima chiave AAA' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P01' config_value = 'prima chiave AAA P01' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P02' config_value = 'prima chiave AAA P02' )
        ( function = 'CIAO' config_key = 'key2' config_value = 'seconda chiave' )
    ) ).

    DATA(lo_config) = zmp_config=>create( iv_function = 'CIAO' ).

    DATA: lv_key     TYPE zmp_param-config_key VALUE 'key1',
          lv_company TYPE zmp_param-company VALUE 'AAA',
          lv_plant   TYPE zmp_param-plant VALUE ''.

    DATA(lv_result) = lo_config->get_key(
      EXPORTING
        iv_key = lv_key
        iv_company = lv_company
        iv_plant = lv_plant
        iv_default = 'DEFAULT'
    ).

    out->write( |{ lv_key } { lv_company } { lv_plant } => { lv_result }| ).
    out->write( lo_config->lt_param_eval ).

  ENDMETHOD.


ENDCLASS.
