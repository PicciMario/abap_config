CLASS zmp_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF lt_param,
             function     TYPE zmp_param-function,
             config_key   TYPE zmp_param-config_key,
             company      TYPE zmp_param-company,
             plant        TYPE zmp_param-plant,
             config_value TYPE zmp_param-config_value,
           END OF lt_param.

    DATA: gv_function TYPE zmp_param-function,
          gt_param    TYPE TABLE OF lt_param.

    CLASS-METHODS:
      create
        IMPORTING
          iv_function        TYPE zmp_param-function
        RETURNING
          VALUE(lo_instance) TYPE REF TO zmp_config.

    METHODS:
      constructor
        IMPORTING
          iv_func TYPE zmp_param-function,
      get_key
        IMPORTING
          iv_key   TYPE zmp_param-config_key
        EXPORTING
          ov_value TYPE zmp_param-config_value.

  PROTECTED SECTION.

  PRIVATE SECTION.

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
    ov_value = |ottimismo!|.
  ENDMETHOD.



ENDCLASS.
