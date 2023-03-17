CLASS zmp_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC . "TODO: rendere il CREATE PRIVATE (serve public per il classrun).

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    "! Rappresentazione delle chiavi così come lette dal db
    "! (più livello gerarchia)
    TYPES: BEGIN OF ty_param,
             function        TYPE zmp_param-function,
             config_key      TYPE zmp_param-config_key,
             company         TYPE zmp_param-company,
             plant           TYPE zmp_param-plant,
             config_value    TYPE zmp_param-config_value,
             hierarchy_level TYPE i,
           END OF ty_param.

    "! Tipo interno usato per registrare punteggio chiavi
    "! durante la valutazione.
    TYPES: BEGIN OF ty_param_eval.
             INCLUDE TYPE ty_param.
    TYPES:   score TYPE i,
           END OF ty_param_eval.
    TYPES: tyt_param_eval TYPE STANDARD TABLE OF ty_param_eval.

    DATA:
      "! Nome chiave letta principale da db
      gv_function TYPE zmp_param-function,
      "! Tabella interna chiavi lette da db
      gt_param    TYPE TABLE OF ty_param.

    CLASS-METHODS:
      create
        IMPORTING
          iv_function        TYPE zmp_param-function
        RETURNING
          VALUE(lo_instance) TYPE REF TO zmp_config
        RAISING
          zmp_cx_config.

    METHODS:

      constructor
        IMPORTING
          iv_func TYPE zmp_param-function OPTIONAL
        RAISING
          zmp_cx_config,

      get_key
        IMPORTING
          iv_key               TYPE zmp_param-config_key
          iv_company           TYPE zmp_param-company OPTIONAL
          iv_plant             TYPE zmp_param-plant OPTIONAL
          iv_default           TYPE zmp_param-config_value OPTIONAL
        EXPORTING
          VALUE(ot_param_eval) TYPE tyt_param_eval
        RETURNING
          VALUE(ov_value)      TYPE zmp_param-config_value.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS:
      _read_function
        IMPORTING
          iv_func            TYPE zmp_param-function
          iv_hierarchy_level TYPE i
        RAISING
          zmp_cx_config.

    DATA: lt_param_eval TYPE TABLE OF ty_param_eval.

ENDCLASS.



CLASS zmp_config IMPLEMENTATION.


  METHOD create.
    TRY.
        lo_instance = NEW zmp_config( iv_func = iv_function  ).
      CATCH zmp_cx_config INTO DATA(exc).
        RAISE EXCEPTION exc.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.

    CHECK iv_func IS NOT INITIAL.

    gv_function = iv_func.
    DATA(lv_hierarchy_level) = 0.

    TRY.
        _read_function( iv_func = gv_function iv_hierarchy_level = lv_hierarchy_level ).
      CATCH zmp_cx_config INTO DATA(exc).
        RAISE EXCEPTION exc.
    ENDTRY.

  ENDMETHOD.


  METHOD _read_function.

    "  Salvaguardia contro letture circolari
    IF ( iv_hierarchy_level > 5 ).
      RAISE EXCEPTION TYPE zmp_cx_config MESSAGE e002(zmp_err_messages).
    ENDIF.

    "  Verifica contro parent multipli (non ammessi)
    DATA: lv_parent_already_found TYPE abap_boolean.

    SELECT
      function,
      config_key,
      company,
      plant,
      config_value
    FROM zmp_param
    WHERE function = @iv_func
    INTO @DATA(lt_param).

      IF ( lt_param-config_key EQ 'parent_function' ).

        IF ( lv_parent_already_found = 'X' ).
          RAISE EXCEPTION TYPE zmp_cx_config MESSAGE e001(zmp_err_messages) WITH iv_func.
        ELSE.
          lv_parent_already_found = 'X'.
          _read_function( iv_func = CONV #( lt_param-config_value ) iv_hierarchy_level =  iv_hierarchy_level + 1 ).
        ENDIF.

      ELSE.

        APPEND INITIAL LINE TO gt_param ASSIGNING FIELD-SYMBOL(<wa_param>).
        MOVE-CORRESPONDING lt_param TO <wa_param>.

        <wa_param>-hierarchy_level = iv_hierarchy_level.

      ENDIF.

    ENDSELECT.

  ENDMETHOD.


  METHOD get_key.

    LOOP AT gt_param INTO DATA(lv_param) WHERE config_key = iv_key.

      APPEND INITIAL LINE TO ot_param_eval ASSIGNING FIELD-SYMBOL(<wa_param_eval>).
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
    IF ( lines( ot_param_eval ) > 0 ).
      SORT ot_param_eval BY score DESCENDING hierarchy_level ASCENDING.
      IF ( ot_param_eval[ 1 ]-score > 0 ).
        ov_value = ot_param_eval[ 1 ]-config_value.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DELETE FROM zmp_param.

    INSERT zmp_param FROM TABLE @( VALUE #(
        ( function = 'CIAO' config_key = 'key1' config_value = 'prima chiave' )
        ( function = 'CIAO' config_key = 'key1' company = 'XXX' config_value = 'prima chiave XXX' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' config_value = 'prima chiave AAA' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P01' config_value = 'prima chiave AAA P01' )
        ( function = 'CIAO' config_key = 'key1' company = 'AAA' plant = 'P02' config_value = 'prima chiave AAA P02' )
        ( function = 'CIAO' config_key = 'parent_function' config_value = 'CIAOPARENT' )
*        ( function = 'CIAO' config_key = 'parent_function' company = 'abc' config_value = 'CIAOPARENT2' ) " Test parent multipli
        ( function = 'CIAOPARENT' config_key = 'key1' company = 'AAA' config_value = 'prima chiave AAA (parent)' )
        ( function = 'CIAOPARENT' config_key = 'parent_function' company = 'AAA' config_value = 'CIAOGRANDPARENT' )
        ( function = 'CIAOGRANDPARENT' config_key = 'key1' company = 'AAA' config_value = 'prima chiave AAA (grandparent)' )
*        ( function = 'CIAOPARENT' config_key = 'parent_function' config_value = 'CIAO' ) " Test parent circolari
        ( function = 'CIAO' config_key = 'key2' config_value = 'seconda chiave' )
    ) ).

    TRY.
        DATA(lo_config) = zmp_config=>create( iv_function = 'CIAO' ).
      CATCH zmp_cx_config INTO DATA(exc).
        out->write( exc->get_longtext(  ) ).
        EXIT.
    ENDTRY.

    DATA: lv_key     TYPE zmp_param-config_key VALUE 'key1',
          lv_company TYPE zmp_param-company VALUE 'AAA',
          lv_plant   TYPE zmp_param-plant VALUE '',
          lt_eval    TYPE tyt_param_eval.

    DATA(lv_result) = lo_config->get_key(
        EXPORTING
            iv_key = lv_key
            iv_company = lv_company
            iv_plant = lv_plant
            iv_default = 'DEFAULT'
        IMPORTING
            ot_param_eval = lt_eval
    ).

    out->write( |{ lv_key } { lv_company } { lv_plant } => { lv_result }| ).
    out->write( lt_eval ).

  ENDMETHOD.


ENDCLASS.
