CLASS zmp_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC . "TODO: rendere il CREATE PRIVATE (serve public per il classrun).

  PUBLIC SECTION.

    INTERFACES: if_oo_adt_classrun.

    " Rappresentazione delle chiavi così come lette dal db
    " (più livello gerarchia)
    TYPES: BEGIN OF ty_param,
             function        TYPE zmp_param-function,
             config_key      TYPE zmp_param-config_key,
             company         TYPE zmp_param-company,
             plant           TYPE zmp_param-plant,
             config_value    TYPE zmp_param-config_value,
             hierarchy_level TYPE i,
           END OF ty_param,
           tyt_param TYPE TABLE OF ty_param.

    " Tipo interno usato per registrare punteggio chiavi
    " durante la valutazione.
    TYPES: BEGIN OF ty_param_eval.
             INCLUDE TYPE ty_param.
    TYPES:   score TYPE i,
           END OF ty_param_eval.
    TYPES: tyt_param_eval TYPE STANDARD TABLE OF ty_param_eval.

    CLASS-METHODS:

      "! Metodo statico di costruzione dell'oggetto di configurazione.
      "!
      "! @parameter iv_function | Nome funzione.
      "! @parameter lo_instance | Istanza classe.
      "! @raising zmp_cx_config | Eccezioni per errori struttura db configurazione.
      create
        IMPORTING
          iv_function        TYPE zmp_param-function
        RETURNING
          VALUE(lo_instance) TYPE REF TO zmp_config
        RAISING
          zmp_cx_config.

    METHODS:

      "! Costruttore (solo uso interno).
      "!
      "! @parameter iv_func     | Nome funzione.
      "! @raising zmp_cx_config | Eccezioni per errori struttura db configurazione.
      constructor
        IMPORTING
          iv_func TYPE zmp_param-function OPTIONAL
        RAISING
          zmp_cx_config,

      "! Legge il valore di una chiave.
      "! <em>Memoizza la chiamata a _get_key.</em>
      "!
      "! @parameter iv_key        | Chiave
      "! @parameter iv_company    | Company
      "! @parameter iv_plant      | Plant
      "! @parameter iv_default    | Default restituito se chiave non trovata
      "! @parameter ot_param_eval | Tabella debug processo di valutazione chiavi
      "! @parameter ov_value      | Valore chiave trovato
      get_key
        IMPORTING
          iv_key               TYPE zmp_param-config_key
          iv_company           TYPE zmp_param-company OPTIONAL
          iv_plant             TYPE zmp_param-plant OPTIONAL
          iv_default           TYPE zmp_param-config_value OPTIONAL
        EXPORTING
          VALUE(ot_param_eval) TYPE tyt_param_eval
        RETURNING
          VALUE(ov_value)      TYPE zmp_param-config_value,

      "! Riempie la struttura passata in ingresso con le corrispondenti
      "! chiavi di configurazione.
      "!
      "! @parameter iv_company | Company
      "! @parameter iv_plant   | Plant
      "! @parameter iv_default | Default assegnato se chiavi non trovate
      "! @parameter is_config  | Struttura da riempire
      get_config
        IMPORTING
          iv_company TYPE zmp_param-company OPTIONAL
          iv_plant   TYPE zmp_param-plant OPTIONAL
          iv_default TYPE zmp_param-config_value OPTIONAL
        CHANGING
          is_config  TYPE any,


      get_all_keys
        IMPORTING
          include_parents TYPE abap_boolean DEFAULT abap_false
        CHANGING
          ot_param        TYPE tyt_param.


  PROTECTED SECTION.

  PRIVATE SECTION.

    " Tipi per cache (statica) istanze
    TYPES: BEGIN OF ty_instances,
             function TYPE zmp_param-function,
             instance TYPE REF TO zmp_config,
           END OF ty_instances.
    TYPES: tyt_instances TYPE TABLE OF ty_instances.

    " Tipi per cache (locale) ricerche
    TYPES: BEGIN OF ty_search_cache,
             config_key   TYPE zmp_param-config_key,
             company      TYPE zmp_param-company,
             plant        TYPE zmp_param-plant,
             config_value TYPE zmp_param-config_value,
           END OF ty_search_cache.
    TYPES: tyt_search_cache TYPE TABLE OF ty_search_cache.

    " Tipi e variabili usati per ricostruzione gerarchia funzioni
    TYPES: BEGIN OF ty_function_eval.
             INCLUDE TYPE zmp_functions.
    TYPES:   hierarchy_level TYPE i,
           END OF ty_function_eval.
    TYPES: tyt_function_eval TYPE STANDARD TABLE OF ty_function_eval.


    DATA:
      "! Tabella interna chiavi lette da db.
      gt_param        TYPE SORTED TABLE OF ty_param WITH NON-UNIQUE KEY primary_key COMPONENTS config_key,
      "! Tabella interna cache ricerche.
      gt_search_cache TYPE tyt_search_cache,
      "! Function principale di questa istanza.
      gt_function     TYPE zmp_functions-function.

    CLASS-DATA:
      "! Cache statica istanze (per function).
      gt_instances     TYPE tyt_instances,
      "! Cache elenco completo funzioni
      lt_all_functions TYPE TABLE OF zmp_functions.

    METHODS:

      "! Metodo di lettura chiavi da db.
      "!
      "! @parameter iv_func            | Nome funzione
      "! @raising zmp_cx_config        | Eccezioni per errori struttura db configurazione.
      _read_function
        IMPORTING
          iv_func TYPE zmp_param-function
        RAISING
          zmp_cx_config,

      "! Legge il valore di una chiave. Uso interno.
      "!
      "! @parameter iv_key        | Chiave
      "! @parameter iv_company    | Company
      "! @parameter iv_plant      | Plant
      "! @parameter ot_param_eval | Tabella debug processo di valutazione chiavi
      "! @parameter ov_value      | Valore chiave trovato
      _get_key
        IMPORTING
          iv_key               TYPE zmp_param-config_key
          iv_company           TYPE zmp_param-company OPTIONAL
          iv_plant             TYPE zmp_param-plant OPTIONAL
        EXPORTING
          VALUE(ot_param_eval) TYPE tyt_param_eval
        RETURNING
          VALUE(ov_value)      TYPE zmp_param-config_value.


ENDCLASS.



CLASS zmp_config IMPLEMENTATION.


  METHOD create.

    " Se esiste istanza di classe per questa function, restituisce quella...
    lo_instance = VALUE #( gt_instances[ function = iv_function ]-instance OPTIONAL ).

    " ...altrimenti inizializza una nuova istanza.
    IF ( lo_instance IS INITIAL ).
      TRY.
          lo_instance = NEW zmp_config( iv_func = iv_function  ).
          APPEND VALUE ty_instances( function = iv_function instance = lo_instance  ) TO gt_instances.
        CATCH zmp_cx_config INTO DATA(exc).
          RAISE EXCEPTION exc.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    CHECK iv_func IS NOT INITIAL.

    " Function principale di questa istanza.
    gt_function = iv_func.

    DATA(lv_hierarchy_level) = 0.

    TRY.
        _read_function( iv_func = iv_func ).
      CATCH zmp_cx_config INTO DATA(exc).
        RAISE EXCEPTION exc.
    ENDTRY.

  ENDMETHOD.


  METHOD _read_function.

    " Legge cache funzioni

    IF ( lt_all_functions IS INITIAL ).
      SELECT * FROM zmp_functions INTO TABLE @lt_all_functions.
    ENDIF.

    " Analizza elenco funzioni e risale la gerarchia dei parent.

    DATA: lt_functions     TYPE tyt_function_eval,
          wa_functions     TYPE ty_function_eval,
          ls_all_functions TYPE zmp_functions.

    DATA: lv_parent TYPE zmp_de_function.
    DATA: lv_hierarchy_level TYPE i .
    lv_parent = iv_func.
    lv_hierarchy_level = 0.

    DO.

      "  Salvaguardia contro letture circolari
      IF ( sy-index > 5 ).
        RAISE EXCEPTION TYPE zmp_cx_config MESSAGE e002(zmp_err_messages).
      ENDIF.

      ls_all_functions = VALUE #( lt_all_functions[ function = lv_parent ] OPTIONAL ).

      IF ( ls_all_functions IS INITIAL ).
        EXIT.
      ENDIF.

      CLEAR wa_functions.
      MOVE-CORRESPONDING ls_all_functions TO wa_functions.
      wa_functions-hierarchy_level = lv_hierarchy_level.
      lv_hierarchy_level += 1.
      APPEND wa_functions TO lt_functions.

      IF ( wa_functions-parent IS INITIAL ).
        EXIT.
      ELSE.
        lv_parent = wa_functions-parent.
      ENDIF.

    ENDDO.

    " Legge chiavi per le funzioni identificate

    LOOP AT lt_functions INTO DATA(ls_function).

      SELECT
        function,
        config_key,
        company,
        plant,
        config_value
      FROM zmp_param
      WHERE function = @ls_function-function
      INTO @DATA(lt_param).

        DATA: wa_param LIKE LINE OF gt_param.
        MOVE-CORRESPONDING lt_param TO wa_param.
        wa_param-hierarchy_level = ls_function-hierarchy_level.
        INSERT wa_param INTO TABLE gt_param.

      ENDSELECT.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_key.

    ov_value =
        VALUE #(
          gt_search_cache[ config_key = iv_key company = iv_company plant = iv_plant ]-config_value
          DEFAULT
            _get_key(
            EXPORTING
              iv_key = iv_key
              iv_company = iv_company
              iv_plant = iv_plant
            IMPORTING
              ot_param_eval = ot_param_eval
            )
        ).

    IF ( ov_value IS INITIAL ).
      ov_value = iv_default.
    ENDIF.

  ENDMETHOD.


  METHOD _get_key.

    LOOP AT gt_param INTO DATA(lv_param) WHERE config_key = iv_key.

      APPEND INITIAL LINE TO ot_param_eval ASSIGNING FIELD-SYMBOL(<wa_param_eval>).
      MOVE-CORRESPONDING lv_param TO <wa_param_eval>.
      <wa_param_eval>-score = 1.

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

    IF ( lines( ot_param_eval ) > 0 ).
      SORT ot_param_eval BY score DESCENDING hierarchy_level ASCENDING.
      IF ( ot_param_eval[ 1 ]-score > 0 ).
        ov_value = ot_param_eval[ 1 ]-config_value.
      ENDIF.
    ENDIF.

    " Registra risultato in cache
    DATA lv_search_cache TYPE ty_search_cache.
    lv_search_cache-config_key = iv_key.
    lv_search_cache-company = iv_company.
    lv_search_cache-plant = iv_plant.
    lv_search_cache-config_value = ov_value.
    INSERT lv_search_cache INTO TABLE gt_search_cache.

  ENDMETHOD.


  METHOD get_config.

    DATA: ls_components TYPE abap_compdescr.
    DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.

    lo_strucdescr ?= cl_abap_structdescr=>describe_by_data( is_config ).

    LOOP AT lo_strucdescr->components INTO ls_components.

      DATA(lv_value) = get_key(
        iv_key = CONV #( ls_components-name )
        iv_company = iv_company
        iv_plant = iv_plant
      ).

      is_config-(ls_components-name) = CONV #( lv_value ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_all_keys.

    IF ( include_parents = abap_true ).
      MOVE-CORRESPONDING gt_param TO ot_param.
    ELSE.
      MOVE-CORRESPONDING gt_param TO ot_param.
      DELETE ot_param WHERE function NE gt_function.
    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DELETE FROM zmp_functions.
    DELETE FROM zmp_param.

    INSERT zmp_functions FROM TABLE @( VALUE #(
        ( function = 'CIAO'            description = 'Funzione CIAO'             parent = 'CIAOPARENT' )
        ( function = 'CIAOPARENT'      description = 'Funzione CIAO PARENT'      parent = 'CIAOGRANDPARENT' )
        ( function = 'CIAOGRANDPARENT' description = 'Funzione CIAO GRANDPARENT' )
    ) ).

    INSERT zmp_param FROM TABLE @( VALUE #(
        ( function = 'CIAO'            config_key = 'KEY1'                                          config_value = 'prima chiave' )
        ( function = 'CIAO'            config_key = 'KEY1'            company = 'XXX'               config_value = 'prima chiave XXX' )
        ( function = 'CIAO'            config_key = 'KEY1'            company = 'AAA'               config_value = 'prima chiave AAA' )
        ( function = 'CIAO'            config_key = 'KEY1'            company = 'AAA' plant = 'P01' config_value = 'prima chiave AAA P01' )
        ( function = 'CIAO'            config_key = 'KEY1'            company = 'AAA' plant = 'P02' config_value = 'prima chiave AAA P02' )

        ( function = 'CIAOPARENT'      config_key = 'KEY1'            company = 'AAA'               config_value = 'prima chiave AAA (parent)' )

        ( function = 'CIAOGRANDPARENT' config_key = 'KEY1'            company = 'AAA'               config_value = 'prima chiave AAA (grandparent)' )

        ( function = 'CIAO'            config_key = 'KEY2'                                          config_value = 'seconda chiave' )
        ( function = 'CIAO'            config_key = 'KEY2'            company = 'AAA'               config_value = 'seconda chiave AAA' )
        ( function = 'CIAO'            config_key = 'KEY2'            company = 'AAA' plant = 'P02' config_value = 'seconda chiave AAA P01' )
    ) ).

    out->write( '------------------------------------------------------------------------' ).
    out->write( '--  TEST  --------------------------------------------------------------' ).
    out->write( '------------------------------------------------------------------------' ).

    TRY.
        DATA(lo_config) = zmp_config=>create( iv_function = 'CIAO' ).
      CATCH zmp_cx_config INTO DATA(exc).
        out->write( exc->get_longtext(  ) ).
        EXIT.
    ENDTRY.

    DATA: lv_key     TYPE zmp_param-config_key VALUE zmp_config_const_cli=>key1,
          lv_company TYPE zmp_param-company VALUE 'AAA',
          lv_plant   TYPE zmp_param-plant VALUE 'P01',
          lt_eval    TYPE tyt_param_eval.

    out->write( '' ).
    out->write( '----- Lettura singola chiave' ).

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

    out->write( '' ).
    out->write( '----- Tabella di valutazione lettura precedente:' ).

    out->write( lt_eval ).

    out->write( ' ' ).
    out->write( '----- Lettura gruppo di chiavi:' ).

    TYPES: BEGIN OF ty_test,
             key1 TYPE c LENGTH 50,
             key2 TYPE c LENGTH 50,
             key3 TYPE c LENGTH 50,
           END OF ty_test.
    DATA: ls_test TYPE ty_test.

    lo_config->get_config(
        EXPORTING
            iv_company = lv_company
            iv_plant = lv_plant
        CHANGING
            is_config = ls_test
    ).

    out->write( ls_test ).

    out->write( ' ' ).
    out->write( '----- Stampa lista chiavi:' ).

    DATA: lt_all_keys TYPE tyt_param.

    lo_config->get_all_keys(
        EXPORTING
            include_parents = abap_true
        CHANGING
            ot_param = lt_all_keys
    ).

    out->write( lt_all_keys ).


  ENDMETHOD.


ENDCLASS.
