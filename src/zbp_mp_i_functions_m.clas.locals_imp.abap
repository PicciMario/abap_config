CLASS lhc_ZMP_I_FUNCTIONS_M DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zmp_i_functions_m RESULT result.
    METHODS simula FOR MODIFY
      IMPORTING keys FOR ACTION zmp_i_functions_m~simula RESULT result.

ENDCLASS.

CLASS lhc_ZMP_I_FUNCTIONS_M IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD simula.
  ENDMETHOD.

ENDCLASS.
