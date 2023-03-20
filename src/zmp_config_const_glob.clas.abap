CLASS zmp_config_const_glob DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:

        "! <p>Il valore di questa chiave indica il nome di una funzione che è
        "! considerata parent della funzione a cui la chiave appartiene.
        "! Quando la funzione viene letta, viene letto anche il suo parent
        "! (e a sua volta il suo parent, se presente, e così via).</p>
        "! Ciascuna funzione può contenere <strong>una sola</strong> chiave di
        "! questo tipo, altrimenti si genera una eccezione in fase di lettura.
        parent_function TYPE string VALUE 'PARENT_FUNCTION'.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zmp_config_const_glob IMPLEMENTATION.
ENDCLASS.
