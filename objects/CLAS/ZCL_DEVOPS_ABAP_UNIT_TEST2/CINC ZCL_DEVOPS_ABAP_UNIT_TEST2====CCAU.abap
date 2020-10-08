*"* use this source file for your ABAP unit test classes
CLASS ztest_class2 DEFINITION  "#AU Risk_Level Harmless
"#AU Duration Short
FOR TESTING.
  PRIVATE SECTION.
    METHODS mytest FOR TESTING.
ENDCLASS.



CLASS ztest_class2 IMPLEMENTATION.
  METHOD mytest.
    zcl_devops_abap_unit_test2=>set_text_to_x( ).
    cl_abap_unit_assert=>assert_equals( act = zcl_devops_abap_unit_test2=>text
                                        exp = 'X' ).
  ENDMETHOD.

ENDCLASS.