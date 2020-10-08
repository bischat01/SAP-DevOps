
CLASS zcl_devops_abap_unit DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>zcl_devops_Abap_Unit
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_devops_ABAP_UNIT_TEST
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_devops_abap_unit_test.  "class under test

    METHODS: select_mara FOR TESTING.
ENDCLASS.       "zcl_devops_Abap_Unit


CLASS zcl_devops_abap_unit IMPLEMENTATION.

  METHOD select_mara.

    DATA: ex_wa_mara TYPE mara.

    CALL METHOD zcl_devops_abap_unit_test=>select_mara
      EXPORTING
        im_v_matnr = 'WD501'
      IMPORTING
        ex_wa_mara = ex_wa_mara.

    CALL METHOD cl_abap_unit_assert=>assert_equals
      EXPORTING
        act = ex_wa_mara-matnr
        exp = 'DUMMY DATA'.



  ENDMETHOD.


ENDCLASS.