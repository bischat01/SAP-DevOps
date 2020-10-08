class ZCL_DEVOPS_ABAP_UNIT_TEST definition
  public
  final
  create public .

public section.

*  class-data CI_I_FINAL type ZABAPUNIT_FINAL_TABLE .

  class-methods SELECT_MARA
    importing
      !IM_V_MATNR type MATNR
    exporting
      !EX_WA_MARA type MARA .