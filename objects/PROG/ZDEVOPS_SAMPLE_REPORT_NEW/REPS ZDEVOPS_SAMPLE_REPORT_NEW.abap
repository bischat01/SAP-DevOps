REPORT zdevops_sample_report_new.

* Report
* Text changed 07012021
TYPE-POOLS: slis.
TABLES mara.

TYPES:BEGIN OF gty_mara,
        matnr TYPE mara-matnr,
        mtart TYPE mara-mtart,
        mbrsh TYPE mara-mbrsh,
        meins TYPE mara-meins,
        maktx TYPE makt-maktx,
        spras TYPE makt-spras,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        loekz TYPE ekpo-loekz,
      END OF gty_mara,
      gtt_mara TYPE TABLE OF gty_mara.

DATA : gt_mara  TYPE  gtt_mara,
       gt_fcat  TYPE  slis_t_fieldcat_alv,
       gr_matnr TYPE ranges_matnr.

CLASS lcl_test DEFINITION FOR TESTING. "#AU Risk_Level Harmless
  "#AU Duration Short
  PRIVATE SECTION.
    METHODS test_po_delete_ind FOR TESTING.
    METHODS test_material_description FOR TESTING.
ENDCLASS.

CLASS lcl_test IMPLEMENTATION.
  METHOD test_po_delete_ind.

    DATA: lt_mara  TYPE gtt_mara,
          lr_matnr TYPE ranges_matnr,
          ls_matnr TYPE range_matnr.

    ls_matnr-sign = 'I'.
    ls_matnr-option = 'EQ'.
    ls_matnr-low = 'CH-6200'.
    APPEND ls_matnr TO lr_matnr.


    PERFORM get_data USING   lr_matnr
                     CHANGING lt_mara.

    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = 'CH-6200'.
    cl_aunit_assert=>assert_equals( act = ls_mara-loekz exp = ' '
     msg = 'Selected PO for material with deletion indicator').
  ENDMETHOD.

  METHOD test_material_description.

    DATA: lt_mara  TYPE gtt_mara,
          lr_matnr TYPE ranges_matnr,
          ls_matnr TYPE range_matnr.

    ls_matnr-sign = 'I'.
    ls_matnr-option = 'EQ'.
    ls_matnr-low = 'CH-6200'.
    APPEND ls_matnr TO lr_matnr.


    PERFORM get_data USING   lr_matnr
                     CHANGING lt_mara.

    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = 'CH-6200'.

    cl_aunit_assert=>assert_equals( act = ls_mara-spras exp = 'E'
     msg = 'Material Description is not in English').
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS so_matnr FOR mara-matnr." NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  IF so_matnr IS INITIAL.
    MESSAGE 'Material Number is Mandatory' TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  APPEND LINES OF so_matnr TO gr_matnr.

  PERFORM get_data USING gr_matnr
                   CHANGING gt_mara.
  PERFORM create_fcat.
  PERFORM display.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data from Database Table
*----------------------------------------------------------------------*
FORM get_data  USING xu_matnr TYPE ranges_matnr
               CHANGING xc_mara TYPE gtt_mara.

  TYPES: BEGIN OF lty_ekpo,
           ebeln TYPE ekpo-ebeln,
           ebelp TYPE ekpo-ebelp,
           loekz TYPE ekpo-loekz,
           matnr TYPE ekpo-matnr,
         END OF lty_ekpo.
  DATA: lt_ekpo TYPE TABLE OF lty_ekpo.


  SELECT  mara~matnr
          mara~mtart
          mara~mbrsh
          mara~meins
          makt~maktx
          makt~spras
     INTO TABLE xc_mara
     FROM mara INNER JOIN makt
     ON mara~matnr EQ makt~matnr
    WHERE mara~matnr IN xu_matnr
    AND makt~spras EQ 'D'.

  DELETE ADJACENT DUPLICATES FROM xc_mara COMPARING matnr.

  SELECT ebeln
         ebelp
         loekz
         matnr
   INTO TABLE lt_ekpo
   FROM ekpo
  WHERE ekpo~matnr IN xu_matnr.
  IF sy-subrc EQ 0.

    LOOP AT xc_mara ASSIGNING FIELD-SYMBOL(<lfs_mara>).
      READ TABLE lt_ekpo INTO DATA(ls_ekpo) WITH KEY matnr = <lfs_mara>-matnr.
      IF sy-subrc EQ 0.
        <lfs_mara>-ebeln = ls_ekpo-ebeln.
        <lfs_mara>-ebelp = ls_ekpo-ebelp.
        <lfs_mara>-loekz = ls_ekpo-loekz.
        CLEAR ls_ekpo.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
FORM display .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = 'SY-REPID'
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = gt_mara.

ENDFORM.                    " DISPLAY

*&---------------------------------------------------------------------*
*&      Form  CREATE FCAT
*&---------------------------------------------------------------------*
*       Create Field Cat
*----------------------------------------------------------------------*

FORM create_fcat .

  DATA: ls_fcat TYPE slis_fieldcat_alv.

  ls_fcat-col_pos = '1'.
  ls_fcat-fieldname = 'MATNR'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-ref_fieldname = 'MATNR'.
  ls_fcat-ref_tabname = 'MARA'.
  ls_fcat-seltext_m = 'MATERIAL NO'.
  ls_fcat-key = 'X'.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.

  ls_fcat-col_pos = '2'.
  ls_fcat-fieldname = 'MAKTX'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-ref_fieldname = 'MAKTX'.
  ls_fcat-ref_tabname = 'MAKT'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.


  ls_fcat-col_pos = '3'.
  ls_fcat-fieldname = 'MTART'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-seltext_m = 'MATERIAL TYPE'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.

  ls_fcat-col_pos = '4'.
  ls_fcat-fieldname = 'MBRSH'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-emphasize = 'C601'.
  ls_fcat-seltext_m = 'IND.SECTOR'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.

  ls_fcat-col_pos = '5'.
  ls_fcat-fieldname = 'MEINS'.
  ls_fcat-tabname = 'GT_MARA'.
*  ls_fcat-edit = 'X'.
  ls_fcat-seltext_m = 'MATERIAL UNITS'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.

  ls_fcat-col_pos = '6'.
  ls_fcat-fieldname = 'EBELN'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-ref_fieldname = 'EBELN'.
  ls_fcat-ref_tabname = 'EKPO'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.

  ls_fcat-col_pos = '7'.
  ls_fcat-fieldname = 'EBELP'.
  ls_fcat-tabname = 'GT_MARA'.
  ls_fcat-ref_fieldname = 'EBELP'.
  ls_fcat-ref_tabname = 'EKPO'.
  APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.
ENDFORM.                    " CREATE_FCAT