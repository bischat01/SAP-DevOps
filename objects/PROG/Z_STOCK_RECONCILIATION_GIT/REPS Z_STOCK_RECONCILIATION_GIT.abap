*&---------------------------------------------------------------------*
*& Report Z_STOCK_RECONCILIATION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_stock_reconciliation_git.
************************************************************************
* Data Declarations
************************************************************************
INCLUDE Z_STOCK_RECON_TOP.
*INCLUDE z_stock_reconciliation_top.
************************************************************************
* Selection Screen
************************************************************************
INCLUDE Z_STOCK_RECON_SEL.
*INCLUDE z_stock_reconciliation_sel.
************************************************************************
* Program Logic - subroutines
************************************************************************
INCLUDE Z_STOCK_RECON_SUB.
*INCLUDE z_stock_reconciliation_sub.
************************************************************************
* Modify Screen Fields
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM sub_modify_screen.
************************************************************************
* F4 Help  before conversion file path
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bfile.
  PERFORM sub_f4_file CHANGING p_bfile.
************************************************************************
* F4 Help after conversion file path
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_afile.
  PERFORM sub_f4_file CHANGING p_afile.
************************************************************************
* F4 Help  before conversion split valuation file
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bsplit.
  PERFORM sub_f4_file CHANGING p_bsplit.
************************************************************************
* F4 Help  after conversion split valuation file path
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_asplit.
  PERFORM sub_f4_file CHANGING p_asplit.

START-OF-SELECTION.
************************************************************************
* Selection screen validations
************************************************************************
* Validate File
************************************************************************
  IF rb_bconv IS NOT INITIAL.  "ECC
* Verify file
  PERFORM file_validation.
  ELSEIF rb_aconv IS NOT INITIAL. "S4
    PERFORM sub_file_validation.
  ENDIF.
************************************************************************
* Validate Plant
************************************************************************
  PERFORM sub_plant_validation.
************************************************************************
* Validate Storage Location
************************************************************************
  PERFORM sub_sloc_validation.

  IF rb_bconv IS NOT INITIAL. "ECC
*   Get all the relevent data for before convertion
    PERFORM get_data_bconv.

  ELSEIF rb_aconv IS NOT INITIAL. "S4
************************************************************************
* Get before conversion data from file
************************************************************************
    PERFORM sub_get_bconv_data.
************************************************************************
* Get s4 data
************************************************************************
    PERFORM sub_get_data_s4.
  ENDIF.

END-OF-SELECTION.
  IF rb_bconv IS NOT INITIAL.  "ECC
*   Process the data
    PERFORM proc_data_before_conv.

* Upload data to App server
    PERFORM upload_file.

  ELSEIF rb_aconv IS NOT INITIAL. "S4
************************************************************************
* reconciliation
************************************************************************
    PERFORM sub_stock_recon.
************************************************************************
* Upload final file
************************************************************************
    PERFORM sub_file_upload.
  ENDIF.
************************************************************************
* Display Output
************************************************************************
  PERFORM sub_recon_output.
************************************************************************