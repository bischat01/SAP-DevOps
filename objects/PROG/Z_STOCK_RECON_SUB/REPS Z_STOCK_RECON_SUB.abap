*&---------------------------------------------------------------------*
*& Include          Z_STOCK_RECONCILIATION_SUB
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SUB_MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& Screen field modifications
*&---------------------------------------------------------------------*
FORM sub_modify_screen .

  LOOP AT SCREEN.
    IF rb_im IS NOT INITIAL."test
      IF screen-group1 = 'LGN'(005).
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LGN'(005).
*       screen-required = 2.
      ENDIF.
    ENDIF.

    IF rb_bconv IS NOT INITIAL.
      IF screen-name = 'P_AFILE'(006).
        screen-input = 0.
      ELSEIF screen-group1 = 'DAT'(007).
        screen-input = 0.
      ELSEIF screen-name = 'P_BFILE'(008).
        screen-required = 2.
      ENDIF.
    ENDIF.

    IF rb_aconv IS NOT INITIAL.
      IF screen-name = 'P_AFILE'(006).
        screen-required = 2.
      ELSEIF screen-group1 = 'DAT'(007).
        screen-input = 0.
      ELSEIF screen-name = 'P_BFILE'(008).
        screen-required = 2.
      ENDIF.

    ENDIF.

    IF screen-group1 = 'WER'(009).
      screen-required = 2.
    ENDIF.

    IF cb_split IS NOT INITIAL.
      IF screen-group1 = 'SPL'(010).
        screen-active = 1.
        screen-required = 2.
      ENDIF.
      IF screen-group1 = 'ASP'(011).
        screen-active = 1.
        IF rb_bconv IS NOT INITIAL.
          screen-input = 0.
          screen-required = 0.
        ELSEIF rb_aconv IS NOT INITIAL.
          screen-input = 1.
          screen-required = 2.
        ENDIF.
      ENDIF.
    ELSE.
      IF screen-group1 = 'SPL'(010).
        screen-active = 0.
      ENDIF.

      IF screen-group1 = 'ASP'(011).
        screen-active = 0.
      ENDIF.
    ENDIF.
    IF cb_split IS INITIAL.
      CLEAR: p_bsplit,
             p_asplit.
    ENDIF.
    IF rb_bconv IS NOT INITIAL.
      CLEAR: p_afile,
             p_asplit.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form  sub_f4_file
*&---------------------------------------------------------------------*
*& Value help for file path selection
*----------------------------------------------------------------------*
FORM sub_f4_file CHANGING cp_file TYPE rlgrap-filename.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = cp_file
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc IS NOT INITIAL.
*   No Action
  ENDIF.


ENDFORM.                    " sub_f4_file
*&---------------------------------------------------------------------*
*& Form SUB_FILE_VALIDATION
*&---------------------------------------------------------------------*
*& FILE VALIDATION
*&---------------------------------------------------------------------*
FORM sub_file_validation .
  DATA: lv_ext      TYPE string,
        lv_filepath TYPE char255,
        lv_fname    TYPE string.
  IF rb_bconv IS NOT INITIAL. "Before Conversion
    IF p_bfile IS INITIAL.
      MESSAGE 'Fill File Path'(012) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE. "After Converison
    IF p_bfile IS INITIAL
      OR p_afile IS INITIAL.
      MESSAGE 'Fill File Path'(012) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF cb_split IS NOT INITIAL.
      IF p_bsplit IS INITIAL
        OR p_asplit IS INITIAL.
        MESSAGE 'Fill File Path'(012) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF. "IF cb_split IS NOT INITIAL.
  ENDIF.
  IF cb_split IS INITIAL.
    CLEAR: p_bsplit,
           p_asplit.
  ENDIF.
  IF rb_bconv IS NOT INITIAL.
    CLEAR: p_afile,
           p_asplit.
  ENDIF.
  IF p_bfile IS NOT INITIAL.
    PERFORM sub_file_ext_chk USING p_bfile.

  ENDIF.
  IF p_afile IS NOT INITIAL.
    PERFORM sub_file_ext_chk USING p_afile.

  ENDIF.
  IF p_bsplit IS NOT INITIAL.
    PERFORM sub_file_ext_chk USING p_bsplit.

  ENDIF.
  IF p_asplit IS NOT INITIAL.
    PERFORM sub_file_ext_chk USING p_asplit.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_PLANT_VALIDATION
*&---------------------------------------------------------------------*
*& Plant Validation
*&---------------------------------------------------------------------*
FORM sub_plant_validation .
  DATA: lv_werks TYPE werks_d.

  IF s_werks IS INITIAL.
    MESSAGE 'Fill out mandatory fields'(018) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
    LEAVE LIST-PROCESSING.
  ELSE.
    SELECT SINGLE
            werks
       FROM t001w
      INTO lv_werks
      WHERE werks IN s_werks.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Invalid Plant'(019) TYPE  'S'(013) DISPLAY LIKE 'E'(014).
 LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_SLOC_VALIDATION
*&---------------------------------------------------------------------*
*& Storage Location Validation
*&---------------------------------------------------------------------*
FORM sub_sloc_validation .
  TYPES: BEGIN OF ty_lgnum,
           lgnum TYPE lgnum,
         END OF ty_lgnum.

  DATA: lv_werks TYPE werks_d,
        lv_lgort TYPE lgort_d,
        lv_xhupf TYPE xhupf.

  DATA: lt_lgnum TYPE TABLE OF ty_lgnum INITIAL SIZE 0.

  IF s_lgort IS INITIAL.

  ELSE.
    IF rb_im IS NOT INITIAL.
      SELECT SINGLE werks lgort FROM t320
        INTO (lv_werks, lv_lgort)
      WHERE werks IN s_werks
        AND lgort IN s_lgort.
      IF sy-subrc IS INITIAL.
        MESSAGE 'Invalid Storage Loc for IM Plant'(020) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF rb_wm IS NOT INITIAL.
      SELECT SINGLE werks lgort FROM t320
        INTO (lv_werks, lv_lgort)
      WHERE werks IN s_werks
        AND lgort IN s_lgort.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE xhupf FROM t001l
          INTO lv_xhupf
          WHERE werks IN s_werks
            AND lgort IN s_lgort
            AND xhupf = 'X'(022).
        IF sy-subrc IS INITIAL.
          MESSAGE 'WM Plant has HU'(021) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
          LEAVE LIST-PROCESSING.
        ENDIF.
      ELSE.
        MESSAGE 'Invalid Storage Loc for WM Plant'(023) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    IF rb_wmhu IS NOT INITIAL.
      SELECT SINGLE werks lgort FROM t320
        INTO (lv_werks, lv_lgort)
      WHERE werks IN s_werks
        AND lgort IN s_lgort.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE xhupf FROM t001l
          INTO lv_xhupf
        WHERE werks IN s_werks
          AND lgort IN s_lgort
          AND xhupf = space.
        IF sy-subrc IS INITIAL.
          MESSAGE 'WM Plant is not HU handeled'(024) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
          LEAVE LIST-PROCESSING.
        ENDIF.
      ELSE.
        MESSAGE 'Invalid Storage Loc for WM Plant'(025) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_BCONV_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM sub_get_bconv_data .
  TYPES:
    BEGIN OF ty_output_tmp,
      werks      TYPE werks_d,
      lgort      TYPE lgort_d,
      matnr      TYPE matnr,
      charg      TYPE charg_d,
      sobkz      TYPE sobkz,
      acc_assign TYPE char10,
      meins      TYPE meins,
      b_clabs    TYPE char17,
      b_ceinm    TYPE char17,
      b_cinsm    TYPE char17,
      b_cspem    TYPE char17,
      b_cumlm    TYPE char17,
      b_return   TYPE char17,
      remarks    TYPE char10,
    END OF ty_output_tmp,

    BEGIN OF ty_split_tmp,
      matnr   TYPE matnr,
      bwkey   TYPE bwkey,
      bwtar   TYPE bwtar_d,
      meins   TYPE meins,
      b_lbkum TYPE char17,
      a_lbkum TYPE char17,
      remarks TYPE char10,
    END OF ty_split_tmp.

  DATA: lv_file       TYPE string.
  DATA: lt_data_tab   TYPE STANDARD TABLE OF rsanm_file_line
        INITIAL SIZE 0.

  DATA: wa_bconv_data  TYPE ty_output_tmp,
        wa_bconv_split TYPE ty_split_tmp,
        lv_pspnr       TYPE ps_psp_pnr,
        lv_acc_assign  TYPE ps_posid.
  CONSTANTS: lc_fsep TYPE char1 VALUE ','.

  DATA lv_cr TYPE c.

  lv_cr = cl_abap_char_utilities=>cr_lf.


  CLEAR: lv_file,
         lt_data_tab[].
***********************************************************************
* read before convertion data from file
***********************************************************************
  lv_file = p_bfile.
  CALL METHOD cl_rsan_ut_appserv_file_reader=>appserver_file_read
    EXPORTING
      i_filename   = lv_file
    CHANGING
      c_data_tab   = lt_data_tab
    EXCEPTIONS
      open_failed  = 1
      read_failed  = 2
      close_failed = 3
      OTHERS       = 4.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_data_tab ASSIGNING FIELD-SYMBOL(<fs_data_tab>).
      IF sy-tabix EQ 1.
        IF <fs_data_tab> CS 'Plant'(026).
          DATA(lv_tabix) = 1.
        ELSE.
          lv_tabix = 4.
        ENDIF.
      ENDIF.
      IF sy-tabix GT lv_tabix. "1. "excluding the header lines
        REPLACE lv_cr IN <fs_data_tab> WITH space.
        SPLIT <fs_data_tab> AT lc_fsep INTO:  wa_bconv_data-werks
                                              wa_bconv_data-lgort
                                              wa_bconv_data-matnr
                                              wa_bconv_data-charg
                                              wa_bconv_data-sobkz
                                              wa_bconv_data-acc_assign
                                              wa_bconv_data-meins
                                              wa_bconv_data-b_clabs
                                              wa_bconv_data-b_ceinm
                                              wa_bconv_data-b_cinsm
                                              wa_bconv_data-b_cspem
                                              wa_bconv_data-b_cumlm
                                              wa_bconv_data-b_return
                                              wa_bconv_data-remarks.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input        = wa_bconv_data-matnr
          IMPORTING
            output       = wa_bconv_data-matnr
          EXCEPTIONS
            length_error = 1
            OTHERS       = 2.
        IF sy-subrc IS NOT INITIAL.
*            No Action
        ENDIF.
        CLEAR: lv_acc_assign.
        lv_acc_assign = wa_bconv_data-acc_assign.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
          EXPORTING
            input     = lv_acc_assign
          IMPORTING
            output    = lv_pspnr
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc IS INITIAL
          AND lv_pspnr IS NOT INITIAL.
          wa_bconv_data-acc_assign = lv_pspnr.
        ENDIF.


        DATA(wa_output) = VALUE ty_output( werks = |{ wa_bconv_data-werks ALPHA = IN }|
                                           lgort = |{ wa_bconv_data-lgort ALPHA = IN }|
                                           matnr = wa_bconv_data-matnr
                                           charg = |{ wa_bconv_data-charg ALPHA = IN }|
                                           sobkz = |{ wa_bconv_data-sobkz ALPHA = IN }|
*                                           acc_assign = |{ wa_bconv_data-acc_assign ALPHA = IN }|
                                           acc_assign = wa_bconv_data-acc_assign
                                           meins = wa_bconv_data-meins
                                           b_clabs = wa_bconv_data-b_clabs
                                           b_ceinm = wa_bconv_data-b_ceinm
                                           b_cinsm = wa_bconv_data-b_cinsm
                                           b_cspem = wa_bconv_data-b_cspem
                                           b_cumlm = wa_bconv_data-b_cumlm
                                           b_return = wa_bconv_data-b_return ).
        APPEND wa_output TO gt_output.
      ENDIF. "IF sy-tabix GT 1
    ENDLOOP.
  ENDIF. "read file sy-subrc check


***********************************************************************
* whenever split option is selected
***********************************************************************
  IF cb_split IS NOT INITIAL.
    CLEAR: lv_file,
           lt_data_tab[].
***********************************************************************
* read before convertion data from file
***********************************************************************
    lv_file = p_bsplit.
    CALL METHOD cl_rsan_ut_appserv_file_reader=>appserver_file_read
      EXPORTING
        i_filename   = lv_file
      CHANGING
        c_data_tab   = lt_data_tab
      EXCEPTIONS
        open_failed  = 1
        read_failed  = 2
        close_failed = 3
        OTHERS       = 4.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_data_tab ASSIGNING FIELD-SYMBOL(<fs_data_tab2>).
        IF sy-tabix EQ 1.
          IF <fs_data_tab2> CS 'Plant'(026).
            DATA(lv_tabix2) = 1.
          ELSE.
            lv_tabix2 = 4.
          ENDIF.
        ENDIF.
        IF sy-tabix GT lv_tabix2. "excluding the header lines
          CLEAR: wa_bconv_data.
          REPLACE lv_cr IN <fs_data_tab2> WITH space.
          SPLIT <fs_data_tab2> AT lc_fsep INTO:  wa_bconv_split-matnr
                                                 wa_bconv_split-bwkey
                                                 wa_bconv_split-bwtar
                                                 wa_bconv_split-meins
                                                 wa_bconv_split-b_lbkum.
*                                                 wa_bconv_split-remarks.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = wa_bconv_split-matnr
            IMPORTING
              output       = wa_bconv_split-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
          IF sy-subrc IS NOT INITIAL.
*            No Action
          ENDIF.
          DATA(wa_split) = VALUE ty_split( matnr = wa_bconv_split-matnr
                                           bwkey = |{ wa_bconv_split-bwkey ALPHA = IN }|
                                           bwtar = |{ wa_bconv_split-bwtar ALPHA = IN }|
                                           meins = wa_bconv_split-meins
                                           b_lbkum = wa_bconv_split-b_lbkum ).
          APPEND wa_split TO gt_split.
        ENDIF.  "IF sy-tabix GT 1
      ENDLOOP.
    ENDIF. "read file sy-subrc check
  ENDIF. "IF cb_split IS NOT INITIAL.

  FREE: lv_file,
        lt_data_tab[],
        wa_bconv_data,
        wa_bconv_split.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_STOCK_RECON
*&---------------------------------------------------------------------*
*& Stock Reconciliation
*&---------------------------------------------------------------------*
FORM sub_stock_recon .
  DATA: lt_output_a TYPE STANDARD TABLE OF ty_output
        INITIAL SIZE 0,
        lt_split    TYPE STANDARD TABLE OF ty_split
        INITIAL SIZE 0.

***********************************************************************
* Batch and Non Batch managed stock reconciliation
***********************************************************************
  SORT gt_output BY werks lgort matnr charg sobkz acc_assign meins.
  SORT gt_output_a BY werks lgort matnr charg sobkz acc_assign meins.

  LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = <fs_output>-matnr
      IMPORTING
        output       = <fs_output>-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc IS NOT INITIAL.
*   No action
    ENDIF.
* reconciliation logic
    READ TABLE gt_output_a ASSIGNING FIELD-SYMBOL(<fs_output_a>)
                         WITH KEY werks = <fs_output>-werks
                                  lgort = <fs_output>-lgort
                                  matnr = <fs_output>-matnr
                                  charg = <fs_output>-charg
                                  sobkz = <fs_output>-sobkz
                                  BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_output_a>-acc_assign = |{ <fs_output_a>-acc_assign ALPHA = IN }|.
      IF <fs_output>-acc_assign EQ <fs_output_a>-acc_assign
        AND <fs_output>-meins EQ <fs_output_a>-meins.
*  ECC data found in S4
*  populate before conversion stock
        <fs_output_a>-b_clabs = <fs_output>-b_clabs.
        <fs_output_a>-b_ceinm = <fs_output>-b_ceinm.
        <fs_output_a>-b_cinsm = <fs_output>-b_cinsm.
        <fs_output_a>-b_cspem = <fs_output>-b_cspem.
        <fs_output_a>-b_cumlm = <fs_output>-b_cumlm.
        <fs_output_a>-b_return = <fs_output>-b_return.
*  Stock Reconciliation
        <fs_output_a>-r_clabs = <fs_output_a>-b_clabs - <fs_output_a>-a_clabs.
        <fs_output_a>-r_ceinm = <fs_output_a>-b_ceinm - <fs_output_a>-a_ceinm.
        <fs_output_a>-r_cinsm = <fs_output_a>-b_cinsm - <fs_output_a>-a_cinsm.
        <fs_output_a>-r_cspem = <fs_output_a>-b_cspem - <fs_output_a>-a_cspem.
        <fs_output_a>-r_cumlm = <fs_output_a>-b_cumlm - <fs_output_a>-a_cumlm.
        <fs_output_a>-r_return = <fs_output_a>-b_return - <fs_output_a>-a_return.

        IF ( <fs_output_a>-b_clabs NE <fs_output_a>-a_clabs
          OR <fs_output_a>-b_ceinm NE <fs_output_a>-a_ceinm
          OR <fs_output_a>-b_cinsm NE <fs_output_a>-a_cinsm
          OR <fs_output_a>-b_cspem NE <fs_output_a>-a_cspem
          OR <fs_output_a>-b_cumlm NE <fs_output_a>-a_cumlm
          OR <fs_output_a>-b_return NE <fs_output_a>-a_return  ).
*  Reconciliation Remarks
          <fs_output_a>-remarks = 'Not OK'(027).
        ELSE.
*  Reconciliation Remarks
          <fs_output_a>-remarks = 'Ok'(028).
        ENDIF.
      ELSE. "not found in S4 for acc_assign and meins
        DATA(wa_output) = VALUE ty_output( werks = <fs_output>-werks
                                           lgort = <fs_output>-lgort
                                           matnr = <fs_output>-matnr
                                           charg = <fs_output>-charg
                                           sobkz = <fs_output>-sobkz
                                           acc_assign = <fs_output>-acc_assign
                                           b_clabs = <fs_output>-b_clabs
                                           b_ceinm = <fs_output>-b_ceinm
                                           b_cinsm = <fs_output>-b_cinsm
                                           b_cspem = <fs_output>-b_cspem
                                           b_cumlm = <fs_output>-b_cumlm
                                           b_return = <fs_output>-b_return
                                           a_clabs = <fs_output>-a_clabs
                                           a_ceinm = <fs_output>-a_ceinm
                                           a_cinsm = <fs_output>-a_cinsm
                                           a_cspem = <fs_output>-a_cspem
                                           a_cumlm = <fs_output>-a_cumlm
                                           a_return = <fs_output>-a_return
                                           r_clabs = <fs_output>-b_clabs
                                           r_ceinm = <fs_output>-b_ceinm
                                           r_cinsm = <fs_output>-b_cinsm
                                           r_cspem = <fs_output>-b_cspem
                                           r_cumlm = <fs_output>-b_cumlm
                                           r_return = <fs_output>-r_return
                                           meins = <fs_output>-meins
                                           remarks = 'Plant/Material does not exist in S4'(088) ).
        APPEND wa_output TO lt_output_a.
      ENDIF.
    ELSE. "not found in s4 at all
      DATA(wa_output2) = VALUE ty_output( werks = <fs_output>-werks
                                         lgort = <fs_output>-lgort
                                         matnr = <fs_output>-matnr
                                         charg = <fs_output>-charg
                                         sobkz = <fs_output>-sobkz
                                         acc_assign = <fs_output>-acc_assign
                                         b_clabs = <fs_output>-b_clabs
                                         b_ceinm = <fs_output>-b_ceinm
                                         b_cinsm = <fs_output>-b_cinsm
                                         b_cspem = <fs_output>-b_cspem
                                         b_cumlm = <fs_output>-b_cumlm
                                         b_return = <fs_output>-b_return
                                         a_clabs = <fs_output>-a_clabs
                                         a_ceinm = <fs_output>-a_ceinm
                                         a_cinsm = <fs_output>-a_cinsm
                                         a_cspem = <fs_output>-a_cspem
                                         a_cumlm = <fs_output>-a_cumlm
                                         a_return = <fs_output>-a_return
                                         r_clabs = <fs_output>-b_clabs
                                         r_ceinm = <fs_output>-b_ceinm
                                         r_cinsm = <fs_output>-b_cinsm
                                         r_cspem = <fs_output>-b_cspem
                                         r_cumlm = <fs_output>-b_cumlm
                                         r_return = <fs_output>-r_return
                                         meins = <fs_output>-meins
                                         remarks = 'Plant/Material does not exist in S4'(088) ).
      APPEND wa_output2 TO lt_output_a.
    ENDIF.
  ENDLOOP.
  APPEND LINES OF lt_output_a TO gt_output_a.
  SORT gt_output_a BY werks lgort matnr charg sobkz acc_assign.


***********************************************************************
* Split valuation stock reconciliation
***********************************************************************
  IF cb_split IS NOT INITIAL.
    SORT gt_split_a BY bwkey matnr bwtar meins.
    SORT gt_split BY bwkey matnr bwtar meins.

    LOOP AT gt_split ASSIGNING FIELD-SYMBOL(<fs_split>).
      READ TABLE gt_split_a ASSIGNING FIELD-SYMBOL(<fs_split_a>)
                           WITH KEY bwkey = <fs_split>-bwkey
                                    matnr = <fs_split>-matnr
                                    bwtar = <fs_split>-bwtar
                                    meins = <fs_split>-meins
                                    BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*  populate before conversion stock
        <fs_split_a>-b_lbkum = <fs_split>-b_lbkum.
*  stock reconciliation
        <fs_split_a>-r_lbkum = <fs_split_a>-a_lbkum - <fs_split_a>-b_lbkum.
*  UoM
        <fs_split_a>-meins = <fs_split>-meins.

        IF <fs_split_a>-b_lbkum NE <fs_split_a>-a_lbkum.
*  Reconciliation Remarks
          <fs_split_a>-remarks = 'Not OK'(027).
        ELSE.
*  Reconciliation Remarks
          <fs_split_a>-remarks = 'OK'(028).
        ENDIF.
      ELSE. "not found in s4
        DATA(wa_split) = VALUE ty_split( matnr = <fs_split>-matnr
                                         bwkey = <fs_split>-bwkey
                                         bwtar = <fs_split>-bwtar
                                         meins = <fs_split>-meins
                                         b_lbkum = <fs_split>-b_lbkum
                                         a_lbkum = <fs_split>-a_lbkum
                                         r_lbkum = <fs_split>-b_lbkum
                                         remarks = 'Plant/Material does not exist in S4'(088) ).
        APPEND wa_split TO lt_split.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF lt_split TO gt_split_a.
    SORT gt_split_a BY bwkey bwtar matnr.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_FILE_UPLOAD
*&---------------------------------------------------------------------*
*& reconciled file upload
*&---------------------------------------------------------------------*
FORM sub_file_upload .
  DATA: lt_file_data     TYPE rsanm_file_table,
        lv_lines_written TYPE i,
        lv_afile         TYPE string.

  DATA: lv_clabs       TYPE char17,
        lv_ceinm       TYPE char17,
        lv_cinsm       TYPE char17,
        lv_cspem       TYPE char17,
        lv_cumln       TYPE char17,
        lv_return      TYPE char17,
        lv_clabs_a     TYPE char17,
        lv_ceinm_a     TYPE char17,
        lv_cinsm_a     TYPE char17,
        lv_cspem_a     TYPE char17,
        lv_cumln_a     TYPE char17,
        lv_return_a    TYPE char17,
        lv_clabs_r     TYPE char17,
        lv_ceinm_r     TYPE char17,
        lv_cinsm_r     TYPE char17,
        lv_cspem_r     TYPE char17,
        lv_cumln_r     TYPE char17,
        lv_return_r    TYPE char17,
        lv_pspnr       TYPE ps_psp_pnr,
        lv_acc_assign  TYPE ps_posid,
        lv_acc_assignc TYPE string.

  CONSTANTS: lc_fsep  TYPE char1 VALUE ',',
             lc_junkv TYPE char1 VALUE '<'.

***********************************************************************
* Batch and Non-Batch Managed
***********************************************************************
  IF gt_output_a IS NOT INITIAL.
***********************************************************************
*   Populate Heading
***********************************************************************
    CONCATENATE space space space
                'Details'(069)
                space space space space space
                'Before Conversion'(070)
                space space space space
                'After Conversion'(071)
                space space space space
                'Reconciliation'(072)
                space
           INTO DATA(wa_file_data)
           SEPARATED BY lc_fsep.
    APPEND wa_file_data TO lt_file_data.
    CLEAR: wa_file_data.
***********************************************************************
*  Populate Sub Heading
***********************************************************************
    CONCATENATE 'Plant'(026)
                'Storage Loc'(073)
                'Material'(074)
                'Batch'(075)
                'Special Stock'(076)
                'A/c Assignment'(077)
                'UOM (BUOM)'(078)
                'Unrestricted Stock'(079)
                'Restricted Stock'(080)
                'Quality Stock'(081)
                'Blocked Stock'(082)
                'Stock In Transit'(083)
                'Returns'(084)
                'Unrestricted Stock'(079)
                'Restricted Stock'(080)
                'Quality Stock'(081)
                'Blocked Stock'(082)
                'Stock In Transit'(083)
                'Returns'(084)
                'Unrestricted Stock'(079)
                'Restricted Stock'(080)
                'Quality Stock'(081)
                'Blocked Stock'(082)
                'Stock In Transit'(083)
                'Returns'(084)
                'Reconciliation'(072)
           INTO wa_file_data SEPARATED BY lc_fsep.
    APPEND wa_file_data TO lt_file_data.
    CLEAR: wa_file_data.
***********************************************************************
*  Populate data lines
***********************************************************************
    LOOP AT gt_output_a ASSIGNING FIELD-SYMBOL(<fs_output>).
      IF <fs_output>-remarks IS INITIAL.
        <fs_output>-r_clabs = <fs_output>-b_clabs - <fs_output>-a_clabs.
        <fs_output>-r_ceinm = <fs_output>-b_ceinm - <fs_output>-a_ceinm.
        <fs_output>-r_cinsm = <fs_output>-b_cinsm - <fs_output>-a_cinsm.
        <fs_output>-r_cspem = <fs_output>-b_cspem - <fs_output>-a_cspem.
        <fs_output>-r_cumlm = <fs_output>-b_cumlm - <fs_output>-a_cumlm.
        <fs_output>-r_return = <fs_output>-b_return - <fs_output>-a_return.
        IF ( <fs_output>-r_clabs IS NOT INITIAL
          OR <fs_output>-r_ceinm IS NOT INITIAL
          OR <fs_output>-r_cinsm IS NOT INITIAL
          OR <fs_output>-r_cspem IS NOT INITIAL
          OR <fs_output>-r_cumlm IS NOT INITIAL
          OR <fs_output>-r_return IS NOT INITIAL ).
          <fs_output>-remarks = 'Not OK'(027).
        ELSE.
          <fs_output>-remarks = 'OK'(028).
        ENDIF.
      ENDIF.
* assign in character type variables
      lv_clabs = <fs_output>-b_clabs.
      lv_ceinm = <fs_output>-b_ceinm.
      lv_cinsm = <fs_output>-b_cinsm.
      lv_cspem = <fs_output>-b_cspem.
      lv_cumln = <fs_output>-b_cumlm.
      lv_return = <fs_output>-b_return.
      lv_clabs_a = <fs_output>-a_clabs.
      lv_ceinm_a = <fs_output>-a_ceinm.
      lv_cinsm_a = <fs_output>-a_cinsm.
      lv_cspem_a = <fs_output>-a_cspem.
      lv_cumln_a = <fs_output>-a_cumlm.
      lv_return_a = <fs_output>-a_return.
      lv_clabs_r = <fs_output>-r_clabs.
      lv_ceinm_r = <fs_output>-r_ceinm.
      lv_cinsm_r = <fs_output>-r_cinsm.
      lv_cspem_r = <fs_output>-r_cspem.
      lv_cumln_r = <fs_output>-r_cumlm.
      lv_return_r = <fs_output>-r_return.
      CLEAR: lv_pspnr.
      lv_pspnr = <fs_output>-acc_assign.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = lv_pspnr
        IMPORTING
          output = lv_acc_assign.
      CLEAR: lv_acc_assignc.
      lv_acc_assignc = lv_acc_assign.
      IF lv_acc_assignc CS lc_junkv.
        CLEAR: lv_acc_assign.
      ENDIF.
      IF lv_acc_assign IS NOT INITIAL.
        <fs_output>-acc_assign = lv_acc_assign.
      ENDIF.

      CONCATENATE <fs_output>-werks
                  <fs_output>-lgort
                  <fs_output>-matnr
                  <fs_output>-charg
                  <fs_output>-sobkz
                  <fs_output>-acc_assign
                  <fs_output>-meins
                  lv_clabs
                  lv_ceinm
                  lv_cinsm
                  lv_cspem
                  lv_cumln
                  lv_return
                  lv_clabs_a
                  lv_ceinm_a
                  lv_cinsm_a
                  lv_cspem_a
                  lv_cumln_a
                  lv_return_a
                  lv_clabs_r
                  lv_ceinm_r
                  lv_cinsm_r
                  lv_cspem_r
                  lv_cumln_r
                  lv_return_r
                  <fs_output>-remarks
             INTO wa_file_data
             SEPARATED BY lc_fsep.

      APPEND wa_file_data TO lt_file_data.
      CLEAR wa_file_data.
      CLEAR: lv_clabs,
             lv_ceinm,
             lv_cinsm,
             lv_cspem,
             lv_cumln,
             lv_return,
             lv_clabs_a,
             lv_ceinm_a,
             lv_cinsm_a,
             lv_cspem_a,
             lv_cumln_a,
             lv_return_a,
             lv_clabs_r,
             lv_ceinm_r,
             lv_cinsm_r,
             lv_cspem_r,
             lv_cumln_r,
             lv_return_r.
    ENDLOOP.
    CLEAR: lv_afile,
           lv_lines_written.
    lv_afile = p_afile.
***********************************************************************
* Upload Filein Application Server
***********************************************************************
    CALL METHOD cl_rsan_ut_appserv_file_writer=>appserver_file_write
      EXPORTING
        i_filename      = lv_afile
        i_overwrite     = 'X'   "(022)
        i_data_tab      = lt_file_data
      IMPORTING
        e_lines_written = lv_lines_written
      EXCEPTIONS
        open_failed     = 1
        write_failed    = 2
        close_failed    = 3
        OTHERS          = 4.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      DATA(wa_mesg) = VALUE ty_mesg( serno = 1
                                     proc  = 'Stock Evaluation'(086)
                                     recno = lv_lines_written
                                     fpath = p_afile ).
      APPEND wa_mesg TO gt_mesg.
    ENDIF.
  ENDIF. "IF gt_output_a IS NOT INITIAL.

***********************************************************************
*  Split Valuation
***********************************************************************
  CLEAR: wa_file_data,
         lt_file_data[].
  IF cb_split IS NOT INITIAL.
    IF gt_split_a[] IS NOT INITIAL.
***********************************************************************
*   Populate Heading
***********************************************************************
      CONCATENATE space
                  'Details'(069)
                  space space
                  'Before Conversion'(070)
                  'After Conversion'(071)
                  'Reconciliation'(072)
                  space
             INTO wa_file_data SEPARATED BY lc_fsep.
      APPEND wa_file_data TO lt_file_data.
      CLEAR: wa_file_data.
***********************************************************************
*  Populate Sub Heading
***********************************************************************
      CONCATENATE 'Material'(074)
                  'Plant'(026)
                  'Valuation Type'(085)
                  'UOM (BUOM)'(078)
                  'Total Valuated Stock'(087)
                  'Total Valuated Stock'(087)
                  'Total Valuated Stock'(087)
                  'Reconciliation'(072)
             INTO wa_file_data SEPARATED BY lc_fsep.
      APPEND wa_file_data TO lt_file_data.
      CLEAR: wa_file_data.
***********************************************************************
*  Populate data lines
***********************************************************************
      LOOP AT gt_split_a ASSIGNING FIELD-SYMBOL(<fs_split>).
        CLEAR: lv_clabs,
               lv_clabs_a,
               lv_clabs_r.
        IF <fs_split>-remarks IS INITIAL.
          <fs_split>-r_lbkum = <fs_split>-b_lbkum - <fs_split>-a_lbkum.
          IF <fs_split>-r_lbkum IS NOT INITIAL.
            <fs_split>-remarks = 'Not OK'(027).
          ELSE.
            <fs_split>-remarks = 'OK'(028).
          ENDIF.
        ENDIF.

        lv_clabs = <fs_split>-b_lbkum.
        lv_clabs_a = <fs_split>-a_lbkum.
        lv_clabs_r = <fs_split>-r_lbkum.

        CONCATENATE <fs_split>-matnr
                    <fs_split>-bwkey
                    <fs_split>-bwtar
                    <fs_split>-meins
                    lv_clabs
                    lv_clabs_a
                    lv_clabs_r
                    <fs_split>-remarks
               INTO wa_file_data SEPARATED BY lc_fsep.

        APPEND wa_file_data TO lt_file_data.
        CLEAR: wa_file_data.
      ENDLOOP.

      CLEAR: lv_afile,
             lv_lines_written.
      lv_afile = p_asplit.
***********************************************************************
* Upload Filein Application Server
***********************************************************************
      CALL METHOD cl_rsan_ut_appserv_file_writer=>appserver_file_write
        EXPORTING
          i_filename      = lv_afile
          i_overwrite     = 'X'       "(022)
          i_data_tab      = lt_file_data
        IMPORTING
          e_lines_written = lv_lines_written
        EXCEPTIONS
          open_failed     = 1
          write_failed    = 2
          close_failed    = 3
          OTHERS          = 4.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        DATA(wa_mesg2) = VALUE ty_mesg( serno = 1
                                        proc  = 'Stock Evaluation'(086)
                                        recno = lv_lines_written
                                        fpath = p_asplit ).
        APPEND wa_mesg2 TO gt_mesg.
      ENDIF.

    ENDIF. "IF gt_split_a[] IS NOT INITIAL.
  ENDIF. "IF cb_split is not INITIAL.

* Free local memory
  FREE: lt_file_data[],
        lv_lines_written,
        lv_afile,
        lv_clabs,
        lv_ceinm,
        lv_cinsm,
        lv_cspem,
        lv_cumln,
        lv_clabs_a,
        lv_ceinm_a,
        lv_cinsm_a,
        lv_cspem_a,
        lv_cumln_a,
        lv_clabs_r,
        lv_ceinm_r,
        lv_cinsm_r,
        lv_cspem_r,
        lv_cumln_r.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_RECON_OUTPUT
*&---------------------------------------------------------------------*
*& display output
*&---------------------------------------------------------------------*
FORM sub_recon_output .

  DATA: li_sel_tab   TYPE STANDARD TABLE OF rsparams,      " ABAP: General Structure for PARAMETERS and SELECT-OPTIONS
        wa_sel_tab   TYPE rsparams,
        lo_log       TYPE REF TO cl_salv_table,            " Basis Class for Simple Tables
        lx_msg       TYPE REF TO cx_salv_msg,              " ALV: General Error Class with Message
        lx_msg1      TYPE REF TO cx_salv_not_found,        " ALV: General Error Class (Checked During Syntax Check)
        lo_cols      TYPE REF TO cl_salv_columns,          " All Column Objects
        lo_col       TYPE REF TO cl_salv_column,           " Individual Column Object
        lo_functions TYPE REF TO cl_salv_functions_list,   " Generic and User-Defined Functions in List-Type Tables
        lo_header    TYPE REF TO cl_salv_form_layout_grid, " Grid Element in Design Object
        lo_h_label   TYPE REF TO cl_salv_form_label,       " Element of Type Label
        l_row_no     TYPE syindex,                         " Loop Index
        l_header     TYPE char50, "char30,                          " Header of type CHAR30
        l_selval     TYPE char50, "char30,
        l_ctr        TYPE i,
        l_counter    TYPE char1.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_log
        CHANGING
          t_table      = gt_mesg.
    CATCH cx_salv_msg INTO lx_msg.
  ENDTRY.

* Set the column headers
  TRY.
      lo_cols = lo_log->get_columns( ).
      lo_col ?= lo_cols->get_column( 'SERNO'(030) ).
      lo_col->set_medium_text( 'No.'(029) ).
      lo_col->set_long_text( 'No.'(029) ).
    CATCH cx_salv_not_found INTO lx_msg1.
  ENDTRY.

  TRY.
      lo_cols = lo_log->get_columns( ).
      lo_col ?= lo_cols->get_column( 'PROC'(031) ).
      lo_col->set_medium_text( 'Process'(032) ).
      lo_col->set_long_text( 'Process'(032) ).
    CATCH cx_salv_not_found INTO lx_msg1.
  ENDTRY.

  TRY.
      lo_cols = lo_log->get_columns( ).
      lo_col ?= lo_cols->get_column( 'RECNO'(033) ).
      lo_col->set_medium_text( 'No. of records'(034) ).
      lo_col->set_long_text( 'No. of records'(034) ).
    CATCH cx_salv_not_found INTO lx_msg1.
  ENDTRY.

  TRY.
      lo_cols = lo_log->get_columns( ).
      lo_col ?= lo_cols->get_column( 'FPATH'(035) ).
      lo_col->set_medium_text( 'Appl server path'(036) ).
      lo_col->set_long_text( 'Appl server path'(036) ).
    CATCH cx_salv_not_found INTO lx_msg1.
  ENDTRY.

*--Add Header to the ALV output
  CREATE OBJECT lo_header.

  CLEAR l_row_no.

  l_row_no = l_row_no + 1.
  l_header = 'Date of run:'(037).
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
  lo_h_label->set_text( sy-datum ).

  CLEAR: l_header.
  l_row_no = l_row_no + 1.
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  l_row_no = l_row_no + 1.
  l_header = 'Time of run:'(038).
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
  lo_h_label->set_text( sy-uzeit ).

  CLEAR: l_header.
  l_row_no = l_row_no + 1.
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  l_row_no = l_row_no + 1.
  l_header = 'User:'(039).
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
  lo_h_label->set_text( sy-uname ).

  CLEAR: l_header.
  l_row_no = l_row_no + 1.
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  l_row_no = l_row_no + 1.
  l_header = 'Selection Screen Parameters:'(040).
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  CLEAR: l_header.
  l_row_no = l_row_no + 1.
  lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
  lo_h_label->set_text( l_header ).

  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-repid
    TABLES
      selection_table = li_sel_tab
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    CLEAR li_sel_tab.
  ELSE.
    IF rb_bconv IS NOT INITIAL.
      l_header = 'Before Conversion run for:'(041).
    ELSEIF rb_aconv IS NOT INITIAL.
      l_header = 'After Conversion run for:'(042).
    ENDIF.

    l_row_no = l_row_no + 1.
    lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
    lo_h_label->set_text( l_header ).
    CLEAR l_header.
***********************************************************************************
*  Blank Line
***********************************************************************************
    l_row_no = l_row_no + 1.
    lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
    lo_h_label->set_text( l_header ).

    IF l_selval IS NOT INITIAL.
      lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
      lo_h_label->set_text( l_selval ).
    ENDIF.

    LOOP AT li_sel_tab INTO wa_sel_tab.
      IF wa_sel_tab-low IS NOT INITIAL.
        CASE wa_sel_tab-selname.
          WHEN 'RB_IM'(043).
            l_header = 'Stock managed in Plant (IM)'(044).
          WHEN 'RB_EWM'(045).
            l_header = 'EWM-Stock managed in Warehouse'(046).
          WHEN 'RB_WM'(047).
            l_header = 'WM-Warehouse w/o Pallet info'(048).
          WHEN 'RB_WMHU'(049).
            l_header = 'WM-Warehouse w/ Pallet info'(050).
          WHEN 'CB_BATCH'(051).
            l_header = 'Batch valuation'(052).
          WHEN 'CB_SPLIT'(053).
            l_header = 'Split valuation'(054).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        l_row_no = l_row_no + 1.
        l_ctr = l_ctr + 1.
        l_counter = l_ctr.
        CONCATENATE l_counter '. ' l_header INTO l_header.
        lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
        lo_h_label->set_text( l_header ).

        CLEAR: l_header.
      ENDIF.
    ENDLOOP.

    CLEAR l_ctr.

***********************************************************************************
*  Blank Line
***********************************************************************************
    l_row_no = l_row_no + 1.
    lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
    lo_h_label->set_text( l_header ).

    IF l_selval IS NOT INITIAL.
      lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
      lo_h_label->set_text( l_selval ).
    ENDIF.
***********************************************************************************
*  Before Conversion files
***********************************************************************************
    LOOP AT li_sel_tab INTO wa_sel_tab.
      IF wa_sel_tab-low IS NOT INITIAL.
        CASE wa_sel_tab-selname.
          WHEN 'P_BFILE'(008).
            l_header = 'Before conversion filepath: '(056).
            l_selval = p_bfile.
          WHEN 'P_BSPLIT'(057).
            l_header = 'Before conversion Split valuation filepath: '(058).
            l_selval = p_bsplit.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        l_row_no = l_row_no + 1.
        lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
        lo_h_label->set_text( l_header ).

        IF l_selval IS NOT INITIAL.
          lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
          lo_h_label->set_text( l_selval ).
        ENDIF.

        CLEAR: l_header, l_selval.
      ENDIF.
    ENDLOOP.

***********************************************************************************
*  Blank Line
***********************************************************************************
    l_row_no = l_row_no + 1.
    lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
    lo_h_label->set_text( l_header ).

    IF l_selval IS NOT INITIAL.
      lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
      lo_h_label->set_text( l_selval ).
    ENDIF.
***********************************************************************************
    LOOP AT li_sel_tab INTO wa_sel_tab.
      IF wa_sel_tab-low IS NOT INITIAL.
        CASE wa_sel_tab-selname.
*          WHEN 'P_BFILE'(008).
*            l_header = 'Before conversion filepath: '(056).
*            l_selval = p_bfile.
          WHEN 'P_AFILE'(006).
            l_header = 'After conversion filepath: '(055).
            l_selval = p_afile.
*          WHEN 'P_BSPLIT'(057).
*            l_header = 'Before conversion Split valuation filepath: '(058).
*            l_selval = p_bsplit.
          WHEN 'P_ASPLIT'(059).
            l_header = 'After conversion Split valuation filepath: '(060).
            l_selval = p_asplit.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.

        l_row_no = l_row_no + 1.
        lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
        lo_h_label->set_text( l_header ).

        IF l_selval IS NOT INITIAL.
          lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
          lo_h_label->set_text( l_selval ).
        ENDIF.

        CLEAR: l_header, l_selval.
      ENDIF.
    ENDLOOP.

    LOOP AT li_sel_tab INTO wa_sel_tab.
      IF wa_sel_tab-low IS NOT INITIAL.
        CASE wa_sel_tab-selname.
          WHEN 'S_WERKS'(061).
            l_header = 'Plant'(026).
          WHEN 'S_LGORT'(062).
            l_header = 'Storage Location'(063).
          WHEN 'S_LGNUM'(064).
            l_header = 'Warehouse'(065).
          WHEN 'S_DATUM'(066).
            l_header = 'Conversion Date'(067).
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ENDIF.

      IF wa_sel_tab-selname = 'S_WERKS'(061) OR
           wa_sel_tab-selname = 'S_LGORT'(062) OR
           wa_sel_tab-selname = 'S_LGNUM'(064) OR
           wa_sel_tab-selname = 'S_DATUM'(066).
        IF wa_sel_tab-high IS INITIAL.
          l_selval = wa_sel_tab-low.
        ELSE.
          CONCATENATE wa_sel_tab-low 'to'(068) wa_sel_tab-high
                 INTO l_selval.
        ENDIF.
      ENDIF.

      l_row_no = l_row_no + 1.
      lo_h_label = lo_header->create_label( row = l_row_no column = 1 ).
      lo_h_label->set_text( l_header ).

      IF l_selval IS NOT INITIAL.
        lo_h_label = lo_header->create_label( row = l_row_no column = 2 ).
        lo_h_label->set_text( l_selval ).
      ENDIF.

      CLEAR: l_header, l_selval.
    ENDLOOP.
  ENDIF.

*--Set the top of list using the header for Online
  lo_log->set_top_of_list( lo_header ).
*--Set the top of list using the header for Print
  lo_log->set_top_of_list_print( lo_header ).
*--Get ALV Functions
  lo_functions = lo_log->get_functions( ).
*--Set Defaults for ALV Toolbar
  lo_functions->set_default( abap_true ).
*--Display ALV
  lo_log->display( ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_FILE_EXT_CHK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_BFILE
*&---------------------------------------------------------------------*
FORM sub_file_ext_chk  USING    p_file TYPE localfile.

  DATA: lv_ext   TYPE char10.
*        lv_fname TYPE string.
  CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
    EXPORTING
      filename  = p_file
      uppercase = 'X'
    IMPORTING
      extension = lv_ext.
  IF sy-subrc IS INITIAL
    AND lv_ext NE 'CSV'(015).
    MESSAGE 'Only .CSV file format allowed'(016) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
    LEAVE LIST-PROCESSING.
  ENDIF.
  DATA(lv_fname) = p_file.
  CONDENSE lv_fname NO-GAPS.
  IF lv_fname NE p_file.
    MESSAGE 'Remove spaces from filename'(017) TYPE 'S'(013) DISPLAY LIKE 'E'(014).
    LEAVE LIST-PROCESSING.
  ENDIF.

  FREE: lv_ext,
        lv_fname.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUB_GET_DATA_S4
*&---------------------------------------------------------------------*
*& Get S4 data
*&---------------------------------------------------------------------*
FORM sub_get_data_s4 .
  DATA: lt_output_a TYPE STANDARD TABLE OF ty_output
        INITIAL SIZE 0,
        lt_master   TYPE STANDARD TABLE OF ty_output
        INITIAL SIZE 0,
        lt_mchb     TYPE STANDARD TABLE OF ty_output
        INITIAL SIZE 0,
        lt_mstb     TYPE STANDARD TABLE OF ty_output
        INITIAL SIZE 0.
  DATA: lv_clabs  TYPE labst,
        lv_ceinm  TYPE einme,
        lv_cinsm  TYPE insme,
        lv_cspem  TYPE speme,
        lv_cumlm  TYPE trame,
        lv_return TYPE retme,
        lv_no_val TYPE bwtar_d.

  DATA(lv_fname) = p_afile.
*
*  IF  lv_fname CS 'MD'
*    OR lv_fname CS 'md'.
***********************************************************************
*   MATDOC Logic
***********************************************************************
  IF cb_batch IS NOT INITIAL.
    SELECT zstockmatdoc~werks AS werks,
       zstockmatdoc~lgort AS lgort,
       zstockmatdoc~matnr AS matnr,
       zstockmatdoc~charg AS charg,
       zstockmatdoc~sobkz AS sobkz,
       zstockmatdoc~acc_assign AS acc_assign,
       zstockmatdoc~meins AS meins,
       0 AS b_clabs,
       0 AS b_ceinm,
       0 AS b_cinsm,
       0 AS b_cspem,
       0 AS b_cumlm,
       0 AS b_return,
       zstockmatdoc~clabs AS a_clabs,
       zstockmatdoc~ceinm AS a_ceinm,
       zstockmatdoc~cinsm AS a_cinsm,
       zstockmatdoc~cspem AS a_cspem,
       zstockmatdoc~cumlm AS a_cumlm,
       zstockmatdoc~a_return AS a_return
  FROM zstockmatdoc AS zstockmatdoc
  INTO TABLE @gt_output_a
  WHERE
*     zstockmatdoc~mandt = @sy-mandt
    zstockmatdoc~werks IN @s_werks
   AND zstockmatdoc~lgort IN @s_lgort.

  ELSE.
    SELECT zstockmatdoc~werks AS werks,
      zstockmatdoc~lgort AS lgort,
      zstockmatdoc~matnr AS matnr,
      zstockmatdoc~charg AS charg,
      zstockmatdoc~sobkz AS sobkz,
      zstockmatdoc~acc_assign AS acc_assign,
      zstockmatdoc~meins AS meins,
      0 AS b_clabs,
      0 AS b_ceinm,
      0 AS b_cinsm,
      0 AS b_cspem,
      0 AS b_cumlm,
      0 AS b_return,
      zstockmatdoc~clabs AS a_clabs,
      zstockmatdoc~ceinm AS a_ceinm,
      zstockmatdoc~cinsm AS a_cinsm,
      zstockmatdoc~cspem AS a_cspem,
      zstockmatdoc~cumlm AS a_cumlm,
      zstockmatdoc~a_return AS a_return
 FROM zstockmatdoc AS zstockmatdoc
 INTO TABLE @gt_output_a
 WHERE
*       zstockmatdoc~mandt = @sy-mandt
   zstockmatdoc~werks IN @s_werks
  AND zstockmatdoc~lgort IN @s_lgort
  AND zstockmatdoc~charg EQ @space.

  ENDIF. " IF cb_batch IS NOT INITIAL.
*
*
*  ELSEIF  lv_fname CS 'CP'
*    OR lv_fname CS 'cp'.
************************************************************************
**    COmpatibility View Logic - Temporary Logic
*************************************************************************
************************************************************************
** Get Project Stock
************************************************************************
*    SELECT mspr~werks AS werks,
*           mspr~lgort AS lgort,
*           mspr~matnr AS matnr,
*           mspr~charg AS charg,
*           mspr~sobkz AS sobkz,
*           mspr~pspnr AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mspr~prlab ) AS a_clabs,
*           SUM( mspr~prein ) AS a_ceinm,
*           SUM( mspr~prins ) AS a_cinsm,
*           SUM( mspr~prspe ) AS a_cspem
*      FROM nsdm_v_mspr AS mspr
*      INNER JOIN mara AS mara
*      ON mspr~matnr = mara~matnr
*      APPENDING TABLE @lt_output_a
*      WHERE mspr~werks IN @s_werks
*        AND mspr~lgort IN @s_lgort
*      GROUP BY mspr~werks, mspr~lgort, mspr~matnr,
*               mspr~charg, mspr~sobkz, mspr~pspnr, mara~meins.
*
************************************************************************
** Get Sales Order Stock data
************************************************************************
*    SELECT mska~werks AS werks,
*           mska~lgort AS lgort,
*           mska~matnr AS matnr,
*           mska~charg AS charg,
*           mska~sobkz AS sobkz,
*           mska~vbeln AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mska~kalab ) AS a_clabs,
*           SUM( mska~kaein ) AS a_ceinm,
*           SUM( mska~kains ) AS a_cinsm,
*           SUM( mska~kaspe ) AS a_cspem
*      FROM nsdm_v_mska AS mska
*      INNER JOIN mara AS mara
*      ON mska~matnr = mara~matnr
*      APPENDING TABLE @lt_output_a
*      WHERE mska~werks IN @s_werks
*        AND mska~lgort IN @s_lgort
*      GROUP BY mska~werks, mska~lgort, mska~matnr,
*               mska~charg, mska~sobkz, mska~vbeln, mara~meins.
*
************************************************************************
** Get Special Stocks with Supplier data
************************************************************************
*    SELECT mslb~werks AS werks,
*           ' ' AS lgort,
*           mslb~matnr AS matnr,
*           mslb~charg AS charg,
*           mslb~sobkz AS sobkz,
*           mslb~lifnr AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mslb~lblab ) AS a_clabs,
*           SUM( mslb~lbein ) AS a_ceinm,
*           SUM( mslb~lbins ) AS a_cinsm
*       FROM nsdm_v_mslb AS mslb
*       INNER JOIN mara AS mara
*       ON mslb~matnr = mara~matnr
*       APPENDING TABLE @lt_output_a
*       WHERE mslb~werks IN @s_werks
*       GROUP BY mslb~werks, mslb~matnr,  mslb~charg, mslb~sobkz,
*                mslb~lifnr, mara~meins.
*
************************************************************************
*
************************************************************************
** Get Special Stocks with Customer data
************************************************************************
*    SELECT msku~werks AS werks,
*           ' ' AS lgort,
*           msku~matnr AS matnr,
*           msku~charg AS charg,
*           msku~sobkz AS sobkz,
*           msku~kunnr AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( msku~kulab ) AS a_clabs,
*           SUM( msku~kuein ) AS a_ceinm,
*           SUM( msku~kuins ) AS a_cinsm
*      FROM nsdm_v_msku AS msku
*      INNER JOIN mara AS mara
*      ON msku~matnr = mara~matnr
*       APPENDING TABLE @lt_output_a
*      WHERE msku~werks IN @s_werks
*      GROUP BY msku~werks, msku~matnr, msku~charg,
*               msku~sobkz, msku~kunnr, mara~meins.
************************************************************************
**  Get Special Stocks from Supplier data
************************************************************************
*    SELECT mkol~werks AS werks,
*           mkol~lgort AS lgort,
*           mkol~matnr AS matnr,
*           mkol~charg AS charg,
*           mkol~sobkz AS sobkz,
*           mkol~lifnr AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mkol~slabs ) AS a_clabs,
*           SUM( mkol~seinm ) AS a_ceinm,
*           SUM( mkol~sinsm ) AS a_cinsm,
*           SUM( mkol~sspem ) AS a_cspem
*      FROM nsdm_v_mkol AS mkol
*      INNER JOIN mara AS mara
*      ON mkol~matnr = mara~matnr
*       APPENDING TABLE @lt_output_a
*      WHERE mkol~werks IN @s_werks
*        AND mkol~lgort IN @s_lgort
*        AND mkol~lvorm = @space
*      GROUP BY mkol~werks, mkol~lgort, mkol~matnr,
*               mkol~charg, mkol~sobkz, mkol~lifnr, mara~meins.
************************************************************************
** Get Non Batch managed Stock data
************************************************************************
*    SELECT mard~werks AS werks,
*           mard~lgort AS lgort,
*           mard~matnr AS matnr,
*           ' ' AS charg,
*           ' ' AS sobkz,
*           ' ' AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mard~labst ) AS a_clabs,
*           SUM( mard~einme ) AS a_ceinm,
*           SUM( mard~insme ) AS a_cinsm,
*           SUM( mard~speme ) AS a_cspem
*       FROM nsdm_v_mard AS mard
*       INNER JOIN mara AS mara
*       ON mard~matnr = mara~matnr
*       APPENDING TABLE @lt_output_a
*       WHERE mard~werks IN @s_werks
*         AND mard~lgort IN @s_lgort
*         AND mard~lvorm = @space
*      GROUP BY mard~werks, mard~lgort, mard~matnr, mara~meins.
*
************************************************************************
*
*
*
************************************************************************
** MSSA - Get stock data for Total Customer Orders on Hand MSSA
************************************************************************
*    SELECT mssa~werks AS werks,
*           ' ' AS lgort,
*           mssa~matnr AS matnr,
*           ' ' AS charg,
*           mssa~sobkz AS sobkz,
*           mssa~vbeln AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           SUM( mssa~salab - mska~kalab ) AS a_clabs,
*           SUM( mssa~saein - mska~kaein ) AS a_ceinm,
*           SUM( mssa~sains - mska~kains ) AS a_cinsm,
*           SUM( mssa~saspe - mska~kaspe ) AS a_cspem
*      FROM nsdm_v_mssa AS mssa
*      INNER JOIN mara AS mara
*      ON mssa~matnr = mara~matnr
*      LEFT OUTER JOIN nsdm_v_mska AS mska
*       ON mssa~matnr = mska~matnr
*      AND mssa~werks = mska~werks
*      AND mssa~sobkz = mska~sobkz
*      AND mssa~vbeln = mska~vbeln
*    APPENDING TABLE @lt_master  "@DATA(lt_mssa)
*      WHERE mssa~werks IN @s_werks
*      GROUP BY mssa~werks, mssa~matnr, mssa~sobkz, mssa~vbeln, mara~meins.
*
************************************************************************
** MSSL Total Special Stocks with Supplier
************************************************************************
*    SELECT  mssl~werks AS werks,
*            ' ' AS lgort,
*            mssl~matnr AS matnr,
*            ' '  AS charg,
*             mssl~sobkz AS sobkz,
*             mssl~lifnr AS acc_assign,
*             mara~meins AS meins,
*             0 AS b_clabs,
*             0 AS b_ceinm,
*             0 AS b_cinsm,
*             0 AS b_cspem,
*             0 AS b_cumlm,
*             0 AS b_return,
*             SUM( mssl~sllab - mslb~lblab ) AS a_clabs,
*             SUM( mssl~slein - mslb~lbein ) AS a_ceinm,
*             SUM( mssl~slins - mslb~lbins ) AS a_cinsm
*        FROM nsdm_v_mssl AS mssl
*        INNER JOIN mara AS mara
*        ON mssl~matnr = mara~matnr
*        LEFT OUTER JOIN nsdm_v_mslb AS mslb
*         ON mssl~werks = mslb~werks
*        AND mssl~matnr = mslb~matnr
*        AND mssl~sobkz = mslb~sobkz
*        AND mssl~lifnr = mslb~lifnr
*    APPENDING TABLE @lt_master  "@DATA(lt_mssl)
*        WHERE mssl~werks IN @s_werks
*        GROUP BY mssl~werks, mssl~matnr, mssl~sobkz, mssl~lifnr, mara~meins.
*
************************************************************************
**MSSQ - Get Project-Total stock
************************************************************************
*    SELECT   mssq~werks AS werks,
*             ' ' AS lgort,
*             mssq~matnr AS matnr,
*             ' ' AS charg,
*              mssq~sobkz AS sobkz,
*              mssq~pspnr AS acc_assign,
*              mara~meins AS meins,
*              0 AS b_clabs,
*              0 AS b_ceinm,
*              0 AS b_cinsm,
*              0 AS b_cspem,
*              0 AS b_cumlm,
*              0 AS b_return,
*              SUM( mssq~sqlab - mspr~prlab ) AS a_clabs,
*              SUM( mssq~sqein - mspr~prein ) AS a_ceinm,
*              SUM( mssq~sqins - mspr~prins ) AS a_cinsm,
*              SUM( mssq~sqspe - mspr~prspe ) AS a_cspem
*         FROM nsdm_v_mssq AS mssq
*         INNER JOIN mara AS mara
*         ON mssq~matnr = mara~matnr
*         LEFT OUTER JOIN nsdm_v_mspr AS mspr
*          ON mssq~werks = mspr~werks
*         AND mssq~matnr = mspr~matnr
*         AND mssq~sobkz = mspr~sobkz
*         AND mssq~pspnr = mspr~pspnr
*    APPENDING TABLE @lt_master  "@DATA(lt_mssq)
*         WHERE mssq~werks IN @s_werks
*         GROUP BY mssq~werks, mssq~matnr, mssq~sobkz, mssq~pspnr, mara~meins.
*
************************************************************************
** MCHB - Get data relevant to Batch managed stock
** if cheked in selection screen
************************************************************************
*    IF cb_batch IS NOT INITIAL.
*      SELECT  mchb~werks AS werks,
*              mchb~lgort AS lgort,
*              mchb~matnr AS matnr,
*              mchb~charg AS charg,
*              ' ' AS sobkz,
*              ' ' AS acc_assign,
*              mara~meins AS meins,
*              0 AS b_clabs,
*              0 AS b_ceinm,
*              0 AS b_cinsm,
*              0 AS b_cspem,
*              0 AS b_cumlm,
*              0 AS b_return,
*              SUM( mchb~clabs - mska~kalab - mkol~slabs - mspr~prlab ) AS a_clabs,
*              SUM( mchb~ceinm - mska~kaein - mkol~seinm - mspr~prein ) AS a_ceinm,
*              SUM( mchb~cinsm - mska~kains - mkol~sinsm - mspr~prins ) AS a_cinsm,
*              SUM( mchb~cspem - mska~kaspe - mkol~sspem - mspr~prspe ) AS a_cspem
*         FROM nsdm_v_mchb AS mchb
*         INNER JOIN mara AS mara
*         ON mchb~matnr = mara~matnr
*         LEFT OUTER JOIN nsdm_v_mska AS mska
*          ON mchb~werks = mska~werks
*         AND mchb~lgort = mska~lgort
*         AND mchb~matnr = mska~matnr
*         AND mchb~charg = mska~charg
*         LEFT OUTER JOIN nsdm_v_mkol AS mkol
*          ON mchb~werks = mkol~werks
*         AND mchb~lgort = mkol~lgort
*         AND mchb~matnr = mkol~matnr
*         AND mchb~charg = mkol~charg
*         LEFT OUTER JOIN nsdm_v_mspr AS mspr
*          ON mchb~werks = mspr~werks
*         AND mchb~lgort = mspr~lgort
*         AND mchb~matnr = mspr~matnr
*         AND mchb~charg = mspr~charg
*    APPENDING TABLE @lt_mchb  "@DATA(lt_mchb)
*         WHERE mchb~werks IN @s_werks
*           AND mchb~lgort IN @s_lgort
*           AND mchb~lvorm = @space
*         GROUP BY mchb~werks, mchb~lgort, mchb~matnr, mchb~charg, mara~meins.
*    ENDIF. "IF cb_batch IS NOT INITIAL.
*
************************************************************************
** Get Stock in Transit for Project data from WBS
************************************************************************
*    SELECT mstq~werks AS werks,
*           ' ' AS lgort,
*           mstq~matnr AS matnr,
*           mstq~charg AS charg,
*           mstq~sobkz AS sobkz,
*           mstq~pspnr AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           0 AS a_clabs,
*           0 AS a_ceinm,
*           0 AS a_cinsm,
*           0 AS a_cspem,
*           SUM( mstq~qwesb ) AS a_cumlm
*      FROM nsdm_v_mstq AS mstq
*      INNER JOIN mara AS mara
*      ON mstq~matnr = mara~matnr
*      APPENDING TABLE @lt_output_a
*      WHERE mstq~werks IN @s_werks
*      GROUP BY mstq~werks, mstq~matnr,
*               mstq~charg, mstq~sobkz, mstq~pspnr, mara~meins.
************************************************************************
** Get Stock in Transit to Sales and Distribution Document data
************************************************************************
*    SELECT mste~werks AS werks,
*           ' ' AS lgort,
*           mste~matnr AS matnr,
*           mste~charg AS charg,
*           mste~sobkz AS sobkz,
*           mste~vbeln AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           0 AS a_clabs,
*           0 AS a_ceinm,
*           0 AS a_cinsm,
*           0 AS a_cspem,
*           SUM( mste~ewesb ) AS a_cumlm
*      FROM nsdm_v_mste AS mste
*      INNER JOIN mara AS mara
*      ON mste~matnr = mara~matnr
*      APPENDING TABLE @lt_output_a
*      WHERE mste~werks IN @s_werks
*      GROUP BY mste~werks, mste~matnr,
*               mste~charg, mste~sobkz, mste~vbeln, mara~meins.
************************************************************************
**      MSTB
************************************************************************
*    SELECT mstb~werks AS werks,
*           ' ' AS lgort,
*           mstb~matnr AS matnr,
*           mstb~charg AS charg,
*           mstb~sobkz AS sobkz,
*           ' ' AS acc_assign,
*           mara~meins AS meins,
*           0 AS b_clabs,
*           0 AS b_ceinm,
*           0 AS b_cinsm,
*           0 AS b_cspem,
*           0 AS b_cumlm,
*           0 AS b_return,
*           0 AS a_clabs,
*           0 AS a_ceinm,
*           0 AS a_cinsm,
*           0 AS a_cspem,
*       SUM( mstb~cwesb - mstq~qwesb - mste~ewesb ) AS a_cumlm
*    FROM nsdm_v_mstb AS mstb
*    INNER JOIN mara AS mara
*    ON mstb~matnr = mara~matnr
*    LEFT OUTER JOIN nsdm_v_mstq AS mstq
*     ON mstb~werks = mstq~werks
*    AND mstb~matnr = mstq~matnr
*    AND mstb~charg = mstq~charg
*    AND mstb~sobkz = mstq~sobkz
*    AND mstb~bwtar = mstq~bwtar
*    LEFT OUTER JOIN nsdm_v_mste AS mste
*     ON mstb~werks = mste~werks
*    AND mstb~matnr = mste~matnr
*    AND mstb~charg = mste~charg
*    AND mstb~sobkz = mste~sobkz
*    AND mstb~bwtar = mste~bwtar
*    APPENDING TABLE @lt_mstb  "@DATA(lt_mstb)
*  WHERE mste~werks IN @s_werks
*  GROUP BY mstb~werks, mstb~matnr,
*           mstb~charg, mstb~sobkz, mara~meins.
*
************************************************************************
** Summing up lines based on plant, storage Location, Material, Batch,
** Special Stock Indicator, Account Assignment, UoM
************************************************************************
*    CLEAR: gt_output_a[],
*           lv_clabs,
*           lv_ceinm,
*           lv_cinsm,
*           lv_cspem,
*           lv_cumlm,
*           lv_return.
*    SORT lt_output_a BY werks lgort matnr charg sobkz acc_assign meins.
*    LOOP AT lt_output_a ASSIGNING FIELD-SYMBOL(<fs_output>).
*      lv_clabs  = lv_clabs + <fs_output>-a_clabs.
*      lv_ceinm  = lv_ceinm + <fs_output>-a_ceinm.
*      lv_cinsm  = lv_cinsm + <fs_output>-a_cinsm.
*      lv_cspem  = lv_cspem + <fs_output>-a_cspem.
*      lv_cumlm  = lv_cumlm + <fs_output>-a_cumlm.
*      lv_return = lv_return + <fs_output>-a_return.
*
*      AT END OF meins.
*        DATA(lw_output) = <fs_output>.
*        lw_output-a_clabs  = lv_clabs.
*        lw_output-a_ceinm  = lv_ceinm.
*        lw_output-a_cinsm  = lv_cinsm.
*        lw_output-a_cspem  = lv_cspem.
*        lw_output-a_cumlm  = lv_cumlm.
*        lw_output-a_return = lv_return.
*        APPEND lw_output TO gt_output_a.
*        CLEAR: lw_output,
*               lv_clabs,
*               lv_ceinm,
*               lv_cinsm,
*               lv_cspem,
*               lv_cumlm,
*               lv_return.
*      ENDAT.
*    ENDLOOP.
*    CLEAR: lt_output_a[].
*    lt_output_a[] = gt_output_a[].
*    SORT lt_output_a BY werks matnr sobkz acc_assign meins.
*    SORT lt_master BY werks matnr sobkz acc_assign meins.
*
************************************************************************
**   master tables
************************************************************************
*    LOOP AT lt_master ASSIGNING FIELD-SYMBOL(<fs_master>).
*      DATA(lv_matnr) = <fs_master>-matnr.
*      READ TABLE lt_output_a ASSIGNING FIELD-SYMBOL(<fs_out>)
*                                       WITH KEY werks = <fs_master>-werks
*                                                matnr = <fs_master>-matnr
*                                                sobkz = <fs_master>-sobkz
*                                                acc_assign = <fs_master>-acc_assign
*                                                meins = <fs_master>-meins
*                                                BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        DATA(lv_tabix) = sy-tabix.
*      ELSE.
*        APPEND <fs_master> TO gt_output_a.
*      ENDIF.
*      IF lv_tabix IS NOT INITIAL.
*        LOOP AT lt_output_a ASSIGNING <fs_out> FROM lv_tabix.
*          IF <fs_out>-werks = <fs_master>-werks
*            AND <fs_out>-matnr = <fs_master>-matnr
*            AND <fs_out>-sobkz = <fs_master>-sobkz
*            AND <fs_out>-acc_assign = <fs_master>-acc_assign
*            AND <fs_out>-meins = <fs_master>-meins.
*            <fs_master>-a_clabs = <fs_master>-a_clabs - <fs_out>-a_clabs.
*            <fs_master>-a_ceinm = <fs_master>-a_ceinm - <fs_out>-a_ceinm.
*            <fs_master>-a_cinsm = <fs_master>-a_cinsm - <fs_out>-a_cinsm.
*            <fs_master>-a_cspem = <fs_master>-a_cspem - <fs_out>-a_cspem.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
**  Append to final table for non zero lines
*        IF <fs_master>-a_clabs IS NOT INITIAL
*          OR <fs_master>-a_ceinm IS NOT INITIAL
*          OR <fs_master>-a_cinsm IS NOT INITIAL
*          OR <fs_master>-a_cspem IS NOT INITIAL.
*          APPEND <fs_master> TO gt_output_a.
*        ENDIF.  "non zero qty check
*      ENDIF. "IF lv_tabix IS NOT INITIAL.
*    ENDLOOP.
************************************************************************
** MCHB
************************************************************************
*    SORT lt_mchb BY werks lgort matnr charg meins.
*    SORT lt_output_a BY werks lgort matnr charg meins.
*    LOOP AT lt_mchb ASSIGNING FIELD-SYMBOL(<fs_mchb>).
*      READ TABLE lt_output_a ASSIGNING <fs_out>
*                                       WITH KEY werks = <fs_mchb>-werks
*                                                lgort = <fs_mchb>-lgort
*                                                matnr = <fs_mchb>-matnr
*                                                charg = <fs_mchb>-charg
*                                                meins = <fs_mchb>-meins
*                                                BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        DATA(lv_tabix2) = sy-tabix.
*      ELSE.
*        APPEND <fs_mchb> TO gt_output_a.
*      ENDIF.
*      IF lv_tabix2 IS NOT INITIAL.
*        LOOP AT lt_output_a ASSIGNING <fs_out> FROM lv_tabix2.
*          IF <fs_out>-werks = <fs_mchb>-werks
*            AND <fs_out>-lgort = <fs_mchb>-lgort
*            AND <fs_out>-matnr = <fs_mchb>-matnr
*            AND <fs_out>-charg = <fs_mchb>-charg
*            AND <fs_out>-meins = <fs_mchb>-meins.
*            <fs_mchb>-a_clabs = <fs_mchb>-a_clabs - <fs_out>-a_clabs.
*            <fs_mchb>-a_ceinm = <fs_mchb>-a_ceinm - <fs_out>-a_ceinm.
*            <fs_mchb>-a_cinsm = <fs_mchb>-a_cinsm - <fs_out>-a_cinsm.
*            <fs_mchb>-a_cspem = <fs_mchb>-a_cspem - <fs_out>-a_cspem.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
**  Append to final table for non zero lines
*        IF <fs_mchb>-a_clabs IS NOT INITIAL
*          OR <fs_mchb>-a_ceinm IS NOT INITIAL
*          OR <fs_mchb>-a_cinsm IS NOT INITIAL
*          OR <fs_mchb>-a_cspem IS NOT INITIAL.
*          APPEND <fs_mchb> TO gt_output_a.
*        ENDIF.  "non zero qty check
*      ENDIF. "IF lv_tabix2 IS NOT INITIAL
*    ENDLOOP.
************************************************************************
** MSTB
************************************************************************
*    SORT lt_mstb BY werks matnr charg sobkz meins.
*    SORT lt_output_a BY werks matnr charg sobkz meins.
*    LOOP AT lt_mstb ASSIGNING FIELD-SYMBOL(<fs_mstb>).
*      READ TABLE lt_output_a ASSIGNING <fs_out>
*                                       WITH KEY werks = <fs_mstb>-werks
*                                                matnr = <fs_mstb>-matnr
*                                                charg = <fs_mstb>-charg
*                                                sobkz = <fs_mstb>-sobkz
*                                                meins = <fs_mstb>-meins
*                                                BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        DATA(lv_tabix3) = sy-tabix.
*      ELSE.
*        APPEND <fs_mstb> TO gt_output_a.
*      ENDIF.
*      IF lv_tabix3 IS NOT INITIAL.
*        LOOP AT lt_output_a ASSIGNING <fs_out> FROM lv_tabix3.
*          IF <fs_out>-werks = <fs_mstb>-werks
*            AND <fs_out>-matnr = <fs_mstb>-matnr
*            AND <fs_out>-charg = <fs_mstb>-charg
*            AND <fs_out>-sobkz = <fs_mstb>-sobkz
*            AND <fs_out>-meins = <fs_mstb>-meins.
*            <fs_mstb>-a_cumlm = <fs_mstb>-a_cumlm - <fs_out>-a_cumlm.
*          ELSE.
*            EXIT.
*          ENDIF.
*        ENDLOOP.
**  Append to final table for non zero lines
*        IF <fs_mstb>-a_cumlm IS NOT INITIAL.
*          APPEND <fs_mstb> TO gt_output_a.
*        ENDIF.  "non zero qty check
*      ENDIF. "IF lv_tabix3 IS NOT INITIAL
*    ENDLOOP.
*    SORT gt_output_a BY werks lgort matnr charg sobkz acc_assign meins.
*
*  ENDIF. "cp or md
***********************************************************************
* Get data relevant to Split Valuation if checked in selection screen
***********************************************************************
  IF cb_split IS NOT INITIAL.
    CLEAR: lv_no_val.
    SELECT mbew~matnr AS matnr
           mbew~bwkey AS bwkey
           mbew~bwtar AS bwtar
           mara~meins AS meins
           SUM( mbew~lbkum ) AS a_lbkum
      FROM mbvmbew AS mbew
      INNER JOIN mara AS mara
      ON mbew~matnr = mara~matnr
      INTO CORRESPONDING FIELDS OF TABLE gt_split_a
      WHERE mbew~bwkey IN s_werks
        AND mbew~bwtar NE lv_no_val
        AND mbew~lvorm EQ space
      GROUP BY mbew~bwkey mbew~matnr mbew~bwtar mara~meins
      ORDER BY bwkey matnr bwtar meins.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_data_bconv
*&---------------------------------------------------------------------*
*       Get all the relevent data for before convertion
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_bconv .

*--1) Get data relevant to Split Valuation if checked in selection screen
  IF cb_split IS NOT INITIAL.
    SELECT matnr
           bwkey
           bwtar
           lbkum
      FROM mbew
      INTO TABLE gt_mbew
      WHERE bwkey IN s_werks.
  ENDIF.

*--2) Get data relevant to Batch managed stock if cheked in selection screen
  IF cb_batch IS NOT INITIAL.
    SELECT matnr
           werks
           lgort
           charg
           clabs
           cinsm
           ceinm
           cspem
      FROM mchb
      INTO TABLE gt_mchb
      WHERE werks IN s_werks
        AND lgort IN s_lgort
        AND lvorm = space.
  ENDIF.

*--3) Get Project-Total stock
  SELECT matnr
         werks
         sobkz
         pspnr
         sqlab
         sqins
         sqspe
         sqein
*         sqtra
    FROM mssq
    INTO TABLE gt_mssq
    WHERE werks IN s_werks.

*--4) Get Project Stock
  SELECT matnr
         werks
         lgort
         charg
         sobkz
         pspnr
         prlab
         prins
         prspe
         prein
    FROM mspr
    INTO TABLE gt_mspr
    WHERE werks IN s_werks
      AND lgort IN s_lgort.

*--5)  Get stock data for Total Customer Orders on Hand
  SELECT matnr
         werks
         sobkz
         vbeln
         SUM( salab )
         SUM( sains )
         SUM( saspe )
         SUM( saein )
*         satra
    FROM mssa
    INTO TABLE gt_mssa
    WHERE werks IN s_werks
    GROUP BY matnr werks sobkz vbeln.

*--6)  Get Special Stocks with Supplier data
  SELECT matnr
         werks
         charg
         sobkz
         lifnr
         lblab
         lbins
         lbein
    FROM mslb
    INTO TABLE gt_mslb
    WHERE werks IN s_werks.

*--7)  Get Special Stocks with Customer data
  SELECT matnr
         werks
         charg
         sobkz
         kunnr
         kulab
         kuins
         kuein
    FROM msku
    INTO TABLE gt_msku
    WHERE werks IN s_werks.

*--8)  Get Sales Order Stock data
  SELECT matnr
         werks
         lgort
         charg
         sobkz
         vbeln
         kalab
         kains
         kaspe
         kaein
    FROM mska
    INTO TABLE gt_mska
    WHERE werks IN s_werks
      AND lgort IN s_lgort.

*--9)  Get Special Stocks from Supplier data
  SELECT matnr
         werks
         lgort
         charg
         sobkz
         lifnr
         slabs
         sinsm
         seinm
         sspem
    FROM mkol
    INTO TABLE gt_mkol
    WHERE werks IN s_werks
      AND lgort IN s_lgort
      AND lvorm = space.

*--10)  Get Non Batch managed Stock data
  SELECT matnr
         werks
         lgort
         labst
         insme
         einme
         speme
    FROM mard
    INTO TABLE gt_mard
    WHERE werks IN s_werks
      AND lgort IN s_lgort
      AND lvorm = space.

*--11)  Get Total Special Stocks with Vendor data
  SELECT matnr
         werks
         sobkz
         lifnr
         sllab
         slins
         slein
    FROM mssl
    INTO TABLE gt_mssl
    WHERE werks IN s_werks.

ENDFORM.                    " get_data_bconv
*&---------------------------------------------------------------------*
*&      Form  proc_data_before_conv
*&---------------------------------------------------------------------*
*       Process data before conversion
*----------------------------------------------------------------------*
*  no parameters
*----------------------------------------------------------------------*
FORM proc_data_before_conv .

  DATA: wa_mssq   TYPE ty_mssq,
        wa_mbew   TYPE ty_mbew,
        wa_mspr   TYPE ty_mspr,
        wa_mssa   TYPE ty_mssa,
        wa_mslb   TYPE ty_mslb,
        wa_msku   TYPE ty_msku,
        wa_mska   TYPE ty_mska,
        wa_mkol   TYPE ty_mkol,
        wa_mard   TYPE ty_mard,
        wa_mssl   TYPE ty_mssl,
        wa_mchb   TYPE ty_mchb,
        wa_split  TYPE ty_split,
        wa_output TYPE ty_output.

  DATA: temp_mchb TYPE tt_mchb,
        temp_out  TYPE tt_output.

  FIELD-SYMBOLS: <fs_temp_mchb> TYPE ty_mchb,
                 <fs_temp_mard> TYPE ty_mard.

*----------------------------------------------------------------------*
*  Project Stock
*----------------------------------------------------------------------*
  LOOP AT gt_mspr INTO wa_mspr.
    wa_output-werks = wa_mspr-werks.
    wa_output-lgort = wa_mspr-lgort.
    wa_output-matnr = wa_mspr-matnr.
    wa_output-sobkz = wa_mspr-sobkz.
    wa_output-charg = wa_mspr-charg.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_mspr-pspnr
      IMPORTING
        output = wa_output-acc_assign.
    wa_output-b_clabs = wa_mspr-prlab.
    wa_output-b_ceinm = wa_mspr-prein.
    wa_output-b_cinsm = wa_mspr-prins.
    wa_output-b_cspem = wa_mspr-prspe.

    COLLECT wa_output INTO gt_output.

*   Collect this record from MCHB
    READ TABLE gt_mchb ASSIGNING <fs_temp_mchb>
    WITH KEY werks = wa_mspr-werks
             lgort = wa_mspr-lgort
             matnr = wa_mspr-matnr
             charg = wa_mspr-charg.
    IF sy-subrc IS INITIAL.
      <fs_temp_mchb>-clabs = <fs_temp_mchb>-clabs - wa_mspr-prlab.
      <fs_temp_mchb>-ceinm = <fs_temp_mchb>-ceinm - wa_mspr-prein.
      <fs_temp_mchb>-cinsm = <fs_temp_mchb>-cinsm - wa_mspr-prins.
      <fs_temp_mchb>-cspem = <fs_temp_mchb>-cspem - wa_mspr-prspe.
    ENDIF.

    READ TABLE gt_mard ASSIGNING <fs_temp_mard>
    WITH KEY matnr = wa_mspr-matnr
             werks = wa_mspr-werks
             lgort = wa_mspr-lgort.
    IF sy-subrc IS INITIAL.
      IF cb_batch IS INITIAL. "For non-batch managed
        <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mspr-prlab.
        <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mspr-prein.
        <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mspr-prins.
        <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mspr-prspe.
      ELSE."For batch managed
        IF wa_mspr-charg IS INITIAL.
          <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mspr-prlab.
          <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mspr-prein.
          <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mspr-prins.
          <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mspr-prspe.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wa_mspr, wa_output.
  ENDLOOP.

  LOOP AT gt_mssq INTO wa_mssq.
    wa_output-werks = wa_mssq-werks.
    wa_output-matnr = wa_mssq-matnr.
    wa_output-sobkz = wa_mssq-sobkz.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = wa_mssq-pspnr
      IMPORTING
        output = wa_output-acc_assign.

    wa_output-b_clabs = wa_mssq-sqlab.
    wa_output-b_ceinm = wa_mssq-sqein.
    wa_output-b_cinsm = wa_mssq-sqins.
    wa_output-b_cspem = wa_mssq-sqspe.

    LOOP AT gt_mspr INTO wa_mspr
      WHERE matnr = wa_mssq-matnr AND
         werks = wa_mssq-werks AND
         sobkz = wa_mssq-sobkz AND
         pspnr = wa_mssq-pspnr.

      wa_output-b_clabs = wa_output-b_clabs - wa_mspr-prlab."+ ( wa_mssq-sqlab - wa_mspr-prlab ).
      wa_output-b_ceinm = wa_output-b_ceinm - wa_mspr-prein."+ ( wa_mssq-sqein - wa_mspr-prein ).
      wa_output-b_cinsm = wa_output-b_cinsm - wa_mspr-prins."+ ( wa_mssq-sqins - wa_mspr-prins ).
      wa_output-b_cspem = wa_output-b_cspem - wa_mspr-prspe."+ ( wa_mssq-sqspe - wa_mspr-prspe ).
    ENDLOOP.

    IF wa_output-b_clabs IS NOT INITIAL OR
       wa_output-b_ceinm IS NOT INITIAL OR
       wa_output-b_cinsm IS NOT INITIAL OR
       wa_output-b_cspem IS NOT INITIAL.
      COLLECT wa_output INTO gt_output.
    ENDIF.

    CLEAR: wa_output.

  ENDLOOP.

*----------------------------------------------------------------------*
*  Sales Order Stock
*----------------------------------------------------------------------*
  LOOP AT gt_mska INTO wa_mska.
    wa_output-werks = wa_mska-werks.
    wa_output-matnr = wa_mska-matnr.
    wa_output-sobkz = wa_mska-sobkz.
    wa_output-charg = wa_mska-charg.
    wa_output-lgort = wa_mska-lgort.
    wa_output-acc_assign = wa_mska-vbeln.
    wa_output-b_clabs = wa_mska-kalab.
    wa_output-b_ceinm = wa_mska-kaein.
    wa_output-b_cinsm = wa_mska-kains.
    wa_output-b_cspem = wa_mska-kaspe.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.

*   Collect this record from MCHB
    READ TABLE gt_mchb ASSIGNING <fs_temp_mchb>
    WITH KEY werks = wa_mska-werks
             lgort = wa_mska-lgort
             matnr = wa_mska-matnr
             charg = wa_mska-charg.
    IF sy-subrc IS INITIAL.
      <fs_temp_mchb>-clabs = <fs_temp_mchb>-clabs - wa_mska-kalab.
      <fs_temp_mchb>-ceinm = <fs_temp_mchb>-ceinm - wa_mska-kaein.
      <fs_temp_mchb>-cinsm = <fs_temp_mchb>-cinsm - wa_mska-kains.
      <fs_temp_mchb>-cspem = <fs_temp_mchb>-cspem - wa_mska-kaspe.
    ENDIF.

    READ TABLE gt_mard ASSIGNING <fs_temp_mard>
    WITH KEY matnr = wa_mska-matnr
             werks = wa_mska-werks
             lgort = wa_mska-lgort.
    IF sy-subrc IS INITIAL.
      IF cb_batch IS INITIAL."For non-batch managed
        <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mska-kalab.
        <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mska-kaein.
        <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mska-kains.
        <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mska-kaspe.
      ELSE.
        IF wa_mska-charg IS INITIAL."For batch managed
          <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mska-kalab.
          <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mska-kaein.
          <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mska-kains.
          <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mska-kaspe.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_mssa INTO wa_mssa.
    wa_output-werks = wa_mssa-werks.
    wa_output-matnr = wa_mssa-matnr.
    wa_output-sobkz = wa_mssa-sobkz.
    wa_output-acc_assign = wa_mssa-vbeln.

    wa_output-b_clabs = wa_mssa-salab.
    wa_output-b_ceinm = wa_mssa-saein.
    wa_output-b_cinsm = wa_mssa-sains.
    wa_output-b_cspem = wa_mssa-saspe.

*   Check if the combination exists in MSKA
    LOOP AT gt_mska INTO wa_mska
    WHERE matnr = wa_mssa-matnr AND
             werks = wa_mssa-werks AND
             sobkz = wa_mssa-sobkz AND
             vbeln = wa_mssa-vbeln.
      wa_output-b_clabs = wa_output-b_clabs - wa_mska-kalab.
      wa_output-b_ceinm = wa_output-b_ceinm - wa_mska-kaein.
      wa_output-b_cinsm = wa_output-b_cinsm - wa_mska-kains.
      wa_output-b_cspem = wa_output-b_cspem - wa_mska-kaspe.
    ENDLOOP.

    IF wa_output-b_clabs IS NOT INITIAL OR
       wa_output-b_ceinm IS NOT INITIAL OR
       wa_output-b_cinsm IS NOT INITIAL OR
       wa_output-b_cspem IS NOT INITIAL.
      COLLECT wa_output INTO gt_output.
    ENDIF.

    CLEAR: wa_mska, wa_output.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Vendor Stock
*----------------------------------------------------------------------*
  LOOP AT gt_mslb INTO wa_mslb.
    wa_output-werks = wa_mslb-werks.
    wa_output-matnr = wa_mslb-matnr.
    wa_output-sobkz = wa_mslb-sobkz.
    wa_output-charg = wa_mslb-charg.
    wa_output-acc_assign = wa_mslb-lifnr.
    wa_output-b_clabs = wa_mslb-lblab.
    wa_output-b_ceinm = wa_mslb-lbein.
    wa_output-b_cinsm = wa_mslb-lbins.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.
  ENDLOOP.

  LOOP AT gt_mssl INTO wa_mssl.
    wa_output-werks = wa_mssl-werks.
    wa_output-matnr = wa_mssl-matnr.
    wa_output-sobkz = wa_mssl-sobkz.
    wa_output-acc_assign = wa_mssl-lifnr.

    wa_output-b_clabs = wa_mssl-sllab.
    wa_output-b_ceinm = wa_mssl-slein.
    wa_output-b_cinsm = wa_mssl-slins.

*   Check if the combination exists in MSLB
    LOOP AT gt_mslb INTO wa_mslb
      WHERE matnr = wa_mssl-matnr AND
            werks = wa_mssl-werks AND
            sobkz = wa_mssl-sobkz AND
            lifnr = wa_mssl-lifnr.

      wa_output-b_clabs = wa_output-b_clabs - wa_mslb-lblab.
      wa_output-b_ceinm = wa_output-b_ceinm - wa_mslb-lbein.
      wa_output-b_cinsm = wa_output-b_cinsm - wa_mslb-lbins.
    ENDLOOP.

    IF wa_output-b_clabs IS NOT INITIAL OR
       wa_output-b_ceinm IS NOT INITIAL OR
       wa_output-b_cinsm IS NOT INITIAL.

      COLLECT wa_output INTO gt_output.
    ENDIF.

    CLEAR wa_output.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Special Stocks with Customer
*----------------------------------------------------------------------*
  LOOP AT gt_msku INTO wa_msku.
    wa_output-werks = wa_msku-werks.
    wa_output-matnr = wa_msku-matnr.
    wa_output-sobkz = wa_msku-sobkz.
    wa_output-charg = wa_msku-charg.
    wa_output-acc_assign = wa_msku-kunnr.
    wa_output-b_clabs = wa_msku-kulab.
    wa_output-b_ceinm = wa_msku-kuein.
    wa_output-b_cinsm = wa_msku-kuins.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Special Stocks from Vendor
*----------------------------------------------------------------------*
  LOOP AT gt_mkol INTO wa_mkol.
    wa_output-werks = wa_mkol-werks.
    wa_output-matnr = wa_mkol-matnr.
    wa_output-sobkz = wa_mkol-sobkz.
    wa_output-charg = wa_mkol-charg.
    wa_output-lgort = wa_mkol-lgort.
    wa_output-acc_assign = wa_mkol-lifnr.
    wa_output-b_clabs = wa_mkol-slabs.
    wa_output-b_ceinm = wa_mkol-seinm.
    wa_output-b_cinsm = wa_mkol-sinsm.
    wa_output-b_cspem = wa_mkol-sspem.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.

*   Collect this record from MCHB
    READ TABLE gt_mchb ASSIGNING <fs_temp_mchb>
    WITH KEY werks = wa_mkol-werks
             lgort = wa_mkol-lgort
             matnr = wa_mkol-matnr
             charg = wa_mkol-charg.
    IF sy-subrc IS INITIAL.
      <fs_temp_mchb>-clabs = <fs_temp_mchb>-clabs - wa_mkol-slabs.
      <fs_temp_mchb>-ceinm = <fs_temp_mchb>-ceinm - wa_mkol-seinm.
      <fs_temp_mchb>-cinsm = <fs_temp_mchb>-cinsm - wa_mkol-sinsm.
      <fs_temp_mchb>-cspem = <fs_temp_mchb>-cspem - wa_mkol-sspem.
    ENDIF.

    READ TABLE gt_mard ASSIGNING <fs_temp_mard>
    WITH KEY matnr = wa_mkol-matnr
             werks = wa_mkol-werks
             lgort = wa_mkol-lgort.
    IF sy-subrc IS INITIAL.
      IF cb_batch IS INITIAL."For non-batch managed
        <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mkol-slabs.
        <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mkol-seinm.
        <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mkol-sinsm.
        <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mkol-sspem.
      ELSE."For batch managed
        IF wa_mkol-charg IS INITIAL.
          <fs_temp_mard>-labst = <fs_temp_mard>-labst - wa_mkol-slabs.
          <fs_temp_mard>-einme = <fs_temp_mard>-einme - wa_mkol-seinm.
          <fs_temp_mard>-insme = <fs_temp_mard>-insme - wa_mkol-sinsm.
          <fs_temp_mard>-speme = <fs_temp_mard>-speme - wa_mkol-sspem.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Storage Location Data for Material
*----------------------------------------------------------------------*
  LOOP AT gt_mard INTO wa_mard.
    wa_output-werks = wa_mard-werks.
    wa_output-matnr = wa_mard-matnr.
    wa_output-lgort = wa_mard-lgort.

    IF cb_batch IS NOT INITIAL."For batch managed
*   Collect this record from MCHB
      READ TABLE gt_mchb INTO wa_mchb
      WITH KEY werks = wa_mard-werks
               lgort = wa_mard-lgort
               matnr = wa_mard-matnr.
      IF sy-subrc IS INITIAL.
        wa_output-b_clabs = wa_mard-labst - wa_mchb-clabs.
        wa_output-b_ceinm = wa_mard-einme - wa_mchb-ceinm.
        wa_output-b_cinsm = wa_mard-insme - wa_mchb-cinsm.
        wa_output-b_cspem = wa_mard-speme - wa_mchb-cspem.
      ENDIF.
    ELSE."For non-batch managed
      wa_output-b_clabs = wa_mard-labst.
      wa_output-b_ceinm = wa_mard-einme.
      wa_output-b_cinsm = wa_mard-insme.
      wa_output-b_cspem = wa_mard-speme.
    ENDIF.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Calculate left over Batch Stocks
*----------------------------------------------------------------------*
  LOOP AT gt_mchb INTO wa_mchb.
    wa_output-werks = wa_mchb-werks.
    wa_output-matnr = wa_mchb-matnr.
    wa_output-lgort = wa_mchb-lgort.
    wa_output-charg = wa_mchb-charg.
    wa_output-b_clabs = wa_mchb-clabs.
    wa_output-b_ceinm = wa_mchb-ceinm.
    wa_output-b_cinsm = wa_mchb-cinsm.
    wa_output-b_cspem = wa_mchb-cspem.

    COLLECT wa_output INTO gt_output.
    CLEAR wa_output.
  ENDLOOP.

*----------------------------------------------------------------------*
*  Special Stocks from Vendor
*----------------------------------------------------------------------*
  LOOP AT gt_mbew INTO wa_mbew.
    IF wa_mbew-bwtar IS NOT INITIAL.
      wa_split-matnr = wa_mbew-matnr.
      wa_split-bwkey = wa_mbew-bwkey.
      wa_split-bwtar = wa_mbew-bwtar.
      wa_split-b_lbkum = wa_mbew-lbkum.
      COLLECT wa_split INTO gt_split.

      CLEAR: wa_mbew, wa_split.
    ENDIF.
  ENDLOOP.

  IF gt_output IS NOT INITIAL.
    temp_out = gt_output.

    IF gt_split IS NOT INITIAL.
      LOOP AT gt_split INTO wa_split.
        wa_output-matnr = wa_split-matnr.
        APPEND wa_output TO temp_out.
      ENDLOOP.
    ENDIF.

    SORT temp_out BY matnr.
    DELETE ADJACENT DUPLICATES FROM temp_out COMPARING matnr.

*   Get UOM of materials
    SELECT matnr
           meins
      FROM mara
      INTO TABLE gt_mara
      FOR ALL ENTRIES IN temp_out
      WHERE matnr = temp_out-matnr.
  ENDIF.

ENDFORM.                    " proc_data_before_conv
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file .

  DATA: lt_file_data     TYPE rsanm_file_table,
        wa_file_data     TYPE rsanm_file_line,
        lv_lines_written TYPE i,
        wa_output        TYPE ty_output,
        wa_split         TYPE ty_split,
        wa_mara          TYPE ty_mara,
        wa_mesg          TYPE ty_mesg,
        lv_clabs         TYPE char15,
        lv_ceinm         TYPE char15,
        lv_cinsm         TYPE char15,
        lv_cspem         TYPE char15,
        lv_cumln         TYPE char15,
        lv_return        TYPE char15,
        lv_bfile         TYPE string.

  IF gt_output IS NOT INITIAL.
    CONCATENATE 'Plant'(b26)
                'Storage Loc'(b40)
                'Material'(b25)
                'Batch'(b31)
                'Special Stock'(b32)
                'A/c Assignment'(b33)
                'UOM (BUOM)'(b28)
                'Unrestricted Stock'(b34)
                'Restricted Stock'(b35)
                'Quality Stock'(b36)
                'Blocked Stock'(b37)
                'Stock in Transit'(b38)
                'Returns'(b39)
           INTO wa_file_data SEPARATED BY ','.
    APPEND wa_file_data TO lt_file_data.


    LOOP AT gt_output INTO wa_output.
      lv_clabs = wa_output-b_clabs.
      lv_ceinm = wa_output-b_ceinm.
      lv_cinsm = wa_output-b_cinsm.
      lv_cspem = wa_output-b_cspem.
      lv_cumln = wa_output-b_cumlm.
      lv_return = wa_output-b_return.

      READ TABLE gt_mara INTO wa_mara
      WITH KEY matnr = wa_output-matnr.

      CONCATENATE wa_output-werks
                  wa_output-lgort
                  wa_output-matnr
                  wa_output-charg
                  wa_output-sobkz
                  wa_output-acc_assign
                  wa_mara-meins
                  lv_clabs
                  lv_ceinm
                  lv_cinsm
                  lv_cspem
                  lv_cumln
                  lv_return
             INTO wa_file_data SEPARATED BY ','.
      CONCATENATE wa_file_data space INTO wa_file_data.

      APPEND wa_file_data TO lt_file_data.
      CLEAR wa_file_data.
      CLEAR: lv_clabs,
             lv_ceinm,
             lv_cinsm,
             lv_cspem,
             lv_cumln.

    ENDLOOP.

    lv_bfile = p_bfile.

    CALL METHOD cl_rsan_ut_appserv_file_writer=>appserver_file_write
      EXPORTING
        i_filename      = lv_bfile
        i_overwrite     = 'X'
        i_data_tab      = lt_file_data
      IMPORTING
        e_lines_written = lv_lines_written
      EXCEPTIONS
        open_failed     = 1
        write_failed    = 2
        close_failed    = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      wa_mesg-serno = 1.
      wa_mesg-proc  = 'Stock Evaluation'(b30).
      wa_mesg-recno = lv_lines_written.
      wa_mesg-fpath = p_bfile.
      APPEND wa_mesg TO gt_mesg.
    ENDIF.
  ENDIF.

  CLEAR: wa_file_data, lt_file_data.

  IF cb_split IS NOT INITIAL.
    CHECK gt_split IS NOT INITIAL.
    CONCATENATE 'Material'(b25)
                'Plant'(b26)
                'Valuation Type'(b27)
                'UOM (BUOM)'(b28)
                'Total Valuated Stock'(b29)
           INTO wa_file_data SEPARATED BY ','.
    APPEND wa_file_data TO lt_file_data.

    LOOP AT gt_split INTO wa_split.
      lv_clabs = wa_split-b_lbkum.

      READ TABLE gt_mara INTO wa_mara
      WITH KEY matnr = wa_split-matnr.

      CONCATENATE wa_split-matnr
                  wa_split-bwkey
                  wa_split-bwtar
                  wa_mara-meins
                  lv_clabs
             INTO wa_file_data SEPARATED BY ','.

      APPEND wa_file_data TO lt_file_data.
      CLEAR wa_file_data.
      CLEAR: lv_clabs.

    ENDLOOP.

    lv_bfile = p_bsplit.

    CALL METHOD cl_rsan_ut_appserv_file_writer=>appserver_file_write
      EXPORTING
        i_filename      = lv_bfile
        i_overwrite     = 'X'
        i_data_tab      = lt_file_data
      IMPORTING
        e_lines_written = lv_lines_written
      EXCEPTIONS
        open_failed     = 1
        write_failed    = 2
        close_failed    = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      wa_mesg-serno = 2.
      wa_mesg-proc  = 'Split Valuation'(b24).
      wa_mesg-recno = lv_lines_written.
      wa_mesg-fpath = p_bsplit.
      APPEND wa_mesg TO gt_mesg.
    ENDIF.

  ENDIF.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  file_validation
*&---------------------------------------------------------------------*
*       Validate file
*----------------------------------------------------------------------*
*  no parameters
*----------------------------------------------------------------------*
FORM file_validation .
  DATA: li_tab   TYPE TABLE OF string,
        lv_lines TYPE i,
        wa_tab   TYPE string.

  IF rb_bconv IS NOT INITIAL.
    IF p_bfile IS INITIAL.
      MESSAGE 'Fill out mandatory fields'(m01) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      SPLIT p_bfile AT space INTO TABLE li_tab.
      lv_lines = lines( li_tab ).
      IF lv_lines > 1.
        MESSAGE 'Space not allowed in filename'(m02) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        SPLIT p_bfile AT '.' INTO TABLE li_tab.
        lv_lines = lines( li_tab ).
        READ TABLE li_tab INTO wa_tab INDEX lv_lines.
        IF sy-subrc IS INITIAL.
          TRANSLATE wa_tab TO UPPER CASE.
          IF wa_tab NE 'CSV'.
            MESSAGE 'Only .csv file allowed'(m03) TYPE 'S' DISPLAY LIKE 'E'.
*           LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR li_tab.

  IF cb_split IS NOT INITIAL.
    IF p_bsplit IS INITIAL.
      MESSAGE 'Fill out mandatory fields'(m04) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      SPLIT p_bsplit AT space INTO TABLE li_tab.
      lv_lines = lines( li_tab ).
      IF lv_lines > 1.
        MESSAGE 'Space not allowed in filename'(m05) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ELSE.
        SPLIT p_bsplit AT '.' INTO TABLE li_tab.
        lv_lines = lines( li_tab ).
        READ TABLE li_tab INTO wa_tab INDEX lv_lines.
        IF sy-subrc IS INITIAL.
          TRANSLATE wa_tab TO UPPER CASE.
          IF wa_tab NE 'CSV'.
            MESSAGE 'Only .csv file allowed'(m06) TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE LIST-PROCESSING.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " file_validation