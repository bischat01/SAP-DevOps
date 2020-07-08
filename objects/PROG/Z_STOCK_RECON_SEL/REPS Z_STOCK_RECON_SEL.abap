*&---------------------------------------------------------------------*
*& Include          Z_STOCK_RECONCILIATION_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: rb_bconv RADIOBUTTON GROUP rad1 USER-COMMAND rad1 DEFAULT 'X',
            rb_aconv RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: cb_batch AS CHECKBOX,
            cb_split AS CHECKBOX USER-COMMAND usr.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
PARAMETERS: rb_im   RADIOBUTTON GROUP rad3 USER-COMMAND rad3 DEFAULT 'X',
            rb_wmhu RADIOBUTTON GROUP rad3,
            rb_wm   RADIOBUTTON GROUP rad3,
            rb_ewm  RADIOBUTTON GROUP rad3.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: s_werks FOR gv_werks MODIF ID wer,
                s_lgort FOR gv_lgort,
                s_lgnum FOR gv_lgnum MODIF ID lgn,
                s_datum FOR gv_datum MODIF ID dat.

PARAMETERS: p_bfile  TYPE localfile,
            p_bsplit TYPE localfile MODIF ID spl,
            p_afile  TYPE localfile,
            p_asplit TYPE localfile MODIF ID asp.
SELECTION-SCREEN END OF BLOCK b4.
*&---------------------------------------------------------------------*