*&---------------------------------------------------------------------*
*& Include          Z_STOCK_RECONCILIATION_TOP
*&---------------------------------------------------------------------*
 TYPE-POOLS : rsanm,
              abap.

 TYPES:
   BEGIN OF ty_output,
     werks      TYPE werks_d,
     lgort      TYPE lgort_d,
     matnr      TYPE matnr,
     charg      TYPE charg_d,
     sobkz      TYPE sobkz,
     acc_assign TYPE char10,
     meins      TYPE meins,
     b_clabs    TYPE labst,
     b_ceinm    TYPE einme,
     b_cinsm    TYPE insme,
     b_cspem    TYPE speme,
     b_cumlm    TYPE trame,
     b_return   TYPE retme,
     a_clabs    TYPE labst,
     a_ceinm    TYPE einme,
     a_cinsm    TYPE insme,
     a_cspem    TYPE speme,
     a_cumlm    TYPE trame,
     a_return   TYPE retme,
     r_clabs    TYPE labst,
     r_ceinm    TYPE einme,
     r_cinsm    TYPE insme,
     r_cspem    TYPE speme,
     r_cumlm    TYPE trame,
     r_return   TYPE retme,
     remarks    TYPE char35, "char10,
   END OF ty_output,

   tt_output TYPE STANDARD TABLE OF ty_output.

 TYPES:
   BEGIN OF ty_split,
     matnr   TYPE matnr,
     bwkey   TYPE bwkey,
     bwtar   TYPE bwtar_d,
     meins   TYPE meins,
     b_lbkum TYPE lbkum,
     a_lbkum TYPE lbkum,
     r_lbkum TYPE lbkum,
     remarks TYPE char35, "char10,
   END OF ty_split,

   tt_split TYPE STANDARD TABLE OF ty_split,

   BEGIN OF ty_mesg,
     serno TYPE i,
     proc  TYPE char20,
     recno TYPE i,
     fpath TYPE char40,
   END OF ty_mesg,

   tt_mesg TYPE TABLE OF ty_mesg INITIAL SIZE 0.

 DATA: gv_werks TYPE werks_d,
       gv_lgort TYPE lgort_d,
       gv_lgnum TYPE lgnum,
       gv_datum TYPE datum.
 DATA:
   gt_output   TYPE tt_output,
   gt_output_a TYPE tt_output,
   gt_split    TYPE tt_split,
   gt_split_a  TYPE tt_split,
   gt_mesg     TYPE tt_mesg.


* ECC
 TYPES: BEGIN OF ty_mssq,
          matnr TYPE matnr,
          werks TYPE  werks_d,
          sobkz TYPE  sobkz,
          pspnr TYPE  ps_psp_pnr,
          sqlab TYPE  labst,
          sqins TYPE  insme,
          sqspe TYPE  speme,
          sqein TYPE  einme,
        END OF ty_mssq,

        tt_mssq TYPE STANDARD TABLE OF ty_mssq,

        BEGIN OF ty_mbew,
          matnr TYPE matnr,
          bwkey TYPE bwkey,
          bwtar TYPE bwtar_d,
          lbkum TYPE lbkum,
        END OF ty_mbew,

        tt_mbew TYPE STANDARD TABLE OF ty_mbew,

        BEGIN OF ty_mchb,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          charg TYPE charg_d,
          clabs TYPE labst,
          cinsm TYPE insme,
          ceinm TYPE einme,
          cspem TYPE speme,
        END OF ty_mchb,

        tt_mchb TYPE SORTED TABLE OF ty_mchb
                WITH UNIQUE KEY matnr werks lgort charg INITIAL SIZE 0,

        BEGIN OF ty_mspr,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          charg TYPE charg_d,
          sobkz TYPE sobkz,
          pspnr TYPE ps_psp_pnr,
          prlab TYPE labst,
          prins TYPE insme,
          prspe TYPE speme,
          prein TYPE einme,
        END OF ty_mspr,

*        tt_mspr TYPE STANDARD TABLE OF ty_mspr,
        tt_mspr TYPE SORTED TABLE OF ty_mspr
                WITH UNIQUE KEY matnr werks lgort charg sobkz pspnr INITIAL SIZE 0,

        BEGIN OF ty_mssa,
          matnr TYPE matnr,
          werks TYPE werks_d,
          sobkz TYPE sobkz,
          vbeln TYPE vbeln,
          salab TYPE labst,
          sains TYPE insme,
          saspe TYPE speme,
          saein TYPE einme,
        END OF ty_mssa,

        tt_mssa TYPE STANDARD TABLE OF ty_mssa,

        BEGIN OF ty_mslb,
          matnr TYPE matnr,
          werks TYPE werks_d,
          charg TYPE charg_d,
          sobkz TYPE sobkz,
          lifnr TYPE lifnr,
          lblab TYPE labst,
          lbins TYPE insme,
          lbein TYPE einme,
        END OF ty_mslb,

*        tt_mslb TYPE STANDARD TABLE OF ty_mslb,
        tt_mslb TYPE SORTED TABLE OF ty_mslb
                WITH UNIQUE KEY matnr werks charg sobkz lifnr INITIAL SIZE 0,

        BEGIN OF ty_msku,
          matnr TYPE matnr,
          werks TYPE werks_d,
          charg TYPE charg_d,
          sobkz TYPE sobkz,
          kunnr TYPE kunnr,
          kulab TYPE labst,
          kuins TYPE insme,
          kuein TYPE einme,
        END OF ty_msku,

        tt_msku TYPE STANDARD TABLE OF ty_msku,

        BEGIN OF ty_mska,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          charg TYPE charg_d,
          sobkz TYPE sobkz,
          vbeln TYPE vbeln,
          kalab TYPE labst,
          kains TYPE insme,
          kaspe TYPE speme,
          kaein TYPE einme,
        END OF ty_mska,

*        tt_mska TYPE STANDARD TABLE OF ty_mska,
        tt_mska TYPE SORTED TABLE OF ty_mska
                WITH NON-UNIQUE KEY matnr werks lgort charg sobkz vbeln INITIAL SIZE 0,

        BEGIN OF ty_mkol,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          charg TYPE charg_d,
          sobkz TYPE sobkz,
          lifnr TYPE lifnr,
          slabs TYPE labst,
          sinsm TYPE insme,
          seinm TYPE einme,
          sspem TYPE speme,
        END OF ty_mkol,

        tt_mkol TYPE STANDARD TABLE OF ty_mkol,

        BEGIN OF ty_mard,
          matnr TYPE matnr,
          werks TYPE werks_d,
          lgort TYPE lgort_d,
          labst TYPE labst,
          insme TYPE insme,
          einme TYPE einme,
          speme TYPE speme,
        END OF ty_mard,

        tt_mard TYPE SORTED TABLE OF ty_mard
                WITH UNIQUE KEY matnr werks lgort,

        BEGIN OF ty_mssl,
          matnr TYPE matnr,
          werks TYPE werks_d,
          sobkz TYPE sobkz,
          lifnr TYPE lifnr,
          sllab TYPE labst,
          slins TYPE insme,
          slein TYPE einme,
        END OF ty_mssl,

        tt_mssl TYPE STANDARD TABLE OF ty_mssl,

        BEGIN OF ty_mara,
          matnr TYPE matnr,
          meins TYPE meins,
        END OF ty_mara,

        tt_mara TYPE SORTED TABLE OF ty_mara
                WITH UNIQUE KEY matnr.


 DATA: gt_mssq TYPE tt_mssq,
       gt_mbew TYPE tt_mbew,
       gt_mchb TYPE tt_mchb,
       gt_mspr TYPE tt_mspr,
       gt_mssa TYPE tt_mssa,
       gt_mslb TYPE tt_mslb,
       gt_msku TYPE tt_msku,
       gt_mska TYPE tt_mska,
       gt_mkol TYPE tt_mkol,
       gt_mard TYPE tt_mard,
       gt_mssl TYPE tt_mssl,
       gt_mara TYPE tt_mara.