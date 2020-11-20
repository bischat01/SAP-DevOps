  METHOD select_mara.

DATA: ls_mara TYPE mara.

* Select dtaa from Mara
    SELECT matnr FROM mara INTO CORRESPONDING FIELDS OF ex_wa_mara WHERE matnr eq im_v_matnr
                                                                    AND ersda EQ sy-datum.
      ENDSELECT.


    SELECT matnr FROM mara INTO CORRESPONDING FIELDS OF ls_mara WHERE matnr eq im_v_matnr
                                                                    AND ersda EQ '01012020'.
      ENDSELECT.

  ENDMETHOD.