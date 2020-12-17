  METHOD select_mara.
* Text changed 17122020 4th
* Select for Mara

    SELECT matnr FROM mara INTO CORRESPONDING FIELDS OF ex_wa_mara WHERE matnr eq im_v_matnr
                                                                    AND ersda EQ sy-datum.
      ENDSELECT.


  ENDMETHOD.