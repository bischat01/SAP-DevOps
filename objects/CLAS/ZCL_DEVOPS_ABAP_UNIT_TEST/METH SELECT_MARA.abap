  METHOD select_mara.
* Text changed 16122020
* Select for Mara

    SELECT matnr FROM mara INTO CORRESPONDING FIELDS OF ex_wa_mara WHERE matnr eq im_v_matnr
                                                                    AND ersda EQ sy-datum.
      ENDSELECT.


  ENDMETHOD.