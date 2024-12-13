*&---------------------------------------------------------------------*
*& Report ZFI_SOFIA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sofia.

DATA: BEGIN OF gs_screen0001,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0001.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string .
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_zfi_sofia DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_messages,
             time    TYPE sy-uzeit,
             bukrs   TYPE bukrs,
             xblnr   TYPE xblnr,
             buzei   TYPE buzei,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
             belnr   TYPE belnr_d,
             gjahr   TYPE gjahr,
           END OF ty_messages.

    TYPES: BEGIN OF ty_post_bapi,
             buzei  TYPE buzei,
             header TYPE bapiache09,
             items  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
             currc  TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
           END OF ty_post_bapi,
           tt_post_bapi TYPE STANDARD TABLE OF ty_post_bapi.

    DATA: mt_messages TYPE STANDARD TABLE OF ty_messages,
          mo_salv     TYPE REF TO cl_salv_table.

    METHODS :execute.
    CLASS-METHODS value_request.

    METHODS post_doc IMPORTING iv_test      TYPE abap_bool OPTIONAL
                               iv_job       TYPE abap_bool OPTIONAL
                     CHANGING  iv_no_commit TYPE abap_bool OPTIONAL.

    METHODS : display_msg IMPORTING iv_no_dock TYPE abap_bool OPTIONAL.
    METHODS: adjust_cols_msg.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_alv_data,
             status      TYPE icon-id,
             prog_nr     TYPE buzei,
             bukrs       TYPE bukrs,
             blart       TYPE blart,
             bldat       TYPE bldat,
             budat       TYPE budat,
             monat       TYPE monat,
             bktxt       TYPE bktxt,
             waers       TYPE waers,
             ldgrp       TYPE ldgrp,
             kursf_ext   TYPE ukrsnr,
             wwert       TYPE wwert_d,
             xblnr       TYPE xblnr,
             pargb_hdr   TYPE pargb,
             xmwst       TYPE xmwst,
             prog_nr_itm TYPE i,
             bukrs_itm   TYPE bukrs,
             hkont       TYPE hkont,
             sgtxt       TYPE sgtxt,
             wrsol       TYPE wrsol,
             wrhab       TYPE wrhab,
             dmbtr       TYPE dmbtr,
             dmbe2       TYPE dmbe2,
             mwskz       TYPE mwskz,
             txjcd       TYPE txjcd,
             kostl       TYPE kostl,
             prctr       TYPE prctr,
             aufnr       TYPE aufnr,
             ps_posid    TYPE ps_posid,
             valut       TYPE valut,
             hbkid       TYPE hbkid,
             hktid       TYPE hktid,
             zuonr       TYPE dzuonr,
             vbund       TYPE vbund,
             segment     TYPE segment,
             gsber       TYPE gsber,
             fipos       TYPE fipos,
             xref2       TYPE xref2,
             xref1       TYPE xref1,
           END OF ty_alv_data.

    DATA: mt_alv_data  TYPE STANDARD TABLE OF ty_alv_data,
          mo_cust_cont TYPE REF TO cl_gui_custom_container,
          mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          lv_error     TYPE abap_bool.

    METHODS csv_to_table.
    METHODS display_alv.

    METHODS call_bapi IMPORTING iv_test    TYPE abap_bool
                                is_head    TYPE bapiache09
                                it_glacc   TYPE bapiacgl09_tab
                                it_curre   TYPE bapiaccr09_tab
                                iv_buzei   TYPE buzei
                                iv_bukrs   TYPE bukrs
                                iv_gjahr   TYPE gjahr
                                iv_doc     TYPE string
                      EXPORTING ev_err     TYPE abap_bool
                                ev_obj_key TYPE bapiache09-obj_key.

    METHODS check_bapi IMPORTING iv_test    TYPE abap_bool
                                 is_head    TYPE bapiache09
                                 it_glacc   TYPE bapiacgl09_tab
                                 it_curre   TYPE bapiaccr09_tab
                                 iv_buzei   TYPE buzei
                                 iv_bukrs   TYPE bukrs
                                 iv_gjahr   TYPE gjahr
                                 iv_doc     TYPE string
                       EXPORTING ev_err     TYPE abap_bool
                                 ev_obj_key TYPE bapiache09-obj_key.

    METHODS handle_logs_ucomm FOR EVENT added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

    METHODS on_msg_link_click
      FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.

    METHODS view_belnr IMPORTING iv_belnr TYPE belnr_d
                                 iv_gjahr TYPE gjahr
                                 iv_bukrs TYPE bukrs.

    METHODS check_filename.

ENDCLASS.

DATA go_zfi_sofia TYPE REF TO lcl_zfi_sofia.

CLASS lcl_zfi_sofia IMPLEMENTATION.

  METHOD execute.

    IF p_file IS INITIAL .
      MESSAGE 'Please, fill the field first!'(002) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    check_filename( ).
    IF lv_error = abap_true.
      MESSAGE 'The filename does not match the required pattern: SOFIA_ANNO_MESE.CSV'(008) TYPE 'S' DISPLAY LIKE 'E'.
      CLEAR: lv_error.
      RETURN.
    ENDIF.

    csv_to_table( ).

    ##UNCOMENT_CONDITION
*    IF lv_error = abap_false.
    display_alv( ).
*    ENDIF.

  ENDMETHOD.                    "EXECUTE

  METHOD check_filename.

    DATA : lv_fname TYPE rlgrap-filename,
           lv_fpath TYPE  rlgrap-filename.

    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_file
      IMPORTING
        stripped_name = lv_fname
        file_path     = lv_fpath
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF strlen( lv_fname ) <> 17.
      lv_error = abap_true.
      RETURN.
    ENDIF.

    IF ( lv_fname(6) <> 'SOFIA_' )                     AND
       ( lv_fname+6(4)  NOT BETWEEN '0000' AND '9999' ) AND
       ( lv_fname+11(2) NOT BETWEEN '01'   AND '12' )   AND
       ( lv_fname+13 <> '.CSV' ).
      lv_error = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD csv_to_table.

    TYPES: BEGIN OF ty_split,
             prog_nr     TYPE string,
             bukrs       TYPE string,
             blart       TYPE string,
             bldat       TYPE string,
             budat       TYPE string,
             monat       TYPE string,
             bktxt       TYPE string,
             waers       TYPE string,
             ldgrp       TYPE string,
             kursf_ext   TYPE string,
             wwert       TYPE string,
             xblnr       TYPE string,
             pargb_hdr   TYPE string,
             xmwst       TYPE string,
             prog_nr_itm TYPE string,
             bukrs_itm   TYPE string,
             hkont       TYPE string,
             sgtxt       TYPE string,
             wrsol       TYPE string,
             wrhab       TYPE string,
             dmbtr       TYPE string,
             dmbe2       TYPE string,
             mwskz       TYPE string,
             txjcd       TYPE string,
             kostl       TYPE string,
             prctr       TYPE string,
             aufnr       TYPE string,
             ps_posid    TYPE string,
             valut       TYPE string,
             hbkid       TYPE string,
             hktid       TYPE string,
             zuonr       TYPE string,
             vbund       TYPE string,
             segment     TYPE string,
             gsber       TYPE string,
             fipos       TYPE string,
             xref2       TYPE string,
             xref1       TYPE string,
           END OF ty_split.

    DATA: lt_strings      TYPE STANDARD TABLE OF string,
          ls_split        TYPE ty_split,
          lv_line         TYPE string,
          lt_filetable    TYPE TABLE OF string,
          lv_prev_prog_nr TYPE string,
          lv_prog_nr      TYPE string,
          lv_skip_hdr     TYPE i VALUE 0,
          lv_skip_itm     TYPE i VALUE 0.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename = p_file
        filetype = 'ASC'
      CHANGING
        data_tab = lt_strings ).


    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 4.

      IF <ls_strings> NP ';*'.
        lv_skip_itm = 8.
        CONTINUE.
      ENDIF.

      IF lv_skip_itm IS NOT INITIAL.
        lv_skip_itm -= 1.
      ENDIF.

      CASE lv_skip_itm.

        WHEN 5.
          lv_prog_nr += 1.

          SPLIT <ls_strings> AT ';' INTO
                         DATA(lv_dummyy)
                         ls_split-bukrs
                         ls_split-blart
                         ls_split-bldat
                         ls_split-budat
                         ls_split-monat
                         ls_split-bktxt
                         ls_split-waers
                         ls_split-ldgrp
                         ls_split-kursf_ext
                         ls_split-wwert
                         ls_split-xblnr
                         ls_split-pargb_hdr
                         ls_split-xmwst.

          ls_split-bldat = |{ ls_split-bldat+6(4) }{ ls_split-bldat+3(2) ALPHA = IN }{ ls_split-bldat(2) ALPHA = IN }|.
          ls_split-budat = |{ ls_split-budat+6(4) }{ ls_split-budat+3(2) ALPHA = IN }{ ls_split-budat(2) ALPHA = IN }|.

        WHEN 0.

          SPLIT <ls_strings> AT ';' INTO
                                 DATA(lv_dummy)
                                 ls_split-bukrs_itm
                                 ls_split-hkont
                                 ls_split-sgtxt
                                 ls_split-wrsol
                                 ls_split-wrhab
                                 ls_split-dmbtr
                                 ls_split-dmbe2
                                 ls_split-mwskz
                                 ls_split-txjcd
                                 ls_split-kostl
                                 ls_split-prctr
                                 ls_split-aufnr
                                 ls_split-ps_posid
                                 ls_split-valut
                                 ls_split-hbkid
                                 ls_split-hktid
                                 ls_split-zuonr
                                 ls_split-vbund
                                 ls_split-segment
                                 ls_split-gsber
                                 ls_split-fipos
                                 ls_split-xref2
                                 ls_split-xref1.

          ls_split-valut = |{ ls_split-valut+6(4) }{ ls_split-valut+3(2) ALPHA = IN }{ ls_split-valut(2) ALPHA = IN }|.

          TRANSLATE ls_split-wrsol USING ',.'.
          TRANSLATE ls_split-wrhab USING ',.'.

          TYPES: BEGIN OF ty_glacc,
                   z_cont_alt       TYPE z_cont_alt,
                   z_cont_operativo TYPE z_cont_operativo,
                 END OF ty_glacc.
          DATA: lt_glacc TYPE SORTED TABLE OF ty_glacc WITH UNIQUE KEY z_cont_alt.

          READ TABLE lt_glacc TRANSPORTING NO FIELDS WITH TABLE KEY z_cont_alt =  ls_split-hkont.
          IF sy-subrc <> 0.
            INSERT VALUE #( z_cont_alt = ls_split-hkont ) INTO TABLE lt_glacc ASSIGNING FIELD-SYMBOL(<ls_glacc>).

            SELECT SINGLE z_cont_operativo
              FROM zfi_sofia_cont
              INTO <ls_glacc>-z_cont_operativo
              WHERE z_cont_alt = ls_split-hkont.

            IF <ls_glacc>-z_cont_operativo IS INITIAL.
              ##UNCOMENT_EXIT
*                  MESSAGE |Errore durante la trascodifica del conto { ls_split-hkont }| TYPE 'S' DISPLAY LIKE 'E'.
*                  lv_error = abap_true.
*                  exit.
            ENDIF.
          ENDIF.

          APPEND VALUE #( prog_nr     = lv_prog_nr
                          bukrs       = ls_split-bukrs
                          blart       = ls_split-blart
                          bldat       = ls_split-bldat
                          budat       = ls_split-budat
                          monat       = ls_split-monat
                          bktxt       = ls_split-bktxt
                          waers       = ls_split-waers
                          ldgrp       = ls_split-ldgrp
                          kursf_ext   = ls_split-kursf_ext
                          wwert       = ls_split-wwert
                          xblnr       = ls_split-xblnr
                          pargb_hdr   = ls_split-pargb_hdr
                          xmwst       = ls_split-xmwst
                          prog_nr_itm = ls_split-prog_nr_itm
                          bukrs_itm   = ls_split-bukrs_itm
                          hkont       = <ls_glacc>-z_cont_operativo
                          sgtxt       = ls_split-sgtxt
                          wrsol       = ls_split-wrsol
                          wrhab       = ls_split-wrhab
                          dmbtr       = ls_split-dmbtr
                          dmbe2       = ls_split-dmbe2
                          mwskz       = ls_split-mwskz
                          txjcd       = ls_split-txjcd
                          kostl       = ls_split-kostl
                          prctr       = ls_split-prctr
                          aufnr       = ls_split-aufnr
                          ps_posid    = ls_split-ps_posid
                          valut       = ls_split-valut
                          hbkid       = ls_split-hbkid
                          hktid       = ls_split-hktid
                          zuonr       = ls_split-zuonr
                          vbund       = ls_split-vbund
                          segment     = ls_split-segment
                          gsber       = ls_split-gsber
                          fipos       = ls_split-fipos
                          xref2       = ls_split-xref2
                          xref1       = ls_split-xref1
                        ) TO mt_alv_data.

      ENDCASE.
    ENDLOOP.

    IF mt_alv_data IS INITIAL.
      RETURN.
    ENDIF.

    SORT mt_alv_data BY prog_nr.

  ENDMETHOD.

  METHOD display_alv.

    DATA: lo_columns TYPE REF TO cl_salv_columns,
          lo_column  TYPE REF TO cl_salv_column_list.

    CREATE OBJECT mo_cust_cont
      EXPORTING
        container_name              = 'CUST_CONT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
      EXPORTING
        r_container      = mo_cust_cont
          IMPORTING
            r_salv_table = mo_salv
          CHANGING
            t_table      = mt_alv_data
        ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        RETURN.
    ENDTRY.

    DATA(lo_funct) = mo_salv->get_functions( ).
    lo_funct->set_all( abap_true ).

    TRY.
        lo_columns = mo_salv->get_columns( ).
        lo_columns->set_optimize( ).

        lo_column ?= lo_columns->get_column( 'STATUS' ).
        lo_column->set_icon( ).
        lo_column->set_short_text( value = 'Stato' ).
        lo_column->set_medium_text( value = 'Stato' ).
        lo_column->set_long_text( value = 'Stato' ).

        lo_column ?= lo_columns->get_column( 'PROG_NR' ).
        lo_column->set_icon( ).
        lo_column->set_short_text( value = 'ID' ).
        lo_column->set_medium_text( value = 'ID' ).
        lo_column->set_long_text( value = 'ID' ).

        TRY.
            DATA(lo_sorts) = mo_salv->get_sorts( ).
            DATA(lo_sort) =
                lo_sorts->add_sort(
                  EXPORTING
                    columnname = 'PROG_NR'
                    position   = 1
                    subtotal   = if_salv_c_bool_sap=>true
                ).

            DATA(lo_aggr) = mo_salv->get_aggregations( ).
            DATA(lo_aggr_col) = lo_aggr->add_aggregation( columnname  = 'WRSOL' ).
            lo_aggr->add_aggregation( columnname  = 'WRHAB' ).

          CATCH cx_salv_data_error.
          CATCH cx_salv_existing.
          CATCH cx_salv_not_found.
        ENDTRY.

        lo_columns->get_column( 'PROG_NR_ITM' )->set_technical( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    mo_salv->display( ).
    CALL SCREEN 0001.

  ENDMETHOD.


  METHOD post_doc.

    TYPES: BEGIN OF ty_post_bapi,
             buzei  TYPE buzei,
             header TYPE bapiache09,
             items  TYPE STANDARD TABLE OF bapiacgl09 WITH DEFAULT KEY,
             currc  TYPE STANDARD TABLE OF bapiaccr09 WITH DEFAULT KEY,
           END OF ty_post_bapi.

    TYPES: BEGIN OF ty_belnr,
             buzei TYPE buzei,
             belnr TYPE belnr_d,
           END OF ty_belnr.

    DATA: lt_selected  TYPE salv_t_row,
          lv_error     TYPE abap_bool,
          lv_obj_key   TYPE bapiache09-obj_key,
          lt_post_bapi TYPE STANDARD TABLE OF ty_post_bapi,
          lr_awkey     TYPE RANGE OF awkey,
          lt_belnr     TYPE STANDARD TABLE OF ty_belnr,
          lv_buzei     TYPE buzei,
          lv_prog_nr   TYPE buzei.

    GET TIME FIELD DATA(lv_uzeit).

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>)
      GROUP BY ( bukrs = <ls_data>-bukrs
                 xblnr = <ls_data>-xblnr
                 buzei =  <ls_data>-prog_nr )

       ASSIGNING FIELD-SYMBOL(<lg_bukrs_xblnr>).

      CLEAR: lt_belnr.

      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post_bapi_main>).
      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post>).

      <ls_post_bapi_main>-buzei = 1.

      LOOP AT GROUP <lg_bukrs_xblnr> ASSIGNING FIELD-SYMBOL(<ls_alv_data>).

        lv_prog_nr = <ls_alv_data>-prog_nr.

        lv_buzei += 1.

        IF <ls_post_bapi_main>-header IS INITIAL.
          <ls_post_bapi_main>-header = VALUE #(
                        username     = sy-uname
                        comp_code    = <ls_alv_data>-bukrs
                        ledger_group = <ls_alv_data>-ldgrp
                        doc_date     = <ls_alv_data>-bldat
                        pstng_date   = <ls_alv_data>-budat
                        doc_type     = <ls_alv_data>-blart
                        fis_period   = <ls_alv_data>-monat
                        header_txt   = <ls_alv_data>-bktxt
                        ref_doc_no   = <ls_alv_data>-xblnr
                                  ).
        ENDIF.

        APPEND VALUE bapiacgl09(  itemno_acc      = lv_buzei
                                  gl_account      = <ls_alv_data>-hkont
                                  comp_code       = <ls_alv_data>-bukrs
                                  item_text       = <ls_alv_data>-sgtxt
                                  tr_part_ba      = <ls_alv_data>-pargb_hdr
                                  bus_area        = <ls_alv_data>-gsber
                                  segment         = <ls_alv_data>-segment
                                  trade_id        = <ls_alv_data>-vbund
                                  housebankid     = <ls_alv_data>-hbkid
                                  housebankacctid = <ls_alv_data>-hktid
                                  alloc_nmbr      = <ls_alv_data>-zuonr
                                  value_date      = <ls_alv_data>-valut
                                  tax_code        = <ls_alv_data>-mwskz
                                  taxjurcode      = <ls_alv_data>-txjcd
                                  costcenter      = <ls_alv_data>-kostl
                                  profit_ctr      = <ls_alv_data>-prctr
                                  orderid         = <ls_alv_data>-aufnr
                                  wbs_element     = <ls_alv_data>-ps_posid
                                  cmmt_item       = <ls_alv_data>-fipos
                                  ref_key_2       = <ls_alv_data>-xref2
                                  ref_key_1       = <ls_alv_data>-xref1
                                  ) TO <ls_post_bapi_main>-items.

        APPEND VALUE bapiaccr09(
                         itemno_acc = lv_buzei
                         curr_type  = '00'
                         currency   = <ls_alv_data>-waers
                         amt_doccur = COND #( WHEN <ls_alv_data>-wrsol <> 0 THEN <ls_alv_data>-wrsol ELSE <ls_alv_data>-wrhab * -1 )
                         exch_rate  = <ls_alv_data>-kursf_ext
                          ) TO <ls_post_bapi_main>-currc ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).

        <ls_post>-buzei  = lv_buzei.
        <ls_post>-header = VALUE #(
                     username     = sy-uname
                     comp_code    = <ls_alv_data>-bukrs
                     doc_date     = <ls_alv_data>-bldat
                     pstng_date   = <ls_alv_data>-budat
                     doc_type     = <ls_alv_data>-blart
                     fis_period    = <ls_alv_data>-monat
                     header_txt   = <ls_alv_data>-bktxt
                     ref_doc_no   = <ls_alv_data>-xblnr
                               ).

        APPEND VALUE #(
       itemno_acc      = lv_buzei
          gl_account      = <ls_alv_data>-hkont
          comp_code       = <ls_alv_data>-bukrs
          item_text       = <ls_alv_data>-sgtxt
          tr_part_ba      = <ls_alv_data>-pargb_hdr
          bus_area        = <ls_alv_data>-gsber
          segment         = <ls_alv_data>-segment
          trade_id        = <ls_alv_data>-vbund
          housebankid     = <ls_alv_data>-hbkid
          housebankacctid = <ls_alv_data>-hktid
          alloc_nmbr      = <ls_alv_data>-zuonr
          value_date      = <ls_alv_data>-valut
          tax_code        = <ls_alv_data>-mwskz
          taxjurcode      = <ls_alv_data>-txjcd
          costcenter      = <ls_alv_data>-kostl
          profit_ctr      = <ls_alv_data>-prctr
          orderid         = <ls_alv_data>-aufnr
          wbs_element     = <ls_alv_data>-ps_posid
          cmmt_item       = <ls_alv_data>-fipos
          ref_key_2       = <ls_alv_data>-xref2
          ref_key_1       = <ls_alv_data>-xref1
                      ) TO <ls_post>-items.

        APPEND VALUE #(
         itemno_acc = lv_buzei
         curr_type  = '00'
         currency   = <ls_alv_data>-waers
         amt_doccur = COND #( WHEN <ls_alv_data>-wrsol <> 0 THEN <ls_alv_data>-wrsol * -1 ELSE <ls_alv_data>-wrhab )
         exch_rate  = <ls_alv_data>-kursf_ext
                       ) TO <ls_post>-currc.

      ENDLOOP.

      iv_no_commit = abap_false.

      LOOP AT lt_post_bapi ASSIGNING <ls_post>.

        CLEAR: lv_error,
               lv_obj_key.

        IF iv_test = abap_false.

          call_bapi(
            EXPORTING
              iv_test  = iv_test
              is_head  = <ls_post>-header
              it_glacc = <ls_post>-items
              it_curre = <ls_post>-currc
              iv_doc   = CONV #( <ls_post>-header-ref_doc_no )
              iv_buzei = lv_prog_nr
              iv_bukrs = <ls_post>-header-comp_code
              iv_gjahr = <ls_post>-header-pstng_date(4)
            IMPORTING
              ev_err     = lv_error
              ev_obj_key = lv_obj_key
          ).

        ELSE.

          check_bapi(
            EXPORTING
              iv_test  = iv_test
              is_head  = <ls_post>-header
              it_glacc = <ls_post>-items
              it_curre = <ls_post>-currc
              iv_doc   = CONV #( <ls_post>-header-ref_doc_no )
              iv_buzei = lv_prog_nr
              iv_bukrs = <ls_post>-header-comp_code
              iv_gjahr = <ls_post>-header-pstng_date(4)
            IMPORTING
              ev_err     = lv_error
              ev_obj_key = lv_obj_key
          ).

        ENDIF.

      ENDLOOP.

      CLEAR lv_prog_nr.

      IF <ls_post>-buzei IS NOT INITIAL.
        APPEND VALUE #( buzei = <ls_post>-buzei
                        belnr = lv_obj_key(10) ) TO lt_belnr.
      ELSE.
        DATA(lv_main_belnr) = lv_obj_key(10).
      ENDIF.

      LOOP AT GROUP <lg_bukrs_xblnr> ASSIGNING <ls_alv_data>.
        IF lv_error IS NOT INITIAL.
          <ls_alv_data>-status = icon_red_light.
          CONTINUE.
        ELSE.
          <ls_alv_data>-status = icon_green_light.
        ENDIF.

        IF iv_test IS NOT INITIAL.
          CONTINUE.
        ENDIF.

      ENDLOOP.

      CLEAR: lt_post_bapi.
    ENDLOOP.

    IF lv_error = abap_true.
      iv_no_commit = abap_true.
      MESSAGE 'Could not proccesss the file. Please check the errors'(009) TYPE 'I' DISPLAY LIKE 'E'.
      IF iv_test IS INITIAL .
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
      RETURN.
    ENDIF.

    IF iv_test IS INITIAL .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    CLEAR: iv_no_commit.
    IF iv_test IS INITIAL.
      MESSAGE 'Data saved successfully!'(004) TYPE 'S'.
    ELSE.
      MESSAGE 'Simulation concluded. Please, check the messages!'(005) TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD check_bapi.

    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_glacc  TYPE STANDARD TABLE OF bapiacgl09,
          lt_curre  TYPE STANDARD TABLE OF bapiaccr09.

    lt_glacc = it_glacc.
    lt_curre = it_curre.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = is_head
      TABLES
        accountgl      = lt_glacc
        currencyamount = lt_curre
        return         = lt_return.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    DATA(lv_uzeit) = sy-uzeit.
    DATA(lv_progr) = 0.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>)
       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).

      lv_progr += 1.
      APPEND VALUE #( time    = lv_uzeit
                      bukrs   = iv_bukrs
                      xblnr   = iv_doc
                      buzei   = iv_buzei
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                      belnr   = ev_obj_key(10)
                      gjahr   = iv_gjahr
                       ) TO mt_messages.

      IF  <ls_return>-type CA 'EAX'.
        ev_err = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD call_bapi.

    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_glacc  TYPE STANDARD TABLE OF bapiacgl09,
          lt_curre  TYPE STANDARD TABLE OF bapiaccr09.

    lt_glacc = it_glacc.
    lt_curre = it_curre.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = is_head
      TABLES
        accountgl      = lt_glacc
        currencyamount = lt_curre
        return         = lt_return.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    DATA(lv_uzeit) = sy-uzeit.
    DATA(lv_progr) = 0.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>)
       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).

      lv_progr += 1.
      APPEND VALUE #( time    = lv_uzeit
                      bukrs   = iv_bukrs
                      xblnr   = iv_doc
                      buzei   = iv_buzei
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                      belnr   = ev_obj_key(10)
                      gjahr   = iv_gjahr
                       ) TO mt_messages.

      IF  <ls_return>-type CA 'EAX'.
        ev_err = abap_true.
      ENDIF.
    ENDLOOP.

    READ TABLE mt_messages TRANSPORTING NO FIELDS WITH KEY icon = icon_red_light.

    IF iv_test = abap_true OR sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = is_head
      IMPORTING
        obj_key        = ev_obj_key
      TABLES
        accountgl      = lt_glacc
        currencyamount = lt_curre
        return         = lt_return.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    lv_uzeit = sy-uzeit.
    lv_progr = 0.

    LOOP AT lt_return ASSIGNING <ls_return>
       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).

      IF <ls_return>-type = 'S' .
        CONCATENATE <ls_return>-message_v2(10) <ls_return>-message_v2+10(4) <ls_return>-message_v2+14 INTO DATA(lv_spaced_msg) SEPARATED BY space.
        CONCATENATE <ls_return>-message(27) lv_spaced_msg <ls_return>-message+55 INTO DATA(lv_finalmsg) SEPARATED BY space.
      ENDIF.

      lv_progr += 1.
      APPEND VALUE #( time    = lv_uzeit
                      bukrs   = iv_bukrs
                      xblnr   = iv_doc
                      buzei   = iv_buzei
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = lv_finalmsg
                      belnr   = ev_obj_key(10)
                       ) TO mt_messages.

      IF  <ls_return>-type CA 'EAX'.
        ev_err = abap_true.
      ENDIF.
    ENDLOOP.

    IF ev_err = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      EXIT.
    ENDIF.

  ENDMETHOD.

  METHOD value_request.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename      = '*.CSV'
        file_filter           = '*.CSV'
        multiselection        = abap_false
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).

    IF sy-subrc = 0.
      READ TABLE lt_filetable INDEX 1 INTO p_file.
    ENDIF.

  ENDMETHOD.

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lo_events    TYPE REF TO cl_salv_events_table.

    IF iv_no_dock IS INITIAL.

      IF mo_salv_msg IS NOT INITIAL.
        mo_dock_msg->set_visible(
         EXPORTING
           visible           =  abap_true
         EXCEPTIONS
           cntl_error        = 1
           cntl_system_error = 2
           OTHERS            = 3
       ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        mo_salv_msg->refresh( ).
        RETURN.
      ENDIF.

      CREATE OBJECT mo_dock_msg
        EXPORTING
          repid = sy-repid
          dynnr = '0001'
          ratio = 50
          side  = cl_gui_docking_container=>dock_at_right.

      TRY.
          cl_salv_table=>factory(
              EXPORTING
              r_container = mo_dock_msg
            IMPORTING
              r_salv_table = mo_salv_msg
            CHANGING
              t_table      = mt_messages ).
        CATCH cx_salv_msg INTO DATA(lx_msg).
          MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDTRY.

      lo_functions = mo_salv_msg->get_functions( ).
      lo_functions->set_all( 'X' ).

      TRY.
          DATA(lv_icon) = icon_close.
          lo_functions->add_function(
            EXPORTING
              name     = 'FC_CLOSE'
              icon     = CONV #( lv_icon )
              tooltip  = 'Close'
              position = if_salv_c_function_position=>right_of_salv_functions
          ).
        CATCH cx_salv_existing.
        CATCH cx_salv_wrong_call.
      ENDTRY.

    ELSE.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = mo_salv_msg
            CHANGING
              t_table      = mt_messages ).
        CATCH cx_salv_msg INTO lx_msg.
          MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
      ENDTRY.

      mo_salv_msg->get_functions( )->set_all( 'X' ).

    ENDIF.

    adjust_cols_msg( ).

    lo_events = mo_salv_msg->get_event( ).
    SET HANDLER handle_logs_ucomm
                on_msg_link_click
           FOR lo_events.

    mo_salv_msg->display( ).

  ENDMETHOD.

  METHOD handle_logs_ucomm.
    CASE e_salv_function.
      WHEN 'FC_CLOSE'.

        mo_dock_msg->set_visible(
      EXPORTING
        visible           = abap_false                 " Visible
      EXCEPTIONS
        cntl_error        = 1                " CNTL_ERROR
        cntl_system_error = 2                " CNTL_SYSTEM_ERROR
        OTHERS            = 3
        ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD on_msg_link_click.
    READ TABLE mt_messages ASSIGNING FIELD-SYMBOL(<ls_msg>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'BELNR'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_msg>-belnr
            iv_gjahr = <ls_msg>-gjahr
            iv_bukrs = <ls_msg>-bukrs
        ).
    ENDCASE.

  ENDMETHOD.

  METHOD view_belnr.

    IF iv_belnr IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_gjahr) = iv_gjahr.
    DATA(lv_bukrs) = iv_bukrs.

    SET PARAMETER ID 'GJR' FIELD lv_gjahr.
    SET PARAMETER ID 'BUK' FIELD lv_bukrs.
    SET PARAMETER ID 'BLN' FIELD iv_belnr.

    CALL TRANSACTION 'FB03' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.

  ENDMETHOD.

  METHOD adjust_cols_msg.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'TIME' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'BUKRS' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'XBLNR' ).
        lo_column->set_key( ).

        DATA(lv_text) = CONV scrtext_s( 'Status'(006) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(007).
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        lo_cols->get_column( 'GJAHR' )->set_technical( ).

      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_zfi_sofia=>value_request( ).

START-OF-SELECTION.
  go_zfi_sofia = NEW lcl_zfi_sofia( ).
  go_zfi_sofia->execute( ).
*&---------------------------------------------------------------------*
*& Module STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF-SOFIA'.
  SET TITLEBAR 'TB-SOFIA'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  DATA lv_no_commit TYPE abap_bool.
  CLEAR: go_zfi_sofia->mt_messages.

  CASE gs_screen0001-ok_code.
    WHEN 'FC_SIMU'.
      go_zfi_sofia->post_doc( iv_test  = abap_true ).
      SORT go_zfi_sofia->mt_messages BY buzei message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_zfi_sofia->mt_messages COMPARING message buzei.
      go_zfi_sofia->display_msg( ).
      go_zfi_sofia->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    WHEN 'FC_POST'.

      go_zfi_sofia->post_doc(
      EXPORTING
         iv_test  = abap_true
      CHANGING
         iv_no_commit = lv_no_commit ).

      DELETE go_zfi_sofia->mt_messages WHERE icon = icon_green_light.
      SORT go_zfi_sofia->mt_messages BY buzei message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_zfi_sofia->mt_messages COMPARING message buzei.

      IF lv_no_commit IS INITIAL.
        CLEAR go_zfi_sofia->mt_messages.
        go_zfi_sofia->post_doc( iv_test  = abap_false ).
      ENDIF.

      go_zfi_sofia->display_msg( ).
      go_zfi_sofia->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    WHEN 'FC_BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
