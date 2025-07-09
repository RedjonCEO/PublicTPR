*&---------------------------------------------------------------------*
*& Report ZFI_SOFIA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sofia.

DATA: BEGIN OF gs_screen0001,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0001.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-010.
  PARAMETERS: p_direct RADIOBUTTON GROUP rb USER-COMMAND u01 DEFAULT 'X',
              p_disply RADIOBUTTON GROUP rb.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS :p_file TYPE string .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_gf_ind AS CHECKBOX USER-COMMAND gf_toggle.
  PARAMETERS :p_blart TYPE blart DEFAULT 'GF'.
SELECTION-SCREEN END OF BLOCK b3.

CLASS lcl_zfi_sofia DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_messages,
             prog_nr    TYPE numc5,
             excel_line TYPE numc10,
             time       TYPE sy-uzeit,
             bukrs      TYPE bukrs,
             xblnr      TYPE xblnr,
             xref2      TYPE xref2,
             sgtxt      TYPE sgtxt,
             icon       TYPE icon-id,
             message    TYPE bapi_msg,
             belnr      TYPE belnr_d,
             gjahr      TYPE gjahr,
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
    METHODS: adjust_cols_msg IMPORTING iv_no_dock TYPE abap_bool .

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_alv_data,
             status      TYPE icon-id,
             prog_nr     TYPE numc5,

             belnr       TYPE belnr_d,
             belnr_gtran TYPE belnr_d,

             bukrs       TYPE bukrs,
             blart       TYPE blart,
             bldat       TYPE bldat,
             budat       TYPE budat,
             monat       TYPE monat,
             bktxt       TYPE bktxt,
             waers       TYPE waers,
             ldgrp       TYPE fins_ledger,
             kursf_ext   TYPE ukrsnr,
             wwert       TYPE wwert_d,
             xblnr       TYPE xblnr,
             pargb_hdr   TYPE pargb,
             xmwst       TYPE xmwst,
             prog_nr_itm TYPE i,
             bukrs_itm   TYPE bukrs,

             hkont       TYPE hkont,
             hkont_gtran TYPE hkont,

             altkt       TYPE altkt,
             desc        TYPE txt50, "desc
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
             fipos       TYPE acdoca-fistl,
             xref2       TYPE xref2,
             xref1       TYPE xref1,

           END OF ty_alv_data.

    DATA: mt_alv_data  TYPE STANDARD TABLE OF ty_alv_data,
          mo_cust_cont TYPE REF TO cl_gui_custom_container,
          mo_salv_msg  TYPE REF TO cl_salv_table,
          mo_dock_msg  TYPE REF TO cl_gui_docking_container,
          mv_error     TYPE abap_bool.

    METHODS csv_to_table.
    METHODS display_alv.

    METHODS call_bapi IMPORTING iv_progr   TYPE numc5
                                iv_test    TYPE abap_bool
                                is_head    TYPE bapiache09
                                it_glacc   TYPE bapiacgl09_tab
                                it_curre   TYPE bapiaccr09_tab
                                iv_buzei   TYPE posnr_acc
                                iv_bukrs   TYPE bukrs
                                iv_gjahr   TYPE gjahr
                                iv_xref2   TYPE xref2
                                iv_sgtxt   TYPE sgtxt
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

    METHODS on_link_click FOR EVENT link_click  "Hotspot Handler
      OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.

DATA go_zfi_sofia TYPE REF TO lcl_zfi_sofia.

CLASS lcl_zfi_sofia IMPLEMENTATION.

  METHOD execute.

    DATA lv_no_commit TYPE abap_bool.

    CASE abap_true.
      WHEN p_direct OR p_disply.

        IF p_file IS INITIAL .
          MESSAGE 'Please, fill the field first!'(002) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        check_filename( ).
        IF mv_error = abap_true.
          MESSAGE 'The filename does not match the required pattern: SOFIA_ANNO_MESE.CSV'(008) TYPE 'S' DISPLAY LIKE 'E'.
          CLEAR: mv_error.
          RETURN.
        ENDIF.

        csv_to_table( ).

        CASE abap_true.
          WHEN p_direct.

            IF mv_error EQ abap_true.
              display_msg( iv_no_dock = abap_true ).
              RETURN.
            ENDIF.

            post_doc(
            EXPORTING
               iv_test  = abap_true
            CHANGING
               iv_no_commit = lv_no_commit ).

            DELETE mt_messages WHERE icon = icon_green_light.
            SORT mt_messages BY prog_nr message ASCENDING.
            DELETE ADJACENT DUPLICATES FROM mt_messages COMPARING message prog_nr.

            IF lv_no_commit IS INITIAL.
              CLEAR mt_messages.
              post_doc( iv_test  = abap_false ).
            ENDIF.
            display_msg( iv_no_dock = abap_true ).

          WHEN p_disply.

            display_alv( ).
        ENDCASE.

    ENDCASE.

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

    IF ( lv_fname(6) <> 'SOFIA_' ) OR
       ( lv_fname+6(4)  NOT BETWEEN '0000' AND '9999' ) OR
       ( lv_fname+11(2) NOT BETWEEN '01'   AND '12'  ).
      mv_error = abap_true.
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

    TYPES: BEGIN OF ty_glacc,
             z_esolver_acc    TYPE z_esolver_acc,
             z_cont_operativo TYPE z_cont_operativo,
           END OF ty_glacc.

    DATA: lt_glacc    TYPE SORTED TABLE OF ty_glacc WITH UNIQUE KEY z_esolver_acc,
          lt_strings  TYPE STANDARD TABLE OF string,
          ls_split    TYPE ty_split,
          lv_prog_nr  TYPE string,
          lv_skip_itm TYPE i VALUE 0,
          lv_hkont    TYPE c LENGTH 10,
          lv_spaces   TYPE string VALUE '    '.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename = p_file
        filetype = 'ASC'
      CHANGING
        data_tab = lt_strings
      EXCEPTIONS
        file_open_error         = 1                " File does not exist and cannot be opened
        file_read_error         = 2                " Error when reading file
        no_batch                = 3                " Cannot execute front-end function in background
        gui_refuse_filetransfer = 4                " Incorrect front end or error on front end
        invalid_type            = 5                " Incorrect parameter FILETYPE
        no_authority            = 6                " No upload authorization
        unknown_error           = 7                " Unknown error
        bad_data_format         = 8                " Cannot Interpret Data in File
        header_not_allowed      = 9                " Invalid header
        separator_not_allowed   = 10               " Invalid separator
        header_too_long         = 11               " Header information currently restricted to 1023 bytes
        unknown_dp_error        = 12               " Error when calling data provider
        access_denied           = 13               " Access to file denied.
        dp_out_of_memory        = 14               " Not enough memory in data provider
        disk_full               = 15               " Storage medium is full.
        dp_timeout              = 16               " Data provider timeout
        not_supported_by_gui    = 17               " GUI does not support this
        error_no_gui            = 18               " GUI not available
        OTHERS                  = 19
    ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_skip_itm = 8.

    LOOP AT lt_strings ASSIGNING FIELD-SYMBOL(<ls_strings>) FROM 4.
      DATA(lv_excel_line) = sy-tabix.

      SPLIT <ls_strings> AT ';' INTO DATA(lv_header) DATA(lv_dummy).
      REPLACE cl_abap_char_utilities=>cr_lf(1) INTO lv_header WITH space.
      CONDENSE lv_header NO-GAPS.

      IF <ls_strings> IS INITIAL .
*        lv_skip_itm -= 1.
        CONTINUE.
      ENDIF.

      IF lv_header IS NOT INITIAL.
        lv_prog_nr  = lv_header.

        lv_skip_itm = 8.
      ENDIF.

      CLEAR: lv_header, lv_dummy.

      CASE lv_skip_itm.

        WHEN 5.
*          lv_prog_nr += 1.

          SPLIT <ls_strings> AT ';' INTO
                         lv_dummy
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
                                 lv_dummy
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

          IF ls_split-hkont IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_split-valut = COND #( WHEN ls_split-valut IS NOT INITIAL
          THEN |{ ls_split-valut+6(4) }{ ls_split-valut+3(2) ALPHA = IN }{ ls_split-valut(2) ALPHA = IN }| ).

          TRANSLATE ls_split-wrsol USING ',.'.
          TRANSLATE ls_split-wrhab USING ',.'.
          TRANSLATE ls_split-dmbtr USING ',.'.
          TRANSLATE ls_split-dmbe2 USING ',.'.

          CONDENSE ls_split-hkont NO-GAPS.
          lv_hkont = to_upper( ls_split-hkont ).
          lv_hkont = |{ lv_hkont ALPHA = IN }|.

          READ TABLE lt_glacc ASSIGNING FIELD-SYMBOL(<ls_glacc>) WITH KEY z_esolver_acc = lv_hkont BINARY SEARCH.
          IF sy-subrc <> 0.

            INSERT VALUE #( z_esolver_acc = lv_hkont ) INTO TABLE lt_glacc ASSIGNING <ls_glacc>.

            SELECT SINGLE z_cont_operativo
              FROM zfi_sofia_cont
              INTO <ls_glacc>-z_cont_operativo
              WHERE z_esolver_acc = lv_hkont.
          ENDIF.

          IF <ls_glacc>-z_cont_operativo IS INITIAL.

            APPEND VALUE #(
              prog_nr    = lv_prog_nr
              excel_line = lv_excel_line
              time       = sy-uzeit
              bukrs      = ls_split-bukrs
              xblnr      = ls_split-xblnr
              xref2      = ls_split-xref2
              sgtxt      = ls_split-sgtxt
              icon       = icon_red_light
              message    = |Errore durante la trascodifica del conto { lv_hkont }|
               ) TO mt_messages.

            mv_error = abap_true.
          ENDIF.

          DATA lv_wrsol TYPE wrhab.
          DATA lv_wrhab TYPE wrhab.
          lv_wrsol = ls_split-wrsol .
          lv_wrhab = ls_split-wrhab .

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
                          wrsol       = COND wrhab( WHEN lv_wrsol <> 0 THEN lv_wrsol ELSE lv_wrsol * -1 )
                          wrhab       = COND wrhab( WHEN lv_wrhab <> 0 THEN lv_wrhab ELSE lv_wrhab * -1 )
                          dmbtr       = ls_split-dmbtr
                          dmbe2       = ls_split-dmbe2
                          mwskz       = ls_split-mwskz
                          txjcd       = ls_split-txjcd
                          kostl       = |{ ls_split-kostl ALPHA = IN }|
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

      IF lv_skip_itm IS NOT INITIAL.
        lv_skip_itm -= 1.
      ENDIF.

    ENDLOOP.

    IF mt_alv_data IS INITIAL ." OR mv_error IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT parent~saknr AS act_saknr,
          child~saknr  AS opp_saknr
     FROM ska1 AS parent
     JOIN ska1 AS child
       ON parent~main_saknr        = child~main_saknr
      AND parent~glaccount_subtype = child~glaccount_subtype
      AND parent~ktopl             = child~ktopl
      AND parent~xbilk             = child~xbilk
      AND parent~glaccount_type    = child~glaccount_type

     JOIN t001
       ON t001~ktopl = parent~ktopl

     FOR ALL ENTRIES IN @mt_alv_data

     WHERE parent~saknr             = @mt_alv_data-hkont
       AND child~saknr             <> @mt_alv_data-hkont
       AND t001~bukrs               = @mt_alv_data-bukrs
       AND parent~glaccount_subtype = 'S'
       AND parent~xbilk             = 'X'
       AND parent~glaccount_type    = 'C'
     INTO TABLE @DATA(lt_ska1).
    SORT lt_ska1 BY act_saknr.

    SELECT skb1~bukrs,
           skat~saknr,
           skb1~altkt,
           skat~txt50
      FROM skb1

      JOIN t001
        ON t001~bukrs = skb1~bukrs

      LEFT JOIN skat
      ON skat~saknr = skb1~saknr
        AND skat~ktopl = t001~ktopl
        AND skat~spras = 'I'

      FOR ALL ENTRIES IN @mt_alv_data
      WHERE skb1~saknr  = @mt_alv_data-hkont
        AND skb1~bukrs  = @mt_alv_data-bukrs
      INTO TABLE @DATA(lt_acc_and_dsc).

    SORT lt_acc_and_dsc BY bukrs saknr .


    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_alv_data>) .

      IF <ls_alv_data>-hbkid IS NOT INITIAL.

        READ TABLE lt_ska1 INTO DATA(ls_ska1)
           WITH KEY act_saknr = <ls_alv_data>-hkont BINARY SEARCH.

        IF sy-subrc = 0.
          <ls_alv_data>-hkont_gtran = ls_ska1-opp_saknr.
        ENDIF.

      ENDIF.

      READ TABLE lt_acc_and_dsc INTO DATA(ls_acc_and_dsc)
      WITH KEY bukrs = <ls_alv_data>-bukrs
               saknr = <ls_alv_data>-hkont BINARY SEARCH.

      IF sy-subrc = 0.
        <ls_alv_data>-altkt = ls_acc_and_dsc-altkt.
        <ls_alv_data>-desc = ls_acc_and_dsc-txt50.
      ENDIF.

    ENDLOOP.

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

        lo_column ?= lo_columns->get_column( 'BELNR' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        lo_column ?= lo_columns->get_column( 'BELNR_GTRAN' ).
        lo_column->set_cell_type(
            value = if_salv_c_cell_type=>hotspot
        ).

        lo_column ?= lo_columns->get_column( 'STATUS' ).
        lo_column->set_icon( ).
        lo_column->set_short_text( value = 'Stato' ).
        lo_column->set_medium_text( value = 'Stato' ).
        lo_column->set_long_text( value = 'Stato' ).

        lo_column ?= lo_columns->get_column( 'PROG_NR' ).
        lo_column->set_short_text( value = 'ID lotto' ).
        lo_column->set_medium_text( value = 'ID lotto' ).
        lo_column->set_long_text( value = 'ID lotto' ).

        lo_column->set_leading_zero(
            value = if_salv_c_bool_sap=>false
        ).

        DATA(lv_text) = 'Conto alternativo'.
        lo_column ?= lo_columns->get_column( 'ALTKT' ).
        lo_column->set_short_text( 'Cont Alt.' ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = 'Descr. Conto alternativo'.
        lo_column ?= lo_columns->get_column( 'DESC' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = 'Conto COGE Giroconto'.
        lo_column ?= lo_columns->get_column( 'HKONT_GTRAN' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_columns->get_column( columnname = 'WRSOL' )->set_currency( value = 'EUR' ).
        lo_columns->get_column( columnname = 'WRHAB' )->set_currency_column( value = 'WAERS' ).

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

        lo_columns->get_column( 'PROG_NR_ITM' )->set_technical( ).
      CATCH cx_salv_not_found.
      CATCH cx_salv_data_error.
      CATCH cx_salv_existing.
    ENDTRY.

    SET HANDLER on_link_click
            FOR mo_salv->get_event( ).

    mo_salv->display( ).

    IF mv_error = abap_true.
      display_msg( iv_no_dock = abap_false ).
    ENDIF.

    CALL SCREEN 0001.

  ENDMETHOD.

  METHOD on_link_click .

    READ TABLE mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'BELNR'.
        view_belnr(
          EXPORTING
            iv_belnr = <ls_data>-belnr
            iv_gjahr = <ls_data>-budat(4)
            iv_bukrs = <ls_data>-bukrs
        ).

      WHEN 'BELNR_GTRAN'.

        view_belnr(
          EXPORTING
            iv_belnr = <ls_data>-belnr_gtran
            iv_gjahr = <ls_data>-budat(4)
            iv_bukrs = <ls_data>-bukrs
        ).

    ENDCASE.

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
          mv_error     TYPE abap_bool,
          lv_obj_key   TYPE bapiache09-obj_key,
          lt_post_bapi TYPE STANDARD TABLE OF ty_post_bapi,
          lt_belnr     TYPE STANDARD TABLE OF ty_belnr,
          lv_buzei     TYPE posnr_acc,
          lv_prog_nr   TYPE numc5.

    GET TIME FIELD DATA(lv_uzeit).

    iv_no_commit = abap_false.

    LOOP AT mt_alv_data ASSIGNING FIELD-SYMBOL(<ls_data>)
      GROUP BY ( prog_nr =  <ls_data>-prog_nr )
      ASSIGNING FIELD-SYMBOL(<lg_prog_nr>).

      CLEAR: lt_belnr.

      APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post_bapi_main>).

      LOOP AT GROUP <lg_prog_nr> ASSIGNING FIELD-SYMBOL(<ls_alv_data>).

        lv_prog_nr =  <ls_alv_data>-prog_nr .

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
                                  funds_ctr       = <ls_alv_data>-fipos
                                  ref_key_2       = <ls_alv_data>-xref2
                                  ref_key_1       = <ls_alv_data>-xref1
                                  ) TO <ls_post_bapi_main>-items.


        "==================================================

        IF <ls_alv_data>-waers <> 'EUR'.

          APPEND VALUE bapiaccr09(
                           itemno_acc    = lv_buzei
                           curr_type     = '10'
                           currency      = 'EUR'
                           amt_doccur = COND #( WHEN <ls_alv_data>-wrsol IS INITIAL AND <ls_alv_data>-wrhab IS INITIAL
                                                THEN <ls_alv_data>-dmbtr
                                                WHEN <ls_alv_data>-wrsol IS NOT INITIAL AND <ls_alv_data>-wrhab IS INITIAL
                                                THEN <ls_alv_data>-dmbtr
                                                ELSE <ls_alv_data>-dmbtr * -1 )
                            ) TO <ls_post_bapi_main>-currc .

        ENDIF.

        "==================================================

        APPEND VALUE bapiaccr09(
                         itemno_acc    = lv_buzei
                         curr_type     = '00'
                         currency      = <ls_alv_data>-waers
                         amt_doccur = COND #( WHEN <ls_alv_data>-wrsol IS NOT INITIAL THEN <ls_alv_data>-wrsol ELSE <ls_alv_data>-wrhab * -1 )
                          ) TO <ls_post_bapi_main>-currc ASSIGNING FIELD-SYMBOL(<ls_bapi_accr>).

*        IF <ls_bapi_accr>-amt_doccur IS INITIAL .
*          <ls_bapi_accr>-exch_rate = 1.
*        ELSE.
*          <ls_bapi_accr>-exch_rate = abs( <ls_alv_data>-dmbtr / <ls_bapi_accr>-amt_doccur ).
*        ENDIF.

        IF <ls_alv_data>-hkont_gtran IS INITIAL OR p_gf_ind IS INITIAL.
          CONTINUE.
        ENDIF.

        APPEND INITIAL LINE TO lt_post_bapi ASSIGNING FIELD-SYMBOL(<ls_post>).
        <ls_post>-buzei  = lv_buzei.
        <ls_post>-header = VALUE #(
                     username     = sy-uname
                     comp_code    = <ls_alv_data>-bukrs
                     doc_date     = <ls_alv_data>-bldat
                     pstng_date   = <ls_alv_data>-budat
                     doc_type     = p_blart
                     fis_period   = <ls_alv_data>-monat
                     header_txt   = <ls_alv_data>-bktxt
                     ref_doc_no   = <ls_alv_data>-xblnr
                               ).

        <ls_post>-items = VALUE #(
         ( itemno_acc      = 1
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
          funds_ctr       = <ls_alv_data>-fipos
          ref_key_2       = <ls_alv_data>-xref2
          ref_key_1       = <ls_alv_data>-xref1
          )
        ( itemno_acc      = 2
          gl_account      = <ls_alv_data>-hkont_gtran
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
          funds_ctr       = <ls_alv_data>-fipos
          ref_key_2       = <ls_alv_data>-xref2
          ref_key_1       = <ls_alv_data>-xref1
                      ) ) .

        <ls_post>-currc = VALUE #(
         (
           itemno_acc = 1
           curr_type  = '00'
           currency   = <ls_alv_data>-waers
           amt_doccur = <ls_bapi_accr>-amt_doccur * -1
           exch_rate  = <ls_bapi_accr>-exch_rate
         )
         ( itemno_acc = 2
           curr_type  = '00'
           currency   = <ls_alv_data>-waers
           amt_doccur = <ls_bapi_accr>-amt_doccur
           exch_rate  = <ls_bapi_accr>-exch_rate
         ) ).

      ENDLOOP.

      DATA(lv_no_commit) = abap_false.

      LOOP AT lt_post_bapi ASSIGNING <ls_post>.

        CLEAR: mv_error,
               lv_obj_key.

        call_bapi(
          EXPORTING
            iv_progr = lv_prog_nr
            iv_test  = iv_test
            is_head  = <ls_post>-header
            it_glacc = <ls_post>-items
            it_curre = <ls_post>-currc
            iv_doc   = CONV #( <ls_post>-header-ref_doc_no )
            iv_buzei = lv_buzei
            iv_bukrs = <ls_post>-header-comp_code
            iv_gjahr = <ls_post>-header-pstng_date(4)
            iv_xref2 = <ls_post>-items[ 1 ]-ref_key_2
            iv_sgtxt = <ls_post>-items[ 1 ]-item_text
          IMPORTING
            ev_err     = mv_error
            ev_obj_key = lv_obj_key
        ).

        IF <ls_post>-buzei IS NOT INITIAL.
          APPEND VALUE #( buzei = <ls_post>-buzei
                          belnr = lv_obj_key(10) ) TO lt_belnr.
        ELSE.
          DATA(lv_main_belnr) = lv_obj_key(10).
        ENDIF.

        IF mv_error = abap_true.
          iv_no_commit = abap_true.
        ENDIF.

      ENDLOOP.

      LOOP AT GROUP <lg_prog_nr> ASSIGNING <ls_alv_data>.
        IF mv_error IS NOT INITIAL.
          <ls_alv_data>-status = icon_red_light.
          CONTINUE.
        ELSE.
          <ls_alv_data>-status = icon_green_light.
        ENDIF.

        IF iv_test IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        <ls_alv_data>-belnr  = lv_main_belnr.


        IF <ls_alv_data>-hkont_gtran IS INITIAL OR p_gf_ind IS INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE lt_belnr ASSIGNING FIELD-SYMBOL(<ls_belnr>) WITH KEY buzei = <ls_alv_data>-prog_nr.
        IF sy-subrc = 0.
          <ls_alv_data>-belnr_gtran = <ls_belnr>-belnr.
        ENDIF.

      ENDLOOP.

      IF iv_test IS INITIAL AND lv_no_commit IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.

      CLEAR: lv_no_commit,
      lt_post_bapi.

    ENDLOOP.

    IF iv_test IS INITIAL.
      MESSAGE 'Data saved successfully!'(004) TYPE 'S'.
    ELSE.
      MESSAGE 'Simulation concluded. Please, check the messages!'(005) TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD call_bapi.

    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_glacc  TYPE STANDARD TABLE OF bapiacgl09,
          lt_curre  TYPE STANDARD TABLE OF bapiaccr09.

    lt_glacc = it_glacc.
    lt_curre = it_curre.

    IF iv_test = abap_true.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = is_head
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return.

    ELSE.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = is_head
        IMPORTING
          obj_key        = ev_obj_key
        TABLES
          accountgl      = lt_glacc
          currencyamount = lt_curre
          return         = lt_return.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.
    DATA(lv_uzeit) = sy-uzeit.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
*       WHERE NOT ( type = 'E' AND id =  'RW' AND number = '609' ).
*
*      IF <ls_return>-type = 'S'
*        AND <ls_return>-id = 'RW'.
*
*        CASE <ls_return>-number.
*          WHEN 614.
*
*            IF iv_buzei IS INITIAL.
*              <ls_return>-message = 'Document Ready for Registration COTEC'(013) .  " Documento registrato COTEC
*            ELSE.
*              <ls_return>-message = 'Document Ready for Registration Girofondo Transito'(014) . "Documento registrato Girofondo Transitorio
*            ENDIF.
*
*          WHEN 605.
*
*            IF iv_buzei IS INITIAL.
*              <ls_return>-message = 'Registered Document COTEC'(015) .
*            ELSE.
*              <ls_return>-message = 'Registered Document Transitional Girofondo'(016).
*            ENDIF.
*        ENDCASE.
*
*      ENDIF.

*      lv_progr += 1.
      APPEND VALUE #( prog_nr = iv_progr
                      time    = lv_uzeit
                      bukrs   = iv_bukrs
                      xblnr   = iv_doc
                      icon    = SWITCH #(
                      <ls_return>-type
                        WHEN 'E' THEN icon_red_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'W' THEN icon_yellow_light )
                      message = <ls_return>-message
                      belnr   = COND #( WHEN ev_obj_key <> '$' THEN ev_obj_key(10) )
                      xref2   = iv_xref2
                      sgtxt   = iv_sgtxt
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

    adjust_cols_msg( iv_no_dock = iv_no_dock ).

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

        DATA(lv_text) = CONV scrtext_m( 'ID lotto' ).
        lo_column ?= lo_cols->get_column( 'PROG_NR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'EXCEL_LINE' ).
        IF iv_no_dock IS NOT INITIAL.
          lv_text = 'Riga Excel'.
          lo_column->set_short_text( CONV #( lv_text ) ).
          lo_column->set_medium_text( lv_text ).
          lo_column->set_long_text( CONV #( lv_text ) ).
          lo_column->set_key( ).
        ELSE.
          lo_column->set_technical( ).
        ENDIF.

        lo_column ?= lo_cols->get_column( 'TIME' ).
        IF iv_no_dock IS NOT INITIAL.
          lo_column->set_technical( ).
        ELSE.
          lo_column->set_key( ).
        ENDIF.

        lo_column ?= lo_cols->get_column( 'BUKRS' ).
        lo_column->set_key( ).

        lo_column ?= lo_cols->get_column( 'XBLNR' ).
        lo_column->set_key( ).

        lv_text = CONV scrtext_s( 'Status'(006) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(007).
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( lv_text ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lo_column ?= lo_cols->get_column( 'BELNR' ).
        IF iv_no_dock IS NOT INITIAL.
          lo_column->set_technical( ).
        ELSE.
          lo_column->set_cell_type(
           value = if_salv_c_cell_type=>hotspot
       ).
        ENDIF.

        lo_cols->get_column( 'GJAHR' )->set_technical( ).

      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN .
    IF screen-name CS 'P_BLART'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

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
      SORT go_zfi_sofia->mt_messages BY prog_nr message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_zfi_sofia->mt_messages COMPARING message prog_nr.
      go_zfi_sofia->display_msg( ).
      go_zfi_sofia->mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).
    WHEN 'FC_POST'.

      go_zfi_sofia->post_doc(
      EXPORTING
         iv_test  = abap_true
      CHANGING
         iv_no_commit = lv_no_commit ).

      DELETE go_zfi_sofia->mt_messages WHERE icon = icon_green_light.
      SORT go_zfi_sofia->mt_messages BY prog_nr message ASCENDING.
      DELETE ADJACENT DUPLICATES FROM go_zfi_sofia->mt_messages COMPARING message prog_nr.

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
