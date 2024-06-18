*&---------------------------------------------------------------------*
*&  Include           ZSD_UPDATESCHEDULE_IMPL
*&---------------------------------------------------------------------*

CLASS lcl_updateprice IMPLEMENTATION.
  METHOD initialization.

    DATA(lv_funct) = VALUE smp_dyntxt(
                      icon_id      = icon_export
                      icon_text    = 'Download Template'
                      quickinfo    = 'Download Excel Template' ).

    sscrfields-functxt_01 = lv_funct.

  ENDMETHOD.
  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      IF screen-name = 'P_FILE'.
        screen-required = 2.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.
  METHOD at_selection_screen.

    CASE sscrfields-ucomm.
      WHEN'FC01'. " Download Excel Template
        download_excel_template( ).
    ENDCASE.

  ENDMETHOD.
  METHOD download_excel_template.
    DATA: lt_temp     TYPE STANDARD TABLE OF ty_upload,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_fullpath TYPE string,
          lv_path     TYPE string.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv_temp)
          CHANGING
            t_table      = lt_temp ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

* itab (data) -> xstring (bytes)
    DATA(lv_xml_bytes) = lo_salv_temp->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).

* xstring (Bytes) -> RAW (iTab)
    cl_scp_change_db=>xstr_to_xtab( EXPORTING
                                      im_xstring = lv_xml_bytes
                                    IMPORTING
                                      ex_size    = DATA(lv_size)
                                      ex_xtab    = DATA(lt_xtab) ).

    IF lt_xtab IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
* SaveDialog aufrufen
        cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                                      default_extension   = 'xlsx'
                                                      default_file_name   = 'Update_Price_Template.xlsx'
                                                      file_filter         = cl_gui_frontend_services=>filetype_excel
                                                      prompt_on_overwrite = abap_true
                                                    CHANGING
                                                      filename            = lv_filename
                                                      path                = lv_path
                                                      fullpath            = lv_fullpath
                                                      user_action         = lv_action ).

        IF lv_action EQ cl_gui_frontend_services=>action_ok.
* iTab (bytes) -> lokale Datei
          cl_gui_frontend_services=>gui_download( EXPORTING
                                                    filename     = lv_fullpath
                                                    filetype     = 'BIN'
                                                    bin_filesize = lv_size " Size ist wichtig für das korrekte Schreiben der Excel-Datei
                                                  CHANGING
                                                    data_tab     = lt_xtab ).

        ENDIF.

      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD execute.
    upload_file( ).
    change_sales_document( ).

    IF mt_logs IS  NOT INITIAL.
      display_logs( ).
    ENDIF.

  ENDMETHOD.
  METHOD display_logs.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv_logs
          CHANGING
            t_table      = mt_logs ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    mo_salv_logs->get_functions( )->set_all( ).

    set_columns( ).
    set_tooltips( ).

    mo_salv_logs->display( ).

  ENDMETHOD.
  METHOD set_columns.
    DATA: lo_column TYPE REF TO cl_salv_column_table,
          lv_text   TYPE scrtext_s.

    DATA(lo_cols) = mo_salv_logs->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        lo_column ?= lo_cols->get_column( 'EBELN' ).
        lo_column->set_key( ).

        lv_text = 'Line no'(002).
        lo_column ?= lo_cols->get_column( 'LINE_MSG' ).
        lo_column->set_key( ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = 'Status'(003).
        lo_column ?= lo_cols->get_column( 'STATUS' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(004).
        lo_column ?= lo_cols->get_column( 'MSG' ).
        lo_column->set_short_text( lv_text ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

      CATCH cx_salv_not_found.
    ENDTRY.

    DATA(lo_sort) = mo_salv_logs->get_sorts( ).

    TRY.
        lo_sort->add_sort( columnname = 'EBELN'    ).
        lo_sort->add_sort( columnname = 'LINE_MSG' ).
      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
      CATCH cx_salv_existing .                          "#EC NO_HANDLER
      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.
  METHOD set_tooltips.

    DATA(lo_tooltips) = mo_salv_logs->get_functional_settings( )->get_tooltips( ).

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_green_light )
          tooltip = 'Success' ).                            "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_yellow_light )
          tooltip = 'Warning' ).                            "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_red_light )
          tooltip = 'Error' ).                              "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_information )
          tooltip = 'Information' ).                        "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.

    TRY.
        lo_tooltips->add_tooltip(
          type    = cl_salv_tooltip=>c_type_icon
          value   = CONV #( icon_message_critical )
          tooltip = 'Excel Error' ).                        "#EC NOTEXT
      CATCH cx_salv_existing.                           "#EC NO_HANDLER
    ENDTRY.


  ENDMETHOD.
  METHOD get_status_icon.
    rv_icon = SWITCH #( iv_type
                        WHEN 'E' OR 'X' OR 'A' THEN icon_red_light
                        WHEN 'W' THEN icon_yellow_light
                        WHEN 'S' THEN icon_green_light
                        WHEN 'I' THEN icon_information ).
  ENDMETHOD.
  METHOD upload_file.

    TYPES: BEGIN OF ty_excel,
             a TYPE string, "Sales order number
             b TYPE string, "Item number
             c TYPE string, "Schedule line
           END OF ty_excel.

    DATA: lt_data  TYPE solix_tab,
          lt_excel TYPE STANDARD TABLE OF ty_excel.

    FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.

    IF p_file IS INITIAL.
      MESSAGE s055(00) DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = p_file    " Name of file
        filetype                = 'BIN'    " File Type (ASCII, Binary)
      CHANGING
        data_tab                = lt_data   " Transfer table for file contents
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring(
                        it_solix   = lt_data
                    ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name     = p_file
        xdocument         = lv_bin_data
    ).

    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheet)
    ).

    IF lt_worksheet IS INITIAL.
      MESSAGE 'No worksheet was found. Please, check your Excel file!'(005)
         TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_table) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).

    ASSIGN lo_worksheet_table->* TO <lt_excel>.

    DELETE <lt_excel> INDEX 1.

    IF <lt_excel> IS INITIAL.

      MESSAGE 'Excel worksheet can not be empty!'(006)
         TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    MOVE-CORRESPONDING <lt_excel> TO lt_excel.

    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<ls_exc>).
      IF <ls_exc> IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(lv_tabix) = sy-tabix.

      TRY.
          APPEND VALUE #(

          purchase_order   = <ls_exc>-a
          purchase_item    = <ls_exc>-b
          net_price        = <ls_exc>-c

           ) TO mt_upload.

        CATCH cx_root INTO DATA(lx_root).

          IF lx_root->previous IS BOUND.
            DATA(lv_message) = lx_root->previous->get_longtext( ).
          ELSE.
            lv_message = lx_root->get_longtext( ).
          ENDIF.

          APPEND VALUE #( ebeln     = <ls_exc>-a
                          line_msg  = lv_tabix
                          status    = icon_message_critical
                          msg       = lv_message
                      ) TO mt_logs.

      ENDTRY.
    ENDLOOP.

  ENDMETHOD.
  METHOD format_date.
    DATA: lv_day(2)   TYPE n,
          lv_month(2) TYPE n,
          lv_year(4)  TYPE n.

    SPLIT iv_date AT '.' INTO lv_day lv_month lv_year.
    UNPACK: lv_day    TO lv_day,
            lv_month  TO lv_month.

    rv_date = lv_year && lv_month && lv_day.

  ENDMETHOD.
  METHOD f4_path.
    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i,
          lv_desk_dir  TYPE string.

    cl_gui_frontend_services=>get_desktop_directory(
      CHANGING
        desktop_directory    = lv_desk_dir   " Desktop Directory
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
    ).

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        initial_directory       = lv_desk_dir
        file_filter             = cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              = lt_filetable  " Table Holding Selected Files
        rc                      = lv_rc         " Return Code, Number of Files or -1 If Error Occurred
        user_action             = lv_user       " User Action (See Class Constants ACTION_OK, ACTION_CANCEL)
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lt_filetable IS NOT INITIAL.
      p_file = lt_filetable[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD change_sales_document.

    DATA: lt_return               TYPE STANDARD TABLE OF bapiret2,
          ls_return               TYPE bapiret2,
          lt_get_item_condition   TYPE STANDARD TABLE OF bapimeoutcondition,
          lt_item_condition       TYPE STANDARD TABLE OF bapimeoutcondition,
          lt_item_conditionx      TYPE STANDARD TABLE OF bapimeoutconditionx,
          lt_get_item_cond_validy TYPE STANDARD TABLE OF bapimeoutvalidity,
          lt_item_cond_validy     TYPE STANDARD TABLE OF bapimeoutvalidity,
          lt_item_cond_validyx    TYPE STANDARD TABLE OF bapimeoutvalidityx,
          lv_order_type           TYPE char1.

    SELECT ebeln, bstyp
    FROM ekko
    INTO TABLE @DATA(lt_bstyp)
    FOR ALL ENTRIES IN @mt_upload
    WHERE ebeln = @mt_upload-purchase_order.

    LOOP AT mt_upload ASSIGNING FIELD-SYMBOL(<ls_up>)
      GROUP BY ( ebeln = <ls_up>-purchase_order )
      ASSIGNING FIELD-SYMBOL(<lg_sales_doc>).

      READ TABLE lt_bstyp ASSIGNING FIELD-SYMBOL(<fs_bstyp>) WITH KEY ebeln = <lg_sales_doc>-ebeln.

      IF ( <fs_bstyp>-bstyp <> 'K' )  AND ( <fs_bstyp>-bstyp <> 'L' ).
        APPEND VALUE #( ebeln     = <fs_bstyp>-ebeln
                        line_msg  = sy-tabix
                        status    = icon_message_critical
                        msg       = 'Entered order is on contract or schedule agreement'
                    ) TO mt_logs.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BAPI_CONTRACT_GETDETAIL'
        EXPORTING
          purchasingdocument = CONV ebeln( <lg_sales_doc>-ebeln )
        TABLES
          item_cond_validity = lt_get_item_cond_validy
          item_condition     = lt_get_item_condition.

      IF lt_item_condition IS INITIAL OR lt_get_item_cond_validy IS INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE lt_get_item_cond_validy ASSIGNING FIELD-SYMBOL(<fs_item_c_v>) INDEX 1.
      READ TABLE lt_get_item_condition ASSIGNING FIELD-SYMBOL(<fs_item_c>) INDEX 1.

      LOOP AT GROUP <lg_sales_doc> ASSIGNING FIELD-SYMBOL(<ls_sales>).

        APPEND VALUE #(  item_no =  <ls_sales>-purchase_item
                         cond_value = <ls_sales>-net_price
                         serial_id = <fs_item_c>-serial_id
                         cond_count = <fs_item_c>-cond_count
                         cond_type = 'PB00'
                         change_id  = 'U'
                         ) TO lt_item_condition.

        APPEND VALUE #(  item_no =  <ls_sales>-purchase_item
                         cond_value = 'X'
                         serial_id = <fs_item_c>-serial_id
                         cond_count = <fs_item_c>-cond_count
                         ) TO lt_item_conditionx.

        APPEND VALUE #( item_no =  <ls_sales>-purchase_item
                         serial_id = <fs_item_c_v>-serial_id
                         plant = <fs_item_c_v>-plant
                         valid_from = <fs_item_c_v>-valid_from
                         valid_to = <fs_item_c_v>-valid_to
                         ) TO lt_item_cond_validy.

        APPEND VALUE #( item_no =  <ls_sales>-purchase_item
                         serial_id = <fs_item_c_v>-serial_id
                         ) TO lt_item_cond_validyx.
      ENDLOOP.

      CASE <fs_bstyp>-bstyp.

        WHEN 'K'.

          CALL FUNCTION 'BAPI_CONTRACT_CHANGE'
            EXPORTING
              purchasingdocument  = <ls_sales>-purchase_order
            TABLES
              item_cond_validity  = lt_item_cond_validy
              item_cond_validityx = lt_item_cond_validyx
              item_condition      = lt_item_condition
              item_conditionx     = lt_item_conditionx
              return              = lt_return.

        WHEN OTHERS.

          CALL FUNCTION 'BAPI_SAG_CHANGE'
            EXPORTING
              purchasingdocument  = <ls_sales>-purchase_order
            TABLES
              item_cond_validity  = lt_item_cond_validy
              item_cond_validityx = lt_item_cond_validyx
              item_condition      = lt_item_condition
              item_conditionx     = lt_item_conditionx
              return              = lt_return.
      ENDCASE.


      LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).
        DATA(lv_tabix) = sy-tabix.

        add_log(
          EXPORTING
            iv_vbeln    = <lg_sales_doc>-ebeln
            iv_line_msg = lv_tabix
            is_return   = <ls_return>
        ).

        IF <ls_return>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.
      ENDLOOP.

      IF lv_error IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = 'X'           " Use of Command `COMMIT AND WAIT`
          IMPORTING
            return = ls_return.    " Return Messages
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
          IMPORTING
            return = ls_return.    " Return Messages
      ENDIF.

      IF ls_return IS NOT INITIAL.

        add_log(
          EXPORTING
            iv_vbeln    = <lg_sales_doc>-ebeln
            iv_line_msg = lv_tabix + 1
            is_return   = ls_return
        ).
      ENDIF.

      CLEAR: lt_return,
             ls_return,
             lt_item_cond_validy,
             lt_item_cond_validyx,
             lt_item_condition,
             lt_item_conditionx,
             lv_tabix,
             lv_error.

    ENDLOOP.

  ENDMETHOD.
  METHOD add_log.

    APPEND INITIAL LINE TO mt_logs ASSIGNING FIELD-SYMBOL(<ls_log>).
    <ls_log> = VALUE #( ebeln    = iv_vbeln
                        line_msg = iv_line_msg
                        status   = get_status_icon( is_return-type )
                      ).

    MESSAGE ID is_return-id
       TYPE is_return-type
     NUMBER is_return-number
       WITH is_return-message_v1
            is_return-message_v2
            is_return-message_v3
            is_return-message_v4
            INTO <ls_log>-msg.

  ENDMETHOD.
ENDCLASS.
