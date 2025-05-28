*&---------------------------------------------------------------------*
*& Report ZLPBNS_VOL_ORDER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlpbns_vol_order.
INCLUDE zlpbns_vol_order_top.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_vkorg  TYPE zlvsd_link_defau-vkorg OBLIGATORY DEFAULT 'O994',
            p_market TYPE zlpbns_vol_order-market OBLIGATORY.
SELECT-OPTIONS: s_zyear  FOR zlpbns_vol_order-zyear,
                s_zmonth FOR zlpbns_vol_order-zmonth,
                s_kunnr  FOR zlpbns_vol_order-kunnr,
                s_zdele  FOR zlpbns_vol_order-zdele,
                s_zpord  FOR zlpbns_vol_order-zporders.
SELECTION-SCREEN END OF BLOCK b1.
CLASS lcl_vol_order DEFINITION FINAL.
  PUBLIC SECTION.
    DATA: mv_upload  TYPE abap_bool VALUE abap_false,
          mv_changed TYPE abap_bool VALUE abap_false,
          mo_grid    TYPE REF TO cl_gui_alv_grid.
    DATA: mt_logs   TYPE bapiret2_t.
    METHODS display_orders.
    METHODS execute.
    METHODS save.
    METHODS extract_data.
    METHODS popup_to_confirm IMPORTING iv_text TYPE string RETURNING VALUE(rv_answer) TYPE char1.
  PRIVATE SECTION.
    TYPES BEGIN OF ty_order_tab.
    TYPES:  status   TYPE icon_d,
            rownr    TYPE sy-index.
            INCLUDE  TYPE zlpbns_vol_order.
            TYPES:   name1    TYPE kna1-name1,
            saved    TYPE abap_bool,
            style    TYPE lvc_t_styl,
            deleted  TYPE abap_bool,
            modified TYPE abap_bool.
    TYPES END OF ty_order_tab.
    TYPES: tt_order_tab TYPE TABLE OF ty_order_tab.
    DATA: mt_orders TYPE tt_order_tab,
          mv_file   TYPE string.
    DATA: BEGIN OF ms_techstyle,
            orders TYPE lvc_t_styl,
          END OF ms_techstyle.
    METHODS enrich_data.
    METHODS reset_row_index.
    METHODS file_upload.
    METHODS check_uploaded_data.
    METHODS template_download.
    METHODS start_container.
    METHODS add_message_sys.
    METHODS display_logs.
    METHODS: handle_toolbar
                FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object .
    METHODS: handle_user_command
                FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm .
    METHODS on_data_changed FOR EVENT data_changed  OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS message_init_show IMPORTING iv_action    TYPE char4.
    METHODS message_store IMPORTING iv_msid TYPE sy-msgid
                                    iv_msg1 TYPE sy-msgv1
                                    iv_msg2 TYPE sy-msgv2
                                    iv_msg3 TYPE sy-msgv3
                                    iv_msg4 TYPE sy-msgv4
                                    iv_msno TYPE sy-msgno
                                    iv_msty TYPE sy-msgty.
ENDCLASS.
CLASS lcl_vol_order IMPLEMENTATION.
  METHOD execute.
    start_container( ).
    extract_data( ).
    CALL  SCREEN 100.
  ENDMETHOD.
  METHOD save.
    DATA: lt_vol_order TYPE STANDARD TABLE OF zlpbns_vol_order.
    mo_grid->check_changed_data( ).
    check_uploaded_data( ).
    IF mt_logs IS NOT INITIAL.
      mv_upload = abap_true.
      RETURN.
    ENDIF.
    LOOP AT mt_orders ASSIGNING FIELD-SYMBOL(<ls_order>) WHERE modified IS NOT INITIAL.
      APPEND VALUE #( market   = <ls_order>-market
                      zyear    = <ls_order>-zyear
                      vkorg    = <ls_order>-vkorg
                      zmonth   = <ls_order>-zmonth
                      kunnr    = <ls_order>-kunnr
                      zdele    = <ls_order>-zdele
                      zporders = <ls_order>-zporders    ) TO lt_vol_order.
      CLEAR <ls_order>-modified.
    ENDLOOP.
    MODIFY zlpbns_vol_order FROM TABLE lt_vol_order.
    IF sy-subrc <> 0.
      MESSAGE 'Error while updating data' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.
    extract_data( ).
    mo_grid->set_ready_for_input( 0 ).
    mo_grid->refresh_table_display( ).
    gv_edit_mode = abap_false.
    CLEAR: mv_changed, mv_upload, mt_logs.
    MESSAGE 'Data saved successfuly!' TYPE 'S'.
  ENDMETHOD.
  METHOD check_uploaded_data.
    DATA: lt_orders_temp TYPE SORTED TABLE OF ty_order_tab
          WITH UNIQUE KEY vkorg
                          market
                          zyear
                          zmonth
                          kunnr.
    CLEAR: mt_logs.
    mo_grid->check_changed_data( ).

    LOOP AT mt_orders ASSIGNING FIELD-SYMBOL(<ls_orders>) WHERE modified = abap_true.
      DATA(lv_kunnr) = CONV kunnr( |{ <ls_orders>-kunnr ALPHA = IN  }| ).
      READ TABLE lt_orders_temp ASSIGNING FIELD-SYMBOL(<ls_temp_order>) WITH KEY vkorg  = <ls_orders>-vkorg
                                                                                 market = <ls_orders>-market
                                                                                 zyear  = <ls_orders>-zyear
                                                                                 zmonth = <ls_orders>-zmonth
                                                                                 kunnr  = <ls_orders>-kunnr BINARY SEARCH.
      IF sy-subrc = 0.
        MESSAGE e031(zlpbns_bonus)
        WITH <ls_orders>-rownr 'Record duplicated' INTO DATA(lv_dummy).
        add_message_sys( ).
<ls_orders>-status = icon_led_red.
        CONTINUE.
      ENDIF.
      INSERT <ls_orders> INTO TABLE lt_orders_temp.
      IF <ls_orders>-saved IS INITIAL.
        IF <ls_orders>-zyear IS INITIAL.
          MESSAGE e032(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Year' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ENDIF.
        IF  <ls_orders>-zmonth NOT BETWEEN '01' AND '12'.
          MESSAGE e032(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Month' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ENDIF.
        IF <ls_orders>-kunnr IS INITIAL.
          MESSAGE e034(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Customer number' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ELSE.
          SELECT SINGLE @abap_true
            FROM kna1
            INTO @DATA(lv_exist)
            WHERE kunnr = @lv_kunnr.
          IF sy-subrc <> 0.
            MESSAGE e033(zlpbns_bonus)
            WITH <ls_orders>-rownr 'Customer number' INTO lv_dummy.
            add_message_sys( ).
<ls_orders>-status = icon_led_red.
          ENDIF.
        ENDIF.
        SELECT SINGLE vkorg
          FROM tvko
          INTO @DATA(lv_vkorg)
          WHERE vkorg = @<ls_orders>-vkorg.
        IF sy-subrc <> 0.
          MESSAGE e033(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Sales Org.' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ENDIF.
        IF <ls_orders>-zporders < 0.
          MESSAGE e032(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Planned Orders' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ENDIF.
        IF <ls_orders>-status IS NOT INITIAL.
          CONTINUE.
        ENDIF.
<ls_orders>-status = icon_led_green.
      ELSE.
        IF <ls_orders>-zporders < 0.
          MESSAGE e032(zlpbns_bonus)
          WITH <ls_orders>-rownr 'Planned Orders' INTO lv_dummy.
          add_message_sys( ).
<ls_orders>-status = icon_led_red.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF mt_logs IS NOT INITIAL.
      display_logs( ).
      RETURN.
    ENDIF.
    MESSAGE 'Uploaded records are correct and ready to be saved' TYPE 'S'.
  ENDMETHOD.
  METHOD add_message_sys.
    APPEND VALUE bapiret2( id         = sy-msgid
                           type       = sy-msgty
                           number     = sy-msgno
                           message_v1 = sy-msgv1
                           message_v2 = sy-msgv2
                           message_v3 = sy-msgv3
                           message_v4 = sy-msgv4 ) TO mt_logs.
  ENDMETHOD.
  METHOD on_data_changed.
    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cells>).
      MODIFY mt_orders INDEX  <ls_mod_cells>-row_id
      FROM VALUE #( modified = abap_true )
      TRANSPORTING modified .
      CASE <ls_mod_cells>-fieldname.
        WHEN 'KUNNR'.
          SELECT SINGLE name1
            FROM kna1
            INTO @DATA(lv_name1)
            WHERE kunnr  = @<ls_mod_cells>-value.
          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'NAME1'
               i_value     = lv_name1
           ).
          CLEAR lv_name1.
      ENDCASE.
    ENDLOOP.
    mv_changed = abap_true.
  ENDMETHOD.
  METHOD display_orders.
    DATA lt_fcat TYPE lvc_t_fcat.
    DATA(ls_layout)  = VALUE lvc_s_layo( sel_mode    = 'A' stylefname = 'STYLE' cwidth_opt = abap_true ).
    DATA(ls_variant) = VALUE disvariant( report = sy-repid  username = sy-uname ).
    DATA(lt_toolbar_excluding) = VALUE ui_functions(  ( cl_gui_alv_grid=>mc_fc_loc_cut           )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_append_row    )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_insert_row    )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_delete_row    )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_copy          )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_copy_row      )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_paste         )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                                                      ( cl_gui_alv_grid=>mc_fc_check             )
                                                      ( cl_gui_alv_grid=>mc_fc_refresh           )
                                                      ( cl_gui_alv_grid=>mc_fc_graph             )
                                                      ( cl_gui_alv_grid=>mc_fc_info              )
                                                      ( cl_gui_alv_grid=>mc_fc_loc_undo          )  ).
    lt_fcat = VALUE #(
    ( fieldname = 'STATUS'     reptext   = 'Status'          )
    ( fieldname = 'ROWNR'      reptext   = 'Line ID.'        )
    ( fieldname = 'VKORG'      ref_table = 'ZLVSD_LINK_DEFAU')
    ( fieldname = 'MARKET'     ref_table = 'ZLPBNS_VOL_ORDER')
    ( fieldname = 'ZYEAR'      ref_table = 'ZLPBNS_VOL_ORDER')
    ( fieldname = 'ZMONTH'     ref_table = 'ZLPBNS_VOL_ORDER')
    ( fieldname = 'KUNNR'      ref_table = 'ZLPBNS_VOL_ORDER')
    ( fieldname = 'NAME1'      ref_table = 'KNA1'            )
    ( fieldname = 'ZDELE'      ref_table = 'ZLPBNS_VOL_ORDER' checkbox = abap_true )
    ( fieldname = 'ZPORDERS'   ref_table = 'ZLPBNS_VOL_ORDER' ) ).
    SET HANDLER : handle_toolbar
                  on_data_changed
                  handle_user_command
                  FOR mo_grid.
    mo_grid->register_edit_event(
  EXPORTING
    i_event_id = cl_gui_alv_grid=>mc_evt_modified
  EXCEPTIONS
    error      = 1                " Error
    OTHERS     = 2
).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
       it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_orders ).
  ENDMETHOD.
  METHOD handle_user_command.
    DATA: lt_selected_rows TYPE lvc_t_roid.
    mo_grid->get_selected_rows(
      IMPORTING
        et_row_no     = lt_selected_rows ).
    CASE e_ucomm.
      WHEN 'FC_INSERT'.
        INSERT INITIAL LINE INTO mt_orders ASSIGNING FIELD-SYMBOL(<ls_ord_line>) INDEX 1.
<ls_ord_line>-vkorg    = p_vkorg.
<ls_ord_line>-market   = p_market.
<ls_ord_line>-modified = abap_true.
<ls_ord_line>-style = VALUE #( ( fieldname = 'ZMONTH'     style = cl_gui_alv_grid=>mc_style_enabled )
                                       ( fieldname = 'ZYEAR'      style = cl_gui_alv_grid=>mc_style_enabled )
                                       ( fieldname = 'ZDELE'      style = cl_gui_alv_grid=>mc_style_enabled )
                                       ( fieldname = 'KUNNR'      style = cl_gui_alv_grid=>mc_style_enabled )
                                       ( fieldname = 'ZPORDERS'   style = cl_gui_alv_grid=>mc_style_enabled )
                                        ).
        reset_row_index( ).
        mv_changed = abap_true.
        mv_upload = abap_true.
      WHEN 'FC_DELETE'.
        IF lt_selected_rows IS INITIAL.
          MESSAGE 'Select at least one row to continue' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        SORT lt_selected_rows BY row_id DESCENDING.
        LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_rows>).
          READ TABLE mt_orders ASSIGNING FIELD-SYMBOL(<ls_line>) INDEX <ls_selected_rows>-row_id.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          IF <ls_line>-status = icon_led_inactive.
            MESSAGE 'Records saved in database can not be deleted' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
<ls_line>-deleted = abap_true.
        ENDLOOP.
        DELETE mt_orders WHERE deleted = abap_true.
        reset_row_index( ).
        mv_changed = abap_true.
      WHEN 'FC_DEL_ALL'.
        DELETE mt_orders WHERE status <> icon_led_inactive .
        reset_row_index( ).
        mv_changed = abap_true.
      WHEN 'FC_DOWNLOAD'.
        template_download( ).
      WHEN 'FC_UPLOAD'.
        file_upload( ).
        reset_row_index( ).
      WHEN 'FC_CHECK'.
        check_uploaded_data( ).
    ENDCASE.
    mo_grid->refresh_table_display( ).
  ENDMETHOD.
  METHOD extract_data.
    SELECT @icon_led_inactive AS status,
           vor~vkorg,
           vor~market,
           vor~zyear,
           vor~zmonth,
           vor~kunnr,
           kna1~name1,
           vor~zdele,
           vor~zporders,
           @abap_true AS saved
      FROM zlpbns_vol_order AS vor
      JOIN kna1
        ON kna1~kunnr = vor~kunnr
      INTO CORRESPONDING FIELDS OF TABLE @mt_orders
      WHERE vor~vkorg    =  @p_vkorg
        AND vor~market   =  @p_market
        AND vor~zyear    IN @s_zyear
        AND vor~zmonth   IN @s_zmonth
        AND vor~kunnr    IN @s_kunnr
        AND vor~zdele    IN @s_zdele
        AND vor~zporders IN @s_zpord.
    enrich_data( ).
  ENDMETHOD.
  METHOD enrich_data.
    LOOP AT mt_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).
<ls_orders>-rownr = sy-tabix.
      IF <ls_orders>-saved = abap_true.
<ls_orders>-style  =  VALUE #( style = cl_gui_alv_grid=>mc_style_enabled
                                       ( fieldname = 'ZDELE'    )
                                       ( fieldname = 'ZPORDERS' ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.
    IF gv_edit_mode = abap_false.
      RETURN.
    ENDIF.
    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
        ( function = 'FC_INSERT'   text = 'Insert'     icon = icon_insert_row   quickinfo = 'Insert new row' )
        ( function = 'FC_DELETE'   text = 'Delete'     icon = icon_delete_row   quickinfo = 'Delete selected rows' )
        ( function = 'FC_DEL_ALL'  text = 'Cancel All' icon = icon_delete       quickinfo = 'Delete all rows' )
        ( function = 'FC_DOWNLOAD' text = 'Download'   icon = icon_export       quickinfo = 'Download template' )
        ( function = 'FC_UPLOAD'   text = 'Upload'     icon = icon_import       quickinfo = 'Upload Data' ) ).
    IF mv_upload = abap_true.
      APPEND VALUE #( function = 'FC_CHECK' text = 'Check' icon = icon_check quickinfo = 'Check inserted data' ) TO  e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.
  METHOD message_store.
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = iv_msid
        msgty                  = iv_msty
        msgv1                  = iv_msg1
        msgv2                  = iv_msg2
        msgv3                  = iv_msg3
        msgv4                  = iv_msg4
        txtnr                  = iv_msno
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2
        OTHERS                 = 3.
  ENDMETHOD.
  METHOD message_init_show.
    CASE iv_action.
      WHEN 'INIT'.
        CALL FUNCTION 'MESSAGES_INITIALIZE'
          EXCEPTIONS
            OTHERS = 1.
      WHEN 'SHOW'.
        CALL FUNCTION 'MESSAGES_SHOW'
          EXPORTING
            i_use_grid         = 'X'
            batch_list_type    = 'L'
            send_if_one        = space
            show_linno         = ''
            show_linno_text    = space
          EXCEPTIONS
            inconsistent_range = 1
            no_messages        = 2
            OTHERS             = 3.
    ENDCASE.
  ENDMETHOD.
  METHOD display_logs.
    message_init_show( 'INIT' ).
    LOOP AT mt_logs ASSIGNING FIELD-SYMBOL(<ls_logs>).
      message_store(
        EXPORTING
          iv_msg1  = <ls_logs>-message_v1
          iv_msg2  = <ls_logs>-message_v2
          iv_msg3  = <ls_logs>-message_v3
          iv_msg4  = <ls_logs>-message_v4
          iv_msno  = <ls_logs>-number
          iv_msty  = <ls_logs>-type
          iv_msid  = <ls_logs>-id
      ).
    ENDLOOP.
    message_init_show( 'SHOW' ).
  ENDMETHOD.                    "display_logs
  METHOD template_download.
    DATA: lt_temp     TYPE STANDARD TABLE OF zlpbns_vol_order,
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
    TRY.
        DATA(lo_columns) = lo_salv_temp->get_columns( ).
        DATA(lo_column)  = lo_columns->get_column( 'MANDT' ).
        lo_column->set_visible( abap_false ).
        lo_column  = lo_columns->get_column( 'VKORG' ).
        lo_column->set_visible( abap_false ).
        lo_column  = lo_columns->get_column( 'MARKET' ).
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.
    DATA(lv_xml_bytes) = lo_salv_temp->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    cl_scp_change_db=>xstr_to_xtab( EXPORTING
                                      im_xstring = lv_xml_bytes
                                    IMPORTING
                                      ex_size    = DATA(lv_size)
                                      ex_xtab    = DATA(lt_xtab) ).
    IF lt_xtab IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        cl_gui_frontend_services=>file_save_dialog( EXPORTING
                                                      default_extension   = 'xlsx'
                                                      default_file_name   = 'Orders_template.xlsx'
                                                      file_filter         = cl_gui_frontend_services=>filetype_excel
                                                      prompt_on_overwrite = abap_true
                                                    CHANGING
                                                      filename            = lv_filename
                                                      path                = lv_path
                                                      fullpath            = lv_fullpath
                                                      user_action         = lv_action ).
        IF lv_action EQ cl_gui_frontend_services=>action_ok.
          cl_gui_frontend_services=>gui_download( EXPORTING
                                                    filename     = lv_fullpath
                                                    filetype     = 'BIN'
                                                    bin_filesize = lv_size
                                                  CHANGING
                                                    data_tab     = lt_xtab ).
        ENDIF.
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE lx_root->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
  METHOD file_upload.
    TYPES: BEGIN OF ty_excel,
             zyear    TYPE string,
             zmonth   TYPE string,
             kunnr    TYPE string,
             zdele    TYPE string,
             zporders TYPE string,
           END OF ty_excel.
    DATA: lt_data      TYPE solix_tab,
          lt_excel     TYPE STANDARD TABLE OF ty_excel,
          lt_filetable TYPE filetable,
          lv_rc        TYPE i.
    FIELD-SYMBOLS: <lt_excel> TYPE STANDARD TABLE.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename        = '*.XLS'
        file_filter             = '*.XLS'
        multiselection          = abap_false
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc = 0.
      READ TABLE lt_filetable INDEX 1 INTO mv_file.
    ENDIF.

    IF mv_file IS INITIAL.
      RETURN.
    ENDIF.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = mv_file
        filetype                = 'BIN'
      CHANGING
        data_tab                = lt_data
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
                        it_solix   = lt_data ).
    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet(
        document_name     = mv_file
        xdocument         = lv_bin_data ).
    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
      IMPORTING
        worksheet_names = DATA(lt_worksheet) ).
    IF lt_worksheet IS INITIAL.
      MESSAGE 'No worksheet was found. Please, check your Excel file!' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    DATA(lo_worksheet_table) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet[ 1 ] ).
    ASSIGN lo_worksheet_table->* TO <lt_excel>.
    DELETE <lt_excel> INDEX 1.
    IF <lt_excel> IS INITIAL.
      MESSAGE 'Excel worksheet can not be empty!' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    LOOP AT <lt_excel> ASSIGNING FIELD-SYMBOL(<ls_excel_upl>).
      APPEND INITIAL LINE TO lt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
      DO.
        DATA(lv_index) = sy-index.
        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel> TO FIELD-SYMBOL(<lv_new_comp>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upl> TO FIELD-SYMBOL(<lv_old_comp>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
<lv_new_comp> = <lv_old_comp>.
      ENDDO.
    ENDLOOP.
    LOOP AT lt_excel ASSIGNING <ls_excel>.
      lv_index = sy-tabix.
      DATA(lv_kunnr) = CONV kunnr( |{ <ls_excel>-kunnr ALPHA = IN  }| ).
      SELECT SINGLE name1
        FROM kna1
        INTO @DATA(lv_name1)
        WHERE kunnr = @lv_kunnr.
      IF <ls_excel>-zyear CN '0123456789'.
        DATA(lv_error_nc) = abap_true.
        CLEAR <ls_excel>-zyear.
      ENDIF.
      IF <ls_excel>-zmonth CN '0123456789'.
        lv_error_nc = abap_true.
        CLEAR <ls_excel>-zmonth.
      ENDIF.
      IF <ls_excel>-zporders CN '0123456789'.
        lv_error_nc = abap_true.
        CLEAR <ls_excel>-zporders.
      ENDIF.
      IF <ls_excel>-zdele IS NOT INITIAL.
<ls_excel>-zdele = abap_true.
      ENDIF.
      SELECT SINGLE @abap_true
        FROM zlpbns_vol_order
        INTO @DATA(lv_exist)
        WHERE market = @p_market
        AND   vkorg  = @p_vkorg
        AND   zyear  = @<ls_excel>-zyear
        AND   zmonth = @<ls_excel>-zmonth
        AND   kunnr  = @lv_kunnr.
      INSERT VALUE #( status      = COND #( WHEN lv_exist IS NOT INITIAL THEN icon_led_yellow )
                      vkorg       = p_vkorg
                      market      = p_market
                      zyear       = <ls_excel>-zyear
                      zmonth      = <ls_excel>-zmonth
                      kunnr       = lv_kunnr
                      zdele       = <ls_excel>-zdele
                      zporders    = <ls_excel>-zporders
                      name1       = lv_name1
                      style = VALUE #( style = cl_gui_alv_grid=>mc_style_enabled
                                       ( fieldname = 'ZMONTH'  )
                                       ( fieldname = 'ZYEAR'    )
                                       ( fieldname = 'ZDELE'    )
                                       ( fieldname = 'KUNNR'    )
                                       ( fieldname = 'ZPORDERS' ) )
                      ) INTO mt_orders INDEX lv_index.
    ENDLOOP.
    mv_upload  = abap_true.
    mv_changed = abap_true.
    CLEAR: lt_excel, mv_file.
  ENDMETHOD.
  METHOD popup_to_confirm.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = iv_text
        text_button_1         = 'Yes'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'No'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = rv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
  ENDMETHOD.
  METHOD start_container.
    DATA: lo_cont   TYPE REF TO cl_gui_custom_container.
    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.
    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUST_CONT100'
        repid                       = sy-repid
        dynnr                       = '0100'
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
    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = lo_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
  METHOD reset_row_index.
    LOOP AT mt_orders ASSIGNING FIELD-SYMBOL(<ls>).
<ls>-rownr = sy-tabix.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
DATA go_vol_order TYPE REF TO lcl_vol_order.
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_VKORG'.
      screen-input = 0.
      MODIFY SCREEN.
      EXIT.
    ENDIF.
  ENDLOOP.
START-OF-SELECTION.
  go_vol_order = NEW lcl_vol_order( ).
  go_vol_order->execute( ).
  INCLUDE zlpbns_vol_order_status_010o01.
