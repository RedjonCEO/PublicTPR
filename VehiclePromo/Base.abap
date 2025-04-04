*&---------------------------------------------------------------------*
*& Report ZLPBNS_PROMO_MD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlpbns_promo_md.

INCLUDE zlpbns_promo_md_top.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_vkorg TYPE vkorg OBLIGATORY DEFAULT 'O994',
            p_markt TYPE zlpbns_promo_mdh-market OBLIGATORY.
SELECT-OPTIONS: s_promo FOR zlpbns_promo_mdh-promo,
                s_prtyp FOR zlpbns_promo_mdh-promo_type,
                s_sellt FOR zlpbns_promo_mdh-znsc_selltyp,
                s_trade FOR zlpbns_promo_mdh-trade_in,
                s_wallb FOR zlpbns_promo_mdh-wallbox,
                s_statu FOR zlpbns_promo_mdh-status,
                s_exten FOR zlpbns_promo_mdh-zextend,
                s_zdele FOR zlpbns_promo_mdh-zdele,
                s_matnr FOR zlpbns_pr_model-matnr,
                s_fleet FOR zlpbns_pr_fleet-fleet,
                s_vhvin FOR zlpbns_pr_vin-vhvin,
                s_creon FOR zlpbns_promo_mdh-credate,
                s_creby FOR zlpbns_promo_mdh-crename,
                s_valon FOR zlpbns_promo_mdh-valid_t NO INTERVALS. "Valid to?

SELECTION-SCREEN SKIP.

PARAMETERS: p_draft TYPE zlpbns_promo_mdh-zdraft.
SELECTION-SCREEN END OF BLOCK b01.

CLASS lcl_promo DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_promo,
             icon            TYPE icon_d,
             vkorg           TYPE vkorg,
             market          TYPE zlpbns_promo_mdh-market,
             promo           TYPE zlpbns_promo_mdh-promo,
             promo_type      TYPE zlpbns_promo_mdh-promo_type,
             promo_type_desc TYPE zlpbns_pr_type_t-promo_type_desc,
             znsc_selltyp    TYPE zlpbns_promo_mdh-znsc_selltyp,
             sell_type_desc  TYPE char50,
             trade_in        TYPE zlpbns_promo_mdh-trade_in,
             financing       TYPE zlpbns_promo_mdh-financing,
             perc            TYPE zlpbns_promo_mdh-perc,
             wallbox         TYPE zlpbns_promo_mdh-wallbox,
             status          TYPE zlpbns_promo_mdh-status,
             status_desc     TYPE zlpbns_pr_stat_t-status_desc,
             valid_f         TYPE zlpbns_promo_mdh-valid_f,
             valid_t         TYPE zlpbns_promo_mdh-valid_t,
             zdraft          TYPE zlpbns_promo_mdh-zdraft,
             zdele           TYPE zlpbns_promo_mdh-zdele,
             zextend         TYPE zlpbns_promo_mdh-zextend,
             credate         TYPE zlpbns_promo_mdh-credate,
             crename         TYPE zlpbns_promo_mdh-crename,
             cretime         TYPE zlpbns_promo_mdh-cretime,
             moddate         TYPE zlpbns_promo_mdh-moddate,
             modname         TYPE zlpbns_promo_mdh-modname,
             modtime         TYPE zlpbns_promo_mdh-modtime,
*             promo_ref       TYPE REF TO lcl_promo,
           END OF ty_promo,
           tt_promo TYPE STANDARD TABLE OF ty_promo.

    TYPES: BEGIN OF ty_model,
             matnr      TYPE zlpbns_pr_model-matnr,
             matnr_desc TYPE maktx,
             perc_am    TYPE zlpbns_pr_model-perc_am,
             amount     TYPE zlpbns_pr_model-amount,
             curr       TYPE zlpbns_pr_model-curr,
             saved      TYPE abap_bool,
           END OF ty_model.

    TYPES: BEGIN OF ty_chassis,
             vhvin   TYPE zlpbns_pr_vin-vhvin,
             perc_am TYPE zlpbns_pr_vin-perc_am,
             amount  TYPE zlpbns_pr_vin-amount,
             curr    TYPE zlpbns_pr_vin-curr,
             saved   TYPE abap_bool,
           END OF ty_chassis.

    TYPES: BEGIN OF ty_fleet,
             fleet      TYPE zlpbns_pr_fleet-fleet,
             fleet_desc TYPE zlpbns_pr_fleet-fleet_desc,
             saved      TYPE abap_bool,
           END OF ty_fleet.

    TYPES: BEGIN OF ty_vin,
             vhvin TYPE zlpbns_pr_vin_ex-vhvin,
             saved TYPE abap_bool,
           END OF ty_vin.

    TYPES: BEGIN OF ty_incomp,
             promo_inc TYPE zlpbns_pr_incom-promo_inc,
             desc      TYPE char50,
             saved     TYPE abap_bool,
           END OF ty_incomp.

    DATA: mv_ok_code      TYPE sy-ucomm,
          mv_state        TYPE string,
          ms_promo_header TYPE ty_promo,
          mv_promo_code   TYPE zlpbns_promo_mdh-promo,
          mv_valid_to     TYPE char15,
          mv_valid_from   TYPE char15,
          mv_last_tab     TYPE char30.

    DATA: mt_model   TYPE STANDARD TABLE OF ty_model,
          mt_chassis TYPE STANDARD TABLE OF ty_chassis,
          mt_fleet   TYPE STANDARD TABLE OF ty_fleet,
          mt_vin     TYPE STANDARD TABLE OF ty_vin,
          mt_incomp  TYPE STANDARD TABLE OF ty_incomp.

    METHODS    execute IMPORTING iv_state TYPE string
                                 is_promo TYPE ty_promo OPTIONAL.

    METHODS pbo.
    METHODS pai.
    METHODS pai_exit200.
    METHODS check_changed_data.
    METHODS get_result EXPORTING es_promo TYPE ty_promo
                                 ev_saved TYPE abap_bool.
* Tabstrip PBO
    METHODS ts_pbo.

  PRIVATE SECTION.

    DATA: mv_saved           TYPE abap_bool,
          mt_messages_header TYPE esp1_message_tab_type,
          mt_messages_item   TYPE esp1_message_tab_type,
          mt_messages        TYPE esp1_message_tab_type.

    DATA: mo_model_container   TYPE REF TO cl_gui_custom_container,
          mo_model             TYPE REF TO cl_gui_alv_grid,
          mt_model_fcat        TYPE lvc_t_fcat,

          mo_chassis_container TYPE REF TO cl_gui_custom_container,
          mo_chassis           TYPE REF TO cl_gui_alv_grid,
          mt_chassis_fcat      TYPE lvc_t_fcat,

          mo_fleet_container   TYPE REF TO cl_gui_custom_container,
          mo_fleet             TYPE REF TO cl_gui_alv_grid,
          mt_fleet_fcat        TYPE lvc_t_fcat,

          mo_vin_container     TYPE REF TO cl_gui_custom_container,
          mo_vin               TYPE REF TO cl_gui_alv_grid,
          mt_vin_fcat          TYPE lvc_t_fcat,

          mo_incomp_container  TYPE REF TO cl_gui_custom_container,
          mo_incomp            TYPE REF TO cl_gui_alv_grid,
          mt_incomp_fcat       TYPE lvc_t_fcat.

    DATA: mt_deleted_model   TYPE STANDARD TABLE OF ty_model,
          mt_deleted_chassis TYPE STANDARD TABLE OF ty_chassis,
          mt_deleted_fleet   TYPE STANDARD TABLE OF ty_fleet,
          mt_deleted_vin     TYPE STANDARD TABLE OF ty_vin,
          mt_deleted_incomp  TYPE STANDARD TABLE OF ty_incomp.

    METHODS modify_screen.
    METHODS popup_to_confirm RETURNING VALUE(rv_answer) TYPE char1.
    METHODS refresh_tabstrip_alv.
    METHODS save.
    METHODS clear_promo IMPORTING iv_clear_header TYPE abap_bool DEFAULT abap_false.
    METHODS create_entries.
    METHODS update_entries.
    METHODS check_header_data.
    METHODS check_item_data.
    METHODS insert_row IMPORTING iv_tab TYPE string.
    METHODS delete_rows IMPORTING iv_tab           TYPE string
                                  it_selected_rows TYPE lvc_t_roid.
    METHODS delete_all IMPORTING iv_tab TYPE string.
    METHODS add_message IMPORTING iv_msgno    TYPE esp1_message_wa_type-msgno
                                  iv_msgty    TYPE esp1_message_wa_type-msgty
                                  iv_msgv1    TYPE esp1_message_wa_type-msgv1 OPTIONAL
                                  iv_msgv2    TYPE esp1_message_wa_type-msgv2 OPTIONAL
                                  iv_msgv3    TYPE esp1_message_wa_type-msgv3 OPTIONAL
                                  iv_msgv4    TYPE esp1_message_wa_type-msgv4 OPTIONAL
                        CHANGING  ct_messages TYPE esp1_message_tab_type.
    METHODS display_messages.
    METHODS fill_tables.
    METHODS model_fieldcatalog.
    METHODS chassis_fieldcatalog.
    METHODS fleet_fieldcatalog.
    METHODS vin_fieldcatalog.
    METHODS incomp_fieldcatalog..
    METHODS update_fcat.

    METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
    METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS on_hotspot      FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id
                                                                                 e_row_id.
    METHODS grid_toolbar    FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object
                                                                           e_interactive.

ENDCLASS.

CLASS lcl_promo IMPLEMENTATION.
  METHOD execute.

    g_ts_promo-pressed_tab = c_ts_promo-tab1. "Reset the tabstrip tab

    ms_promo_header = is_promo.
    mv_state = iv_state.

    CASE mv_state.
      WHEN gc_state-new.
        mv_promo_code = '$$$'.
        ms_promo_header = VALUE #( market  = p_markt
                                   status  = '01'
                                   credate = sy-datum
                                   cretime = sy-uzeit
                                   crename = sy-uname
                                   zdraft  = abap_true ).
        RETURN.
      WHEN gc_state-copy.
        mv_promo_code = '$$$'.
        ms_promo_header = VALUE #( BASE ms_promo_header
                                   credate = sy-datum
                                   cretime = sy-uzeit
                                   crename = sy-uname
                                   modname = ''
                                   moddate = ''
                                   modtime = '' ).
      WHEN gc_state-view.
        mv_promo_code = ms_promo_header-promo.
    ENDCASE.

    fill_tables( ).
  ENDMETHOD.
  METHOD pbo.

    modify_screen( ).
    check_changed_data( ).

    IF mv_ok_code <> 'FC_SAVE'.
      CLEAR mv_saved.
    ELSE.
      CLEAR mv_ok_code.
    ENDIF.

    READ TABLE gt_status_text ASSIGNING FIELD-SYMBOL(<ls_status_text>) WITH KEY status = ms_promo_header-status.

    IF sy-subrc = 0.
      ms_promo_header-status_desc = <ls_status_text>-status_desc.
    ENDIF.

*    CASE mv_state.
*      WHEN gc_state-new.
*      WHEN gc_state-copy.
*      WHEN gc_state-view OR gc_state-edit.
    update_fcat( ).
*    ENDCASE.

    mv_last_tab = g_ts_promo-pressed_tab.

  ENDMETHOD.
  METHOD pai.
    CASE mv_ok_code.
      WHEN 'FC_DELETE'.
        clear_promo( iv_clear_header = abap_true ).
        MESSAGE s012(zlpbns_bonus) DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0.
      WHEN 'FC_SAVE'.
        save( ).
      WHEN 'FC_EDIT'.
        mv_state = gc_state-edit.
      WHEN 'FC_DISPLAY'.
        mv_state = gc_state-view.
      WHEN c_ts_promo-tab1. "Model
        g_ts_promo-pressed_tab = c_ts_promo-tab1.
      WHEN c_ts_promo-tab2. "Chassis
        g_ts_promo-pressed_tab = c_ts_promo-tab2.
      WHEN c_ts_promo-tab3. "Fleet
        g_ts_promo-pressed_tab = c_ts_promo-tab3.
      WHEN c_ts_promo-tab4. "Vin
        g_ts_promo-pressed_tab = c_ts_promo-tab4.
      WHEN c_ts_promo-tab5. "Incomp
        g_ts_promo-pressed_tab = c_ts_promo-tab5.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD pai_exit200.
    CASE mv_ok_code.
      WHEN 'FC_BACK'.
        IF mv_saved IS INITIAL.
          IF popup_to_confirm( ) = '2'.
            RETURN.
          ENDIF.
        ENDIF.
        clear_promo( ).
        LEAVE TO SCREEN 0.
      WHEN 'FC_EXIT'.
        LEAVE PROGRAM.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
  METHOD modify_screen.

    LOOP AT SCREEN.
      READ TABLE gt_tabstrip_header ASSIGNING FIELD-SYMBOL(<ls_header>) WITH KEY name = screen-name.
      IF sy-subrc = 0.

        ASSIGN (<ls_header>-name) TO FIELD-SYMBOL(<lv_tab_text>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN (<ls_header>-data_tab) TO FIELD-SYMBOL(<ls_data>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <ls_data> IS INITIAL.
          <lv_tab_text> = <ls_header>-empty.
        ELSE.
          <lv_tab_text> = <ls_header>-full.
        ENDIF.
      ENDIF.

      CASE screen-group1.
        WHEN 'HD'.
          IF mv_state = gc_state-view.
            screen-input = 0.
          ELSE.
            screen-input = 1.
          ENDIF.

          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_result.

    es_promo = ms_promo_header.
    ev_saved = mv_saved.

  ENDMETHOD.
  METHOD update_fcat.

    FIELD-SYMBOLS: <lo_grid>         TYPE REF TO cl_gui_alv_grid.
*                   <lt_fieldcatalog> TYPE lvc_t_fcat.

    DATA(lv_tabstrip_name) = g_ts_promo-pressed_tab+3.
*    DATA(lv_fieldcatalog) = |MT_{ lv_tabstrip_name }_FCAT|.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.
*    DATA(lv_fcat_method_name) = |{ lv_tabstrip_name }_FIELDCATALOG|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

*    ASSIGN (lv_fieldcatalog) TO <lt_fieldcatalog>.
*    IF sy-subrc <> 0.
*      RETURN.
*    ENDIF.

*    CALL METHOD (lv_fcat_method_name)
*      EXPORTING
*        iv_edit = COND abap_bool( WHEN mv_state = gc_state-view
*                                  THEN abap_false
*                                  ELSE abap_true ).
*
*    <lo_grid>->set_frontend_fieldcatalog( it_fieldcatalog = <lt_fieldcatalog> ).

    IF mv_state = gc_state-view.
      <lo_grid>->set_ready_for_input( 0 ).

    ELSE.
      <lo_grid>->set_ready_for_input( 1 ).
    ENDIF.

    <lo_grid>->refresh_table_display(
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD check_changed_data.

    FIELD-SYMBOLS: <lo_grid>         TYPE REF TO cl_gui_alv_grid.

    DATA(lv_tabstrip_name) = mv_last_tab+3.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->check_changed_data( ).
  ENDMETHOD.
  METHOD ts_pbo.

    DATA: ls_layout            TYPE lvc_s_layo,
          lt_toolbar_excluding TYPE ui_functions.

    FIELD-SYMBOLS: <lo_container>    TYPE REF TO cl_gui_custom_container,
                   <lo_grid>         TYPE REF TO cl_gui_alv_grid,
                   <lt_outtab>       TYPE ANY TABLE,
                   <lt_fieldcatalog> TYPE lvc_t_fcat.

    DATA(lv_tabstrip_name) = g_ts_promo-pressed_tab+3.

    DATA(lv_container_name) = |MO_{ lv_tabstrip_name }_CONTAINER|.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.
    DATA(lv_data_tab_name) = |MT_{ lv_tabstrip_name }|.
    DATA(lv_fcat_name) = |MT_{ lv_tabstrip_name }_FCAT|.
    DATA(lv_fcat_method_name) = |{ lv_tabstrip_name }_FIELDCATALOG|.

    ASSIGN (lv_container_name) TO <lo_container>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_data_tab_name) TO <lt_outtab>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_fcat_name) TO <lt_fieldcatalog>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF <lo_container> IS BOUND.
      <lo_grid>->refresh_table_display(
        EXCEPTIONS
          finished       = 1                " Display was Ended (by Export)
          OTHERS         = 2
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      RETURN.

    ENDIF.

    <lo_container> = NEW cl_gui_custom_container(
      container_name = |{ lv_tabstrip_name }_CONTAINER|
    ).

    <lo_grid> = NEW cl_gui_alv_grid(
      i_parent         = <lo_container>
    ).

    ls_layout = VALUE #( cwidth_opt = abap_true
                         sel_mode = 'D' ).

    lt_toolbar_excluding = VALUE #( ( '&LOCAL&CUT' )
                                   ( '&LOCAL&COPY'      )
*                                   ( '&LOCAL&PASTE'     )
                                   ( '&LOCAL&UNDO'      )
                                   ( '&&SEP00'          )
                                   ( '&CHECK'           )
                                   ( '&REFRESH'         )
                                   ( '&&SEP02'          )
                                   ( '&LOCAL&APPEND'    )
                                   ( '&LOCAL&INSERT_ROW')
                                   ( '&LOCAL&DELETE_ROW')
                                   ( '&LOCAL&COPY_ROW'  )
*                                   ( '&LOCAL&PASTE_NEW_ROW' )
                                   ( '&&SEP03'          )
                                   ( '&MB_SUM'          )
                                   ( '&MB_SUBTOT'       )
                                   ( '&&SEP05'          )
                                   ( '&PRINT_BACK'      )
                                   ( '&MB_VIEW'         )
                                   ( '&GRAPH'           )
                                   ( '&&SEP07'          )
                                   ( '&INFO'            ) ).

    CALL METHOD (lv_fcat_method_name).
*      EXPORTING
*        iv_edit = COND abap_bool( WHEN mv_state = gc_state-view
*                                  THEN abap_false
*                                  ELSE abap_true ).

    SET HANDLER on_user_command FOR <lo_grid>.
    SET HANDLER on_data_changed FOR <lo_grid>.
    SET HANDLER on_hotspot FOR <lo_grid>.
    SET HANDLER grid_toolbar FOR <lo_grid>.

    <lo_grid>->register_edit_event(
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

    <lo_grid>->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
        it_toolbar_excluding          = lt_toolbar_excluding
      CHANGING
        it_outtab                     = <lt_outtab>
        it_fieldcatalog               = <lt_fieldcatalog>
      EXCEPTIONS
        invalid_parameter_combination = 1                " Wrong Parameter
        program_error                 = 2                " Program Errors
        too_many_lines                = 3                " Too many Rows in Ready for Input Grid
        OTHERS                        = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD on_user_command.

    DATA: lt_selected_rows TYPE lvc_t_roid.

    FIELD-SYMBOLS: <lo_grid>  TYPE REF TO cl_gui_alv_grid,
                   <lt_table> TYPE ANY TABLE.

    check_header_data( ).

    IF mt_messages_header IS NOT INITIAL.
      MESSAGE TEXT-e07 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    DATA(lv_tabstrip_name) = CONV string( g_ts_promo-pressed_tab+3 ).

    DATA(lv_data_tab_name) = |MT_{ lv_tabstrip_name }|.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->get_selected_rows(
      IMPORTING
        et_row_no     = lt_selected_rows
    ).

    CASE e_ucomm.
      WHEN 'FC_INSERT_ROW'.
        insert_row( iv_tab = lv_tabstrip_name ).
      WHEN 'FC_DELETE_ROW'.
        IF lt_selected_rows IS INITIAL.
          MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        delete_rows( iv_tab = lv_tabstrip_name
                     it_selected_rows = lt_selected_rows ).
      WHEN 'FC_DELETE_ALL'.
        ASSIGN (lv_data_tab_name) TO <lt_table>.
        IF sy-subrc = 0.
          LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table_row>).
            APPEND VALUE #( row_id = sy-tabix ) TO lt_selected_rows.
          ENDLOOP.
          SORT lt_selected_rows BY row_id DESCENDING.
          delete_rows( iv_tab = lv_tabstrip_name
                       it_selected_rows = lt_selected_rows ).
        ENDIF.
    ENDCASE.

    cl_gui_cfw=>set_new_ok_code( 'DUMMY' ).
  ENDMETHOD.
  METHOD on_data_changed.

    check_header_data( ).

    IF mt_messages_header IS NOT INITIAL.
      MESSAGE TEXT-e07 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cells>).

      CASE <ls_mod_cells>-fieldname.
        WHEN 'MATNR'.

          SELECT SINGLE maktx
            FROM makt
            INTO @DATA(lv_maktx)
            WHERE matnr = @<ls_mod_cells>-value
              AND spras = @sy-langu.

          er_data_changed->modify_cell(
             EXPORTING
               i_row_id    = <ls_mod_cells>-row_id
               i_fieldname = 'MATNR_DESC'
               i_value     = lv_maktx
           ).
          CLEAR lv_maktx.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
  METHOD on_hotspot.
  ENDMETHOD.
  METHOD grid_toolbar.
    DATA: lt_add_func TYPE ttb_button.

    IF mv_state = gc_state-view.
      RETURN.
    ENDIF.

    lt_add_func = VALUE #( ( function = 'FC_INSERT_ROW' icon = icon_insert_row text = 'Insert' )
                           ( function = 'FC_DELETE_ROW' icon = icon_delete_row text = 'Delete Rows' )
                           ( function = 'FC_DELETE_ALL' icon = icon_delete text = 'Remove ALL' ) ).

    APPEND LINES OF lt_add_func TO e_object->mt_toolbar.
  ENDMETHOD.
  METHOD model_fieldcatalog.

    mt_model_fcat = VALUE #( ( fieldname = 'MATNR'      ref_table = 'ZLPBNS_PR_MODEL' ref_field = 'MATNR'   edit = abap_true )
                             ( fieldname = 'MATNR_DESC' ref_table = 'MAKT'            ref_field = 'MAKTX' )
                             ( fieldname = 'PERC_AM'    ref_table = 'ZLPBNS_PR_MODEL' ref_field = 'PERC_AM' edit = abap_true )
                             ( fieldname = 'AMOUNT'     ref_table = 'ZLPBNS_PR_MODEL' ref_field = 'AMOUNT'  cfieldname = 'CURR' edit = abap_true )
                             ( fieldname = 'CURR'       ref_table = 'ZLPBNS_PR_MODEL' ref_field = 'CURR'    edit = abap_true ) ).
  ENDMETHOD.
  METHOD chassis_fieldcatalog.

    mt_chassis_fcat = VALUE #( ( fieldname = 'VHVIN'      ref_table = 'ZLPBNS_PR_VIN' ref_field = 'VHVIN'   edit = abap_true )
                               ( fieldname = 'PERC_AM'    ref_table = 'ZLPBNS_PR_VIN' ref_field = 'PERC_AM' edit = abap_true )
                               ( fieldname = 'AMOUNT'     ref_table = 'ZLPBNS_PR_VIN' ref_field = 'AMOUNT'  cfieldname = 'CURR' edit = abap_true )
                               ( fieldname = 'CURR'       ref_table = 'ZLPBNS_PR_VIN' ref_field = 'CURR'    edit = abap_true ) ).

  ENDMETHOD.
  METHOD fleet_fieldcatalog.

    mt_fleet_fcat = VALUE #( ( fieldname = 'FLEET'      ref_table = 'ZLPBNS_PR_FLEET' ref_field = 'FLEET'      edit = abap_true )
                             ( fieldname = 'FLEET_DESC' ref_table = 'ZLPBNS_PR_FLEET' ref_field = 'FLEET_DESC' edit = abap_true ) ).

  ENDMETHOD.
  METHOD vin_fieldcatalog.

    mt_vin_fcat = VALUE #( ( fieldname = 'VHVIN'      ref_table = 'ZLPBNS_PR_VIN_EX' ref_field = 'VHVIN'   edit = abap_true ) ).

  ENDMETHOD.
  METHOD incomp_fieldcatalog.

    mt_incomp_fcat = VALUE #( ( fieldname = 'PROMO_INC'      ref_table = 'ZLPBNS_PR_INCOM' ref_field = 'PROMO_INC'   edit = abap_true )
                              ( fieldname = 'DESC' scrtext_s = TEXT-h03 scrtext_m = TEXT-h04 ) ).

  ENDMETHOD.
  METHOD save.

    check_changed_data( ).

    check_header_data( ).

    check_item_data( ).

    display_messages( ).

    IF mt_messages IS NOT INITIAL.
      MESSAGE TEXT-e08 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE mv_state.
      WHEN gc_state-new OR gc_state-copy.
        IF mv_saved = abap_true.
          RETURN.
        ENDIF.
        create_entries( ).
      WHEN gc_state-edit OR gc_state-view.
        update_entries( ).
    ENDCASE.

    display_messages( ).

    IF mt_messages IS NOT INITIAL.
      ROLLBACK WORK.
      MESSAGE s005(zlpbns_bonus) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    COMMIT WORK.
    mv_saved = abap_true.

    CASE mv_state.
      WHEN gc_state-new OR gc_state-copy.
        MESSAGE |Promo { ms_promo_header-promo } { TEXT-s01 }| TYPE 'S'.
      WHEN gc_state-edit OR gc_state-view.
        MESSAGE |Promo { ms_promo_header-promo } { TEXT-s02 }| TYPE 'S'.
    ENDCASE.

    LEAVE TO SCREEN 0.

  ENDMETHOD.
  METHOD create_entries.

    DATA: ls_promo_mdh TYPE zlpbns_promo_mdh,
          ls_model     TYPE zlpbns_pr_model,
          ls_chassis   TYPE zlpbns_pr_vin,
          ls_fleet     TYPE zlpbns_pr_fleet,
          ls_vin       TYPE zlpbns_pr_vin_ex,
          ls_incomp    TYPE zlpbns_pr_incom.

* Create the promo code
    SELECT MAX( promo )
      FROM zlpbns_promo_mdh
      INTO @DATA(lv_last_promo).

    IF sy-subrc = 0.
      mv_promo_code = ms_promo_header-promo = CONV char3( CONV i( lv_last_promo ) + 1 ). " To be changed
    ELSE.
      mv_promo_code = ms_promo_header-promo = '000'.
    ENDIF.

    ls_promo_mdh = CORRESPONDING #( ms_promo_header ).
    ls_promo_mdh-promo_desc = ms_promo_header-promo_type_desc.

    INSERT zlpbns_promo_mdh FROM ls_promo_mdh.
    IF sy-subrc <> 0.
      add_message(
        EXPORTING
          iv_msgno    = '003'
          iv_msgty    = 'E'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_model> ).
      ls_model-market = ms_promo_header-market.
      ls_model-promo = ms_promo_header-promo.

      INSERT zlpbns_pr_model FROM ls_model.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Model'
          iv_msgv2    = 'material'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_model-matnr ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_chassis ASSIGNING FIELD-SYMBOL(<ls_chassis>).
      CLEAR ls_chassis.
      ls_chassis = CORRESPONDING #( <ls_chassis> ).
      ls_chassis-market = ms_promo_header-market.
      ls_chassis-promo = ms_promo_header-promo.

      INSERT zlpbns_pr_vin FROM ls_chassis.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Chassis'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_chassis-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_fleet ASSIGNING FIELD-SYMBOL(<ls_fleet>).
      CLEAR ls_fleet.
      ls_fleet = CORRESPONDING #( <ls_fleet> ).
      ls_fleet-market = ms_promo_header-market.
      ls_fleet-promo = ms_promo_header-promo.

      INSERT zlpbns_pr_fleet FROM ls_fleet.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Fleet'
          iv_msgv2    = 'fleet'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_fleet-fleet ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_vin> ).
      ls_vin-market = ms_promo_header-market.
      ls_vin-promo = ms_promo_header-promo.

      INSERT zlpbns_pr_vin_ex FROM ls_vin.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Vin exclusion'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_vin-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_incomp ASSIGNING FIELD-SYMBOL(<ls_incomp>).
      CLEAR ls_incomp.
      ls_incomp = CORRESPONDING #( <ls_incomp> ).
      ls_incomp-market = ms_promo_header-market.
      ls_incomp-promo = ms_promo_header-promo.

      INSERT zlpbns_pr_incom FROM ls_incomp.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Incompatible promo'
          iv_msgv2    = 'promo'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_incomp-promo_inc ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD update_entries.

    DATA: ls_promo_mdh TYPE zlpbns_promo_mdh,
          ls_model     TYPE zlpbns_pr_model,
          ls_chassis   TYPE zlpbns_pr_vin,
          ls_fleet     TYPE zlpbns_pr_fleet,
          ls_vin       TYPE zlpbns_pr_vin_ex,
          ls_incomp    TYPE zlpbns_pr_incom.

    ls_promo_mdh = CORRESPONDING #( ms_promo_header ).
    ls_promo_mdh-promo_desc = ms_promo_header-promo_type_desc.
    ls_promo_mdh-modname = ms_promo_header-modname = sy-uname.
    ls_promo_mdh-moddate = ms_promo_header-moddate = sy-datum.
    ls_promo_mdh-modtime = ms_promo_header-modtime = sy-uzeit.

    SELECT SINGLE valid_f,
                  valid_t,
                  zextend
      FROM zlpbns_promo_mdh
      INTO @DATA(ls_promo_mdh_old)
      WHERE vkorg = @p_vkorg
        AND market = @ms_promo_header-market
        AND promo = @ms_promo_header-promo.

    IF ls_promo_mdh_old-zextend IS INITIAL.
      IF ls_promo_mdh_old-valid_f <> ls_promo_mdh-valid_f OR
         ls_promo_mdh_old-valid_t <> ls_promo_mdh-valid_t.
        ls_promo_mdh-zextend = abap_true.
      ENDIF.
    ENDIF.

    UPDATE zlpbns_promo_mdh FROM ls_promo_mdh.
    IF sy-subrc <> 0.
      add_message(
        EXPORTING
          iv_msgno    = '006'
          iv_msgty    = 'E'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_model> ).
      ls_model-market = ms_promo_header-market.
      ls_model-promo = ms_promo_header-promo.

      MODIFY zlpbns_pr_model FROM ls_model.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '007'
          iv_msgty    = 'E'
          iv_msgv1    = 'Model'
          iv_msgv2    = 'material'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_model-matnr ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_model ASSIGNING FIELD-SYMBOL(<ls_deleted_model>).
      CLEAR ls_model.
      ls_model = CORRESPONDING #( <ls_deleted_model> ).
      ls_model-market = ms_promo_header-market.
      ls_model-promo = ms_promo_header-promo.

      DELETE zlpbns_pr_model FROM ls_model.

      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '013'
          iv_msgty    = 'E'
          iv_msgv1    = 'Model'
          iv_msgv2    = 'material'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_model-matnr ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_chassis ASSIGNING FIELD-SYMBOL(<ls_chassis>).
      CLEAR ls_chassis.
      ls_chassis = CORRESPONDING #( <ls_chassis> ).
      ls_chassis-market = ms_promo_header-market.
      ls_chassis-promo = ms_promo_header-promo.

      MODIFY zlpbns_pr_vin FROM ls_chassis.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '007'
          iv_msgty    = 'E'
          iv_msgv1    = 'Chassis'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_chassis-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_chassis ASSIGNING FIELD-SYMBOL(<ls_deleted_chassis>).
      CLEAR ls_chassis.
      ls_chassis = CORRESPONDING #( <ls_deleted_chassis> ).
      ls_chassis-market = ms_promo_header-market.
      ls_chassis-promo = ms_promo_header-promo.

      DELETE zlpbns_pr_vin FROM ls_chassis.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '013'
          iv_msgty    = 'E'
          iv_msgv1    = 'Chassis'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_chassis-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_fleet ASSIGNING FIELD-SYMBOL(<ls_fleet>).
      CLEAR ls_fleet.
      ls_fleet = CORRESPONDING #( <ls_fleet> ).
      ls_fleet-market = ms_promo_header-market.
      ls_fleet-promo = ms_promo_header-promo.

      MODIFY zlpbns_pr_fleet FROM ls_fleet.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '007'
          iv_msgty    = 'E'
          iv_msgv1    = 'Fleet'
          iv_msgv2    = 'fleet'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_fleet-fleet ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_fleet ASSIGNING FIELD-SYMBOL(<ls_deleted_fleet>).
      CLEAR ls_fleet.
      ls_fleet = CORRESPONDING #( <ls_deleted_fleet> ).
      ls_fleet-market = ms_promo_header-market.
      ls_fleet-promo = ms_promo_header-promo.

      DELETE zlpbns_pr_fleet FROM ls_fleet.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '013'
          iv_msgty    = 'E'
          iv_msgv1    = 'Fleet'
          iv_msgv2    = 'fleet'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_fleet-fleet ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    SELECT *
      FROM zlpbns_pr_vin_ex
      INTO TABLE @DATA(lt_excluded_vin)
      WHERE promo = @ms_promo_header-promo
        AND market = @ms_promo_header-market
      ORDER BY vhvin.

    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_vin> ).
      ls_vin-market = ms_promo_header-market.
      ls_vin-promo = ms_promo_header-promo.

      READ TABLE lt_excluded_vin TRANSPORTING NO FIELDS WITH KEY vhvin = ls_vin-vhvin BINARY SEARCH.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      INSERT zlpbns_pr_vin_ex FROM ls_vin.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Vin exclusion'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_vin-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_vin ASSIGNING FIELD-SYMBOL(<ls_deleted_vin>).
      CLEAR ls_vin.
      ls_vin = CORRESPONDING #( <ls_deleted_vin> ).
      ls_vin-market = ms_promo_header-market.
      ls_vin-promo = ms_promo_header-promo.

      DELETE zlpbns_pr_vin_ex FROM ls_vin.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '013'
          iv_msgty    = 'E'
          iv_msgv1    = 'Vin exclusion'
          iv_msgv2    = 'vin'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_vin-vhvin ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    SELECT *
     FROM zlpbns_pr_incom
     INTO TABLE @DATA(lt_incom)
     WHERE promo = @ms_promo_header-promo
       AND market = @ms_promo_header-market
     ORDER BY promo_inc.

    LOOP AT mt_incomp ASSIGNING FIELD-SYMBOL(<ls_incomp>).
      CLEAR ls_incomp.
      ls_incomp = CORRESPONDING #( <ls_incomp> ).
      ls_incomp-market = ms_promo_header-market.
      ls_incomp-promo = ms_promo_header-promo.

      READ TABLE lt_incom TRANSPORTING NO FIELDS WITH KEY promo_inc = ls_incomp-promo_inc.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      INSERT zlpbns_pr_incom FROM ls_incomp.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '004'
          iv_msgty    = 'E'
          iv_msgv1    = 'Incompatible promo'
          iv_msgv2    = 'promo'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_incomp-promo_inc ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

    LOOP AT mt_deleted_incomp ASSIGNING FIELD-SYMBOL(<ls_deleted_incomp>).
      CLEAR ls_incomp.
      ls_incomp = CORRESPONDING #( <ls_deleted_incomp> ).
      ls_incomp-market = ms_promo_header-market.
      ls_incomp-promo = ms_promo_header-promo.

      DELETE zlpbns_pr_incom FROM ls_incomp.
      IF sy-subrc <> 0.
        add_message(
        EXPORTING
          iv_msgno    = '013'
          iv_msgty    = 'E'
          iv_msgv1    = 'Incompatible promo'
          iv_msgv2    = 'promo'
          iv_msgv3    = condense( val = CONV sy-msgv3( ls_incomp-promo_inc ) to = '' )
        CHANGING
          ct_messages = mt_messages_item
      ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD insert_row.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO <lt_table>.

    refresh_tabstrip_alv( ).

  ENDMETHOD.
  METHOD delete_rows.

    DATA: lo_line_type TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table>         TYPE STANDARD TABLE,
                   <lt_deleted_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.
    DATA(lv_deleted_data_tab_name) = |MT_DELETED_{ iv_tab }|.
    DATA(lv_data_type) = |TY_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_deleted_data_tab_name) TO <lt_deleted_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA lo_line_type TYPE (lv_data_type).
    ASSIGN lo_line_type->* TO FIELD-SYMBOL(<ls_line>).

    LOOP AT it_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_rows>).

      CLEAR <ls_line>.
      READ TABLE <lt_table> INTO <ls_line> INDEX <ls_selected_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'SAVED' OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_saved>).
      IF sy-subrc = 0 AND <lv_saved> = abap_true.
        APPEND <ls_line> TO <lt_deleted_table>. "Append only entries saved in DB to the deletion table
      ENDIF.

      DELETE <lt_table> INDEX <ls_selected_rows>-row_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    refresh_tabstrip_alv( ).
  ENDMETHOD.
  METHOD delete_all.

    DATA: lo_line_type TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table>         TYPE STANDARD TABLE,
                   <lt_deleted_table> TYPE STANDARD TABLE.

    DATA(lv_data_tab_name) = |MT_{ iv_tab }|.
    DATA(lv_deleted_data_tab_name) = |MT_DELETED_{ iv_tab }|.
    DATA(lv_data_type) = |TY_{ iv_tab }|.

    ASSIGN (lv_data_tab_name) TO <lt_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN (lv_deleted_data_tab_name) TO <lt_deleted_table>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CREATE DATA lo_line_type TYPE (lv_data_type).
    ASSIGN lo_line_type->* TO FIELD-SYMBOL(<ls_line>).

    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).

      ASSIGN COMPONENT 'SAVED' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_saved>).
      IF sy-subrc = 0 AND <lv_saved> = abap_true.
        APPEND <ls_table> TO <lt_deleted_table>. "Append only entries saved in DB to the deletion table
      ENDIF.
    ENDLOOP.

    CLEAR <lt_table>.

    refresh_tabstrip_alv( ).
  ENDMETHOD.
  METHOD refresh_tabstrip_alv.

    FIELD-SYMBOLS: <lo_grid> TYPE REF TO cl_gui_alv_grid.

    DATA(lv_tabstrip_name) = g_ts_promo-pressed_tab+3.
    DATA(lv_grid_name) = |MO_{ lv_tabstrip_name }|.

    ASSIGN (lv_grid_name) TO <lo_grid>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    <lo_grid>->refresh_table_display(
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  METHOD add_message.

    APPEND VALUE #( msgid = 'ZLPBNS_BONUS'
                    msgty = iv_msgty
                    msgno = iv_msgno
                    msgv1 = iv_msgv1
                    msgv2 = iv_msgv2
                    msgv3 = iv_msgv3
                    msgv4 = iv_msgv4 ) TO ct_messages.

  ENDMETHOD.
  METHOD check_header_data.

    CLEAR mt_messages_header.

    IF ms_promo_header-market IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Market'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-promo_type IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Promo Type'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-promo_type_desc IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Promo desc'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-znsc_selltyp IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Sell type'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-trade_in IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Trade in'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-financing IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Financing'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-wallbox IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Wallbox'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-perc IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Percentage'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-valid_f IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Valid from'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-valid_t IS INITIAL.
      add_message(
        EXPORTING
          iv_msgno = '001'
          iv_msgty = 'E'
          iv_msgv1 = 'Valid to'
        CHANGING
          ct_messages = mt_messages_header
      ).
    ENDIF.

    IF ms_promo_header-valid_f IS NOT INITIAL AND
       ms_promo_header-valid_t IS NOT INITIAL.

      IF ms_promo_header-valid_f > ms_promo_header-valid_t.
        add_message(
        EXPORTING
          iv_msgno = '002'
          iv_msgty = 'E'
        CHANGING
          ct_messages = mt_messages_header
      ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD check_item_data.

    DATA: lv_tabix TYPE sy-tabix.

    SELECT *
      FROM tcurx
      INTO TABLE @DATA(lt_tcurx).

    LOOP AT mt_model ASSIGNING FIELD-SYMBOL(<ls_model>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        add_message(
          EXPORTING
            iv_msgno    = '000'
            iv_msgty    = 'W'
            iv_msgv1    = 'Model table errors'
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_model> IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '010'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          CHANGING
            ct_messages = mt_messages_item
        ).
        CONTINUE.
      ENDIF.

      IF <ls_model>-matnr IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Material'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_model>-perc_am IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF <ls_model>-amount IS INITIAL AND <ls_model>-perc_am IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '009'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Amount'|
            iv_msgv3    = |'Percentage'|
          CHANGING
            ct_messages = mt_messages_item
        ).
        CONTINUE.
      ENDIF.

      IF <ls_model>-amount IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Amount'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_model>-curr IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Currency'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      READ TABLE lt_tcurx TRANSPORTING NO FIELDS WITH KEY currkey = <ls_model>-curr.

      IF sy-subrc <> 0.
        add_message(
          EXPORTING
            iv_msgno    = '011'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'{ <ls_model>-curr }'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.
    ENDLOOP.


    DATA(lt_model) = mt_model.
    SORT lt_model BY matnr.

    LOOP AT lt_model ASSIGNING <ls_model>.
      DATA(lv_next_index) = sy-tabix + 1.
      READ TABLE lt_model ASSIGNING FIELD-SYMBOL(<ls_mod_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_mod_row>-matnr = <ls_model>-matnr.
        add_message(
          EXPORTING
            iv_msgno    = '014'
            iv_msgty    = 'E'
            iv_msgv1    = CONV syst_msgv( <ls_model>-matnr )
          CHANGING
            ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_chassis ASSIGNING FIELD-SYMBOL(<ls_chassis>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        add_message(
          EXPORTING
            iv_msgno    = '000'
            iv_msgty    = 'W'
            iv_msgv1    = 'Chassis table errors'
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_chassis> IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '010'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          CHANGING
            ct_messages = mt_messages_item
        ).
        CONTINUE.
      ENDIF.

      IF <ls_chassis>-vhvin IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Vin'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_chassis>-perc_am IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF <ls_chassis>-amount IS INITIAL AND <ls_chassis>-perc_am IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '009'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Amount'|
            iv_msgv3    = |'Percentage'|
          CHANGING
            ct_messages = mt_messages_item
        ).
        CONTINUE.
      ENDIF.

      IF <ls_chassis>-amount IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Amount'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_chassis>-curr IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Currency'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      READ TABLE lt_tcurx TRANSPORTING NO FIELDS WITH KEY currkey = <ls_model>-curr.

      IF sy-subrc <> 0.
        add_message(
          EXPORTING
            iv_msgno    = '011'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'{ <ls_model>-curr }'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.
    ENDLOOP.

    DATA(lt_chassis) = mt_chassis.
    CLEAR lv_next_index.
    SORT lt_chassis BY vhvin.

    LOOP AT lt_chassis ASSIGNING <ls_chassis>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_chassis ASSIGNING FIELD-SYMBOL(<ls_chas_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_chas_row>-vhvin = <ls_chassis>-vhvin.
        add_message(
          EXPORTING
            iv_msgno    = '014'
            iv_msgty    = 'E'
            iv_msgv1    = CONV syst_msgv( <ls_chassis>-vhvin )
          CHANGING
            ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_fleet ASSIGNING FIELD-SYMBOL(<ls_fleet>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        add_message(
          EXPORTING
            iv_msgno    = '000'
            iv_msgty    = 'W'
            iv_msgv1    = 'Fleet table errors'
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_fleet> IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '010'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          CHANGING
            ct_messages = mt_messages_item
        ).
        CONTINUE.
      ENDIF.

      IF <ls_fleet>-fleet IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Fleet'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_fleet>-fleet_desc IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '008'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
            iv_msgv2    = |'Description'|
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.
    ENDLOOP.

    DATA(lt_fleet) = mt_fleet.
    CLEAR lv_next_index.
    SORT lt_fleet BY fleet.

    LOOP AT lt_fleet ASSIGNING <ls_fleet>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_fleet ASSIGNING FIELD-SYMBOL(<ls_fleet_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_fleet>-fleet = <ls_fleet_row>-fleet.
        add_message(
          EXPORTING
            iv_msgno    = '014'
            iv_msgty    = 'E'
            iv_msgv1    = CONV syst_msgv( <ls_fleet>-fleet )
          CHANGING
            ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        add_message(
          EXPORTING
            iv_msgno    = '000'
            iv_msgty    = 'W'
            iv_msgv1    = 'Vin exclusion table errors'
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_vin> IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '010'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.
    ENDLOOP.

    DATA(lt_vin) = mt_vin.
    CLEAR lv_next_index.
    SORT lt_vin BY vhvin.

    LOOP AT lt_vin ASSIGNING <ls_vin>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_vin ASSIGNING FIELD-SYMBOL(<ls_vin_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_vin>-vhvin = <ls_vin_row>-vhvin.
        add_message(
          EXPORTING
            iv_msgno    = '014'
            iv_msgty    = 'E'
            iv_msgv1    = CONV syst_msgv( <ls_vin>-vhvin )
          CHANGING
            ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR lv_tabix.
    LOOP AT mt_incomp ASSIGNING FIELD-SYMBOL(<ls_incomp>).
      lv_tabix = sy-tabix.
      IF lv_tabix = 1.
        add_message(
          EXPORTING
            iv_msgno    = '000'
            iv_msgty    = 'W'
            iv_msgv1    = 'Incompatile promo table errors'
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.

      IF <ls_incomp> IS INITIAL.
        add_message(
          EXPORTING
            iv_msgno    = '010'
            iv_msgty    = 'E'
            iv_msgv1    = condense( val = CONV sy-msgv1( lv_tabix ) to = '' )
          CHANGING
            ct_messages = mt_messages_item
        ).
      ENDIF.
    ENDLOOP.

    DATA(lt_incomp) = mt_incomp.
    CLEAR lv_next_index.
    SORT lt_incomp BY promo_inc.

    LOOP AT lt_incomp ASSIGNING <ls_incomp>.
      lv_next_index = sy-tabix + 1.
      READ TABLE lt_incomp ASSIGNING FIELD-SYMBOL(<ls_incomp_row>) INDEX lv_next_index.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <ls_incomp>-promo_inc = <ls_incomp_row>-promo_inc.
        add_message(
          EXPORTING
            iv_msgno    = '014'
            iv_msgty    = 'E'
            iv_msgv1    = CONV syst_msgv( <ls_incomp>-promo_inc )
          CHANGING
            ct_messages = mt_messages_item ).
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD display_messages.

    CLEAR mt_messages.

    IF mt_messages_header IS NOT INITIAL.
      add_message(
        EXPORTING
          iv_msgno    = '000'
          iv_msgty    = 'W'
          iv_msgv1    = 'Header messages'
        CHANGING
          ct_messages = mt_messages
       ).

      APPEND LINES OF mt_messages_header TO mt_messages.
      CLEAR mt_messages_header.
    ENDIF.

    LOOP AT mt_messages_item TRANSPORTING NO FIELDS WHERE msgty CA 'E'.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      add_message(
        EXPORTING
          iv_msgno    = '000'
          iv_msgty    = 'W'
          iv_msgv1    = 'Item messages'
        CHANGING
          ct_messages = mt_messages
       ).

      APPEND LINES OF mt_messages_item TO mt_messages.
      CLEAR mt_messages_item.
    ENDIF.

    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = mt_messages.

  ENDMETHOD.
  METHOD clear_promo.

    IF iv_clear_header IS NOT INITIAL.
      CLEAR ms_promo_header.
    ENDIF.

    CLEAR: mt_model,
           mt_fleet,
           mt_chassis,
           mt_vin,
           mt_incomp,
           mt_deleted_model,
           mt_deleted_chassis,
           mt_deleted_fleet,
           mt_deleted_vin,
           mt_deleted_incomp,
           mt_messages,
           mt_messages_header,
           mt_messages_item,
           mv_last_tab,
           mv_saved.
  ENDMETHOD.
  METHOD fill_tables.
    SELECT m~matnr,
     m~perc_am,
     m~amount,
     m~curr,
     k~maktx AS matnr_desc,
     @abap_true AS saved
FROM zlpbns_pr_model AS m
      LEFT JOIN makt AS k ON k~matnr = m~matnr
INTO CORRESPONDING FIELDS OF TABLE @mt_model
WHERE promo = @ms_promo_header-promo
  AND market = @ms_promo_header-market.

    SELECT vhvin,
           perc_am,
           amount,
           curr,
           @abap_true AS saved
      FROM zlpbns_pr_vin
      INTO CORRESPONDING FIELDS OF TABLE @mt_chassis
      WHERE promo = @ms_promo_header-promo
        AND market = @ms_promo_header-market.

    SELECT fleet,
           fleet_desc,
           @abap_true AS saved
     FROM zlpbns_pr_fleet
     INTO CORRESPONDING FIELDS OF TABLE @mt_fleet
     WHERE promo = @ms_promo_header-promo
       AND market = @ms_promo_header-market.

    SELECT vhvin,
           @abap_true AS saved
      FROM zlpbns_pr_vin_ex
      INTO CORRESPONDING FIELDS OF TABLE @mt_vin
      WHERE promo = @ms_promo_header-promo
        AND market = @ms_promo_header-market.

    SELECT i~promo_inc,
           h~promo_desc AS desc,
           @abap_true AS saved
      FROM zlpbns_pr_incom AS i
      LEFT JOIN zlpbns_promo_mdh AS h ON h~promo = i~promo
      INTO CORRESPONDING FIELDS OF TABLE @mt_incomp
      WHERE i~promo = @ms_promo_header-promo
        AND i~market = @ms_promo_header-market.

  ENDMETHOD.

  METHOD popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Unsaved data will be lost. Do you want to continue?'
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
ENDCLASS.

DATA: go_promo           TYPE REF TO lcl_promo.

CLASS lcl_zlpbns_promo_md DEFINITION.
  PUBLIC SECTION.

    DATA: mv_ok_code         TYPE sy-ucomm.

    CONSTANTS: mc_header_table(30) TYPE c VALUE 'ZLPBNS_PROMO_MDH'.

    METHODS execute.
    METHODS pai.
    METHODS pbo_0100.
    METHODS pbo_0300.
    METHODS pbo_0400.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_model,
             market        TYPE zlpbns_promo_mdh-market,
             promo         TYPE zlpbns_promo_mdh-promo,
             matnr         TYPE zlpbns_pr_model-matnr,
             descr_veicolo TYPE zrsdap_psa_pc016-descr_veicolo,
             perc_am       TYPE zlpbns_pr_model-perc_am,
             amount        TYPE zlpbns_pr_model-amount,
             curr          TYPE zlpbns_pr_model-curr,
           END OF ty_model.

    TYPES: BEGIN OF ty_vin,
             market           TYPE zlpbns_promo_mdh-market,
             promo            TYPE zlpbns_promo_mdh-promo,
             vhvin            TYPE zlpbns_pr_vin-vhvin,
             zmmw0_descr_veic TYPE zmmw0_descr_veic,
             perc_am          TYPE zlpbns_pr_model-perc_am,
             amount           TYPE zlpbns_pr_model-amount,
             curr             TYPE zlpbns_pr_model-curr,
           END OF ty_vin.

    DATA: mo_salv_promo      TYPE REF TO cl_salv_table,
          mo_promo_container TYPE REF TO cl_gui_custom_container,
          mt_promo           TYPE STANDARD TABLE OF lcl_promo=>ty_promo.

    DATA: mo_salv_model      TYPE REF TO cl_salv_table,
          mo_model_list_cont TYPE REF TO cl_gui_custom_container,
          mt_model           TYPE STANDARD TABLE OF ty_model.

    DATA: mo_salv_vin      TYPE REF TO cl_salv_table,
          mo_vin_container TYPE REF TO cl_gui_custom_container,
          mt_vin           TYPE STANDARD TABLE OF ty_vin.

    CONSTANTS: BEGIN OF mc_rc,
                 no_data    TYPE i VALUE 1, " No data was found
                 select_one TYPE i VALUE 2, " Select only one row
               END OF mc_rc.

    METHODS get_data.
    METHODS display_alv.
    METHODS salv_functions.
    METHODS salv_events.
    METHODS salv_fieldcatalog.

    METHODS get_model_data IMPORTING it_selected_data TYPE lcl_promo=>tt_promo.
    METHODS get_vin_data IMPORTING it_selected_data TYPE lcl_promo=>tt_promo.

    METHODS new_promo.
    METHODS copy_promo IMPORTING it_selected_data TYPE lcl_promo=>tt_promo.
    METHODS view_promo IMPORTING it_selected_rows  TYPE salv_t_row.

    METHODS create_tabstrip_text_table.
    METHODS get_status_text.
    METHODS append_master_data IMPORTING is_promo TYPE lcl_promo=>ty_promo.
    METHODS adjust_data.

    METHODS on_user_command FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function.
    METHODS on_hotspot FOR EVENT link_click OF cl_salv_events_table IMPORTING column row.

ENDCLASS.

CLASS lcl_zlpbns_promo_md IMPLEMENTATION.
  METHOD execute.

    create_tabstrip_text_table( ).

    get_status_text( ).

    get_data( ).

    adjust_data( ).

    display_alv( ).

    CALL SCREEN 100.
  ENDMETHOD.
  METHOD get_data.

    DATA: lr_valid_f TYPE RANGE OF sy-datum,
          lr_valid_t TYPE RANGE OF sy-datum.

    IF s_valon IS NOT INITIAL.
      lr_valid_f = VALUE #( ( sign = 'I' option = 'LE' low = s_valon[ 1 ]-low ) ).
      lr_valid_t = VALUE #( ( sign = 'I' option = 'GE' low = s_valon[ 1 ]-low ) ).
    ENDIF.

    SELECT md~vkorg,
           md~market,
           md~promo,
           md~promo_type,
           type~promo_type_desc,
           md~znsc_selltyp,
           sell~zzsales_ty_descr AS sell_type_desc,
           md~trade_in,
           md~financing,
           md~perc,
           md~wallbox,
           md~status,
           st~status_desc,
           md~valid_f,
           md~valid_t,
           md~zdraft,
           md~zdele,
           md~zextend,
           md~credate,
           md~crename,
           md~cretime,
           md~moddate,
           md~modname,
           md~modtime,
           fl~fleet,
           model~matnr,
           vin~vhvin

      FROM zlpbns_promo_mdh AS md
      LEFT JOIN zlpbns_pr_fleet AS fl
        ON md~market = fl~market
       AND md~promo = fl~promo
      LEFT JOIN zlpbns_pr_model AS model
        ON md~market = model~market
       AND md~promo = model~promo
      LEFT JOIN zlpbns_pr_vin AS vin
        ON md~market = vin~market
       AND md~promo = vin~promo
      LEFT JOIN zlpbns_pr_type_t AS type
        ON md~promo_type = type~promo_type
        AND type~spras = @sy-langu
      LEFT JOIN zlpbns_pr_stat_t AS st
        ON md~status = st~status
       AND st~spras = @sy-langu
      LEFT JOIN zrsdnsc_selltype AS sell
        ON sell~vkorg = @p_vkorg
       AND md~znsc_selltyp = sell~znsc_selltyp

      INTO TABLE @DATA(lt_promo_temp)

      WHERE md~vkorg = @p_vkorg
        AND md~market = @p_markt
        AND md~promo  IN @s_promo
        AND md~promo_type IN @s_prtyp
        AND md~znsc_selltyp IN @s_sellt
        AND md~trade_in  IN @s_trade
        AND md~wallbox IN @s_wallb
        AND md~status IN @s_statu
        AND md~valid_f  IN @lr_valid_f
        AND md~valid_t  IN @lr_valid_t
        AND md~zextend IN @s_exten
        AND md~zdele   IN @s_zdele
        AND model~matnr   IN @s_matnr
        AND fl~fleet   IN @s_fleet
        AND vin~vhvin  IN @s_vhvin
        AND md~credate IN @s_creon
        AND md~crename   IN @s_creby
        AND md~zdraft  = @p_draft.

    SORT lt_promo_temp BY vkorg market promo.
    DELETE ADJACENT DUPLICATES FROM lt_promo_temp COMPARING vkorg market promo.

    LOOP AT lt_promo_temp ASSIGNING FIELD-SYMBOL(<ls_promo_temp>).

      IF s_fleet IS NOT INITIAL AND <ls_promo_temp>-fleet NOT IN s_fleet.
        CONTINUE.
      ENDIF.

      IF s_matnr IS NOT INITIAL AND <ls_promo_temp>-matnr NOT IN s_matnr.
        CONTINUE.
      ENDIF.

      IF s_vhvin IS NOT INITIAL AND <ls_promo_temp>-vhvin NOT IN s_vhvin.
        CONTINUE.
      ENDIF.

      APPEND CORRESPONDING #( <ls_promo_temp> ) TO mt_promo.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_model_data.

    SELECT mod~market,
           mod~promo,
           mod~matnr,
           dsc~descr_veicolo,
           mod~perc_am,
           mod~amount,
           mod~curr
      FROM zlpbns_pr_model AS mod
      LEFT JOIN zrsdap_psa_pc016 AS dsc
        ON dsc~mercato = mod~market
       AND dsc~matnr = mod~matnr
      INTO TABLE @mt_model
      FOR ALL ENTRIES IN @it_selected_data
      WHERE mod~market = @it_selected_data-market
        AND mod~promo  = @it_selected_data-promo.

  ENDMETHOD.
  METHOD get_vin_data.

    DATA: lt_vhcle TYPE zrsdnr_vhcle_t,
          lt_out   TYPE zst_vlc_vhcle_desc_t.

    CLEAR mt_vin.

    SELECT vin~market,
           vin~promo,
           vin~vhvin,
           vin~perc_am,
           vin~amount,
           vin~curr,
           vlc~vhcle
      FROM zlpbns_pr_vin AS vin
      JOIN vlcvehicle AS vlc
        ON vlc~vhvin = vin~vhvin
      INTO TABLE @DATA(lt_vehicle)
      FOR ALL ENTRIES IN @it_selected_data
      WHERE vin~market = @it_selected_data-market
        AND vin~promo  = @it_selected_data-promo.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_vhcle = VALUE #( FOR ls_vehicle IN lt_vehicle
                        ( vhcle = ls_vehicle-vhcle ) ).

    CALL FUNCTION 'ZRSDNSC_DESC_VHCLE_MAS'
      EXPORTING
        tb_vhcle = lt_vhcle
      TABLES
        tb_out   = lt_out.

    LOOP AT lt_vehicle ASSIGNING FIELD-SYMBOL(<ls_vehicle>).
      READ TABLE lt_out ASSIGNING FIELD-SYMBOL(<ls_out>) WITH KEY zvlc_vhcle = <ls_vehicle>-vhcle.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO mt_vin ASSIGNING FIELD-SYMBOL(<ls_vin>).

      <ls_vin> = CORRESPONDING #( <ls_vehicle> ).
      <ls_vin>-zmmw0_descr_veic = <ls_out>-zmmw0_descr_veic.
    ENDLOOP.

  ENDMETHOD.
  METHOD display_alv.

    IF mo_promo_container IS BOUND.
      RETURN.
    ENDIF.

    mo_promo_container = NEW cl_gui_custom_container( container_name = 'PROMO_CONTAINER' ).

    TRY.
        cl_salv_table=>factory(
      EXPORTING
        r_container    = mo_promo_container
          IMPORTING
            r_salv_table   = mo_salv_promo
          CHANGING
            t_table        = mt_promo
        ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    mo_salv_promo->get_selections( )->set_selection_mode(
        value = if_salv_c_selection_mode=>row_column
    ).

    salv_functions( ).

    salv_events( ).

    salv_fieldcatalog( ).

    mo_salv_promo->display( ).
  ENDMETHOD.
  METHOD pai.

    CASE mv_ok_code.
      WHEN 'FC_BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'FC_EXIT'.
        LEAVE PROGRAM.
    ENDCASE.

  ENDMETHOD.
  METHOD on_user_command.

    DATA: lt_selected_data TYPE lcl_promo=>tt_promo.

    DATA(lt_selected_rows) = mo_salv_promo->get_selections( )->get_selected_rows( ).

    LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<lv_row>).

      READ TABLE mt_promo ASSIGNING FIELD-SYMBOL(<ls_promo>) INDEX <lv_row>.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND <ls_promo> TO lt_selected_data.
    ENDLOOP.

    IF lt_selected_data IS INITIAL AND e_salv_function <> 'NEW'.
      MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE e_salv_function.
      WHEN 'COPY' OR 'VIEW'.
        IF lines( lt_selected_data ) <> 1.
          MESSAGE TEXT-e05 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    CASE e_salv_function.
      WHEN 'NEW'.
        new_promo( ).
      WHEN 'COPY'.
        copy_promo( lt_selected_data ).
      WHEN 'VIEW'.
        view_promo( lt_selected_rows ).
      WHEN 'MODEL'.
        get_model_data( lt_selected_data ).

        IF mt_model IS INITIAL.
          MESSAGE TEXT-e03 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL SCREEN 300.
      WHEN 'VIN'.

        get_vin_data( lt_selected_data ).

        IF mt_vin IS INITIAL.
          MESSAGE TEXT-e04 TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        CALL SCREEN 400.
    ENDCASE.

    mo_salv_promo->refresh( ).
  ENDMETHOD.
  METHOD pbo_0100.

  ENDMETHOD.
  METHOD pbo_0300.

    DATA: lo_column TYPE REF TO cl_salv_column_table.

    IF mo_model_list_cont IS NOT INITIAL.
      mo_salv_model->refresh( ).
      RETURN.
    ENDIF.

    mo_model_list_cont = NEW cl_gui_custom_container( container_name = 'MODEL_LIST_CONTAINER' ).

    IF mo_model_list_cont IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_model_list_cont
          IMPORTING
            r_salv_table   = mo_salv_model
          CHANGING
            t_table        = mt_model
        ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    mo_salv_model->get_functions( )->set_all( ).

    mo_salv_model->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).

    DATA(lo_columns) = mo_salv_model->get_columns( ).
    lo_columns->set_optimize( ).

    TRY.

        lo_column ?= lo_columns->get_column( 'MARKET' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'PROMO' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'MATNR' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'VALUE' ).
        lo_column->set_currency_column( 'CURR' ).

      CATCH cx_root INTO DATA(lx_root).

    ENDTRY.

    mo_salv_model->display( ).

  ENDMETHOD.
  METHOD pbo_0400.

    DATA: lo_column TYPE REF TO cl_salv_column_table.

    IF mo_vin_container IS NOT INITIAL.
      mo_salv_vin->refresh( ).
      RETURN.
    ENDIF.

    mo_vin_container = NEW cl_gui_custom_container( container_name = 'VIN_LIST_CONTAINER' ).

    IF mo_vin_container IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = mo_vin_container
          IMPORTING
            r_salv_table   = mo_salv_vin
          CHANGING
            t_table        = mt_model
        ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    mo_salv_vin->get_functions( )->set_all( ).

    mo_salv_vin->get_selections( )->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).

    DATA(lo_columns) = mo_salv_vin->get_columns( ).
    lo_columns->set_optimize( ).

    TRY.
        lo_column ?= lo_columns->get_column( 'MARKET' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'PROMO' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'MATNR' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'VALUE' ).
        lo_column->set_currency_column( 'CURR' ).

      CATCH cx_root INTO DATA(lx_root).

    ENDTRY.

    mo_salv_vin->display( ).
  ENDMETHOD.
  METHOD salv_functions.

    DATA(lo_functions) = mo_salv_promo->get_functions( ).
    lo_functions->set_all( ).

    TRY.

        lo_functions->add_function(
          EXPORTING
            name     = 'NEW'                 " ALV Function
            icon     = CONV string( icon_create )
            text     = 'New'
            tooltip  = 'Create new promo'
            position = if_salv_c_function_position=>right_of_salv_functions " Positioning Function
        ).

        lo_functions->add_function(
          EXPORTING
            name     = 'COPY'                 " ALV Function
            icon     = CONV string( icon_copy_object )
            text     = 'Copy'
            tooltip  = 'Copy a promo'
            position = if_salv_c_function_position=>right_of_salv_functions " Positioning Function
        ).

        lo_functions->add_function(
          EXPORTING
            name     = 'VIEW'                 " ALV Function
            icon     = CONV string( icon_display )
            text     = 'Show ID'
            tooltip  = 'Show ID'
            position = if_salv_c_function_position=>right_of_salv_functions " Positioning Function
        ).

        lo_functions->add_function(
          EXPORTING
            name     = 'MODEL'                 " ALV Function
            icon     = CONV string( icon_display )
            text     = 'Model list'
            tooltip  = 'Model list'
            position = if_salv_c_function_position=>right_of_salv_functions " Positioning Function
        ).

        lo_functions->add_function(
          EXPORTING
            name     = 'VIN'                 " ALV Function
            icon     = CONV string( icon_display )
            text     = 'Vin list'
            tooltip  = 'Vin list'
            position = if_salv_c_function_position=>right_of_salv_functions " Positioning Function
        ).
      CATCH cx_salv_existing.   " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_wrong_call. " ALV: General Error Class (Checked During Syntax Check)
    ENDTRY.
  ENDMETHOD.
  METHOD salv_events.

    DATA(lo_events) = mo_salv_promo->get_event( ).

    SET HANDLER on_user_command FOR lo_events.
    SET HANDLER on_hotspot FOR lo_events.
  ENDMETHOD.
  METHOD salv_fieldcatalog.

    DATA: lo_column TYPE REF TO cl_salv_column_table.
    DATA(lo_columns) = mo_salv_promo->get_columns( ).

    lo_columns->set_optimize( ).

    TRY.
        lo_column ?= lo_columns->get_column( 'ICON' ).
        lo_column->set_icon( ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_long_text('View Promo').

        lo_column ?= lo_columns->get_column( 'VKORG' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'MARKET' ).
        lo_column->set_key( ).

        lo_column ?= lo_columns->get_column( 'PROMO' ).
        lo_column->set_key( ).
        lo_column->set_icon( ).                                   "modified
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ). "modified

        lo_column ?= lo_columns->get_column( 'SELL_TYPE_DESC' ).
        lo_column->set_short_text( TEXT-h01 ).
        lo_column->set_medium_text( TEXT-h01 ).
        lo_column->set_long_text( TEXT-h01 ).

      CATCH cx_root INTO DATA(lx_root).

    ENDTRY.
  ENDMETHOD.
  METHOD new_promo.

    DATA: ls_promo TYPE lcl_promo=>ty_promo,
          lv_saved TYPE abap_bool.

    IF go_promo IS NOT BOUND.
      go_promo = NEW lcl_promo( ).
    ENDIF.

    go_promo->execute( iv_state = gc_state-new ).

    CALL SCREEN 200.

    go_promo->get_result(
      IMPORTING
        es_promo = ls_promo
        ev_saved = lv_saved ).

    IF lv_saved <> abap_true.
      RETURN.
    ENDIF.

    append_master_data( ls_promo ).

  ENDMETHOD.
  METHOD copy_promo.

    DATA: ls_promo TYPE lcl_promo=>ty_promo,
          lv_saved TYPE abap_bool.

    READ TABLE it_selected_data ASSIGNING FIELD-SYMBOL(<ls_selected_data>) INDEX 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF go_promo IS NOT BOUND.
      go_promo = NEW lcl_promo( ).
    ENDIF.
    go_promo->execute( iv_state = gc_state-copy
                    is_promo = <ls_selected_data> ).

    CALL SCREEN 200.

    go_promo->get_result(
      IMPORTING
        es_promo = ls_promo
        ev_saved = lv_saved ).

    IF lv_saved <> abap_true.
      RETURN.
    ENDIF.

    append_master_data( ls_promo ).
  ENDMETHOD.
  METHOD view_promo.

    DATA: ls_promo TYPE lcl_promo=>ty_promo,
          lv_saved TYPE abap_bool.

    READ TABLE it_selected_rows ASSIGNING FIELD-SYMBOL(<lv_row>) INDEX 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE mt_promo ASSIGNING FIELD-SYMBOL(<ls_promo>) INDEX <lv_row>.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF go_promo IS NOT BOUND.
      go_promo = NEW lcl_promo( ).
    ENDIF.
    go_promo->execute( iv_state = gc_state-view
                    is_promo = <ls_promo> ).

    CALL SCREEN 200.

    go_promo->get_result(
      IMPORTING
        es_promo = ls_promo
        ev_saved = lv_saved ).

    IF lv_saved <> abap_true.
      RETURN.
    ENDIF.

    MODIFY mt_promo ASSIGNING <ls_promo> FROM ls_promo INDEX <lv_row>.

  ENDMETHOD.
  METHOD create_tabstrip_text_table.

    DATA: lv_empty TYPE char24,
          lv_full  TYPE char24.

    WRITE icon_wf_workitem_cancel AS ICON TO lv_empty.
    WRITE icon_wf_workitem_completed AS ICON TO lv_full.
    gt_tabstrip_header = VALUE #( ( name = 'TS_PROMO_TAB1' data_tab = 'MT_MODEL'   empty = |{ lv_empty } Model| full = |{ lv_full } Model| )
                                  ( name = 'TS_PROMO_TAB2' data_tab = 'MT_CHASSIS' empty = |{ lv_empty } Chassis| full = |{ lv_full } Chassis| )
                                  ( name = 'TS_PROMO_TAB3' data_tab = 'MT_FLEET'   empty = |{ lv_empty } Fleet codes| full = |{ lv_full } Fleet codes| )
                                  ( name = 'TS_PROMO_TAB4' data_tab = 'MT_VIN'     empty = |{ lv_empty } Vin exclusion| full = |{ lv_full } Vin exclusion| )
                                  ( name = 'TS_PROMO_TAB5' data_tab = 'MT_INCOMP'  empty = |{ lv_empty } Incompatibility| full = |{ lv_full } Incompatibility| ) ).
  ENDMETHOD.
  METHOD append_master_data.

    APPEND INITIAL LINE TO mt_promo ASSIGNING FIELD-SYMBOL(<ls_promo>).

    <ls_promo> = CORRESPONDING #( is_promo ).

    <ls_promo>-icon = icon_pdir_foreward_switch.

  ENDMETHOD.
  METHOD adjust_data.

    LOOP AT mt_promo ASSIGNING FIELD-SYMBOL(<ls_promo>).
      <ls_promo>-icon = icon_pdir_foreward_switch.
    ENDLOOP.
  ENDMETHOD.
  METHOD on_hotspot.

    READ TABLE mt_promo ASSIGNING FIELD-SYMBOL(<ls_promo>) INDEX row.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_selected_rows) = mo_salv_promo->get_selections( )->get_selected_rows( ). "modified
    APPEND row TO lt_selected_rows.                                                  "modified

    CASE column.
      WHEN 'ICON'.
        view_promo( lt_selected_rows ).                                              "modified
      WHEN 'PROMO'.                                                             "modified
        view_promo( lt_selected_rows ).                                              "modified
    ENDCASE.
    mo_salv_promo->refresh( ).
  ENDMETHOD.
  METHOD get_status_text.
    SELECT status,
           status_desc
      FROM zlpbns_pr_stat_t
      INTO CORRESPONDING FIELDS OF TABLE @gt_status_text
      WHERE spras = @sy-langu.
  ENDMETHOD.
ENDCLASS.

DATA: go_zlpbns_promo_md TYPE REF TO lcl_zlpbns_promo_md.

START-OF-SELECTION.

  go_zlpbns_promo_md = NEW lcl_zlpbns_promo_md( ).

  IF go_zlpbns_promo_md IS BOUND.
    go_zlpbns_promo_md->execute( ).
  ELSE.
    "Error handle failed object creation
  ENDIF.

  INCLUDE zlpbns_promo_md_0100m01.
