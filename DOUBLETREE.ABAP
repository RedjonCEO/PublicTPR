REPORT zrd_test_hier.

DATA: BEGIN OF gs_screen0100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0100.

DATA gv_changed TYPE char1.

DATA: BEGIN OF gs_screen0101,
        ok_code      TYPE sy-ucomm,
        rb_child     TYPE char1,
        rb_sibling   TYPE char1,
        inp_end_val  TYPE lvc_value,
        inp_tech_val TYPE lvc_value,
      END OF gs_screen0101.
*----------------------------------------------------------------------*
*       CLASS lcl_double_hierarchy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_double_hierarchy DEFINITION FINAL.
  PUBLIC SECTION.


    TYPES ty_hierarchy TYPE zrd_hierarchy.

    DATA: BEGIN OF ty_hier.
            INCLUDE TYPE ty_hierarchy.
    DATA: delete_ind TYPE char1.
    DATA END OF ty_hier.

    TYPES: ty_hier_data LIKE ty_hier,
           tt_nodes     TYPE TABLE OF ty_hier_data.

    TYPES: BEGIN OF ty_structure,
             tech_value TYPE char30,
             end_value  TYPE char30,
           END OF ty_structure.

    TYPES: BEGIN OF ty_folders,
             node_key  TYPE lvc_nkey,
             nodelevel TYPE i,
           END OF ty_folders.

    DATA : mo_split_row   TYPE REF TO cl_gui_splitter_container,
           mo_left_tree   TYPE REF TO cl_gui_alv_tree,
           mo_right_tree  TYPE REF TO cl_gui_alv_tree,
           mt_data        TYPE TABLE OF ty_structure,
           mt_nodes       TYPE tt_nodes,
           mt_left_nodes  TYPE TABLE OF zrd_hierarchy,
           mt_right_nodes TYPE TABLE OF zrd_hierarchy.

    METHODS execute.
    METHODS save_data.

  PRIVATE SECTION.
    METHODS extract_data.
    METHODS reset_id.
    METHODS start_container.
    METHODS generate_nodes.
    METHODS display_tree.
    METHODS add_menu_function CHANGING  ct_menu  TYPE REF TO cl_ctmenu.
    METHODS menu_fcode        IMPORTING iv_fcode   TYPE ui_func
                                        iv_nodekey TYPE lvc_nkey
                                        iv_sender  TYPE REF TO cl_gui_alv_tree.

    METHODS:handle_node_cm_req
                  FOR EVENT node_context_menu_request OF cl_gui_alv_tree
      IMPORTING node_key menu.

    METHODS:handle_node_cm_sel
                  FOR EVENT node_context_menu_selected OF cl_gui_alv_tree
      IMPORTING node_key fcode sender.

    METHODS:handle_item_cm_req
                  FOR EVENT item_context_menu_request OF cl_gui_alv_tree
      IMPORTING node_key menu.

    METHODS:handle_item_cm_sel
                  FOR EVENT item_context_menu_selected OF cl_gui_alv_tree
      IMPORTING node_key fcode sender.

ENDCLASS.                    "

DATA go_double_hierarchy TYPE REF TO lcl_double_hierarchy.
INCLUDE zrd_test_hier_pbo.
INCLUDE zrd_test_hier_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_double_hierarchy IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_double_hierarchy IMPLEMENTATION.

  METHOD execute.
    start_container( ).
    display_tree( ).
    CALL SCREEN 100.
  ENDMETHOD.                    "execute

  METHOD  menu_fcode.

    DATA: lv_answer   TYPE char1,
          lv_node_key TYPE lvc_nkey.
    DATA(lv_id) = |{ CONV lvc_nkey( iv_nodekey ) ALPHA = IN } |.

    CASE iv_sender.
      WHEN mo_left_tree.
        DATA(lv_char) = 'ATWRT1'.
      WHEN mo_right_tree.
        lv_char = 'ATWRT2'.
    ENDCASE.

    READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_node>) WITH KEY id = lv_id charact = lv_char.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_row_index) = sy-tabix.

    CASE iv_fcode.
      WHEN 'INSERT'.
        CALL SCREEN 101 STARTING AT 10 10.
        IF gs_screen0101-ok_code = 'FC_EXIT'.
          RETURN.
        ENDIF.
        gv_changed = abap_true.
        DATA(ls_new_row) = VALUE zrd_hierarchy( tech_value = gs_screen0101-inp_tech_val end_value = gs_screen0101-inp_end_val ).

        CASE abap_true.
          WHEN gs_screen0101-rb_child.
            DATA(lv_parent) = iv_nodekey.
            DATA(lv_level)  = <ls_node>-node_level + 1.
          WHEN gs_screen0101-rb_sibling.
            iv_sender->get_parent(
            EXPORTING
              i_node_key        =   iv_nodekey
            IMPORTING
              e_parent_node_key =    lv_parent ).
            lv_level = <ls_node>-node_level.
        ENDCASE.

        DATA selected_nodes TYPE lvc_t_nkey.
        DATA lv_lastnode    TYPE lvc_nkey.

        iv_sender->get_subtree(
          EXPORTING
            i_node_key         =    iv_nodekey " Parent Node Key
          IMPORTING
            et_subtree_nodes   =     selected_nodes
          EXCEPTIONS
            node_key_not_found = 1
            OTHERS             = 2 ).

        LOOP AT selected_nodes ASSIGNING FIELD-SYMBOL(<ls_selnodes>).
          lv_lastnode =  |{ <ls_selnodes> ALPHA = IN }|.
        ENDLOOP.

        iv_sender->add_node(
          EXPORTING
            i_relat_node_key     =     lv_parent
            is_outtab_line       =     ls_new_row
            i_relationship       =     COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
            i_node_text          =     gs_screen0101-inp_tech_val
          IMPORTING
            e_new_node_key       =     lv_node_key
          EXCEPTIONS
            relat_node_not_found = 1
            node_not_found       = 2
            OTHERS               = 3 ).

        ls_new_row-id         = |{ lv_node_key ALPHA = IN }|.
        ls_new_row-charact    = lv_char.
        ls_new_row-node_level = lv_level.

        READ TABLE mt_nodes TRANSPORTING NO FIELDS WITH KEY id = lv_lastnode charact = lv_char.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        lv_row_index = sy-tabix.

        INSERT ls_new_row INTO mt_nodes INDEX lv_row_index + 1.

      WHEN 'MODIFY'.

        DATA(lt_values) = VALUE ty_sval( ( tabname = 'ZRD_HIERARCHY' fieldname = 'END_VALUE' value = <ls_node>-end_value fieldtext = 'Set new end value' ) ).

        CALL FUNCTION 'POPUP_GET_VALUES'
          EXPORTING
            popup_title     = 'Insert new end value'
          IMPORTING
            returncode      = lv_answer
          TABLES
            fields          = lt_values
          EXCEPTIONS
            error_in_fields = 1
            OTHERS          = 2.

        IF lv_answer = 'A'.
          RETURN.
        ENDIF.

        <ls_node>-end_value = lt_values[ 1 ]-value.

        iv_sender->change_node(
          EXPORTING
            i_node_key     =    iv_nodekey
            i_outtab_line  =    <ls_node>
          EXCEPTIONS
            node_not_found = 1
            OTHERS         = 2 ).

        gv_changed = abap_true.
      WHEN 'DELETE'.

        DATA deleted_tab TYPE lvc_t_nkey.

        iv_sender->get_subtree(
          EXPORTING
            i_node_key         =    iv_nodekey " Parent Node Key
          IMPORTING
            et_subtree_nodes   =     deleted_tab
          EXCEPTIONS
            node_key_not_found = 1
            OTHERS             = 2 ).

        iv_sender->delete_subtree(
          EXPORTING
            i_node_key                =  iv_nodekey
            i_update_parents_expander = 'X'
            i_update_parents_folder   = 'X'
          EXCEPTIONS
            node_key_not_in_model     = 1
            OTHERS                    = 2  ).

        LOOP AT deleted_tab ASSIGNING FIELD-SYMBOL(<ls_deleted>).
          DELETE mt_nodes WHERE id =  |{ <ls_deleted> ALPHA = IN }| AND charact = lv_char.
        ENDLOOP.
        gv_changed = abap_true.
    ENDCASE.

    mo_left_tree->frontend_update( ).
    mo_right_tree->frontend_update( ).

  ENDMETHOD.

  METHOD  save_data.

    IF mt_nodes IS NOT INITIAL.
      reset_id( ).
    ENDIF.

    DELETE FROM zrd_hierarchy.
    INSERT zrd_hierarchy FROM TABLE mt_nodes.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE 'Error while saving data' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    MESSAGE 'Data save successfully' TYPE 'S'.
    gv_changed = abap_false.

  ENDMETHOD.

  METHOD extract_data.

    SELECT *
      FROM zrd_hierarchy
      INTO CORRESPONDING FIELDS OF TABLE mt_nodes.
    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      <ls_nodes>-id = |{ <ls_nodes>-id ALPHA = IN }|.
    ENDLOOP.
    SORT mt_nodes BY charact id.

  ENDMETHOD.                    "extract_data

  METHOD  reset_id.

    DATA: lv_old_char TYPE char30.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      IF lv_old_char <> <ls_nodes>-charact.
        DATA(lv_new_id) = 0.
      ENDIF.
      <ls_nodes>-id = lv_new_id + 1.
      lv_new_id     = lv_new_id + 1.
      lv_old_char   = <ls_nodes>-charact.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_nodes.

    DATA : lv_parent   TYPE lvc_nkey,
           lt_folders  TYPE STANDARD TABLE OF ty_folders,
           lv_node_key TYPE lvc_nkey.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      DATA(lv_next_tabix) = sy-tabix + 1.
      DATA(ls_data) = VALUE ty_structure( tech_value = <ls_nodes>-tech_value end_value  = <ls_nodes>-end_value ).
      CLEAR lv_parent.

      DELETE lt_folders WHERE nodelevel >= <ls_nodes>-node_level.
      READ TABLE lt_folders ASSIGNING FIELD-SYMBOL(<ls_folder>) INDEX 1.
      IF sy-subrc = 0.
        lv_parent = <ls_folder>-node_key.
      ENDIF.

      READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_next_node>) INDEX lv_next_tabix.
      IF sy-subrc = 0 AND <ls_next_node>-node_level > <ls_nodes>-node_level.
        DATA(lv_parent_icon) = abap_true.
      ENDIF.

      TRY.
          CASE <ls_nodes>-charact.
            WHEN 'ATWRT1'.
              mo_left_tree->add_node(
                EXPORTING
                  i_relat_node_key     =     lv_parent
                  is_outtab_line       =     <ls_nodes>
                  i_relationship       =     COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
                  i_node_text          =     <ls_nodes>-tech_value
                IMPORTING
                  e_new_node_key       =     lv_node_key
                EXCEPTIONS
                  relat_node_not_found = 1
                  node_not_found       = 2
                  OTHERS               = 3 ).
            WHEN 'ATWRT2'.
              mo_right_tree->add_node(
                EXPORTING
                  i_relat_node_key     =     lv_parent
                  is_outtab_line       =     <ls_nodes>
                  i_relationship       =     COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )
                  i_node_text          =     <ls_nodes>-tech_value
                IMPORTING
                  e_new_node_key       =     lv_node_key
                EXCEPTIONS
                  relat_node_not_found = 1
                  node_not_found       = 2
                  OTHERS               = 3 ).
          ENDCASE.

          INSERT VALUE #( node_key = lv_node_key nodelevel = <ls_nodes>-node_level ) INTO lt_folders INDEX 1.
          CLEAR lv_parent_icon.
        CATCH cx_salv_msg.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD  display_tree.

    DATA lt_event TYPE cntl_simple_events.
    DATA(ls_header) = VALUE treev_hhdr( heading   = 'Description' tooltip   = 'Description' width     = 40 width_pix = '' ).
    DATA(lt_fcat) = VALUE lvc_t_fcat( ( fieldname = 'TECH_VALUE'  scrtext_s = 'Tech.Value'  outputlen = 45 )
                                      ( fieldname = 'END_VALUE'   scrtext_s = 'End.Value'   outputlen = 45 ) ).
    extract_data( ).

    CALL METHOD mo_left_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = ls_header
      CHANGING
        it_outtab           = mt_left_nodes
        it_fieldcatalog     = lt_fcat.

    CALL METHOD mo_right_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = ls_header
      CHANGING
        it_outtab           = mt_right_nodes
        it_fieldcatalog     = lt_fcat.

    lt_event = VALUE #( ( eventid = cl_gui_column_tree=>eventid_expand_no_children    )
                        ( eventid = cl_gui_column_tree=>eventid_item_context_menu_req )
                        ( eventid = cl_gui_column_tree=>eventid_node_context_menu_req ) ).

    CALL METHOD mo_left_tree->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4.

    SET HANDLER handle_node_cm_req
                handle_node_cm_sel
                handle_item_cm_req
                handle_item_cm_sel FOR mo_left_tree.

    CALL METHOD mo_right_tree->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4.

    SET HANDLER handle_node_cm_req
                handle_node_cm_sel
                handle_item_cm_req
                handle_item_cm_sel FOR mo_right_tree.

    generate_nodes( ).
    mo_left_tree->frontend_update( ).
    mo_right_tree->frontend_update( ).

  ENDMETHOD.

  METHOD start_container.

    DATA: lo_cont TYPE REF TO cl_gui_custom_container.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONT100'
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

    CREATE OBJECT mo_split_row
      EXPORTING
        parent            = lo_cont
        rows              = 1
        columns           = 2
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    mo_split_row->set_column_width(
      EXPORTING
        id                =   1
        width             =   50
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).

    mo_split_row->set_row_sash( id    = 1
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = cl_gui_splitter_container=>true ).

    CREATE OBJECT mo_left_tree
      EXPORTING
        parent                      = mo_split_row->get_container( row = 1 column = 1 )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_toolbar                  = ' '
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.

    CREATE OBJECT mo_right_tree
      EXPORTING
        parent                      = mo_split_row->get_container( row = 1 column = 2 )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_toolbar                  = ' '
        no_html_header              = 'X'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7
        OTHERS                      = 8.
  ENDMETHOD.                    "start_container

  METHOD  handle_node_cm_sel.
    menu_fcode( EXPORTING iv_fcode = fcode iv_nodekey = node_key iv_sender = sender ).
  ENDMETHOD.

  METHOD  handle_node_cm_req.
    add_menu_function( CHANGING ct_menu = menu ).
  ENDMETHOD.

  METHOD  handle_item_cm_req.
    add_menu_function( CHANGING ct_menu = menu ).
  ENDMETHOD.

  METHOD  handle_item_cm_sel.
    menu_fcode( EXPORTING iv_fcode = fcode iv_nodekey = node_key iv_sender = sender ).
  ENDMETHOD.

  METHOD  add_menu_function.
    ct_menu->add_function( fcode = 'INSERT' text  = 'Insert').
    ct_menu->add_function( fcode = 'MODIFY' text  = 'Modify').
    ct_menu->add_function( fcode = 'DELETE' text  = 'Delete').
    ct_menu->add_separator( ).
    ct_menu->add_function( fcode = 'EXIT' text  = 'Exit').
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  CREATE OBJECT go_double_hierarchy.
  go_double_hierarchy->execute( ).
