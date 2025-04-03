REPORT zrd_test_hier.


DATA: BEGIN OF gs_screen0100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen0100.
*----------------------------------------------------------------------*
*       CLASS lcl_double_hierarchy DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_double_hierarchy DEFINITION FINAL.
  PUBLIC SECTION.

    TYPES: BEGIN OF ty_structure,
             tech_value TYPE char100,
             end_value  TYPE char100,
           END OF ty_structure.

    DATA : mo_grid      TYPE REF TO cl_gui_alv_grid,
           mo_top       TYPE REF TO cl_dd_document,
           mo_split_row TYPE REF TO cl_gui_splitter_container.

    DATA: mo_left_tree   TYPE REF TO cl_salv_tree,
          mo_right_tree  TYPE REF TO cl_salv_tree,
          mo_left_nodes  TYPE REF TO cl_salv_nodes,
          mo_right_nodes TYPE REF TO cl_salv_nodes,
          mo_left_node   TYPE REF TO cl_salv_node,
          mo_right_node  TYPE REF TO cl_salv_node,
          mt_data        TYPE TABLE OF ty_structure,
          mv_image       TYPE salv_de_tree_image,
          mt_nodes       TYPE TABLE OF zrd_hierarchy.

    METHODS execute.

  PRIVATE SECTION.
    METHODS extract_data.
    METHODS start_container.
    METHODS generate_hierarchy.
    METHODS generate_nodes.
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
    extract_data( ).
    generate_hierarchy( ).
    CALL SCREEN 100.
  ENDMETHOD.                    "execute

  METHOD extract_data.

    SELECT *
      FROM zrd_hierarchy
      INTO CORRESPONDING FIELDS OF TABLE mt_nodes.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      <ls_nodes>-id = |{ <ls_nodes>-id ALPHA = IN }|.
    ENDLOOP.

    SORT mt_nodes BY charact id.

  ENDMETHOD.                    "extract_data



  METHOD generate_hierarchy.

    DATA: lo_column TYPE REF TO cl_salv_column_tree,
          lr_events TYPE REF TO cl_salv_events_tree.

    TRY.

        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = mo_split_row->get_container( row = 1 column = 1 )
          IMPORTING
            r_salv_tree = mo_left_tree
          CHANGING
            t_table     = mt_data.

        DATA(lo_functions) = mo_left_tree->get_functions( ).
        lo_functions->set_all( abap_true ).

        DATA(lo_columns) = mo_left_tree->get_columns( ).
        lo_column ?= lo_columns->get_column( 'TECH_VALUE' ).
        lo_column->set_short_text( 'Tech.Value' ).
        lo_column->set_output_length( 35 ).

        lo_column ?= lo_columns->get_column( 'END_VALUE' ).
        lo_column->set_short_text( 'End.Value' ).
        lo_column->set_output_length( 30 ).

        DATA(lo_settings) = mo_left_tree->get_tree_settings( ).
        lo_settings->set_header( 'Double Tree Report' ).
        lo_settings->set_hierarchy_header( 'Double Tree Report' ).
        lo_settings->set_hierarchy_size( 60 ).
        mo_left_nodes = mo_left_tree->get_nodes( ).


        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = mo_split_row->get_container( row = 1 column = 2 )
          IMPORTING
            r_salv_tree = mo_right_tree
          CHANGING
            t_table     = mt_data.

        lo_functions = mo_right_tree->get_functions( ).
        lo_functions->set_all( abap_true ).

        lo_columns = mo_right_tree->get_columns( ).
        lo_column ?= lo_columns->get_column( 'TECH_VALUE' ).
        lo_column->set_short_text( 'Tech.Value' ).
        lo_column->set_output_length( 35 ).

        lo_column ?= lo_columns->get_column( 'END_VALUE' ).
        lo_column->set_short_text( 'End.Value' ).
        lo_column->set_output_length( 30 ).

        lo_settings = mo_right_tree->get_tree_settings( ).
        lo_settings->set_header( 'Double Tree Report' ).
        lo_settings->set_hierarchy_header( 'Double Tree Report' ).
        lo_settings->set_hierarchy_size( 60 ).
        mo_right_nodes = mo_right_tree->get_nodes( ).

        generate_nodes( ).

        mo_left_tree->display( ).
        mo_right_tree->display( ).

      CATCH: cx_salv_not_found, cx_salv_error.
    ENDTRY.

  ENDMETHOD.                    "extract_data

  METHOD generate_nodes.

    TYPES: BEGIN OF ty_folders,
             node_key  TYPE salv_de_node_key,
             nodelevel TYPE i,
           END OF ty_folders.

    DATA : ls_data    TYPE ty_structure,
           lv_parent  TYPE salv_de_node_key,
           lt_folders TYPE STANDARD TABLE OF ty_folders.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>). "WHERE charact = 'ATWRT1'.
      DATA(lv_next_tabix) = sy-tabix + 1.

      ls_data-tech_value = <ls_nodes>-tech_value.
      ls_data-end_value  = <ls_nodes>-end_value.

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
          IF <ls_nodes>-charact = 'ATWRT1'.
            mo_left_node = mo_left_nodes->add_node(  related_node   = lv_parent
                                           text           = <ls_nodes>-tech_value
                                           data_row       = ls_data
                                           relationship   = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )  ).
            INSERT VALUE #( node_key = mo_left_node->get_key( ) nodelevel = <ls_nodes>-node_level ) INTO lt_folders INDEX 1.
          ELSE.
            mo_right_node = mo_right_nodes->add_node(  related_node   = lv_parent
                                           text           = <ls_nodes>-tech_value
                                           data_row       = ls_data
                                           relationship   = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )  ).
            INSERT VALUE #( node_key = mo_right_node->get_key( ) nodelevel = <ls_nodes>-node_level ) INTO lt_folders INDEX 1.
          ENDIF.
          CLEAR lv_parent_icon.
        CATCH cx_salv_msg.
      ENDTRY.

    ENDLOOP.

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

  ENDMETHOD.                    "start_container

ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_double_hierarchy.
  go_double_hierarchy->execute( ).
