*&---------------------------------------------------------------------*
*& Report  YMCMS_ARCH_MAN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ymcms_arch_man.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_archivio_mandati DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_structure,
             nomerep  TYPE sy-cprog,
             tcode    TYPE sy-tcode,
             descript TYPE settext,
           END OF ty_structure.

    TYPES: BEGIN OF ty_tabnodes,
             nodetext  TYPE lvc_value,
             nodelevel TYPE i,
             nomerep   TYPE sy-cprog,
             tcode     TYPE sy-tcode,
             descript  TYPE settext,
             type      TYPE char1,
           END OF ty_tabnodes,
           tt_tabnodes TYPE ty_tabnodes.

    METHODS execute.

  PRIVATE SECTION.

    DATA: mo_tree  TYPE REF TO cl_salv_tree,
          mo_nodes TYPE REF TO cl_salv_nodes,
          mo_node  TYPE REF TO cl_salv_node,
          mt_data  TYPE TABLE OF ty_structure,
          mv_image TYPE salv_de_tree_image,
          mt_nodes TYPE TABLE OF tt_tabnodes.

    METHODS generate_hierarchy.
    METHODS generate_nodes.
    METHODS fill_nodestable.
    METHODS call_object IMPORTING is_tabnodes TYPE ty_tabnodes.

    METHODS on_double_click FOR EVENT double_click OF cl_salv_events_tree
      IMPORTING node_key columnname.

ENDCLASS.                    "

DATA go_archivio_mandati TYPE REF TO lcl_archivio_mandati.

*----------------------------------------------------------------------*
*       CLASS lcl_class_name IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_archivio_mandati IMPLEMENTATION.

  METHOD execute.
    generate_hierarchy( ).

  ENDMETHOD.                    "execute

  METHOD on_double_click.
    DATA: nodes    TYPE REF TO cl_salv_nodes,
          node     TYPE REF TO cl_salv_node,
          ls_nodes TYPE ty_tabnodes.

    nodes = mo_tree->get_nodes( ).
    node = nodes->get_node( node_key ).
    ls_nodes-nodetext = node->get_text( ).
    READ TABLE mt_nodes INTO ls_nodes WITH KEY nodetext = ls_nodes-nodetext.
    CHECK sy-subrc = 0.
    call_object( is_tabnodes = ls_nodes ).

  ENDMETHOD.

  METHOD call_object.

    IF is_tabnodes-type = 'R'.
      CALL FUNCTION 'RS_RTR_SUBMIT'
        EXPORTING
          i_report                   = is_tabnodes-nomerep
        EXCEPTIONS
          report_cannot_be_generated = 1
          report_does_not_exist      = 2
          submit_auth_error          = 3
          variant_does_not_exist     = 4
          via_selscreen_only         = 5
          via_variant_only           = 6
          batch_submit_failed        = 7
          wrong_trdir_subc           = 8
          wrong_vari_env             = 9
          variant_obligatory         = 10
          no_batch_submit            = 11
          no_report                  = 12
          OTHERS                     = 13.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_message).
        MESSAGE lv_message TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    ELSEIF is_tabnodes-type = 'T'.

      SELECT SINGLE tcode FROM tstc
        WHERE tcode = @is_tabnodes-nomerep
        INTO @DATA(lv_exist).

      IF sy-subrc <> 0.
        MESSAGE |Transazione { is_tabnodes-nomerep } non esiste| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CALL TRANSACTION is_tabnodes-tcode.

    ENDIF.

  ENDMETHOD.


  METHOD generate_hierarchy.

    DATA: lo_column TYPE REF TO cl_salv_column_tree,
          lr_events TYPE REF TO cl_salv_events_tree.

    CALL METHOD cl_salv_tree=>factory
      IMPORTING
        r_salv_tree = mo_tree
      CHANGING
        t_table     = mt_data.

    DATA(lo_functions) = mo_tree->get_functions( ).
    lo_functions->set_all( abap_true ).

    DATA(lo_columns) = mo_tree->get_columns( ).
    lo_column ?= lo_columns->get_column( 'NOMEREP' ).
    lo_column->set_long_text( 'Nome Report' ).
    lo_column->set_medium_text( 'Nome Report' ).
    lo_column->set_short_text( 'Nome Rep.' ).
    lo_column->set_output_length( 40 ).

    lo_column ?= lo_columns->get_column( 'TCODE' ).
    lo_column->set_output_length( 40 ).

    lo_column ?= lo_columns->get_column( 'DESCRIPT' ).
    lo_column->set_long_text( 'Descrizione' ).
    lo_column->set_medium_text( 'Descrizione' ).
    lo_column->set_short_text( 'Descr.' ).
    lo_column->set_output_length( 110 ).

    DATA(lo_settings) = mo_tree->get_tree_settings( ).
    lo_settings->set_header( 'Archivio Mandati Passivi' ).
    lo_settings->set_hierarchy_header( 'Workflow Archivio Mandati Passivi' ).
    lo_settings->set_hierarchy_size( 80 ).
    mo_nodes = mo_tree->get_nodes( ).

    generate_nodes( ).
    lr_events = mo_tree->get_event( ).
    SET HANDLER on_double_click FOR lr_events.

    mo_tree->display( ).

  ENDMETHOD.                    "extract_data

  METHOD fill_nodestable.

    mt_nodes = VALUE #(
                      ( nodetext  = 'Parametrizzazioni'
                        nodelevel = 1 )

                      ( nodetext  = 'Parametrizzazioni società Prelios'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_SOC'
                        nomerep   = 'YMCMS_ARCH_SOC'
                        type      = 'T'
                        descript  = 'Parametrizzazione dati società Prelios' )

                      ( nodetext  = 'Parametrizzazioni menù campi'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_MENU'
                        nomerep   = 'YMCMS_ARCH_MENU'
                        type      = 'T'
                        descript  = 'Parametrizzazioni menù campi' )

                      ( nodetext  = 'Parametrizzazioni Capogruppo/Subfornitori'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_SUBFOR'
                        nomerep   = 'YMCMS_ARCH_SUBFOR'
                        type      = 'R'
                        descript  = 'Parametrizzazioni Capogruppo/Subfornitori' )

                      ( nodetext  = 'Parametrizzazioni ID Registri'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_ID_REG'
                        nomerep   = 'YMCMS_ARCH_ID_REG'
                        type      = 'T'
                        descript  = 'Parametrizzazioni ID Registri' )

                      ( nodetext  = 'Parametrizzazioni specifiche per il registro delle informazioni'
                        nodelevel = 2 )

                      ( nodetext  = 'Lista e gerarchia società Prelios nel perimetro'
                        nodelevel = 3
                        tcode     = 'YMCMS_ARCH_GERARC'
                        nomerep   = 'YMCMS_ARCH_GERARC'
                        type      = 'T'
                        descript  = 'Lista e gerarchia società Prelios nel perimetro' )

                      ( nodetext  = 'Lista ID Funzioni'
                        nodelevel = 3
                        tcode     = 'YMCMS_ARCH_ID_FUNZ'
                        nomerep   = 'YMCMS_ARCH_ID_FUNZ'
                        type      = 'T'
                        descript  = 'Lista ID Funzioni' )

                      ( nodetext  = 'Lista gruppi merci rilevanti'
                        nodelevel = 3
                        tcode     = 'YMCMS_ARCH_GRP_MER'
                        nomerep   = 'YMCMS_ARCH_GRP_MER'
                        type      = 'T'
                        descript  = 'Lista gruppi merci rilevanti' )

                        "==================================================================

                      ( nodetext  = 'Caricamento dati'
                        nodelevel = 1 )

                      ( nodetext  = 'Caricamento dati singolo contratto'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_MAND'
                        nomerep   = 'YMCMS_ARCH_MAND'
                        type      = 'T'
                        descript  = 'Gestione dati singolo mandato' )

                      ( nodetext  = 'Caricamento dati mandato da file Excel'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_MAN_EXCEL'
                        nomerep   = 'YMCMS_ARCH_MAN_EXCEL'
                        type      = 'T'
                        descript  = 'Caricamento dati mandato da file Excel' )

                      ( nodetext  = 'Gestione singolo codici LEI ed AS in anagrafica fornitore'
                        nodelevel = 2
                        tcode     = ' '
                        nomerep   = ' '
                        type      = 'T'
                        descript  = 'Gestione singolo codici LEI ed AS in anagrafica fornitore' )

                      ( nodetext  = 'Modifica transazione Creazione fornitori da file (YFCMS_CREAZ_FORN)'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_GERARC'
                        nomerep   = 'YMCMS_ARCH_GERARC'
                        type      = 'T'
                        descript  = 'Lista e gerarchia società Prelios nel perimetro' )

                      ( nodetext  = 'Modifica codici LEI ed AS in anagrafica fornitore da Excel'
                        nodelevel = 2
                        tcode     = 'YFCMS_FORN_COD_AS_LEI'
                        nomerep   = 'YFCMS_FORN_COD_AS_LEI'
                        type      = 'T'
                        descript  = 'Lista e gerarchia società Prelios nel perimetro' )

                        "==================================================================

                      ( nodetext  = 'Reportistica'
                        nodelevel = 1 )

                      ( nodetext  = 'Reportistica segnalazioni di vigilanza'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_REP_BOI'
                        nomerep   = 'YMCMS_ARCH_REP_BOI'
                        type      = 'T'
                        descript  = 'Reportistica segnalazioni di vigilanza per Banca D’Italia' )

                      ( nodetext  = 'Reportistica registro delle informazioni'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_REP_DORA'
                        nomerep   = 'YMCMS_ARCH_REP_DORA'
                        type      = 'T'
                        descript  = 'Reportistica registro delle informazioni DORA' )

                      ( nodetext  = 'Reportistica Codici AS e codici LEI'
                        nodelevel = 2
                        tcode     = 'YMCMS_ARCH_COD_FOR'
                        nomerep   = 'YMCMS_ARCH_COD_FOR'
                        type      = 'T'
                        descript  = 'Reportistica Codici AS e Codici LEI anagrafiche fornitori' )  ) .

  ENDMETHOD.

  METHOD generate_nodes.

    TYPES: BEGIN OF ty_folders,
             node_key  TYPE salv_de_node_key,
             nodelevel TYPE i,
           END OF ty_folders.

    DATA : ls_data    TYPE ty_structure,
           lv_parent  TYPE salv_de_node_key,
           lt_folders TYPE STANDARD TABLE OF ty_folders.

    fill_nodestable( ).

    mv_image = icon_execute_object.

    LOOP AT mt_nodes ASSIGNING FIELD-SYMBOL(<ls_nodes>).
      DATA(lv_next_tabix) = sy-tabix + 1.

      ls_data-descript = <ls_nodes>-descript.
      ls_data-tcode    = <ls_nodes>-tcode.
      ls_data-nomerep  = <ls_nodes>-nomerep.

      CLEAR lv_parent.

      DELETE lt_folders WHERE nodelevel >= <ls_nodes>-nodelevel.
      READ TABLE lt_folders ASSIGNING FIELD-SYMBOL(<ls_folder>) INDEX 1.
      IF sy-subrc = 0.
        lv_parent = <ls_folder>-node_key.
      ENDIF.

      READ TABLE mt_nodes ASSIGNING FIELD-SYMBOL(<ls_next_node>) INDEX lv_next_tabix.
      IF sy-subrc = 0 AND <ls_next_node>-nodelevel > <ls_nodes>-nodelevel.
        DATA(lv_parent_icon) = abap_true.
      ENDIF.

      mo_node = mo_nodes->add_node(  related_node   = lv_parent
                                     text           = <ls_nodes>-nodetext
                                     data_row       = ls_data
                                     expanded_icon  = COND #( WHEN lv_parent_icon IS INITIAL THEN mv_image )
                                     collapsed_icon = COND #( WHEN lv_parent_icon IS INITIAL THEN mv_image )
                                     relationship   = COND #( WHEN lv_parent IS NOT INITIAL THEN cl_gui_column_tree=>relat_last_child )  ).

      INSERT VALUE #( node_key = mo_node->get_key( ) nodelevel = <ls_nodes>-nodelevel ) INTO lt_folders INDEX 1.
      CLEAR lv_parent_icon.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.
  CREATE OBJECT go_archivio_mandati.
  go_archivio_mandati->execute( ).
