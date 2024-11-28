*&---------------------------------------------------------------------*
*& Report ZTR_R_GIIROF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztr_r_giirof.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_upl  RADIOBUTTON GROUP rb1 USER-COMMAND u01 DEFAULT 'X',
              p_dwl  RADIOBUTTON GROUP rb1,
              p_file TYPE string.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_prv RADIOBUTTON GROUP rb2  DEFAULT 'X' MODIF ID bl1,
              p_req RADIOBUTTON GROUP rb2 MODIF ID bl1,
              p_pay RADIOBUTTON GROUP rb2 MODIF ID bl1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_gruppo TYPE rpgroup MODIF ID bl2,
              p_bukrs  TYPE bukrs MODIF ID bl2,
              p_valut  TYPE valut MODIF ID bl2.
SELECTION-SCREEN END OF BLOCK b3.

CLASS lcl_girofondi DEFINITION.

  PUBLIC SECTION.

    METHODS: execute,
      get_filename.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_excel_data,
             bukrs     TYPE fibl_rpcode-bukrs,
             rpgroup   TYPE fibl_rpcode_grou-rpgroup,
             rpcode    TYPE frft_bank_rep-rpcode,
             valut     TYPE reguh-valut,
             rp_text   TYPE frft_bank_rep-rp_text,
             paymt_ref TYPE frft_bank_rep-paymt_ref,
             waers     TYPE frft_bank_rep-waers,
             rwbtr     TYPE string, "frft_bank_rep-rwbtr,
           END OF ty_excel_data,
           tt_excel_data TYPE STANDARD TABLE OF ty_excel_data WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_fibl_rpcode.
             INCLUDE TYPE fibl_rpcode_i.
    TYPES:   used TYPE abap_bool,
           END OF ty_fibl_rpcode,
           tt_fibl_rpcode TYPE STANDARD TABLE OF ty_fibl_rpcode WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_messages,
             icon    TYPE icon-id,
             message TYPE bapi_msg,
           END OF ty_messages.

    DATA: mt_excel_data TYPE tt_excel_data,
          mo_salv_msg   TYPE REF TO cl_salv_table,
          mt_messages   TYPE STANDARD TABLE OF ty_messages.

    METHODS: read_excel_data,
      download_excel,
      get_batch_input,
      get_fibl_rp_code IMPORTING iv_bukrs              TYPE bukrs
                                 iv_group              TYPE rpgroup
                       RETURNING VALUE(rt_fibl_rpcode) TYPE tt_fibl_rpcode,

      display_messages.
ENDCLASS.


CLASS lcl_girofondi IMPLEMENTATION.

  METHOD execute.

    IF p_file IS INITIAL .
      MESSAGE 'Please, enter a file name first!'(004) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CASE abap_true.
      WHEN p_upl.

        read_excel_data( ).
        get_batch_input( ).
        display_messages( ).

      WHEN p_dwl.

        IF p_gruppo IS INITIAL OR p_bukrs IS INITIAL.
          MESSAGE 'Please, fill obligatory fields!'(005) TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        download_excel( ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_filename.

    DATA: lt_filetable TYPE STANDARD TABLE OF file_table,
          lv_rc        TYPE i,
          lv_user      TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        file_filter             =  cl_gui_frontend_services=>filetype_excel
      CHANGING
        file_table              =  lt_filetable
        rc                      =  lv_rc
        user_action             =  lv_user
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF lt_filetable IS NOT INITIAL.
      p_file = lt_filetable[ 1 ]-filename.
    ENDIF.

  ENDMETHOD.

  METHOD read_excel_data.

    DATA: lt_data  TYPE solix_tab.

    FIELD-SYMBOLS: <lt_excel_upload> TYPE STANDARD TABLE.

    IF p_file IS INITIAL.
      MESSAGE 'Please, enter a file name first!'(004) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    cl_gui_frontend_services=>gui_upload(
          EXPORTING
            filename                = p_file
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_data ).

    DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = p_file
                                               xdocument     = lv_bin_data ).


    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet_names) ).

    IF lt_worksheet_names IS INITIAL.

      MESSAGE 'Problem with excel file'(006) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    DATA(lo_worksheet_itab) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_worksheet_names[ 1 ] ).
    ASSIGN lo_worksheet_itab->* TO <lt_excel_upload>.
    DELETE <lt_excel_upload> INDEX 1.

    LOOP AT <lt_excel_upload> ASSIGNING FIELD-SYMBOL(<ls_excel_upload>).

      APPEND INITIAL LINE TO mt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel_new>).
      DO.

        DATA(lv_index) =  sy-index.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_upload> TO FIELD-SYMBOL(<fs>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT lv_index OF STRUCTURE <ls_excel_new> TO FIELD-SYMBOL(<fs2>).

        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        CASE lv_index .

          WHEN 4 .

            SPLIT <fs> AT '.' INTO DATA(lv_day) DATA(lv_month) DATA(lv_year).

            <fs2> = lv_year && lv_month && lv_day.

            CONTINUE.

          WHEN OTHERS.

            <fs2> =  <fs> .
        ENDCASE.
      ENDDO.
    ENDLOOP.

    IF mt_excel_data IS INITIAL.
      MESSAGE 'The excel file does not have any data!'(007) TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDMETHOD.

  METHOD get_fibl_rp_code.

    DATA: lr_bukrs_sel   TYPE STANDARD TABLE OF fibl_rpcode_bukrs_sel,
          lr_group_sel   TYPE STANDARD TABLE OF fibl_rpcode_rpcode_sel,
          lr_ptype_sel   TYPE STANDARD TABLE OF fibl_rpcode_ptype_sel,
          lt_fibl_rpcode TYPE STANDARD TABLE OF fibl_rpcode_i.

    lr_bukrs_sel = VALUE #( (  sign = 'I' option = 'EQ' bukrs_low = iv_bukrs ) ).
    lr_group_sel = VALUE #( (  sign = 'I' option = 'EQ' rpcode_low = iv_group ) ).
    lr_ptype_sel = VALUE #( (  sign = 'I' option = 'EQ' ptype_low = '03' ) ).

    CALL FUNCTION 'FIBL_RPCODE_MULTI_READ'
      EXPORTING
        id_only_released = abap_true
      TABLES
        it_r_bukrs_sel   = lr_bukrs_sel
        it_r_group_sel   = lr_group_sel
        et_fibl_rpcode_i = lt_fibl_rpcode
        it_r_ptype_sel   = lr_ptype_sel
      EXCEPTIONS
        not_found        = 1
        no_released      = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    rt_fibl_rpcode = CORRESPONDING #( lt_fibl_rpcode ).
  ENDMETHOD.
  METHOD download_excel.
    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lt_fibl_rpcode_i) = get_fibl_rp_code(
                               EXPORTING
                                 iv_bukrs       = p_bukrs
                                 iv_group       = p_gruppo
                             ).

    LOOP AT lt_fibl_rpcode_i ASSIGNING FIELD-SYMBOL(<ls_fibl_rpcode_i>).
      APPEND INITIAL LINE TO mt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel>).
      <ls_excel> = VALUE #( bukrs = p_bukrs
                            rpgroup = p_gruppo
                            rpcode = <ls_fibl_rpcode_i>-rpcode
                            waers = <ls_fibl_rpcode_i>-waers
                            valut = p_valut ).
    ENDLOOP.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_salv)
          CHANGING
            t_table      = mt_excel_data ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    DATA(lo_cols) = lo_salv->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.

        DATA(lv_text) = CONV string( 'Company Code'(009) ).
        lo_column ?= lo_cols->get_column( 'BUKRS' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = CONV string( 'Amount'(008) ).
        lo_column ?= lo_cols->get_column( 'RWBTR' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
      CATCH cx_salv_not_found.
    ENDTRY.

    DATA(lv_xml) = lo_salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    DATA(lt_xstring) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_xml ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( lv_xml )
        filename                  = p_file
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_xstring                      " Transfer table
      EXCEPTIONS
        file_write_error          = 1                    " Cannot write to file
        no_batch                  = 2                    " Cannot execute front-end function in background
        gui_refuse_filetransfer   = 3                    " Incorrect Front End
        invalid_type              = 4                    " Invalid value for parameter FILETYPE
        no_authority              = 5                    " No Download Authorization
        unknown_error             = 6                    " Unknown error
        header_not_allowed        = 7                    " Invalid header
        separator_not_allowed     = 8                    " Invalid separator
        filesize_not_allowed      = 9                    " Invalid file size
        header_too_long           = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create           = 11                   " Cannot create DataProvider
        dp_error_send             = 12                   " Error Sending Data with DataProvider
        dp_error_write            = 13                   " Error Writing Data with DataProvider
        unknown_dp_error          = 14                   " Error when calling data provider
        access_denied             = 15                   " Access to file denied.
        dp_out_of_memory          = 16                   " Not enough memory in data provider
        disk_full                 = 17                   " Storage medium is full.
        dp_timeout                = 18                   " Data provider timeout
        file_not_found            = 19                   " Could not find file
        dataprovider_exception    = 20                   " General Exception Error in DataProvider
        control_flush_error       = 21                   " Error in Control Framework
        not_supported_by_gui      = 22                   " GUI does not support this
        error_no_gui              = 23                   " GUI not available
        OTHERS                    = 24
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD get_batch_input.

    DATA : lt_bdcdata  TYPE STANDARD TABLE OF bdcdata,
           lt_messages TYPE STANDARD TABLE OF bdcmsgcoll.

*   Setting options for CALL TRANSACTION
    DATA(ls_options) = VALUE ctu_params(
                          dismode = COND #( WHEN p_prv IS NOT INITIAL THEN 'E' ELSE 'N' )
                          updmode = 'S'
                          cattmode = 'N'
                          defsize = 'X'
                          racommit = COND #( WHEN p_prv IS NOT INITIAL THEN ' ' ELSE 'X')
*                              NOBINPT = 'X'
*                              NOBIEND
                          ).

    DATA(ls_header) = mt_excel_data[ 1 ].

*   Retrieving FIBL RP code based on company code and group from Excel header
    DATA(lt_fibl_rpcode_i) = get_fibl_rp_code(
                                               EXPORTING
                                                 iv_bukrs = ls_header-bukrs
                                                 iv_group = ls_header-rpgroup
                                             ).

    DATA(lv_valut) = |{ ls_header-valut+6(2) }{ ls_header-valut+4(2) }{ ls_header-valut(4) }|.

*   Initial BDC data setup
    lt_bdcdata = VALUE #( ( dynbegin = 'T'            fnam   = 'FRFT_B' )
                          ( program  = 'FIBL_FRFT'    dynpro = '0100' dynbegin = 'X' )
                          ( fnam     = 'BDC_OKCODE'   fval   = '=ENTER' )
                          ( fnam     = 'REGUH-VALUT'  fval   = lv_valut )
                          ( fnam     = 'RNG_GRP-LOW'  fval   = ls_header-rpgroup )
                          ( fnam     = 'RNG_BUK-LOW'  fval   = ls_header-bukrs )
                              ).
*   Loop through Excel data to fill BDC data
    LOOP AT mt_excel_data ASSIGNING FIELD-SYMBOL(<ls_excel_data>) WHERE rwbtr <> 0.

      READ TABLE lt_fibl_rpcode_i ASSIGNING FIELD-SYMBOL(<ls_fibl_rpcode_i>)
        WITH KEY rpcode = to_upper( <ls_excel_data>-rpcode ).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

*      Mark FIBL RP code as used
      <ls_fibl_rpcode_i>-used = abap_true.

*      Adding additional BDC steps based on Excel data
      lt_bdcdata = VALUE #( BASE lt_bdcdata
                                ( program = 'FIBL_FRFT'                   dynpro = '0100'     dynbegin = 'X' )
                                ( fnam    = 'BDC_OKCODE'                  fval   = '=POSI_RP_B'              )
                                ( program = 'FIBL_FRFT'                   dynpro = '0300'     dynbegin = 'X' )
                                ( fnam    = 'BDC_OKCODE'                  fval   = '=ENTER'                  )
                                ( fnam    = 'SEARCH_VALUE'                fval   = to_upper( <ls_excel_data>-rpcode ) )
                                ( program = 'FIBL_FRFT'                   dynpro = '0100'     dynbegin = 'X' )
                                ( fnam    = 'BDC_OKCODE'                  fval   = '=ENTER'                  )
                                ( fnam    = 'FRFT_BANK_REP-RWBTR(01)'     fval   = <ls_excel_data>-rwbtr     )
                                ( fnam    = 'FRFT_BANK_REP-PAYMT_REF(01)' fval   = <ls_excel_data>-paymt_ref )
                                ( fnam    = 'FRFT_BANK_REP-XPORE(01)'     fval   = 'X'                       )
                                ).

    ENDLOOP.

*   Additional BDC steps if p_prv is not set
    IF p_prv IS INITIAL.

      lt_bdcdata = VALUE #( BASE lt_bdcdata
                          ( program = 'FIBL_FRFT'           dynpro = '0100'     dynbegin = 'X' )
                          ( fnam    = 'BDC_OKCODE'          fval   = '=POST_PRQ'               )
                          ( program = 'SAPLSBAL_DISPLAY'    dynpro = '0200'     dynbegin = 'X' )
                          ( fnam    = 'BDC_OKCODE'          fval   = '=&ONT'                   )
                          ).

    ENDIF.

*   Execute transaction 'FRFT_B'
    CALL TRANSACTION 'FRFT_B' WITH AUTHORITY-CHECK USING lt_bdcdata
                              OPTIONS FROM ls_options.

*   Return if p_prv is set
    IF p_prv IS NOT INITIAL.
      RETURN.
    ENDIF.

*   Commit work
    COMMIT WORK AND WAIT.

*   Remove unused FIBL RP codes
    DELETE lt_fibl_rpcode_i WHERE used IS INITIAL.

*   Get pay request data
    SELECT rpc~rpcode, payrq~rfttrn, MAX( payrq~keyno ) AS keyno
      FROM @lt_fibl_rpcode_i AS rpc
     LEFT JOIN payrq
         ON payrq~rfttrn = rpc~rpcode
        AND payrq~valut  = @ls_header-valut
        AND payrq~zbukr  = @ls_header-bukrs
        AND payrq~usnam  = @sy-uname
        AND payrq~cpudt  = @sy-datum

      GROUP BY rpc~rpcode, payrq~rfttrn
      INTO TABLE @DATA(lt_payrq).

*   Additional BDC steps if p_pay is not set
    IF p_pay IS NOT INITIAL.
      lt_bdcdata = VALUE #(
          ( dynbegin = 'T'      fnam   = 'FRFT_B' )
          ( program  = 'FIBL_FRFT'    dynpro = '0100'   dynbegin = 'X' )
          ( fnam     = 'BDC_OKCODE'   fval   = '=ENTER' )
          ( fnam     = 'REGUH-VALUT'  fval   = lv_valut )
          ( fnam     = 'RNG_GRP-LOW'  fval   = ls_header-rpgroup )
          ( fnam     = 'RNG_BUK-LOW'  fval   = ls_header-bukrs )
              ).
    ENDIF.

*   Loop through pay request data to fill messages
    LOOP AT lt_payrq ASSIGNING FIELD-SYMBOL(<ls_payrq>).
      APPEND INITIAL LINE TO mt_messages ASSIGNING FIELD-SYMBOL(<ls_msg>).

      DATA(lv_key) = |{ <ls_payrq>-keyno ALPHA = OUT }|.

      IF <ls_payrq>-rfttrn IS NOT INITIAL.
        <ls_msg>-icon = icon_green_light.
        MESSAGE s068(fibl_rpcode) WITH lv_key <ls_payrq>-rpcode INTO <ls_msg>-message.
      ELSE.
        <ls_msg>-icon = icon_red_light.
        MESSAGE e026(fibl_rpcode) WITH <ls_payrq>-rpcode '' ls_header-bukrs INTO <ls_msg>-message.
        DATA(lv_error) = abap_true.
      ENDIF.

*     Additional BDC steps if p_pay is not set
      IF p_pay IS NOT INITIAL.
        lt_bdcdata = VALUE #( BASE lt_bdcdata
                      ( program = 'FIBL_FRFT'                   dynpro = '0100'     dynbegin = 'X' )
                      ( fnam    = 'BDC_OKCODE'                  fval   = '=POSI_PQ' )
                      ( program = 'FIBL_FRFT'                   dynpro = '0300'     dynbegin = 'X' )
                      ( fnam    = 'BDC_OKCODE'                  fval   = '=ENTER' )
                      ( fnam    = 'SEARCH_VALUE'                fval   = lv_key )
                      ( program = 'FIBL_FRFT'                   dynpro = '0100'     dynbegin = 'X' )
                      ( fnam    = 'FRFT_PAYRQ-MARK_ROW(01)'     fval   = 'X' )
                        ).
      ENDIF.

    ENDLOOP.

*   Return if any errors occurred or p_req is not set
    IF p_req IS NOT INITIAL OR lv_error IS NOT INITIAL.
      RETURN.
    ENDIF.

*   Proceed with pay request
    lt_bdcdata = VALUE #( BASE lt_bdcdata
                            ( program = 'FIBL_FRFT'             dynpro = '0100'     dynbegin = 'X' )
                            ( fnam    = 'BDC_OKCODE'            fval   = '=PAY_PAYRQ' )
                            ).

*   Execute transaction 'FRFT_B'
    CALL TRANSACTION 'FRFT_B' WITH AUTHORITY-CHECK USING lt_bdcdata
                              OPTIONS FROM ls_options.

*   Commit work
    COMMIT WORK AND WAIT.

*   Get pay data
    SELECT prq~keyno, payrq~xrelp
      FROM @lt_payrq AS prq
      JOIN payrq
         ON payrq~keyno = prq~keyno
      INTO TABLE @DATA(lt_payrq_rel).

*   Loop through pay data to fill messages
    LOOP AT lt_payrq_rel ASSIGNING FIELD-SYMBOL(<ls_payrq_rel>).
      APPEND INITIAL LINE TO mt_messages ASSIGNING <ls_msg>.

      DATA(lv_key_rel) = |{ <ls_payrq_rel>-keyno ALPHA = OUT }|.

      IF <ls_payrq_rel>-xrelp IS NOT INITIAL.
        <ls_msg>-icon = icon_green_light.
        MESSAGE s071(fibl_rpcode) WITH lv_key_rel INTO <ls_msg>-message.

      ELSE.
        <ls_msg>-icon = icon_red_light.
        <ls_msg>-message = |{ TEXT-010 } { lv_key_rel } { TEXT-011 }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD display_messages.

    DATA: lo_column TYPE REF TO cl_salv_column_table.

    IF p_prv IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_salv_msg
          CHANGING
            t_table      = mt_messages ).
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    mo_salv_msg->get_functions( )->set_all( 'X' ).


    DATA(lo_cols) = mo_salv_msg->get_columns( ).
    lo_cols->set_optimize(  ).

    TRY.
        DATA(lv_text) = CONV scrtext_s( 'Status'(012) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    mo_salv_msg->display( ).

  ENDMETHOD.

ENDCLASS.

DATA : go_girofondi TYPE REF TO lcl_girofondi.

INITIALIZATION.
  CREATE OBJECT go_girofondi.

AT SELECTION-SCREEN OUTPUT. " Handle fields show/hide

  LOOP AT SCREEN.

    CASE abap_true.
      WHEN p_upl.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 1.
          WHEN 'BL2'.
            screen-active = 0.
        ENDCASE.
      WHEN p_dwl.
        CASE screen-group1.
          WHEN 'BL1'.
            screen-active = 0.
          WHEN 'BL2'.
            screen-active = 1.
        ENDCASE.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file. " excel upload
  go_girofondi->get_filename( ).

START-OF-SELECTION.

  go_girofondi->execute( ).
