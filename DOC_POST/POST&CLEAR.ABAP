*&---------------------------------------------------------------------*
*& Report  ZCMS_INCASSI_AMPRE_LEASECO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zcms_incassi_ampre_leaseco.
TABLES: bkpf, bseg, yfcms_amex_buk.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

INCLUDE zcms_incassi_ampre_leaseco_pai.
INCLUDE zcms_incassi_ampre_leaseco_pbo.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
TYPES:      t_cdnuc  TYPE char2.
DATA:       d_cdnuc  TYPE t_cdnuc.
PARAMETERS: p_reginc AS CHECKBOX DEFAULT abap_true,
            p_testb1 AS CHECKBOX DEFAULT abap_true,
            p_file   TYPE string.
SELECT-OPTIONS: s_nrcont FOR yfcms_amex_buk-control_account.
PARAMETERS: p_blart1 TYPE bkpf-blart DEFAULT 'PD',
            p_budat1 TYPE bkpf-budat DEFAULT sy-datum,
            p_hkont  TYPE bseg-hkont DEFAULT 'C210010200'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS: p_prgprt AS CHECKBOX DEFAULT abap_true,
            p_testb2 AS CHECKBOX DEFAULT abap_true.
SELECT-OPTIONS: s_bukrs FOR bkpf-bukrs,
                s_kunnr FOR bseg-kunnr.
PARAMETERS: p_blart2 TYPE bkpf-blart DEFAULT 'AB',
            p_budat2 TYPE bkpf-budat.

SELECTION-SCREEN END OF BLOCK b2.



*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_incassi_ampre_leaseco DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS value_request.
    METHODS execute.
    METHODS display_msg.
    METHODS adjust_cols_msg CHANGING co_salv_msg TYPE REF TO cl_salv_table.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_incasso,
             data_incasso TYPE sy-datum,
             imp_alloc    TYPE bapidoccur,
             altkn        TYPE altkn,
             cdnuc        TYPE yfcms_amex_ca,
             xblnr        TYPE xblnr,
             budat        TYPE budat,
             kunnr        TYPE kunnr,
             bukrs        TYPE bukrs,
             belnr        TYPE belnr_d,
             gjahr        TYPE gjahr,
             waers        TYPE waers,
             blocked      TYPE char1,
           END OF ty_incasso,
           tt_incasso TYPE STANDARD TABLE OF ty_incasso.

    TYPES: BEGIN OF ty_messages,
             buzei   TYPE buzei,
             bukrs   TYPE bukrs,
             kunnr   TYPE kunnr,
             xblnr   TYPE xblnr,
             bldat   TYPE bldat,
             budat   TYPE budat,
             icon    TYPE icon-id,
             belnr   TYPE belnr_d,
             message TYPE bapi_msg,
           END OF ty_messages.

    TYPES: BEGIN OF ty_excel,
             a TYPE string,
             b TYPE string,
             c TYPE string,
             d TYPE string,
             e TYPE string,
             f TYPE string,
             g TYPE string,
             h TYPE string,
             i TYPE string,
             j TYPE string,
             k TYPE string,
             l TYPE string,
             m TYPE string,
             n TYPE string,
             o TYPE string,
             p TYPE string,
             q TYPE string,
             r TYPE string,
             s TYPE string,
             t TYPE string,
             u TYPE string,
             v TYPE string,
           END OF ty_excel,
           tt_excel TYPE STANDARD TABLE OF ty_excel.

    DATA: mt_incasso_msg  TYPE STANDARD TABLE OF ty_messages,
          mt_pareggio_msg TYPE STANDARD TABLE OF ty_messages,
          mt_incasso      TYPE tt_incasso,
          mt_pareggio     TYPE tt_incasso,
          mo_incasso_msg  TYPE REF TO cl_salv_table,
          mo_pareggio_msg TYPE REF TO cl_salv_table,
          mt_excel        TYPE tt_excel,
          mt_bsid         TYPE STANDARD TABLE OF bsid,
          mo_split_row    TYPE REF TO cl_gui_splitter_container,
          mo_dock_msg     TYPE REF TO cl_gui_docking_container.

    METHODS data_checks.
    METHODS start_container.
    METHODS registrazione_incassi.
    METHODS pareggio_partite.
    METHODS excel_to_itab.
    METHODS prepare_bapi_data .
    METHODS fill_clear_table  IMPORTING is_bsid    TYPE bsid
                              CHANGING  ct_ftclear TYPE epic_t_ebr_ftclear.
    METHODS fill_post_table   IMPORTING is_bsid   TYPE bsid
                              CHANGING  ct_ftpost TYPE fagl_t_ftpost.

    METHODS call_bapi IMPORTING is_bapiache09 TYPE bapiache09
                                it_bapiacgl09 TYPE bapiacgl09_tab
                                it_bapiaccr09 TYPE bapiaccr09_tab
                                it_bapiacar09 TYPE bapiacar09_tab
                                iv_buzei      TYPE buzei OPTIONAL
                                iv_bukrs      TYPE bukrs
                                iv_xblnr      TYPE xblnr
                                iv_kunnr      TYPE kunnr
                                iv_budat      TYPE budat
                                iv_bldat      TYPE bldat
                                iv_belnr      TYPE belnr_d
                      EXPORTING ev_err        TYPE abap_bool
                                ev_obj_key    TYPE bapiache09-obj_key.

ENDCLASS.

DATA go_incassi_ampre_leaseco TYPE REF TO lcl_incassi_ampre_leaseco.

*----------------------------------------------------------------------*
*       CLASS LCL_CLASS_NAME IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_incassi_ampre_leaseco IMPLEMENTATION.

  METHOD execute.

    IF p_prgprt IS INITIAL AND
       p_reginc IS INITIAL.
      MESSAGE 'Select at least one action to continue'(003) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF (   p_reginc IS NOT INITIAL )
      AND  ( ( p_file   IS INITIAL )   OR
             ( p_blart1 IS INITIAL )   OR
             ( p_hkont  IS INITIAL ) ).
      MESSAGE 'Filename, Document type and GL Account are obligatory for incoming registration'(004) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSEIF (   p_prgprt IS NOT INITIAL )
      AND  ( ( p_blart2  IS INITIAL )   OR ( p_budat2  IS INITIAL     ) ).
      MESSAGE 'Document type and Posting Date are obligatory for interface clearing'(005) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    start_container( ).

    IF p_reginc IS NOT INITIAL.
      excel_to_itab( ).
      registrazione_incassi( ).
    ENDIF.

    IF p_prgprt IS NOT INITIAL.
      pareggio_partite( ).
    ENDIF.

    IF ( mt_pareggio_msg IS NOT INITIAL ) OR ( mt_incasso_msg IS NOT INITIAL  ).
      display_msg( ).
    ENDIF.

  ENDMETHOD.                    "EXECUTE

  METHOD registrazione_incassi.

    DELETE mt_incasso WHERE cdnuc NOT IN s_nrcont.

    data_checks( ).

    IF mt_incasso IS INITIAL .
      RETURN.
    ENDIF.

    prepare_bapi_data( ).

  ENDMETHOD.                    "extract_data

  METHOD prepare_bapi_data.

    DATA: ls_bapiache09 TYPE bapiache09,
          lt_bapiaccr09 TYPE bapiaccr09_tab,
          lt_bapiacar09 TYPE bapiacar09_tab,
          lt_bapiacgl09 TYPE bapiacgl09_tab,
          lv_amt_doccur TYPE bapiaccr09-amt_doccur.

    LOOP AT mt_incasso ASSIGNING FIELD-SYMBOL(<ls_incasso>)
    GROUP BY ( bukrs = <ls_incasso>-bukrs xblnr = <ls_incasso>-xblnr )
    ASSIGNING FIELD-SYMBOL(<lg_bukrs_xblnr>).

      DATA(lv_buzei) = 1.

      READ TABLE mt_incasso ASSIGNING FIELD-SYMBOL(<wa_incasso>) WITH KEY bukrs = <lg_bukrs_xblnr>-bukrs
                                                                          xblnr = <lg_bukrs_xblnr>-xblnr.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      ls_bapiache09 = VALUE #(
                        username     = sy-uname
                        comp_code    = <wa_incasso>-bukrs
                        doc_date     = <wa_incasso>-data_incasso
                        pstng_date   = COND #( WHEN p_budat1 IS INITIAL THEN <wa_incasso>-data_incasso ELSE p_budat1 )
                        doc_type     = p_blart1
                        ref_doc_no   = <wa_incasso>-xblnr ).

      lt_bapiacgl09 =  VALUE #( BASE lt_bapiacgl09
      (  itemno_acc      = lv_buzei
         gl_account      = p_hkont
         comp_code       = <wa_incasso>-bukrs ) ).

      lt_bapiaccr09 = VALUE #( BASE lt_bapiaccr09
      (  itemno_acc      = lv_buzei
         currency        = <wa_incasso>-waers ) ).

      LOOP AT GROUP <lg_bukrs_xblnr> ASSIGNING FIELD-SYMBOL(<ls_group_wa>).

        lv_buzei = lv_buzei + 1.

        lt_bapiacar09 = VALUE #( BASE lt_bapiacar09
        (  itemno_acc      = lv_buzei
           customer        = <ls_group_wa>-kunnr ) ).

        lt_bapiaccr09 = VALUE #( BASE lt_bapiaccr09
        (  itemno_acc      = lv_buzei
           currency        = <ls_group_wa>-waers
           amt_doccur      = <ls_group_wa>-imp_alloc * -1  ) ).

        lv_amt_doccur = lv_amt_doccur + <ls_group_wa>-imp_alloc.

      ENDLOOP.

      ASSIGN lt_bapiaccr09[ 1 ] TO FIELD-SYMBOL(<ls_currency>).
      <ls_currency>-amt_doccur = lv_amt_doccur.

      call_bapi(
      EXPORTING
        is_bapiache09 = ls_bapiache09
        it_bapiaccr09 = lt_bapiaccr09
        it_bapiacar09 = lt_bapiacar09
        it_bapiacgl09 = lt_bapiacgl09
        iv_bukrs      = <wa_incasso>-bukrs
        iv_xblnr      = <wa_incasso>-xblnr
        iv_kunnr      = <wa_incasso>-kunnr
        iv_belnr      = <wa_incasso>-belnr
        iv_budat      = <wa_incasso>-budat
        iv_bldat      = <wa_incasso>-data_incasso
      IMPORTING
        ev_err        = DATA(lv_error)
        ev_obj_key    = DATA(lv_obj_key) ).

      CLEAR: ls_bapiache09, lt_bapiaccr09, lt_bapiacar09, lt_bapiacgl09, lv_amt_doccur.
    ENDLOOP.

  ENDMETHOD.

  METHOD data_checks.

    DATA: lv_prev  TYPE altkn,
          lv_kunnr TYPE kunnr,
          lv_altkn TYPE altkn,
          lv_key   TYPE char100.

    SELECT bukrs,
           budat,
           xblnr,
           kunnr,
           belnr,
           waers,
           gjahr
    FROM bsid
      INTO TABLE @DATA(lt_bsid)
      FOR ALL ENTRIES IN @mt_incasso
      WHERE bukrs = @mt_incasso-bukrs
      AND   budat = @mt_incasso-budat
      AND   xblnr = @mt_incasso-xblnr.

    SORT mt_incasso BY altkn.

    LOOP AT mt_incasso ASSIGNING FIELD-SYMBOL(<ls_incasso>).

      CONCATENATE <ls_incasso>-bukrs <ls_incasso>-xblnr INTO DATA(lv_rowkey) SEPARATED BY ''.

      IF lv_rowkey <> lv_key.
        lv_key = lv_rowkey.
        DATA(lv_buzei) = 1.
        CLEAR lv_rowkey.
      ENDIF.

      READ TABLE lt_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>) WITH KEY bukrs = <ls_incasso>-bukrs
                                                                    budat = <ls_incasso>-budat
                                                                    xblnr = <ls_incasso>-xblnr.
      IF ( sy-subrc <> 0 ) OR ( <ls_incasso>-xblnr IS INITIAL ).

        APPEND VALUE #( buzei   = lv_buzei
                        bukrs   = <ls_incasso>-bukrs
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        icon    = icon_red_light
                        belnr   = <ls_incasso>-belnr
                        message = 'Opened invoice not found'(006)
                      ) TO mt_incasso_msg.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.

      <ls_incasso>-waers = <ls_bsid>-waers.

      SELECT SINGLE kunnr
        FROM bseg
        INTO lv_kunnr
        WHERE bukrs = <ls_bsid>-bukrs
        AND   belnr = <ls_bsid>-belnr
        AND   gjahr = <ls_bsid>-gjahr
        AND   koart = 'D'.

      IF sy-subrc <> 0.
        APPEND VALUE #( buzei   = lv_buzei
                        bukrs   = <ls_incasso>-bukrs
                        kunnr   = <ls_incasso>-kunnr
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        icon    = icon_red_light
                        belnr   = <ls_incasso>-belnr
                        message = 'Customer number not found'(007)
                      ) TO mt_incasso_msg.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.

      <ls_incasso>-kunnr = lv_kunnr.

      lv_kunnr = |{ lv_kunnr ALPHA = IN }|.

      SELECT SINGLE altkn
        FROM knb1
        INTO lv_altkn
        WHERE bukrs = <ls_bsid>-bukrs
        AND   kunnr = lv_kunnr.

      lv_altkn = |{ lv_altkn ALPHA = IN }|.

      IF ( sy-subrc <> 0 ) OR ( lv_altkn <> <ls_incasso>-altkn ).
        APPEND VALUE #( buzei   = lv_buzei
                        bukrs   = <ls_incasso>-bukrs
                        kunnr   = <ls_incasso>-kunnr
                        xblnr   = <ls_incasso>-xblnr
                        budat   = <ls_incasso>-budat
                        icon    = icon_red_light
                        belnr   = <ls_incasso>-belnr
                        message = 'Customer code does not match'(008)
                      ) TO mt_incasso_msg.
        <ls_incasso>-blocked = abap_true.
        lv_buzei = lv_buzei + 1.
        CONTINUE.
      ENDIF.

    ENDLOOP.

    DELETE mt_incasso WHERE blocked = abap_true.

  ENDMETHOD.

  METHOD pareggio_partite.

    DATA: blntab         TYPE STANDARD TABLE OF  blntab,
          ftclear        TYPE STANDARD TABLE OF  ftclear,
          ftpost         TYPE STANDARD TABLE OF  ftpost,
          fttax          TYPE STANDARD TABLE OF  fttax,
          ls_pareggio    TYPE ty_incasso,
          auglv          TYPE t041a-auglv,
          lv_buzei       TYPE i,
          lv_wrbtr_total TYPE wrbtr.

    DATA: BEGIN OF ls_return,
            id         TYPE  sy-msgid,
            number     TYPE  sy-msgno,
            type       TYPE  sy-msgty,
            message_v1 TYPE  sy-msgv1,
            message_v2 TYPE  sy-msgv2,
            message_v3 TYPE  sy-msgv3,
            message_v4 TYPE  sy-msgv4,
          END OF ls_return.

    SELECT bukrs
           xblnr
           kunnr
           bldat
           monat
           waers
           shkzg
           blart
           belnr
           bschl
           wrbtr
           sgtxt
    FROM bsid
      INTO CORRESPONDING FIELDS OF TABLE mt_bsid
      WHERE bukrs IN s_bukrs
      AND   kunnr IN s_kunnr
      AND   blart =  p_blart2.

    IF mt_bsid IS INITIAL.
      MESSAGE 'No data found'(016)
       TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT mt_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid>)
    GROUP BY ( bukrs = <ls_bsid>-bukrs  kunnr = <ls_bsid>-kunnr belnr = <ls_bsid>-belnr )
    ASSIGNING FIELD-SYMBOL(<ls_bsid_gr>).

      READ TABLE mt_bsid ASSIGNING FIELD-SYMBOL(<ls_bsid_wa>)
      WITH KEY bukrs = <ls_bsid_gr>-bukrs
               kunnr = <ls_bsid_gr>-kunnr
               belnr = <ls_bsid_gr>-belnr.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_buzei = lv_buzei + 1.

      CALL FUNCTION 'POSTING_INTERFACE_START'
        EXPORTING
          i_client           = sy-mandt
          i_function         = 'C'   " call transaction
          i_mode             = 'N'   " no display (not 'A')
          i_update           = 'S'   " synchron
          i_user             = sy-uname
        EXCEPTIONS
          client_incorrect   = 1
          function_invalid   = 2
          group_name_missing = 3
          mode_invalid       = 4
          update_invalid     = 5
          OTHERS             = 6.

      IF sy-subrc <> 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_message).

        APPEND VALUE #( buzei   = lv_buzei
                        bukrs   = <ls_bsid_wa>-bukrs
                        kunnr   = <ls_bsid_wa>-kunnr
                        xblnr   = <ls_bsid_wa>-xblnr
                        bldat   = <ls_bsid_wa>-bldat
                        budat   = <ls_bsid_wa>-budat
                        icon    = icon_red_light
                        belnr   = <ls_bsid_wa>-belnr
                        message = lv_message
                      ) TO mt_pareggio_msg.
        CLEAR lv_message.
        CONTINUE.
      ENDIF.

      LOOP AT GROUP <ls_bsid_gr> ASSIGNING FIELD-SYMBOL(<ls_group_wa>).

        IF <ls_group_wa>-shkzg = 'H'.

          lv_wrbtr_total = lv_wrbtr_total - <ls_group_wa>-wrbtr.

        ELSEIF <ls_group_wa>-shkzg = 'S'.

          lv_wrbtr_total = lv_wrbtr_total + <ls_group_wa>-wrbtr.

        ENDIF.

      ENDLOOP.

      <ls_bsid_wa>-wrbtr = lv_wrbtr_total.
      CLEAR lv_wrbtr_total.

      fill_clear_table(
        EXPORTING
          is_bsid = <ls_bsid_wa>
        CHANGING
          ct_ftclear = ftclear ).

      fill_post_table(
      EXPORTING
        is_bsid = <ls_bsid_wa>
      CHANGING
        ct_ftpost = ftpost ).


      CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
        EXPORTING
          i_tcode   = 'FB05'
          i_auglv   = 'UMBUCHNG'
          i_xsimu   = COND #( WHEN p_testb2 = abap_true THEN 'X' ELSE '' )
          i_sgfunct = 'C'
        IMPORTING
          e_msgid   = ls_return-id
          e_msgno   = ls_return-number
          e_msgty   = ls_return-type
          e_msgv1   = ls_return-message_v1
          e_msgv2   = ls_return-message_v2
          e_msgv3   = ls_return-message_v3
          e_msgv4   = ls_return-message_v4
        TABLES
          t_ftpost  = ftpost
          t_ftclear = ftclear
          t_fttax   = fttax
          t_blntab  = blntab.

      MESSAGE ID ls_return-id
      TYPE ls_return-type
      NUMBER ls_return-number
      WITH ls_return-message_v1 ls_return-message_v2
           ls_return-message_v3 ls_return-message_v4
      INTO lv_message.

      IF lv_message = 'No batch input data found for dynpro SAPMF05A 0700. SAPDF05X 3100 expected.'.
        lv_message = 'Document check - no errors found'(017).
      ELSEIF lv_message = 'No batch input data found for dynpro SAPDF05X 3100. SAPDF05X 3100 expected.'.
        lv_message = 'Difference is to large for clearing'(018).
        ls_return-type = 'W'.
      ENDIF.

      APPEND VALUE #( buzei   = lv_buzei
                      bukrs   = <ls_bsid_wa>-bukrs
                      kunnr   = <ls_bsid_wa>-kunnr
                      xblnr   = <ls_bsid_wa>-xblnr
                      budat   = p_budat2
                      bldat   = <ls_bsid_wa>-bldat
                      icon    = SWITCH #(
                    ls_return-type
                      WHEN 'E' THEN icon_red_light
                      WHEN 'S' THEN icon_green_light
                      WHEN 'W' THEN icon_yellow_light )
                      belnr   = <ls_bsid_wa>-belnr
                      message = lv_message
                    ) TO mt_pareggio_msg.

      CLEAR: lv_message,ftclear,ls_return.

      IF sy-subrc <> 0 .
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'POSTING_INTERFACE_END'
        EXPORTING
          i_bdcimmed              = 'X'
        EXCEPTIONS
          session_not_processable = 1
          OTHERS                  = 2.

      IF sy-subrc <> 0.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.

        APPEND VALUE #( buzei   = lv_buzei
                        bukrs   = <ls_bsid_wa>-bukrs
                        kunnr   = <ls_bsid_wa>-kunnr
                        xblnr   = <ls_bsid_wa>-xblnr
                        budat   = p_budat2
                        bldat   = <ls_bsid_wa>-bldat
                        icon    = icon_red_light
                        belnr   = <ls_bsid_wa>-belnr
                        message = lv_message
                      ) TO mt_pareggio_msg.
      ENDIF.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM mt_pareggio_msg COMPARING message belnr bukrs.

  ENDMETHOD.                    "extract_data

  METHOD fill_clear_table.

    ct_ftclear = VALUE #(
   ( agkoa = 'D' xnops = 'X' agbuk = is_bsid-bukrs agkon = is_bsid-kunnr
     selvon = is_bsid-belnr selbis = is_bsid-belnr selfd = 'BELNR' )
   ( agkoa = 'D' xnops = 'X' agbuk = is_bsid-bukrs agkon = is_bsid-kunnr
     selvon = is_bsid-xblnr selbis = is_bsid-xblnr selfd = 'XBLNR' ) ).

  ENDMETHOD.


  METHOD fill_post_table.

    DATA: lv_bldat TYPE bdc_fval,
          lv_budat TYPE bdc_fval,
          lv_wrbrt TYPE string.

    WRITE sy-datum       TO lv_bldat DD/MM/YYYY.
    WRITE p_budat2       TO lv_budat DD/MM/YYYY.
    lv_wrbrt = is_bsid-wrbtr.
    TRANSLATE lv_wrbrt USING '.,'.

    ct_ftpost = VALUE #(
    ( stype = 'K' count = '001' fnam = 'BKPF-BUKRS'  fval = is_bsid-bukrs )
    ( stype = 'K' count = '001' fnam = 'BKPF-BLDAT'  fval = lv_bldat )
    ( stype = 'K' count = '001' fnam = 'BKPF-BUDAT'  fval = lv_budat )
    ( stype = 'K' count = '001' fnam = 'BKPF-BLART'  fval = is_bsid-blart )
    ( stype = 'K' count = '001' fnam = 'BKPF-WAERS'  fval = is_bsid-waers )
    ( stype = 'K' count = '001' fnam = 'BKPF-MONAT'  fval = is_bsid-monat )
    ( stype = 'K' count = '001' fnam = 'BKPF-XBLNR'  fval = is_bsid-xblnr )
    ( stype = 'K' count = '001' fnam = 'BKPF-BKTXT'  fval = 'DOC CLEARING' )
    ( stype = 'K' count = '001' fnam = 'BSEG-SGTXT'  fval = is_bsid-sgtxt )
    ( stype = 'P' count = '002' fnam = 'RF05A-NEWBS' fval = '07' )
    ( stype = 'P' count = '002' fnam = 'RF05A-NEWKO' fval = is_bsid-kunnr )
    ( stype = 'P' count = '002' fnam = 'BSEG-WRBTR'  fval = lv_wrbrt )
     ).

  ENDMETHOD.

  METHOD display_msg.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list,
          lr_display   TYPE REF TO cl_salv_display_settings.


    IF mt_incasso_msg IS  INITIAL OR mt_pareggio_msg IS INITIAL.

      mo_split_row->set_column_sash( id    = 1
                                     type  = cl_gui_splitter_container=>type_sashvisible
                                     value = cl_gui_splitter_container=>false ).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
              EXPORTING
              r_container = mo_split_row->get_container( row = 1 column = 1 )
          IMPORTING
            r_salv_table = mo_incasso_msg
          CHANGING
            t_table      = mt_incasso_msg ).

      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    lr_display = mo_incasso_msg->get_display_settings( ).
    lr_display->set_list_header( 'Incoming Registration Logs'(009) ).
    mo_incasso_msg->get_functions( )->set_all( 'X' ).
    adjust_cols_msg( CHANGING co_salv_msg = mo_incasso_msg ).
    mo_incasso_msg->display( ).

    TRY.
        cl_salv_table=>factory(
              EXPORTING
              r_container = mo_split_row->get_container( row = 1 column = 2 )
          IMPORTING
            r_salv_table = mo_pareggio_msg
          CHANGING
            t_table      = mt_pareggio_msg ).

      CATCH cx_salv_msg INTO lx_msg.
        MESSAGE lx_msg->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
    ENDTRY.

    lr_display = mo_pareggio_msg->get_display_settings( ).
    lr_display->set_list_header( 'Interface Clearing Logs'(010) ).
    mo_pareggio_msg->get_functions( )->set_all( 'X' ).
    adjust_cols_msg( CHANGING co_salv_msg = mo_pareggio_msg ).
    mo_pareggio_msg->display( ).


    IF mt_incasso_msg IS  INITIAL.

      mo_split_row->set_column_width(
        EXPORTING
          id                = 1
          width             = 0
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).
    ELSEIF mt_pareggio_msg IS INITIAL.

      mo_split_row->set_column_width(
        EXPORTING
          id                = 2
          width             = 0
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3 ).

    ENDIF.

    CALL SCREEN 100.

  ENDMETHOD.

  METHOD adjust_cols_msg.

    DATA lo_column TYPE REF TO cl_salv_column_table.

    DATA(lo_cols) = co_salv_msg->get_columns( ).

    TRY.

        lo_column ?= lo_cols->get_column( 'BUZEI' ).
        lo_column->set_key( ).

        DATA(lv_text) = CONV scrtext_s( 'Status'(011) ).
        lo_column ?= lo_cols->get_column( 'ICON' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).
        lo_column->set_icon( ).

        lv_text = 'Message'(012).
        lo_column ?= lo_cols->get_column( 'MESSAGE' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

        lv_text = 'Control Account'(013).
        lo_column ?= lo_cols->get_column( 'CDNUC' ).
        lo_column->set_short_text( CONV #( lv_text ) ).
        lo_column->set_medium_text( CONV #( lv_text ) ).
        lo_column->set_long_text( CONV #( lv_text ) ).

      CATCH cx_salv_not_found.
    ENDTRY.
    lo_cols->set_optimize(  ).

  ENDMETHOD.


  METHOD excel_to_itab.

    DATA: ls_incasso   TYPE ty_incasso,
          lv_regex     TYPE string VALUE '^-?\d+(\.\d+)?$',
          lv_match     TYPE abap_bool,
          lv_imp_val   TYPE char100,
          lv_doc_dat   TYPE char100,
          lv_missing   TYPE char100,
          lv_message   TYPE string,
          lv_incorrect TYPE abap_bool.

    DATA(lv_filename) = CONV file_table-filename( p_file ).

    CALL FUNCTION 'Z_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = lv_filename
        i_begin_col             = 1
        i_begin_row             = 1
        i_end_col               = 23
        i_end_row               = 50000
      TABLES
        e_tab                   = mt_excel
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    DELETE mt_excel INDEX 1.

    IF mt_excel IS INITIAL.
      MESSAGE 'Excel worksheet can not be empty!'(015)
         TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lv_imp_val   = 'Importing value of excel line'(019).
    lv_doc_dat   = 'Importing date of excel line'(020).
    lv_missing   = 'is missing or not in the correct format'(021).

    LOOP AT mt_excel ASSIGNING FIELD-SYMBOL(<ls_exc>) WHERE i IN s_nrcont.

      DATA(lv_excl_row) = CONV string( sy-tabix + 1 ).
      DATA(lv_buzei) = 1.

      CLEAR: lv_match, lv_message, lv_incorrect.
      TRANSLATE <ls_exc>-g USING ',.'.
      FIND REGEX lv_regex IN <ls_exc>-g MATCH COUNT lv_match.

      IF lv_match = 0.

        CONCATENATE lv_imp_val lv_excl_row lv_missing INTO lv_message SEPARATED BY space.

        APPEND VALUE #( buzei   = lv_buzei
                        xblnr   = <ls_exc>-o
                        icon    = icon_red_light
                        message = lv_message
                      ) TO mt_incasso_msg.
        lv_incorrect = abap_true.
        lv_buzei = lv_buzei + 1.
      ENDIF.

      MOVE <ls_exc>-b TO ls_incasso-data_incasso.

      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = ls_incasso-data_incasso
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.

        CONCATENATE lv_doc_dat lv_excl_row lv_missing INTO lv_message SEPARATED BY space.

        APPEND VALUE #( buzei   = lv_buzei
                        xblnr   = <ls_exc>-o
                        icon    = icon_red_light
                        message = lv_message
                      ) TO mt_incasso_msg.
        CLEAR ls_incasso.
        lv_incorrect = abap_true.
      ENDIF.

      IF lv_incorrect = abap_true.
        CONTINUE.
      ENDIF.

      MOVE <ls_exc>-p TO ls_incasso-budat.
      MOVE <ls_exc>-g TO ls_incasso-imp_alloc.
      MOVE <ls_exc>-h TO ls_incasso-altkn.
      MOVE <ls_exc>-i TO ls_incasso-cdnuc.
      MOVE <ls_exc>-o TO ls_incasso-xblnr.
      APPEND  ls_incasso TO mt_incasso.
      CLEAR ls_incasso.
    ENDLOOP.

    SELECT control_account,
         bukrs
    FROM yfcms_amex_buk
    INTO TABLE @DATA(lt_bukrs_decode)
    FOR ALL ENTRIES IN @mt_incasso
    WHERE control_account = @mt_incasso-cdnuc.

    LOOP AT mt_incasso ASSIGNING FIELD-SYMBOL(<ls_incasso>).

      READ TABLE lt_bukrs_decode ASSIGNING FIELD-SYMBOL(<ls_bukrs_decode>) WITH KEY control_account = <ls_incasso>-cdnuc.
      IF sy-subrc = 0.
        <ls_incasso>-bukrs = <ls_bukrs_decode>-bukrs.
      ENDIF.
    ENDLOOP.

    mt_pareggio = mt_incasso.

  ENDMETHOD.

  METHOD value_request.

    DATA: lt_filetable TYPE filetable,
          lv_rc        TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        default_filename      = '*.XLS'
        file_filter           = '*.XLS'
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

  METHOD call_bapi.
    DATA: lt_return     TYPE  bapiret2_tab.

    IF p_testb1 = abap_true.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = is_bapiache09
        TABLES
          accountgl         = it_bapiacgl09
          currencyamount    = it_bapiaccr09
          accountreceivable = it_bapiacar09
          return            = lt_return.

    ELSE.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = is_bapiache09
        IMPORTING
          obj_key           = ev_obj_key
        TABLES
          accountgl         = it_bapiacgl09
          currencyamount    = it_bapiaccr09
          accountreceivable = it_bapiacar09
          return            = lt_return.

    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_return COMPARING message.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      APPEND VALUE #( buzei   = sy-tabix
                      bukrs   = iv_bukrs
                      kunnr   = iv_kunnr
                      xblnr   = iv_xblnr
                      budat   = iv_budat
                      bldat   = iv_bldat
                      icon    = SWITCH #(
                    <ls_return>-type
                      WHEN 'E' THEN icon_red_light
                      WHEN 'S' THEN icon_green_light
                      WHEN 'W' THEN icon_yellow_light )
                      belnr  = ev_obj_key
                    message = <ls_return>-message
                     ) TO mt_incasso_msg.

      IF  <ls_return>-type CA 'EAX'.
        ev_err = abap_true.
      ENDIF.
    ENDLOOP.

    IF ev_err = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSEIF p_testb1 IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDMETHOD.

  METHOD start_container.

    DATA:  lo_cont   TYPE REF TO cl_gui_custom_container.

    CREATE OBJECT lo_cont
      EXPORTING
        container_name              = 'CUSTOM_CONTAINER'
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

    mo_split_row->set_column_sash( id    = 1
                                   type  = cl_gui_splitter_container=>type_sashvisible
                                   value = cl_gui_splitter_container=>true ).

    mo_split_row->set_column_width(
      EXPORTING
        id                = 1
        width             = 50
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).


  ENDMETHOD.                    "start_container


ENDCLASS.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_incassi_ampre_leaseco=>value_request( ).

INITIALIZATION.
  CLEAR s_bukrs.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '3532' ) TO s_bukrs.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = '3616' ) TO s_bukrs.

  CLEAR s_nrcont.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'AM' ) TO s_nrcont.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CV' ) TO s_nrcont.

START-OF-SELECTION.
  go_incassi_ampre_leaseco = NEW lcl_incassi_ampre_leaseco( ).
  go_incassi_ampre_leaseco->execute( ).
