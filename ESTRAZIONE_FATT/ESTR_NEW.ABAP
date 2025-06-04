*&---------------------------------------------------------------------*
*&  Include           YFICMS_ESTRAT_FAT_TOP
*&---------------------------------------------------------------------*
REPORT yficms_estrattore_fatture_v2.

TABLES: bkpf.

DATA: BEGIN OF gs_screen100,
        ok_code TYPE sy-ucomm,
      END OF gs_screen100.

*Dati Estrazione
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_integ   TYPE bkpf-bukrs DEFAULT '0017'.           " Codice Prelios Integra
SELECT-OPTIONS: s_dt_reg  FOR bkpf-budat,                       " Date registrazione Integra
                s_ft_int  FOR bkpf-blart DEFAULT 'K*' TO 'M*'.  " Tipo documento Integra
SELECTION-SCREEN END OF BLOCK b1.
*Fondo SGR
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_fondo TYPE bkpf-bukrs,                            " Fondo SGR
            p_for_i TYPE bseg-lifnr,                            " Fornitore Prelios Integra
            p_cli_f TYPE bseg-kunnr.                            " Cliente Fondo SGR
SELECTION-SCREEN END OF BLOCK b2.
*Dati documenti per il ribaltamento
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_dt_r   TYPE bkpf-budat,                           " Data registrazione documenti contabili
            p_td_fat TYPE bkpf-blart,                           " Tipo documento fattura attiva
            p_td_nc  TYPE bkpf-blart,                           " Tipo documento nota di credito attiva
            p_td_gir TYPE bkpf-blart DEFAULT 'SA',              " Tipo documento giroconto
            p_rec_cs TYPE bseg-hkont DEFAULT 'M240700000',      " Conto recupero costi
            p_cdc    TYPE bseg-kostl DEFAULT 'EXXXXX',          " Centro di costo giroconto costi
            p_bsart  TYPE ekko-bsart DEFAULT 'ZFTE' NO-DISPLAY, " Tipo Ordine di acquisto
            p_ekorg  TYPE ekko-ekorg DEFAULT 'OPRE' NO-DISPLAY, " Organizzazione acquisti
            p_ekgrp  TYPE ekko-ekgrp DEFAULT '010'  NO-DISPLAY, " Gruppo acquisti
            p_waers  TYPE bkpf-waers DEFAULT 'EUR'  NO-DISPLAY, " Divisa
            p_meins  TYPE ekpo-meins DEFAULT 'ATT'  NO-DISPLAY, " Unità di misura
            p_rif_gc TYPE bkpf-xblnr DEFAULT 'GC COSTI/RICAVI' NO-DISPLAY. " Riferimento Giroconto
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
*       CLASS lcl_estrattore_fatture DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_estrattore_fatture DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS execute.
    CLASS-METHODS initialization.
    CLASS-METHODS at_selection_screen RETURNING VALUE(ev_error) TYPE abap_bool.

  PRIVATE SECTION.

    TYPES: ty_fattura_type TYPE yficms_estrat_fat_out_main,
           BEGIN OF ty_invoice,
             bukrs  TYPE bkpf-bukrs,
             belnr  TYPE bkpf-belnr,
             gjahr  TYPE bkpf-gjahr,
             errore TYPE char1,
           END OF ty_invoice,
           tt_invoice TYPE STANDARD TABLE OF ty_invoice.

    TYPES: BEGIN OF ty_cod_iva,
             mwskz TYPE bseg-mwskz,
             kschl TYPE a003-kschl,
             ktosl TYPE bset-ktosl,
             msatz TYPE msatz_f05l,
             hkont TYPE bseg-hkont,
             wrbtr TYPE bseg-wrbtr,
             hwbas TYPE bset-hwbas,
             hwste TYPE bset-hwste,
           END OF ty_cod_iva,
           tt_cod_iva TYPE SORTED TABLE OF ty_cod_iva WITH UNIQUE KEY mwskz.

    TYPES: BEGIN OF ty_appunto,
             itemno_acc TYPE posnr_acc,
             text       TYPE string,
           END OF ty_appunto,
           tt_appunto TYPE STANDARD TABLE OF ty_appunto.

    TYPES: BEGIN OF ty_fattura.
             INCLUDE TYPE ty_fattura_type.
             TYPES: knttp   TYPE ekpo-knttp,
             blocked TYPE char1.
    TYPES: END OF ty_fattura,
    tt_fattura TYPE TABLE OF ty_fattura,
      tr_prctr   TYPE RANGE OF cepc-prctr.

    DATA: BEGIN OF ms_zfte_tables, "le tabelle per creazione OdA ZFTE
            zfte_poheader    TYPE bapimepoheader,
            zfte_poheaderx   TYPE bapimepoheaderx,
            zfte_poitem      TYPE bapimepoitem_tp,
            zfte_poitemx     TYPE bapimepoitemx_tp,
            zfte_poschedule  TYPE bapimeposchedule_tp,
            zfte_poschedulex TYPE bapimeposchedulx_tp,
            zfte_poaccount   TYPE bapimepoaccount_tp,
            zfte_poaccountx  TYPE bapimepoaccountx_tp,
          END OF ms_zfte_tables.

    DATA: BEGIN OF ms_fatt_tables, "le tabelle per fattura attiva
            fatt_documentheader    TYPE bapiache09,
            fatt_accountgl         TYPE bapiacgl09_tab,
            fatt_accountreceivable TYPE bapiacar09_tab,
            fatt_accounttax        TYPE bapiactx09_tab,
            fatt_currencyamount    TYPE bapiaccr09_tab,
            fatt_extension2        TYPE bapiparex_tp,
            fatt_realestate        TYPE bapiacre09_tab,
          END OF ms_fatt_tables.

    DATA: BEGIN OF ms_ntdc_tables, "le tabelle per nota di credito
            ntdc_documentheader    TYPE bapiache09,
            ntdc_accountgl         TYPE bapiacgl09_tab,
            ntdc_accountreceivable TYPE bapiacar09_tab,
            ntdc_accounttax        TYPE bapiactx09_tab,
            ntdc_currencyamount    TYPE bapiaccr09_tab,
            ntdc_realestate        TYPE bapiacre09_tab,
            ntdc_appunto           TYPE tt_appunto,
          END OF ms_ntdc_tables.

    DATA: BEGIN OF ms_gccr_tables, "le tabelle per giroconto costo/ricavi
            gccr_documentheader TYPE bapiache09,
            gccr_accountgl      TYPE bapiacgl09_tab,
            gccr_currencyamount TYPE bapiaccr09_tab,
          END OF ms_gccr_tables.

    DATA: BEGIN OF ms_gdrc_tables, "le tabelle per creazione BEM
            gdrc_goodsmvt_header TYPE bapi2017_gm_head_01,
            gdrc_goodsmvt_item   TYPE tab_bapi_goodsmvt_item,
          END OF ms_gdrc_tables.

    CONSTANTS: BEGIN OF ms_status,
                 fatto  TYPE char1 VALUE 'S',
                 attesa TYPE char1 VALUE 'W',
                 errore TYPE char1 VALUE 'E',
               END OF ms_status.

    CONSTANTS: mc_output_str TYPE dd02l-tabname VALUE 'YFICMS_ESTRAT_FAT_OUT_MAIN',
               mc_fatt       TYPE char20 VALUE 'FATTURA',
               mc_ndc        TYPE char20 VALUE 'NOTA DI CREDITO'.

    DATA: mt_fattura      TYPE tt_fattura,
          mt_fattura_copy TYPE tt_fattura,
          mt_logs         TYPE bapiret2_t,
          mo_grid         TYPE REF TO cl_gui_alv_grid,
          mo_top          TYPE REF TO cl_dd_document,
          mo_top_cnt      TYPE REF TO cl_gui_container,
          mv_registered   TYPE char1,
          mv_ribal_status TYPE char1 VALUE ms_status-attesa,
          mt_fat_rev      TYPE tt_invoice,
          mt_fat_imm      TYPE tt_invoice,
          mt_nc_rev       TYPE tt_invoice,
          mt_nc_imm       TYPE tt_invoice,
          mv_test         TYPE abap_bool VALUE abap_true,
          mr_msg_type     TYPE RANGE OF bapiret2-type.

    METHODS extract_data.

    METHODS post_ribaltamento.

    METHODS display_logs.

    METHODS update_record_status IMPORTING it_fatture TYPE tt_invoice
                                           iv_msg     TYPE sy-msgv1.
    METHODS start_container.

    METHODS display_invoice.

    METHODS refresh_top_of_page.

    METHODS fill_ribal_tables       EXPORTING ev_error   TYPE abap_bool.  "compilazione delle tabelle per ribaltamento

    METHODS fill_gccr_tables        IMPORTING it_fatture TYPE tt_invoice. "compilazione delle tabelle per giroconto

    METHODS fill_ntdc_tables        IMPORTING it_fatture TYPE tt_invoice  "compilazione delle tabelle per nota di credito
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS fill_fatt_tables        IMPORTING it_fatture TYPE tt_invoice  "compilazione delle tabelle per fattura attiva
                                              iv_ebeln   TYPE ebeln
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS fill_gdrc_tables        IMPORTING it_fatture TYPE tt_invoice  "compilazione delle tabelle per crezione BEM
                                              iv_ebeln   TYPE ebeln.

    METHODS fill_zfte_tables        IMPORTING it_fatture TYPE tt_invoice  "compilazione delle tabelle per creazione OdA ZFTE
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS set_fcat                EXPORTING ev_fieldcat TYPE lvc_t_fcat.

    METHODS enrich_data             IMPORTING ir_prctr   TYPE tr_prctr.

    METHODS check_goods_received    IMPORTING iv_tabname TYPE tabname     "controllo per crezione BEM
                                              it_fatture TYPE tt_invoice.
    METHODS message_init_show       IMPORTING iv_action  TYPE char4.

    METHODS execute_po_creation     IMPORTING iv_tabname TYPE tabname      "registrazione delle creazione OdA ZFTE
                                              it_fatture TYPE tt_invoice
                                    EXPORTING ev_ebeln   TYPE ebeln
                                              ev_error   TYPE abap_bool.

    METHODS block_unblock_document  IMPORTING iv_action       TYPE char7   "esclude/include documento
                                              it_selected_doc TYPE tt_invoice.

    METHODS post_active_invoices    IMPORTING iv_tabname TYPE tabname      "registrazione delle fatture attive
                                              iv_ebeln   TYPE ebeln
                                              it_fatture TYPE tt_invoice
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS post_goods_received     IMPORTING iv_tabname TYPE tabname      "registrazione delle BEM
                                              iv_ebeln   TYPE ebeln
                                              it_fatture TYPE tt_invoice
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS post_credit_note        IMPORTING iv_tabname TYPE tabname      "registrazione delle nota di credito
                                              it_fatture TYPE tt_invoice
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS post_giroconto          IMPORTING it_fatture TYPE tt_invoice   "registrazione delle giroconto
                                              iv_tabname TYPE tabname
                                    EXPORTING ev_error   TYPE abap_bool.

    METHODS pop_up_info             IMPORTING iv_title  TYPE string
                                              iv_text   TYPE string
                                    CHANGING  cv_answer TYPE char01.

    METHODS message_store           IMPORTING iv_arbgb  TYPE smesg-arbgb OPTIONAL
                                              iv_msgty  TYPE smesg-msgty OPTIONAL
                                              iv_msg1   TYPE sy-msgv1
                                              iv_msg2   TYPE sy-msgv2
                                              iv_msg3   TYPE sy-msgv3
                                              iv_msg4   TYPE sy-msgv4
                                              iv_txtnr  TYPE numc3       OPTIONAL
                                              iv_lineno TYPE i.

    METHODS handle_toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid IMPORTING e_object .

    METHODS top_of_page         FOR EVENT top_of_page  OF cl_gui_alv_grid.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm .
ENDCLASS.                    "

DATA go_estrattore_fatture TYPE REF TO lcl_estrattore_fatture.
*&---------------------------------------------------------------------*
*& Report  YFICMS_ESTRATTORE_FATTURE_V2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE yficms_estrat_fat_top.
INCLUDE yficms_estrat_fat_pbo.
INCLUDE yficms_estrat_fat_pai.

*----------------------------------------------------------------------*
*       CLASS lcl_estrattore_fatture IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_estrattore_fatture IMPLEMENTATION.

  METHOD execute.

    start_container( ).
    display_invoice( ).

    IF mt_fattura IS NOT INITIAL.
      CALL SCREEN 0100.
    ELSE.
      MESSAGE 'Nessun dato estratto' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "execute


  METHOD handle_user_command.

    DATA: lt_selected_doc TYPE tt_invoice,
          lv_error        TYPE abap_bool.

    mr_msg_type = VALUE #( sign = 'I' option = 'EQ' ( low  = 'E' ) ( low  = 'A' ) ( low  = 'X' ) ).

    CASE e_ucomm.
      WHEN 'FC_EXCLUDE' OR 'FC_ANULLA'.

        mo_grid->get_selected_rows(
          IMPORTING
            et_index_rows = DATA(lt_selected_rows) ).

        DATA(lv_selected_nr)  = lines( lt_selected_rows ).
        CLEAR lt_selected_doc.

        IF lv_selected_nr = 0.
          MESSAGE 'Selezionare almeno una riga' TYPE 'I' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        IF e_ucomm = 'FC_EXCLUDE'.
          DATA(lv_message) = EXACT string('Selezionare solo righe da escludere').
          DATA(lv_action)  = EXACT char7( 'BLOCK' ) .
          DATA(lv_status)  = EXACT char8( '' ).
        ELSE.
          lv_message = 'Selezionare solo righe escluse per cui si vuole procedere con l’annullo'.
          lv_action  = 'UNBLOCK'.
          lv_status  = 'BLOCCATO'.
        ENDIF.

        LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_selected_row>).
          READ TABLE mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>) INDEX <ls_selected_row>-index.
          IF sy-subrc = 0.
            IF <ls_fattura>-xref2_hd <> lv_status.
              MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
              RETURN.
            ENDIF.
            APPEND VALUE #( bukrs = <ls_fattura>-bukrs belnr = <ls_fattura>-belnr gjahr = <ls_fattura>-gjahr ) TO lt_selected_doc.
          ENDIF.
        ENDLOOP.

        block_unblock_document(
          EXPORTING
            iv_action = lv_action
            it_selected_doc = lt_selected_doc ).

      WHEN 'FC_RIBALTAMENTO'.

        IF mv_registered = abap_true.
          MESSAGE 'ATTENZIONE Registrazioni già avvenute' TYPE 'I'.
          RETURN.
        ENDIF.

        CLEAR: mt_fat_imm, mt_fat_rev, mt_nc_imm, mt_nc_rev.

        fill_ribal_tables(
          IMPORTING
            ev_error = lv_error ).

        IF lv_error = abap_true.
          RETURN.
          refresh_top_of_page( ).
        ENDIF.

        mt_fattura_copy = mt_fattura.
        SORT mt_fattura_copy BY bukrs belnr gjahr.

        post_ribaltamento( ).

        LOOP AT mt_logs ASSIGNING FIELD-SYMBOL(<ls>) WHERE type IN mr_msg_type.
          MESSAGE 'Attenzione: sussistono errori bloccanti (vedi LOG dedicato)' TYPE 'I' DISPLAY LIKE 'E'.
          mv_registered = abap_false.
          mv_ribal_status = ms_status-errore.
          refresh_top_of_page( ).
          RETURN.
        ENDLOOP.

        CLEAR: mv_test, mt_logs.

        post_ribaltamento( ).
        mv_test = abap_true.

        LOOP AT mt_logs ASSIGNING <ls> WHERE type IN mr_msg_type.
          MESSAGE 'Attenzione: sussistono errori bloccanti (vedi LOG dedicato)' TYPE 'I' DISPLAY LIKE 'E'.
          mv_registered = abap_false.
          mv_ribal_status = ms_status-errore.
          refresh_top_of_page( ).
          RETURN.
        ENDLOOP.

        MESSAGE 'OK - Registrazioni avvenute con successo (Vedi LOG dedicato)' TYPE 'I' DISPLAY LIKE 'E'.
        mv_registered   = abap_true.
        mv_ribal_status = ms_status-fatto.

        CLEAR: mt_fattura_copy.

      WHEN 'FC_LOGS'.

        IF mt_logs IS INITIAL.
          MESSAGE 'Nessun Log da riportare' TYPE 'I'.
          RETURN.
        ENDIF.
        display_logs( ).

    ENDCASE.
    refresh_top_of_page( ).
  ENDMETHOD.                    "handle_user_command

  METHOD block_unblock_document.

    DATA: lv_answer       TYPE char1.

    CASE iv_action.
      WHEN 'BLOCK'.
        DATA(lv_icon)    = EXACT icon-id( icon_interval_exclude_red ).
        DATA(lv_xref_hd) = 'BLOCCATO'.
        DATA(lv_message) = 'ATTENZIONE: Il documento risulta bloccato per il ribaltamento'.
        DATA(lv_pop_up_message) = EXACT string('Si desidera procedere con il blocco delle fatture bloccate?').
      WHEN 'UNBLOCK'.
        lv_icon    =  icon_led_green.
        lv_xref_hd = ''.
        lv_message = 'OK – Documento ribaltabile'.
        lv_pop_up_message = 'Si desidera procedere togliere il blocco alle fatture selezionate. Continuare?'.
    ENDCASE.

    pop_up_info(
      EXPORTING
        iv_title  = 'ATTENZIONE'
        iv_text   = lv_pop_up_message
      CHANGING
        cv_answer = lv_answer ).

    IF lv_answer = 2.
      RETURN.
    ENDIF.

    LOOP AT it_selected_doc ASSIGNING FIELD-SYMBOL(<ls_selected_doc>)
    GROUP BY ( bukrs = <ls_selected_doc>-bukrs
               belnr = <ls_selected_doc>-belnr
               gjahr = <ls_selected_doc>-gjahr ).

      SELECT SINGLE *
        FROM bkpf
        INTO @DATA(ls_bkpf_old)
        WHERE bukrs = @<ls_selected_doc>-bukrs
        AND   belnr = @<ls_selected_doc>-belnr
        AND   gjahr = @<ls_selected_doc>-gjahr.

      UPDATE bkpf SET xref2_hd = lv_xref_hd WHERE bukrs = <ls_selected_doc>-bukrs
                                            AND   belnr = <ls_selected_doc>-belnr
                                            AND   gjahr = <ls_selected_doc>-gjahr.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        MESSAGE 'Errore aggiornamento Blocco documento' TYPE 'I' DISPLAY LIKE 'E'.
        mv_ribal_status = ms_status-errore.
        RETURN.
      ENDIF.

      COMMIT WORK.

      DATA(lv_objectid) = EXACT cdhdr-objectid( sy-mandt && <ls_selected_doc>-bukrs && <ls_selected_doc>-belnr && <ls_selected_doc>-gjahr ).

      CALL FUNCTION 'CHANGEDOCUMENT_OPEN'
        EXPORTING
          objectclass      = 'BELEG'
          objectid         = lv_objectid
        EXCEPTIONS
          sequence_invalid = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      DATA(ls_bkpf_new) = ls_bkpf_old.
      ls_bkpf_new-xref2_hd = lv_xref_hd.

      CALL FUNCTION 'CHANGEDOCUMENT_SINGLE_CASE'
        EXPORTING
          change_indicator       = 'U'
          tablename              = 'BKPF'
          workarea_new           = ls_bkpf_new
          workarea_old           = ls_bkpf_old
        EXCEPTIONS
          nametab_error          = 1
          open_missing           = 2
          position_insert_failed = 3
          OTHERS                 = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'CHANGEDOCUMENT_CLOSE'
        EXPORTING
          date_of_change         = sy-datum
          objectclass            = 'BELEG'
          objectid               = lv_objectid
          tcode                  = sy-tcode
          time_of_change         = sy-uzeit
          username               = sy-uname
        EXCEPTIONS
          header_insert_failed   = 1
          no_position_inserted   = 2
          object_invalid         = 3
          open_missing           = 4
          position_insert_failed = 5
          OTHERS                 = 6.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      MODIFY mt_fattura FROM VALUE #( id = lv_icon xref2_hd = lv_xref_hd mess = lv_message )
      TRANSPORTING id mess xref2_hd WHERE bukrs = <ls_selected_doc>-bukrs
                                    AND   belnr = <ls_selected_doc>-belnr
                                    AND   gjahr = <ls_selected_doc>-gjahr.
    ENDLOOP.
  ENDMETHOD.                    "block_unblock_document

  METHOD fill_ribal_tables.

    DATA: lr_mwskz_rev TYPE RANGE OF bseg-mwskz,
          lt_ebeln_rev TYPE ebeln,
          lt_ebeln_imm TYPE ebeln.

    SELECT 'I'            AS sign,
           'EQ'           AS option,
           mwskz_acquisti AS low
      FROM yfcms_fatt_iva
      INTO CORRESPONDING FIELDS OF TABLE @lr_mwskz_rev
      WHERE reverse_charge = 'X'.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<fs>) WHERE xref2_hd IS INITIAL
    GROUP BY ( bukrs = <fs>-bukrs belnr = <fs>-belnr gjahr = <fs>-gjahr tipo = <fs>-tipo )
    ASSIGNING FIELD-SYMBOL(<ls_doc_group>).

      DATA(lv_reverse_charg) = abap_false.
      DATA(lv_count) = 0.

      LOOP AT GROUP <ls_doc_group> ASSIGNING FIELD-SYMBOL(<ls>).
        lv_count = lv_count + 1.
        IF lr_mwskz_rev IS NOT INITIAL AND <ls>-mwskz IN lr_mwskz_rev.
          lv_reverse_charg = abap_true.
        ENDIF.
      ENDLOOP.

      CASE <ls_doc_group>-tipo.
        WHEN mc_fatt.
          IF lv_count > 950.
            DATA(lv_error_cnt) = abap_true.
            ev_error           = abap_true.
          ENDIF.
          IF lv_reverse_charg = abap_true.
            IF lv_error_cnt IS NOT INITIAL.
              APPEND VALUE #( message_v1 = 'ERRORE: Numerosità righe fattura reverse charge'
                              message_v2 = 'supera il limite massimo di 950 righe' ) TO mt_logs.
              DATA(lv_message) = EXACT char100( 'ERRORE: Numerosità righe fattura reverse charge supera il limite massimo di 950 righe' ).
              mv_ribal_status = ms_status-errore.
            ELSE.
              APPEND CORRESPONDING #( <ls_doc_group> ) TO mt_fat_rev.
            ENDIF.
          ELSE.
            IF lv_error_cnt IS NOT INITIAL.
              APPEND VALUE #( message_v1 = 'ERRORE: Numerosità righe fattura esigibilità'
                              message_v2 = 'supera il limite massimo di 950 righe' ) TO mt_logs.
              lv_message = 'ERRORE: Numerosità righe fattura esigibilità immediata supera il limite massimo di 950 righe'.
              mv_ribal_status = ms_status-errore.
            ELSE.
              APPEND CORRESPONDING #( <ls_doc_group> ) TO mt_fat_imm.
            ENDIF.
          ENDIF.
        WHEN mc_ndc.
          IF lv_count > 450.
            lv_error_cnt = abap_true.
            ev_error     = abap_true.
          ENDIF.
          IF lv_reverse_charg = abap_true.
            IF lv_error_cnt IS NOT INITIAL.
              APPEND VALUE #( message_v1 = 'ERRORE: Numerosità righe note di credito'
                              message_v2 = 'reverse charge supera il limite'
                              message_v3 = 'massimo di 950 righe' ) TO mt_logs.
              lv_message = 'ERRORE: Numerosità righe nota di credito reverse charge supera il limite massimo di 450 righe'.
              mv_ribal_status = ms_status-errore.
            ELSE.
              APPEND CORRESPONDING #( <ls_doc_group> ) TO mt_nc_rev.
            ENDIF.
          ELSE.
            IF lv_error_cnt IS NOT INITIAL.
              APPEND VALUE #( message_v1 = 'ERRORE: Numerosità righe note di credito'
                              message_v2 = 'esigibilità immediata supera il limite massimo di'
                              message_v3 = '450 righe' ) TO mt_logs.
              lv_message = 'ERRORE: Numerosità righe note di credito esigibilità immediata supera il limite massimo di 450 righe'.
              mv_ribal_status = ms_status-errore.
            ELSE.
              APPEND CORRESPONDING #( <ls_doc_group> ) TO mt_nc_imm.
            ENDIF.
          ENDIF.
      ENDCASE.

      IF lv_error_cnt IS NOT INITIAL.
        LOOP AT GROUP <ls_doc_group> ASSIGNING <ls>.
          <ls>-id = icon_led_red.
          <ls>-mess = lv_message.
        ENDLOOP.
      ENDIF.

      CLEAR: lv_error_cnt, lv_reverse_charg, lv_message.
    ENDLOOP.

    IF ev_error IS NOT INITIAL.
      APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di classificazione delle tabelle' ) TO mt_logs.
      MESSAGE 'ATTENZIONE: si sono verificati degli errori. Vedere apposito log per dettaglio' TYPE 'I'.
      mv_ribal_status = ms_status-errore.
    ENDIF.

  ENDMETHOD.                   "fill_ribal_tables

  METHOD post_ribaltamento.

    DATA: lv_rev_ebeln TYPE ebeln,
          lv_imm_ebeln TYPE ebeln,
          lv_error     TYPE abap_bool.

    CLEAR mt_logs.

    APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di creazione OdA ZFTE' ) TO mt_logs.

    execute_po_creation(
       EXPORTING
         iv_tabname = 'MT_FAT_REV'
         it_fatture = mt_fat_rev
       IMPORTING
         ev_ebeln   = lv_rev_ebeln
         ev_error   = lv_error ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    execute_po_creation(
      EXPORTING
        iv_tabname = 'MT_FAT_IMM'
        it_fatture = mt_fat_imm
      IMPORTING
        ev_ebeln   = lv_imm_ebeln
        ev_error   = lv_error ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di Entrate merci' ) TO mt_logs.

    IF mv_test = abap_true.
      check_goods_received(
        EXPORTING
          iv_tabname = 'MT_FAT_REV'
          it_fatture = mt_fat_rev ).

      check_goods_received(
        EXPORTING
          iv_tabname = 'MT_FAT_IMM'
          it_fatture = mt_fat_imm ).
    ELSE.
      post_goods_received(
        EXPORTING
          iv_tabname = 'MT_FAT_REV'
          iv_ebeln   = lv_rev_ebeln
          it_fatture = mt_fat_rev
        IMPORTING
          ev_error   = lv_error ).

      IF lv_error = abap_true.
        RETURN.
      ENDIF.

      post_goods_received(
        EXPORTING
          iv_tabname = 'MT_FAT_IMM'
          iv_ebeln   = lv_imm_ebeln
          it_fatture = mt_fat_imm
        IMPORTING
          ev_error   = lv_error ).

      IF lv_error = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di Fattura attiva' ) TO mt_logs.

    post_active_invoices(
      EXPORTING
        iv_tabname = 'MT_FAT_REV'
        iv_ebeln   = lv_rev_ebeln
        it_fatture = mt_fat_rev
      IMPORTING
        ev_error   = lv_error ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    post_active_invoices(
      EXPORTING
        iv_tabname = 'MT_FAT_IMM'
        iv_ebeln   = lv_imm_ebeln
        it_fatture = mt_fat_imm
      IMPORTING
        ev_error   = lv_error ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di Nota di credito' ) TO mt_logs.

    post_credit_note(
      EXPORTING
        iv_tabname = 'MT_NC_REV'
        it_fatture = mt_nc_rev ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    post_credit_note(
      EXPORTING
        iv_tabname = 'MT_NC_IMM'
        it_fatture = mt_nc_imm ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    APPEND VALUE #( type = 'I' message_v1 = 'Messaggi di Giroconto Costi/Ricavi' ) TO mt_logs.

    post_giroconto(
      EXPORTING
        iv_tabname = 'MT_FAT_REV'
        it_fatture = mt_fat_rev ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    post_giroconto(
      EXPORTING
        iv_tabname = 'MT_FAT_IMM'
        it_fatture = mt_fat_imm ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    post_giroconto(
      EXPORTING
        iv_tabname = 'MT_NC_REV'
        it_fatture = mt_nc_rev ).

    IF lv_error = abap_true AND mv_test  = abap_false.
      RETURN.
    ENDIF.

    post_giroconto(
      EXPORTING
        iv_tabname = 'MT_NC_IMM'
        it_fatture = mt_nc_imm ).

  ENDMETHOD.                    "post_ribaltamento

  METHOD execute_po_creation.

    DATA: lt_return       TYPE STANDARD TABLE OF bapiret2,
          ls_bapieikp     TYPE bapieikp,
          ls_bapipoheader TYPE bapimepoheader,
          lv_error        TYPE abap_bool.

    IF it_fatture IS INITIAL.
      RETURN.
    ENDIF.

    fill_zfte_tables(
    EXPORTING
      it_fatture = it_fatture
    IMPORTING
      ev_error   = lv_error ).

    IF lv_error = abap_true.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_PO_CREATE1'
      EXPORTING
        poheader          = ms_zfte_tables-zfte_poheader
        poheaderx         = ms_zfte_tables-zfte_poheaderx
        testrun           = mv_test
      IMPORTING
        exppurchaseorder  = ev_ebeln
        exppoexpimpheader = ls_bapieikp
        expheader         = ls_bapipoheader
      TABLES
        return            = lt_return
        poitem            = ms_zfte_tables-zfte_poitem
        poitemx           = ms_zfte_tables-zfte_poitemx
        poschedule        = ms_zfte_tables-zfte_poschedule
        poschedulex       = ms_zfte_tables-zfte_poschedulex
        poaccount         = ms_zfte_tables-zfte_poaccount
        poaccountx        = ms_zfte_tables-zfte_poaccountx.

    CASE iv_tabname.
      WHEN 'MT_FAT_IMM'.
        DATA(lv_message) = EXACT symsgv( 'per fatture esigibilità immediata' ).
      WHEN 'MT_FAT_REV'.
        lv_message = 'per fatture reverse charge'.
    ENDCASE.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type IN mr_msg_type.

      update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE OdA ZFTE. Vedi Log di dettaglio.').

      ev_error = abap_true.
      APPEND VALUE #( message_v1 = 'Errore generazione OdA per'
                      message_v2 = lv_message
                      message_v3 = 'di seguito i messaggi di errore.' ) TO mt_logs.
      APPEND LINES OF lt_return TO mt_logs.
      RETURN.
    ENDLOOP.

    IF ls_bapieikp IS INITIAL.
      APPEND VALUE #( message_v1 = 'Errore generazione OdA'
                      message_v2 = lv_message
                      message_v3 = 'di seguito i messaggi di errore.' ) TO mt_logs.
      APPEND LINES OF lt_return TO mt_logs.
      ev_error = abap_true.
    ENDIF.

    IF mv_test = abap_true OR
     ( mv_test = abap_false AND ev_error = abap_true ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE OdA ZFTE. Vedi Log di dettaglio.').
      IF mv_test = abap_true AND ev_error = abap_false.
        APPEND VALUE #( type = 'I' message_v1 = 'OK generazione OdA' message_v2 = lv_message  ) TO mt_logs.
      ENDIF.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    APPEND VALUE #( type       = 'S'
                    message_v1 = 'OK – Creato OdA n.'
                    message_v2 = ev_ebeln
                    message_v3 = lv_message ) TO mt_logs.

  ENDMETHOD.                "execute_po_creation

  METHOD fill_zfte_tables.

    DATA: lv_po_item  TYPE ekpo-ebelp.

    SELECT hkont_terzi,
           matkl
      FROM yfcms_fatt_conto
      INTO TABLE @DATA(lt_fatt_conto)
      ORDER BY hkont_terzi.

    IF sy-subrc <> 0.
      ev_error = abap_true.
      APPEND VALUE #( message_v1 = 'ERRORE:OdA ZFTE: nessun record nella tabella'
                      message_v2 = 'di transcodifica YFCMS_FATT_CONTO' ) TO mt_logs.
      RETURN.
    ENDIF.

    CLEAR ms_zfte_tables.

    ms_zfte_tables-zfte_poheader = VALUE #( comp_code    = p_fondo
                                          doc_type     = p_bsart
                                          vendor       = p_for_i
                                          langu        = sy-langu
                                          langu_iso    = sy-langu
                                          purch_org    = p_ekorg
                                          pur_group    = p_ekgrp
                                          currency     = p_waers
                                          currency_iso = p_waers
                                          doc_date     = p_dt_r ).

    ms_zfte_tables-zfte_poheaderx = VALUE #( comp_code    = abap_true
                                           doc_type     = abap_true
                                           vendor       = abap_true
                                           langu        = abap_true
                                           langu_iso    = abap_true
                                           purch_org    = abap_true
                                           pur_group    = abap_true
                                           currency     = abap_true
                                           currency_iso = abap_true
                                           doc_date     = abap_true ).
    SELECT f~valfrom  AS knttp_terzi,
           t~descript AS knttp_gruppo
      FROM setleaf AS f
      INNER JOIN setlinet AS t
        ON  t~setclass = f~setclass
        AND t~subclass = f~subclass
        AND t~setname  = f~setname
        AND t~langu    = @sy-langu
        AND t~lineid   = f~lineid
      INTO TABLE @DATA(lt_fatt_contab)
      WHERE f~setclass = '0000'
      AND   f~setname  = 'YFCMS_FATT_CONTABILIZZ'
      ORDER BY f~valfrom.

    IF sy-subrc <> 0.
      ev_error = abap_true.
      APPEND VALUE #( message_v1 = 'ERRORE:OdA ZFTE: nessun record nel SET di'
                      message_v2 = 'transcodifica YFCMS_FATT_CONTABILIZZ' ) TO mt_logs.
      RETURN.
    ENDIF.

    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatture>).

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS WITH KEY bukrs = <ls_fatture>-bukrs
                                                                 belnr = <ls_fatture>-belnr
                                                                 gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING FIELD-SYMBOL(<ls_fatt_row>) FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        lv_po_item = lv_po_item + 10.

        CONCATENATE 'N. Doc.' <ls_fatt_row>-belnr 'del' <ls_fatt_row>-gjahr 'posizione' <ls_fatt_row>-buzei
        INTO DATA(lv_short_text) SEPARATED BY space.

        IF <ls_fatt_row>-matkl IS INITIAL.
          READ TABLE lt_fatt_conto ASSIGNING FIELD-SYMBOL(<ls_conto>) WITH KEY hkont_terzi = <ls_fatt_row>-hkont BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_fatt_row>-matkl = <ls_conto>-matkl.
            DATA(lv_knttp) = <ls_conto>-hkont_terzi.
          ELSE.
            APPEND VALUE #( message_v1 = 'ERRORE: transcodifica conto terzi'
                            message_v2 =  <ls_fatt_row>-hkont
                            message_v3 = 'documento'
                            message_v4 =  <ls_fatt_row>-belnr && '-' && <ls_fatt_row>-gjahr && '-' && <ls_fatt_row>-buzei ) TO mt_logs.
          ENDIF.
        ENDIF.

        IF <ls_fatt_row>-matkl IS NOT INITIAL.
          READ TABLE lt_fatt_contab ASSIGNING FIELD-SYMBOL(<ls_contab>) WITH KEY knttp_terzi = <ls_fatt_row>-knttp BINARY SEARCH.
          IF sy-subrc = 0.
            lv_knttp = <ls_contab>-knttp_terzi.
          ELSE.

            APPEND VALUE #( message_v1 = 'ERRORE: transcodifica contabilizzazione Terzi'
                            message_v2 =  <ls_fatt_row>-knttp
                            message_v3 = 'documento'
                            message_v4 =  <ls_fatt_row>-belnr && '-' && <ls_fatt_row>-gjahr && '-' && <ls_fatt_row>-buzei ) TO mt_logs.
          ENDIF.
        ENDIF.


        DATA(lv_bukrs) = EXACT bukrs( <ls_fatt_row>-prctr+2(4) ).
        DATA(lv_swenr) = EXACT rebdbeno( |{ <ls_fatt_row>-prctr+6(4) ALPHA = IN }| ).

        SELECT SINGLE imkey
          FROM vibdbe
          INTO @DATA(lv_imkey)
          WHERE bukrs = @lv_bukrs
          AND   swenr = @lv_swenr.

        IF sy-subrc <> 0.
          APPEND VALUE #( message_v1 = 'ERRORE: imputazione Unità economica in OdA ZFTE'
                          message_v2 = 'documento'
                          message_v3 =  <ls_fatt_row>-belnr && '-' && <ls_fatt_row>-gjahr && '-' && <ls_fatt_row>-buzei ) TO mt_logs.
        ENDIF.

        APPEND VALUE #( po_item        = lv_po_item
                        short_text     = lv_short_text
                        plant          = p_fondo
                        matl_group     = <ls_fatt_row>-matkl
                        vend_mat       = <ls_fatt_row>-bukrs && <ls_fatt_row>-belnr && <ls_fatt_row>-gjahr && <ls_fatt_row>-buzei
                        quantity       = 1
                        po_unit        = p_meins
                        po_unit_iso    = p_meins
                        orderpr_un     = p_meins
                        orderpr_un_iso = p_meins
                        net_price      = <ls_fatt_row>-wrbtr
                        price_unit     = 1
                        tax_code       = <ls_fatt_row>-matkl
                        acctasscat     = lv_knttp
                        ) TO ms_zfte_tables-zfte_poitem.

        APPEND VALUE #( po_item        = lv_po_item
                        po_itemx       = abap_true
                        short_text     = abap_true
                        plant          = abap_true
                        matl_group     = abap_true
                        vend_mat       = abap_true
                        quantity       = abap_true
                        po_unit        = abap_true
                        po_unit_iso    = abap_true
                        orderpr_un     = abap_true
                        orderpr_un_iso = abap_true
                        net_price      = abap_true
                        price_unit     = abap_true
                        tax_code       = abap_true
                        acctasscat     = abap_true
                        ) TO ms_zfte_tables-zfte_poitemx.

        APPEND VALUE #( po_item        = lv_po_item
                        sched_line     = 1
                        delivery_date  = p_dt_r
                        quantity       = 1
                        ) TO ms_zfte_tables-zfte_poschedule.

        APPEND VALUE #( po_item        = lv_po_item
                        sched_line     = 1
                        po_itemx       = abap_true
                        sched_linex    = abap_true
                        delivery_date  = abap_true
                        quantity       = abap_true
                        ) TO ms_zfte_tables-zfte_poschedulex.

        APPEND VALUE #( po_item        = lv_po_item
                        serial_no      = 1
                        quantity       = 1
                        co_area        = p_fondo
                        profit_ctr     = <ls_fatt_row>-prctr
                        rl_est_key      = lv_imkey
                        ) TO ms_zfte_tables-zfte_poaccount.

        APPEND VALUE #( po_item        = lv_po_item
                        serial_no      = 1
                        po_itemx       = abap_true
                        serial_nox     = abap_true
                        quantity       = abap_true
                        co_area        = abap_true
                        profit_ctr     = abap_true
                        rl_est_key     = abap_true
                        ) TO ms_zfte_tables-zfte_poaccountx.

        CLEAR: lv_knttp, lv_bukrs, lv_swenr, lv_imkey.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                  "fill_zfte_tables

  METHOD  check_goods_received.

    DATA: lv_gjahr TYPE gjahr,
          lv_monat TYPE monat,
          lv_poper TYPE poper,
          lv_oper  TYPE frper,
          lv_error TYPE abap_bool.

    CASE iv_tabname.
      WHEN 'MT_FAT_IMM'.
        DATA(lv_message) = EXACT symsgv( 'fatture esigibilità immediata' ).
      WHEN 'MT_FAT_REV'.
        lv_message = 'fatture reverse charge'.
    ENDCASE.

    "Determinare il periodo corrente ===================================================================

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat        = p_dt_r
        i_bukrs        = p_fondo
      IMPORTING
        e_gjahr        = lv_gjahr
        e_monat        = lv_monat
        e_poper        = lv_poper
      EXCEPTIONS
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      APPEND VALUE #( message_v1 = 'Errore Entrate merci per'
                      message_v2 = lv_message ) TO mt_logs.

      APPEND VALUE #( message_v1 = 'Errore generico controllo periodo FI per'
                      message_v2 = lv_message ) TO mt_logs.
      update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Entrate merci. Vedi Log di dettaglio.').
      RETURN.
    ENDIF.

    "Controllo periodo contabile aperto ===================================================================

    CALL FUNCTION 'FI_PERIOD_CHECK'
      EXPORTING
        i_bukrs          = p_fondo
        i_gjahr          = lv_gjahr
        i_koart          = 'S'
        i_monat          = lv_poper
      IMPORTING
        e_oper           = lv_oper
      EXCEPTIONS
        error_period     = 1
        error_period_acc = 2
        invalid_input    = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      lv_error = abap_true.
      APPEND VALUE #( id         = sy-msgid
                      type       = sy-msgty
                      number     = sy-msgno
                      message_v1 = sy-msgv1
                      message_v2 = sy-msgv2
                      message_v3 = sy-msgv3
                      message_v4 = sy-msgv4 ) TO mt_logs.

      APPEND VALUE #( message_v1 = 'Errore Entrate merci per'
                      message_v2 = lv_message ) TO mt_logs.
    ENDIF.

    "Controllo periodo logistico aperto ===================================================================
    CALL FUNCTION 'MR_PERIOD_DETERMINE'
      EXPORTING
        i_bukrs                = p_fondo
        i_budat                = p_dt_r
      EXCEPTIONS
        invalid_posting_period = 1
        marv_no_entry          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      lv_error = abap_true.
      APPEND VALUE #( id         = sy-msgid
                      type       = sy-msgty
                      number     = sy-msgno
                      message_v1 = sy-msgv1
                      message_v2 = sy-msgv2
                      message_v3 = sy-msgv3
                      message_v4 = sy-msgv4 ) TO mt_logs.

      APPEND VALUE #( message_v1 = 'Errore Entrate merci per'
                      message_v2 = lv_message ) TO mt_logs.
    ENDIF.

    "Controllo versione CO parametrizzata ===================================================================
    CALL FUNCTION 'K_VERSN_READ'
      EXPORTING
        i_gjahr          = lv_gjahr
        i_kokrs          = p_fondo
        i_versn          = '0'
        bypassing_buffer = abap_true
      EXCEPTIONS
        not_found        = 1
        not_found_gjahr  = 2
        no_authority     = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      lv_error = abap_true.
      APPEND VALUE #( id         = sy-msgid
                      type       = sy-msgty
                      number     = sy-msgno
                      message_v1 = sy-msgv1
                      message_v2 = sy-msgv2
                      message_v3 = sy-msgv3
                      message_v4 = sy-msgv4 ) TO mt_logs.

      APPEND VALUE #( message_v1 = 'Errore Entrate merci per'
                      message_v2 = lv_message ) TO mt_logs.
    ENDIF.

    IF lv_error = abap_true.
      update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Entrate merci. Vedi Log di dettaglio.').
      RETURN.
    ENDIF.

    APPEND VALUE #( type       = 'I'
                    message_v1 = 'No errore Entrate merci per'
                    message_v2 = lv_message ) TO mt_logs.

  ENDMETHOD.                "check_goods_received

  METHOD post_goods_received.

    DATA: lt_return          TYPE STANDARD TABLE OF bapiret2,
          lv_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year,
          lv_matdocument     TYPE bapi2017_gm_head_ret-mat_doc.

    IF it_fatture IS INITIAL.
      RETURN.
    ENDIF.

    fill_gdrc_tables(
      EXPORTING
        it_fatture = it_fatture
        iv_ebeln   = iv_ebeln ).

    CASE iv_tabname.
      WHEN 'LT_FAT_IMM'.
        DATA(lv_message) = EXACT symsgv( 'per esigibilità immediata' ).
      WHEN 'LT_FAT_REV'.
        lv_message = 'per reverse charge'.
    ENDCASE.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
      EXPORTING
        goodsmvt_header  = ms_gdrc_tables-gdrc_goodsmvt_header
        goodsmvt_code    = '01'
      IMPORTING
        matdocumentyear  = lv_matdocumentyear
        materialdocument = lv_matdocument
      TABLES
        goodsmvt_item    = ms_gdrc_tables-gdrc_goodsmvt_item
        return           = lt_return.

    IF lv_matdocumentyear IS NOT INITIAL AND
       lv_matdocument     IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      APPEND VALUE #( type       = 'S'
                      message_v1 = 'OK – Entrata merci n.'
                      message_v2 = lv_matdocument && lv_matdocumentyear
                      message_v3 = lv_message ) TO mt_logs.
      RETURN.
    ENDIF.

    ev_error = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND VALUE #( message_v1 = 'Errore Entrata merci'
                    message_v2 = lv_message
                    message_v3 = 'di seguito gli errori riscontrati.' ) TO mt_logs.

    APPEND LINES OF lt_return TO mt_logs.

    update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Entrate merci. Vedi Log di dettaglio.').

  ENDMETHOD.                    "post_goods_received

  METHOD fill_gdrc_tables.

    DATA: lv_item    TYPE ebelp.

    ms_gdrc_tables-gdrc_goodsmvt_header = VALUE #( pstng_date = p_dt_r
                                                   doc_date   = p_dt_r
                                                   ref_doc_no = 'OdA' && ' ' && iv_ebeln
                                                   header_txt = 'OdA' && ' ' && iv_ebeln ).
    CLEAR: lv_item, ms_gdrc_tables.

    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatture>).

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS
      WITH KEY bukrs = <ls_fatture>-bukrs
               belnr = <ls_fatture>-belnr
               gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING FIELD-SYMBOL(<ls_fatt_row>) FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        lv_item = lv_item + 10.

        APPEND VALUE #( plant         = p_fondo
                        move_type     = '101'
                        entry_qnt     = '1'
                        entry_uom     = p_meins
                        entry_uom_iso = p_meins
                        po_number     = iv_ebeln
                        po_item       = lv_item
                        mvt_ind       = 'B'
                        item_text     = COND #( WHEN <ls_fatt_row>-txz01 IS NOT INITIAL
                                                THEN <ls_fatt_row>-txz01
                                                ELSE <ls_fatt_row>-sgtxt )
                        ) TO ms_gdrc_tables-gdrc_goodsmvt_item.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.                "fill_gdrc_tables

  METHOD post_active_invoices.

    DATA: lt_return   TYPE STANDARD TABLE OF bapiret2,
          lv_obj_type TYPE  bapiache09-obj_type,
          lv_obj_key  TYPE  bapiache09-obj_key,
          lv_obj_sys  TYPE  bapiache09-obj_sys,
          lv_error    TYPE abap_bool.

    IF it_fatture IS INITIAL.
      RETURN.
    ENDIF.

    fill_fatt_tables(
          EXPORTING
            it_fatture = it_fatture
            iv_ebeln   = iv_ebeln
          IMPORTING
            ev_error   = lv_error ).

    IF lv_error = abap_true.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    CASE iv_tabname.
      WHEN 'LT_FAT_IMM'.
        DATA(lv_message) = EXACT symsgv( 'esigibilità immediata' ).
      WHEN 'LT_FAT_REV'.
        lv_message = 'reverse charge'.
    ENDCASE.

    IF mv_test IS NOT INITIAL.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = ms_fatt_tables-fatt_documentheader
        TABLES
          accountgl         = ms_fatt_tables-fatt_accountgl
          accountreceivable = ms_fatt_tables-fatt_accountreceivable
          accounttax        = ms_fatt_tables-fatt_accounttax
          currencyamount    = ms_fatt_tables-fatt_currencyamount
          return            = lt_return
          extension2        = ms_fatt_tables-fatt_extension2
          realestate        = ms_fatt_tables-fatt_realestate.
    ELSE.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = ms_fatt_tables-fatt_documentheader
        IMPORTING
          obj_type          = lv_obj_type
          obj_key           = lv_obj_key
          obj_sys           = lv_obj_sys
        TABLES
          accountgl         = ms_fatt_tables-fatt_accountgl
          accountreceivable = ms_fatt_tables-fatt_accountreceivable
          accounttax        = ms_fatt_tables-fatt_accounttax
          currencyamount    = ms_fatt_tables-fatt_currencyamount
          return            = lt_return
          extension2        = ms_fatt_tables-fatt_extension2
          realestate        = ms_fatt_tables-fatt_realestate.
    ENDIF.

    IF lt_return IS INITIAL.
      IF mv_test = abap_true.
        APPEND VALUE #( message_v1 = 'ERRORE Generico Check fattura'  message_v2 = lv_message ) TO mt_logs.
        RETURN.
      ENDIF.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 614.
      IF sy-subrc = 0.
        APPEND VALUE #( type = 'I' message_v1 = 'OK Generico Check fattura' message_v2 = lv_message ) TO mt_logs.
        RETURN.
      ENDIF.
    ELSE.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 605.

      IF sy-subrc = 0.

        CONCATENATE lv_obj_key(10) 'del' lv_obj_key+10(4) 'Soc.' lv_obj_key+14(4) INTO DATA(lv_msg3) SEPARATED BY ' '.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        APPEND VALUE #( type       = 'S'
                        message_v1 = 'OK – Registrazione fattura.'
                        message_v2 = lv_message
                        message_v3 = lv_msg3  ) TO mt_logs.
        RETURN.
      ENDIF.
    ENDIF.

    ev_error = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND LINES OF lt_return TO mt_logs.
    APPEND VALUE #( message_v1 = 'ERRORE registrazione fattura'
                    message_v2 = lv_message
                    message_v3 = 'di seguito gli errori riscontrati.' ) TO mt_logs.

    update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Fattura attiva. Vedi Log di dettaglio').

  ENDMETHOD.                    "post_active_invoices

  METHOD  fill_fatt_tables.

    DATA: lt_cod_iva TYPE tt_cod_iva,
          lt_mwdat   TYPE STANDARD TABLE OF rtax1u15,
          lv_gjahr   TYPE gjahr,
          lv_monat   TYPE monat,
          lv_poper   TYPE poper,
          lv_item    TYPE posnr_acc.

    CLEAR ms_fatt_tables.

    SELECT mwskz_acquisti,
           mwskz_vendita
      FROM yfcms_fatt_iva
      INTO TABLE @DATA(lt_fatt_iva)
      ORDER BY mwskz_acquisti.

    IF sy-subrc <> 0.
      APPEND VALUE #( message_v1 = 'ERRORE Fattura attiva: manca la transcodifica codici IVA'
                      message_v2 = '(Tab. YFCMS_FATT_IVA)' ) TO mt_logs.
      LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatt>).
        MODIFY mt_fattura FROM VALUE #( id = icon_led_red mess = 'ERRORE Fattura attiva: manca la transcodifica codici IVA' )
        TRANSPORTING id mess  WHERE bukrs = <ls_fatt>-bukrs
                              AND   belnr = <ls_fatt>-belnr
                              AND   gjahr = <ls_fatt>-gjahr.
      ENDLOOP.
      ev_error = abap_true.
      DATA(lv_exit) = abap_true..
      EXIT.
    ENDIF.

    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatture>).

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS
      WITH KEY bukrs = <ls_fatture>-bukrs
               belnr = <ls_fatture>-belnr
               gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING FIELD-SYMBOL(<ls_fatt_row>) FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        READ TABLE lt_fatt_iva ASSIGNING FIELD-SYMBOL(<ls_fatt_iva>)
        WITH KEY mwskz_acquisti = <ls_fatt_row>-mwskz BINARY SEARCH.
        IF sy-subrc <> 0.
          APPEND VALUE #( message_v1 = |ERRORE:Fattura attiva manca la transcodifica per codice IVA acquisti { <ls_fatt_row>-mwskz }|
                          message_v2 = '(Tab. YFCMS_FATT_IVA)' ) TO mt_logs.
          MODIFY mt_fattura FROM VALUE #( id = icon_led_red mess = 'ERRORE Fattura attiva: manca la transcodifica per codice IVA acquisti' )
          TRANSPORTING id mess  WHERE bukrs = <ls_fatt_row>-bukrs
                                AND   belnr = <ls_fatt_row>-belnr
                                AND   gjahr = <ls_fatt_row>-gjahr.
          ev_error = abap_true.
          lv_exit = abap_true.
          EXIT.
        ENDIF.

        CLEAR: lt_mwdat.

        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = p_integ
            i_mwskz           = <ls_fatt_iva>-mwskz_vendita
            i_waers           = <ls_fatt_row>-waers
            i_wrbtr           = <ls_fatt_row>-wrbtr
          TABLES
            t_mwdat           = lt_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.

        READ TABLE lt_cod_iva ASSIGNING FIELD-SYMBOL(<ls_cod_iva>)
        WITH TABLE KEY mwskz = <ls_fatt_iva>-mwskz_vendita.

        LOOP AT lt_mwdat ASSIGNING FIELD-SYMBOL(<ls_mwdat>).
          IF <ls_cod_iva> IS ASSIGNED.
            <ls_cod_iva>-wrbtr = <ls_cod_iva>-wrbtr + <ls_mwdat>-wmwst.
            <ls_cod_iva>-hwbas = <ls_cod_iva>-hwbas + <ls_mwdat>-kawrt.
            <ls_cod_iva>-hwste = <ls_cod_iva>-hwste + <ls_mwdat>-wmwst.
          ELSE.
            INSERT VALUE #( mwskz = <ls_fatt_row>-mwskz
                            kschl = <ls_mwdat>-kschl
                            ktosl = <ls_mwdat>-ktosl
                            hkont = <ls_mwdat>-hkont
                            wrbtr = <ls_fatt_row>-wrbtr
                            hwbas = <ls_mwdat>-kawrt
                            hwste = <ls_mwdat>-wmwst
                            msatz = <ls_mwdat>-msatz )
                  INTO TABLE lt_cod_iva ASSIGNING <ls_cod_iva>.
          ENDIF.
        ENDLOOP.
        UNASSIGN <ls_cod_iva>.
      ENDLOOP.

      IF lv_exit = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_exit IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE zterm,
                  zwels
      FROM knb1
      INTO (@DATA(lv_zterm), @DATA(lv_zlsch))
      WHERE kunnr = @p_cli_f
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      lv_zterm = 'CACC'.
    ENDIF.

    SELECT SINGLE zdart
      FROM t052
      INTO @DATA(lv_zdart)
      WHERE zterm = @lv_zterm.
    IF sy-subrc = 0 AND lv_zdart IS INITIAL.
      DATA(lv_zfbdt) = EXACT bseg-zfbdt( p_dt_r ).
    ENDIF.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat        = p_dt_r
        i_bukrs        = p_integ
      IMPORTING
        e_gjahr        = lv_gjahr
        e_monat        = lv_monat
        e_poper        = lv_poper
      EXCEPTIONS
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      lv_gjahr = p_dt_r(4).
      lv_monat = p_dt_r+4(2).
      lv_poper = p_dt_r+4(2).
    ENDIF.

    READ TABLE s_dt_reg ASSIGNING FIELD-SYMBOL(<ls_date_row>) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_header_txt) = 'Rib. Da' && <ls_date_row>-low+4(2)  &&
                            'a'       && <ls_date_row>-high+4(2) &&
                            'del'     && <ls_date_row>-low(4).
    ENDIF.

    ms_fatt_tables-fatt_documentheader = VALUE #(
                                 bus_act     = 'RFBU'
                                 username    = sy-uname
                                 header_txt  = lv_header_txt
                                 comp_code   = p_integ
                                 doc_date    = p_dt_r
                                 pstng_date  = p_dt_r
                                 fisc_year   = lv_gjahr
                                 fis_period  = lv_monat
                                 doc_type    = p_td_fat  ).
    UNASSIGN <ls_cod_iva>.

    LOOP AT lt_cod_iva ASSIGNING <ls_cod_iva>.

      lv_item = lv_item + 1.

      APPEND VALUE #( itemno_acc = lv_item
                      customer   = p_cli_f
                      ref_key_1  = sy-uname
                      comp_code  = p_integ
                      pmnttrms   = lv_zterm
                      bline_date = lv_zfbdt
                      pymt_meth  = lv_zlsch
                      item_text  = lv_header_txt
                      tax_code   = <ls_cod_iva>-mwskz
                      ) TO ms_fatt_tables-fatt_accountreceivable.

      APPEND VALUE #( itemno_acc   = lv_item
                      curr_type    = '00'
                      currency     = p_waers
                      currency_iso = p_waers
                      amt_doccur   = <ls_cod_iva>-wrbtr
                      ) TO ms_fatt_tables-fatt_currencyamount.

      APPEND VALUE #( structure  = 'ZZ_RANL'
                      valuepart1 = lv_item
                      valuepart2 = sy-mandt && iv_ebeln && 'Z'
                      ) TO ms_fatt_tables-fatt_extension2.
    ENDLOOP.

    UNASSIGN: <ls_fatture>, <ls_fatt_row>.

    LOOP AT it_fatture ASSIGNING <ls_fatture>.

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS WITH KEY bukrs = <ls_fatture>-bukrs
                                                                 belnr = <ls_fatture>-belnr
                                                                 gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING <ls_fatt_row> FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        lv_item = lv_item + 1.

        IF <ls_fatt_row>-ebeln IS NOT INITIAL.
          CONCATENATE 'OdA' <ls_fatt_row>-ebeln '-' <ls_fatt_row>-ebelp
          INTO DATA(lv_alloc_nr) SEPARATED BY ' '.
        ENDIF.

        READ TABLE lt_fatt_iva INTO DATA(ls_fatt_iva)
        WITH KEY mwskz_acquisti = <ls_fatt_row>-mwskz BINARY SEARCH.

        APPEND VALUE #( itemno_acc = lv_item
                                 gl_account =  p_rec_cs
                                 item_text  = COND #( WHEN <ls_fatt_row>-txz01 IS NOT INITIAL
                                                      THEN <ls_fatt_row>-txz01
                                                      ELSE <ls_fatt_row>-sgtxt )
                                 alloc_nmbr  = lv_alloc_nr
                                 doc_type    = p_td_fat
                                 comp_code   = p_integ
                                 fis_period  = lv_monat
                                 fisc_year   = lv_gjahr
                                 pstng_date  = p_dt_r
                                 ref_key_3   = <ls_fatt_row>-belnr && <ls_fatt_row>-gjahr && <ls_fatt_row>-buzei
                                 tax_code    = ls_fatt_iva-mwskz_vendita
                                 profit_ctr  = <ls_fatt_row>-prctr
                                 ) TO ms_fatt_tables-fatt_accountgl.

        APPEND VALUE #( itemno_acc   = lv_item
                        curr_type    = '00'
                        currency     = p_waers
                        currency_iso = p_waers
                        amt_doccur   = <ls_fatt_row>-wrbtr * -1
                        ) TO ms_fatt_tables-fatt_currencyamount.

        APPEND VALUE #( itemno_acc      = lv_item
                        business_entity = <ls_fatt_row>-prctr+2(8)
                        ) TO ms_fatt_tables-fatt_realestate.

        CLEAR: lv_alloc_nr, ls_fatt_iva.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_cod_iva ASSIGNING <ls_cod_iva>.

      lv_item = lv_item + 1.

      APPEND VALUE #( itemno_acc = lv_item
                      gl_account = <ls_cod_iva>-hkont
                      cond_key   = <ls_cod_iva>-kschl
                      acct_key   = <ls_cod_iva>-ktosl
                      tax_code   = <ls_cod_iva>-mwskz
                      tax_rate   = <ls_cod_iva>-msatz
                      ) TO ms_fatt_tables-fatt_accounttax.

      APPEND VALUE #( itemno_acc   = lv_item
                      curr_type    = '00'
                      currency     = p_waers
                      currency_iso = p_waers
                      amt_base     = <ls_cod_iva>-hwbas * -1
                      tax_amt      = <ls_cod_iva>-hwste * -1
                      ) TO ms_fatt_tables-fatt_currencyamount.
    ENDLOOP.

  ENDMETHOD.              "fill_fatt_tables

  METHOD  post_credit_note.

    DATA: lt_return   TYPE STANDARD TABLE OF bapiret2,
          lv_obj_type TYPE  bapiache09-obj_type,
          lv_obj_key  TYPE  bapiache09-obj_key,
          lv_obj_sys  TYPE  bapiache09-obj_sys,
          lv_buzei    TYPE buzei,
          lt_lines    TYPE STANDARD TABLE OF tline,
          lv_error    TYPE abap_bool.

    IF it_fatture IS INITIAL.
      RETURN.
    ENDIF.

    fill_ntdc_tables(
    EXPORTING
       it_fatture = it_fatture
    IMPORTING
      ev_error = lv_error ).

    IF lv_error = abap_true.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    CASE iv_tabname.
      WHEN 'LT_NC_IMM'.
        DATA(lv_message) = EXACT symsgv( 'esigibilità immediata' ).
      WHEN 'LT_NC_REV'.
        lv_message = 'reverse charge'.
    ENDCASE.

    IF mv_test = abap_true.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader    = ms_ntdc_tables-ntdc_documentheader
        TABLES
          accountgl         = ms_ntdc_tables-ntdc_accountgl
          accountreceivable = ms_ntdc_tables-ntdc_accountreceivable
          accounttax        = ms_ntdc_tables-ntdc_accounttax
          currencyamount    = ms_ntdc_tables-ntdc_currencyamount
          return            = lt_return
          realestate        = ms_ntdc_tables-ntdc_realestate.
    ELSE.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader    = ms_ntdc_tables-ntdc_documentheader
        IMPORTING
          obj_type          = lv_obj_type
          obj_key           = lv_obj_key
          obj_sys           = lv_obj_sys
        TABLES
          accountgl         = ms_ntdc_tables-ntdc_accountgl
          accountreceivable = ms_ntdc_tables-ntdc_accountreceivable
          accounttax        = ms_ntdc_tables-ntdc_accounttax
          currencyamount    = ms_ntdc_tables-ntdc_currencyamount
          return            = lt_return
          realestate        = ms_ntdc_tables-ntdc_realestate.
    ENDIF.

    IF lt_return IS INITIAL.
      IF mv_test = abap_true.
        APPEND VALUE #( message_v1 = 'ERRORE Generico Check Nota di credito' message_v2 = lv_message ) TO mt_logs.
        RETURN.
      ENDIF.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 614.
      IF sy-subrc = 0.
        APPEND VALUE #( type = 'I' message_v1 = 'OK Generico Check Nota di credito' message_v2 = lv_message ) TO mt_logs.
        RETURN.
      ENDIF.
    ELSE.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 605.

      IF sy-subrc = 0.

        CONCATENATE lv_obj_key(10) 'del' lv_obj_key+10(4) 'Soc.' lv_obj_key+14(4) INTO DATA(lv_msg3) SEPARATED BY ' '.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        SORT ms_ntdc_tables-ntdc_appunto BY itemno_acc.

        LOOP AT ms_ntdc_tables-ntdc_accountreceivable ASSIGNING FIELD-SYMBOL(<ls_accrc>).
          READ TABLE ms_ntdc_tables-ntdc_appunto ASSIGNING FIELD-SYMBOL(<ls_appunto>)
          WITH KEY itemno_acc = <ls_accrc>-itemno_acc BINARY SEARCH.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          CLEAR lv_buzei.
          lv_buzei = <ls_accrc>-itemno_acc+7.

          DATA(ls_headertxt) = VALUE thead( tdobject = 'DOC_ITEM'
                                            tdname   = lv_obj_key(10) && lv_obj_key+10(4) && lv_obj_key+14(4) && lv_buzei
                                            tdid     = '0001'
                                            tdspras  = sy-langu ).

          CALL FUNCTION 'SWA_STRING_SPLIT'
            EXPORTING
              input_string                 = <ls_appunto>-text
              max_component_length         = 132
            TABLES
              string_components            = lt_lines
            EXCEPTIONS
              max_component_length_invalid = 1
              OTHERS                       = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              header          = ls_headertxt
              savemode_direct = 'X'
            TABLES
              lines           = lt_lines
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDLOOP.

        APPEND VALUE #( type       = 'S'
                        message_v1 = 'OK – Registrazione nota di credito.'
                        message_v2 = lv_message
                        message_v3 = lv_msg3  ) TO mt_logs.
        RETURN.
      ENDIF.
    ENDIF.

    ev_error = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND LINES OF lt_return TO mt_logs.
    APPEND VALUE #( message_v1 = 'ERRORE registrazione Nota di credito'
                    message_v2 = lv_message
                    message_v3 = 'di seguito gli errori riscontrati.' ) TO mt_logs.

    update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Nota di credito. Vedi Log di dettaglio.').

  ENDMETHOD.                    "post_credit_note

  METHOD  fill_ntdc_tables.

    DATA: lt_cod_iva TYPE tt_cod_iva,
          lt_mwdat   TYPE STANDARD TABLE OF rtax1u15,
          lv_gjahr   TYPE gjahr,
          lv_monat   TYPE monat,
          lv_poper   TYPE poper,
          lv_item    TYPE posnr_acc,
          lv_wmwst   TYPE wmwst.

    CLEAR ms_ntdc_tables.

    SELECT mwskz_acquisti,
           mwskz_vendita
      FROM yfcms_fatt_iva
      INTO TABLE @DATA(lt_fatt_iva)
      ORDER BY mwskz_acquisti.
    IF sy-subrc <> 0.
      APPEND VALUE #( message_v1 = 'ERRORE Nota di credito: manca la transcodifica codici IVA'
                      message_v2 = '(Tab. YFCMS_FATT_IVA)' ) TO mt_logs.
      LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatt>).
        MODIFY mt_fattura FROM VALUE #( id = icon_led_red mess = 'ERRORE Nota di credito: manca la transcodifica codici IVA' )
        TRANSPORTING id mess  WHERE bukrs = <ls_fatt>-bukrs
                              AND   belnr = <ls_fatt>-belnr
                              AND   gjahr = <ls_fatt>-gjahr.
      ENDLOOP.
      ev_error = abap_true.
      DATA(lv_exit) = abap_true.
      EXIT.
    ENDIF.

    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatture>).

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS
      WITH KEY bukrs = <ls_fatture>-bukrs
               belnr = <ls_fatture>-belnr
               gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING FIELD-SYMBOL(<ls_fatt_row>) FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        READ TABLE lt_fatt_iva ASSIGNING FIELD-SYMBOL(<ls_fatt_iva>)
        WITH KEY mwskz_acquisti = <ls_fatt_row>-mwskz BINARY SEARCH.
        IF sy-subrc <> 0.
          APPEND VALUE #( message_v1 = |ERRORE Nota di credito: manca la transcodifica per codice IVA acquisti { <ls_fatt_row>-mwskz }|
                          message_v2 = '(Tab. YFCMS_FATT_IVA)' ) TO mt_logs.
          MODIFY mt_fattura FROM VALUE #( id = icon_led_red mess = 'ERRORE Nota di credito: manca la transcodifica per codice IVA acquisti' )
          TRANSPORTING id mess  WHERE bukrs = <ls_fatt_row>-bukrs
                                AND   belnr = <ls_fatt_row>-belnr
                                AND   gjahr = <ls_fatt_row>-gjahr.
          ev_error = abap_true.
          lv_exit = abap_true.
          EXIT.
        ENDIF.

        CLEAR: lt_mwdat.

        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = p_integ
            i_mwskz           = <ls_fatt_iva>-mwskz_vendita
            i_waers           = <ls_fatt_row>-waers
            i_wrbtr           = <ls_fatt_row>-wrbtr
          TABLES
            t_mwdat           = lt_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.

        READ TABLE lt_cod_iva ASSIGNING FIELD-SYMBOL(<ls_cod_iva>)
         WITH TABLE KEY mwskz = <ls_fatt_iva>-mwskz_vendita.

        LOOP AT lt_mwdat ASSIGNING FIELD-SYMBOL(<ls_mwdat>).
          IF <ls_cod_iva> IS ASSIGNED.
            READ TABLE lt_cod_iva ASSIGNING <ls_cod_iva>
            WITH KEY mwskz = <ls_fatt_iva>-mwskz_vendita kschl = <ls_mwdat>-kschl ktosl = <ls_mwdat>-ktosl.
            IF sy-subrc = 0.
              <ls_cod_iva>-wrbtr = <ls_cod_iva>-wrbtr + <ls_mwdat>-wmwst.
              <ls_cod_iva>-hwbas = <ls_cod_iva>-hwbas + <ls_mwdat>-kawrt.
              <ls_cod_iva>-hwste = <ls_cod_iva>-hwste + <ls_mwdat>-wmwst.
            ENDIF.
          ELSE.
            INSERT VALUE #( mwskz = <ls_fatt_row>-mwskz
                            kschl = <ls_mwdat>-kschl
                            ktosl = <ls_mwdat>-ktosl
                            hkont = <ls_mwdat>-hkont
                            wrbtr = <ls_fatt_row>-wrbtr
                            hwbas = <ls_mwdat>-kawrt
                            hwste = <ls_mwdat>-wmwst
                            msatz = <ls_mwdat>-msatz )
                  INTO TABLE lt_cod_iva ASSIGNING <ls_cod_iva>.
          ENDIF.
        ENDLOOP.
        DATA(lv_iva_mwskz) = <ls_cod_iva>-mwskz.
        UNASSIGN <ls_cod_iva>.
      ENDLOOP.

      IF lv_exit = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_exit IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE zterm,
                  zwels
      FROM knb1
      INTO (@DATA(lv_zterm), @DATA(lv_zlsch))
      WHERE kunnr = @p_cli_f
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      lv_zterm = 'CACC'.
    ENDIF.

    SELECT SINGLE zdart
      FROM t052
      INTO @DATA(lv_zdart)
      WHERE zterm = @lv_zterm.
    IF sy-subrc = 0 AND lv_zdart IS INITIAL.
      DATA(lv_zfbdt) = EXACT bseg-zfbdt( p_dt_r ).
    ENDIF.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat        = p_dt_r
        i_bukrs        = p_integ
      IMPORTING
        e_gjahr        = lv_gjahr
        e_monat        = lv_monat
        e_poper        = lv_poper
      EXCEPTIONS
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      lv_gjahr = p_dt_r(4).
      lv_monat = p_dt_r+4(2).
      lv_poper = p_dt_r+4(2).
    ENDIF.

    READ TABLE s_dt_reg ASSIGNING FIELD-SYMBOL(<ls_date_row>) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_header_txt) = 'Rib. Da' && <ls_date_row>-low+4(2)  &&
                            'a'       && <ls_date_row>-high+4(2) &&
                            'del'     && <ls_date_row>-low(4).
    ENDIF.

    ms_ntdc_tables-ntdc_documentheader = VALUE #(
                                 bus_act     = 'RFBU'
                                 username    = sy-uname
                                 header_txt  = lv_header_txt
                                 comp_code   = p_integ
                                 doc_date    = p_dt_r
                                 pstng_date  = p_dt_r
                                 fisc_year   = lv_gjahr
                                 fis_period  = lv_monat
                                 doc_type    = p_td_nc  ).

    LOOP AT it_fatture ASSIGNING <ls_fatture>.

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS
      WITH KEY bukrs = <ls_fatture>-bukrs
               belnr = <ls_fatture>-belnr
               gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING <ls_fatt_row> FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        lv_item = lv_item + 1.

        READ TABLE lt_cod_iva ASSIGNING <ls_cod_iva>
         WITH TABLE KEY mwskz = <ls_fatt_iva>-mwskz_vendita.

        APPEND VALUE #( itemno_acc = lv_item
                        customer   =  p_cli_f
                        ref_key_1  = sy-uname
                        comp_code  = p_integ
                        pmnttrms   = lv_zterm
                        bline_date = lv_zfbdt
                        pymt_meth  = lv_zlsch
                        item_text  = COND #( WHEN <ls_fatt_row>-txz01 IS NOT INITIAL
                                             THEN <ls_fatt_row>-txz01
                                             ELSE <ls_fatt_row>-sgtxt )
                        tax_code    = lv_iva_mwskz
                        ) TO ms_ntdc_tables-ntdc_accountreceivable.

        CLEAR: lt_mwdat, lv_iva_mwskz.

        CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
          EXPORTING
            i_bukrs           = p_integ
            i_mwskz           = <ls_fatt_iva>-mwskz_vendita
            i_waers           = <ls_fatt_row>-waers
            i_wrbtr           = <ls_fatt_row>-wrbtr
          TABLES
            t_mwdat           = lt_mwdat
          EXCEPTIONS
            bukrs_not_found   = 1
            country_not_found = 2
            mwskz_not_defined = 3
            mwskz_not_valid   = 4
            ktosl_not_found   = 5
            kalsm_not_found   = 6
            parameter_error   = 7
            knumh_not_found   = 8
            kschl_not_found   = 9
            unknown_error     = 10
            account_not_found = 11
            txjcd_not_valid   = 12
            OTHERS            = 13.

        LOOP AT lt_mwdat ASSIGNING <ls_mwdat>.
          lv_wmwst = lv_wmwst + <ls_mwdat>-wmwst.
        ENDLOOP.

        APPEND VALUE #( itemno_acc   = lv_item
                        curr_type    = '00'
                        currency     = p_waers
                        currency_iso = p_waers
                        amt_doccur   = <ls_fatt_row>-wrbtr + lv_wmwst
                        ) TO ms_ntdc_tables-ntdc_currencyamount.

        DATA(lv_date) = <ls_fatt_row>-budat+6(2) && '.' && <ls_fatt_row>-budat+4(2) && '.' && <ls_fatt_row>-budat(4).
        CONCATENATE 'Documento n.' <ls_fatt_row>-belnr 'del' lv_date 'riga' <ls_fatt_row>-buzei
                    '- CoGe Costo' <ls_fatt_row>-hkont '-'   <ls_fatt_row>-txt50 '- Unita economica' <ls_fatt_row>-prctr+2(8)
        INTO DATA(lv_text) SEPARATED BY space.

        APPEND VALUE #( itemno_acc = lv_item
                        text       = lv_text
                        ) TO ms_ntdc_tables-ntdc_appunto.

        READ TABLE lt_fatt_iva INTO DATA(ls_fatt_iva)
        WITH KEY mwskz_acquisti = <ls_fatt_row>-mwskz BINARY SEARCH.

        IF <ls_fatt_row>-ebeln IS NOT INITIAL.
          CONCATENATE 'OdA' <ls_fatt_row>-ebeln '-' <ls_fatt_row>-ebelp
          INTO DATA(lv_alloc_nr) SEPARATED BY ' '.
        ENDIF.

        APPEND VALUE #( itemno_acc = lv_item
                                 gl_account =  p_rec_cs
                                 item_text  = COND #( WHEN <ls_fatt_row>-txz01 IS NOT INITIAL
                                                      THEN <ls_fatt_row>-txz01
                                                      ELSE <ls_fatt_row>-sgtxt )
                                 alloc_nmbr  = lv_alloc_nr
                                 doc_type    = p_td_fat
                                 comp_code   = p_integ
                                 fis_period  = lv_monat
                                 fisc_year   = lv_gjahr
                                 pstng_date  = p_dt_r
                                 ref_key_3   = <ls_fatt_row>-belnr && <ls_fatt_row>-gjahr && <ls_fatt_row>-buzei
                                 tax_code    = ls_fatt_iva-mwskz_vendita
                                 profit_ctr  = <ls_fatt_row>-prctr
                                 ) TO ms_ntdc_tables-ntdc_accountgl.

        APPEND VALUE #( itemno_acc   = lv_item
                                 curr_type    = '00'
                                 currency     = p_waers
                                 currency_iso = p_waers
                                 amt_doccur   = <ls_fatt_row>-wrbtr * -1
                                 ) TO ms_ntdc_tables-ntdc_currencyamount.

        APPEND VALUE #( itemno_acc      = lv_item
                                 business_entity = <ls_fatt_row>-prctr+2(8)
                                 ) TO ms_ntdc_tables-ntdc_realestate.

        CLEAR: lv_alloc_nr, ls_fatt_iva, lv_text, lv_date.
      ENDLOOP.
    ENDLOOP.

    LOOP AT lt_cod_iva ASSIGNING <ls_cod_iva>.

      lv_item = lv_item + 1.

      APPEND VALUE #( itemno_acc = lv_item
                      gl_account = <ls_cod_iva>-hkont
                      cond_key   = <ls_cod_iva>-kschl
                      acct_key   = <ls_cod_iva>-ktosl
                      tax_code   = <ls_cod_iva>-mwskz
                      tax_rate   = <ls_cod_iva>-msatz
                      ) TO ms_ntdc_tables-ntdc_accounttax.

      APPEND VALUE #( itemno_acc   = lv_item
                      curr_type    = '00'
                      currency     = p_waers
                      currency_iso = p_waers
                      amt_base     = <ls_cod_iva>-hwbas * -1
                      tax_amt      = <ls_cod_iva>-hwste * -1
                      ) TO ms_ntdc_tables-ntdc_currencyamount.
    ENDLOOP.
  ENDMETHOD.            "fill_ntdc_tables

  METHOD post_giroconto.

    DATA: lt_return   TYPE STANDARD TABLE OF bapiret2,
          lv_obj_type TYPE  bapiache09-obj_type,
          lv_obj_key  TYPE  bapiache09-obj_key,
          lv_obj_sys  TYPE  bapiache09-obj_sys.

    CASE iv_tabname.
      WHEN 'MT_FAT_IMM'.
        DATA(lv_message) = EXACT symsgv( 'Fatture esigibilità immediata n.' ).
      WHEN 'MT_FAT_REV'.
        lv_message = 'Fatture reverse charge n.'.
      WHEN 'MT_NC_REV'.
        lv_message = 'Note di credito esigibilità immediata n.'.
      WHEN 'MT_NC_IMM'.
        lv_message = 'Note di credito reverse charge n.'.
    ENDCASE.

    IF it_fatture IS INITIAL.
      RETURN.
    ENDIF.

    fill_gccr_tables( it_fatture ).

    IF mv_test IS NOT INITIAL.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
        EXPORTING
          documentheader = ms_gccr_tables-gccr_documentheader
        TABLES
          accountgl      = ms_gccr_tables-gccr_accountgl
          currencyamount = ms_gccr_tables-gccr_currencyamount
          return         = lt_return.
    ELSE.
      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ms_gccr_tables-gccr_documentheader
        IMPORTING
          obj_type       = lv_obj_type
          obj_key        = lv_obj_key
          obj_sys        = lv_obj_sys
        TABLES
          accountgl      = ms_gccr_tables-gccr_accountgl
          currencyamount = ms_gccr_tables-gccr_currencyamount
          return         = lt_return.
    ENDIF.

    IF mv_test IS NOT INITIAL.
      IF lt_return IS INITIAL.
        APPEND VALUE #( message_v1 = 'ERRORE Generico Check Giroconto' message_v2 = 'costi/ricavi' ) TO mt_logs.
        update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Giroconto. Vedi Log di dettaglio.').
        RETURN.
      ENDIF.
      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 614.
      IF sy-subrc = 0.
        SPLIT lv_message AT ' n.' INTO DATA(lv_okmsg) DATA(str2).
        APPEND VALUE #( type = 'I' message_v1 = 'OK Generico Check Giroconto' message_v2 = lv_okmsg ) TO mt_logs.
        RETURN.
      ENDIF.
    ELSE.

      READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'S' id = 'RW' number = 605.
      IF sy-subrc = 0.

        CONCATENATE lv_obj_key(10) 'del' lv_obj_key+10(4) 'Soc.' lv_obj_key+14(4) INTO DATA(lv_msg3) SEPARATED BY ' '.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        APPEND VALUE #( type       = 'S'
                        message_v1 = 'OK – Registrazione Giroconto.'
                        message_v2 = lv_message
                        message_v3 = lv_msg3  ) TO mt_logs.
        RETURN.
      ENDIF.
    ENDIF.

    ev_error = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND LINES OF lt_return TO mt_logs.
    APPEND VALUE #( message_v1 = 'ERRORE registrazione Giroconto'
                    message_v2 = 'costi/ricavi'
                    message_v3 = 'di seguito gli errori riscontrati.' ) TO mt_logs.

    update_record_status( EXPORTING it_fatture = it_fatture iv_msg = 'ERRORE Giroconto. Vedi Log di dettaglio.').

  ENDMETHOD.                    "post_giroconto

  METHOD  fill_gccr_tables.

    DATA: lv_gjahr   TYPE gjahr,
          lv_monat   TYPE monat,
          lv_poper   TYPE poper,
          lv_total   TYPE bseg-wrbtr,
          lv_po_item TYPE posnr_acc.

    CLEAR ms_gccr_tables.

    CALL FUNCTION 'FI_PERIOD_DETERMINE'
      EXPORTING
        i_budat        = p_dt_r
        i_bukrs        = p_integ
      IMPORTING
        e_gjahr        = lv_gjahr
        e_monat        = lv_monat
        e_poper        = lv_poper
      EXCEPTIONS
        fiscal_year    = 1
        period         = 2
        period_version = 3
        posting_period = 4
        special_period = 5
        version        = 6
        posting_date   = 7
        OTHERS         = 8.

    IF sy-subrc <> 0.
      lv_gjahr = p_dt_r.
      lv_monat = p_dt_r.
      lv_poper = p_dt_r.
    ENDIF.

    READ TABLE s_dt_reg ASSIGNING FIELD-SYMBOL(<ls_date_row>) INDEX 1.
    IF sy-subrc = 0.
      DATA(lv_header_txt) = 'Rib. Da' && <ls_date_row>-low+4(2)  &&
                            'a'       && <ls_date_row>-high+4(2) &&
                            'del'     && <ls_date_row>-low(4).
    ENDIF.

    ms_gccr_tables-gccr_documentheader = VALUE #(
                                 bus_act     = 'RFBU'
                                 username    = sy-uname
                                 header_txt  = lv_header_txt
                                 ref_doc_no  = p_rif_gc
                                 comp_code   = p_integ
                                 doc_date    = p_dt_r
                                 pstng_date  = p_dt_r
                                 fisc_year   = lv_gjahr
                                 fis_period  = lv_monat
                                 doc_type    = p_td_gir  ).

    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatture>).

      READ TABLE mt_fattura_copy TRANSPORTING NO FIELDS
      WITH KEY bukrs = <ls_fatture>-bukrs
               belnr = <ls_fatture>-belnr
               gjahr = <ls_fatture>-gjahr BINARY SEARCH.

      LOOP AT mt_fattura_copy ASSIGNING FIELD-SYMBOL(<ls_fatt_row>) FROM sy-tabix.

        IF <ls_fatt_row>-bukrs <> <ls_fatture>-bukrs OR
           <ls_fatt_row>-belnr <> <ls_fatture>-belnr OR
           <ls_fatt_row>-gjahr <> <ls_fatture>-gjahr.
          EXIT.
        ENDIF.

        lv_po_item = lv_po_item + 1.

        IF <ls_fatt_row>-ebeln IS NOT INITIAL.
          CONCATENATE 'G/c Ordine' <ls_fatt_row>-ebeln 'posizione' <ls_fatt_row>-ebelp
          INTO DATA(lv_short_text) SEPARATED BY space.
        ENDIF.

        APPEND VALUE #( itemno_acc   = lv_po_item
                        gl_account   = <ls_fatt_row>-hkont
                        item_text    = lv_short_text
                        doc_type     = p_td_gir
                        comp_code    = p_integ
                        fis_period   = lv_monat
                        fisc_year    = lv_gjahr
                        pstng_date = p_dt_r
                        alloc_nmbr   = <ls_fatt_row>-belnr && <ls_fatt_row>-gjahr && <ls_fatt_row>-buzei
                        costcenter   = p_cdc
                        trade_id     = <ls_fatt_row>-vbund
                        ) TO ms_gccr_tables-gccr_accountgl.

        APPEND VALUE #(  itemno_acc = lv_po_item
                         curr_type  = '00'
                         currency   = p_waers
                         currency_iso = p_waers
                         amt_doccur   = <ls_fatt_row>-wrbtr * -1
                         ) TO ms_gccr_tables-gccr_currencyamount.

        lv_total = lv_total + ( <ls_fatt_row>-wrbtr * -1 ).

      ENDLOOP.
    ENDLOOP.

    SELECT SINGLE rcomp
      FROM t001
      INTO @DATA(lv_rcomp)
      WHERE bukrs = @p_fondo.

    lv_po_item = lv_po_item + 1.

    APPEND VALUE #( itemno_acc   = lv_po_item
                    gl_account   = <ls_fatt_row>-hkont
                    item_text    = lv_short_text
                    doc_type     = p_td_gir
                    comp_code    = p_integ
                    fis_period   = lv_monat
                    fisc_year    = lv_gjahr
                    pstng_date   = p_dt_r
                    alloc_nmbr   = <ls_fatt_row>-belnr && <ls_fatt_row>-gjahr && <ls_fatt_row>-buzei
                    costcenter   = p_cdc
                    trade_id     = lv_rcomp
                    ) TO ms_gccr_tables-gccr_accountgl.

    APPEND VALUE #(  itemno_acc = lv_po_item
                     curr_type  = '00'
                     currency   = p_waers
                     currency_iso = p_waers
                     amt_doccur   = lv_total * -1
                     ) TO ms_gccr_tables-gccr_currencyamount.

  ENDMETHOD.                 "fill_gccr_tables

  METHOD display_logs.

    message_init_show( 'INIT' ).

    LOOP AT mt_logs ASSIGNING FIELD-SYMBOL(<ls_logs>).
      message_store(
        EXPORTING
          iv_arbgb  = <ls_logs>-id
          iv_msgty  = <ls_logs>-type
          iv_msg1   = <ls_logs>-message_v1
          iv_msg2   = <ls_logs>-message_v2
          iv_msg3   = <ls_logs>-message_v3
          iv_msg4   = <ls_logs>-message_v4
          iv_txtnr  = <ls_logs>-number
          iv_lineno = sy-tabix
      ).
    ENDLOOP.

    message_init_show( 'SHOW' ).

  ENDMETHOD.                    "display_logs

  METHOD enrich_data.

    CONSTANTS: lc_awtyp TYPE awtyp  VALUE 'RMRP',
               lc_land1 TYPE land1  VALUE 'IT',
               lc_datbi TYPE datbi  VALUE '99991231',
               lc_koart TYPE koart  VALUE 'K'.

    DATA: lr_blart TYPE RANGE OF bkpf-blart.

    lr_blart = VALUE #( ( sign = 'I' option = 'EQ' low = p_td_fat )
                        ( sign = 'I' option = 'EQ' low = p_td_nc  ) ).

    mr_msg_type = VALUE #( sign = 'I' option = 'EQ'
                         ( low  = 'E' )
                         ( low  = 'A' )
                         ( low  = 'X' ) ).
    SELECT *
      FROM cepct
      INTO TABLE @DATA(lt_cepct)
      WHERE spras = @sy-langu
      AND   datbi = @lc_datbi
      AND   kokrs = @p_integ.

    SELECT SINGLE kalsm
      FROM t005
      INTO @DATA(lv_kalsm)
      WHERE land1 = @lc_land1.

    SELECT *
      FROM t007s
      INTO TABLE @DATA(lt_t007s)
      WHERE spras = @sy-langu
      AND kalsm = @lv_kalsm.

    SELECT bukrs,
           belnr,
           gjahr,
           shkzg,
           lifnr
      FROM bseg
      INTO TABLE @DATA(lt_bseg_for)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE bukrs = @mt_fattura-bukrs
      AND   belnr = @mt_fattura-belnr
      AND   gjahr = @mt_fattura-gjahr
      AND   koart = @lc_koart.

    SELECT bukrs,
           belnr,
           gjahr,
           buzei,
           prctr
      FROM bseg
      INTO TABLE @DATA(lt_all_bseg)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE bukrs = @mt_fattura-bukrs
      AND   belnr = @mt_fattura-belnr
      AND   gjahr = @mt_fattura-gjahr
      AND   buzei = @mt_fattura-buzei.

    SELECT prctr
      FROM cepc
      INTO TABLE @DATA(lt_cepc)
      FOR ALL ENTRIES IN @mt_fattura
      WHERE prctr IN @ir_prctr
      AND   prctr =  @mt_fattura-prctr.
    SORT lt_cepc BY prctr.

    SELECT lifnr,
           name1
      FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      FOR ALL ENTRIES IN @lt_bseg_for
      WHERE lifnr = @lt_bseg_for-lifnr.
    SORT lt_lfa1 BY lifnr.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>).

      IF <ls_fattura>-awtyp = lc_awtyp AND <ls_fattura>-prctr IS INITIAL.
        READ TABLE lt_all_bseg ASSIGNING FIELD-SYMBOL(<ls_all_bseg>) WITH KEY bukrs = <ls_fattura>-bukrs
                                                                              belnr = <ls_fattura>-belnr
                                                                              gjahr = <ls_fattura>-gjahr
                                                                              buzei = <ls_fattura>-buzei.
        IF sy-subrc <> 0.
          <ls_fattura>-blocked = abap_true.
          CONTINUE.
        ENDIF.
        <ls_fattura>-prctr = <ls_all_bseg>-prctr.
      ENDIF.

      IF <ls_fattura>-awtyp = lc_awtyp AND <ls_fattura>-prctr IS NOT INITIAL.
        READ TABLE lt_cepc TRANSPORTING NO FIELDS WITH KEY prctr = <ls_fattura>-prctr BINARY SEARCH.
        IF sy-subrc <> 0.
          <ls_fattura>-blocked = abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE lt_bseg_for ASSIGNING FIELD-SYMBOL(<ls_bseg_for>) WITH KEY bukrs = <ls_fattura>-bukrs
                                                                            belnr = <ls_fattura>-belnr
                                                                            gjahr = <ls_fattura>-gjahr.
      IF sy-subrc = 0.
        IF <ls_bseg_for>-shkzg = 'H'.
          <ls_fattura>-tipo = mc_fatt.
        ELSE.
          <ls_fattura>-tipo = mc_ndc.
        ENDIF.

        <ls_fattura>-lifnr = <ls_bseg_for>-lifnr.

        READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>) WITH KEY lifnr = <ls_bseg_for>-lifnr BINARY SEARCH.
        IF sy-subrc = 0.
          <ls_fattura>-rag_soc = <ls_lfa1>-name1.
        ENDIF.
      ENDIF.

      READ TABLE lt_t007s ASSIGNING FIELD-SYMBOL(<ls_t007s>) WITH KEY mwskz = <ls_fattura>-mwskz.
      IF sy-subrc = 0.
        <ls_fattura>-text1 = <ls_t007s>-text1.
      ENDIF.

      CONCATENATE <ls_fattura>-belnr <ls_fattura>-gjahr <ls_fattura>-buzei INTO DATA(lv_xref3) SEPARATED BY ''.
      CONDENSE lv_xref3.

      READ TABLE lt_cepct ASSIGNING FIELD-SYMBOL(<ls_cepc>) WITH KEY prctr = <ls_fattura>-prctr.
      IF sy-subrc = 0.
        <ls_fattura>-ltext = <ls_cepc>-ltext.
      ENDIF.

      SELECT SINGLE bkpf~belnr, bkpf~gjahr
        FROM bseg
        INNER JOIN bkpf
        ON  bkpf~bukrs = bseg~bukrs
        AND bkpf~belnr = bseg~belnr
        AND bkpf~gjahr = bseg~gjahr
        INTO @DATA(ls_exist)
        WHERE bkpf~bukrs = @<ls_fattura>-bukrs
        AND   bkpf~blart IN @lr_blart
        AND   bkpf~stblg = ''
        AND   bseg~xref3 = @lv_xref3.

      IF sy-subrc = 0.
        <ls_fattura>-id = icon_led_red.
        <ls_fattura>-mess = 'ERRORE: Il documento risulta ribaltato nella fattura attiva n. ' && ls_exist-belnr && '-' && ls_exist-gjahr.
      ELSE.
        IF <ls_fattura>-xref2_hd IS NOT INITIAL.
          <ls_fattura>-id = icon_interval_exclude_red.
          <ls_fattura>-mess = 'ATTENZIONE: Il documento risulta bloccato per il ribaltamento'.
        ELSE.
          <ls_fattura>-id = icon_led_green.
          <ls_fattura>-mess = 'OK – Documento ribaltabile'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DELETE mt_fattura WHERE blocked = abap_true.
  ENDMETHOD.                    "enrich_data

  METHOD extract_data.

    DATA(lr_prctr) = VALUE tr_prctr( ( sign = 'I' option = 'CP' low = 'EO' && p_fondo && '*' ) ).

    SELECT b~bukrs,
           b~belnr,
           b~gjahr,
           s~buzei,
           b~budat,
           b~bldat,
           b~xblnr,
           s~ebeln,
           s~ebelp,
           s~hkont,
           t~txt50,
           s~prctr,
           CASE s~shkzg
             WHEN 'H' THEN s~wrbtr * -1
             WHEN 'S' THEN s~wrbtr END AS wrbtr,
           b~waers,
           s~mwskz,
           b~xref2_hd,
           b~awkey,
           b~awtyp,
           s~vbund,
           s~sgtxt

        FROM bkpf AS b

        INNER JOIN bseg AS s
             ON  s~bukrs = b~bukrs
             AND s~belnr = b~belnr
             AND s~gjahr = b~gjahr

        LEFT OUTER JOIN skat AS t
             ON  t~saknr = s~hkont
             AND t~ktopl = 'STD'
             AND t~spras = @sy-langu

             WHERE b~bukrs = @p_integ
              AND  b~blart IN @s_ft_int
              AND  b~budat IN @s_dt_reg
              AND  s~koart = 'S'
              ##UNCOMMENT
*              AND  s~prctr IN @lr_prctr
              AND  s~buzid <> 'T'
              AND  b~awkey <> 'RMRP'
             INTO CORRESPONDING FIELDS OF TABLE @mt_fattura.

    SELECT b~bukrs,
           b~belnr,
           b~gjahr,
           s~buzei,
           b~budat,
           b~bldat,
           b~xblnr,
           s~ebeln,
           s~ebelp,
           s~hkont,
           t~txt50,
           s~prctr,
           CASE s~shkzg
             WHEN 'H' THEN s~wrbtr * -1
             WHEN 'S' THEN s~wrbtr END AS wrbtr,
           b~waers,
           s~mwskz,
           b~xref2_hd,
           b~awkey,
           b~awtyp,
           s~vbund,
           s~sgtxt,
           p~matkl,
           p~knttp,
           p~txz01
        FROM bkpf AS b

        INNER JOIN bseg AS s
             ON  s~bukrs = b~bukrs
             AND s~belnr = b~belnr
             AND s~gjahr = b~gjahr

        LEFT OUTER JOIN skat AS t
             ON  t~saknr = s~hkont
             AND t~ktopl = 'STD'
             AND t~spras = @sy-langu

        LEFT OUTER JOIN ekkn AS n
             ON  n~ebeln = s~ebeln
             AND n~ebelp = s~ebelp
             AND n~zekkn = 1

        LEFT OUTER JOIN ekpo AS p
             ON  p~ebeln = s~ebeln
             AND p~ebelp = s~ebelp

             WHERE b~bukrs =  @p_integ
              AND  b~blart IN @s_ft_int
              AND  b~budat IN @s_dt_reg
              AND  s~koart = 'S'
              ##UNCOMMENT
*              AND  s~prctr IN @lr_prctr
              AND  s~buzid <> 'T'
             AND   b~awkey = 'RMRP'
             AND   n~prctr IN @lr_prctr

        APPENDING CORRESPONDING FIELDS OF TABLE @mt_fattura.

    IF mt_fattura IS INITIAL.
      RETURN.
    ENDIF.

    enrich_data( EXPORTING ir_prctr = lr_prctr ).

  ENDMETHOD.                    "extract_data

  METHOD update_record_status.
    LOOP AT it_fatture ASSIGNING FIELD-SYMBOL(<ls_fatt>).
      MODIFY mt_fattura FROM VALUE #( id = icon_led_red mess = iv_msg )
      TRANSPORTING id mess  WHERE bukrs = <ls_fatt>-bukrs
                            AND   belnr = <ls_fatt>-belnr
                            AND   gjahr = <ls_fatt>-gjahr.
    ENDLOOP.
  ENDMETHOD.                    "update_record_status

  METHOD display_invoice.

    DATA lt_fcat TYPE lvc_t_fcat.

    extract_data( ).

    DATA(ls_layout)  = VALUE lvc_s_layo( sel_mode    = 'A' no_rowmark  = ''  edit = 0 cwidth_opt = abap_true ).
    DATA(ls_variant) = VALUE disvariant( report = sy-repid  username = sy-uname ).

    SET HANDLER : handle_toolbar
                  top_of_page
                  handle_user_command FOR mo_grid.

    DATA(lt_toolbar_excluding) = VALUE ui_functions(
    ( cl_gui_alv_grid=>mc_fc_loc_cut            ) ( cl_gui_alv_grid=>mc_fc_loc_append_row     )
    ( cl_gui_alv_grid=>mc_fc_loc_insert_row     ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row     )
    ( cl_gui_alv_grid=>mc_fc_loc_copy           ) ( cl_gui_alv_grid=>mc_fc_loc_undo           )
    ( cl_gui_alv_grid=>mc_fc_loc_copy_row       ) ( cl_gui_alv_grid=>mc_fc_loc_paste          )
    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row  ) ( cl_gui_alv_grid=>mc_fc_info               )
    ( cl_gui_alv_grid=>mc_fc_check              ) ( cl_gui_alv_grid=>mc_fc_refresh            )
    ( cl_gui_alv_grid=>mc_fc_graph              ) ).

    set_fcat( IMPORTING ev_fieldcat = lt_fcat ) .

    mo_grid->set_table_for_first_display(
       EXPORTING
         i_save               = 'X'
         is_layout            = ls_layout
         is_variant           = ls_variant
         it_toolbar_excluding = lt_toolbar_excluding
       CHANGING
         it_fieldcatalog    = lt_fcat
         it_outtab          = mt_fattura ).

    mo_top->initialize_document( ).

    mo_grid->list_processing_events(
     EXPORTING
       i_event_name = 'TOP_OF_PAGE'
       i_dyndoc_id  = mo_top ).

  ENDMETHOD.                    "display_invoice

  METHOD start_container.

    DATA: lo_cont      TYPE REF TO cl_gui_custom_container,
          lo_split_col TYPE REF TO cl_gui_splitter_container.

    IF mo_grid IS BOUND.
      RETURN.
    ENDIF.

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

    CREATE OBJECT lo_split_col
      EXPORTING
        parent            = lo_cont
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.

    lo_split_col->set_row_sash( id    = 1
                                type  = cl_gui_splitter_container=>type_sashvisible
                                value = cl_gui_splitter_container=>false ).

    lo_split_col->set_row_height(
      EXPORTING
        id                = 1
        height            = 11
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3 ).

    mo_top_cnt = lo_split_col->get_container( row = 1 column = 1 ).

    CREATE OBJECT mo_top
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT mo_grid
      EXPORTING
        i_parent          = lo_split_col->get_container( row = 2 column = 1 )
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

  ENDMETHOD.                    "start_container

  METHOD  refresh_top_of_page.
    mo_grid->refresh_table_display( ).
    mo_top->initialize_document( ).
    top_of_page( ).
  ENDMETHOD.                    "refresh_top_of_page

  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
        ( function = 'FC_EXCLUDE'      text = 'Escludere documenti'   icon = icon_interval_exclude_red     quickinfo = 'Escludere documenti' )
        ( function = 'FC_ANULLA'       text = 'Annulla esclusione'    icon = icon_interval_include_green   quickinfo = 'Annulla esclusione' )
        ( function = 'FC_RIBALTAMENTO' text = 'Effettua ribaltamento' icon = icon_execute_object           quickinfo = 'Effettua ribaltamento' )
        ( function = 'FC_LOGS'         text = 'Log dei ribaltamenti'  icon = icon_display_text             quickinfo = 'Log dei ribaltamenti' ) ).

  ENDMETHOD.    "handle_toolbar

  METHOD top_of_page.

    CONSTANTS: lc_color   TYPE string VALUE '#E5E8E8'.

    DATA: lv_fatt_blocked TYPE i,
          lv_fatt_unblckd TYPE i,
          lv_fatt_error   TYPE i,
          lt_html_tab     TYPE sdydo_html_table.

    CASE mv_ribal_status.
      WHEN ms_status-fatto.
        DATA(lv_status) = EXACT char9('Fatto').
        DATA(lv_color)  = EXACT char6('green').
        DATA(lv_title)  = EXACT string('Elaborazione con successo').
      WHEN ms_status-errore.
        lv_status = 'Errore'.
        lv_color = 'red'.
        lv_title = |Errore durante l'elaborazione|.
      WHEN ms_status-attesa.
        lv_status = 'In Attesa'.
        lv_color = 'orange'.
        lv_title = |In attesa di elaborazione|.
    ENDCASE.

    LOOP AT mt_fattura ASSIGNING FIELD-SYMBOL(<ls_fattura>).
      IF <ls_fattura>-id = icon_interval_exclude_red.
        lv_fatt_blocked = lv_fatt_blocked + 1.
      ELSE.
        lv_fatt_unblckd = lv_fatt_unblckd + 1.
      ENDIF.
    ENDLOOP.

    DATA(lv_text) = EXACT sdydo_text_element( |Fatture estrate : { lines( mt_fattura ) }| ).

    lt_html_tab = VALUE #(
    ( |<style>| )
    ( | .header2 { '{' }| )
    ( |   font-size: 10pt;| )
    ( |   font-family:Sans-Serif;| )
    ( |   user-select:none;| )
    ( |   font-weight:bold;| )
    ( |{ '}' }| )
    ( |</style>| )
    ( |<body style="background-color: { lc_color }; border: 1px solid gray; margin: 0; padding: 10px ">| )
    ( |  <div style="display: flex; justify-content: space-between;">| )
    ( |    <font style = "font-size: 12pt; font-weight:bold; color="black"> { lv_text } </font>| )
    ( |    <span>| )
    ( |      <font class="header2" color="black"; title ="{ lv_title }"> Ribaltamento status: </font>| )
    ( |      <font class="header2" color="{ lv_color }" title ="{ lv_title }"> { lv_status } </font>| )
    ( |    </span>| )
    ( |  </div>| )
    ( |  <hr style=" margin-left: 0;">| )
    ( |  <font class="header2" color="black"> N. fatture ribaltabile : </font>| )
    ( |  <font class="header2" color="green"> { lv_fatt_unblckd } </font> &nbsp &nbsp &nbsp| )
    ( |  <font class="header2" color="black"> N. fatture esclude : </font>| )
    ( |  <font class="header2" color="red"> { lv_fatt_blocked } </font>| )
    ( |</body>| ) ).

    mo_top->add_static_html( table_with_html  = lt_html_tab ).
    mo_top->display_document(
     EXPORTING
       reuse_control      = 'X'
       parent             = mo_top_cnt
     EXCEPTIONS
       html_display_error = 1
       OTHERS             = 2 ).

  ENDMETHOD.

  METHOD set_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = mc_output_str
      CHANGING
        ct_fieldcat            = ev_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      MESSAGE e000(db) WITH 'Errore tecnico interno Fieldcat'.
    ENDIF.

    LOOP AT ev_fieldcat ASSIGNING FIELD-SYMBOL(<fcat>).
      <fcat>-reptext = <fcat>-scrtext_m.
      CASE <fcat>-fieldname.
        WHEN 'ID'.
          <fcat>-icon = abap_true.
          MOVE 'Status' TO <fcat>-scrtext_s.
          MOVE 'Status' TO <fcat>-reptext.
        WHEN 'RAG_SOC'.
          MOVE text-t01 TO <fcat>-scrtext_s.
          MOVE text-t02 TO <fcat>-scrtext_m.
          MOVE text-t03 TO <fcat>-scrtext_l.
          MOVE text-t03 TO <fcat>-reptext.
        WHEN 'MESS'.
          MOVE text-t04 TO <fcat>-scrtext_s.
          MOVE text-t05 TO <fcat>-scrtext_m.
          MOVE text-t06 TO <fcat>-scrtext_l.
          MOVE text-t06 TO <fcat>-reptext.
        WHEN 'TIPO'.
          MOVE text-t07 TO <fcat>-scrtext_s.
          MOVE text-t08 TO <fcat>-scrtext_m.
          MOVE text-t09 TO <fcat>-scrtext_l.
          MOVE text-t09 TO <fcat>-reptext.
        WHEN 'EBELN_ZFTE'.
          MOVE text-t10 TO <fcat>-scrtext_s.
          MOVE text-t10 TO <fcat>-scrtext_m.
          MOVE text-t10 TO <fcat>-scrtext_l.
          MOVE text-t10 TO <fcat>-reptext.
        WHEN 'VBUND' OR 'XREF2_HD' OR 'AWTYP' OR 'AWKEY' OR 'MATKL' OR 'TXZ01' OR 'KNTTP' OR 'SGTXT'.
          <fcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD message_store.

    DATA: lv_arbgb TYPE bapiret2-id   VALUE 'DB',
          lv_msgty TYPE bapiret2-type VALUE 'E'.

    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = COND #( WHEN iv_arbgb IS INITIAL THEN lv_arbgb ELSE iv_arbgb )
        msgty                  = COND #( WHEN iv_msgty IS INITIAL THEN lv_msgty ELSE iv_msgty )
        msgv1                  = iv_msg1
        msgv2                  = iv_msg2
        msgv3                  = iv_msg3
        msgv4                  = iv_msg4
        txtnr                  = COND #( WHEN iv_txtnr IS INITIAL THEN '000' ELSE iv_txtnr )
        zeile                  = iv_lineno
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

  METHOD pop_up_info.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_text
        text_button_1         = 'SI'
        text_button_2         = 'NO'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = cv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD at_selection_screen.

    "Controllo dei campi obbligatori ===================================================================
    IF p_integ    IS INITIAL OR s_dt_reg[] IS INITIAL OR
       s_ft_int[] IS INITIAL OR p_fondo    IS INITIAL OR
       p_for_i    IS INITIAL OR p_cli_f    IS INITIAL OR
       p_dt_r     IS INITIAL OR p_td_fat   IS INITIAL OR
       p_td_nc    IS INITIAL OR p_td_gir   IS INITIAL OR
       p_rec_cs   IS INITIAL OR p_cdc      IS INITIAL .

      MESSAGE 'ERRORE: Tutti i campi devono essere valorizzati' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per codice Prelios Integra ===================================================================
    SELECT SINGLE @abap_true
      FROM t001
      INTO @DATA(lv_exist)
      WHERE bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Codice Prelios Integra non valorizzato correttamente' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento Integra ===================================================================
    SELECT SINGLE @abap_true
      FROM t003
      INTO @lv_exist
      WHERE blart IN @s_ft_int.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento Integra non corretto' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per Fondo SGR ===================================================================
    SELECT SINGLE @abap_true
      FROM t001
      INTO @lv_exist
      WHERE bukrs = @p_fondo.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Fondo SGR non valorizzato correttamente' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per fornitore Prelios Integra ===================================================================
    SELECT SINGLE sperr, sperm
      FROM lfa1
      INTO @DATA(ls_lfa1)
      WHERE lifnr = @p_for_i.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Fornitore Prelios Integra non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF ls_lfa1-sperr = abap_true OR ls_lfa1-sperm = abap_true.
      MESSAGE 'Errore: Fornitore Prelios Integra bloccato' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperr
      FROM lfb1
      INTO @DATA(lv_sperr)
      WHERE lifnr = @p_for_i
      AND   bukrs = @p_fondo.

    IF sy-subrc <> 0.
      MESSAGE |Errore Fornitore Prelios Integra non aperto per la soc. { p_fondo }| TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_sperr = abap_true.
      MESSAGE |Errore: Fornitore Prelios Integra bloccato per la soc. { p_fondo }| TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperm
      FROM lfm1
      INTO @DATA(lv_sperm)
      WHERE lifnr = @p_for_i
      AND   ekorg = @p_ekorg.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Fornitore Prelios Integra non aperto lato MM' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_sperm = abap_true.
      MESSAGE 'Errore: Fornitore Prelios Integra bloccato per gli acquisti' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per cliente Fondo SGR ===================================================================
    SELECT SINGLE sperr
      FROM kna1
      INTO @DATA(ls_sperr)
      WHERE kunnr = @p_cli_f.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Cliente Fondo SGR non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF ls_sperr = abap_true.
      MESSAGE 'Errore: Cliente Fondo SGR è bloccato per le registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE sperr
      FROM knb1
      INTO @ls_sperr
      WHERE kunnr = @p_cli_f
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE |Errore: Cliente Fondo SGR non aperto per la soc. { p_integ }| TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_sperr = abap_true.
      MESSAGE 'Errore: Cliente Fondo SGR bloccato a livello societario' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per data registrazione documenti contabili ===================================================================
    IF p_dt_r > sy-datum.
      MESSAGE 'Attenzione: inserita una data registrazione futura' TYPE 'W'.
    ENDIF.

    "Controllo per tipo documento fattura attiva ===================================================================
    SELECT blart
      FROM t003
      INTO TABLE @DATA(lt_t003)
      WHERE blart IN ( @p_td_fat, @p_td_nc, @p_td_gir ).

    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_fat.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento fattura attiva non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento nota di credito attiva ===================================================================
    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_nc.
    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento nota di credito attiva non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per tipo documento giroconto ===================================================================
    READ TABLE lt_t003 TRANSPORTING NO FIELDS WITH KEY blart = p_td_gir.
    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Tipo documento giroconto non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per conto recupero costi ===================================================================
    SELECT SINGLE xspeb
      FROM ska1
      INTO @DATA(lv_xspeb)
      WHERE ktopl = 'STD'
      AND   saknr = @p_rec_cs.

    IF sy-subrc <> 0.
      MESSAGE 'ERRORE: Conto recupero costi non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_xspeb = abap_true.
      MESSAGE 'Errore: Conto recupero costi bloccato per registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE xspeb
      FROM skb1
      INTO @lv_xspeb
      WHERE saknr = @p_rec_cs
      AND   bukrs = @p_integ.

    IF sy-subrc <> 0.
      MESSAGE |Errore: Conto recupero costi non aperto per la soc. { p_integ }| TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_xspeb = abap_true.
      MESSAGE 'Errore: Conto recupero costi bloccato per le registrazioni' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

    "Controllo per centro di costo giroconto costi ===================================================================
    SELECT SINGLE bkzkp
      FROM csks
      INTO @DATA(lv_bkzkp)
      WHERE kostl = @p_cdc
      AND   kokrs = @p_integ
      AND   datbi = '99991231'.

    IF sy-subrc <> 0.
      MESSAGE 'Errore: Centro di costo giroconto costi non esiste in SAP' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ELSEIF lv_bkzkp = abap_true.
      MESSAGE 'Errore: Centro di costo giroconto costi bloccato' TYPE 'S' DISPLAY LIKE 'E'.
      ev_error = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.                    "at_selection_screen

  METHOD initialization.

    DATA: lc_pas_fatt TYPE tvarvc-name VALUE 'YFICMS_FACILITY_PAS_FATT', "fattura attiva
          lc_pas_nc   TYPE tvarvc-name VALUE 'YFICMS_FACILITY_PAS_NC'.   "nota di credito

    SELECT name,
           low
      FROM tvarvc
      INTO TABLE @DATA(lt_tvarvc)
      WHERE name IN ( @lc_pas_fatt, @lc_pas_nc ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    "Read per tipo documento fattura attiva
    READ TABLE lt_tvarvc ASSIGNING FIELD-SYMBOL(<ls_tvarvc>) WITH KEY name = lc_pas_fatt.
    IF sy-subrc = 0.
      p_td_fat = <ls_tvarvc>-low.
    ENDIF.

    "Read per tipo documento nota di credito attiva
    READ TABLE lt_tvarvc ASSIGNING <ls_tvarvc> WITH KEY name = lc_pas_nc.
    IF sy-subrc = 0.
      p_td_nc = <ls_tvarvc>-low.
    ENDIF.

  ENDMETHOD.                    "initialization

ENDCLASS.

INITIALIZATION.
  lcl_estrattore_fatture=>initialization( ).

AT SELECTION-SCREEN.
  IF lcl_estrattore_fatture=>at_selection_screen( ) = abap_true .
    STOP.
  ENDIF.

START-OF-SELECTION.
  CREATE OBJECT go_estrattore_fatture.
  go_estrattore_fatture->execute( ).
