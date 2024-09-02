**&---------------------------------------------------------------------*
**& Report Z_DEL_SA
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
REPORT z_del_sa.
TABLES: ekko, ekpo , eket .
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
*       CLASS lcl_duplicate DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_duplicate DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS execute.
  PRIVATE SECTION.
    METHODS extract_data.
    METHODS block  IMPORTING iv_ebeln      TYPE ebeln
                   CHANGING  it_items_seg  TYPE bapimeout_t_item
                             it_itemsx_seg TYPE bapimeout_t_itemx
                             ct_messages   TYPE esp1_message_tab_type.
    METHODS eleminate IMPORTING iv_ebeln    TYPE ebeln
                      CHANGING  it_items_seg  TYPE bapimeout_t_item
                                it_itemsx_seg TYPE bapimeout_t_itemx
                                ct_messages   TYPE esp1_message_tab_type.
ENDCLASS.
DATA go_duplicate TYPE REF TO lcl_duplicate.
*----------------------------------------------------------------------*
*       CLASS lcl_duplicate IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_duplicate IMPLEMENTATION.
  METHOD execute.
    extract_data( ).
  ENDMETHOD.                    "execute              "
  METHOD extract_data.
    DATA: lt_messages TYPE STANDARD TABLE OF bapiret2,
          lt_items    TYPE STANDARD TABLE OF bapimepoitem,
          ls_items    TYPE bapimepoitem,
          lt_itemsx   TYPE STANDARD TABLE OF bapimepoitemx,
          ls_itemsx   TYPE bapimepoitemx,
          lt_all_msg  TYPE STANDARD TABLE OF esp1_message_wa_type.

    DATA: lt_items_sag  TYPE STANDARD TABLE OF bapimeoutitem,
          ls_items_sag  TYPE bapimeoutitem,
          lt_itemsx_sag TYPE STANDARD TABLE OF bapimeoutitemx,
          ls_itemsx_sag TYPE bapimeoutitemx.

    SELECT ebeln , ebelp , loekz
      FROM ekpo
      WHERE ebeln IN @s_ebeln
      AND loekz IS INITIAL
       INTO TABLE @DATA(lt_ekpo).
    SELECT ebeln , ebelp
      FROM eket
      INTO TABLE @DATA(lt_eket)
      FOR ALL ENTRIES IN @lt_ekpo
      WHERE ebeln = @lt_ekpo-ebeln.

    LOOP AT lt_ekpo INTO DATA(ls_ekpo) GROUP BY ( ebeln = ls_ekpo-ebeln  ) ASSIGNING FIELD-SYMBOL(<fs_group>).
      READ TABLE lt_eket ASSIGNING FIELD-SYMBOL(<fs_eket>) WITH KEY ebeln = <fs_group>-ebeln .
      IF sy-subrc = 0.
        DATA(lv_value) = abap_true.
      ENDIF.
      LOOP AT GROUP <fs_group> ASSIGNING FIELD-SYMBOL(<fs_grp_wa>).
        IF lv_value = abap_true.
          ls_items_sag = VALUE #( item_no = <fs_grp_wa>-ebelp no_more_gr = 'X' final_inv = 'X'  delete_ind = 'S'  ).
          ls_itemsx_sag = VALUE #(  item_no = <fs_grp_wa>-ebelp no_more_gr = 'X' final_inv = 'X'  delete_ind = 'X' ).
        ELSE.
          ls_items_sag  = VALUE #( item_no = <fs_grp_wa>-ebelp delete_ind = 'L'  ).
          ls_itemsx_sag = VALUE #(  item_no = <fs_grp_wa>-ebelp delete_ind = 'X' ).
        ENDIF.
        APPEND ls_items_sag TO lt_items_sag.
        APPEND ls_itemsx_sag TO lt_itemsx_sag.
*
*        APPEND ls_items TO lt_items.
*        APPEND ls_itemsx TO lt_itemsx.
      ENDLOOP.

      CASE lv_value.
        WHEN  abap_true.   "S PER BLOCCARE
          block(
          EXPORTING
            iv_ebeln  = <fs_grp_wa>-ebeln
          CHANGING
            it_items_seg  = lt_items_sag
            it_itemsx_seg = lt_itemsx_sag
            ct_messages = lt_all_msg ).
        WHEN OTHERS.    "L sta per delete
          eleminate(
          EXPORTING
            iv_ebeln  = <fs_grp_wa>-ebeln
          CHANGING
            it_items_seg  = lt_items_sag
            it_itemsx_seg = lt_itemsx_sag
            ct_messages = lt_all_msg ).
      ENDCASE.
      CLEAR: lt_items, lt_itemsx, ls_items, ls_itemsx.
    ENDLOOP.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = lt_all_msg.
  ENDMETHOD.                    "extract_data

  METHOD block.
    DATA: lt_messages TYPE STANDARD TABLE OF bapiret2,
          lt_all_msg  TYPE STANDARD TABLE OF esp1_message_wa_type.

*    CALL FUNCTION 'BAPI_PO_CHANGE'
*      EXPORTING
*        purchaseorder = iv_ebeln
*      TABLES
*        return        = lt_messages
*        poitem        = it_items
*        poitemx       = it_itemsx.

    CALL FUNCTION 'BAPI_SAG_CHANGE'
      EXPORTING
        purchasingdocument = iv_ebeln
      TABLES
        return             = lt_messages
        item               = it_items_seg
        itemx              = it_itemsx_seg.
    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
      AT FIRST.
        IF <fs_msg>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.
      ENDAT.
      APPEND VALUE #( msgid = <fs_msg>-id
                      msgty = <fs_msg>-type
                      msgno = <fs_msg>-number
                      msgv1 = <fs_msg>-message_v1
                      msgv2 = <fs_msg>-message_v2
                      msgv3 = <fs_msg>-message_v3
                      msgv4 = <fs_msg>-message_v4
                      lineno = sy-tabix ) TO ct_messages.
    ENDLOOP.
    IF lv_error = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR lv_error.
      RETURN.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.
  METHOD eleminate.
    DATA: lt_messages TYPE STANDARD TABLE OF bapiret2,
          lt_all_msg  TYPE STANDARD TABLE OF esp1_message_wa_type.
*    CALL FUNCTION 'BAPI_PO_CHANGE'
*      EXPORTING
*        purchaseorder = iv_ebeln
*      TABLES
*        return        = lt_messages
*        poitem        = it_items
*        poitemx       = it_itemsx.
      CALL FUNCTION 'BAPI_SAG_CHANGE'
      EXPORTING
        purchasingdocument = iv_ebeln
      TABLES
        return             = lt_messages
        item               = it_items_seg
        itemx              = it_itemsx_seg.
    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<fs_msg>).
      AT FIRST.
        IF <fs_msg>-type CA 'EAX'.
          DATA(lv_error) = abap_true.
        ENDIF.
      ENDAT.
      APPEND VALUE #( msgid = <fs_msg>-id
                      msgty = <fs_msg>-type
                      msgno = <fs_msg>-number
                      msgv1 = <fs_msg>-message_v1
                      msgv2 = <fs_msg>-message_v2
                      msgv3 = <fs_msg>-message_v3
                      msgv4 = <fs_msg>-message_v4
                      lineno = sy-tabix ) TO ct_messages.
    ENDLOOP.
    IF lv_error = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      CLEAR lv_error.
      RETURN.
    ENDIF.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  CREATE OBJECT go_duplicate.
  go_duplicate->execute( ).
