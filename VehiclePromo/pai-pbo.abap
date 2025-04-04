*----------------------------------------------------------------------*
***INCLUDE ZLPBNS_PROMO_MD_0100M01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PROMO_STATUS'.
  SET TITLEBAR 'PROMO_TITLE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  go_zlpbns_promo_md->pbo_0100( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  go_zlpbns_promo_md->pai( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.

  go_zlpbns_promo_md->pai( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'PROMO_STATUS'.
  SET TITLEBAR 'MODEL_TITLE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.

  go_zlpbns_promo_md->pbo_0300( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0300 INPUT.

  go_zlpbns_promo_md->pai( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'PROMO_STATUS'.
  SET TITLEBAR 'VIN_TITLE'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.

  go_zlpbns_promo_md->pbo_0400( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0400 INPUT.

  go_zlpbns_promo_md->pai( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  DATA: lt_ex_fcode TYPE TABLE OF sy-ucomm.

  CASE go_promo->mv_state.
    WHEN gc_state-new OR gc_state-copy.
      lt_ex_fcode = VALUE #( ( 'FC_EDIT' )
                             ( 'FC_DISPLAY' ) ).
    WHEN gc_state-edit.
      lt_ex_fcode = VALUE #( ( 'FC_EDIT' )
                             ( 'FC_DELETE' ) ).
    WHEN gc_state-view.
      lt_ex_fcode = VALUE #( ( 'FC_DISPLAY' )
                             ( 'FC_DELETE')
                             ( 'FC_SAVE') ).
    WHEN OTHERS.
  ENDCASE.

  DATA(lv_title) = |Promo - { go_promo->ms_promo_header-market } - { go_promo->ms_promo_header-promo } - [{ go_promo->mv_state }]|.

  SET PF-STATUS 'PROMO_DETAIL_STATUS' EXCLUDING lt_ex_fcode.
  SET TITLEBAR 'PROMO_DETAIL_TITLE' WITH lv_title.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.

  go_promo->pbo( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  go_promo->pai( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0200 INPUT.

  go_promo->pai_exit200( ).

ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TS_PROMO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE ts_promo_active_tab_set OUTPUT.
  ts_promo-activetab = g_ts_promo-pressed_tab.
  CASE g_ts_promo-pressed_tab.
    WHEN c_ts_promo-tab1.
      g_ts_promo-subscreen = '0201'.
    WHEN c_ts_promo-tab2.
      g_ts_promo-subscreen = '0202'.
    WHEN c_ts_promo-tab3.
      g_ts_promo-subscreen = '0203'.
    WHEN c_ts_promo-tab4.
      g_ts_promo-subscreen = '0204'.
    WHEN c_ts_promo-tab5.
      g_ts_promo-subscreen = '0205'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'TS_PROMO'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE ts_promo_active_tab_get INPUT.
  go_promo->mv_ok_code = sy-ucomm.
  CASE go_promo->mv_ok_code.
    WHEN c_ts_promo-tab1.
      g_ts_promo-pressed_tab = c_ts_promo-tab1.
    WHEN c_ts_promo-tab2.
      g_ts_promo-pressed_tab = c_ts_promo-tab2.
    WHEN c_ts_promo-tab3.
      g_ts_promo-pressed_tab = c_ts_promo-tab3.
    WHEN c_ts_promo-tab4.
      g_ts_promo-pressed_tab = c_ts_promo-tab4.
    WHEN c_ts_promo-tab5.
      g_ts_promo-pressed_tab = c_ts_promo-tab5.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_TABSTRIP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_tabstrip OUTPUT.

  go_promo->ts_pbo( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_0200 INPUT.
  DATA: lv_dynfield      TYPE help_info-dynprofld,
        lv_dynfield_copy TYPE help_info-dynprofld,
        lv_fieldname     TYPE dfies-fieldname.

  GET CURSOR FIELD lv_dynfield.
  lv_dynfield_copy = lv_dynfield.

  REPLACE FIRST OCCURRENCE OF 'GO_PROMO->MS_PROMO_HEADER-' IN lv_dynfield_copy WITH ''.
  lv_fieldname = lv_dynfield_copy.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname           = lcl_zlpbns_promo_md=>mc_header_table
      fieldname         = lv_fieldname
      dynpprog          = sy-repid
      dynpnr            = sy-dynnr
      dynprofield       = lv_dynfield
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMODULE.
