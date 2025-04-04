*&---------------------------------------------------------------------*
*&  Include           ZLPBNS_PROMO_MD_TOP
*&---------------------------------------------------------------------*

TYPE-POOLS: icon, esp1.

TABLES: zlpbns_promo_mdh,
        zlpbns_pr_model,
        zlpbns_pr_vin,
        zlpbns_pr_fleet.

TYPES: BEGIN OF ty_tabstrip_header,
         name     TYPE screen-name,
         data_tab TYPE char24,
         empty    TYPE char50,
         full     TYPE char50,
       END OF ty_tabstrip_header.

CONSTANTS: BEGIN OF gc_state,
             new  TYPE string VALUE 'NEW',
             edit TYPE string VALUE 'EDIT',
             view TYPE string VALUE 'VIEW',
             copy TYPE string VALUE 'COPY',
           END OF gc_state.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TS_PROMO'
CONSTANTS: BEGIN OF c_ts_promo,
             tab1 LIKE sy-ucomm VALUE 'FC_MODEL',
             tab2 LIKE sy-ucomm VALUE 'FC_CHASSIS',
             tab3 LIKE sy-ucomm VALUE 'FC_FLEET',
             tab4 LIKE sy-ucomm VALUE 'FC_VIN',
             tab5 LIKE sy-ucomm VALUE 'FC_INCOMP',
           END OF c_ts_promo.
*&SPWIZARD: DATA FOR TABSTRIP 'TS_PROMO'
CONTROLS:  ts_promo TYPE TABSTRIP.
DATA: BEGIN OF g_ts_promo,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZLPBNS_PROMO_MD',
        pressed_tab LIKE sy-ucomm VALUE c_ts_promo-tab1,
      END OF g_ts_promo.

DATA: ts_promo_tab1      TYPE char30,
      ts_promo_tab2      TYPE char30,
      ts_promo_tab3      TYPE char30,
      ts_promo_tab4      TYPE char30,
      ts_promo_tab5      TYPE char30,
      gt_tabstrip_header TYPE STANDARD TABLE OF ty_tabstrip_header.

DATA: gt_status_text TYPE STANDARD TABLE OF zlpbns_pr_stat_t.
