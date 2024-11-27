FUNCTION-POOL ysscms_usr                 MESSAGE-ID sv.
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lysscms_usrt00                          . "view rel. data dcl.

CONSTANTS:
  on  LIKE sonv-flag VALUE 'X',
  off LIKE sonv-flag VALUE ' '.

CONSTANTS:  gc_esc VALUE '"'.

TYPES: BEGIN OF ty_s_senderline,
         line(30000) TYPE c,
       END OF ty_s_senderline.

TYPES: ty_t_sender TYPE ty_s_senderline  OCCURS 0.


TYPES: BEGIN OF z_alsmex_tabline,
          ROW type KCD_EX_ROW_N,
          COL type KCD_EX_COL_N,
          value type c LENGTH 4096,
       END OF z_alsmex_tabline.

TYPES: ty_d_itabvalue TYPE z_alsmex_tabline-value,
       ty_t_itab      TYPE z_alsmex_tabline   OCCURS 0.
