REPORT zrd_updateprice.

INCLUDE zrd_updateprice_top.
INCLUDE zrd_updateprice_implement.

INITIALIZATION.
  lcl_updateprice=>initialization( ).

AT SELECTION-SCREEN.
  lcl_updateprice=>at_selection_screen( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_updateprice=>at_selection_screen_output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_updateprice=>f4_path( ).

START-OF-SELECTION.
  NEW lcl_updateprice( )->execute( ).
