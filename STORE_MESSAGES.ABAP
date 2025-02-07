


        CALL FUNCTION 'MESSAGES_INITIALIZE'
          EXCEPTIONS
            OTHERS = 1.

    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = 'N1ME'
        msgty                  = 'E'
        msgv1                  = iv_msg_field
        txtnr                  = '087'
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2
        OTHERS                 = 3.

        CALL FUNCTION 'MESSAGES_SHOW'
          EXPORTING
            i_use_grid         = 'X'  " Comment for list display
            batch_list_type    = 'L'
            send_if_one        = space
            show_linno         = ''
            show_linno_text    = space
          EXCEPTIONS
            inconsistent_range = 1
            no_messages        = 2
            OTHERS             = 3.




