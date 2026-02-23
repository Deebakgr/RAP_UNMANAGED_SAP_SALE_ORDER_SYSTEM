CLASS lsc_ZMK_SALE_I DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.
    METHODS finalize REDEFINITION.
    METHODS check_before_save REDEFINITION.
    METHODS save REDEFINITION.
    METHODS cleanup REDEFINITION.
    METHODS cleanup_finalize REDEFINITION.
ENDCLASS.

CLASS lsc_ZMK_SALE_I IMPLEMENTATION.
  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
    "Save Data to Buffer
    DATA(lo_util) = zmk_sales_util=>get_instance( ).

    " Get Header (Single Structure)
    lo_util->get_hdr_value( IMPORTING ex_sales_hdr = DATA(ls_sales_hdr) ).

    " Get Items (Returns a Table now)
    lo_util->get_itm_value( IMPORTING ex_sales_itm = DATA(lt_sales_itm) ).

    " Get Deletions
    lo_util->get_hdr_t_deletion( IMPORTING ex_sales_docs = DATA(lt_sales_header) ).
    lo_util->get_itm_t_deletion( IMPORTING ex_sales_info = DATA(lt_sales_items) ).
    lo_util->get_deletion_flags( IMPORTING ex_so_hdr_del = DATA(lv_so_hdr_del) ).

    " 1. Save/Update Header
    IF ls_sales_hdr IS NOT INITIAL.
      MODIFY ZMK_DEEBAK_T FROM @ls_sales_hdr.
    ENDIF.

    " 2. Save/Update Items (Using FROM TABLE)
    IF lt_sales_itm IS NOT INITIAL.
      MODIFY ZMK_DEEPAK_T FROM TABLE @lt_sales_itm.
    ENDIF.

    " 3. Handle Deletions
    IF lv_so_hdr_del = abap_true.
      LOOP AT lt_sales_header INTO DATA(ls_del_hdr).
        DELETE FROM ZMK_DEEBAK_T WHERE salesdocument = @ls_del_hdr-salesdocument.
        DELETE FROM ZMK_DEEPAK_T WHERE salesdocument = @ls_del_hdr-salesdocument.
      ENDLOOP.
    ELSE.
      LOOP AT lt_sales_header INTO ls_del_hdr.
        DELETE FROM ZMK_DEEBAK_T WHERE salesdocument = @ls_del_hdr-salesdocument.
      ENDLOOP.
      LOOP AT lt_sales_items INTO DATA(ls_del_itm).
        DELETE FROM ZMK_DEEPAK_T WHERE salesdocument = @ls_del_itm-salesdocument
                                 AND salesitemnumber = @ls_del_itm-salesitemnumber.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    zmk_sales_util=>get_instance( )->cleanup_buffer( ).
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.
ENDCLASS.
