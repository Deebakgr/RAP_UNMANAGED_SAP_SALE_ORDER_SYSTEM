CLASS lhc_SalesOrderItem DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS update FOR MODIFY IMPORTING entities FOR UPDATE SalesOrderItm.
    METHODS delete FOR MODIFY IMPORTING keys FOR DELETE SalesOrderItm.
    METHODS read FOR READ IMPORTING keys FOR READ SalesOrderItm RESULT result.
    METHODS rba_Salesheader FOR READ IMPORTING keys_rba FOR READ SalesOrderItm\_Salesheader FULL result_requested RESULT result LINK association_links.
ENDCLASS.

CLASS lhc_SalesOrderItem IMPLEMENTATION.
  METHOD update.
    DATA: ls_sales_itm TYPE ZMK_DEEPAK_T.
    LOOP AT entities INTO DATA(ls_entities).
      ls_sales_itm = CORRESPONDING #( ls_entities MAPPING FROM ENTITY ).
      IF ls_sales_itm-salesdocument IS NOT INITIAL.
        SELECT FROM ZMK_DEEPAK_T FIELDS * WHERE salesdocument = @ls_sales_itm-salesdocument
          AND salesitemnumber = @ls_sales_itm-salesitemnumber
          INTO TABLE @DATA(lt_sales_itm).

        IF sy-subrc EQ 0.
          DATA(lo_util) = zmk_sales_util=>get_instance( ).
          lo_util->set_itm_value( EXPORTING im_sales_itm = ls_sales_itm IMPORTING ex_created = DATA(lv_created) ).
          IF lv_created EQ abap_true.
            " FIX: REMOVED MAPPED FILLING HERE - THIS CAUSED THE DUMP
            APPEND VALUE #( %key = ls_entities-%key
                            %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Sales Item Updation Successful' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderitm.
          ENDIF.
        ELSE.
          APPEND VALUE #( %cid = ls_entities-%cid_ref salesdocument = ls_sales_itm-salesdocument salesitemnumber = ls_sales_itm-salesitemnumber ) TO failed-salesorderitm.
          APPEND VALUE #( %cid = ls_entities-%cid_ref salesdocument = ls_sales_itm-salesdocument
                          %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Duplicate Sales Order' severity = if_abap_behv_message=>severity-error ) ) TO reported-salesorderitm.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    TYPES: BEGIN OF ty_sales_item, salesdocument TYPE vbeln, salesitemnumber TYPE int2, END OF ty_sales_item.
    DATA ls_sales_itm TYPE ty_sales_item.
    DATA(lo_util) = zmk_sales_util=>get_instance( ).
    LOOP AT keys INTO DATA(ls_key).
      CLEAR ls_sales_itm.
      ls_sales_itm-salesdocument = ls_key-salesdocument.
      ls_sales_itm-salesitemnumber = ls_key-SalesItemnumber.
      lo_util->set_itm_t_deletion( im_sales_itm_info = ls_sales_itm ).
      APPEND VALUE #( %cid = ls_key-%cid_ref salesdocument = ls_key-salesdocument salesitemnumber = ls_key-SalesItemnumber
                      %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Sales Item Deletion Successful' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderitm.
    ENDLOOP.
  ENDMETHOD.

  METHOD read. ENDMETHOD.
  METHOD rba_Salesheader. ENDMETHOD.
ENDCLASS.
