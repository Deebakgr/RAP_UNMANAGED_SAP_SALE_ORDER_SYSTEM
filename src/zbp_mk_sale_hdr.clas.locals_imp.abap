" MAIN BEHAVIOR HANDLER CLASS  (No helper class needed anymore)
"--------------------------------------------------------------
CLASS lhc_SalesOrderHdr DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION IMPORTING keys REQUEST requested_authorizations FOR SalesOrderHdr RESULT result.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION IMPORTING REQUEST requested_authorizations FOR SalesOrderHdr RESULT result.
    METHODS create FOR MODIFY IMPORTING entities FOR CREATE SalesOrderHdr.
    METHODS update FOR MODIFY IMPORTING entities FOR UPDATE SalesOrderHdr.
    METHODS delete FOR MODIFY IMPORTING keys FOR DELETE SalesOrderHdr.
    METHODS read FOR READ IMPORTING keys FOR READ SalesOrderHdr RESULT result.
    METHODS lock FOR LOCK IMPORTING keys FOR LOCK SalesOrderHdr.
    METHODS rba_Salesitem FOR READ IMPORTING keys_rba FOR READ SalesOrderHdr\_Salesitem FULL result_requested RESULT result LINK association_links.
    METHODS cba_Salesitem FOR MODIFY IMPORTING entities_cba FOR CREATE SalesOrderHdr\_Salesitem.
    METHODS GenerateInvoice FOR MODIFY IMPORTING keys FOR ACTION SalesOrderHdr~GenerateInvoice RESULT result.
ENDCLASS.

CLASS lhc_SalesOrderHdr IMPLEMENTATION.

  METHOD get_instance_authorizations. ENDMETHOD.
  METHOD get_global_authorizations. ENDMETHOD.
  METHOD lock. ENDMETHOD.

  METHOD create.
    DATA: ls_sales_hdr TYPE zmk_deebak_t.
    LOOP AT entities INTO DATA(ls_entities).
      ls_sales_hdr = CORRESPONDING #( ls_entities MAPPING FROM ENTITY ).

      IF ls_sales_hdr-salesdocument IS NOT INITIAL.
        SELECT SINGLE FROM zmk_deebak_t FIELDS salesdocument
          WHERE salesdocument = @ls_sales_hdr-salesdocument
          INTO @DATA(lv_exists).

        IF sy-subrc NE 0.
          DATA(lo_util) = zmk_sales_util=>get_instance( ).
          lo_util->set_hdr_value( EXPORTING im_sales_hdr = ls_sales_hdr IMPORTING ex_created = DATA(lv_created) ).
          IF lv_created EQ abap_true.
            APPEND VALUE #( %cid = ls_entities-%cid salesdocument = ls_sales_hdr-salesdocument ) TO mapped-salesorderhdr.
            APPEND VALUE #( %cid = ls_entities-%cid salesdocument = ls_sales_hdr-salesdocument
                            %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Sales Order Created' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderhdr.
          ENDIF.
        ELSE.
          APPEND VALUE #( %cid = ls_entities-%cid salesdocument = ls_sales_hdr-salesdocument ) TO failed-salesorderhdr.
          APPEND VALUE #( %cid = ls_entities-%cid salesdocument = ls_sales_hdr-salesdocument
                          %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Duplicate Sales Order' severity = if_abap_behv_message=>severity-error ) ) TO reported-salesorderhdr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update.

    " ---------------------------------------------------------------
    " FIXED: Structure now matches your Excel (7 Columns, A to G)
    " ---------------------------------------------------------------
    TYPES: BEGIN OF ty_excel_row,
             col_a TYPE string,   " Column A - Sales Doc (Ignored)
             col_b TYPE string,   " Column B - Item Number (Ignored, we auto-gen)
             col_c TYPE string,   " Column C - Material
             col_d TYPE string,   " Column D - Plant
             col_e TYPE string,   " Column E - Quantity
             col_f TYPE string,   " Column F - NetPrice
             col_g TYPE string,   " Column G - Currency
           END OF ty_excel_row.

    DATA: ls_sales_hdr   TYPE zmk_deebak_t.
    DATA(lo_util) = zmk_sales_util=>get_instance( ).

    LOOP AT entities INTO DATA(ls_entities).

      " 1. TRY READING FROM BUFFER FIRST
      lo_util->get_hdr_value( IMPORTING ex_sales_hdr = ls_sales_hdr ).

      " 2. IF BUFFER EMPTY or WRONG ID, READ FROM DB
      IF ls_sales_hdr-salesdocument <> ls_entities-SalesDocument.
        CLEAR ls_sales_hdr.
        SELECT SINGLE FROM zmk_deebak_t FIELDS *
          WHERE salesdocument = @ls_entities-SalesDocument
          INTO @ls_sales_hdr.
      ENDIF.

      " 3. MERGE CHANGES
      IF ls_entities-%control-SalesDocumentType   = if_abap_behv=>mk-on. ls_sales_hdr-salesdocumenttype   = ls_entities-SalesDocumentType.   ENDIF.
      IF ls_entities-%control-OrderReason         = if_abap_behv=>mk-on. ls_sales_hdr-orderreason         = ls_entities-OrderReason.         ENDIF.
      IF ls_entities-%control-SalesOrganization   = if_abap_behv=>mk-on. ls_sales_hdr-salesorganization   = ls_entities-SalesOrganization.   ENDIF.
      IF ls_entities-%control-DistributionChannel = if_abap_behv=>mk-on. ls_sales_hdr-distributionchannel = ls_entities-DistributionChannel. ENDIF.
      IF ls_entities-%control-Division            = if_abap_behv=>mk-on. ls_sales_hdr-division            = ls_entities-Division.            ENDIF.
      IF ls_entities-%control-SalesOffice         = if_abap_behv=>mk-on. ls_sales_hdr-salesoffice         = ls_entities-SalesOffice.         ENDIF.
      IF ls_entities-%control-SalesGroup          = if_abap_behv=>mk-on. ls_sales_hdr-salesgroup          = ls_entities-SalesGroup.          ENDIF.
      IF ls_entities-%control-NetPrice            = if_abap_behv=>mk-on. ls_sales_hdr-netprice            = ls_entities-NetPrice.            ENDIF.
      IF ls_entities-%control-Currency            = if_abap_behv=>mk-on. ls_sales_hdr-currency            = ls_entities-Currency.            ENDIF.

      " --- Existing Invoice Attachment Logic ---
      IF ls_entities-%control-Attachment = if_abap_behv=>mk-on.
        ls_sales_hdr-attachment = ls_entities-Attachment.
        ls_sales_hdr-mimetype   = ls_entities-MimeType.
        ls_sales_hdr-filename   = ls_entities-FileName.
      ENDIF.

     " -----------------------------------------------------------------
     " --- FIXED EXCEL UPLOAD LOGIC ------------------------------------
     " -----------------------------------------------------------------
     IF ls_entities-%control-ExcelAttachment = if_abap_behv=>mk-on.

      ls_sales_hdr-excel_attachment = ls_entities-ExcelAttachment.
      ls_sales_hdr-excel_mimetype   = ls_entities-ExcelMimeType.
      ls_sales_hdr-excel_filename   = ls_entities-ExcelFileName.

      TRY.
          DATA(lo_xlsx) = xco_cp_xlsx=>document->for_file_content(
                              ls_entities-ExcelAttachment )->read_access( ).

          DATA(lo_worksheet) = lo_xlsx->get_workbook( )->worksheet->at_position( 1 ).

          " ✅ READ A TO G (7 Columns)
          DATA(lo_selection_pattern) = xco_cp_xlsx_selection=>pattern_builder->simple_from_to(
              )->from_column( xco_cp_xlsx=>coordinate->for_alphabetic_value( 'A' )
              )->to_column(   xco_cp_xlsx=>coordinate->for_alphabetic_value( 'G' )
              )->from_row(    xco_cp_xlsx=>coordinate->for_numeric_value( 1 )
              )->get_pattern( ).

          DATA lt_excel_rows TYPE STANDARD TABLE OF ty_excel_row.

          lo_worksheet->select( lo_selection_pattern
              )->row_stream( )->operation->write_to( REF #( lt_excel_rows )
              )->set_value_transformation(
                  xco_cp_xlsx_read_access=>value_transformation->string_value
              )->if_xco_xlsx_ra_operation~execute( ).

          " Remove Header Row
         DELETE lt_excel_rows INDEX 1.

          " Safety check: Skip if Material (Col C) is empty
          DELETE lt_excel_rows WHERE col_c IS INITIAL.

          DATA lv_item_pos TYPE int2 VALUE 10.

          LOOP AT lt_excel_rows INTO DATA(ls_row).
            " ✅ FIXED MAPPING:
            " Col B is ItemNumber (ignored), Col C is Material, Col D is Plant...

            DATA(ls_new_item) = VALUE zmk_deepak_t(
              salesdocument   = ls_entities-SalesDocument
              salesitemnumber = lv_item_pos
              material        = ls_row-col_c   " Was col_b (Wrong)
              plant           = ls_row-col_d   " Was col_c (Wrong)
              quantity        = ls_row-col_e   " Was col_d (Wrong)
              netprice        = ls_row-col_f   " Was col_e (Wrong)
              currency        = ls_row-col_g   " Was col_f (Wrong)
            ).

            lo_util->set_itm_value( EXPORTING im_sales_itm = ls_new_item
                                    IMPORTING ex_created   = DATA(lv_itm_created) ).
            lv_item_pos += 10.
          ENDLOOP.

          APPEND VALUE #( %tky = ls_entities-%tky
                          %msg = new_message( id       = 'ZMK_SALES_MSG'
                                              number   = 001
                                              v1       = |Excel Uploaded. { lines( lt_excel_rows ) } items processed.|
                                              severity = if_abap_behv_message=>severity-success )
                        ) TO reported-salesorderhdr.

        CATCH cx_root INTO DATA(lx_err).
          APPEND VALUE #( %tky = ls_entities-%tky
                          %msg = new_message( id       = 'ZMK_SALES_MSG'
                                              number   = 001
                                              v1       = 'Excel Parsing Failed. Check file format.'
                                              severity = if_abap_behv_message=>severity-error )
                        ) TO reported-salesorderhdr.
      ENDTRY.
    ENDIF.

      " 4. SAVE BACK TO BUFFER
      IF ls_sales_hdr-salesdocument IS NOT INITIAL.
        lo_util->set_hdr_value( EXPORTING im_sales_hdr = ls_sales_hdr IMPORTING ex_created = DATA(lv_created) ).
        IF lv_created EQ abap_true.
          APPEND VALUE #( %key = ls_entities-%key
                          %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Update Successful' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderhdr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD delete.
    TYPES: BEGIN OF ty_sales_hdr, salesdocument TYPE vbeln, END OF ty_sales_hdr.
    DATA ls_sales_hdr TYPE ty_sales_hdr.
    DATA(lo_util) = zmk_sales_util=>get_instance( ).
    LOOP AT keys INTO DATA(ls_key).
      CLEAR ls_sales_hdr.
      ls_sales_hdr-salesdocument = ls_key-salesdocument.
      lo_util->set_hdr_t_deletion( EXPORTING im_sales_doc = ls_sales_hdr ).
      lo_util->set_hdr_deletion_flag( EXPORTING im_so_delete = abap_true ).
      APPEND VALUE #( %cid = ls_key-%cid_ref salesdocument = ls_key-salesdocument
                      %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Deletion Successful' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderhdr.
    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    LOOP AT keys INTO DATA(ls_key).
      SELECT SINGLE FROM zmk_deebak_t FIELDS *
        WHERE salesdocument = @ls_key-salesdocument
        INTO @DATA(ls_hdr).
      IF sy-subrc = 0.
        APPEND CORRESPONDING #( ls_hdr ) TO result.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD rba_Salesitem.
    LOOP AT keys_rba INTO DATA(ls_key).
      SELECT FROM zmk_deepak_t FIELDS *
        WHERE salesdocument = @ls_key-salesdocument
        INTO TABLE @DATA(lt_items).
      LOOP AT lt_items INTO DATA(ls_item).
        APPEND CORRESPONDING #( ls_item ) TO result.
        APPEND VALUE #( source-salesdocument   = ls_key-salesdocument
                        target-salesdocument   = ls_item-salesdocument
                        target-salesitemnumber = ls_item-salesitemnumber ) TO association_links.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD cba_Salesitem.
    DATA ls_sales_itm TYPE zmk_deepak_t.
    LOOP AT entities_cba INTO DATA(ls_entities_cba).
      LOOP AT ls_entities_cba-%target INTO DATA(ls_item_create).
        ls_sales_itm = CORRESPONDING #( ls_item_create MAPPING FROM ENTITY ).
        ls_sales_itm-salesdocument = ls_entities_cba-SalesDocument.
        IF ls_sales_itm-salesdocument IS NOT INITIAL AND ls_sales_itm-salesitemnumber IS NOT INITIAL.
          DATA(lo_util) = zmk_sales_util=>get_instance( ).
          lo_util->set_itm_value( EXPORTING im_sales_itm = ls_sales_itm IMPORTING ex_created = DATA(lv_created) ).
          IF lv_created EQ abap_true.
            APPEND VALUE #( %cid = ls_item_create-%cid salesdocument = ls_sales_itm-salesdocument salesitemnumber = ls_sales_itm-salesitemnumber ) TO mapped-salesorderitm.
            APPEND VALUE #( %cid = ls_item_create-%cid salesdocument = ls_sales_itm-salesdocument
                            %msg = new_message( id = 'ZMK_SALES_MSG' number = 001 v1 = 'Item Created' severity = if_abap_behv_message=>severity-success ) ) TO reported-salesorderitm.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD GenerateInvoice.
    DATA: lv_content TYPE string,
          lv_xstring TYPE xstring.

    READ ENTITIES OF zmk_sale_i IN LOCAL MODE
      ENTITY SalesOrderHdr ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_headers).

    READ ENTITIES OF zmk_sale_i IN LOCAL MODE
      ENTITY SalesOrderHdr BY \_salesitem ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_items).

    LOOP AT lt_headers INTO DATA(ls_header).
      lv_content = |INVOICE RECEIPT\n| &&
                   |------------------\n| &&
                   |Order: { ls_header-SalesDocument }\n| &&
                   |Total Order Value: { ls_header-NetPrice } { ls_header-Currency }\n\n| &&
                   |Items:\n|.

      LOOP AT lt_items INTO DATA(ls_item) WHERE SalesDocument = ls_header-SalesDocument.
        lv_content = |{ lv_content }- { ls_item-Material } (Qty: { ls_item-Quantity }) | &&
                     |Price: { ls_item-NetPrice } { ls_item-Currency }\n|.
      ENDLOOP.

      DATA(lo_conv) = cl_abap_conv_codepage=>create_out( ).
      lv_xstring = lo_conv->convert( source = lv_content ).

      MODIFY ENTITIES OF zmk_sale_i IN LOCAL MODE
        ENTITY SalesOrderHdr
        UPDATE FIELDS ( Attachment MimeType FileName )
        WITH VALUE #( ( %tky       = ls_header-%tky
                        Attachment = lv_xstring
                        MimeType   = 'text/plain'
                        FileName   = |Invoice_{ ls_header-SalesDocument }.txt| ) ).

      APPEND VALUE #( %tky = ls_header-%tky
                      %msg = new_message( id = 'ZMK_SALES_MSG' number = 001
                             v1 = 'Invoice Generated Successfully' severity = if_abap_behv_message=>severity-success )
                    ) TO reported-salesorderhdr.

      DATA ls_res LIKE LINE OF result.
      ls_res-%tky              = ls_header-%tky.
      ls_res-%param            = CORRESPONDING #( ls_header ).
      ls_res-%param-Attachment = lv_xstring.
      ls_res-%param-MimeType   = 'text/plain'.
      ls_res-%param-FileName   = |Invoice_{ ls_header-SalesDocument }.txt|.
      APPEND ls_res TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
