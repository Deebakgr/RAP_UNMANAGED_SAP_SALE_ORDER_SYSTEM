CLASS zmk_sales_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sales_hdr,
             salesdocument TYPE vbeln,
           END OF ty_sales_hdr,
           BEGIN OF ty_sales_item,
             salesdocument   TYPE vbeln,
             salesitemnumber TYPE int2,
           END OF ty_sales_item.

    TYPES: tt_sales_item     TYPE STANDARD TABLE OF ty_sales_item,
           tt_sales_hdr      TYPE STANDARD TABLE OF ty_sales_hdr,
           tt_sales_itm_data TYPE STANDARD TABLE OF ZMK_DEEPAK_T WITH DEFAULT KEY. " <--- NEW TABLE TYPE

    CLASS-METHODS get_instance RETURNING VALUE(ro_instance) TYPE REF TO zmk_sales_util.

    METHODS:
      set_hdr_value IMPORTING im_sales_hdr TYPE ZMK_DEEBAK_T
                    EXPORTING ex_created   TYPE abap_boolean,
      get_hdr_value EXPORTING ex_sales_hdr TYPE ZMK_DEEBAK_T,

      set_itm_value IMPORTING im_sales_itm TYPE ZMK_DEEPAK_T
                    EXPORTING ex_created   TYPE abap_boolean,

      get_itm_value EXPORTING ex_sales_itm TYPE tt_sales_itm_data, " <--- RETURNS TABLE NOW

      set_hdr_t_deletion IMPORTING im_sales_doc TYPE ty_sales_hdr,
      set_itm_t_deletion IMPORTING im_sales_itm_info TYPE ty_sales_item,
      get_hdr_t_deletion EXPORTING ex_sales_docs TYPE tt_sales_hdr,
      get_itm_t_deletion EXPORTING ex_sales_info TYPE tt_sales_item,
      set_hdr_deletion_flag IMPORTING im_so_delete TYPE abap_boolean,
      get_deletion_flags EXPORTING ex_so_hdr_del TYPE abap_boolean,
      cleanup_buffer.

  PRIVATE SECTION.
    CLASS-DATA: gs_sales_hdr_buff   TYPE ZMK_DEEBAK_T,
                gt_sales_itm_buff   TYPE tt_sales_itm_data, " <--- CHANGED TO TABLE
                gt_sales_hdr_t_buff TYPE tt_sales_hdr,
                gt_sales_itm_t_buff TYPE tt_sales_item,
                gv_so_delete        TYPE abap_boolean.
    CLASS-DATA mo_instance TYPE REF TO zmk_sales_util.
ENDCLASS.

CLASS zmk_sales_util IMPLEMENTATION.
  METHOD get_instance.
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.

  METHOD set_hdr_value.
    IF im_sales_hdr-salesdocument IS NOT INITIAL.
      gs_sales_hdr_buff = im_sales_hdr.
      ex_created = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_hdr_value.
    ex_sales_hdr = gs_sales_hdr_buff.
  ENDMETHOD.

  METHOD set_itm_value.
    "Save Item Values to Transaction Buffer (Handle Multiple Items)
    IF im_sales_itm IS NOT INITIAL.
      " Check if this item is already in buffer to update it, otherwise append
      READ TABLE gt_sales_itm_buff WITH KEY salesdocument = im_sales_itm-salesdocument
                                            salesitemnumber = im_sales_itm-salesitemnumber
                                            TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY gt_sales_itm_buff FROM im_sales_itm INDEX sy-tabix.
      ELSE.
        APPEND im_sales_itm TO gt_sales_itm_buff.
      ENDIF.
      ex_created = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_itm_value.
    ex_sales_itm = gt_sales_itm_buff. " Returns the whole table
  ENDMETHOD.

  METHOD set_hdr_t_deletion.
    APPEND im_sales_doc TO gt_sales_hdr_t_buff.
  ENDMETHOD.

  METHOD set_itm_t_deletion.
    APPEND im_sales_itm_info TO gt_sales_itm_t_buff.
  ENDMETHOD.

  METHOD get_hdr_t_deletion.
    ex_sales_docs = gt_sales_hdr_t_buff.
  ENDMETHOD.

  METHOD get_itm_t_deletion.
    ex_sales_info = gt_sales_itm_t_buff.
  ENDMETHOD.

  METHOD set_hdr_deletion_flag.
    gv_so_delete = im_so_delete.
  ENDMETHOD.

  METHOD get_deletion_flags.
    ex_so_hdr_del = gv_so_delete.
  ENDMETHOD.

  METHOD cleanup_buffer.
    CLEAR: gs_sales_hdr_buff, gt_sales_itm_buff, gt_sales_hdr_t_buff, gt_sales_itm_t_buff, gv_so_delete.
  ENDMETHOD.
ENDCLASS.
