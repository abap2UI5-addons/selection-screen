CLASS z2ui5_cl_sel_var_pop_read DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_variant,
        uname   TYPE string,
        handle1 TYPE string,
        handle2 TYPE string,
        handle3 TYPE string,
      END OF ty_s_variant.

    DATA ms_variant TYPE ty_s_variant.

    TYPES:
      BEGIN OF ty_s_variant_out,
        s_variant  TYPE ty_s_variant,
        name       TYPE string,
        descr      TYPE string,
        selkz      TYPE abap_bool,
        check_user TYPE abap_bool,
        check_def  TYPE abap_bool,
        s_db       TYPE z2ui5_cl_sel_var_db=>ty_s_db,
        t_filter   TYPE z2ui5_cl_util=>ty_t_filter_multi,
      END OF ty_s_variant_out.
    TYPES ty_t_variant_out TYPE STANDARD TABLE OF ty_s_variant_out WITH EMPTY KEY.

    DATA mt_variant      TYPE ty_t_variant_out.
    DATA mt_variant_db   TYPE z2ui5_cl_sel_var_db=>ty_t_db.

    DATA ms_variant_save TYPE ty_s_variant_out.

    CLASS-METHODS factory
      IMPORTING
*        val             TYPE z2ui5_cl_util=>ty_t_filter_multi
*        check_db_active TYPE abap_bool DEFAULT abap_true
        var_check_user  TYPE abap_bool DEFAULT abap_true
        var_handle1     TYPE clike     DEFAULT sy-repid
        var_handle2     TYPE clike     OPTIONAL
        var_handle3     TYPE clike     OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_var_pop_read.

    TYPES:
      BEGIN OF ty_s_result,
        t_filter        TYPE z2ui5_cl_util=>ty_t_filter_multi,
        s_variant       TYPE z2ui5_cl_sel_var_db=>ty_s_db,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result TYPE ty_s_result.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

    CLASS-METHODS read_default
      IMPORTING
        var_handle1   TYPE clike DEFAULT sy-repid
        var_handle2   TYPE clike OPTIONAL
        var_handle3   TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE ty_s_result.

  PROTECTED SECTION.
    DATA check_db_active TYPE abap_bool.
    DATA client          TYPE REF TO z2ui5_if_client.
    DATA mv_popup_name   TYPE LINE OF string_table.

    METHODS popup_variant_read.
    METHODS init.
    METHODS db_read.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_pop_read IMPLEMENTATION.

  METHOD db_read.

    mt_variant_db = z2ui5_cl_sel_var_db=>db_read( s_info = VALUE #( uname    = ms_variant-uname
                                                                name     = ms_variant-handle2
                                                                handle01 = ms_variant-handle1
                                              )
     ).

    CLEAR mt_variant.
    LOOP AT mt_variant_db REFERENCE INTO DATA(lr_var).
      DATA(ls_var_out) = VALUE ty_s_variant_out( ).
      ls_var_out-s_db = lr_var->*.
      INSERT ls_var_out INTO TABLE mt_variant.
    ENDLOOP.

  ENDMETHOD.

  METHOD factory.

    r_result = NEW #( ).
    r_result->ms_variant = VALUE #( uname   = COND #( WHEN var_check_user = abap_true THEN sy-uname )
                                    handle1 = var_handle1
                                    handle2 = var_handle2
                                    handle3 = var_handle3
    ).

  ENDMETHOD.

  METHOD init.

    db_read( ).
    popup_variant_read( ).

  ENDMETHOD.

  METHOD popup_variant_read.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title         = 'Variant Read'
                                  contentheight = `50%`
                                  contentwidth  = `50%`
                                  afterclose    = client->_event( 'CANCEL' ) ).

    dialog->table( mode  = 'SingleSelectLeft'
                   items = client->_bind_edit( mt_variant )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( 'Description' )->get_parent(
                    )->column( )->text( 'Default' )->get_parent(
                    )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{S_DB/NAME}'
                            )->text( '{S_DB/DESCR}'
                            )->text( '{S_DB/CHECK_DEF}' ).

    dialog->buttons(
        )->button( text  = 'Cancel'
                   icon  = 'sap-icon://sys-cancel'
                   press = client->_event( 'CANCEL' )
     )->button( text  = 'Open'
                icon  = 'sap-icon://accept'
                press = client->_event( 'CONFIRM' )
                type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD result.
    result = ms_result.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
      init( ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'CANCEL'.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `CONFIRM`.
        DATA(ls_variant) = mt_variant[ selkz = abap_true ].
        ms_result-check_confirmed = abap_true.
        ms_result-s_variant       = ls_variant-s_db.
        client->popup_destroy( ).
        client->nav_app_leave( ).

    ENDCASE.
  ENDMETHOD.

  METHOD read_default.
    TRY.

        DATA(r_result) = NEW z2ui5_cl_sel_var_pop_read( ).

        r_result->ms_variant = VALUE #(
*                                        uname   = COND #( WHEN var_check_user = abap_true THEN sy-uname )
                                        handle1 = var_handle1
                                        handle2 = var_handle2
                                        handle3 = var_handle3
        ).

        r_result->db_read( ).

        result-t_filter = r_result->mt_variant[ check_def = abap_true ]-t_filter.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
