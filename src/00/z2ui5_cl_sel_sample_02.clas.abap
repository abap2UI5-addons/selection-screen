CLASS z2ui5_cl_sel_sample_02 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_tab,
        selkz            TYPE abap_bool,
        product          TYPE string,
        create_date      TYPE string,
        create_by        TYPE string,
        storage_location TYPE string,
        quantity         TYPE i,
      END OF ty_s_tab.
    TYPES ty_t_table TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

    DATA mt_table TYPE ty_t_table.
    DATA mo_variant TYPE REF TO z2ui5_cl_sel_multisel_pop.
*    DATA mt_filter TYPE z2ui5_cl_util=>ty_t_filter_multi.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA mv_check_initialized TYPE abap_bool.
    METHODS on_event.
    METHODS view_display.
    METHODS set_data.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_sample_02 IMPLEMENTATION.


  METHOD on_event.

    CASE client->get( )-event.
      WHEN `BUTTON_START`.
        set_data( ).
        client->view_model_update( ).
      WHEN `PREVIEW_FILTER`.
        client->nav_app_call( mo_variant ).
*        z2ui5_cl_sel_multisel_pop=>factory(
*                                val             = mt_filter
**                                check_db_active = abap_true
**                                var_check_user  = abap_true
*                                var_handle1     = 'TEST_POP'
**                                var_handle2     =
**                                var_handle3     =
*                              ) ).
      WHEN 'BACK'.
        client->nav_app_leave( ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_data.

    mt_table = VALUE #(
        ( product = 'table'    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = 'chair'    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = 'sofa'     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = 'computer' create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = 'oven'     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
        ( product = 'table2'   create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 ) ).

    z2ui5_cl_util=>filter_itab(
      EXPORTING
       filter = mo_variant->mo_multiselect->ms_result-t_filter
      CHANGING
        val   = mt_table ).

  ENDMETHOD.


  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    view = view->shell( )->page( id = `page_main`
             title          = 'abap2UI5 - Select-Options'
             navbuttonpress = client->_event( 'BACK' )
             shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
          ).

    DATA(vbox) = view->vbox( ).

    DATA(tab) = vbox->table(
        items = client->_bind( val = mt_table )
           )->header_toolbar(
             )->overflow_toolbar(
                 )->toolbar_spacer(
                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
           )->button(  text = `Go` press = client->_event( `BUTTON_START` ) type = `Emphasized`
            )->get_parent( )->get_parent( ).

    DATA(lo_columns) = tab->columns( ).
    lo_columns->column( )->text( text = `Product` ).
    lo_columns->column( )->text( text = `Date` ).
    lo_columns->column( )->text( text = `Name` ).
    lo_columns->column( )->text( text = `Location` ).
    lo_columns->column( )->text( text = `Quantity` ).

    DATA(lo_cells) = tab->items( )->column_list_item( ).
    lo_cells->text( `{PRODUCT}` ).
    lo_cells->text( `{CREATE_DATE}` ).
    lo_cells->text( `{CREATE_BY}` ).
    lo_cells->text( `{STORAGE_LOCATION}` ).
    lo_cells->text( `{QUANTITY}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).

      mo_variant = z2ui5_cl_sel_multisel_pop=>factory_by_data(
        data             = mt_table
        var_handle1     = 'TEST_POP'
                            ).
      set_data( ).
      view_display( ).
      RETURN.
    ENDIF.

    IF client->check_on_navigated( ).
      TRY.
          DATA(lo_value_help) = CAST z2ui5_cl_sel_multisel_pop( client->get_app_prev( ) ).
          IF lo_value_help->result( )-check_confirmed = abap_true.
            mo_variant->mo_multiselect->ms_result-t_filter = lo_value_help->result( )-t_filter.
            set_data( ).
            client->view_model_update( ).
          ENDIF.
        CATCH cx_root.
      ENDTRY.
      RETURN.
    ENDIF.

    IF client->get( )-event IS NOT INITIAL.
      on_event( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
