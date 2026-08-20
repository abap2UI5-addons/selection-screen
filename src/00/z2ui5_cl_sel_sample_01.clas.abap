CLASS z2ui5_cl_sel_sample_01 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_tabname     TYPE string VALUE `T100`.
    DATA mr_table       TYPE REF TO data.
    DATA mo_multiselect TYPE REF TO z2ui5_cl_sel_multisel.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA mv_expanded TYPE abap_bool VALUE abap_true.

    METHODS on_event.
    METHODS view_display.
    METHODS set_data.
    METHODS on_init.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_sel_sample_01 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `BUTTON_START`.
        set_data( ).
        client->view_model_update( ).

      WHEN `BACK`.
        client->nav_app_leave( ).

    ENDCASE.

  ENDMETHOD.

  METHOD set_data.

    DATA(lv_where) = z2ui5_cl_util=>filter_get_sql_where( mo_multiselect->ms_result-t_filter ).
    SELECT FROM (mv_tabname)
     FIELDS
       *
      WHERE (lv_where)
     INTO TABLE @mr_table->*
     UP TO 100 ROWS.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory( 
                     )->ele( n = `View` ns = `mvc` 
                     )->a( n = `xmlns` v = `sap.m` 
                     )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc` 
                     )->a( n = `xmlns:core` v = `sap.ui.core` 
                     )->a( n = `displayBlock` v = `true` 
                     )->a( n = `height` v = `100%` ).

    view = view->ele( `Shell` 
               )->ele( `Page` 
               )->a( n = `id` v = `page_main` 
               )->a( n = `title` v = `abap2UI5 - Select-Options` 
               )->a( n = `navButtonPress` v = client->_event( `BACK` ) 
               )->a( n = `showNavButton` b = client->check_app_prev_stack( ) ).

    DATA(vbox) = view->ele( `VBox` ).

    DATA(lo_panel) = vbox->ele( `Panel` 
                         )->a( n = `expandable` b = abap_true 
                         )->a( n = `expanded` v = client->_bind_edit( mv_expanded ) 
                         )->a( n = `headerText` v = `Selection Screen` ).

    mo_multiselect->set_output( client = client
                                view   = lo_panel ).

    ASSIGN mr_table->* TO FIELD-SYMBOL(<table>).
    DATA(tab) = vbox->ele( `Table` 
                    )->a( n = `items` v = client->_bind( <table> ) 
                    )->ele( `headerToolbar` 
                    )->ele( `OverflowToolbar` 
                    )->tag( `ToolbarSpacer` 
                    )->tag( `Button` 
                    )->a( n = `text` v = `Go` 
                    )->a( n = `press` v = client->_event( `BUTTON_START` ) 
                    )->a( n = `type` v = `Emphasized` 
                    )->end( 
                    )->end( ).

    DATA(lo_columns) = tab->ele( `columns` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `SPRSL` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `ARBGB` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `MSGNR` ).
    lo_columns->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `TEXT` ).

    DATA(lo_cells) = tab->ele( `items` 
                         )->ele( `ColumnListItem` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{SPRSL}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{ARBGB}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{MSGNR}` ).
    lo_cells->tag( `Text` 
        )->a( n = `text` v = `{TEXT}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF client->check_on_init( ).
      on_init( ).
      RETURN.
    ENDIF.

    IF mo_multiselect->main( client ).
      RETURN.
    ENDIF.

    IF client->get( )-event IS NOT INITIAL.
      on_event( ).
    ENDIF.

  ENDMETHOD.

  METHOD on_init.

    mr_table = z2ui5_cl_util=>rtti_create_tab_by_name( mv_tabname ).
    mo_multiselect = z2ui5_cl_sel_multisel=>factory_by_name( val       = mv_tabname
                                                             s_variant = VALUE #( handle01 = `TEST` ) ).

    view_display( ).

  ENDMETHOD.

ENDCLASS.
