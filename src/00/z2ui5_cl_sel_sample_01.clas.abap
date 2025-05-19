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

      WHEN 'BACK'.
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

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    view = view->shell( )->page( id             = `page_main`
                                 title          = 'abap2UI5 - Select-Options'
                                 navbuttonpress = client->_event( 'BACK' )
                                 shownavbutton  = client->check_app_prev_stack( ) ).

    DATA(vbox) = view->vbox( ).

    DATA(lo_panel) = vbox->panel(
         expandable = abap_true
         expanded         = client->_bind_edit( mv_expanded )
         headertext       = |Selection Screen|
    ).

    mo_multiselect->set_output( client = client view = lo_panel ).

    ASSIGN mr_table->* TO FIELD-SYMBOL(<table>).
    DATA(tab) = vbox->table( client->_bind( <table> )
           )->header_toolbar(
             )->overflow_toolbar(
                 )->toolbar_spacer(
               )->button( text  = `Go`
                      press = client->_event( `BUTTON_START` )
                      type  = `Emphasized`
            )->get_parent( )->get_parent( ).

    DATA(lo_columns) = tab->columns( ).
    lo_columns->column( )->text( text = `SPRSL` ).
    lo_columns->column( )->text( text = `ARBGB` ).
    lo_columns->column( )->text( text = `MSGNR` ).
    lo_columns->column( )->text( text = `TEXT` ).

    DATA(lo_cells) = tab->items( )->column_list_item( ).
    lo_cells->text( `{SPRSL}` ).
    lo_cells->text( `{ARBGB}` ).
    lo_cells->text( `{MSGNR}` ).
    lo_cells->text( `{TEXT}` ).

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
    mo_multiselect = z2ui5_cl_sel_multisel=>factory_by_name(
                       val       = mv_tabname
                      s_variant =  value #( handle01 = 'TEST' )
                     ).

    view_display( ).

  ENDMETHOD.

ENDCLASS.
