CLASS z2ui5_cl_sel_multisel_pop DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mo_multiselect TYPE REF TO z2ui5_cl_sel_multisel.
    DATA ms_variant TYPE z2ui5_cl_sel_var_db=>ty_s_db.

    CLASS-METHODS factory_by_data
      IMPORTING
        data            TYPE any
        check_db_active TYPE abap_bool DEFAULT abap_true
        var_check_user  TYPE abap_bool DEFAULT abap_true
        var_handle1     TYPE clike     DEFAULT sy-repid
        var_handle2     TYPE clike     OPTIONAL
        var_handle3     TYPE clike     OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_multisel_pop.

    TYPES:
      BEGIN OF ty_s_result,
        t_filter        TYPE z2ui5_cl_util=>ty_t_filter_multi,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result TYPE ty_s_result.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

  PROTECTED SECTION.
    DATA client            TYPE REF TO z2ui5_if_client.

    METHODS popup_display.

    METHODS init.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_multisel_pop IMPLEMENTATION.

  METHOD factory_by_data.

    r_result = NEW #( ).

    r_result->ms_variant      = VALUE #( uname   = COND #( WHEN var_check_user = abap_true THEN sy-uname )
                                         handle01 = var_handle1
                                         handle02 = var_handle2
                                         handle03 = var_handle3
         ).

    r_result->mo_multiselect = z2ui5_cl_sel_multisel=>factory_by_data(
                   val       = data
                   check_popup = abap_true
*                      s_variant =  value #( handle01 = 'TEST' )
                 ).

  ENDMETHOD.

  METHOD init.

    popup_display( ).

  ENDMETHOD.

  METHOD popup_display.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( ).
    lo_popup = lo_popup->dialog( afterclose    = client->_event( 'BUTTON_CANCEL' )
                                 contentheight = `50%`
                                 contentwidth  = `50%`
                                 title         = 'Define Filter Conditons' ).




    mo_multiselect->set_output( client = client view = lo_popup ).


    lo_popup->buttons(
)->button( text  = 'Cancel'
    icon  = 'sap-icon://sys-cancel'
          press = client->_event( 'BUTTON_CANCEL' )
)->button( text  = 'OK'
          press = client->_event( 'BUTTON_CONFIRM' )
          type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

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

    IF mo_multiselect->main( client ).
      IF client->get( )-check_on_navigated = abap_true.
        popup_display( ).
      ENDIF.
      RETURN.
    ENDIF.


    CASE client->get( )-event.

      WHEN `BUTTON_CONFIRM`.
        ms_result-check_confirmed = abap_true.
        ms_result-t_filter = mo_multiselect->ms_result-t_filter.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `BUTTON_CANCEL`.
        client->popup_destroy( ).
        client->nav_app_leave( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
