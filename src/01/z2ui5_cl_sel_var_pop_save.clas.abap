CLASS z2ui5_cl_sel_var_pop_save DEFINITION
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
        s_variant     TYPE ty_s_variant,
        name          TYPE string,
        description   TYPE string,
        selkz         TYPE abap_bool,
        check_user    TYPE abap_bool,
        check_default TYPE abap_bool,
        t_filter      TYPE z2ui5_cl_util=>ty_t_filter_multi,
      END OF ty_s_variant_out.
    TYPES ty_t_variant_out TYPE STANDARD TABLE OF ty_s_variant_out WITH EMPTY KEY.

    DATA mt_variant      TYPE ty_t_variant_out.

    DATA ms_variant_save TYPE ty_s_variant_out.

    DATA s_variant       TYPE z2ui5_cl_sel_multisel=>ty_s_result.

    CLASS-METHODS factory
      IMPORTING
        val             TYPE z2ui5_cl_sel_multisel=>ty_s_result
*        check_db_active TYPE abap_bool DEFAULT abap_true
        var_check_user  TYPE abap_bool DEFAULT abap_true
        var_handle1     TYPE clike     DEFAULT sy-repid
        var_handle2     TYPE clike     OPTIONAL
        var_handle3     TYPE clike     OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_var_pop_save.

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
    DATA check_db_active   TYPE abap_bool.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    DATA mv_popup_name     TYPE LINE OF string_table.

    METHODS popup_variant_save.
    METHODS init.
    METHODS save_variant.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_pop_save IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).
    r_result->s_variant  = val.
*    r_result->check_db_active = check_db_active.

    r_result->ms_variant = VALUE #( uname   = COND #( WHEN var_check_user = abap_true THEN sy-uname )
                                    handle1 = var_handle1
                                    handle2 = var_handle2
                                    handle3 = var_handle3
    ).

  ENDMETHOD.

  METHOD init.

    popup_variant_save( ).

  ENDMETHOD.

  METHOD popup_variant_save.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title         = 'Variant Save'
                                  contentheight = `50%`
                                  contentwidth  = `50%`
                                  afterclose    = client->_event( 'DB_SAVE_CLOSE' ) ).

    DATA(form) = dialog->simple_form( editable        = abap_true
                                      labelspanxl     = `4`
                                      labelspanl      = `4`
                                      labelspanm      = `4`
                                      labelspans      = `4`
                                      adjustlabelspan = abap_false
                                      ).

    form->toolbar( )->title( 'Layout' ).

    form->content( 'form'
                           )->label( 'Layout'
                           )->input( client->_bind_edit( ms_variant_save-name )
                           )->label( 'Description'
                           )->input( client->_bind_edit( ms_variant_save-description ) ).

    form->toolbar( )->title( `` ).

    form->content( 'form'
                           )->label( 'Default Layout'
                           )->switch( type  = 'AcceptReject'
                                      state = client->_bind_edit( ms_variant_save-check_default )
                           )->label( 'User specific'
                           )->switch( type  = 'AcceptReject'
                                      state = client->_bind_edit( ms_variant_save-check_user )
                           ).

    dialog->buttons(
        )->button( text  = 'Cancel'
                   icon  = 'sap-icon://sys-cancel'
                   press = client->_event( 'DB_SAVE_CLOSE' )
     )->button( text  = 'Save'
                press = client->_event( 'DB_SAVE' )
                type  = 'Success'
                icon  = 'sap-icon://save' ).

    client->popup_display( popup->get_root( )->xml_get( ) ).

  ENDMETHOD.

  METHOD result.
    result = ms_result.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      init( ).
      RETURN.
    ENDIF.

    CASE client->get( )-event.

      WHEN `DB_SAVE_CLOSE`.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `DB_SAVE`.
        save_variant( ).
        ms_result-check_confirmed = abap_true.
        client->popup_destroy( ).
        client->nav_app_leave( ).

    ENDCASE.
  ENDMETHOD.

  METHOD save_variant.

    ms_variant_save-t_filter = ms_result-t_filter.
    INSERT ms_variant_save INTO TABLE mt_variant.

    z2ui5_cl_sel_var_db=>db_save( s_info = VALUE #( uname     = ms_variant-uname
                                                    name      = ms_variant_save-name
                                                    descr     = ms_variant_save-description
                                                    check_def = ms_variant_save-check_default
                                                    check_usr = ms_variant_save-check_default
                                                    handle01  = ms_variant-handle1
*                                                    handle02  =
*                                                    handle03  = ms_variant-handle3
                                ) data   = s_variant
).



  ENDMETHOD.


ENDCLASS.
