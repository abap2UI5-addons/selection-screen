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
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS popup_variant_save.
    METHODS init.
    METHODS save_variant.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_pop_save IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).
    r_result->s_variant = val.

    r_result->ms_variant = VALUE #( uname   = COND #( WHEN var_check_user = abap_true THEN sy-uname )
                                    handle1 = var_handle1
                                    handle2 = var_handle2
                                    handle3 = var_handle3 ).

  ENDMETHOD.

  METHOD init.

    popup_variant_save( ).

  ENDMETHOD.

  METHOD popup_variant_save.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory( 
                      )->ele( n = `FragmentDefinition` ns = `core` 
                      )->a( n = `xmlns` v = `sap.m` 
                      )->a( n = `xmlns:core` v = `sap.ui.core` 
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).

    DATA(dialog) = popup->ele( `Dialog` 
                       )->a( n = `title` v = `Variant Save` 
                       )->a( n = `contentHeight` v = `50%` 
                       )->a( n = `contentWidth` v = `50%` 
                       )->a( n = `afterClose` v = client->_event( `DB_SAVE_CLOSE` ) ).

    DATA(form) = dialog->ele( n = `SimpleForm` ns = `form` 
                     )->a( n = `editable` b = abap_true 
                     )->a( n = `labelSpanXL` v = `4` 
                     )->a( n = `labelSpanL` v = `4` 
                     )->a( n = `labelSpanM` v = `4` 
                     )->a( n = `labelSpanS` v = `4` 
                     )->a( n = `adjustLabelSpan` b = abap_false ).

    form->ele( `Toolbar` 
        )->tag( `Title` 
        )->a( n = `text` v = `Layout` ).

    form->ele( n = `content` ns = `form` 
        )->tag( `Label` 
        )->a( n = `text` v = `Layout` 
        )->tag( `Input` 
        )->a( n = `value` v = client->_bind_edit( ms_variant_save-name ) 
        )->tag( `Label` 
        )->a( n = `text` v = `Description` 
        )->tag( `Input` 
        )->a( n = `value` v = client->_bind_edit( ms_variant_save-description ) ).

    form->ele( `Toolbar` 
        )->tag( `Title` 
        )->a( n = `text` v = `` ).

    form->ele( n = `content` ns = `form` 
        )->tag( `Label` 
        )->a( n = `text` v = `Default Layout` 
        )->tag( `Switch` 
        )->a( n = `type` v = `AcceptReject` 
        )->a( n = `state` v = client->_bind_edit( ms_variant_save-check_default ) 
        )->tag( `Label` 
        )->a( n = `text` v = `User specific` 
        )->tag( `Switch` 
        )->a( n = `type` v = `AcceptReject` 
        )->a( n = `state` v = client->_bind_edit( ms_variant_save-check_user ) ).

    dialog->ele( `buttons` 
        )->tag( `Button` 
        )->a( n = `text` v = `Cancel` 
        )->a( n = `icon` v = `sap-icon://sys-cancel` 
        )->a( n = `press` v = client->_event( `DB_SAVE_CLOSE` ) 
        )->tag( `Button` 
        )->a( n = `text` v = `Save` 
        )->a( n = `press` v = client->_event( `DB_SAVE` ) 
        )->a( n = `type` v = `Success` 
        )->a( n = `icon` v = `sap-icon://save` ).

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
                                                    check_usr = ms_variant_save-check_user
                                                    handle01  = ms_variant-handle1
                                                    handle02  = ms_variant-handle2
                                                    handle03  = ms_variant-handle3 )
                                  data   = s_variant ).

  ENDMETHOD.

ENDCLASS.
