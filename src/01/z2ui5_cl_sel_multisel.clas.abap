CLASS z2ui5_cl_sel_multisel DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    DATA mv_check_popup TYPE abap_bool.

    DATA ms_variant TYPE z2ui5_cl_sel_var_db=>ty_s_db.

    TYPES:
      BEGIN OF ty_s_result,
        tab_name        TYPE string,
        var_name        TYPE string,
        t_filter        TYPE z2ui5_cl_util=>ty_t_filter_multi,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result     TYPE ty_s_result.
    DATA mv_popup_name TYPE string.

    TYPES:
      BEGIN OF ty_s_component,
        name             TYPE string,
        visible          TYPE abap_bool,
        check_value_help TYPE abap_bool,
      END OF ty_S_component.

    DATA mt_component TYPE STANDARD TABLE OF ty_S_component WITH EMPTY KEY.

    CLASS-METHODS factory_by_filter
      IMPORTING
        val             TYPE z2ui5_cl_util=>ty_t_filter_multi
        check_popup     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_multisel.

    CLASS-METHODS factory_by_name
      IMPORTING
        val             TYPE clike
        s_variant       TYPE z2ui5_cl_sel_var_db=>ty_s_db OPTIONAL
*        report          type clike DEFAULT sy-repid
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_multisel.

    CLASS-METHODS factory_by_data
      IMPORTING
        val             TYPE any
        check_popup     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_sel_multisel.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

    METHODS set_output
      IMPORTING
        client TYPE REF TO z2ui5_if_client
        view   TYPE REF TO z2ui5_cl_xml_view.

    METHODS main
      IMPORTING
        client        TYPE REF TO z2ui5_if_client
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS set_var_default.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_sel_multisel IMPLEMENTATION.

  METHOD factory_by_filter.

    r_result = NEW #( ).
    r_result->mv_check_popup = check_popup.
    r_result->ms_result-t_filter = val.

    LOOP AT r_result->ms_result-t_filter REFERENCE INTO DATA(lr_filter).
      INSERT VALUE #( name    = lr_filter->name
                      visible = abap_true
      ) INTO TABLE r_result->mt_component.
    ENDLOOP.

  ENDMETHOD.

  METHOD factory_by_data.

    r_result = factory_by_filter( z2ui5_cl_util=>filter_get_multi_by_data( val ) ).
    r_result->mv_check_popup = check_popup.
    r_result->set_var_default( ).

  ENDMETHOD.

  METHOD result.
    result = ms_result.
  ENDMETHOD.

  METHOD set_output.

    DATA(tab) = view->table( " nodata          = `no conditions defined`
                             items           = client->_bind( ms_result-t_filter )

                             selectionchange = client->_event( 'SELCHANGE' )
                ).

    tab->header_toolbar(
         )->toolbar(
      )->title( ms_result-tab_name
      )->toolbar_spacer(
        )->button( text  = `Clear`
                   icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   press = client->_event( val = `DELETE_ALL` )
        )->button( text  = 'Load'
                   icon  = 'sap-icon://download-from-cloud'
                   press = client->_event( 'BUTTON_LOAD' )
*                   type  = 'Emphasized'
       )->button( text  = 'Save'
                  icon  = 'sap-icon://save'
                  press = client->_event( 'BUTTON_SAVE' )
                  ).

    tab->columns(
         )->column(
             )->text( 'Name' )->get_parent(
         )->column(
             )->text( 'Options' )->get_parent(
         )->column(
             )->text( 'Select' )->get_parent(
         )->column(
             )->text( 'Clear' )->get_parent(
              ).
    DATA(cells) = tab->items( )->column_list_item( )->cells( ).
    cells->text( text = `{NAME}` ).
    cells->multi_input( tokens           = `{T_TOKEN}`
                        enabled          = abap_false
*                        valueHelpOnly    = abap_true
                        valuehelprequest = client->_event( val   = `LIST_OPEN`
                                                           t_arg = VALUE #( ( `${NAME}` ) ) )
         )->tokens(
              )->token( key      = `{KEY}`
                        text     = `{TEXT}`
                        visible  = `{VISIBLE}`
                        selected = `{SELKZ}`
                        editable = `{EDITABLE}` ).
    cells->button( text  = `Select`
                   press = client->_event( val   = `LIST_OPEN`
                                           t_arg = VALUE #( ( `${NAME}` ) ) ) ).
    cells->button( icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   text  = `Clear`
                   press = client->_event( val   = `LIST_DELETE`
                                           t_arg = VALUE #( ( `${NAME}` ) ) )
     ).

  ENDMETHOD.

  METHOD main.

    result = abap_true.

    IF client->check_on_navigated( ).
      TRY.
          DATA(lo_popup) = CAST z2ui5_cl_pop_get_range( client->get_app_prev( ) ).
          IF lo_popup->result( )-check_confirmed = abap_true.
            FIELD-SYMBOLS <tab> TYPE z2ui5_cl_util=>ty_s_filter_multi.
            ASSIGN ms_result-t_filter[ name = mv_popup_name ] TO <tab>.
            <tab>-t_range = lo_popup->result( )-t_range.
            <tab>-t_token = z2ui5_cl_util=>filter_get_token_t_by_range_t( <tab>-t_range ).
            client->view_model_update( ).
            RETURN.
          ENDIF.
        CATCH cx_root.
      ENDTRY.

      TRY.
          DATA(ls_value) = CAST z2ui5_cl_sel_var_pop_read( client->get_app_prev( ) )->result( ).
          IF ls_value-check_confirmed = abap_true.

            z2ui5_cl_util=>xml_parse( EXPORTING xml = ls_value-s_variant-data
                                      IMPORTING any = ms_result
            ).
            IF mv_check_popup = abap_false.
              client->view_model_update( ).
            ELSE.
              client->popup_model_update( ).
            ENDIF.

          ENDIF.
          RETURN.
        CATCH cx_root.
      ENDTRY.

      TRY.
          DATA(ls_value2) = CAST z2ui5_cl_sel_var_pop_save( client->get_app_prev( ) )->result( ).
          IF ls_value2-check_confirmed = abap_true.

          ENDIF.
          RETURN.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'LIST_OPEN'.
        result = abap_true.
        mv_popup_name = client->get_event_arg( 1 ).
        DATA(ls_sql) = ms_result-t_filter[ name = client->get_event_arg( 1 ) ].
        client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).
        "
      WHEN 'LIST_DELETE'.
        result = abap_true.
        mv_popup_name = client->get_event_arg( 1 ).
        CLEAR ms_result-t_filter[ name = mv_popup_name ]-t_range.
        CLEAR ms_result-t_filter[ name = mv_popup_name ]-t_token.
        IF mv_check_popup = abap_false.
          client->view_model_update( ).
        ELSE.
          client->popup_model_update( ).
        ENDIF.

      WHEN `BUTTON_LOAD`.
        DATA(lo_popup3) = z2ui5_cl_sel_var_pop_read=>factory( var_check_user = abap_true
                                                              var_handle1    = ms_variant-handle01
                                                              var_handle2    = ms_variant-handle02
      ).
        client->nav_app_call( lo_popup3 ).

      WHEN `BUTTON_SAVE`.
        DATA(lo_popup4) = z2ui5_cl_sel_var_pop_save=>factory( val            = ms_result
                                                              var_check_user = abap_true
                                                              var_handle1    = ms_variant-handle01
                                                              var_handle2    = ms_variant-handle02
       ).
        client->nav_app_call( lo_popup4 ).

      WHEN `DELETE_ALL`.
        result = abap_true.
        LOOP AT ms_result-t_filter REFERENCE INTO DATA(lr_sql).
          CLEAR lr_sql->t_token.
          CLEAR lr_sql->t_range.
        ENDLOOP.
        IF mv_check_popup = abap_false.
          client->view_model_update( ).
        ELSE.
          client->popup_model_update( ).
        ENDIF.

      WHEN OTHERS.
        result = abap_false.

    ENDCASE.

  ENDMETHOD.

  METHOD factory_by_name.

    r_result = factory_by_data( z2ui5_cl_util=>rtti_create_tab_by_name( val ) ).
    r_result->ms_result-tab_name = val.
    r_result->ms_variant = s_variant.
    r_result->set_var_default( ).

  ENDMETHOD.

  METHOD set_var_default.

    DATA(ls_default) = z2ui5_cl_sel_var_db=>db_read_default( ms_variant ).

    z2ui5_cl_util=>xml_parse( EXPORTING xml = ls_default-data
                              IMPORTING any = ms_result
   ).

  ENDMETHOD.

ENDCLASS.
