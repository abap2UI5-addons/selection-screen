CLASS z2ui5_cl_layo_selscreen DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

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
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_layo_selscreen.

    CLASS-METHODS factory_by_name
      IMPORTING
        val             TYPE clike
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_layo_selscreen.

    CLASS-METHODS factory_by_data
      IMPORTING
        val             TYPE any
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_cl_layo_selscreen.

    TYPES:
      BEGIN OF ty_s_result,
        tab_name        TYPE string,
        var_name        TYPE string,
        t_filter        TYPE z2ui5_cl_util=>ty_t_filter_multi,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result TYPE ty_s_result.
    DATA mv_popup_name TYPE string.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

    METHODS set_output
      IMPORTING
        client TYPE REF TO z2ui5_if_client
        view    TYPE REF TO z2ui5_cl_xml_view.

    METHODS main
      IMPORTING
        client      TYPE REF TO z2ui5_if_client
      RETURNING
        VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_load_id TYPE string.

ENDCLASS.



CLASS z2ui5_cl_layo_selscreen IMPLEMENTATION.

  METHOD factory_by_filter.

    r_result = NEW #( ).
    r_result->ms_result-t_filter = val.

    LOOP AT r_result->ms_result-t_filter REFERENCE INTO DATA(lr_filter).
      INSERT VALUE #(
            name = lr_filter->name
            visible = abap_true
      ) INTO TABLE r_result->mt_component .
    ENDLOOP.

  ENDMETHOD.

  METHOD factory_by_data.

    r_result = factory_by_filter( z2ui5_cl_util=>filter_get_multi_by_data( val ) ).

  ENDMETHOD.



  METHOD result.
    result = ms_result.
  ENDMETHOD.


  METHOD set_output.

*    DATA(vbox) = view->vbox( height         = `100%`
*                                 justifycontent = 'SpaceBetween' ).

*    DATA(item) = view->list( "nodata          = `no conditions defined`
*                             items           = client2->_bind( t_filter )
*                             selectionchange = client2->_event( 'SELCHANGE' )
*                )->custom_list_item( ).

    DATA(tab) = view->table( "nodata          = `no conditions defined`
                             items           = client->_bind( ms_result-t_filter )

                             selectionchange = client->_event( 'SELCHANGE' )
                ).


*                ->custom_list_item( ).
    tab->header_toolbar(
         )->toolbar(
*             )->input(
*         value = client->_bind_edit( mv_tabname )
*         description = `Tablename`
      )->title( ms_result-tab_name
      )->toolbar_spacer(
        )->button( text  = `Clear`
                   icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   press = client->_event( val = `DELETE_ALL` )
*       )->button( text  = 'View'
*                    icon  = 'sap-icon://edit'
*                  press = client2->_event( 'BUTTON_CANCEL' )
        )->button( text  = 'Load'
                    icon  = 'sap-icon://download-from-cloud'
                  press = client->_event( 'BUTTON_LOAD' )
*                  type  = 'Emphasized'
       )->button( text  = 'Save'
                    icon  = 'sap-icon://save'
                  press = client->_event( 'BUTTON_SAVE' )
*                  type  = 'Emphasized'
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
    DATA(cells) =  tab->items( )->column_list_item( )->cells( ).
    cells->text( text = `{NAME}` ).
    cells->multi_input( tokens = `{T_TOKEN}`
     enabled               = abap_false
*     valueHelpOnly = abap_true
          valuehelprequest = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) )
         )->tokens(
              )->token( key      = `{KEY}`
                        text     = `{TEXT}`
                        visible  = `{VISIBLE}`
                        selected = `{SELKZ}`
                        editable = `{EDITABLE}` ).
    cells->button( text  = `Select`
               press = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
    cells->button( icon  = 'sap-icon://delete'
               type  = `Transparent`
               text  = `Clear`
               press = client->_event( val = `LIST_DELETE` t_arg = VALUE #( ( `${NAME}` ) ) )
     ).


*    data(vl) = item->horizontal_layout(
*      EXPORTING
*        class   = 'sapUiContentPadding equalColumns'
*        width   = '100%'
*        enabled =
*        visible =
*        id      =
*      RECEIVING
*        result  =
*    ).
*    DATA(grid) = item->grid( class = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
*    DATA(grid) = item. "->hbox( ). "->flex_box( alignItems = 'Start' ). "( class = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
*    grid->text( text = `{NAME}` width = `20%` ).

*    grid->multi_input( tokens = `{T_TOKEN}`
*        enabled               = abap_false
*             valuehelprequest = client2->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) )
*            )->tokens(
*                 )->token( key      = `{KEY}`
*                           text     = `{TEXT}`
*                           visible  = `{VISIBLE}`
*                           selected = `{SELKZ}`
*                           editable = `{EDITABLE}` ).
*
*    grid->button( text  = `Select`
*                  press = client2->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
*    grid->button( icon  = 'sap-icon://delete'
*                  type  = `Transparent`
*                  text  = `Clear`
*                  press = client2->_event( val = `LIST_DELETE` t_arg = VALUE #( ( `${NAME}` ) ) ) ).



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
          DATA(ls_value) = CAST z2ui5_cl_pop_input_val( client->get_app_prev( )  )->result( ).
          IF ls_value-check_confirmed = abap_true.
            ms_result-var_name = ls_value-value.

            IF mv_load_id IS NOT INITIAL.

              SELECT SINGLE *
              FROM z2ui5_t_13
              WHERE  name = @ms_result-var_name
              INTO @DATA(ls_db).
              z2ui5_cl_util=>xml_parse(
                EXPORTING
                  xml = ls_db-data
                IMPORTING
                  any = ms_result
              ).
              client->view_model_update( ).

            ELSE.

              MODIFY z2ui5_t_13 FROM @( VALUE #(
                  name = ms_result-var_name
                  data = z2ui5_cl_util=>xml_stringify( ms_result )
              ) ).
              commit work and wait.


            ENDIF.
            RETURN.
          ENDIF.

        CATCH cx_root.
      ENDTRY.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'LIST_OPEN'.
        result = abap_true.
        mv_popup_name = client->get_event_arg( 1 ).
        DATA(ls_sql) = ms_result-t_filter[ name = client->get_event_arg( 1 ) ].
        client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).

      WHEN 'LIST_DELETE'.
        result = abap_true.
        mv_popup_name = client->get_event_arg( 1 ).
        CLEAR ms_result-t_filter[ name = mv_popup_name ]-t_range.
        CLEAR ms_result-t_filter[ name = mv_popup_name ]-t_token.
        client->view_model_update( ).

      WHEN `BUTTON_LOAD`.
        mv_load_id = client->nav_app_call( z2ui5_cl_pop_input_val=>factory( ) ).

      WHEN `BUTTON_SAVE`.
        client->nav_app_call( z2ui5_cl_pop_input_val=>factory( ) ).

      WHEN `BUTTON_CONFIRM`.
        result = abap_true.
        ms_result-check_confirmed = abap_true.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `BUTTON_CANCEL`.
        result = abap_true.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `DELETE_ALL`.
        result = abap_true.
        LOOP AT ms_result-t_filter REFERENCE INTO DATA(lr_sql).
          CLEAR lr_sql->t_token.
          CLEAR lr_sql->t_range.
        ENDLOOP.
        client->view_model_update( ).

      WHEN OTHERS.
        result = abap_false.

    ENDCASE.

  ENDMETHOD.

  METHOD factory_by_name.

    r_result = factory_by_data( z2ui5_cl_util=>rtti_create_tab_by_name( val ) ).
    r_result->ms_result-tab_name = val.

  ENDMETHOD.

ENDCLASS.
