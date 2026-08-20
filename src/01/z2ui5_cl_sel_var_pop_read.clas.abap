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
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS popup_variant_read.
    METHODS init.
    METHODS db_read.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_pop_read IMPLEMENTATION.

  METHOD db_read.

    mt_variant_db = z2ui5_cl_sel_var_db=>db_read( s_info = VALUE #( uname    = ms_variant-uname
                                                                    handle01 = ms_variant-handle1
                                                                    handle02 = ms_variant-handle2 ) ).

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
                                    handle3 = var_handle3 ).

  ENDMETHOD.

  METHOD init.

    db_read( ).
    popup_variant_read( ).

  ENDMETHOD.

  METHOD popup_variant_read.

    DATA(popup) = z2ui5_cl_ui5_view_builder=>factory( 
                      )->ele( n = `FragmentDefinition` ns = `core` 
                      )->a( n = `xmlns` v = `sap.m` 
                      )->a( n = `xmlns:core` v = `sap.ui.core` ).

    DATA(dialog) = popup->ele( `Dialog` 
                       )->a( n = `title` v = `Variant Read` 
                       )->a( n = `contentHeight` v = `50%` 
                       )->a( n = `contentWidth` v = `50%` 
                       )->a( n = `afterClose` v = client->_event( `CANCEL` ) ).

    dialog->ele( `Table` 
        )->a( n = `mode` v = `SingleSelectLeft` 
        )->a( n = `items` v = client->_bind_edit( mt_variant ) 
        )->ele( `columns` 
        )->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Layout` 
        )->end( 
        )->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Description` 
        )->end( 
        )->ele( `Column` 
        )->tag( `Text` 
        )->a( n = `text` v = `Default` 
        )->end( 
        )->end( 
        )->ele( `items` 
        )->ele( `ColumnListItem` 
        )->a( n = `selected` v = `{SELKZ}` 
        )->ele( `cells` 
        )->tag( `Text` 
        )->a( n = `text` v = `{S_DB/NAME}` 
        )->tag( `Text` 
        )->a( n = `text` v = `{S_DB/DESCR}` 
        )->tag( `Text` 
        )->a( n = `text` v = `{S_DB/CHECK_DEF}` ).

    dialog->ele( `buttons` 
        )->tag( `Button` 
        )->a( n = `text` v = `Cancel` 
        )->a( n = `icon` v = `sap-icon://sys-cancel` 
        )->a( n = `press` v = client->_event( `CANCEL` ) 
        )->tag( `Button` 
        )->a( n = `text` v = `Open` 
        )->a( n = `icon` v = `sap-icon://accept` 
        )->a( n = `press` v = client->_event( `CONFIRM` ) 
        )->a( n = `type` v = `Emphasized` ).

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

      WHEN `CANCEL`.
        client->popup_destroy( ).
        client->nav_app_leave( ).

      WHEN `CONFIRM`.
        IF NOT line_exists( mt_variant[ selkz = abap_true ] ).
          client->message_toast_display( `Select a variant first` ).
          RETURN.
        ENDIF.
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

        r_result->ms_variant = VALUE #( handle1 = var_handle1
                                        handle2 = var_handle2
                                        handle3 = var_handle3 ).

        r_result->db_read( ).

        result-t_filter = r_result->mt_variant[ check_def = abap_true ]-t_filter.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
