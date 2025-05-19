CLASS z2ui5_cl_sel_sample_03 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mo_screen TYPE REF TO z2ui5_cl_sel_screen.

    DATA mv_param TYPE string.
  PROTECTED SECTION.
    METHODS view_display.
    METHODS on_navigated.
    METHODS on_event.
    DATA client TYPE REF TO z2ui5_if_client.
  PRIVATE SECTION.


ENDCLASS.



CLASS z2ui5_cl_sel_sample_03 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF client->check_on_init( ).
      mo_screen = z2ui5_cl_sel_screen=>factory( ).
      view_display( ).
      RETURN.
    ENDIF.

    IF mo_screen->main( client ).
      RETURN.
    ENDIF.

    IF client->check_on_navigated( ).
      on_navigated( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD view_display.

    DATA(page) = z2ui5_cl_xml_view=>factory( )->shell( )->page( ).

    DATA(lo_selscreen) = mo_screen->factory_selscreen( page ).
    lo_selscreen->parameters( mv_param ).

    client->view_display( page->stringify( ) ).

  ENDMETHOD.


  METHOD on_navigated.

  ENDMETHOD.


  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'ABC'.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
