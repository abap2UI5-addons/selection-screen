CLASS z2ui5_cl_sel_screen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    CLASS-METHODS factory
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_sel_screen.

    METHODS main
      IMPORTING
        client        TYPE REF TO z2ui5_if_client
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS factory_selscreen
      IMPORTING
        val           TYPE REF TO z2ui5_cl_xml_view
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_sel_screen_sel.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.
    METHODS on_navigated.
    METHODS on_event.

  PRIVATE SECTION.

ENDCLASS.



CLASS z2ui5_cl_sel_screen IMPLEMENTATION.

  METHOD factory.

  ENDMETHOD.


  METHOD main.
    me->client = client.

    IF client->check_on_navigated( ).
      on_navigated( ).
    ELSE.
      on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_navigated.

  ENDMETHOD.


  METHOD on_event.

  ENDMETHOD.

  METHOD factory_selscreen.

    result = z2ui5_cl_sel_screen_sel=>factory( val ).

  ENDMETHOD.

ENDCLASS.
