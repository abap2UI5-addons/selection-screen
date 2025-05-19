CLASS z2ui5_cl_sel_screen_sel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS factory
      IMPORTING
        root TYPE REF TO z2ui5_cl_xml_view
      RETURNING
        value(r_result) TYPE REF TO z2ui5_cl_sel_screen_sel.

    DATA mo_root TYPE REF TO z2ui5_cl_xml_view.

    METHODS parameters
      IMPORTING
        v                   TYPE any
        radiobutton_group   TYPE clike OPTIONAL
        radiobutton_default TYPE abap_bool OPTIONAL
        as_checkbox         TYPE abap_bool DEFAULT abap_false
        f4                  TYPE string OPTIONAL.

    METHODS parameters_rb
      IMPORTING
        v                   TYPE any
        radiobutton_group   TYPE clike OPTIONAL
        radiobutton_default TYPE abap_bool OPTIONAL
        as_checkbox         TYPE abap_bool DEFAULT abap_false
        f4                  TYPE string OPTIONAL.

    METHODS parameters_cb
      IMPORTING
        v                   TYPE any
        radiobutton_group   TYPE clike OPTIONAL
        radiobutton_default TYPE abap_bool OPTIONAL
        as_checkbox         TYPE abap_bool DEFAULT abap_false
        f4                  TYPE string OPTIONAL.

    METHODS select_options
      IMPORTING
        v        TYPE any
        name_tmp TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_sel_screen_sel IMPLEMENTATION.

  METHOD factory.

    r_result = NEW #( ).

    r_result->mo_root = root.

  ENDMETHOD.
  METHOD parameters.

  ENDMETHOD.

  METHOD parameters_cb.

  ENDMETHOD.

  METHOD parameters_rb.

  ENDMETHOD.

  METHOD select_options.

  ENDMETHOD.

ENDCLASS.
