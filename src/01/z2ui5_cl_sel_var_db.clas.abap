CLASS z2ui5_cl_sel_var_db DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS factory
      IMPORTING
        report        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_sel_var_db.

    TYPES ty_s_db TYPE z2ui5_t_13.
*    TYPES ty_s_pos TYPE z2ui5_t_14.

    DATA mt_variant TYPE STANDARD TABLE OF ty_s_db WITH EMPTY KEY.

    METHODS check_default.
    METHODS get_default.

    TYPES ty_t_db TYPE STANDARD TABLE OF z2ui5_t_13 WITH EMPTY KEY.

    METHODS db_variant_read
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE z2ui5_cl_util=>ty_t_filter_multi.

    METHODS db_variant_save
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE z2ui5_cl_util=>ty_t_filter_multi.

    CLASS-METHODS db_read_default
      IMPORTING
        s_info        TYPE z2ui5_t_13
      RETURNING
        VALUE(result) TYPE ty_s_db.

    CLASS-METHODS db_read
      IMPORTING
        s_info        TYPE z2ui5_t_13
      RETURNING
        VALUE(result) TYPE ty_t_db.

    CLASS-METHODS db_save
      IMPORTING
        s_info TYPE z2ui5_t_13
        data   TYPE data.

  PROTECTED SECTION.
    DATA:
      BEGIN OF mS_config,
        classname TYPE string,
        name      TYPE string,
        user      TYPE string,
        timestamp TYPE timestamp,
        data      TYPE string,
      END OF mS_config.

    DATA object    TYPE REF TO object.
    DATA mt_filter TYPE z2ui5_cl_util=>ty_t_filter_multi.

    METHODS obj_to_filter.
    METHODS filter_to_object.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_db IMPLEMENTATION.

  METHOD obj_to_filter.

  ENDMETHOD.

  METHOD filter_to_object.

  ENDMETHOD.

  METHOD db_read.

    SELECT FROM z2ui5_t_13
      FIELDS
       *
      WHERE uname    = @s_info-uname
        AND handle01 = @s_info-handle01
      INTO TABLE @result.

  ENDMETHOD.

  METHOD db_save.

    DATA(ls_db) = s_info.
    ls_db-uuid = z2ui5_cl_util=>uuid_get_c32( ).
    ls_db-data = z2ui5_cl_util=>xml_stringify( data ).

    MODIFY z2ui5_t_13 FROM @ls_db.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD factory.

    result = NEW #( ).

*    SELECT FROM z2ui5_t_13
*      FIELDS *
*      WHERE classname = @report
*      INTO TABLE @result->mt_variant.

  ENDMETHOD.

  METHOD check_default.

  ENDMETHOD.

  METHOD db_variant_read.

  ENDMETHOD.

  METHOD get_default.

  ENDMETHOD.

  METHOD db_variant_save.

  ENDMETHOD.

  METHOD db_read_default.

    IF s_info-uname IS INITIAL.
      DATA(lv_uname) = sy-uname.
    ELSE.
      lv_uname = s_info-uname.
    ENDIF.

    SELECT SINGLE FROM z2ui5_t_13
      FIELDS
       *
      WHERE uname    = @lv_uname
        AND handle01 = @s_info-handle01
        AND check_def = @abap_true
      INTO @result.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    SELECT SINGLE FROM z2ui5_t_13
      FIELDS
       *
      WHERE
*        uname    = @s_info-uname AND
         handle01 = @s_info-handle01 AND
         check_def = @abap_true
      INTO @result.


  ENDMETHOD.

ENDCLASS.
