CLASS z2ui5_cl_sel_var_db DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_s_db TYPE z2ui5_t_13.
    TYPES ty_t_db TYPE STANDARD TABLE OF z2ui5_t_13 WITH EMPTY KEY.

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
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_sel_var_db IMPLEMENTATION.

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
      WHERE handle01 = @s_info-handle01
        AND check_def = @abap_true
      INTO @result.

  ENDMETHOD.

ENDCLASS.
