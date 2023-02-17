# steampunkification
Notes and snippets for Steampunk compatibility.

## XSTRING to STRING utf8 conversion

```abap
  METHOD from_xstring.

    DATA conv TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
          RECEIVING
            instance = conv.

        CALL METHOD conv->('IF_ABAP_CONV_IN~CONVERT')
          EXPORTING
            source = xstring
          RECEIVING
            result = string.
      CATCH cx_sy_dyn_call_illegal_class.
        DATA(conv_in_class) = 'CL_ABAP_CONV_IN_CE'.
        CALL METHOD (conv_in_class)=>create
          EXPORTING
            encoding = 'UTF-8'
          RECEIVING
            conv     = conv.

        CALL METHOD conv->('CONVERT')
          EXPORTING
            input = xstring
          IMPORTING
            data  = string.
    ENDTRY.
```

## STRING to XSTRING utf8 conversion

```abap
  METHOD to_xstring.

    DATA conv TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
          RECEIVING
            instance = conv.

        CALL METHOD conv->('IF_ABAP_CONV_OUT~CONVERT')
          EXPORTING
            source = string
          RECEIVING
            result = xstring.
      CATCH cx_sy_dyn_call_illegal_class.
        DATA(conv_out_class) = 'CL_ABAP_CONV_OUT_CE'.
        CALL METHOD (conv_out_class)=>create
          EXPORTING
            encoding = 'UTF-8'
          RECEIVING
            conv     = conv.

        CALL METHOD conv->('CONVERT')
          EXPORTING
            data   = string
          IMPORTING
            buffer = xstring.
    ENDTRY.

  ENDMETHOD.
```

## List key fields of a database table

```abap
  METHOD list_key_fields.
    DATA obj TYPE REF TO object.
    DATA lv_tabname TYPE c LENGTH 16.
    DATA lr_ddfields TYPE REF TO data.
    FIELD-SYMBOLS <any> TYPE any.
    FIELD-SYMBOLS <field> TYPE simple.
    FIELD-SYMBOLS <ddfields> TYPE ANY TABLE.

* convert to correct type,
    lv_tabname = mv_table.

    TRY.
        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
          EXPORTING
            iv_name           = lv_tabname
          RECEIVING
            ro_database_table = obj.
        ASSIGN obj->('IF_XCO_DATABASE_TABLE~FIELDS->IF_XCO_DBT_FIELDS_FACTORY~KEY') TO <any>.
        ASSERT sy-subrc = 0.
        obj = <any>.
        CALL METHOD obj->('IF_XCO_DBT_FIELDS~GET_NAMES')
          RECEIVING
            rt_names = names.
      CATCH cx_sy_dyn_call_illegal_class.
        DATA(workaround) = 'DDFIELDS'.
        CREATE DATA lr_ddfields TYPE (workaround).
        ASSIGN lr_ddfields->* TO <ddfields>.
        ASSERT sy-subrc = 0.
        <ddfields> = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name(
          lv_tabname ) )->get_ddic_field_list( ).
        LOOP AT <ddfields> ASSIGNING <any>.
          ASSIGN COMPONENT 'KEYFLAG' OF STRUCTURE <any> TO <field>.
          IF sy-subrc <> 0 OR <field> <> abap_true.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT 'FIELDNAME' OF STRUCTURE <any> TO <field>.
          ASSERT sy-subrc = 0.
          APPEND <field> TO names.
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.
```

## IXML

Most classes and methods exists, but with "_CORE" postfix

eg. use `CL_IXML_CORE` instead of `CL_IXML`

## HTTP Client

todo, https://github.com/open-abap/cross-http-client

## DESCRIBE FIELD IN CHARACTER MODE

replace

`DESCRIBE FIELD <lg_line> LENGTH lv_length IN CHARACTER MODE.`

with

`lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length.`

## DESCRIBE FIELD TYPE

replace

`DESCRIBE FIELD <lg_line> TYPE foo.`

with

`foo = cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind.`

## GET RUN TIME

??

## cl_http_utility=>unescape_url

Replace `cl_http_utility=>unescape_url` with `cl_web_http_utility=>unescape_url`

## TADIR

use `I_CustABAPObjDirectoryEntry` instead