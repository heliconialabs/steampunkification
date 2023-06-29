# steampunkification
Notes and snippets for Steampunk compatibility.

| On-Prem  | Steampunk  |
|---|---|
| ARBGB | SYMSGID |
| CL_ABAP_CODEPAGE | CL_ABAP_CONV_CODEPAGE |
| CL_ABAP_CONV_IN_CE | CL_ABAP_CONV_CODEPAGE |
| CL_ABAP_CONV_OUT_CE | CL_ABAP_CONV_CODEPAGE |
| CL_GUI_FRONTEND_SERVICES | no replacement |
| CL_HTTP_CLIENT | CL_HTTP_DESTINATION_PROVIDER |
| CL_HTTP_UTILITY | CL_WEB_HTTP_UTILITY |
| CL_IXML | CL_IXML_CORE |
| DATUM | XSDDATE_D |
| DEVCLASS | I_CustABAPObjDirectoryEntry-ABAPPackage |
| FLAG_X | ABAP_BOOLEAN |
| FUNCNAME | SXCO_FM_NAME |
| IF_IXML_ISTREAM | IF_IXML_ISTREAM_CORE |
| IF_IXML_OSTREAM | IF_IXML_OSTREAM_CORE |
| IF_IXML_PARSE_ERROR | IF_IXML_PARSE_ERROR_CORE |
| IF_IXML_PARSER | IF_IXML_PARSER_CORE |
| IF_IXML_RENDERER | IF_IXML_RENDERER_CORE |
| IF_IXML_STREAM_FACTORY | IF_IXML_STREAM_FACTORY_CORE |
| LOWERCASE | ABAP_BOOLEAN |
| MSGNR | SYMSGNO |
| MSGV1 | SYMSGV |
| MSGV2 | SYMSGV |
| MSGV3 | SYMSGV |
| MSGV4 | SYMSGV |
| SAP_BOOL | ABAP_BOOLEAN |
| SUBRC | SYSUBRC |
| PROGRAMM | SYREPID |
| T002 | I_Language |
| T002T | I_LanguageText |
| TADIR | I_CustABAPObjDirectoryEntry	 |
| TRKORR | SXCO_TRANSPORT |

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
ENDMETHOD.
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
## Create UUID in c32

```abap
METHOD get_uuid.

  DATA uuid TYPE sysuuid_c32.

  TRY.
      CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_c32_static
        RECEIVING
          uuid = uuid.
    CATCH cx_sy_dyn_call_illegal_class.
      DATA lv_fm TYPE string.
      lv_fm = 'GUID_CREATE'.
      CALL FUNCTION lv_fm
        IMPORTING
          ev_guid_32 = uuid.
  ENDTRY.

ENDMETHOD.
```

## decode base64
```abap
  METHOD decode_x_base64.

    TRY.

        CALL METHOD ('CL_WEB_HTTP_UTILITY')=>('DECODE_X_BASE64')
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

      CATCH cx_sy_dyn_call_illegal_class.

        DATA(classname) = 'CL_HTTP_UTILITY'.
        CALL METHOD (classname)=>('DECODE_X_BASE64')
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

    ENDTRY.

  ENDMETHOD.
```

## encode base64
```abap
  METHOD encode_x_base64.

    TRY.

        CALL METHOD ('CL_WEB_HTTP_UTILITY')=>('ENCODE_X_BASE64')
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

      CATCH cx_sy_dyn_call_illegal_class.

        DATA(classname) = 'CL_HTTP_UTILITY'.
        CALL METHOD (classname)=>('ENCODE_X_BASE64')
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

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
        IF sy-subrc  <> 0.
* fallback to RTTI, KEY field does not exist in S/4 2020
          RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
        ENDIF.
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

## Get table delivery class

```abap
DATA lv_contflag TYPE c LENGTH 1.

DATA lo_table          TYPE REF TO object.
DATA lo_content        TYPE REF TO object.
DATA lo_delivery_class TYPE REF TO object.
FIELD-SYMBOLS <any>    TYPE any.
TRY.
    CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>database_table
      EXPORTING
        iv_name           = 'ZHVAM_CUST'
      RECEIVING
        ro_database_table = lo_table.
    CALL METHOD lo_table->('IF_XCO_DATABASE_TABLE~CONTENT')
      RECEIVING
        ro_content = lo_content.
    CALL METHOD lo_content->('IF_XCO_DBT_CONTENT~GET_DELIVERY_CLASS')
      RECEIVING
        ro_delivery_class = lo_delivery_class.
    ASSIGN lo_delivery_class->('VALUE') TO <any>.
    lv_contflag = <any>.
  CATCH cx_sy_dyn_call_illegal_class.
    SELECT SINGLE contflag FROM ('DD02L') WHERE tabname = 'ZHVAM_CUST' INTO @lv_contflag.
ENDTRY.
```

## Get system time zone

```abap
DATA lv_zone TYPE tznzone.
DATA lv_fm TYPE string.
lv_fm = 'GET_SYSTEM_TIMEZONE'.

TRY.
    CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
      RECEIVING
        system_timezone = lv_zone.
  CATCH cx_sy_dyn_call_illegal_method.
    CALL FUNCTION lv_fm
      IMPORTING
        timezone            = lv_zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
ENDTRY.
```

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
