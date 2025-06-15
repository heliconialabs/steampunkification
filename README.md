# steampunkification
Notes and snippets for Steampunk compatibility.

Snippets are compatible with both classic ABAP and ABAP Cloud Programming model, dont use the snippets if targeting only Classic or Cloud.

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
| MD5_CALCULATE_HASH_FOR_CHAR | CL_ABAP_HMAC |
| MSGNR | SYMSGNO |
| MSGV1 | SYMSGV |
| MSGV2 | SYMSGV |
| MSGV3 | SYMSGV |
| MSGV4 | SYMSGV |
| TABLE_OF_STRINGS | STRING_TABLE |
| STRING_T | STRING_TABLE |
| SAP_BOOL | ABAP_BOOLEAN |
| SUBRC | SYSUBRC |
| PROGRAMM | SYREPID |
| T002 | I_Language |
| T002T | I_LanguageText |
| TADIR | I_CustABAPObjDirectoryEntry	 |
| TRKORR | SXCO_TRANSPORT |
| OPTION | BAPIOPTION |
| SYST_MSGV | SYMSGV |
| BAPIRET2_T | BAPIRETTAB |

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

## Decode base64
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

## Encode base64
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
    TYPES ty_c30 TYPE c LENGTH 30.
    DATA names TYPE STANDARD TABLE OF ty_c30 WITH EMPTY KEY.
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

`lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length / cl_abap_char_utilities=>charsize.`

## DESCRIBE FIELD TYPE

replace

`DESCRIBE FIELD <lg_line> TYPE foo.`

with

`foo = cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind.`

## Convert language to iso code language

```abap
    DATA input  TYPE spras.
    DATA result TYPE laiso.

    DATA lv_class TYPE string.
    lv_class = 'CL_I18N_LANGUAGES'.

    input = 'E'.

    TRY.
        SELECT SINGLE LanguageISOCode FROM ('I_LANGUAGE')
          WHERE language = @input INTO @result.
      CATCH cx_sy_dynamic_osql_error.
        CALL METHOD (lv_class)=>sap1_to_sap2
          EXPORTING
            im_lang_sap1  = input
          RECEIVING
            re_lang_sap2  = result
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
    ENDTRY.
```

## Convert iso code language to language

```abap
    DATA input  TYPE laiso.
    DATA result TYPE spras.

    DATA lv_class TYPE string.
    lv_class = 'CL_I18N_LANGUAGES'.

    input = 'E'.

    TRY.
        SELECT SINGLE language FROM ('I_LANGUAGE')
          WHERE LanguageISOCode = @input INTO @result.
      CATCH cx_sy_dynamic_osql_error.
        CALL METHOD (lv_class)=>sap2_to_sap1
          EXPORTING
            im_lang_sap2  = input
          RECEIVING
            re_lang_sap1  = result
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
    ENDTRY.
```

## Find ABAP Language Version for running program

```abap
  TRY.
    cl_abap_typedescr=>describe_by_name( 'T100' ).
    out->write( 'Classic' ).
  CATCH cx_root.
    out->write( 'ABAP Cloud' ).
  ENDTRY.
```

Alternative:

```abap
DATA program      TYPE c LENGTH 40 VALUE sy-repid.
DATA abap_version TYPE string.

OVERLAY program WITH '==============================CP'.

TRY.
    SELECT SINGLE FROM ('PROGDIR')
      FIELDS CASE uccheck
               WHEN 'X' THEN 'Classic'
               WHEN '5' THEN 'Cloud'
             END
      WHERE name = @program
        AND state = 'A'
      INTO @abap_version.
  CATCH cx_sy_dynamic_osql_error.
    abap_version = 'Cloud'.
ENDTRY.
```
## Retrieving a List of All Classes Implementing an Interface
```abap
 METHOD get_list_classes_impl_intf.

    TRY.

        DATA obj TYPE REF TO object.
        CALL METHOD ('XCO_CP_ABAP')=>interface
          EXPORTING
            iv_name      = val
          RECEIVING
            ro_interface = obj.

        FIELD-SYMBOLS <any> TYPE any.
        ASSIGN obj->('IF_XCO_AO_INTERFACE~IMPLEMENTATIONS') TO <any>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
        ENDIF.
        obj = <any>.

        ASSIGN obj->('IF_XCO_INTF_IMPLEMENTATIONS_FC~ALL') TO <any>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_dyn_call_illegal_class.
        ENDIF.
        obj = <any>.

        CALL METHOD obj->('IF_XCO_INTF_IMPLEMENTATIONS~GET').

        DATA lt_implementation_names TYPE string_table.
        CALL METHOD obj->('IF_XCO_INTF_IMPLEMENTATIONS~GET_NAMES')
          RECEIVING
            rt_names = lt_implementation_names.

        result = lt_implementation_names.

      CATCH cx_sy_dyn_call_illegal_class.

        TYPES:
          BEGIN OF ty_s_impl,
            clsname    TYPE c LENGTH 30,
            refclsname TYPE c LENGTH 30,
          END OF ty_s_impl.
        DATA lt_impl TYPE STANDARD TABLE OF ty_s_impl.

        TYPES:
          BEGIN OF ty_s_key,
            intkey TYPE c LENGTH 30,
          END OF ty_s_key.
        DATA ls_key TYPE ty_s_key.
        ls_key-intkey = val.

        DATA(lv_fm) = `SEO_INTERFACE_IMPLEM_GET_ALL`.
        CALL FUNCTION lv_fm
          EXPORTING
            intkey       = ls_key
          IMPORTING
            impkeys      = lt_impl
          EXCEPTIONS
            not_existing = 1
            OTHERS       = 2.

        LOOP AT lt_impl REFERENCE INTO DATA(lr_impl).
          INSERT CONV #( lr_impl->clsname ) INTO TABLE result.
        ENDLOOP.

    ENDTRY.

  ENDMETHOD.
```
## Read the Source Code of a Method of a Class
```abap
    DATA(result) = VALUE string_table( ).
    DATA object TYPE REF TO object.
    FIELD-SYMBOLS <any> TYPE any.
    DATA lt_source TYPE string_table.

    TRY.
        DATA lv_class TYPE c LENGTH 30.
        DATA lv_method TYPE c LENGTH 61.
        lv_class  = iv_classname.
        lv_method = iv_methodname.

        CALL METHOD ('XCO_CP_ABAP')=>('CLASS')
          EXPORTING
            iv_name  = lv_class
          RECEIVING
            ro_class = object.

        ASSIGN ('OBJECT->IF_XCO_AO_CLASS~IMPLEMENTATION') TO <any>.
        object = <any>.

        CALL METHOD object->('IF_XCO_CLAS_IMPLEMENTATION~METHOD')
          EXPORTING
            iv_name   = lv_method
          RECEIVING
            ro_method = object.

        CALL METHOD object->('IF_XCO_CLAS_I_METHOD~CONTENT')
          RECEIVING
            ro_content = object.

        CALL METHOD object->('IF_XCO_CLAS_I_METHOD_CONTENT~GET_SOURCE')
          RECEIVING
            rt_source = result.

      CATCH cx_sy_dyn_call_error INTO DATA(x).

        DATA(lv_name) = 'CL_OO_FACTORY'.
        CALL METHOD (lv_name)=>('CREATE_INSTANCE')
          RECEIVING
            result = object.

        CALL METHOD object->('IF_OO_CLIF_SOURCE_FACTORY~CREATE_CLIF_SOURCE')
          EXPORTING
            clif_name = iv_classname
          RECEIVING
            result    = object.

        CALL METHOD object->('IF_OO_CLIF_SOURCE~GET_SOURCE')
          IMPORTING
            source = lt_source.

        DATA(lv_check_method) = abap_false.
        LOOP AT lt_source INTO DATA(lv_source).
          DATA(lv_source_upper) = to_upper( lv_source ).

          IF lv_source_upper CS `ENDMETHOD`.
            lv_check_method = abap_false.
          ENDIF.

          IF lv_source_upper CS to_upper( |{ iv_methodname }| ).
            lv_check_method = abap_true.
            CONTINUE.
          ENDIF.

          IF lv_check_method = abap_true.
            INSERT lv_source INTO TABLE result.
          ENDIF.

        ENDLOOP.

    ENDTRY.
```

## Get data element texts
```abap

    TYPES:
      BEGIN OF ty_data_element_texts,
        header TYPE string,
        short  TYPE string,
        medium TYPE string,
        long   TYPE string,
      END OF ty_data_element_texts.

    METHOD get_data_element_texts
      IMPORTING
        i_data_element_name TYPE string
      RETURNING
        VALUE(result)       TYPE ty_data_element_texts.

  METHOD get_data_element_texts.

    DATA:
      data_element_name TYPE c LENGTH 30,
      ddic_ref          TYPE REF TO data,
      data_element      TYPE REF TO object,
      content           TYPE REF TO object,
      BEGIN OF ddic,
        reptext   TYPE string,
        scrtext_s TYPE string,
        scrtext_m TYPE string,
        scrtext_l TYPE string,
      END OF ddic,
      exists TYPE abap_bool.

    data_element_name = i_data_element_name.

    TRY.
        cl_abap_typedescr=>describe_by_name( 'T100' ).

        DATA(struct_desrc) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'DFIES' ) ).

        CREATE DATA ddic_ref TYPE HANDLE struct_desrc.
        ASSIGN ddic_ref->* TO FIELD-SYMBOL(<ddic>).
        ASSERT sy-subrc = 0.

        DATA(data_descr) = CAST cl_abap_datadescr( cl_abap_elemdescr=>describe_by_name( data_element_name ) ).

        CALL METHOD data_descr->('GET_DDIC_FIELD')
          RECEIVING
            p_flddescr   = <ddic>
          EXCEPTIONS
            not_found    = 1
            no_ddic_type = 2
            OTHERS       = 3.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        ddic = CORRESPONDING #( <ddic> ).
        result-header = ddic-reptext.
        result-short  = ddic-scrtext_s.
        result-medium = ddic-scrtext_m.
        result-long   = ddic-scrtext_l.

      CATCH cx_root. " ABAP Cloud

        CALL METHOD ('XCO_CP_ABAP_DICTIONARY')=>('DATA_ELEMENT')
          EXPORTING
            iv_name         = data_element_name
          RECEIVING
            ro_data_element = data_element.

        CALL METHOD data_element->('IF_XCO_AD_DATA_ELEMENT~EXISTS')
          RECEIVING
            rv_exists = exists.

        IF exists = abap_false.
          RETURN.
        ENDIF.

        CALL METHOD data_element->('IF_XCO_AD_DATA_ELEMENT~CONTENT')
          RECEIVING
            ro_content = content.

        CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_HEADING_FIELD_LABEL')
          RECEIVING
            rs_heading_field_label = result-header.

        CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_SHORT_FIELD_LABEL')
          RECEIVING
            rs_short_field_label = result-short.

        CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_MEDIUM_FIELD_LABEL')
          RECEIVING
            rs_medium_field_label = result-medium.

        CALL METHOD content->('IF_XCO_DTEL_CONTENT~GET_LONG_FIELD_LABEL')
          RECEIVING
            rs_long_field_label = result-long.

    ENDTRY.

  ENDMETHOD.
```

## cl_abap_conv_in_ce=>uccp

https://github.com/larshp/uccp

```abap
  METHOD uccp.

    DATA lv_class TYPE string.
    DATA lv_xstr  TYPE xstring.
    DATA instance TYPE REF TO object.

    lv_class = 'CL_ABAP_CONV_IN_CE'.

    TRY.
        CALL METHOD (lv_class)=>uccp
          EXPORTING
            uccp = uccp
          RECEIVING
            char = char.
      CATCH cx_sy_dyn_call_illegal_class.
        lv_xstr = uccp.

        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
          EXPORTING
            codepage = 'UTF-16'
          RECEIVING
            instance = instance.

* convert endianness
        CONCATENATE lv_xstr+1(1) lv_xstr(1) INTO lv_xstr IN BYTE MODE.

        CALL METHOD instance->('IF_ABAP_CONV_IN~CONVERT')
          EXPORTING
            source = lv_xstr
          RECEIVING
            result = char.
    ENDTRY.

  ENDMETHOD.
```
