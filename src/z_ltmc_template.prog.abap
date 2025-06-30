*&---------------------------------------------------------------------*
*& Report Z_LTMC_TEMPLATE
*&---------------------------------------------------------------------*
*& Based on DMC_MWB_DOWNLOAD_FILE_TEMPLATE
*&---------------------------------------------------------------------*
REPORT z_ltmc_template.

CONSTANTS: co_xml(4)         TYPE c VALUE '.xml' ##NO_TEXT,
           co_retention_days TYPE i VALUE 30 ##NO_TEXT.

DATA: go_mig_obj             TYPE REF TO cl_dmc_conv_object,
      lo_proj_proxy          TYPE REF TO /ltb/if_mc_proj_proxy,
      gv_subproject_ident    TYPE dmc_sident,
      gv_project_ident       TYPE dmc_pident,
      gv_mo_active_view      TYPE dmc_vm_ident,
      gt_dataset             TYPE dmc_string_tab,
      gs_dataset             LIKE LINE OF gt_dataset,
      gv_ident               TYPE dmc_ident,
      gx_file_services       TYPE REF TO cx_dmc_file_services,
      go_file                TYPE REF TO if_dmc_file,
      gv_language            TYPE c LENGTH 2,
      gv_content             TYPE string,
      gv_content_x           TYPE xstring,
      gv_code_page           TYPE cpcodepage VALUE '4110',
      gv_filename_suggestion TYPE string,
      gv_file_name           TYPE dmc_file_name,
      gt_message             TYPE dmc_bal_t_msg,
      go_loghandler          TYPE REF TO cl_dmc_log_handler,
      go_zip_content         TYPE REF TO cl_abap_zip,
      gv_zip_content         TYPE xstring,
      gv_error_occurred      TYPE abap_bool,
      gv_tmpl_downloaded     TYPE abap_bool,
      gv_log_number          TYPE balognr,
      gs_file_store          TYPE /ltb/mc_fileproc,   " File store name to be confirm
      gv_timestamp           TYPE timestamp.

TYPES: BEGIN OF tp_struct,
         sstruct TYPE dmc_pident,
         sfield  TYPE dmc_pident,
         sparam  TYPE dmc_pident,
       END OF tp_struct.

TYPES tpt_struct TYPE STANDARD TABLE OF tp_struct WITH EMPTY KEY.

TYPES: BEGIN OF tp_map,
         tstruct TYPE dmc_pident,
         tfield  TYPE dmc_pident,
         tparam  TYPE dmc_pident,
         sstruct TYPE dmc_pident,
         sfield  TYPE dmc_pident,
         rule    TYPE dmc_pident,
         source  TYPE tpt_struct,
         src_cnt TYPE sy-dbcnt,
         tdata   TYPE dmc_field,
       END OF tp_map.

DATA: tg_dom TYPE TABLE OF dd01l,
      tg_map TYPE TABLE OF tp_map.

TYPES: BEGIN OF tp_migr,
         sprjct  TYPE dmc_sprjct-ident,
         sprjctt TYPE dmcsprjctt-descr,
         objmg   TYPE dmc_cobj-ident,
         objmgt  TYPE dmc_cobjt-descr,
         prjct   TYPE dmc_prjct-ident,
         prjctt  TYPE dmc_prjctt-descr,
       END OF tp_migr.

DATA: tg_migr TYPE TABLE OF tp_migr.

TYPES: BEGIN OF tp_excep,
         table TYPE dd03l-tabname,
         field TYPE dd03l-fieldname,
       END OF tp_excep.

TYPES: tpt_excep TYPE STANDARD TABLE OF tp_excep WITH KEY table.

TYPES: BEGIN OF tp_where,
         objmg  TYPE dmc_cobj-ident,
         table  TYPE dd03l-tabname,
         field  TYPE dd03l-fieldname,
         values TYPE string,
       END OF tp_where.

TYPES: tpt_where TYPE STANDARD TABLE OF tp_where WITH KEY objmg table field.

TYPES: tpt_skip TYPE RANGE OF dd03l-tabname.

TYPES: BEGIN OF tp_valp,
         domname TYPE dd01l-domname,
         table   TYPE dd02l-tabname,
         tptab   TYPE dd02l-contflag,
         vals    TYPE html_table,
       END OF tp_valp.

DATA tg_valp TYPE SORTED TABLE OF tp_valp WITH UNIQUE KEY domname.

FINAL(tg_excep) = VALUE tpt_excep( ( table = 'T001W' field = 'NAME1' )
                                   ( table = 'T001'  field = 'BUTXT' )
                                   ( table = 'TKA01' field = 'BEZEI' )
                                   ( table = 'T880'  field = 'NAME1' ) ).

FINAL(tg_where) = VALUE tpt_where( objmg = 'Z_CUSTOMER_2_M06'
                                   ( table = 'T015W' field = 'BANKS' values = |('BR')| )
                                   ( table = 'TZONE' field = 'LAND1' values = |('BR')| )
                                   ( table = 'T005S' field = 'LAND1' values = |('BR')| )
                                   ( table = 'T059Z' field = 'LAND1' values = |('BR')| )
                                   ( table = 'T059P' field = 'LAND1' values = |('BR')| )
                                   ( table = 'TBZ0'  field = 'OBJAP' values = |('BUPA')| )
                                   ( table = 'TB037' field = 'AUOBJ' values = |('BUPA')| )
                                   ( table = 'TBRG'  field = 'BROBJ' values = |('KNA1')| ) ).

FINAL(rg_skip) = VALUE tpt_skip( sign = 'I' option = 'CP' ( low = 'DD*' )
                                 sign = 'I' option = 'EQ'
                                                          ( low = 'SKA1' )
                                                          ( low = 'T685' )
                                                          ( low = 'T005E' )
                                                          ( low = 'T005G' )
                                                          ( low = 'TTXID' ) ).

FINAL(tg_fieldheader1) = VALUE tline_tab( ( tdline = '   <Style ss:ID="FIELDHEADER1">' )
                                          ( tdline = '    <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>' )
                                          ( tdline = '    <Borders>' )
                                          ( tdline = '     <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '    </Borders>' )
                                          ( tdline = '    <Font ss:FontName="Arial" x:Family="Swiss" ss:Color="#0D0D0C"/>' )
                                          ( tdline = '    <Interior ss:Color="#FA7602" ss:Pattern="Solid"/>' )
                                          ( tdline = '    <Protection/>' )
                                          ( tdline = '   </Style>' ) ).

FINAL(tg_fieldheader2) = VALUE tline_tab( ( tdline = '   <Style ss:ID="FIELDHEADER2">' )
                                          ( tdline = '    <Alignment ss:Horizontal="Left" ss:Vertical="Top" ss:WrapText="1"/>' )
                                          ( tdline = '    <Borders>' )
                                          ( tdline = '     <Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '     <Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/>' )
                                          ( tdline = '    </Borders>' )
                                          ( tdline = '    <Font ss:FontName="Arial" x:Family="Swiss" ss:Color="#0D0D0C"/>' )
                                          ( tdline = '    <Interior ss:Color="#339116" ss:Pattern="Solid"/>' )
                                          ( tdline = '    <Protection/>' )
                                          ( tdline = '   </Style>' ) ).

*--------------------------------------------------------------------*
*SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) c_sproj FOR FIELD p_sproj.
  PARAMETERS: p_sproj TYPE dmc_sprjct-ident.
  SELECTION-SCREEN COMMENT 55(30) c_sprojt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) c_objmg FOR FIELD p_objmg.
  PARAMETERS: p_objmg TYPE dmc_cobj-ident.
  SELECTION-SCREEN COMMENT 55(30) c_objmgt.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) c_dir FOR FIELD p_dir.
  PARAMETERS: p_dir TYPE text255.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_xml RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND r2.
  SELECTION-SCREEN COMMENT 5(50) c_xml FOR FIELD p_xml.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_csv RADIOBUTTON GROUP r2.
  SELECTION-SCREEN COMMENT 5(50) c_csv FOR FIELD p_csv.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_1file RADIOBUTTON GROUP r1 DEFAULT 'X' MODIF ID xml.
  SELECTION-SCREEN COMMENT 5(50) c_1file FOR FIELD p_1file MODIF ID xml.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_split RADIOBUTTON GROUP r1 MODIF ID xml.
  SELECTION-SCREEN COMMENT 5(50) c_split FOR FIELD p_split MODIF ID xml.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_open AS CHECKBOX MODIF ID xml.
  SELECTION-SCREEN COMMENT 5(50) c_open FOR FIELD p_open MODIF ID xml.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS: p_alv AS CHECKBOX MODIF ID xml.
  SELECTION-SCREEN COMMENT 5(50) c_alv FOR FIELD p_alv MODIF ID xml.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_debug TYPE c NO-DISPLAY.

*--------------------------------------------------------------------*
*INITIALIZATION.
*--------------------------------------------------------------------*
INITIALIZATION.

  PERFORM zf_init.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM zf_scr_output.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM zf_sel_screen.

**--------------------------------------------------------------------*
**AT SELECTION-SCREEN ON p_sproj.
**--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON p_sproj.
*
*  READ TABLE tg_migr INTO DATA(wl_migr) WITH KEY sprjct = p_sproj.
*  p_objmg = wl_migr-objmg.
*
**--------------------------------------------------------------------*
**AT SELECTION-SCREEN ON p_objmg.
**--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON p_objmg.
*
*  READ TABLE tg_migr INTO DATA(wl_migr) WITH KEY objmg = p_objmg.
*  p_sproj = wl_migr-sprjct.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.

  PERFORM zf_get_dir CHANGING p_dir.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sproj.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sproj.

  PERFORM zf_search_help.

*--------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_objmg.
*--------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_objmg.

  PERFORM zf_search_help.

*--------------------------------------------------------------------*
*START-OF-SELECTION.
*--------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(vl_ptype) = COND /ltb/mc_fileproc_cat( WHEN p_xml IS NOT INITIAL THEN 'U' ELSE 'V' ).

  PERFORM zf_standard USING vl_ptype.

  CHECK p_xml IS NOT INITIAL.

  PERFORM zf_select_data.

  IF p_alv IS NOT INITIAL.
    PERFORM zf_alv USING tg_map.

  ENDIF.

  PERFORM zf_adjust_xml.

*&---------------------------------------------------------------------*
*& Form zf_val_pos_dom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_val_pos_dom USING pu_domname TYPE dd01l-domname
                 CHANGING pc_t_valp  TYPE html_table
                          pc_lines TYPE numc3.

  DEFINE zml_replace.

    REPLACE ALL OCCURRENCES OF '&' IN &1 WITH ''.
    REPLACE ALL OCCURRENCES OF '<' IN &1 WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN &1 WITH ''.

  END-OF-DEFINITION.

  DATA: tl_selfld  TYPE html_table,
        tl_fields  TYPE TABLE OF dfies,
        tl_tfields TYPE TABLE OF dd03p,
        tl_vals    TYPE TABLE OF dd07v,
        tl_forkey  TYPE TABLE OF dd05m,
        tl_keys    TYPE TABLE OF dd08v,
        tl_xvalp   TYPE html_table,
        wl_xvalp   LIKE LINE OF tl_xvalp,
        vl_textt   TYPE dd08v-tabname,
        vl_selfrm  TYPE string.

  DATA: ol_elemdescr   TYPE REF TO cl_abap_elemdescr,
        ol_structdescr TYPE REF TO cl_abap_structdescr,
        ol_tabledescr  TYPE REF TO cl_abap_tabledescr,
        ol_seltab      TYPE REF TO data,
        tl_comps       TYPE cl_abap_structdescr=>component_table,
        vl_field       TYPE text120.

  FIELD-SYMBOLS: <fl_t_seltab> TYPE STANDARD TABLE.

  DATA wl_excep LIKE LINE OF tg_excep.
  DATA wl_where LIKE LINE OF tg_where.

  CLEAR: pc_t_valp,
         pc_lines.

  READ TABLE tg_dom INTO DATA(wl_dom) WITH KEY domname = pu_domname.

  CHECK sy-subrc EQ 0 AND rg_skip IS NOT INITIAL AND wl_dom-entitytab NOT IN rg_skip.

  READ TABLE tg_valp INTO DATA(wl_valp) WITH TABLE KEY domname = pu_domname.

  IF sy-subrc NE 0.
    IF wl_dom-valexi IS NOT INITIAL.
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = pu_domname
        TABLES
          values_tab      = tl_vals
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.

      LOOP AT tl_vals ASSIGNING FIELD-SYMBOL(<fl_w_vals>).

        zml_replace: <fl_w_vals>-ddtext.

      ENDLOOP.

      INSERT VALUE tp_valp( domname = pu_domname
                             vals    = VALUE html_table( FOR rw_vals IN tl_vals WHERE ( domvalue_l IS NOT INITIAL )
                                             ( line = condense( |{ rw_vals-domvalue_l } - { rw_vals-ddtext }&#10;| ) ) ) )
                             INTO TABLE tg_valp.

    ELSE.
      SELECT SINGLE contflag
        INTO @DATA(vl_tptab)
        FROM dd02l
        WHERE tabname EQ @wl_dom-entitytab.

      CHECK vl_tptab NE 'A'.

      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          tabname        = wl_dom-entitytab
        TABLES
          dfies_tab      = tl_fields                 " Field List if Necessary
        EXCEPTIONS
          not_found      = 1                " Nothing found
          internal_error = 2                " Internal error occurred
          OTHERS         = 3.

      DELETE tl_fields WHERE keyflag IS INITIAL
                          OR domname EQ 'MANDT'.

      tl_selfld = VALUE html_table( FOR rw_fields IN tl_fields ( line = |a~{ rw_fields-fieldname }| ) ).

      CHECK tl_fields IS NOT INITIAL.

      IF lines( tl_fields ) GT 1.
        APPEND VALUE w3html( line = REDUCE w3_html( INIT str = ``                                    sep = ``
                                                    FOR rw_fkey IN tl_fields
                                                    NEXT str = |{ str }{ sep }{ rw_fkey-scrtext_s }| sep = | / | ) && |&#10;|
                           ) TO wl_valp-vals.

      ENDIF.

      CALL FUNCTION 'DDUT_TEXTTABLE_GET'
        EXPORTING
          tabname   = wl_dom-entitytab                 " Name of Table for Text Table Search
        IMPORTING
          texttable = vl_textt.                 " Text Table if it Exists. Otherwise SPACE.

      IF vl_textt IS NOT INITIAL.
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = vl_textt         " Name of the Table to be Readble
          TABLES
            dd03p_tab     = tl_tfields                  " Table Fields
            dd05m_tab     = tl_forkey        " Foreign Key Fields of the Table
            dd08v_tab     = tl_keys
          EXCEPTIONS
            illegal_input = 1                " Value not Allowed for Parameter
            OTHERS        = 2.

        TRY.
            DATA(wl_keys) = tl_keys[ frkart = 'TEXT' ].

            DELETE tl_forkey WHERE tabname    NE wl_keys-tabname
                                OR fieldname  NE wl_keys-fieldname
                                OR forkey     EQ 'MANDT'
                                OR checkfield EQ 'MANDT'.

            LOOP AT tl_tfields INTO DATA(wl_tfields) WHERE keyflag IS NOT INITIAL
                                                       AND datatype EQ 'LANG'.

              READ TABLE tl_forkey TRANSPORTING NO FIELDS WITH KEY forkey = wl_tfields-fieldname.

              CHECK sy-subrc NE 0.

              DATA(wl_langtab) = wl_tfields.

            ENDLOOP.

            DELETE tl_tfields WHERE keyflag IS NOT INITIAL
                                 OR datatype NE 'CHAR'.

            DELETE tl_tfields FROM 2.

            tl_selfld = VALUE html_table( BASE tl_selfld FOR rw_tfields IN tl_tfields ( line = |b~{ rw_tfields-fieldname }| ) ).

            vl_selfrm = |{ wl_dom-entitytab } AS a LEFT OUTER JOIN { vl_textt } AS b ON |
                     && REDUCE string( INIT str = `` sep = ``
                                       FOR rw_forkey IN tl_forkey WHERE ( forkey NE space )
                                       NEXT str = |{ str }{ sep }a~{ rw_forkey-checkfield } = b~{ rw_forkey-forkey }| sep = ` AND ` ).

            vl_selfrm = |{ vl_selfrm } AND b~{ wl_langtab-fieldname } = sy-langu|.

          CATCH cx_sy_itab_line_not_found.
            CLEAR tl_tfields.

        ENDTRY.

      ELSE.
        CLEAR wl_excep.
        READ TABLE tg_excep INTO wl_excep WITH KEY table = wl_dom-entitytab.

        IF sy-subrc EQ 0.
          tl_selfld = VALUE html_table( BASE tl_selfld ( line = |a~{ wl_excep-field }| ) ).

        ENDIF.
      ENDIF.

      IF vl_selfrm IS INITIAL.
        vl_selfrm = |{ wl_dom-entitytab } AS a|.

      ENDIF.

      LOOP AT tl_selfld INTO DATA(wl_selfld).

        FREE ol_elemdescr.

        REPLACE 'a~' IN wl_selfld-line WITH |{ wl_dom-entitytab }-|.
        REPLACE 'b~' IN wl_selfld-line WITH |{ vl_textt }-|.

        ol_elemdescr ?= cl_abap_elemdescr=>describe_by_name( |{ wl_selfld-line }| ).

        CHECK ol_elemdescr IS BOUND.

        SPLIT wl_selfld-line AT '-' INTO wl_selfld-line vl_field.

        APPEND VALUE cl_abap_structdescr=>component( type = ol_elemdescr name = vl_field ) TO tl_comps.

      ENDLOOP.

      IF tl_comps IS NOT INITIAL.
        ol_structdescr = cl_abap_structdescr=>create( p_components = tl_comps ).

        CHECK ol_structdescr IS BOUND.

        ol_tabledescr = cl_abap_tabledescr=>create( p_line_type = ol_structdescr ).

        CHECK ol_tabledescr IS BOUND.

        CREATE DATA ol_seltab TYPE HANDLE ol_tabledescr.

        ASSIGN ol_seltab->* TO <fl_t_seltab>.

        CHECK <fl_t_seltab> IS ASSIGNED.

        READ TABLE tg_where INTO wl_where WITH KEY objmg = p_objmg
                                                   table = wl_dom-entitytab.

        DATA(vl_where) = COND string( WHEN sy-subrc EQ 0 THEN |a~{ wl_where-field } IN { wl_where-values }| ).

        SELECT (tl_selfld)
          INTO TABLE <fl_t_seltab>
          FROM (vl_selfrm)
          WHERE (vl_where).

        IF ( ( lines( tl_selfld ) GT 2 AND vl_where IS INITIAL ) OR lines( tl_selfld ) EQ 1 ) AND sy-subrc NE 0 AND p_debug EQ 'X'.
          BREAK-POINT.

        ENDIF.

        IF wl_valp-vals IS NOT INITIAL.
          APPEND LINES OF wl_valp-vals TO tl_xvalp.

        ENDIF.

        LOOP AT <fl_t_seltab> ASSIGNING FIELD-SYMBOL(<fl_w_seltab>).

          CLEAR wl_xvalp.

          LOOP AT tl_fields INTO DATA(wl_fields).
            ASSIGN COMPONENT wl_fields-fieldname OF STRUCTURE <fl_w_seltab> TO FIELD-SYMBOL(<fl_field>).

            CHECK sy-subrc EQ 0.

            WRITE <fl_field> TO vl_field.
            CONDENSE vl_field NO-GAPS.

            IF wl_xvalp-line IS NOT INITIAL.
              CONCATENATE wl_xvalp-line '/' vl_field INTO wl_xvalp-line SEPARATED BY space.

            ELSE.
              wl_xvalp-line = vl_field.

            ENDIF.

            CONDENSE wl_xvalp-line.

          ENDLOOP.

          CHECK wl_xvalp-line IS NOT INITIAL.

          IF tl_tfields IS NOT INITIAL.
            ASSIGN COMPONENT tl_tfields[ 1 ]-fieldname OF STRUCTURE <fl_w_seltab> TO <fl_field>.

            IF sy-subrc EQ 0.
              CONCATENATE wl_xvalp-line '-' <fl_field> INTO wl_xvalp-line SEPARATED BY space.

            ENDIF.

          ELSEIF wl_excep IS NOT INITIAL.
            ASSIGN COMPONENT wl_excep-field OF STRUCTURE <fl_w_seltab> TO <fl_field>.

            IF sy-subrc EQ 0.
              CONCATENATE wl_xvalp-line '-' <fl_field> INTO wl_xvalp-line SEPARATED BY space.

            ENDIF.
          ENDIF.

          zml_replace: wl_xvalp-line.

          wl_xvalp-line = condense( |{ wl_xvalp-line }&#10;| ).

          APPEND wl_xvalp TO tl_xvalp.

        ENDLOOP.

        INSERT VALUE tp_valp( domname = pu_domname
                              table   = wl_dom-entitytab
                              tptab   = vl_tptab
                              vals    = tl_xvalp )
                              INTO TABLE tg_valp.

      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE tg_valp INTO wl_valp WITH TABLE KEY domname = pu_domname.

  CHECK sy-subrc EQ 0.

  pc_t_valp = wl_valp-vals.
  pc_lines = lines( wl_valp-vals ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_Alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TG_MAP
*&---------------------------------------------------------------------*
FORM zf_alv USING pu_table TYPE STANDARD TABLE.
*--------------------------------------------------------------------*
*  DEFINE zml_rename.
*--------------------------------------------------------------------*
  DEFINE zml_rename.

    vl_short = vl_medium = vl_long = &1.
    ol_column->set_long_text( vl_long ).
    ol_column->set_medium_text( vl_medium ).
    ol_column->set_short_text( vl_short ).

  END-OF-DEFINITION.

  DATA: ol_alv    TYPE REF TO cl_salv_table,
        ol_column TYPE REF TO cl_salv_column_list.

  DATA: wl_layout TYPE salv_s_layout_key.

  DATA: vl_variant TYPE slis_vari.

  DATA: vl_long   TYPE scrtext_l,
        vl_medium TYPE scrtext_m,
        vl_short  TYPE scrtext_s.
*--------------------------------------------------------------------*

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = ol_alv CHANGING t_table = pu_table ).

      ol_alv->get_functions( )->set_all( abap_on ).
      ol_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      ol_alv->get_columns( )->set_optimize( 'X' ).

      LOOP AT ol_alv->get_columns( )->get( ) INTO DATA(wl_cols).

        ol_column ?= wl_cols-r_column.

        CASE wl_cols-columnname.
          WHEN 'TSTRUCT'.
            zml_rename: 'Estr.Destino'.
          WHEN 'TFIELD'.
            zml_rename: 'Campo Dest.'.
          WHEN 'TPARAM'.
            zml_rename: 'Param.Dest'.
          WHEN 'SSTRUCT'.
            zml_rename: 'Estr.Origem'.
          WHEN 'SFIELD'.
            zml_rename: 'Campo Orig.'.
          WHEN 'RULE'.
            zml_rename: 'Regra Map.'.
          WHEN 'SRC_CNT'.
            zml_rename: 'Param.Regra'.
          WHEN OTHERS.
            IF wl_cols-columnname CS 'TDATA'.
              ol_column->set_visible( abap_false ).  " Oculta

            ENDIF.
        ENDCASE.

      ENDLOOP.

      ol_alv->display( ).

    CATCH cx_salv_error.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_standard
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_standard USING pu_ftype TYPE /ltb/mc_fileproc_cat.

  TYPES: BEGIN OF tpl_bin,
           line TYPE x LENGTH 1000,
         END OF tpl_bin.

  DATA: tl_bin TYPE STANDARD TABLE OF tpl_bin WITH EMPTY KEY.

  DATA: vl_fullpath TYPE string,
        vl_lbin     TYPE i.
* Initialize a ZIP instance, we will add all the MO templates to the ZIP
* Finally export the ZIP to the file stroe
  FREE go_zip_content.
  go_zip_content = NEW cl_abap_zip( ).

  "Support the "language encoding flag" (general purpose flag bit 11).SAP NOTE 1386671
  go_zip_content->support_unicode_names = abap_true.

  "  Get an instance of log handler to avoid dump when creating conversion object
  go_loghandler = cl_dmc_log_handler=>get_or_create_loghandler( ).

  "Create an instance of conversion object
  cl_dmc_conv_object=>load(
    EXPORTING
      im_c_name            = p_objmg
      im_project_name      = p_sproj
      im_subproject_name   = p_sproj
    RECEIVING
      conv_obj             = go_mig_obj
    EXCEPTIONS
      conv_obj_not_valid   = 1
      not_found            = 2
      project_not_valid    = 3
      subproject_not_valid = 4
      not_productive       = 5
      OTHERS               = 6 ).

  IF sy-subrc <> 0 OR go_mig_obj IS NOT BOUND.
    IF sy-msgid IS NOT INITIAL.
      go_loghandler->add_message( ).
    ENDIF.
    "Application log
    MESSAGE ID '/LTB/MC'  TYPE 'E' NUMBER 127 WITH p_objmg INTO go_loghandler->dummy.
    go_loghandler->add_message( ).
    gv_log_number = go_loghandler->save_log( ).
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

  SELECT SINGLE guid
    INTO @DATA(lv_prj_id)
    FROM dmc_prjct
    WHERE ident EQ @p_sproj.


  TRY.
      IF lo_proj_proxy IS NOT BOUND.
        lo_proj_proxy = /ltb/cl_mc_proxy_factory=>get_proj_proxy_by_uuid( CONV #( lv_prj_id ) ).
      ENDIF.

      DATA(lo_object_proxy) = lo_proj_proxy->get_migobj_proxy_by_uuid( CONV #( go_mig_obj->if_dmc_pobject~guid ) ).

      IF sy-batch = abap_true.
        "check content update and custom fields
        DATA(ls_result) = lo_object_proxy->check_update_custfield( ).
        IF ls_result-is_update_required = abap_true.
          MESSAGE w111(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO cl_dmc_log_handler=>dummy.
          go_loghandler->add_message( ).
        ENDIF.

        IF ls_result-is_custfield_required = abap_true.
          MESSAGE w341(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO cl_dmc_log_handler=>dummy.
          go_loghandler->add_message( ).
        ENDIF.

        DATA(lv_active_view)  = lo_object_proxy->get_migobj_view( ).

        IF lv_active_view IS INITIAL.
          MESSAGE w151(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO cl_dmc_log_handler=>dummy.
          go_loghandler->add_message( ).
        ENDIF.

        IF ls_result-is_update_required = abap_true OR
           ls_result-is_custfield_required = abap_true OR
           lv_active_view IS INITIAL.
          DATA(gv_update_required) = abap_true.
          MESSAGE w127(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO cl_dmc_log_handler=>dummy.
          go_loghandler->add_message( ).
          RETURN.

        ENDIF.
      ENDIF.

      IF go_mig_obj->if_dmc_pobject~descr IS INITIAL.
        go_mig_obj->if_dmc_pobject~descr = lo_object_proxy->get_migobj_name( ).
      ENDIF.

      IF pu_ftype = /ltb/if_mc_constants=>gc_fileproc-category-csv.
        IF go_mig_obj->if_dmc_pobject~descr IS NOT INITIAL.
          DATA(l_migobj_name) = go_mig_obj->if_dmc_pobject~descr.
        ELSE.
          l_migobj_name = go_mig_obj->if_dmc_pobject~ident.
        ENDIF.

        "Source data for <object name>
        DATA(l_folder_name) =
            |{ /ltb/if_mc_constants=>gc_filename_prefix-srcdata-single } { l_migobj_name }|.

*         remove invalid path characters which will impact ZIP file open
        REPLACE ALL OCCURRENCES OF '"' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '<' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '>' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '|' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '/' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '*' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '?' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF '\' IN l_folder_name WITH ''.
        REPLACE ALL OCCURRENCES OF ':' IN l_folder_name WITH ''.

        DATA(lo_csv_services) = /ltb/cl_mc_csv_service_factory=>create( CONV #( lv_prj_id ) ).

        DATA(lt_template) = lo_csv_services->create_template( go_mig_obj->if_dmc_pobject~guid ).

        LOOP AT lt_template ASSIGNING FIELD-SYMBOL(<tmpl>).
          DATA(l_ident_encode) = escape( val = <tmpl>-ident format = cl_abap_format=>e_xss_url ).

          DATA(l_filename) = COND #(
            WHEN l_ident_encode = /ltb/cl_mc_tmpl_info_data_src=>filename THEN |{ l_folder_name }/{ l_ident_encode }{ /ltb/if_mc_constants=>gc_filename_ext-pdf }|
              ELSE |{ l_folder_name }/{ l_ident_encode }{ /ltb/if_mc_csv_file_services=>gc_csv_file_freetext }{ /ltb/if_mc_constants=>gc_filename_ext-csv }|
          ).

          go_zip_content->add(
            EXPORTING
              name    = l_filename
              content = <tmpl>-xdata
          ).
        ENDLOOP.

      ELSE.
        gv_mo_active_view = cl_dmc_vm_view_manager=>get_active_view_cobj( gv_ident ).

        cl_dmc_file_services_factory=>create( iv_type = if_dmc_md_extractor=>gc_type_excel_xml )->download(
          EXPORTING
            io_cobj        = go_mig_obj
            io_log_handler = go_loghandler
            iv_view_ident  = gv_mo_active_view
          IMPORTING
            et_dataset     = gt_dataset
            et_message     = gt_message
        ).
        "add message from download method to return messages
*          APPEND LINES OF lt_message TO gt_message.
*
        IF gt_dataset IS INITIAL.
          LOOP AT gt_message ASSIGNING FIELD-SYMBOL(<fs_message>).
            MESSAGE ID     <fs_message>-msgid
                    TYPE   'W'
                    NUMBER <fs_message>-msgno
            WITH    <fs_message>-msgv1
                    <fs_message>-msgv2
                    <fs_message>-msgv3
                    <fs_message>-msgv4
            INTO    cl_dmc_log_handler=>dummy.
            go_loghandler->add_message( ).
          ENDLOOP.
          MESSAGE ID '/LTB/MC'  TYPE 'E' NUMBER 127 WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
          go_loghandler->add_message( ).
          RETURN.

        ENDIF.

*         Compose xml file name
        IF go_mig_obj->if_dmc_pobject~descr IS NOT INITIAL.
          DATA(l_obj_name) = go_mig_obj->if_dmc_pobject~descr.
        ELSE.
          l_obj_name = go_mig_obj->if_dmc_pobject~ident.
        ENDIF.

        "Source data for <object name>.xml
        gv_filename_suggestion =
            |{ /ltb/if_mc_constants=>gc_filename_prefix-srcdata-single } { l_obj_name }{ /ltb/if_mc_constants=>gc_filename_ext-xml }|.

*         remove invalid path characters which will impact ZIP file open
        REPLACE ALL OCCURRENCES OF '"' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '<' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '>' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '|' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '/' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '*' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '?' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF '\' IN gv_filename_suggestion WITH ''.
        REPLACE ALL OCCURRENCES OF ':' IN gv_filename_suggestion WITH ''.
*
        LOOP AT gt_dataset INTO gs_dataset.
          gv_content = gv_content && cl_abap_char_utilities=>cr_lf && gs_dataset.
        ENDLOOP.
*
        CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
          EXPORTING
            text   = gv_content
          IMPORTING
            buffer = gv_content_x
          EXCEPTIONS
            failed = 1
            OTHERS = 2.
        IF sy-subrc <> 0.
          "add application log, skip this MO
          MESSAGE ID '/LTB/MC'  TYPE 'E' NUMBER 127 WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
          go_loghandler->add_message( ).
          RETURN.

        ENDIF.

        go_zip_content->add( name    = gv_filename_suggestion
                             content = gv_content_x ).
      ENDIF.

      "at least one file template is added to zip
      gv_tmpl_downloaded = abap_true.

      "add log message
      "Template for migration object &1 is added to downloaded file
      MESSAGE s128(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
      go_loghandler->add_message( ).
    CATCH cx_dmc_file_services INTO gx_file_services.
      "at least one file template can not download
      gv_error_occurred = abap_true.
      "Error handling
      gx_file_services->send_appl_log( ).
      "Download template for migration object &1 failed
      MESSAGE  e127(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
      go_loghandler->add_message( ).
    CATCH /ltb/cx_mc_static_check_msg INTO DATA(gx_proxy_error).
      gv_error_occurred = abap_true.
      "Download template for migration object &1 failed
      MESSAGE  e127(/ltb/mc) WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
      go_loghandler->add_message( ).
  ENDTRY.

  IF pu_ftype = /ltb/if_mc_constants=>gc_fileproc-category-csv.
    IF lines( go_zip_content->files ) = 0.
      MESSAGE e344(/ltb/mc) INTO cl_dmc_log_handler=>dummy.
      go_loghandler->add_message( ).
      RETURN.

    ELSE.
      DATA(l_number) = lines( go_zip_content->files ).
      MESSAGE i343(/ltb/mc) WITH l_number INTO cl_dmc_log_handler=>dummy.
      go_loghandler->add_message( ).

    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = go_zip_content->save( )
*       append_to_table = space
      IMPORTING
        output_length = vl_lbin
      TABLES
        binary_tab    = tl_bin.

    vl_fullpath = c_sprojt.

    REPLACE ALL OCCURRENCES OF '"' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '<' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '|' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '/' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '*' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '?' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF '\' IN vl_fullpath WITH ''.
    REPLACE ALL OCCURRENCES OF ':' IN vl_fullpath WITH ''.

    vl_fullpath = condense( |{ p_dir }\\{ vl_fullpath }{ /ltb/if_mc_constants=>gc_filename_ext-zip }| ).

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        bin_filesize            = vl_lbin              " File length for binary files
        filename                = vl_fullpath          " Name of file
        filetype                = 'BIN'                " File type (ASCII, binary ...)
        confirm_overwrite       = space                " Overwrite File Only After Confirmation
      CHANGING
        data_tab                = tl_bin               " Transfer table
      EXCEPTIONS
        file_write_error        = 1                    " Cannot write to file
        no_batch                = 2                    " Front-End Function Cannot Be Executed in Backgrnd
        gui_refuse_filetransfer = 3                    " Incorrect Front End
        invalid_type            = 4                    " Invalid value for parameter FILETYPE
        no_authority            = 5                    " No Download Authorization
        unknown_error           = 6                    " Unknown error
        header_not_allowed      = 7                    " Invalid header
        separator_not_allowed   = 8                    " Invalid separator
        filesize_not_allowed    = 9                    " Invalid file size
        header_too_long         = 10                   " Header information currently restricted to 1023 bytes
        dp_error_create         = 11                   " Cannot create DataProvider
        dp_error_send           = 12                   " Error Sending Data with DataProvider
        dp_error_write          = 13                   " Error Writing Data with DataProvider
        unknown_dp_error        = 14                   " Error when calling data provider
        access_denied           = 15                   " Access to File Denied
        dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
        disk_full               = 17                   " Storage Medium full
        dp_timeout              = 18                   " Timeout of DataProvider
        file_not_found          = 19                   " Could not find file
        dataprovider_exception  = 20                   " General Exception Error in DataProvider
        control_flush_error     = 21                   " Error in Control Framework
        not_supported_by_gui    = 22                   " GUI does not support this
        error_no_gui            = 23                   " GUI not available
        OTHERS                  = 24.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.

  ELSE.
    IF gt_dataset IS INITIAL.
      LOOP AT gt_message ASSIGNING <fs_message>.
        MESSAGE ID     <fs_message>-msgid
                TYPE   'W'
                NUMBER <fs_message>-msgno
        WITH    <fs_message>-msgv1
                <fs_message>-msgv2
                <fs_message>-msgv3
                <fs_message>-msgv4
        INTO    cl_dmc_log_handler=>dummy.
        go_loghandler->add_message( ).
      ENDLOOP.
      MESSAGE ID '/LTB/MC'  TYPE 'E' NUMBER 127 WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
      go_loghandler->add_message( ).
      gv_log_number = go_loghandler->save_log( ).
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*         Compose xml file name
    IF go_mig_obj->if_dmc_pobject~descr IS NOT INITIAL.
      l_obj_name = go_mig_obj->if_dmc_pobject~descr.
    ELSE.
      l_obj_name = go_mig_obj->if_dmc_pobject~ident.
    ENDIF.

    READ TABLE tg_migr INTO DATA(wl_migr) WITH KEY sprjct = p_sproj
                                                   objmg  = p_objmg.

    "Source data for <object name>.xml
    gv_filename_suggestion =
        |{ wl_migr-prjctt }{ /ltb/if_mc_constants=>gc_filename_ext-xml }|.
*      |{ /ltb/if_mc_constants=>gc_filename_prefix-srcdata-single } { l_obj_name }{ /ltb/if_mc_constants=>gc_filename_ext-xml }|.

*         remove invalid path characters which will impact ZIP file open
    REPLACE ALL OCCURRENCES OF '"' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '<' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '|' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '/' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '*' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '?' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF '\' IN gv_filename_suggestion WITH ''.
    REPLACE ALL OCCURRENCES OF ':' IN gv_filename_suggestion WITH ''.

    LOOP AT gt_dataset INTO gs_dataset.
      gv_content = gv_content && cl_abap_char_utilities=>cr_lf && gs_dataset.
    ENDLOOP.
*
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = gv_content
      IMPORTING
        buffer = gv_content_x
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc <> 0.
      "add application log, skip this MO
      MESSAGE ID '/LTB/MC'  TYPE 'E' NUMBER 127 WITH go_mig_obj->if_dmc_pobject~descr INTO go_loghandler->dummy.
      go_loghandler->add_message( ).
      gv_log_number = go_loghandler->save_log( ).
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF p_split IS NOT INITIAL.
      vl_fullpath = |{ p_dir }\\Template_{ gv_filename_suggestion }|.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = vl_fullpath          " Name of file
          confirm_overwrite       = space                " Overwrite File Only After Confirmation
          codepage                = '4110'
        CHANGING
          data_tab                = gt_dataset           " Transfer table
        EXCEPTIONS
          file_write_error        = 1                    " Cannot write to file
          no_batch                = 2                    " Front-End Function Cannot Be Executed in Backgrnd
          gui_refuse_filetransfer = 3                    " Incorrect Front End
          invalid_type            = 4                    " Invalid value for parameter FILETYPE
          no_authority            = 5                    " No Download Authorization
          unknown_error           = 6                    " Unknown error
          header_not_allowed      = 7                    " Invalid header
          separator_not_allowed   = 8                    " Invalid separator
          filesize_not_allowed    = 9                    " Invalid file size
          header_too_long         = 10                   " Header information currently restricted to 1023 bytes
          dp_error_create         = 11                   " Cannot create DataProvider
          dp_error_send           = 12                   " Error Sending Data with DataProvider
          dp_error_write          = 13                   " Error Writing Data with DataProvider
          unknown_dp_error        = 14                   " Error when calling data provider
          access_denied           = 15                   " Access to File Denied
          dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
          disk_full               = 17                   " Storage Medium full
          dp_timeout              = 18                   " Timeout of DataProvider
          file_not_found          = 19                   " Could not find file
          dataprovider_exception  = 20                   " General Exception Error in DataProvider
          control_flush_error     = 21                   " Error in Control Framework
          not_supported_by_gui    = 22                   " GUI does not support this
          error_no_gui            = 23                   " GUI not available
          OTHERS                  = 24.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      ELSEIF p_open IS NOT INITIAL.
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            application            = 'notepad'        " Path and Name of Application
            parameter              = vl_fullpath      " Parameter for Application
          EXCEPTIONS
            cntl_error             = 1                " Control error
            error_no_gui           = 2                " No GUI available
            bad_parameter          = 3                " Incorrect parameter combination
            file_not_found         = 4                " File not found
            path_not_found         = 5                " Path not found
            file_extension_unknown = 6                " Could not find application for specified extension
            error_execute_failed   = 7                " Could not execute application or document
            synchronous_failed     = 8                " Cannot Call Application Synchronously
            not_supported_by_gui   = 9                " GUI does not support this
            OTHERS                 = 10.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_select_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_select_data .

  DATA: wl_map    LIKE LINE OF tg_map,
        wl_struct TYPE tp_struct.

  SELECT *
    INTO TABLE @DATA(tl_sprjct)
    FROM dmc_sprjct
    WHERE ident EQ @p_sproj.

  CHECK sy-subrc EQ 0.

  SELECT *
    INTO TABLE @DATA(tl_cobj)
    FROM dmc_cobj
    FOR ALL ENTRIES IN @tl_sprjct
    WHERE subproject EQ @tl_sprjct-guid
      AND ident      EQ @p_objmg.

  CHECK sy-subrc EQ 0.

  SELECT *
    INTO TABLE @DATA(tl_target)
    FROM dmc_stree
    FOR ALL ENTRIES IN @tl_cobj
    WHERE container EQ @tl_cobj-rcontainer.

  IF sy-subrc EQ 0.
    SELECT *
      INTO TABLE @DATA(tl_relat)
      FROM dmc_rsrel
      FOR ALL ENTRIES IN @tl_target
      WHERE rstruct EQ @tl_target-guid.

    SELECT *
      INTO TABLE @DATA(tl_fields)
      FROM dmc_field
      FOR ALL ENTRIES IN @tl_target
      WHERE dstructure EQ @tl_target-struct.

    SELECT *
      INTO TABLE @tg_dom
      FROM dd01l
      FOR ALL ENTRIES IN @tl_fields
      WHERE domname EQ @tl_fields-domname
        AND ( valexi EQ 'X' OR
              entitytab NE @space ).

  ENDIF.

  SELECT *
    INTO TABLE @DATA(tl_source)
    FROM dmc_stree
    FOR ALL ENTRIES IN @tl_cobj
    WHERE container EQ @tl_cobj-scontainer.

  IF sy-subrc EQ 0.
    SELECT *
      APPENDING TABLE @tl_fields
      FROM dmc_field
      FOR ALL ENTRIES IN @tl_source
      WHERE dstructure EQ @tl_source-struct.

  ENDIF.

  IF tl_fields IS NOT INITIAL.
    SELECT *
      INTO TABLE @DATA(tl_rcall)
      FROM dmc_rcall
      FOR ALL ENTRIES IN @tl_fields
      WHERE guid EQ @tl_fields-rulecall.

    SELECT *
      INTO TABLE @DATA(tl_rpar)
      FROM dmc_actp
      FOR ALL ENTRIES IN @tl_fields
      WHERE rcall EQ @tl_fields-rulecall.

    IF sy-subrc EQ 0.
      SELECT *
        INTO TABLE @DATA(tl_parm)
        FROM dmc_param
        FOR ALL ENTRIES IN  @tl_rpar
        WHERE guid EQ @tl_rpar-formalp.

    ENDIF.
  ENDIF.

  LOOP AT tl_target INTO DATA(wl_target).

    LOOP AT tl_relat INTO DATA(wl_relat) WHERE rstruct = wl_target-guid.

      CLEAR wl_map.

      READ TABLE tl_source INTO DATA(wl_source) WITH KEY guid = wl_relat-sstruct.

      wl_map-tstruct = wl_target-ident.

      LOOP AT tl_fields INTO DATA(wl_tfield) WHERE dstructure EQ wl_target-struct
                                               AND rulecall IS NOT INITIAL.

        wl_map-tfield = wl_tfield-fieldname.
        wl_map-tdata  = wl_tfield.

        READ TABLE tl_rcall INTO DATA(wl_rcall) WITH KEY guid = wl_tfield-rulecall.

        wl_map-rule = wl_rcall-ident.

        CLEAR: wl_map-source,
               wl_map-sstruct,
               wl_map-sfield.

        LOOP AT tl_rpar INTO DATA(wl_rpar) WHERE rcall EQ wl_tfield-rulecall.

          READ TABLE tl_parm TRANSPORTING NO FIELDS WITH KEY guid = wl_rpar-formalp
                                                             io_f = '1'.

          IF sy-subrc NE 0.
            wl_map-tparam = wl_rpar-ident.

          ELSE.
            CLEAR wl_struct.

            wl_struct-sparam = wl_rpar-ident.

            IF wl_rpar-valu CS '-'.
              SPLIT wl_rpar-valu AT '-' INTO wl_struct-sstruct wl_struct-sfield.
              wl_map-sstruct = wl_source-ident.

            ELSE.
              wl_struct-sfield = wl_rpar-valu.

            ENDIF.

            APPEND wl_struct TO wl_map-source.

          ENDIF.
        ENDLOOP.

        IF lines( wl_map-source ) EQ 1.
          wl_map-sfield = wl_map-source[ 1 ]-sfield.

        ELSEIF wl_map-rule CS 'CURR_TO_INTERN'.
          READ TABLE wl_map-source INTO DATA(wl_mpsrc) WITH KEY sparam = 'IV_AMOUNT_EXTERNAL'.

          wl_map-sfield = wl_mpsrc-sfield.

        ELSE.
          READ TABLE tl_fields INTO DATA(wl_sfield) WITH KEY dstructure = wl_source-struct
                                                             fieldname  = wl_tfield-rollname.

          IF sy-subrc NE 0.
            READ TABLE tl_fields INTO wl_sfield WITH KEY dstructure = wl_source-struct
                                                         fieldname  = wl_tfield-domname.

          ENDIF.

          IF sy-subrc EQ 0.
            wl_map-sfield = wl_sfield-fieldname.

          ENDIF.

          IF wl_map-sfield IS INITIAL AND wl_map-tparam CS '_'.
            SPLIT wl_map-tparam AT '_' INTO DATA(vl_pre) DATA(vl_name).

            LOOP AT wl_map-source INTO wl_mpsrc WHERE sparam CS vl_name.
              wl_map-sfield = wl_mpsrc-sfield.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.

        wl_map-src_cnt = lines( wl_map-source ).

        TRANSLATE wl_map-tstruct TO UPPER CASE.
        TRANSLATE wl_map-tfield  TO UPPER CASE.
        TRANSLATE wl_map-tparam  TO UPPER CASE.
        TRANSLATE wl_map-sstruct TO UPPER CASE.
        TRANSLATE wl_map-sfield  TO UPPER CASE.
        TRANSLATE wl_map-rule    TO UPPER CASE.

        APPEND wl_map TO tg_map.

      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  SORT tg_map BY sfield sstruct src_cnt DESCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_adjust_xml
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_adjust_xml .

  DATA: wl_map LIKE LINE OF tg_map.

  DATA: vl_offset1 TYPE i,
        vl_offset2 TYPE i,
        tl_valp    TYPE html_table,
        vl_lines   TYPE numc3.

  DATA: vl_cdataset TYPE html4096.

  DATA: vl_filename TYPE string,
        vl_path     TYPE string,
        vl_fullpath TYPE string,
        vl_action   TYPE i,
        vl_from     TYPE sy-tabix.

  FIND FIRST OCCURRENCE OF '<Styles>' IN TABLE gt_dataset MATCH LINE DATA(vl_tabix_sty).

  INSERT LINES OF tg_fieldheader2 INTO gt_dataset INDEX vl_tabix_sty + 1.
  INSERT LINES OF tg_fieldheader1 INTO gt_dataset INDEX vl_tabix_sty + 1.

  FIND ALL OCCURRENCES OF '<Worksheet ss' IN TABLE gt_dataset RESULTS DATA(tl_worksheets).

  IF p_split IS NOT INITIAL.
    vl_fullpath = |{ p_dir }\\Mapping_{ gv_filename_suggestion }|.
    DELETE gt_dataset FROM tl_worksheets[ 3 ]-line TO lines( gt_dataset ) - 1.
    DELETE gt_dataset FROM tl_worksheets[ 1 ]-line TO tl_worksheets[ 2 ]-line - 1.
    vl_from = 1.

  ELSE.
    vl_fullpath = |{ p_dir }\\{ gv_filename_suggestion }|.
    vl_from = tl_worksheets[ 2 ]-line.

  ENDIF.

  FIND FIRST OCCURRENCE OF '<Style ss:ID="FIELDLIST">' IN TABLE gt_dataset MATCH LINE vl_tabix_sty.

  LOOP AT gt_dataset ASSIGNING FIELD-SYMBOL(<fl_w_dataset>) FROM vl_tabix_sty WHERE table_line CS '<Protection/>'.
    <fl_w_dataset> = '<Protection ss:Protected="0"/>'.
    EXIT.
  ENDLOOP.

  LOOP AT gt_dataset FROM vl_from ASSIGNING FIELD-SYMBOL(<fl_dataset>).

    DATA(vl_tabix) = sy-tabix.

    IF <fl_dataset> CS '<Row'.
      DATA(vl_tabrow) = sy-tabix.

    ENDIF.

    IF <fl_dataset> CS '</Worksheet>'.
      EXIT.

    ENDIF.

    vl_cdataset = <fl_dataset>.

    CLEAR: vl_offset1, vl_offset2.
    FIND FIRST OCCURRENCE OF 'ss:ExpandedColumnCount="' IN vl_cdataset MATCH OFFSET vl_offset1.

    IF sy-subrc EQ 0.
      ADD 24 TO vl_offset1.
      FIND FIRST OCCURRENCE OF '"' IN vl_cdataset+vl_offset1 MATCH OFFSET vl_offset2.

      ADD 1 TO vl_offset2.
      vl_cdataset+vl_offset1(vl_offset2) = '23"'.

    ENDIF.

    CLEAR: vl_offset1, vl_offset2.
    FIND FIRST OCCURRENCE OF '<Column ss:Index="9"' IN vl_cdataset MATCH OFFSET vl_offset1.

    IF sy-subrc EQ 0.
      REPLACE 'ss:Span="1"' IN  vl_cdataset WITH 'ss:Span="3"'.
      REPLACE 'ss:Width="56.25"' IN  vl_cdataset WITH 'ss:Width="82.5"'.
      REPLACE 'ss:Hidden="1"' IN vl_cdataset WITH ''.

      INSERT '<Column ss:Index="15" ss:AutoFitWidth="0" ss:Width="171.75"/>'
        INTO gt_dataset INDEX vl_tabix + 1.

    ENDIF.

    CLEAR: vl_offset1, vl_offset2.
    FIND FIRST OCCURRENCE OF '<Column ss:Index="11' IN vl_cdataset MATCH OFFSET vl_offset1.

    IF sy-subrc EQ 0.
      ADD 18 TO vl_offset1.
      FIND FIRST OCCURRENCE OF '"' IN vl_cdataset+vl_offset1 MATCH OFFSET vl_offset2.

      ADD 1 TO vl_offset2.
      vl_cdataset+vl_offset1(vl_offset2) = '23"'.

    ENDIF.

    IF vl_cdataset CS '<Row ss:Index="4">'.
      DATA(vl_header) = 'X'.

    ELSEIF vl_header IS NOT INITIAL AND vl_cdataset CS '</Row>'.
      CLEAR vl_header.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Regras Transformação</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Vazio/Nulo (S/N)</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Sanear (S/N)</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Construir (S/N)</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Descr. Origem</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Campo Origem</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER2"><Data ss:Type="String">Tabela Origem</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER1"><Data ss:Type="String">Val. Poss. SAP</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER1"><Data ss:Type="String">Comprimento Interno SAP</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER1"><Data ss:Type="String">Tipo Interno SAP</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER1"><Data ss:Type="String">Campo Interno SAP</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

      INSERT '<Cell ss:StyleID="FIELDHEADER1"><Data ss:Type="String">Estr. Interna SAP</Data></Cell>'
        INTO gt_dataset INDEX vl_tabix.

    ENDIF.

    CLEAR: vl_offset1, vl_offset2.
    IF vl_cdataset CS '<Cell ss:Index="9"'.
      FIND FIRST OCCURRENCE OF 'ss:Type="String">' IN  vl_cdataset MATCH OFFSET vl_offset1.
      ADD 17 TO vl_offset1.

      FIND FIRST OCCURRENCE OF '<' IN vl_cdataset+vl_offset1 MATCH OFFSET vl_offset2.

      DATA(vl_sstruct) = |{ vl_cdataset+vl_offset1(vl_offset2) }|.

    ENDIF.

    IF vl_cdataset CS '<Cell ss:Index="10"'.
      INSERT |<Cell ss:Index="22" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="21" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="20" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="19" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="18" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="17" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      INSERT |<Cell ss:Index="16" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
        INTO gt_dataset INDEX vl_tabix + 1.

      FIND FIRST OCCURRENCE OF 'ss:Type="String">' IN  vl_cdataset MATCH OFFSET vl_offset1.
      ADD 17 TO vl_offset1.

      FIND FIRST OCCURRENCE OF '<' IN vl_cdataset+vl_offset1 MATCH OFFSET vl_offset2.

      DATA(vl_sfield) = |{ vl_cdataset+vl_offset1(vl_offset2) }|.

      CLEAR wl_map.
      READ TABLE tg_map INTO wl_map WITH KEY sstruct = vl_sstruct
                                             sfield  = vl_sfield.

      IF wl_map-tstruct IS NOT INITIAL.

        PERFORM zf_val_pos_dom USING wl_map-tdata-domname CHANGING tl_valp vl_lines.

        IF vl_lines GT 1.
          READ TABLE gt_dataset ASSIGNING FIELD-SYMBOL(<fl_tagrow>) INDEX vl_tabrow.

          vl_lines = vl_lines * '13.2'.

          REPLACE '<Row' IN <fl_tagrow> WITH |<Row ss:AutoFitHeight="0" ss:Height="{ vl_lines }"|.

          INSERT |</Data></Cell>| INTO gt_dataset INDEX vl_tabix + 1.

          DO vl_lines TIMES.
            DATA(vl_index) = COND i( WHEN 1 = 1 THEN vl_lines - sy-index + 1 ).

            READ TABLE tl_valp INTO DATA(wl_valp) INDEX vl_index.

            CHECK sy-subrc EQ 0.

            INSERT |{ wl_valp-line }| INTO gt_dataset INDEX vl_tabix + 1.

          ENDDO.

          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String">|
            INTO gt_dataset INDEX vl_tabix + 1.

        ELSEIF vl_lines EQ 1.
          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String">| &&
                 |{ tl_valp[ 1 ]-line }</Data></Cell>|
            INTO gt_dataset INDEX vl_tabix + 1.

        ELSE.
          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
            INTO gt_dataset INDEX vl_tabix + 1.

        ENDIF.

        DATA(vl_len) = COND string( WHEN wl_map-tdata-decs IS INITIAL THEN condense( |{ wl_map-tdata-outputlen ALPHA = OUT }| )
                                    ELSE condense( |{ wl_map-tdata-outputlen ALPHA = OUT } dec { wl_map-tdata-decs ALPHA = OUT }| ) ).

        INSERT |<Cell ss:Index="14" ss:StyleID="FIELDLIST"><Data ss:Type="String">| &&
               |{ vl_len }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="13" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tdata-datatype }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="12" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tfield }&#10;({ wl_map-tdata-rollname })</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="11" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tstruct }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

      ELSE.
        READ TABLE tg_map INTO wl_map WITH KEY sfield  = vl_sfield.
        IF wl_map-sfield IS NOT INITIAL.
          PERFORM zf_val_pos_dom USING wl_map-tdata-domname CHANGING tl_valp vl_lines.

        IF vl_lines GT 1.
          READ TABLE gt_dataset ASSIGNING <fl_tagrow> INDEX vl_tabrow.

          vl_lines = vl_lines * '13.2'.

          REPLACE '<Row' IN <fl_tagrow> WITH |<Row ss:AutoFitHeight="0" ss:Height="{ vl_lines }"|.

          INSERT |</Data></Cell>| INTO gt_dataset INDEX vl_tabix + 1.

          CLEAR vl_index.
          DO vl_lines TIMES.
            vl_index = COND i( WHEN 1 = 1 THEN vl_lines - sy-index + 1 ).

            READ TABLE tl_valp INTO wl_valp INDEX vl_index.

            CHECK sy-subrc EQ 0.

            INSERT |{ wl_valp-line }| INTO gt_dataset INDEX vl_tabix + 1.

          ENDDO.

          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String">|
            INTO gt_dataset INDEX vl_tabix + 1.

        ELSEIF vl_lines EQ 1.
          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String">| &&
                 |{ tl_valp[ 1 ]-line }</Data></Cell>|
            INTO gt_dataset INDEX vl_tabix + 1.

        ELSE.
          INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
            INTO gt_dataset INDEX vl_tabix + 1.

        ENDIF.

        vl_len = COND string( WHEN wl_map-tdata-decs IS INITIAL THEN condense( |{ wl_map-tdata-outputlen ALPHA = OUT }| )
                                    ELSE condense( |{ wl_map-tdata-outputlen ALPHA = OUT } dec { wl_map-tdata-decs ALPHA = OUT }| ) ).

        INSERT |<Cell ss:Index="14" ss:StyleID="FIELDLIST"><Data ss:Type="String">| &&
               |{ vl_len }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="13" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tdata-datatype }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="12" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tfield }&#10;({ wl_map-tdata-rollname })</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="11" ss:StyleID="FIELDLIST"><Data ss:Type="String">{ wl_map-tstruct }</Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.
        ELSE.
        INSERT |<Cell ss:Index="15" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="14" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="13" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="12" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.

        INSERT |<Cell ss:Index="11" ss:StyleID="FIELDLIST"><Data ss:Type="String"></Data></Cell>|
          INTO gt_dataset INDEX vl_tabix + 1.
       ENDIF.
      ENDIF.

      CLEAR: vl_sstruct, vl_sfield.

    ENDIF.

    REPLACE ALL OCCURRENCES OF 'ss:MergeAcross="8"' IN vl_cdataset WITH 'ss:MergeAcross="20"'.

    <fl_dataset> = vl_cdataset.

  ENDLOOP.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                = vl_fullpath          " Name of file
      confirm_overwrite       = space                " Overwrite File Only After Confirmation
      codepage                = '4110'
    CHANGING
      data_tab                = gt_dataset           " Transfer table
    EXCEPTIONS
      file_write_error        = 1                    " Cannot write to file
      no_batch                = 2                    " Front-End Function Cannot Be Executed in Backgrnd
      gui_refuse_filetransfer = 3                    " Incorrect Front End
      invalid_type            = 4                    " Invalid value for parameter FILETYPE
      no_authority            = 5                    " No Download Authorization
      unknown_error           = 6                    " Unknown error
      header_not_allowed      = 7                    " Invalid header
      separator_not_allowed   = 8                    " Invalid separator
      filesize_not_allowed    = 9                    " Invalid file size
      header_too_long         = 10                   " Header information currently restricted to 1023 bytes
      dp_error_create         = 11                   " Cannot create DataProvider
      dp_error_send           = 12                   " Error Sending Data with DataProvider
      dp_error_write          = 13                   " Error Writing Data with DataProvider
      unknown_dp_error        = 14                   " Error when calling data provider
      access_denied           = 15                   " Access to File Denied
      dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
      disk_full               = 17                   " Storage Medium full
      dp_timeout              = 18                   " Timeout of DataProvider
      file_not_found          = 19                   " Could not find file
      dataprovider_exception  = 20                   " General Exception Error in DataProvider
      control_flush_error     = 21                   " Error in Control Framework
      not_supported_by_gui    = 22                   " GUI does not support this
      error_no_gui            = 23                   " GUI not available
      OTHERS                  = 24.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSEIF p_open IS NOT INITIAL.
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        application            = 'notepad'        " Path and Name of Application
        parameter              = vl_fullpath      " Parameter for Application
      EXCEPTIONS
        cntl_error             = 1                " Control error
        error_no_gui           = 2                " No GUI available
        bad_parameter          = 3                " Incorrect parameter combination
        file_not_found         = 4                " File not found
        path_not_found         = 5                " Path not found
        file_extension_unknown = 6                " Could not find application for specified extension
        error_execute_failed   = 7                " Could not execute application or document
        synchronous_failed     = 8                " Cannot Call Application Synchronously
        not_supported_by_gui   = 9                " GUI does not support this
        OTHERS                 = 10.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_search_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_search_help .

  DATA: tl_return TYPE TABLE OF ddshretval,
        tl_fields TYPE TABLE OF dfies,
        tl_dynmap TYPE TABLE OF dselc.

  DATA: vl_retfield TYPE dfies-fieldname.

  vl_retfield = 'SPRJCT'.

  PERFORM get_fields_of_value_tab IN PROGRAM saplsdhi
        TABLES tg_migr tl_fields
        CHANGING vl_retfield.

  tl_dynmap = VALUE vec_t_dselc( ( fldname = 'F0001' dyfldname = 'P_SPROJ' )
                                 ( fldname = 'F0002' dyfldname = 'C_SPROJT' )
                                 ( fldname = 'F0003' dyfldname = 'P_OBJMG' )
                                 ( fldname = 'F0004' dyfldname = 'C_OBJMGT' ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = vl_retfield
      dynpprog        = sy-cprog
      dynpnr          = '1000'
      dynprofield     = 'P_SPROJ'
      value_org       = 'S'
    TABLES
      field_tab       = tl_fields
      value_tab       = tg_migr
      return_tab      = tl_return
      dynpfld_mapping = tl_dynmap
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_init
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_init .

  c_sproj = 'Subproject'.
  c_objmg = 'Migration Object'.
  c_dir   = 'Target Directory'.
  c_1file = 'Single File (Template and Mapping)'.
  c_split = '2 Files (Template and Mapping)'.
  c_open  = 'Open XML file on Notepad'.
  c_alv   = 'Display report for LTMOM Mapping'.
  c_csv   = 'CSV format'.
  c_xml   = 'XML format'.

  SELECT s~ident, st~descr, o~ident, ot~descr, pp~ident, pt~descr
    INTO TABLE @tg_migr
    FROM dmc_sprjct AS s
    INNER JOIN /ltb/mc_proj AS p
      ON  p~uuid          EQ s~project
      AND p~to_be_deleted EQ @space
    INNER JOIN dmc_cobj AS o
      ON o~subproject EQ s~guid
    LEFT OUTER JOIN dmc_prjct AS pp
      ON  pp~guid  EQ s~project
    LEFT OUTER JOIN dmc_prjctt AS pt
      ON  pt~guid  EQ s~project
      AND pt~langu EQ @sy-langu
    LEFT OUTER JOIN dmcsprjctt AS st
      ON  st~guid  EQ s~guid
      AND st~langu EQ @sy-langu
    LEFT OUTER JOIN dmc_cobjt AS ot
      ON  ot~guid  EQ o~guid
      AND ot~langu EQ @sy-langu
    WHERE s~ident LIKE 'Z%'.

  LOOP AT tg_migr ASSIGNING FIELD-SYMBOL(<fl_w_migr>) WHERE sprjctt IS INITIAL
                                                         OR objmgt  IS INITIAL
                                                         OR prjctt  IS INITIAL.

    SELECT SINGLE pt~descr AS prjctt, st~descr AS sprjctt, ot~descr AS objmgt
      INTO @DATA(wl_text)
      FROM dmc_sprjct AS s
      INNER JOIN dmc_cobj AS o
        ON o~subproject EQ s~guid
      INNER JOIN dmcsprjctt AS st
        ON  st~guid  EQ s~guid
        AND st~langu EQ 'E'
      INNER JOIN dmc_cobjt AS ot
        ON  ot~guid  EQ o~guid
        AND ot~langu EQ 'E'
      LEFT OUTER JOIN dmc_prjctt AS pt
        ON  pt~guid  EQ s~project
        AND pt~langu EQ 'E'
      WHERE s~ident EQ @<fl_w_migr>-sprjct
        AND o~ident EQ @<fl_w_migr>-objmg.

    IF <fl_w_migr>-sprjctt IS INITIAL.
      <fl_w_migr>-sprjctt = wl_text-sprjctt.

    ENDIF.

    IF <fl_w_migr>-objmgt IS INITIAL.
      <fl_w_migr>-objmgt = wl_text-objmgt.

    ENDIF.

    IF <fl_w_migr>-prjctt IS INITIAL.
      <fl_w_migr>-prjctt = wl_text-prjctt.

    ENDIF.

  ENDLOOP.

  SORT tg_migr BY sprjctt.

  DATA vl_str TYPE string.

  CALL METHOD cl_gui_frontend_services=>get_desktop_directory
    CHANGING
      desktop_directory    = vl_str           " Desktop Directory
    EXCEPTIONS
      cntl_error           = 1                " Control error
      error_no_gui         = 2                " No GUI available
      not_supported_by_gui = 3                " GUI does not support this
      OTHERS               = 4.

  p_dir = vl_str.

  CALL METHOD cl_gui_cfw=>update_view.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_sel_screen
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_sel_screen .

  CASE sy-ucomm.
    WHEN 'ONLI' OR 'SJOB' OR 'PRIN'.
      IF p_dir IS INITIAL.
        PERFORM zf_get_dir CHANGING p_dir.

      ENDIF.

      IF p_dir IS INITIAL OR p_sproj IS INITIAL OR p_objmg IS INITIAL.
        MESSAGE e398(00) WITH 'Informe todos os campos'.

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  READ TABLE tg_migr INTO DATA(wl_migr) WITH KEY sprjct = p_sproj
                                                 objmg  = p_objmg.

  c_sprojt = wl_migr-sprjctt.
  c_objmgt = wl_migr-objmgt.
  gv_ident = p_objmg.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_scr_output
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_scr_output .

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'XML'.
        IF p_xml IS NOT INITIAL.
          screen-invisible = 0.
          screen-active    = 1.

        ELSE.
          screen-invisible = 1.
          screen-active    = 0.

        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zf_get_dir
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM zf_get_dir CHANGING p_p_dir.

  DATA: vl_str TYPE string.

  vl_str = p_dir.

  IF vl_str IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory    = vl_str           " Desktop Directory
      EXCEPTIONS
        cntl_error           = 1                " Control error
        error_no_gui         = 2                " No GUI available
        not_supported_by_gui = 3                " GUI does not support this
        OTHERS               = 4.

    CALL METHOD cl_gui_cfw=>update_view.

  ENDIF.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      initial_folder       = vl_str
    CHANGING
      selected_folder      = vl_str            " Folder Selected By User
    EXCEPTIONS
      cntl_error           = 1                " Control error
      error_no_gui         = 2                " No GUI available
      not_supported_by_gui = 3                " GUI does not support this
      OTHERS               = 4.

  p_dir = vl_str.

ENDFORM.