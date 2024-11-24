*&---------------------------------------------------------------------*
*& Report zconvert_forms_into_methods
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zconvert_forms_into_methods NO STANDARD PAGE HEADING LINE-SIZE 255.

CLASS lcl_app DEFINITION DEFERRED.

DATA gv_ok_code LIKE sy-ucomm.
DATA gv_input  TYPE string.
DATA gv_output TYPE string.

SELECT-OPTIONS s_prog FOR sy-repid.

START-OF-SELECTION.
  DATA app TYPE REF TO lcl_app.

  CREATE OBJECT app TYPE ('LCL_APP').
  CALL METHOD app->('MAIN').

FORM test_without_parameter.
  PERFORM test_without_parameter.
ENDFORM.

DATA gs_bapiret2 TYPE bapiret2.
FORM test_tables_structure TABLES a STRUCTURE gs_bapiret2.
  PERFORM test_tables_structure TABLES a.
ENDFORM.

DATA itab            TYPE TABLE OF sflight.
DATA struct_bapiret2 TYPE bapiret2.
TYPES itab_type TYPE TABLE OF sflight.

FORM test_with_parameters
  TABLES   generic_itab
           ddic_itab                 STRUCTURE sflight
           param_itab_1              STRUCTURE struct_bapiret2
           param_itab_2              LIKE itab
           param_itab_3              TYPE itab_type
  USING    generic_1
           VALUE(generic_2)
           complete                  TYPE i
           VALUE(complete_2)         TYPE i
           generic_itab_2            TYPE STANDARD TABLE
           itab_line                 TYPE LINE OF itab_type
           reference                 TYPE REF TO cl_ixml
           complete_like             LIKE itab
           itab_line_like            LIKE LINE OF itab
           reference_like            LIKE REF TO itab
  CHANGING generic_changing
           VALUE(generic_changing_2)
           complete_changing         TYPE i.

  " Compiler warning MESSAGEG@4 (table TRMSG):
  " Do not modify a USING reference parameter. Instead, define the parameter as a USING-VALUE(...) or CHANGING parameter.
  generic_1 = '1'. " <=== compiler warning
  generic_2 = '1'. " <=== no compiler warning
  generic_changing = '1'. " <=== no compiler warning
  DATA(reference_like_2) = reference_like.
  PERFORM test_with_parameters
    TABLES   generic_itab
             ddic_itab
             param_itab_1
             param_itab_2
             param_itab_3
    USING    generic_1
             generic_2
             complete
             complete_2
             generic_itab_2
             itab_line
             reference
             complete_like
             itab_line_like
    CHANGING reference_like_2 " <=== no compiler warning (passed as CHANGING instead of USING)
             generic_changing
             generic_changing_2
             complete_changing.
ENDFORM.

CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      pbo,
      pai.

  PRIVATE SECTION.
    CLASS-DATA:
      go_splitter TYPE REF TO cl_gui_splitter_container,
      go_text1    TYPE REF TO cl_gui_textedit,
      go_text2    TYPE REF TO cl_gui_textedit.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD pbo.

    DATA: lo_left  TYPE REF TO cl_gui_container,
          lo_right TYPE REF TO cl_gui_container.

    IF NOT go_splitter IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT go_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0
        rows    = 1
        columns = 2.

    lo_left  = go_splitter->get_container( row    = 1
                                           column = 1 ).
    lo_right = go_splitter->get_container( row    = 1
                                           column = 2 ).

    CREATE OBJECT go_text1
      EXPORTING
        parent = lo_left.
    go_text1->set_toolbar_mode( 0 ).
    go_text1->set_statusbar_mode( 0 ).
    go_text1->set_readonly_mode( 1 ).
    go_text1->set_font_fixed( ).
    go_text1->set_wordwrap_behavior(
      wordwrap_mode = cl_gui_textedit=>wordwrap_off ).
    go_text1->set_textstream( gv_input ).

    CREATE OBJECT go_text2
      EXPORTING
        parent = lo_right.
    go_text2->set_toolbar_mode( 0 ).
    go_text2->set_statusbar_mode( 0 ).
    go_text2->set_readonly_mode( 1 ).
    go_text2->set_font_fixed( ).
    go_text1->set_wordwrap_behavior(
      wordwrap_mode = cl_gui_textedit=>wordwrap_off ).
    go_text2->set_textstream( gv_output ).

    SET TITLEBAR 'TITLE_3000'.
    SET PF-STATUS 'STATUS_3000'.

  ENDMETHOD.

  METHOD pai.

    CASE gv_ok_code.
      WHEN 'BACK'.
        CLEAR gv_ok_code.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ENDMETHOD.

  METHOD run.

    CALL SCREEN 3000.

  ENDMETHOD.
ENDCLASS.

MODULE status_3000 OUTPUT.
  lcl_gui=>pbo( ).
ENDMODULE.

MODULE user_command_3000 INPUT.
  lcl_gui=>pai( ).
ENDMODULE.

CLASS lcx_shrinker DEFINITION
  INHERITING FROM cx_static_check FINAL.
ENDCLASS.


CLASS lcl_shrinker_abap_scan DEFINITION
*  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_syntax_check,
        itab  TYPE string_table,
        mess  TYPE string,
        lin   TYPE i,
        wrd   TYPE string,
        prog  TYPE syrepid,
        dir   TYPE trdir,
        incl  TYPE string,
        off   TYPE i,
        mid   TYPE trmsg_key,
        subrc TYPE sysubrc,
      END OF ty_syntax_check.
    TYPES:
      BEGIN OF ty_get_next_lines_of_statement,
        first_line_index TYPE i,
        last_line_index  TYPE i,
        "! Offset of whole_text where the statement starts
        offset           TYPE i,
        "! Length of whole_text corresponding to the statement
        length           TYPE i,
        "! Each line feed character is replaced with one space. May contain characters \r\n which each indicate one linefeed position.
        whole_text       TYPE string,
      END OF ty_get_next_lines_of_statement.
    TYPES:
      BEGIN OF ty_slevel,
        depth TYPE level_dpth,
        level TYPE level_levl,
        stmnt TYPE level_stmt,
        from  TYPE level_from,
        to    TYPE level_to,
        name  TYPE level_name,
        type  TYPE level_type,
      END OF ty_slevel.
    TYPES:
      BEGIN OF ty_sstruc,
        type       TYPE stru_type,
        stmnt_type TYPE stru_type,
        key_start  TYPE stru_keyw,
        key_end    TYPE stru_keyw,
        stmnt_from TYPE stru_from1,
        stmnt_to   TYPE stru_to1,
        struc_from TYPE stru_from2,
        struc_to   TYPE stru_to2,
        back       TYPE stru_back,
      END OF ty_sstruc.
    TYPES:
      "! Scan information about one statement
      BEGIN OF ty_sstmnt,
        "! Index of the "LEVEL" source unit which contains this statement
        level      TYPE stmnt_levl,
        "! Index of the "STRUC" block which contains this statement
        struc      TYPE stmnt_stru,
        "! Index of the first token of this statement in the token table (NB: the first token in the token table has index 1)
        from       TYPE stmnt_from,
        "! Index of the last token of this statement in the token table (NB: the first token in the token table has index 1)
        to         TYPE stmnt_to,
        "! Statement number in the LEVEL source unit (NB: the first statement in the LEVEL source unit has index 1)
        number     TYPE stmnt_nr,
        "! Row of the chained statement colon in the LEVEL source unit (>= 1 if TERMINATOR = ',' / 1 = first row, otherwise 0)
        colonrow   TYPE stmnt_crow,
        "! Row of terminator in the LEVEL source unit (>= 1 if TERMINATOR <> SPACE / 1 = first row, otherwise 0)
        trow       TYPE stmnt_trow,
        "! Column of the chained statement colon (>= 0 if TERMINATOR = ',' - 0 = first column, otherwise 0)
        coloncol   TYPE stmnt_ccol,
        "! Column of terminator (>= 0 if TERMINATOR <> SPACE - 0 = first column, otherwise 0)
        tcol       TYPE stmnt_tcol,
        "! Number of tokens before the colon (with chain statements >= 1, otherwise 0)
        prefixlen  TYPE stmnt_plen,
        "! The possible values are defined in the structured constant SCAN_STMNT_TYPE.
        type       TYPE stmnt_type,
        "! Terminator character (period if not a chained statement, comma if it's a chained statement)
        "! or empty for native SQL statements and internal macro definitions
        terminator TYPE stmnt_term,
        "! Index in the enhancement table of type SENHMT, if the statement was enhanced or originates completely from
        "! an enhancement implementation. If addition ENHANCEMENTS INTO itab is not specified, this value is always 0.
        enhmt      TYPE i,
      END OF ty_sstmnt.
    TYPES:
      BEGIN OF ty_stokes,
        str  TYPE string,
        row  TYPE token_row,
        col  TYPE token_col,
        "! The possible values are defined in the structured constant SCAN_TOKEN_TYPE.
        type TYPE token_type,
      END OF ty_stokes.
    TYPES ty_ut_slevel TYPE STANDARD TABLE OF ty_slevel WITH EMPTY KEY.
    TYPES ty_ut_sstruc TYPE STANDARD TABLE OF ty_sstruc WITH EMPTY KEY.
    TYPES ty_ut_stokes TYPE STANDARD TABLE OF ty_stokes WITH EMPTY KEY.
    TYPES ty_ut_sstmnt TYPE STANDARD TABLE OF ty_sstmnt WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_scan_result,
        slevel TYPE ty_ut_slevel,
        sstruc TYPE ty_ut_sstruc,
        sstmnt TYPE ty_ut_sstmnt,
        stokes TYPE ty_ut_stokes,
      END OF ty_scan_result.

    CONSTANTS:
      BEGIN OF c_pseudo_token,
        colon TYPE token_type      VALUE ':',
        comma TYPE token_type      VALUE ',',
        dot   TYPE token_type      VALUE '.',
        std   LIKE scan_token_type VALUE scan_token_type,
      END OF c_pseudo_token.
    CONSTANTS:
      BEGIN OF type,
        level        LIKE scan_level_type       VALUE scan_level_type,
        struc        LIKE scan_struc_type       VALUE scan_struc_type,
        struc_stmnt  LIKE scan_struc_stmnt_type VALUE scan_struc_stmnt_type,
        stmnt        LIKE scan_stmnt_type       VALUE scan_stmnt_type,
        token        LIKE scan_token_type       VALUE scan_token_type,
        pseudo_token LIKE c_pseudo_token        VALUE c_pseudo_token,
      END OF type.

    "! Get the ABAP statement located at a position (line and column) in a given ABAP source code.
    CLASS-METHODS get_abap_statement_at_cursor
      IMPORTING it_source       TYPE ty_abap_source_code
                VALUE(i_linenr) TYPE numeric
                VALUE(i_offset) TYPE numeric DEFAULT 0
      RETURNING VALUE(result)   TYPE ty_scan_result.

    "! Get the ABAP statement located at a line of a given ABAP source code.
    CLASS-METHODS get_whole_abap_statement
      IMPORTING line_index       TYPE i
                abap_source_code TYPE ty_abap_source_code
      RETURNING VALUE(result)    TYPE ty_get_next_lines_of_statement.

    CLASS-METHODS syntax_check
      IMPORTING abap_source_code TYPE ty_abap_source_code
      RETURNING VALUE(result)    TYPE ty_syntax_check
      RAISING   lcx_shrinker.

ENDCLASS.

*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

" -----------------------------------------------------------------------
" CLASS lcl_program_load DEFINITION
" -----------------------------------------------------------------------
CLASS lcl_program_load DEFINITION.
  PUBLIC SECTION.
    TYPES ty_t_rng_prog TYPE RANGE OF repoload-progname.
    TYPES ty_t_rng_udat TYPE RANGE OF repoload-udat.
    TYPES ty_t_rng_sdat TYPE RANGE OF repoload-sdat.
    TYPES : BEGIN OF ty_s_load_status,
              progname    TYPE repoload-progname,
              sdat        TYPE repoload-sdat,     " last time regen was requested
              stime       TYPE repoload-stime,
              udat        TYPE repoload-udat,     " last regen time
              utime       TYPE repoload-utime,
              stat        TYPE string,
              gen_result  TYPE sysubrc,
              gen_message TYPE string,
            END OF ty_s_load_status.
    TYPES ty_t_load_status TYPE TABLE OF ty_s_load_status WITH DEFAULT KEY.

    DATA ku_program TYPE string   READ-ONLY.
    DATA ku_date    TYPE sy-datum READ-ONLY.
    DATA ku_time    TYPE sy-uzeit READ-ONLY.

    "!
    "! @parameter it_rng_prog | Range of main programs (including classes, etc.)
    "! @parameter it_rng_udat | Last DDIC modification date
    "! @parameter it_rng_sdat | Last generation date
    "! @parameter i_nevergen  | <ul>
    "! <li>True = include the programs which have never been generated</li>
    "! <li>False = exclude the programs which have never been generated</li>
    "! </ul>
    METHODS select_prog
      IMPORTING it_rng_prog TYPE ty_t_rng_prog OPTIONAL
                it_rng_udat TYPE ty_t_rng_udat OPTIONAL
                it_rng_sdat TYPE ty_t_rng_sdat OPTIONAL
                i_nevergen  TYPE flag          DEFAULT abap_true.

    METHODS get_progs
      RETURNING VALUE(et_load_status) TYPE ty_t_load_status.

    METHODS get_number_of_progs
      RETURNING VALUE(e_number) TYPE i.

    "!
    "! @parameter nevergen | <ul>
    "! <li>false : remove the programs which have never been generated</li>
    "! </ul>
    "! @parameter toberege | <ul>
    "! <li>false : remove the programs which have to be generated</li>
    "! </ul>
    "! @parameter generatd | <ul>
    "! <li>false : remove the programs which don't need to be regenerated</li>
    "! </ul>
    METHODS filter_prog
      IMPORTING nevergen TYPE flag DEFAULT abap_true
                toberege TYPE flag DEFAULT abap_true
                generatd TYPE flag DEFAULT abap_false.

    METHODS gen_progs
      IMPORTING i_commit_frequency TYPE i.

    METHODS invalidate
      IMPORTING i_test TYPE flag.

    EVENTS program_generated
      EXPORTING
        VALUE(progname) TYPE progname
        VALUE(subrc)    TYPE sysubrc
        VALUE(counter)  TYPE i
        VALUE(total)    TYPE i.

  PRIVATE SECTION.
    DATA kit_load_status TYPE ty_t_load_status.

ENDCLASS.


CLASS lcl_abap_statement_at_cursor DEFINITION.

  PUBLIC SECTION.
    "! Return the statement at cursor position. If the cursor is before the colon of a chained
    "! statement, it will return several statements (all the ones of the chained statement).
    CLASS-METHODS get
      IMPORTING it_source       TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
                VALUE(i_linenr) TYPE numeric
                VALUE(i_offset) TYPE numeric DEFAULT 0
      RETURNING VALUE(result)   TYPE lcl_shrinker_abap_scan=>ty_scan_result.

  PRIVATE SECTION.
    TYPES:
      "! Scan information about one "pseudo-statement" (line scan)
      BEGIN OF ts_pseudo_sstmnt,
        "! Index of the first token of this statement in the token table (NB: the first token in the token table has index 1).
        "! It may be zero if the pseudo-statement has no token at all (empty line, dot alone, etc.)
        from       TYPE stmnt_from,
        "! Index of the last token of this statement in the token table (NB: the first token in the token table has index 1)
        to         TYPE stmnt_to,
        "! Statement number in the LEVEL source unit (NB: the first statement in the LEVEL source unit has index 1)
        number     TYPE stmnt_nr,
        "! Row of the chained statement colon in the LEVEL source unit (>= 1 if TERMINATOR = ',' / 1 = first row, otherwise 0)
        colonrow   TYPE stmnt_crow,
        "! Row of terminator in the LEVEL source unit (>= 1 if TERMINATOR <> SPACE / 1 = first row, otherwise 0)
        trow       TYPE stmnt_trow,
        "! Column of the chained statement colon (>= 0 if TERMINATOR = ',' - 0 = first column, otherwise 0)
        coloncol   TYPE stmnt_ccol,
        "! Column of terminator (>= 0 if TERMINATOR <> SPACE - 0 = first column, otherwise 0)
        tcol       TYPE stmnt_tcol,
        "! Number of tokens before the colon (with chain statements >= 1, otherwise 0)
        prefixlen  TYPE stmnt_plen,
        "! Terminator character (period if not a chained statement, comma if it's a chained statement)
        "! or empty for native SQL statements and internal macro definitions
        terminator TYPE stmnt_term,
        zz_1st_row TYPE i,
        zz_1st_col TYPE i,
      END OF ts_pseudo_sstmnt.
    TYPES:
      "! A pseudo-token may also contain comma, dot and colon.
      BEGIN OF ty_pseudo_token,
        str  TYPE string,
        row  TYPE token_row,
        col  TYPE token_col,
        "! The possible values are defined in the structured constant C_PSEUDO_TOKEN.
        type TYPE token_type,
      END OF ty_pseudo_token.
    TYPES ty_pseudo_tokens TYPE STANDARD TABLE OF ty_pseudo_token WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_line_scan,
        linenr        TYPE i,
        pseudo_tokens TYPE ty_pseudo_tokens,
      END OF ty_line_scan.
    TYPES ty_ref_line_scan TYPE REF TO ty_line_scan.
    TYPES:
      BEGIN OF ty_parsed_line_statement,
        parsed_linenr   TYPE i,
        ref_parsed_line TYPE ty_ref_line_scan,
        tabix_sstmnt    TYPE sytabix,
      END OF ty_parsed_line_statement.
    TYPES:
      BEGIN OF ty_raw_scan_lines_around,
        pseudo_tokens TYPE ty_pseudo_tokens,
      END OF ty_raw_scan_lines_around.

    CONSTANTS type LIKE lcl_shrinker_abap_scan=>type VALUE lcl_shrinker_abap_scan=>type.

    CLASS-METHODS parse_line
      IMPORTING i_line        TYPE csequence
                i_linenr      TYPE numeric DEFAULT 0
      RETURNING VALUE(result) TYPE ty_line_scan.

    "!
    "! @parameter it_source | X
    "! @parameter i_linenr  | X
    "! @parameter i_offset  | X
    "! @parameter result    | NB: because RESULT contains references to self-contained data, to avoid these references to be FREED, it was required to:
    "!                          <ul>
    "!                          <li>EITHER define it as a data reference, i.e. create it via CREATE DATA,</li>
    "!                          <li>OR not pass it by value, i.e. use EXPORTING instead of RETURNING.</li>
    "!                          </ul>
    CLASS-METHODS raw_scan_lines_around
      IMPORTING it_source       TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
                VALUE(i_linenr) TYPE numeric
                VALUE(i_offset) TYPE numeric DEFAULT 0
      EXPORTING !result         TYPE ty_raw_scan_lines_around.

    "! Input is raw lines around the cursor, output is only one statement or several if cursor is before the colon ":" of a chained statement.
    "! @parameter raw_scan_lines_around | Raw lines, may contain several statements
    "! @parameter i_linenr              | Cursor row
    "! @parameter i_offset              | Cursor column
    "! @parameter result                | In general only one statement is returned
    CLASS-METHODS rework_raw_scan_lines
      IMPORTING raw_scan_lines_around TYPE ty_raw_scan_lines_around
                i_linenr              TYPE numeric
                i_offset              TYPE numeric
                remove_comments       TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result)         TYPE lcl_shrinker_abap_scan=>ty_scan_result.

ENDCLASS.


CLASS lcl_uuid DEFINITION.

  PUBLIC SECTION.
    CLASS-DATA uuid_generator TYPE REF TO if_system_uuid.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_c26
      RETURNING VALUE(result) TYPE sysuuid_c26.

    CLASS-METHODS get_x16
      RETURNING VALUE(result) TYPE sysuuid_x16.

ENDCLASS.


CLASS lcl_shrinker_abap_scan IMPLEMENTATION.
  METHOD get_abap_statement_at_cursor.
    result = lcl_abap_statement_at_cursor=>get( it_source = it_source
                                                i_linenr  = i_linenr
                                                i_offset  = i_offset ).
  ENDMETHOD.

  METHOD get_whole_abap_statement.
    DATA(abap_statement) = get_abap_statement_at_cursor( it_source = abap_source_code
                                                         i_linenr  = line_index ).
    IF abap_statement-stokes IS INITIAL.
      result = VALUE #( first_line_index = line_index
                        last_line_index  = line_index
                        whole_text       = '' ).
    ELSE.
      result = VALUE #(
          LET tabix_last_stokes = lines( abap_statement-stokes )
              whole_text        = concat_lines_of(
                                      sep   = |\r\n|
                                      table = VALUE string_table( ( LINES OF
                                                                    abap_source_code
                                                                    FROM
                                                                    abap_statement-stokes[ 1 ]-row
                                                                    TO
                                                                    abap_statement-stokes[ tabix_last_stokes ]-row ) ) )
          IN  first_line_index = abap_statement-stokes[ 1 ]-row
              last_line_index  = abap_statement-stokes[ tabix_last_stokes ]-row
              whole_text       = whole_text
              offset           = abap_statement-stokes[ 1 ]-col
              length           = strlen( whole_text ) - abap_statement-stokes[ 1 ]-col ).
    ENDIF.
  ENDMETHOD.

  METHOD syntax_check.
    DATA(synt) = VALUE ty_syntax_check( dir = VALUE #( name    = '$$DUMMY'
                                                       subc    = '1'
                                                       fixpt   = 'X'
                                                       uccheck = 'X' ) ).
    " X   VARCL *S   DBAPL *D$  DBNA
    SYNTAX-CHECK FOR abap_source_code MESSAGE synt-mess LINE synt-lin WORD synt-wrd DIRECTORY ENTRY synt-dir INCLUDE synt-incl OFFSET synt-off MESSAGE-ID synt-mid.
    synt-subrc = sy-subrc.
    " SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd
    "                  [PROGRAM prog] [DIRECTORY ENTRY dir]
    "                  [WITH CURRENT SWITCHSTATES]
    " ... [INCLUDE incl]
    "     [OFFSET off]
    "     [MESSAGE-ID mid] ...

    result = synt.
  ENDMETHOD.
ENDCLASS.

*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_program_load IMPLEMENTATION.
  METHOD select_prog.
    DATA ls_load_status TYPE ty_s_load_status.
    " TODO: variable is never used (ABAP cleaner)
    DATA: BEGIN OF ls_trdir_partial,
            name TYPE trdir-name,
          END OF ls_trdir_partial.
    DATA l_utimstp TYPE tzntimestp.
    DATA l_stimstp TYPE tzntimestp.

    DATA(lt_rng_prog) = it_rng_prog.
    WHILE lt_rng_prog IS NOT INITIAL.
      DATA(lt_small_rng_prog) = VALUE ty_t_rng_prog( ( LINES OF lt_rng_prog FROM 1 TO 100 ) ).
      DELETE lt_rng_prog FROM 1 TO 100.
      SELECT trdir~name,
             repoload~sdat,
             repoload~stime,
             repoload~udat,
             repoload~utime
        FROM trdir
               LEFT OUTER JOIN
                 repoload ON trdir~name = repoload~progname
        WHERE trdir~name IN @lt_small_rng_prog
          AND trdir~subc <> 'I'
          AND ( repoload~udat IS NULL OR repoload~udat IN @it_rng_udat )
          AND ( repoload~sdat IS NULL OR repoload~sdat IN @it_rng_sdat )
        APPENDING TABLE @kit_load_status.
    ENDWHILE.

    IF i_nevergen = abap_false.
      DELETE kit_load_status WHERE sdat IS INITIAL.
    ENDIF.

    LOOP AT kit_load_status INTO ls_load_status.
      IF ls_load_status-sdat IS INITIAL.
        ls_load_status-stat = 'NO_LOAD'.
      ELSE.
        CONCATENATE ls_load_status-udat ls_load_status-utime INTO l_utimstp.
        CONCATENATE ls_load_status-sdat ls_load_status-stime INTO l_stimstp.
        IF l_utimstp < l_stimstp.
          ls_load_status-stat = 'INVALID_LOAD'.
        ELSE.
          ls_load_status-stat = 'VALID_LOAD'.
        ENDIF.
      ENDIF.
      MODIFY kit_load_status FROM ls_load_status TRANSPORTING stat.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_progs.
    et_load_status = kit_load_status.
  ENDMETHOD.

  METHOD get_number_of_progs.
    e_number = lines( kit_load_status ).
  ENDMETHOD.

  METHOD filter_prog.
    DATA ls_load_status TYPE ty_s_load_status.
    DATA g_do_generate  TYPE flag.

    LOOP AT kit_load_status INTO ls_load_status.

      CLEAR g_do_generate.
      IF ls_load_status-stat = 'NO_LOAD' AND nevergen = 'X'.
        g_do_generate = 'X'.
      ELSEIF ls_load_status-stat = 'INVALID_LOAD' AND toberege = 'X'.
        g_do_generate = 'X'.
      ELSEIF ls_load_status-stat = 'VALID_LOAD' AND generatd = 'X'.
        g_do_generate = 'X'.
      ENDIF.

      IF g_do_generate IS INITIAL.
        DELETE kit_load_status.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD gen_progs.
    DATA ls_load_status TYPE ty_s_load_status.
    DATA l_commit_count TYPE i.

    LOOP AT kit_load_status INTO ls_load_status.
      GENERATE REPORT ls_load_status-progname MESSAGE ls_load_status-gen_message.
      ls_load_status-gen_result = sy-subrc.
      MODIFY kit_load_status FROM ls_load_status.
      RAISE EVENT program_generated EXPORTING progname = ls_load_status-progname
                                              subrc    = sy-subrc
                                              counter  = sy-tabix
                                              total    = lines( kit_load_status ).
      " the following is needed to avoid PXA_NO_FREE_SPACE dump
      " (see note 302500 Transaction SGEN terminates with PXA_NO_FREE_SPACE)
      " when there are many GENERATE REPORT executed
      l_commit_count = l_commit_count + 1.
      IF i_commit_frequency > 0 AND l_commit_count = i_commit_frequency.
        COMMIT WORK.
        l_commit_count = 0.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD invalidate.
    DATA ls_load_status TYPE ty_s_load_status.

    ls_load_status-sdat  = sy-datum.
    ls_load_status-stime = sy-uzeit.
    ls_load_status-stat  = 'INVALID_LOAD'.
    MODIFY kit_load_status FROM ls_load_status TRANSPORTING sdat stime stat
           WHERE stat <> ls_load_status-stat.
    ku_date = ls_load_status-sdat.
    ku_time = ls_load_status-stime.

    IF i_test IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT kit_load_status INTO ls_load_status.
      TRY.
          ku_program = ls_load_status-progname.
          EXEC SQL.
            UPDATE repoload
                  SET sdat = :ku_date
                      stime = :ku_time
                  WHERE prog = :ku_program
          ENDEXEC.
          DATA exc_ref TYPE REF TO cx_sy_native_sql_error.
        CATCH cx_sy_native_sql_error INTO exc_ref.
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA error_text TYPE string.
          error_text = exc_ref->get_text( ).
      ENDTRY.
    ENDLOOP.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_abap_statement_at_cursor IMPLEMENTATION.
  METHOD get.
    IF '*' = substring( val = it_source[ i_linenr ]
                        off = 0
                        len = 1 ).
      RETURN.
    ENDIF.

    raw_scan_lines_around( EXPORTING it_source = it_source
                                     i_linenr  = i_linenr
                                     i_offset  = i_offset
                           IMPORTING result    = DATA(raw_scan_lines_around) ).

    result = rework_raw_scan_lines( raw_scan_lines_around = raw_scan_lines_around
                                    i_linenr              = i_linenr
                                    i_offset              = i_offset ).
  ENDMETHOD.

  METHOD parse_line.
    TYPES ty_char_1 TYPE c LENGTH 1.
    DATA l_empty_chars    TYPE string.
    DATA l_offset         TYPE i.
    DATA l_offset2        TYPE i.
    DATA l_length         TYPE i.
    DATA l_length2        TYPE i.
    DATA l_state          TYPE i.
    DATA l_delimiter      TYPE c LENGTH 1.
    DATA ls_current_token TYPE lcl_shrinker_abap_scan=>ty_stokes.
    DATA l_is_in_token    TYPE abap_bool.
    DATA l_end_of_line    TYPE abap_bool.
    DATA l_end_of_token   TYPE abap_bool.
    DATA l_concat         TYPE abap_bool.
    DATA l_character_2    TYPE ty_char_1.

    result-linenr = i_linenr.

    l_concat = abap_false.

    CONCATENATE ` ` cl_abap_char_utilities=>horizontal_tab INTO l_empty_chars.

    IF i_line IS INITIAL OR i_line CO l_empty_chars.
      " line is empty
      RETURN.
    ENDIF.

    IF i_line(1) = '*'.
      " line is a comment
      result = VALUE #( pseudo_tokens = VALUE #( ( str  = i_line
                                                   row  = i_linenr
                                                   col  = 0
                                                   type = scan_token_type-comment ) ) ).
      RETURN.
    ENDIF.

    l_offset = 0.
    l_state = 0.
    l_end_of_line = abap_false.
    l_end_of_token = abap_false.

    DO.

      DATA(l_character) = VALUE ty_char_1( ).
      IF l_offset < strlen( i_line ).
        l_character = i_line+l_offset(1).
        l_length = 1.
      ELSE.
        l_character = space.
        l_end_of_token = abap_true.
        l_end_of_line = abap_true.
        l_length = 0.
      ENDIF.

      l_is_in_token = abap_false.
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(l_colon_detected) = abap_false.

      CASE l_state.
        WHEN 0.
          CASE l_character.
            WHEN ':'.
              l_colon_detected = abap_true.
              l_end_of_token = abap_true.
            WHEN '.'.
              l_end_of_token = abap_true.
            WHEN ','.
              l_end_of_token = abap_true.
            WHEN '"'.
              " remaining characters of the line are a comment
              IF ls_current_token IS NOT INITIAL.
                l_length = 0.
                l_end_of_token = abap_true.
              ELSE.
                l_end_of_token = abap_true.
                l_end_of_line = abap_true.
                " the comment is stored without the leading double quote (what if it's a double quote without comment behind?)
                DATA(end_comment) = substring( val = i_line
                                               off = l_offset + 1 ).
                " NB: completely ignore the double quote without a comment
                IF end_comment IS NOT INITIAL.
                  ls_current_token-str  = substring( val = i_line
                                                     off = l_offset + 1 ).
                  ls_current_token-type = type-token-comment.
                ENDIF.
              ENDIF.
            WHEN '#'.
              ls_current_token-type = type-token-pragma.
            WHEN '''' OR '`'.
              " start of text or string literal
              IF ls_current_token IS NOT INITIAL.
                " special case no space before quote (var ='').
                " ('DYNAMIC') -> continue current token
                IF '(' = substring( val = ls_current_token-str
                                    off = strlen( ls_current_token-str ) - 1 ).
                  l_is_in_token = abap_true.
                ELSE.
                  l_length = 0.
                  l_end_of_token = abap_true.
                ENDIF.
              ELSE.
                l_state = 1.
                l_is_in_token = abap_true.
                l_delimiter = l_character.
                ls_current_token-col  = l_offset.
                ls_current_token-type = type-token-literal. " type-token-literal_with_ampersand ?
              ENDIF.
            WHEN '|'.
              " start of string template
              l_state = 2.
              l_end_of_token = abap_true.
            WHEN '}'.
              IF ls_current_token IS INITIAL.
                l_state = 2.
                l_end_of_token = abap_true.
              ELSE.
                " TODO error
              ENDIF.
            WHEN space OR cl_abap_char_utilities=>horizontal_tab.
              " token separator
              l_end_of_token = abap_true.
            WHEN '&'.
              " could be & or && or &[1-9] in macros
              l_offset2 = l_offset + 1.
              l_character_2 = COND #( WHEN l_offset2 < strlen( i_line )
                                      THEN i_line+l_offset2(1)
                                      ELSE space ).
              IF l_character_2 = '&'.
                " &&
                l_length = 2.
                l_is_in_token = abap_true.
                l_end_of_token = abap_true.
              ELSEIF l_character_2 CO '123456789'.
                " &[1-9] in macros
                l_is_in_token = abap_true.
              ELSE.
                " &
                " previous token must be continued
                l_concat = abap_true.
                l_state = 6.
                l_end_of_token = abap_true.
              ENDIF.
            WHEN '('.
              " 'Textsymbol'(001)
              " CALL METHOD (DYNAMIC)
              " SELECT ... WHERE column IN ('A',1)
              " method( )
              " variable(length)
              " A = ( 1 + 2 ).
              " ASSIGN (ZIF=>C)=>(MEMBER)
              " ASSIGN (ZIF=>C)=>('MEMBER')
              l_offset2 = l_offset + 1.
              l_character_2 = COND #( WHEN l_offset2 < strlen( i_line )
                                      THEN i_line+l_offset2(1)
                                      ELSE space ).
              IF ls_current_token IS INITIAL.
                IF l_character_2 = space.
                  l_is_in_token = abap_true.
                  l_end_of_token = abap_true.
                ELSE.
                  " start of list
                  l_state = 7.
                  l_is_in_token = abap_true.
                  ls_current_token-col  = l_offset.
                  ls_current_token-type = type-pseudo_token-std-list.
                ENDIF.
              ELSE.
                IF l_character_2 = space.
                  " method( )
                  l_is_in_token = abap_true.
                  l_end_of_token = abap_true.
                ELSE.
                  " variable(length)
                  " ASSIGN (ZIF=>C)=>(MEMBER)
                  " ASSIGN (ZIF=>C)=>('MEMBER')
                  l_state = 7.
                  l_is_in_token = abap_true.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              " "payload" character
              l_is_in_token = abap_true.
              IF ls_current_token IS INITIAL.
                ls_current_token-type = type-token-identifier.
                ls_current_token-col  = l_offset.
              ENDIF.
          ENDCASE.

        WHEN 1.
          "=============================================
          " we are inside a text or string literal
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF     l_offset2           < strlen( i_line )
               AND i_line+l_offset2(1) = l_delimiter.
              " doubled delimiter, so process the 2 subsequent delimiters as one normal
              " character of value "delimiter" (either ' or `)
              l_length = 2.
            ELSE.
              " end of text or string literal
              l_offset2 = l_offset + 1.
              IF     l_offset2           < strlen( i_line )
                 AND i_line+l_offset2(1) = '('.
                " aa = 'text symbol'(001).
                l_end_of_token = abap_false.
              ELSE.
                l_end_of_token = abap_true.
              ENDIF.
              l_state = 0.
              " note: important to keep L_DELIMITER value in case followed with literal operator (&)
            ENDIF.
          ENDIF.

        WHEN 2.
          "=============================================
          " we are inside a string template |...|
          " (or after } which means the next possible character is same after the opening pipe)
          "=============================================
          CASE l_character.
            WHEN '|'.
              " end of string template
              l_state = 0.
              l_end_of_token = abap_true.
            WHEN '{'.
              " start of a string template expression
              l_state = 3.
              l_end_of_token = abap_true.
            WHEN '\'.
              " it's an escape character; ignore both this one and the next one
              l_length = 2.
          ENDCASE.

        WHEN 3.
          "=============================================
          " we are inside a string template expression |...{ ... }...|
          "=============================================
          CASE l_character.
            WHEN ''''
                  OR '`'.
              l_state = 4.
              l_delimiter = l_character.
              l_is_in_token = abap_true.
            WHEN '}'.
              " end of string template expression
              l_state = 2.
              l_end_of_token = abap_true.
            WHEN space OR cl_abap_char_utilities=>horizontal_tab.
              " token separator
              l_end_of_token = abap_true.
            WHEN OTHERS.
              l_is_in_token = abap_true.
              IF ls_current_token IS INITIAL.
                ls_current_token-type = type-token-identifier.
                ls_current_token-col  = l_offset.
              ENDIF.
          ENDCASE.

        WHEN 4.
          "=============================================
          " we are inside a text or string literal within a string expression expression |...{ ...'...'... }...|
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF l_character_2 = l_delimiter.
              " doubled delimiter = consider a single occurrence as being part of the literal
              l_length = 2.
            ELSE.
              " end of literal
              l_state = 3.
            ENDIF.
          ENDIF.

        WHEN 6.
          "=============================================
          " after literal operator (&), we are expecting the same separator
          " (either '...' & '...' or `...` & `...`)
          "=============================================
          IF l_character CO l_empty_chars.
            " do nothing, process next character
          ELSEIF l_character = l_delimiter.
            " continue literal
            l_state = 1.
          ELSE.
            " error ( this is an invalid syntax: 'literal' &[\s]*[^'] )
            " TODO
          ENDIF.

        WHEN 7.
          "=============================================
          " start of list (1,2,3) or dynamic name
          "=============================================
          " (AB)
          " ('AB')
          " (AB)=>(CD)
          " (AB)=>('CD')
          " AB=>('CD')
          l_is_in_token = abap_true.
          IF ls_current_token IS INITIAL.
            ls_current_token-type = type-token-list.
          ENDIF.
          CASE l_character.
            WHEN '''' OR '`'.
              l_state = 8.
              l_delimiter = l_character.
            WHEN ')'.
              " TODO ERROR
            WHEN ' '.
              " TODO ERROR
            WHEN OTHERS.
              l_state = 9.
          ENDCASE.

        WHEN 8.
          "=============================================
          " we are inside a text or string literal within a list ('A','B')
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF l_offset2 < strlen( i_line ) AND i_line+l_offset2(1) = l_delimiter.
              " doubled delimiter = consider a single occurrence as being part of the literal
              l_length = 2.
            ELSE.
              " end of literal
              l_state = 10.
            ENDIF.
          ENDIF.

        WHEN 9.
          "=============================================
          " we are inside a non-text/string literal within a list (123,constant,variable)
          "=============================================
          CASE l_character.
            WHEN ' ' OR ','.
              l_is_in_token = abap_true.
            WHEN ')'.
              l_is_in_token = abap_true.
              l_state = 12.
            WHEN OTHERS.
              l_is_in_token = abap_true.
          ENDCASE.

        WHEN 10.
          "=============================================
          " After a literal within a list (1,'A'...
          "=============================================
          CASE l_character.
            WHEN ' '.
            WHEN ')'.
              l_is_in_token = abap_true.
              l_state = 12.
            WHEN ','.
              l_is_in_token = abap_true.
              l_state = 11.
            WHEN OTHERS.
              " TODO ERROR
          ENDCASE.

        WHEN 11.
          "=============================================
          " After comma within a list ('A','B')
          "=============================================
          l_is_in_token = abap_true.
          ls_current_token-type = type-token-list.
          CASE l_character.
            WHEN '''' OR '`'.
              l_state = 8.
              l_delimiter = l_character.
            WHEN OTHERS.
              l_state = 9.
          ENDCASE.

        WHEN 12.
          "=============================================
          " Right after RIGHT PARENTHESIS within a list ('A','B')
          "=============================================
          " call method (xxxxx)=>xxxxxxxx : 3 tokens, (..)=>.. is just one token of type List.
          IF ls_current_token-str CA ','.
            l_end_of_token = abap_true.
            l_state = 0.
          ELSEIF     ls_current_token-str NA space
                 AND l_character          CA '-='.
            l_offset2 = l_offset + 1.
            IF     l_offset2           < strlen( i_line )
               AND i_line+l_offset2(1) = '>'.
              l_is_in_token = abap_true.
              l_state = 0.
            ELSE.
              l_end_of_token = abap_true.
              l_state = 0.
            ENDIF.
          ELSE.
            l_end_of_token = abap_true.
            l_state = 0.
          ENDIF.

      ENDCASE.

      IF l_is_in_token = abap_true.
        IF ls_current_token IS INITIAL.
          " start of token
          ls_current_token-col = l_offset.
        ENDIF.
        IF l_offset + l_length > strlen( i_line ).
          " i_line+l_offset(l_length) would trigger CX_SY_RANGE_OUT_OF_BOUNDS
          ASSERT 1 = 1. " debug helper
        ENDIF.
        ls_current_token-str = ls_current_token-str && i_line+l_offset(l_length).
      ENDIF.

      IF l_end_of_token = abap_true.
        IF ls_current_token IS NOT INITIAL.
          IF l_concat = abap_true.
            " 'hello' & ' world' must be interpreted like 'hello world'
            DESCRIBE TABLE result-pseudo_tokens.
            READ TABLE result-pseudo_tokens INDEX sy-tfill INTO ls_current_token.
            DELETE result-pseudo_tokens INDEX sy-tfill.
            l_length2 = strlen( ls_current_token-str ) - 1.
            ls_current_token-str = ls_current_token-str(l_length2).
          ENDIF.
          IF ls_current_token-type = type-pseudo_token-std-list.
            " SELECT ... WHERE col IN (AB,1,'A',`B`,'Paul''s')
            FIND ALL OCCURRENCES OF
                 REGEX `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.` ##REGEX_POSIX
                 IN ls_current_token-str
                 RESULTS DATA(matches).
            LOOP AT matches REFERENCE INTO DATA(match).
              IF ls_current_token-str+match->offset(1) NA |(),'`|.
                REPLACE SECTION OFFSET match->offset LENGTH match->length OF ls_current_token-str
                        WITH to_upper( ls_current_token-str+match->offset(match->length) ).
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF     ls_current_token-type  = type-pseudo_token-std-list
             AND ls_current_token-str  CS ','.
            " SELECT ... WHERE col IN (AB,1,'A',`B`,'Paul''s')
*            FIND ALL OCCURRENCES OF
*                REGEX `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.` ##regex_posix
*                IN ls_current_token-str
*                RESULTS matches.
            result-pseudo_tokens = VALUE #( FOR <match> IN matches
                                            ( str  = ls_current_token-str+<match>-offset(<match>-length)
                                              col  = ls_current_token-col + <match>-offset
                                              type = type-pseudo_token-std-identifier ) ).
          ELSE.
            IF ls_current_token-str IS INITIAL.
              ASSERT 1 = 1. " debug helper
            ENDIF.
            IF     ls_current_token-type    = type-pseudo_token-std-identifier
               AND ls_current_token-str(1) NA '''`'.
              TRANSLATE ls_current_token-str TO UPPER CASE.
            ENDIF.
            ls_current_token-row = i_linenr.
            APPEND ls_current_token TO result-pseudo_tokens.
          ENDIF.
        ENDIF.

        DATA(pseudo_token_type) = SWITCH token_type( l_character
                                                     WHEN '.' THEN type-pseudo_token-dot
                                                     WHEN ',' THEN type-pseudo_token-comma
                                                     WHEN ':' THEN type-pseudo_token-colon
                                                     WHEN '}' THEN type-pseudo_token-std-identifier
                                                     WHEN '{' THEN type-pseudo_token-std-identifier
                                                     WHEN '|' THEN type-pseudo_token-std-identifier ).
        IF pseudo_token_type IS NOT INITIAL.
          APPEND VALUE #( str  = l_character
                          row  = i_linenr
                          col  = l_offset
                          type = pseudo_token_type )
                 TO result-pseudo_tokens.
        ENDIF.

        CLEAR ls_current_token.
        l_end_of_token = abap_false.
        l_concat = abap_false.
      ENDIF.

*      IF l_end_of_statement = abap_true.
*        l_end_of_statement = abap_false.
*      ENDIF.

      IF l_end_of_line = abap_true.
        EXIT.
      ENDIF.

      l_offset = l_offset + l_length.
    ENDDO.
  ENDMETHOD.

  METHOD raw_scan_lines_around.
    DATA pseudo_token TYPE REF TO lcl_abap_statement_at_cursor=>ty_pseudo_token.

    result = VALUE #( ).

    DATA(parsed_line) = VALUE ty_line_scan( ).

    "=======================================================
    " Go backwards from cursor till finding the previous statement (one ending with period).
    "=======================================================
    DATA(l_linenr) = EXACT i( i_linenr ) + 1.
    WHILE l_linenr > 1.
      l_linenr = l_linenr - 1.

      parsed_line = parse_line( i_line   = it_source[ l_linenr ]
                                i_linenr = l_linenr ).

      " Backup parsing of i_linenr to avoid parsing again during forward parsing.
      IF l_linenr = i_linenr.
        DATA(parsed_line_of_requested_line) = parsed_line.
      ELSE.
        INSERT LINES OF parsed_line-pseudo_tokens INTO result-pseudo_tokens INDEX 1.
      ENDIF.

      LOOP AT parsed_line-pseudo_tokens TRANSPORTING NO FIELDS
           WHERE     str = '.'
                 AND (    row < i_linenr
                       OR (     row = i_linenr
                            AND col < i_offset ) ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    DATA(tabix_dot) = line_index( result-pseudo_tokens[ str = '.' ] ). " REFERENCE INTO pseudo_token
    IF tabix_dot >= 1.
      DELETE result-pseudo_tokens TO tabix_dot.
      tabix_dot = 0.
    ENDIF.
    IF result-pseudo_tokens IS NOT INITIAL.
      LOOP AT result-pseudo_tokens
           TRANSPORTING NO FIELDS
           FROM tabix_dot + 1
           WHERE type <> type-pseudo_token-std-comment.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        " Only comments after the dot, delete them.
        DELETE result-pseudo_tokens FROM tabix_dot + 1.
      ENDIF.
    ENDIF.

    "=======================================================
    " Go forward from the cursor till finding end of statement.
    "=======================================================
    DATA(terminators) = `.,`.
    DATA(terminators_determined) = abap_false.

    DATA(terminator_found) = abap_false.
    l_linenr = i_linenr - 1.

    WHILE     terminator_found = abap_false
          AND l_linenr         < lines( it_source ).
      l_linenr = l_linenr + 1.

      IF l_linenr = i_linenr.
        parsed_line = parsed_line_of_requested_line.
      ELSE.
        parsed_line = parse_line( i_line   = it_source[ l_linenr ]
                                  i_linenr = l_linenr ).
      ENDIF.

      LOOP AT parsed_line-pseudo_tokens REFERENCE INTO pseudo_token.

        APPEND pseudo_token->* TO result-pseudo_tokens.

        IF terminators_determined = abap_false.
          IF     strlen( pseudo_token->str )  = 1
             AND pseudo_token->str           CA ',.:'
             AND (    pseudo_token->row > i_linenr
                   OR (     pseudo_token->row  = i_linenr
                        AND pseudo_token->col >= i_offset ) ).
            IF pseudo_token->str = ':'.
              terminators = `.`.
            ELSE.
              terminators = '.,'.
            ENDIF.
            terminators_determined = abap_true.
          ENDIF.
        ENDIF.

        IF     terminators_determined       = abap_true
           AND strlen( pseudo_token->str )  = 1
           AND pseudo_token->str           CA terminators.
          terminator_found = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF terminator_found = abap_true.
        EXIT.
      ENDIF.

    ENDWHILE.
  ENDMETHOD.

  METHOD rework_raw_scan_lines.
    DATA pseudo_token TYPE REF TO ty_pseudo_token.

    DATA(pseudo_tokens) = raw_scan_lines_around-pseudo_tokens.

    IF remove_comments = abap_true.
      DELETE pseudo_tokens WHERE type = type-pseudo_token-std-comment.
    ENDIF.

    READ TABLE pseudo_tokens REFERENCE INTO pseudo_token
         WITH KEY row = i_linenr
                  col = i_offset
         BINARY SEARCH.
    DATA(tabix_word_at_cursor) = sy-tabix.

    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
         FROM tabix_word_at_cursor
         WHERE str = '.'.
      DELETE pseudo_tokens FROM sy-tabix + 1.
      EXIT.
    ENDLOOP.

    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
         FROM 1
         TO tabix_word_at_cursor - 1
         WHERE str = '.'.
      tabix_word_at_cursor = tabix_word_at_cursor - sy-tabix + 1.
      DELETE pseudo_tokens FROM 1 TO sy-tabix.
      EXIT.
    ENDLOOP.

    DATA(line_sstmnt) = VALUE lcl_shrinker_abap_scan=>ty_sstmnt( ).
    DATA(sstmnt) = VALUE lcl_shrinker_abap_scan=>ty_ut_sstmnt( ).
    DATA(line_stokes) = VALUE lcl_shrinker_abap_scan=>ty_stokes( ).
    DATA(stokes) = VALUE lcl_shrinker_abap_scan=>ty_ut_stokes( ).

    DATA(tabix_colon) = line_index( pseudo_tokens[ str = ':' ] ).

    IF tabix_colon > 0 AND tabix_word_at_cursor > tabix_colon.

      DATA(tabix_comma) = 0.
      LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
           FROM tabix_colon + 1
           TO tabix_word_at_cursor
           WHERE str = ','.
        tabix_comma = sy-tabix.
      ENDLOOP.
      IF tabix_comma > 0.
        tabix_word_at_cursor = tabix_word_at_cursor - tabix_comma + tabix_colon + 1.
        DELETE pseudo_tokens FROM tabix_colon + 1 TO tabix_comma.
      ENDIF.
      LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
           FROM tabix_word_at_cursor
           WHERE str = ','.
        " Delete all tokens from this comma to the last token except the last one (dot)
        DELETE pseudo_tokens FROM sy-tabix TO lines( pseudo_tokens ) - 1.
        EXIT.
      ENDLOOP.

    ENDIF.

    DATA(colonrow) = 0.
    DATA(coloncol) = 0.
    DATA(from_token) = 1.
    DATA(prefixlen) = 0.
    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token.
      CASE pseudo_token->str.
        WHEN ':'.
          colonrow = pseudo_token->row.
          coloncol = pseudo_token->col.
          prefixlen = lines( stokes ).
          DATA(stokes_before_colon) = stokes.
        WHEN ',' OR '.'.
          line_sstmnt = VALUE lcl_shrinker_abap_scan=>ty_sstmnt( level      = 0
                                                                 struc      = 0
                                                                 from       = from_token
                                                                 to         = lines( stokes )
                                                                 number     = lines( sstmnt ) + 1
                                                                 colonrow   = colonrow
                                                                 trow       = pseudo_token->row
                                                                 coloncol   = coloncol
                                                                 tcol       = pseudo_token->col
                                                                 prefixlen  = prefixlen
                                                                 type       = COND #(
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-abap_doc
*                                   WHEN result-stokes[ ls_sstmnt-from ]-type = type-token-comment THEN
*                                      type-stmnt-comment
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-comment_in_stmnt
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-compute_direct
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-empty
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-include
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-include_miss
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-macro_call
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-macro_definition
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-method_direct
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-native_sql
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-opaque_body
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-pragma
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-trmac_call
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-type_pools
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-type_pools_miss
                                                                                      WHEN 1 = 2 THEN
                                                                                        type-stmnt-unknown
                                                                                      ELSE
                                                                                        type-stmnt-standard )
                                                                 terminator = pseudo_token->str
                                                                 enhmt      = 0 ).
          APPEND line_sstmnt TO sstmnt.
          from_token = lines( stokes ) + 1.
          DATA(stokes_buffer) = stokes_before_colon.
        WHEN OTHERS.
          line_stokes = VALUE lcl_shrinker_abap_scan=>ty_stokes( str  = pseudo_token->str
                                                                 row  = pseudo_token->row
                                                                 col  = pseudo_token->col
                                                                 type = pseudo_token->type ).
          APPEND LINES OF stokes_buffer TO stokes.
          stokes_buffer = VALUE #( ).
          APPEND line_stokes TO stokes.
      ENDCASE.
    ENDLOOP.

    result-sstmnt = sstmnt.
    result-stokes = stokes.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_uuid IMPLEMENTATION.
  METHOD class_constructor.
    uuid_generator = cl_uuid_factory=>create_system_uuid( ).
  ENDMETHOD.

  METHOD get_c26.
    result = uuid_generator->create_uuid_c26( ).
  ENDMETHOD.

  METHOD get_x16.
    result = uuid_generator->create_uuid_x16( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_app DEFINITION
  FINAL.

  PUBLIC SECTION.
    METHODS main.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_parameter,
        name                  TYPE string,
        by_value              TYPE abap_bool,
        typing                TYPE string,
        base_type_or_variable TYPE string,
      END OF ts_parameter.
    TYPES tt_parameter TYPE STANDARD TABLE OF ts_parameter WITH EMPTY KEY.
    TYPES tt_raising   TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_form_details,
        subroutine_name             TYPE string,
        tables_parameters           TYPE tt_parameter,
        using_parameters            TYPE tt_parameter,
        changing_parameters         TYPE tt_parameter,
        raising                     TYPE tt_raising,
        parameter_names_by_position TYPE string_table,
      END OF ts_form_details.
    TYPES tt_form_details TYPE SORTED TABLE OF ts_form_details WITH UNIQUE KEY subroutine_name.
    TYPES tt_scan_result  TYPE STANDARD TABLE OF lcl_shrinker_abap_scan=>ty_scan_result WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_result_of_get_subroutines,
        abap_statements TYPE tt_scan_result,
        form_details    TYPE tt_form_details,
      END OF ts_result_of_get_subroutines.
    TYPES:
      BEGIN OF ts_method_parameter,
        category              TYPE string,
        name                  TYPE string,
        by_value              TYPE abap_bool,
        typing                TYPE string,
        base_type_or_variable TYPE string,
      END OF ts_method_parameter.
    TYPES tt_method_parameter TYPE STANDARD TABLE OF ts_method_parameter WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_method_details,
        method_name            TYPE string,
        parameters             TYPE tt_method_parameter,
        definition_abap_source TYPE string_table,
        counter_importing      type i,
        counter_changing       type i,
      END OF ts_method_details.
    TYPES tt_method_details TYPE STANDARD TABLE OF ts_method_details WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_type_for_tables_with_struct,
        name      TYPE string,
        structure TYPE string,
      END OF ts_type_for_tables_with_struct.
    TYPES tt_type_for_tables_with_struct TYPE STANDARD TABLE OF ts_type_for_tables_with_struct WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_methods_to_create,
        method_details                 TYPE tt_method_details,
        types_for_tables_with_structur TYPE tt_type_for_tables_with_struct,
      END OF ts_methods_to_create.

    METHODS convert_forms_into_methods
      IMPORTING "abap_source        TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
                subroutine_details TYPE ts_result_of_get_subroutines
      RETURNING VALUE(result)      TYPE ts_methods_to_create.

    METHODS convert_abap_source
      IMPORTING abap_source        TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
*                form_details       TYPE tt_form_details
                subroutine_details TYPE ts_result_of_get_subroutines
                methods_to_create  TYPE ts_methods_to_create
      RETURNING VALUE(result)      TYPE lcl_shrinker_abap_scan=>ty_abap_source_code.

    METHODS get_subroutines
      IMPORTING abap_source   TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
      RETURNING VALUE(result) TYPE ts_result_of_get_subroutines.

    METHODS build_class_definition
      IMPORTING
        methods_to_create TYPE lcl_app=>ts_methods_to_create
      RETURNING
        VALUE(result)     TYPE lcl_shrinker_abap_scan=>ty_abap_source_code.

    METHODS get_updated_abap_source
      IMPORTING
        abap_source        TYPE lcl_shrinker_abap_scan=>ty_abap_source_code
        subroutine_details TYPE lcl_app=>ts_result_of_get_subroutines
        methods_to_create  TYPE lcl_app=>ts_methods_to_create
      RETURNING
        VALUE(result)      TYPE lcl_shrinker_abap_scan=>ty_abap_source_code.
ENDCLASS.


CLASS ltc_app DEFINITION
    FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_app IMPLEMENTATION.
  METHOD test.
    NEW lcl_app( )->main( ).
    cl_abap_unit_assert=>assert_equals( act = 1
                                        exp = 1 ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    SELECT name FROM trdir
      WHERE name IN @s_prog
      INTO TABLE @DATA(table_trdir).

    DATA(abap_source) = VALUE lcl_shrinker_abap_scan=>ty_abap_source_code( ).
    LOOP AT table_trdir REFERENCE INTO DATA(trdir).
      DATA(abap_source_include) = VALUE lcl_shrinker_abap_scan=>ty_abap_source_code( ).
      READ REPORT trdir->name INTO abap_source_include.
      INSERT LINES OF VALUE
             lcl_shrinker_abap_scan=>ty_abap_source_code( ( )
                                                          ( `*~~~~~~~~~~~~~~~~~` )
                                                          ( |* { trdir->name }| )
                                                          ( `*~~~~~~~~~~~~~~~~~` )
                                                          ( )
                                                          ( LINES OF abap_source_include ) )
             INTO TABLE abap_source.
    ENDLOOP.

    DATA(subroutine_details) = get_subroutines( abap_source ).

    DATA(methods_to_create) = convert_forms_into_methods( subroutine_details ).

    DATA(new_abap_source) = convert_abap_source( abap_source        = abap_source
                                                 subroutine_details = subroutine_details
                                                 methods_to_create  = methods_to_create ).

    gv_input = concat_lines_of( sep = |\n| table = abap_source ).
    gv_output = concat_lines_of( sep = |\n| table = new_abap_source ).

    lcl_gui=>run( ).

*    LOOP AT new_abap_source REFERENCE INTO DATA(final_code_line).
*      IF final_code_line->* IS INITIAL.
*        SKIP 1.
*      ELSE.
*        WRITE / final_code_line->*.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.

  METHOD get_subroutines.
    DATA tabix_current TYPE i.
    DATA stokes_line   TYPE REF TO lcl_shrinker_abap_scan=>ty_stokes.
    DATA parameter     TYPE REF TO ts_parameter.

    FIND ALL OCCURRENCES OF REGEX '\<form\>|\<endform\>|\<perform\>'
         IN TABLE abap_source
         IGNORING CASE
         RESULTS DATA(matches).

    LOOP AT matches REFERENCE INTO DATA(match).
      INSERT lcl_shrinker_abap_scan=>get_abap_statement_at_cursor( it_source = abap_source
                                                                   i_linenr  = match->line
                                                                   i_offset  = match->offset )
             INTO TABLE result-abap_statements.
    ENDLOOP.

    DELETE result-abap_statements WHERE stokes IS INITIAL.

    DATA(form_statements) = VALUE tt_scan_result( ).
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(endform_statements) = VALUE tt_scan_result( ).
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(perform_statements) = VALUE tt_scan_result( ).
    LOOP AT result-abap_statements REFERENCE INTO DATA(abap_statement).
      CASE abap_statement->stokes[ 1 ]-str.
        WHEN 'FORM'.
          INSERT abap_statement->* INTO TABLE form_statements.
        WHEN 'ENDFORM'.
*          INSERT abap_statement->* INTO TABLE endform_statements.
        WHEN 'PERFORM'.
*          INSERT abap_statement->* INTO TABLE perform_statements.
        WHEN OTHERS.
          DELETE result-abap_statements USING KEY loop_key.
      ENDCASE.
    ENDLOOP.

* https://help.sap.com/doc/abapdocu_758_index_htm/7.58/en-US/index.htm?file=abapform.htm
*
* FORM
*
* OBSOLETE SYNTAX
*
* FORM subr [TABLES table_parameters]
*           [USING parameters]
*           [CHANGING parameters]
*           [RAISING exc1|RESUMABLE(exc1) exc2|RESUMABLE(exc2) ...].
*
* table_parameters:
* ... t1 [{TYPE itab_type}|{LIKE itab}|{STRUCTURE struc}]
*     t2 [{TYPE itab_type}|{LIKE itab}|{STRUCTURE struc}]
*     ...
*
* parameters:
* ... { VALUE(p1) | p1 } [ typing|structure]
*     { VALUE(p2) | p2 } [ typing|structure]
*     ...
*
* typing:
* ... generic_typing | complete_typing  ...
*
* generic_typing:
* ... { TYPE generic_type }
*   | { LIKE <generic_fs>|generic_para } ...
*
* generic_type:
* ... any | { any table } | c | clike | csequence | data | decfloat | { hashed table } | { index table }
*   | n | numeric | object | p | simple | { sorted table } | { standard table } | table | x | xsequence ...
*
* complete_typing:
* ... { TYPE {[LINE OF] complete_type}
*          | {REF TO type} }
*   | { LIKE {[LINE OF] dobj}
*          | {REF TO dobj} } ...
*
* structure:
* ... STRUCTURE struc ...

    LOOP AT form_statements REFERENCE INTO abap_statement.
      LOOP AT abap_statement->stokes REFERENCE INTO stokes_line FROM 3.
        tabix_current = sy-tabix.
        DATA(three_tokens) = concat_lines_of(
            sep   = ` `
            table = VALUE string_table( FOR <token> IN abap_statement->stokes FROM sy-tabix TO sy-tabix + 2
                                        ( <token>-str ) ) ).
        CASE three_tokens.
          WHEN 'TYPE ANY TABLE'
            OR 'TYPE INDEX TABLE'
            OR 'TYPE HASHED TABLE'
            OR 'TYPE SORTED TABLE'
            OR 'TYPE STANDARD TABLE'
            OR 'TYPE LINE OF'
            OR 'TYPE REF TO'
            OR 'LIKE LINE OF'
            OR 'LIKE REF TO'.
            stokes_line->str = three_tokens.
            DELETE abap_statement->stokes FROM tabix_current + 1 TO tabix_current + 2.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    LOOP AT form_statements REFERENCE INTO abap_statement.
      DATA(form_details) = VALUE ts_form_details( subroutine_name = abap_statement->stokes[ 2 ]-str ).

      DATA(state) = 1.
      LOOP AT abap_statement->stokes REFERENCE INTO stokes_line FROM 3.
        tabix_current = sy-tabix.

        CASE state.
          WHEN 1.
            CASE stokes_line->str.
              WHEN 'TABLES'.
                state = 2.
              WHEN 'USING'.
                state = 3.
              WHEN 'CHANGING'.
                state = 4.
              WHEN 'RAISING'.
                state = 5.
              WHEN OTHERS.
                " TODO exception
            ENDCASE.
          WHEN 2.
            " First word after TABLES
            INSERT VALUE #( name   = stokes_line->str
                            typing = 'TYPE STANDARD TABLE' )
                   INTO TABLE form_details-tables_parameters
                   REFERENCE INTO parameter.
            state = 6.
          WHEN 3.
            " First word after USING
            INSERT VALUE #( name                  = stokes_line->str
                            typing                = 'TYPE'
                            base_type_or_variable = 'ANY' )
                   INTO TABLE form_details-using_parameters
                   REFERENCE INTO parameter.
            state = 9.
          WHEN 4.
            " First word after CHANGING
            INSERT VALUE #( name                  = stokes_line->str
                            typing                = 'TYPE'
                            base_type_or_variable = 'ANY' )
                   INTO TABLE form_details-changing_parameters
                   REFERENCE INTO parameter.
            state = 12.
          WHEN 5.
            " All words after RAISING
            INSERT stokes_line->str INTO TABLE form_details-raising.
          WHEN 6.
            " Type clause after TABLES parameter name.
            IF    stokes_line->str  = 'TYPE'
               OR stokes_line->str  = 'LIKE'
               OR stokes_line->str  = 'STRUCTURE'.
              parameter->typing = stokes_line->str.
              state = 7.
            ELSE.
              CASE stokes_line->str.
                WHEN 'USING'.
                  state = 3.
                WHEN 'CHANGING'.
                  state = 4.
                WHEN 'RAISING'.
                  state = 5.
                WHEN OTHERS.
                  INSERT VALUE #( name   = stokes_line->str
                                  typing = 'TYPE STANDARD TABLE' )
                         INTO TABLE form_details-tables_parameters
                         REFERENCE INTO parameter.
                  state = 6.
              ENDCASE.
            ENDIF.
          WHEN 7.
            " Base type or variable after TABLES parameter name and TYPE/LIKE clause.
            parameter->base_type_or_variable = stokes_line->str.
            state = 8.
          WHEN 8.
            " Next token after one TABLES parameter.
            CASE stokes_line->str.
              WHEN 'USING'.
                state = 3.
              WHEN 'CHANGING'.
                state = 4.
              WHEN 'RAISING'.
                state = 5.
              WHEN OTHERS.
                INSERT VALUE #( name   = stokes_line->str
                                typing = 'TYPE STANDARD TABLE' )
                       INTO TABLE form_details-tables_parameters
                       REFERENCE INTO parameter.
                state = 6.
            ENDCASE.
          WHEN 9.
            " Type clause after USING parameter name.
            IF stokes_line->str CP 'TYPE * TABLE'.
              parameter->typing                = stokes_line->str.
              parameter->base_type_or_variable = ''.
              state = 11.
            ELSEIF    stokes_line->str CP 'TYPE *'
                   OR stokes_line->str CP 'LIKE *'
                   OR stokes_line->str  = 'TYPE'
                   OR stokes_line->str  = 'LIKE'
                   OR stokes_line->str  = 'STRUCTURE'.
              parameter->typing = stokes_line->str.
              state = 10.
            ELSE.
              CASE stokes_line->str.
                WHEN 'CHANGING'.
                  state = 4.
                WHEN 'RAISING'.
                  state = 5.
                WHEN OTHERS.
                  INSERT VALUE #( name                  = stokes_line->str
                                  typing                = 'TYPE'
                                  base_type_or_variable = 'ANY' )
                         INTO TABLE form_details-using_parameters
                         REFERENCE INTO parameter.
                  state = 9.
              ENDCASE.
            ENDIF.
          WHEN 10.
            " Base type or variable after USING parameter name and TYPE/LIKE clause.
            parameter->base_type_or_variable = stokes_line->str.
            state = 11.
          WHEN 11.
            " Next token after one USING parameter.
            CASE stokes_line->str.
              WHEN 'CHANGING'.
                state = 4.
              WHEN 'RAISING'.
                state = 5.
              WHEN OTHERS.
                INSERT VALUE #( name                  = stokes_line->str
                                typing                = 'TYPE'
                                base_type_or_variable = 'ANY' )
                       INTO TABLE form_details-using_parameters
                       REFERENCE INTO parameter.
                state = 9.
            ENDCASE.
          WHEN 12.
            " Type clause after CHANGING parameter name.
            IF stokes_line->str CP 'TYPE * TABLE'.
              parameter->typing                = stokes_line->str.
              parameter->base_type_or_variable = ''.
              state = 14.
            ELSEIF    stokes_line->str CP 'TYPE *'
                   OR stokes_line->str CP 'LIKE *'
                   OR stokes_line->str  = 'TYPE'
                   OR stokes_line->str  = 'LIKE'
                   OR stokes_line->str  = 'STRUCTURE'.
              parameter->typing = stokes_line->str.
              state = 13.
            ELSE.
              CASE stokes_line->str.
                WHEN 'RAISING'.
                  state = 5.
                WHEN OTHERS.
                  INSERT VALUE #( name                  = stokes_line->str
                                  typing                = 'TYPE'
                                  base_type_or_variable = 'ANY' )
                         INTO TABLE form_details-changing_parameters
                         REFERENCE INTO parameter.
                  state = 12.
              ENDCASE.
            ENDIF.
          WHEN 13.
            " Base type or variable after CHANGING parameter name and TYPE/LIKE clause.
            parameter->base_type_or_variable = stokes_line->str.
            state = 14.
          WHEN 14.
            " Next token after one CHANGING parameter.
            CASE stokes_line->str.
              WHEN 'RAISING'.
                state = 5.
              WHEN OTHERS.
                INSERT VALUE #( name                  = stokes_line->str
                                typing                = 'TYPE'
                                base_type_or_variable = 'ANY' )
                       INTO TABLE form_details-changing_parameters
                       REFERENCE INTO parameter.
                state = 12.
            ENDCASE.
        ENDCASE.
      ENDLOOP.

      LOOP AT form_details-using_parameters REFERENCE INTO DATA(using_parameter)
           WHERE name CP 'VALUE(*)'.
        using_parameter->name     = substring( val = using_parameter->name
                                               off = 6
                                               len = strlen( using_parameter->name ) - 7 ).
        using_parameter->by_value = abap_true.
      ENDLOOP.
      LOOP AT form_details-changing_parameters REFERENCE INTO DATA(changing_parameter)
           WHERE name CP 'VALUE(*)'.
        changing_parameter->name     = substring( val = changing_parameter->name
                                                  off = 6
                                                  len = strlen( changing_parameter->name ) - 7 ).
        changing_parameter->by_value = abap_true.
      ENDLOOP.

      form_details-parameter_names_by_position = VALUE #(
          ( LINES OF VALUE #( FOR <parameter> IN form_details-tables_parameters
                              ( <parameter>-name ) ) )
          ( LINES OF VALUE #( FOR <parameter> IN form_details-using_parameters
                              ( <parameter>-name ) ) )
          ( LINES OF VALUE #( FOR <parameter> IN form_details-changing_parameters
                              ( <parameter>-name ) ) ) ).

      INSERT form_details INTO TABLE result-form_details.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_forms_into_methods.
    LOOP AT subroutine_details-form_details REFERENCE INTO data(form_details).
      DATA(method_details) = VALUE ts_method_details( method_name = form_details->subroutine_name ).

      LOOP AT form_details->using_parameters REFERENCE INTO DATA(parameter).
        INSERT VALUE #( category              = 'IMPORTING'
                        name                  = parameter->name
                        by_value              = parameter->by_value
                        typing                = parameter->typing
                        base_type_or_variable = parameter->base_type_or_variable )
               INTO TABLE method_details-parameters.
        method_details-counter_importing = method_details-counter_importing + 1.
      ENDLOOP.

      LOOP AT form_details->tables_parameters REFERENCE INTO parameter.
        INSERT VALUE #( category              = 'CHANGING'
                        name                  = parameter->name
                        by_value              = abap_false
                        typing                = parameter->typing
                        base_type_or_variable = COND #( WHEN parameter->base_type_or_variable IS NOT INITIAL
                                                        THEN |TT_{ parameter->base_type_or_variable }| ) )
               INTO TABLE method_details-parameters.
        method_details-counter_changing = method_details-counter_changing + 1.
        IF parameter->typing = 'STRUCTURE'.
          " The parameter could be based on a DDIC type or on a variable, difficult to
          " be sure. I prefer going to LIKE to force a syntax error if it's a DDIC type
          " and the developer will choose how to fix it.
          INSERT VALUE #( name      = |TT_{ parameter->name }|
                          structure = parameter->base_type_or_variable )
                 INTO TABLE result-types_for_tables_with_structur.
        ENDIF.
      ENDLOOP.

      LOOP AT form_details->changing_parameters REFERENCE INTO parameter.
        INSERT VALUE #( category              = 'CHANGING'
                        name                  = parameter->name
                        by_value              = parameter->by_value
                        typing                = parameter->typing
                        base_type_or_variable = parameter->base_type_or_variable )
               INTO TABLE method_details-parameters.
        method_details-counter_changing = method_details-counter_changing + 1.
      ENDLOOP.

      INSERT method_details INTO TABLE result-method_details.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_abap_source.
    DATA(class_definition) = build_class_definition( methods_to_create ).

    DATA(updated_abap_source) = get_updated_abap_source( abap_source        = abap_source
                                                         subroutine_details = subroutine_details
                                                         methods_to_create  = methods_to_create ).

    result = VALUE #( ( LINES OF class_definition )
                      ( )
                      ( `CLASS lcl_app IMPLEMENTATION.` )
                      ( `ENDCLASS.` )
                      ( )
                      ( LINES OF updated_abap_source ) ).
  ENDMETHOD.

  METHOD build_class_definition.
    result = VALUE lcl_shrinker_abap_scan=>ty_abap_source_code( ( `CLASS lcl_app DEFINITION.` )
                                                                ( `PUBLIC SECTION.` ) ).

    LOOP AT methods_to_create-types_for_tables_with_structur REFERENCE INTO DATA(type).
      " The parameter could be based on a DDIC type or on a variable, difficult to
      " be sure. I prefer going to LIKE to force a syntax error if it's a DDIC type
      " and the developer will choose how to fix it.
      INSERT |TYPES { to_lower( type->name )
             } LIKE STANDARD TABLE OF {
             to_lower( type->structure ) }|
             INTO TABLE result.
    ENDLOOP.

    LOOP AT methods_to_create-method_details REFERENCE INTO DATA(method_details2).
      INSERT |CLASS-METHODS { to_lower( method_details2->method_name ) }|
             INTO TABLE result
             REFERENCE INTO DATA(method_definition_line).

      IF method_details2->counter_importing > 0.
        INSERT `IMPORTING` INTO TABLE result.
        LOOP AT method_details2->parameters REFERENCE INTO DATA(using_parameter)
             WHERE category = 'IMPORTING'.
          INSERT |{ COND #( WHEN using_parameter->by_value = abap_false
                            THEN to_lower( using_parameter->name )
                            ELSE |value({ to_lower( using_parameter->name ) })| )
                 } { using_parameter->typing } {
                 to_lower( using_parameter->base_type_or_variable ) }|
                 INTO TABLE result
                 REFERENCE INTO method_definition_line.
        ENDLOOP.
      ENDIF.

      IF method_details2->counter_changing > 0.
        INSERT `CHANGING` INTO TABLE result.
        LOOP AT method_details2->parameters REFERENCE INTO DATA(changing_parameter)
             WHERE category = 'CHANGING'.
          INSERT |{ COND #( WHEN changing_parameter->by_value = abap_false
                            THEN to_lower( changing_parameter->name )
                            ELSE |value({ to_lower( changing_parameter->name ) })| )
                 } { changing_parameter->typing } {
                 to_lower( changing_parameter->base_type_or_variable ) }|
                 INTO TABLE result
                 REFERENCE INTO method_definition_line.
        ENDLOOP.
      ENDIF.

      method_definition_line->* = method_definition_line->* && '.'.
    ENDLOOP.

    INSERT `ENDCLASS.` INTO TABLE result.
  ENDMETHOD.

  METHOD get_updated_abap_source.
    DATA last_method_call_line TYPE REF TO string.
    DATA procedure_name        TYPE lcl_shrinker_abap_scan=>ty_stokes-str.

    result = abap_source.

    " From bottom to top
    DATA(abap_statement_tabix) = lines( subroutine_details-abap_statements ).
    WHILE abap_statement_tabix >= 1.
      DATA(abap_statement) = REF #( subroutine_details-abap_statements[ abap_statement_tabix ] ).

      " TODO: currently, only the INDEX 1 is processed BUT each line of PERFORM_STATEMENTS
      "       may refer to several PERFORM (e.g. chaining of several PERFORM).
      DATA(starting_token_number) = abap_statement->sstmnt[ 1 ]-from.
      DATA(first_token) = abap_statement->stokes[ starting_token_number ].
      DATA(terminal_row) = abap_statement->sstmnt[ 1 ]-trow.

      DATA(new_abap_source_line) = REF #( result[ first_token-row ] ).

      CASE first_token-str.
        WHEN 'FORM'.
          procedure_name = abap_statement->stokes[ starting_token_number + 1 ]-str.
          DELETE result FROM first_token-row TO terminal_row.
          INSERT |METHOD { procedure_name }.| INTO result INDEX first_token-row.

        WHEN 'ENDFORM'.
          REPLACE `ENDFORM.` IN new_abap_source_line->* WITH `ENDMETHOD.`.

        WHEN 'PERFORM'.
          DELETE result FROM first_token-row TO terminal_row.

          procedure_name = abap_statement->stokes[ 2 ]-str.
          DATA(arguments) = VALUE lcl_shrinker_abap_scan=>ty_ut_stokes( FOR <stokes_line> IN abap_statement->stokes FROM 3
                                                                        WHERE (     str <> 'TABLES'
                                                                                AND str <> 'USING'
                                                                                AND str <> 'CHANGING' )
                                                                        ( <stokes_line> ) ).

          DATA(index_for_insert) = first_token-row.

          INSERT |lcl_app=>{ procedure_name }(| INTO result INDEX index_for_insert REFERENCE INTO last_method_call_line.
          index_for_insert = index_for_insert + 1.

          DATA(form_details) = subroutine_details-form_details[ subroutine_name = procedure_name ].
          DATA(method) = methods_to_create-method_details[ method_name = procedure_name ].

          DATA(previous_category) = `?`.
          LOOP AT method-parameters REFERENCE INTO DATA(method_parameter).
            IF method_parameter->category <> previous_category.
              INSERT SWITCH #( method_parameter->category
                               WHEN `IMPORTING` THEN `EXPORTING`
                               WHEN `EXPORTING` THEN `IMPORTING`
                               WHEN `CHANGING`  THEN `CHANGING` )
                     INTO result
                     INDEX index_for_insert
                     REFERENCE INTO last_method_call_line.
              index_for_insert = index_for_insert + 1.
            ENDIF.
            DATA(form_parameter_position) = line_index(
                form_details-parameter_names_by_position[ table_line = method_parameter->name ] ).
            INSERT |{ method_parameter->name } = { arguments[ form_parameter_position ]-str }|
                   INTO result
                   INDEX index_for_insert
                   REFERENCE INTO last_method_call_line.
            index_for_insert = index_for_insert + 1.
            previous_category = method_parameter->category.
          ENDLOOP.

          last_method_call_line->* = last_method_call_line->* && ` ).`.
      ENDCASE.

      abap_statement_tabix = abap_statement_tabix - 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
