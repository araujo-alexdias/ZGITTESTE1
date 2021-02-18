*&---------------------------------------------------------------------*
*& Report Z_SOURCE_CODE_ABAP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SOURCE_CODE_ABAP.

*&---------------------------------------------------------------------*
*&   Data Declaration
*&---------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_obj_list,
    pgmid    TYPE e071-pgmid,
    object   TYPE e071-object,
    obj_name TYPE e071-obj_name,
  END OF ty_obj_list.
TYPES:
  ty_obj_list_tt TYPE TABLE OF ty_obj_list.

TYPES:
  BEGIN OF ty_obj_list_aux,
    pgmid    TYPE e071-pgmid,
    object   TYPE e071-object,
    obj_name TYPE tadir-obj_name,
  END OF ty_obj_list_aux.
TYPES:
  ty_obj_list_aux_tt TYPE TABLE OF ty_obj_list_aux.

TYPES:
  BEGIN OF ty_pckg_list,
    devclass TYPE tadir-devclass,
  END OF ty_pckg_list.
TYPES:
  ty_pckg_list_tt TYPE TABLE OF ty_pckg_list.

TYPES:
  ty_type  TYPE c LENGTH 12 .
TYPES:
  ty_value TYPE c LENGTH 12 .
TYPES:
  BEGIN OF ty_content,
    type     TYPE ty_type,
    value    TYPE ty_value,
    data_str TYPE string,
  END OF ty_content .
TYPES:
  ty_contents TYPE SORTED TABLE OF ty_content WITH UNIQUE KEY type value.

TYPES: BEGIN OF ty_repo_xml,
         url             TYPE string,
         branch_name     TYPE string,
         selected_commit TYPE zif_abapgit_definitions=>ty_sha1,
         package         TYPE devclass,
         created_by      TYPE xubname,
         created_at      TYPE timestampl,
         deserialized_by TYPE xubname,
         deserialized_at TYPE timestampl,
         offline         TYPE abap_bool,
         switched_origin TYPE string,
*           local_checksums TYPE ty_local_checksum_tt,
         dot_abapgit     TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit,
         head_branch     TYPE string,   " HEAD symref of the repo, master branch
*           local_settings  TYPE ty_local_settings,
       END OF ty_repo_xml.

TYPES: BEGIN OF ty_repo,
         key TYPE ty_value.
         INCLUDE TYPE ty_repo_xml.
       TYPES: END OF ty_repo.
TYPES: ty_repos TYPE STANDARD TABLE OF ty_repo WITH DEFAULT KEY.

TYPES: BEGIN OF ty_progdir,
         name    TYPE progdir-name,
         state   TYPE progdir-state,
         sqlx    TYPE progdir-sqlx,
         edtx    TYPE progdir-edtx,
         varcl   TYPE progdir-varcl,
         dbapl   TYPE progdir-dbapl,
         dbna    TYPE progdir-dbna,
         clas    TYPE progdir-clas,
         type    TYPE progdir-type,
         occurs  TYPE progdir-occurs,
         subc    TYPE progdir-subc,
         appl    TYPE progdir-appl,
         secu    TYPE progdir-secu,
         cnam    TYPE progdir-cnam,
         cdat    TYPE progdir-cdat,
         unam    TYPE progdir-unam,
         udat    TYPE progdir-udat,
         vern    TYPE progdir-vern,
         levl    TYPE progdir-levl,
         rstat   TYPE progdir-rstat,
         rmand   TYPE progdir-rmand,
         rload   TYPE progdir-rload,
         fixpt   TYPE progdir-fixpt,
         sset    TYPE progdir-sset,
         sdate   TYPE progdir-sdate,
         stime   TYPE progdir-stime,
         idate   TYPE progdir-idate,
         itime   TYPE progdir-itime,
         ldbname TYPE progdir-ldbname,
         uccheck TYPE progdir-uccheck,
       END OF ty_progdir.

TYPES:
  ty_sha1    TYPE c LENGTH 40 .
TYPES: ty_sha1_tt TYPE STANDARD TABLE OF ty_sha1 WITH DEFAULT KEY .

TYPES ty_git_branch_type TYPE c LENGTH 2 .
TYPES:
  BEGIN OF ty_git_branch,
    sha1         TYPE ty_sha1,
    name         TYPE string,
    type         TYPE ty_git_branch_type,
    is_head      TYPE abap_bool,
    display_name TYPE string,
  END OF ty_git_branch .
TYPES:
  ty_git_branch_list_tt TYPE STANDARD TABLE OF ty_git_branch WITH DEFAULT KEY .

CONSTANTS:
  BEGIN OF c_service,
    receive TYPE string VALUE 'receive',                    "#EC NOTEXT
    upload  TYPE string VALUE 'upload',                     "#EC NOTEXT
  END OF c_service .

*&---------------------------------------------------------------------*
*&   Simulate
*&---------------------------------------------------------------------*

DATA lv_strkorr TYPE e070-strkorr.
lv_strkorr = 'ST2K900319'.

*&---------------------------------------------------------------------*
*&   Select Object List
*&---------------------------------------------------------------------*

DATA lt_obj_list TYPE ty_obj_list_tt.
DATA lt_obj_aux_list TYPE ty_obj_list_aux_tt.
DATA ls_obj_list_aux LIKE LINE OF lt_obj_aux_list.
FIELD-SYMBOLS: <ls_obj_list>     TYPE ty_obj_list,
               <ls_obj_list_aux> TYPE ty_obj_list_aux.

SELECT pgmid object obj_name
  FROM e071
  INTO TABLE lt_obj_list
  WHERE trkorr = lv_strkorr.

IF sy-subrc NE 0.
  MESSAGE s001(00) WITH 'No objects in the Request' DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING.
ENDIF.

LOOP AT lt_obj_list ASSIGNING <ls_obj_list>.
  CLEAR ls_obj_list_aux.
  ls_obj_list_aux-pgmid    = <ls_obj_list>-pgmid.
  ls_obj_list_aux-object   = <ls_obj_list>-object.
  ls_obj_list_aux-obj_name = <ls_obj_list>-obj_name.
  APPEND ls_obj_list_aux TO lt_obj_aux_list.
ENDLOOP.

*&---------------------------------------------------------------------*
*&   Get packages
*&---------------------------------------------------------------------*

DATA lt_pckg_list TYPE ty_pckg_list_tt.

SELECT devclass
  FROM tadir
  INTO TABLE lt_pckg_list
  FOR ALL ENTRIES IN lt_obj_aux_list
  WHERE pgmid    = lt_obj_aux_list-pgmid
    AND object   = lt_obj_aux_list-object
    AND obj_name = lt_obj_aux_list-obj_name.

IF sy-subrc NE 0.
  MESSAGE s001(00) WITH 'No packages in the Request' DISPLAY LIKE 'E'.
  LEAVE LIST-PROCESSING.
ENDIF.

*&---------------------------------------------------------------------*
*&   Get objects of packages
*&---------------------------------------------------------------------*

DATA: l_tadir_objs TYPE scts_tadir.
FIELD-SYMBOLS: <ls_pckg_list> TYPE ty_pckg_list.

BREAK-POINT.

LOOP AT lt_pckg_list ASSIGNING <ls_pckg_list>.

  CALL FUNCTION 'TRINT_SELECT_OBJECTS'
    EXPORTING
      iv_srcsystem             = space
      iv_devclass              = <ls_pckg_list>-devclass
      iv_only_existing_objects = space
*     IV_SRCSYSTEM_VISIBLE     = 'X'
*     IV_DEVCLASS_VISIBLE      = 'X'
*     IV_TITLE                 =
      iv_via_selscreen         = space
    IMPORTING
*     ET_OBJECTS               =
      et_objects_tadir         = l_tadir_objs
*     EV_NUMBER_OF_OBJECTS     =
    EXCEPTIONS
*     CANCELLED_BY_USER        = 1
*     INVALID_INPUT            = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE s001(00) WITH 'Error retriving object list (function)' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDLOOP.

*&---------------------------------------------------------------------*
*&   Get URL
*&---------------------------------------------------------------------*

DATA: lt_content  TYPE ty_contents,
      ls_content  LIKE LINE OF lt_content,
      lv_xml      TYPE string,
      ls_repo_xml TYPE ty_repo_xml,
      ls_repo     TYPE ty_repo,
      rt_repos    TYPE ty_repos.

SELECT * FROM zabapgit
  INTO TABLE lt_content
  WHERE type = 'REPO'
  ORDER BY PRIMARY KEY.

IF sy-subrc EQ 0.

  LOOP AT lt_content INTO ls_content.
    lv_xml = ls_content-data_str.

*         fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
    OPTIONS value_handling = 'accept_data_loss'
    SOURCE XML lv_xml
    RESULT repo = ls_repo_xml.

    MOVE-CORRESPONDING ls_repo_xml TO ls_repo.
    INSERT ls_repo INTO TABLE rt_repos.

***      MOVE-CORRESPONDING from_xml( ls_content-data_str ) TO ls_repo.
***      IF ls_repo-local_settings-write_protected = abap_false AND
***         zcl_abapgit_factory=>get_environment( )->is_repo_object_changes_allowed( ) = abap_false.
***        ls_repo-local_settings-write_protected = abap_true.
***      ENDIF.
**      ls_repo-key = ls_content-value.
**      INSERT ls_repo INTO TABLE rt_repos.
  ENDLOOP.
ENDIF.


*&---------------------------------------------------------------------*
*&   Get Source Code
*&---------------------------------------------------------------------*

DATA: ls_tadir_objs   TYPE tadir,
      ls_progdir      TYPE ty_progdir,
      lv_program_name TYPE programm,
      lt_source       TYPE TABLE OF abaptxt255,
      lt_tpool        TYPE textpool_table,
      ls_tpool        LIKE LINE OF lt_tpool,
      li_node         TYPE REF TO if_ixml_node,
      li_doc          TYPE REF TO if_ixml_document,
      lt_stab         TYPE abap_trans_srcbind_tab,
      iv_name         TYPE string,   " CLIKE
      lt_files        TYPE zif_abapgit_definitions=>ty_files_tt,
      ls_file         TYPE zif_abapgit_definitions=>ty_file,
      lv_source       TYPE string,
      lv_len          TYPE i,
      lv_char10       TYPE c LENGTH 10,
      lv_string       TYPE string,
      lv_xstring      TYPE xstring,
      lv_hash         TYPE string,
      lv_key          TYPE xstring,
      lx_error        TYPE REF TO cx_abap_message_digest.

FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files .


LOOP AT l_tadir_objs INTO ls_tadir_objs.

  IF ls_tadir_objs-object EQ 'PROG'.
    lv_program_name = ls_tadir_objs-obj_name.

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_includelist = abap_false
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc = 0.

      CONCATENATE LINES OF lt_source INTO lv_source SEPARATED BY cl_abap_char_utilities=>newline.
*     when editing files via eg. GitHub web interface it adds a newline at end of file
      lv_source = lv_source && cl_abap_char_utilities=>newline.

      ls_file-path = '/'.
      ls_file-filename = 'zgit_flight.prog.abap'.

      ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_source ).
      APPEND ls_file TO lt_files.

*          generates a hash
      LOOP AT lt_files ASSIGNING <ls_file>.

        lv_len = xstrlen( <ls_file>-data ).
        lv_char10 = lv_len.
        CONDENSE lv_char10.
        CONCATENATE 'blob' lv_char10 INTO lv_string SEPARATED BY space.
        lv_xstring = zcl_abapgit_convert=>string_to_xstring_utf8( lv_string ).

        lv_string = lv_xstring.
        CONCATENATE lv_string '00' INTO lv_string.
        lv_xstring = lv_string.
        CONCATENATE lv_xstring <ls_file>-data INTO lv_xstring IN BYTE MODE.

        TRY.
            cl_abap_hmac=>calculate_hmac_for_raw(
          EXPORTING
            if_key        = lv_key
            if_data       = lv_xstring
          IMPORTING
            ef_hmacstring = lv_hash ).
          CATCH cx_abap_message_digest INTO lx_error.
            zcx_abapgit_exception=>raise_with_text( lx_error ).
        ENDTRY.

        <ls_file>-sha1 = lv_hash.
        TRANSLATE <ls_file>-sha1 TO LOWER CASE.

      ENDLOOP.
    ENDIF.

  ELSE.
*        lv_program_name = iv_program.



  ENDIF.

***          iv_name = 'PROGDIR'.
***          FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.
***          ASSERT NOT iv_name IS INITIAL.
***
***          DATA: ls_sapdir TYPE progdir.
***          CALL FUNCTION 'READ_PROGDIR'
***            EXPORTING
***               i_progname = lv_program_name
***               i_state    = 'A'
***            IMPORTING
***               e_progdir  = ls_sapdir.
***           MOVE-CORRESPONDING ls_sapdir TO ls_progdir.
***           IF ls_progdir IS INITIAL.
***               RETURN.
***           ENDIF.
***
***           APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
***           <ls_stab>-name = iv_name.
***           GET REFERENCE OF ls_progdir INTO <ls_stab>-value.
***
***           li_doc = cl_ixml=>create( )->create_document( ).
***
***           CALL TRANSFORMATION id
***           OPTIONS initial_components = 'suppress'
***           SOURCE (lt_stab)
***           RESULT XML li_doc.
ENDLOOP.

*&---------------------------------------------------------------------*
*&   Get Source Code from repository (Git)
*&---------------------------------------------------------------------*
DATA: lv_uri                 TYPE string,
      lv_scheme              TYPE string,
      li_client              TYPE REF TO if_http_client,
      lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config,
      ro_client              TYPE REF TO zcl_abapgit_http_client,
      lv_text                TYPE string,
      lv_host                TYPE string,
      lv_path                TYPE string,
      lv_name                TYPE string,
      lv_agent               TYPE string.

CREATE OBJECT lo_proxy_configuration.

LOOP AT rt_repos INTO ls_repo.
  IF ls_repo-offline = abap_false.

*  Get Host Server
    FIND REGEX '(https?://[^/]*)(.*/)(.*)\.git$' IN ls_repo-url
       SUBMATCHES lv_host lv_path lv_name.
    IF sy-subrc <> 0.
      FIND REGEX '(https?://[^/]*)(.*/)(.*)$' IN ls_repo-url
        SUBMATCHES lv_host lv_path lv_name.
      IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'Malformed URL' ).
      ENDIF.
    ENDIF.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_host
        ssl_id             = 'ANONYM'
        proxy_host         = lo_proxy_configuration->get_proxy_url( ls_repo-url )
        proxy_service      = lo_proxy_configuration->get_proxy_port( ls_repo-url )
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " make sure:
          " a) SSL is setup properly in STRUST
          lv_text = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'.
        WHEN OTHERS.
          lv_text = 'While creating HTTP Client'.

      ENDCASE.
*      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    lv_agent = |git/2.0 (abapGit { zif_abapgit_version=>gc_abap_version })|.
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = lv_agent ).
    lv_uri = zcl_abapgit_url=>path_name( ls_repo-url ) &&
             '/info/refs?service=git-' &&
             c_service-upload &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

*   Send and Receive message
    DATA:
      lv_code    TYPE i,
      lv_message TYPE string.

    li_client->send( ).
    li_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    IF sy-subrc <> 0.
      " in case of HTTP_COMMUNICATION_FAILURE
      " make sure:
      " a) SSL is setup properly in STRUST
      " b) no firewalls
      " check trace file in transaction SMICM
      li_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      lv_text = |HTTP error { lv_code } occured: { lv_message }|.
***      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.


***  ro_client->check_http_200( ).
    DATA: lv_code_response TYPE i.
*          lv_text TYPE string.

    li_client->response->get_status( IMPORTING code = lv_code_response ).
    CASE lv_code_response.
      WHEN 200.
*        RETURN. " Success, OK
      WHEN 302.
*        zcx_abapgit_exception=>raise( 'Resource access temporarily redirected. Check the URL (HTTP 302)' ).
      WHEN 401.
*        zcx_abapgit_exception=>raise( 'Unauthorized access to resource. Check your credentials (HTTP 401)' ).
      WHEN 403.
*        zcx_abapgit_exception=>raise( 'Access to resource forbidden (HTTP 403)' ).
      WHEN 404.
*        zcx_abapgit_exception=>raise( 'Resource not found. Check the URL (HTTP 404)' ).
      WHEN 407.
*        zcx_abapgit_exception=>raise( 'Proxy authentication required. Check your credentials (HTTP 407)' ).
      WHEN 408.
*        zcx_abapgit_exception=>raise( 'Request timeout (HTTP 408)' ).
      WHEN 415.
*        zcx_abapgit_exception=>raise( 'Unsupported media type (HTTP 415)' ).
      WHEN OTHERS.
        lv_text = li_client->response->get_cdata( ).
*        zcx_abapgit_exception=>raise( |{ lv_text } (HTTP { lv_code })| ).
    ENDCASE.
***    IF lv_scheme <> c_scheme-digest.
***      zcl_abapgit_login_manager=>save( iv_uri    = iv_url
***                                       ii_client = li_client ).
***    ENDIF.

*&---------------------------------------------------------------------*
*&   Get data from Response
*&---------------------------------------------------------------------*
    DATA: lv_data TYPE string.
    DATA: lv_expected_content_type TYPE string.

    lv_expected_content_type = 'application/x-git-<service>-pack-advertisement'.
    REPLACE '<service>' IN lv_expected_content_type WITH c_service-upload.

    DATA: lv_content_type TYPE string.

    IF lv_expected_content_type IS NOT INITIAL.
      lv_content_type = li_client->response->get_content_type( ).
      IF lv_content_type <> lv_expected_content_type.
*        zcx_abapgit_exception=>raise( 'Wrong Content-Type sent by server - no fallback to the dumb protocol!' ).
      ENDIF.
    ENDIF.

    lv_data = li_client->response->get_cdata( ).
    FIND REGEX '^[0-9a-f]{4}#' IN lv_data.
    IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( 'Wrong Content sent by server' ).
    ENDIF.


*&---------------------------------------------------------------------*
*&   Get hash from Response
*&---------------------------------------------------------------------*

    DATA: lt_result            TYPE TABLE OF string,
          lv_hash2             TYPE c LENGTH 40,
*        lv_name              TYPE string,
          lv_head_params       TYPE string,
          lv_char              TYPE c,
*          lv_data              LIKE LINE OF lt_result,
          lv_current_row_index TYPE syst-tabix,
          et_list              TYPE ty_git_branch_list_tt,
          ev_head_symref       TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

*--------------------------------------------------
*    lv_data = skip_first_pkt( iv_data ).
    DATA: lv_hex    TYPE x LENGTH 1,
          lv_length TYPE i.
*   channel
    ASSERT lv_data(2) = '00'.

    lv_hex = to_upper( lv_data+2(2) ).
    lv_length = lv_hex.

    lv_data = lv_data+lv_length.

*-------------------------------------------------------


    SPLIT lv_data AT zif_abapgit_definitions=>c_newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      lv_current_row_index = sy-tabix.

      IF sy-tabix = 1 AND strlen( lv_data ) > 49.
        lv_hash2 = lv_data+8.
        lv_name  = lv_data+49.
        lv_char  = zcl_abapgit_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.

*        ev_head_symref = parse_head_params( lv_head_params ).
        DATA: ls_match    TYPE match_result,
              ls_submatch LIKE LINE OF ls_match-submatches.

        FIND FIRST OCCURRENCE OF REGEX '\ssymref=HEAD:([^\s]+)' IN lv_data RESULTS ls_match.
        READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
        IF sy-subrc IS INITIAL.
          ev_head_symref = lv_data+ls_submatch-offset(ls_submatch-length).
        ENDIF.

      ELSEIF sy-tabix > 1 AND strlen( lv_data ) > 45.
        lv_hash2 = lv_data+4.
        lv_name  = lv_data+45.
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        zcx_abapgit_exception=>raise( 'No branches, create branch manually by adding file' ).
      ELSE.
        CONTINUE.
      ENDIF.

      ASSERT lv_name IS NOT INITIAL.

      APPEND INITIAL LINE TO et_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1         = lv_hash2.
      <ls_branch>-name         = lv_name.

*      <ls_branch>-display_name = get_display_name( lv_name ).
      <ls_branch>-display_name = lv_name.
      IF <ls_branch>-display_name CP zif_abapgit_definitions=>c_git_branch-heads.
        REPLACE FIRST OCCURRENCE OF zif_abapgit_definitions=>c_git_branch-heads_prefix IN <ls_branch>-display_name WITH ''.
      ELSEIF <ls_branch>-display_name CP zif_abapgit_definitions=>c_git_branch-tags.
        REPLACE FIRST OCCURRENCE OF zif_abapgit_definitions=>c_git_branch-prefix IN <ls_branch>-display_name WITH ''.
      ENDIF.

*      <ls_branch>-type         = get_type( iv_branch_name       = lv_name
*                                           it_result            = lt_result
*                                           iv_current_row_index = lv_current_row_index ).
      DATA: lv_annotated_tag_with_suffix TYPE string.

      FIELD-SYMBOLS: <lv_result> TYPE LINE OF string_table.

      <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-other.

      IF lv_name CP zif_abapgit_definitions=>c_git_branch-heads OR
         lv_name = zif_abapgit_definitions=>c_head_name.
        <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-branch.

      ELSEIF lv_name CP zif_abapgit_definitions=>c_git_branch-tags.

        lv_annotated_tag_with_suffix = lv_name && '^{}'.

        READ TABLE lt_result ASSIGNING <lv_result>
                             INDEX lv_current_row_index + 1.
        IF sy-subrc = 0 AND <lv_result> CP '*' && lv_annotated_tag_with_suffix.
          <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
        ELSE.
          <ls_branch>-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
        ENDIF.

      ENDIF.

      IF <ls_branch>-name = zif_abapgit_definitions=>c_head_name OR <ls_branch>-name = ev_head_symref.
        <ls_branch>-is_head = abap_true.
      ENDIF.
    ENDLOOP.


*&---------------------------------------------------------------------*
*&   Get hash from Response
*&---------------------------------------------------------------------*

    DATA: ls_branch      TYPE ty_git_branch,
          lv_branch      TYPE ty_sha1,
          lv_branch_name TYPE string.

*   ev_branch = eo_branch_list->find_by_name( iv_branch_name )-sha1.
    IF ls_repo-branch_name IS INITIAL.
*      zcx_abapgit_exception=>raise( 'Branch name empty' ).
    ELSE.
      IF ls_repo-branch_name CP zif_abapgit_definitions=>c_git_branch-tags.
*      rs_branch = find_tag_by_name( iv_branch_name ).
        lv_branch_name = ls_repo-branch_name && '^{}'.

        READ TABLE et_list INTO ls_branch
            WITH KEY name = lv_branch_name.
        IF sy-subrc <> 0.

          READ TABLE et_list INTO ls_branch
            WITH KEY name = ls_repo-branch_name.
          IF sy-subrc <> 0.
*          zcx_abapgit_exception=>raise( 'Branch not found' ).
          ENDIF.
        ENDIF.
      ELSE.

        READ TABLE et_list INTO ls_branch
          WITH KEY name = ls_repo-branch_name.
        IF sy-subrc <> 0.
*        zcx_abapgit_exception=>raise( |Branch { get_display_name( iv_branch_name )
*          } not found. Use 'Branch' > 'Switch' to select a different branch| ).
        ENDIF.

      ENDIF.
      lv_branch = ls_branch-sha1.
    ENDIF.

*    IF it_branches IS INITIAL.
*      APPEND ev_branch TO lt_hashes.
*    ELSE.
*      LOOP AT it_branches ASSIGNING <ls_branch>.
*        APPEND <ls_branch>-sha1 TO lt_hashes.
*      ENDLOOP.
*    ENDIF.


  ENDIF.
ENDLOOP.























*****DATA: lo_client   TYPE REF TO zcl_abapgit_http_client,
*****      lt_hashes   TYPE ty_sha1_tt,
*****      it_branches TYPE ty_git_branch_list_tt,
*****      ev_branch   TYPE ty_sha1.
*****
*****FIELD-SYMBOLS: <ls_branch> LIKE LINE OF it_branches.
******    CLEAR: et_objects,
******           ev_branch.
*****
*****LOOP AT rt_repos INTO ls_repo.
*****
*****
*****
*****
*****
*****
********       perform find_branch  using ls_repo-url
********                                  c_service-upload
********                                  iv_branch_name
********                         changing lo_client
********                                  ev_branch.
*****
*****
******    find_branch(
******      EXPORTING
******        iv_url         = iv_url
******        iv_service     = c_service-upload
******        iv_branch_name = iv_branch_name
******      IMPORTING
******        eo_client      = lo_client
******        ev_branch      = ev_branch ).
******
*****  IF it_branches IS INITIAL.
*****    APPEND ev_branch TO lt_hashes.
*****  ELSE.
*****    LOOP AT it_branches ASSIGNING <ls_branch>.
*****      APPEND <ls_branch>-sha1 TO lt_hashes.
*****    ENDLOOP.
*****  ENDIF.
*****
*****ENDLOOP.
