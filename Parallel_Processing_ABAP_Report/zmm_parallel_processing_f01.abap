*&---------------------------------------------------------------------*
*& Include          ZMM_SHORTAGE_POC_ALV_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_data .
  FIELD-SYMBOLS <mrp_lines> TYPE bapi_mrp_ind_lines.
  DATA lv_stock TYPE vrfmg.

  CONSTANTS: lc_group TYPE rzllitab-applserver VALUE 'parallel_generators'.

  DATA: lv_max_threads TYPE i,
        lv_task_count  TYPE i,
        lv_initiated   TYPE i.

  DATA: lt_supplier_details TYPE STANDARD TABLE OF zmms_shortage_supp_details,
        lt_mara             TYPE STANDARD TABLE OF mara.

  "Select vendor data to be passed into main function
  SELECT psl~matnr, psl~werks, lfa1~lifnr, lfa1~name1, psl~vdatu, psl~bdatu
    FROM eord AS psl INNER JOIN lfa1
                             ON psl~lifnr = lfa1~lifnr
    FOR ALL ENTRIES IN @gt_matnr
   WHERE psl~matnr EQ @gt_matnr-matnr
     AND psl~werks EQ @p_werks
     AND psl~vdatu LE @sy-datum
     AND psl~bdatu GE @sy-datum
    INTO CORRESPONDING FIELDS OF TABLE @lt_supplier_details.

  lt_mara = CORRESPONDING #( gt_matnr ).

  "Get the available threads
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = lc_group
    IMPORTING
      free_pbt_wps                   = lv_max_threads
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.
  IF sy-subrc <> 0.
*   fallback to running sequentially. If SPBT_INITIALIZE fails, check transactions
*   RZ12, SM50, SM21, SARFC
    lv_max_threads = 1.
  ENDIF.

  IF lv_max_threads > 2.
    lv_max_threads = lv_max_threads - 2.
  ELSEIF lv_max_threads EQ 2.
    lv_max_threads = lv_max_threads - 1.
  ENDIF.

  ASSERT lv_max_threads >= 1.

  IF lv_max_threads > 32.
* https://en.wikipedia.org/wiki/Amdahl%27s_law
    lv_max_threads = 32.
  ENDIF.

  "Create the parallel tasks based on available threads
  DO.
    ADD 1 TO lv_task_count.
    DESCRIBE TABLE lt_mara LINES DATA(lv_tot_records).

    IF lv_tot_records IS NOT INITIAL AND sy-index EQ 1.
      DATA(lv_package_count) = COND i(
                                 WHEN lv_tot_records GE lv_max_threads
                                   THEN lv_tot_records / lv_max_threads
                                 ELSE 1
                               ).

      DATA(lv_lines_to_be_deleted) = COND i(
                                       WHEN lv_tot_records LT lv_package_count - 1
                                         THEN 0
                                       ELSE lv_package_count + 1
                                     ).
    ELSEIF lv_tot_records LT lv_lines_to_be_deleted.
      CLEAR lv_lines_to_be_deleted.
    ENDIF.

    DATA(lt_matnr_package) = lt_mara.

    IF lv_lines_to_be_deleted IS NOT INITIAL.
      DELETE lt_matnr_package FROM lv_lines_to_be_deleted.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_timestamp).
    DATA(lv_task_name) = |ZMM_SHORTAGE_{ lv_task_count }_{ lv_timestamp }|.

    CALL FUNCTION 'ZMM_SHORTAGE_PARALLEL_PROC'
      STARTING NEW TASK lv_task_name
      DESTINATION IN GROUP lc_group
      PERFORMING f_get_final_data ON END OF TASK
      EXPORTING
        werks                 = p_werks
      TABLES
        t_mara                = lt_matnr_package
        t_supplier_details    = lt_supplier_details
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc NE 0.
      MESSAGE |Error at starting parallel processing: { lv_task_name }| TYPE 'I' DISPLAY LIKE 'W'.
    ELSE.
      ADD 1 TO lv_initiated.
    ENDIF.

    IF lv_lines_to_be_deleted IS NOT INITIAL.
      DATA(lv_lines_to_del_from_source) = COND i( WHEN lv_lines_to_be_deleted GT 1
                                              THEN lv_lines_to_be_deleted - 1
                                            ELSE lv_lines_to_be_deleted
                                    ).

      DELETE lt_mara FROM 1 TO lv_lines_to_del_from_source.
    ELSE.
      CLEAR lt_mara.
    ENDIF.

    IF lt_mara IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

  "Wait until all initiated processess be processed OR until 15 minutes
  WAIT UNTIL gv_processed GE lv_initiated UP TO 900 SECONDS. "15 minutes
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INIT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_init_data .
  IF s_matnr IS NOT INITIAL.
    gt_matnr = VALUE tty_matnr(
                 FOR ls_matnr IN s_matnr
                 ( matnr = ls_matnr-low )
               ).
  ELSE.
    SELECT matnr
      FROM nsdm_v_marc
       INTO CORRESPONDING FIELDS OF TABLE gt_matnr
      WHERE werks EQ p_werks
        AND dispo IN s_dispo.
  ENDIF.

  IF gt_matnr[] IS INITIAL.
    MESSAGE |No data found.| TYPE 'S' DISPLAY LIKE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module PBO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
*  SET PF-STATUS 'MAIN100'.
*
*
*  IF g_custom_container IS INITIAL.
*    CREATE OBJECT g_custom_container
*      EXPORTING
*        container_name = g_container.
*    CREATE OBJECT grid1
*      EXPORTING
*        i_parent = g_custom_container.
*
*    CALL METHOD grid1->set_table_for_first_display
*      EXPORTING
*        i_structure_name = 'ZMM_SHORTAGE'
*      CHANGING
*        it_outtab        = gt_shortage.
*  ENDIF.
*
**Create object of the event class and setting handler for double click
*  CREATE OBJECT event_receiver.
*  SET HANDLER event_receiver->handle_double_click FOR grid1.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
*   to react on oi_custom_events:
*  CALL METHOD cl_gui_cfw=>dispatch.
*  CASE ok_code.
*    WHEN 'EXIT'.
*      PERFORM exit_program.
*    WHEN OTHERS.
**     do nothing
*  ENDCASE.
*  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exit_program .
*  CALL METHOD G_CUSTOM_CONTAINER->FREE.
*  CALL METHOD CL_GUI_CFW=>FLUSH.
  LEAVE PROGRAM.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_show_alv .
  SORT gt_shortage BY matnr.

  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_shortage ).

    CATCH cx_root INTO DATA(lcx).
      MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  gr_table->get_functions( )->set_all( abap_true ).
  gr_table->get_display_settings( )->set_striped_pattern( cl_salv_display_settings=>true ).
  gr_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  gr_table->get_columns( )->set_optimize( abap_true ).

*  PERFORM f_send_alv_in_email_attach.

  DATA(lr_handle_events) = NEW gcl_handle_events( ).
  DATA(lr_events) = gr_table->get_event( ).

  SET HANDLER lr_handle_events->handle_double_click FOR lr_events.

  gr_table->display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SEND_ALV_IN_EMAIL_ATTACH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_send_alv_in_email_attach .
  DATA: lv_xstring      TYPE xstring,
        lv_body_content TYPE string,
        lv_date         TYPE char10.

  CHECK gr_table IS BOUND.
  lv_body_content = '<html xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:w="urn:schemas-microsoft-com:office:word xmlns' &&
                    ':m="http://schemas.microsoft.com/office/2004/12/omml" xmlns="http://www.w3.org/TR/REC-html40"><head><meta http-equiv=Content-Type content="' &&
                    'text/html; charset=windows-1252"><meta name=ProgId content=Word.Document><meta name=Generator content="Microsoft Word 15"><meta name=' &&
                    'Originator content="Microsoft Word 15"><style><!--  /* Font Definitions */  @font-face   {font-family:"Cambria Math";  panose-1:2 4 5 3 5 4 6 3 2 4;' &&
                    'mso-font-charset:0;  mso-generic-font-family:roman;  mso-font-pitch:variable;  mso-font-signature:3 0 0 0 1 0;} @font-face   {font-family:Calibri;' &&
                    'panose-1:2 15 5 2 2 2 4 3 2 4;   mso-font-charset:0;   mso-generic-font-family:swiss;  mso-font-pitch:variable;  mso-font-signature:-469750017 -10737' &&
                    '32485 9 0 511 0;}  /* Style Definitions */  p.MsoNormal, li.MsoNormal, div.MsoNormal   {mso-style-unhide:no;   mso-style-qformat:yes;  mso-style-' &&
                    'parent:"";   margin:0cm;   mso-pagination:widow-orphan;  font-size:11.0pt;   font-family:"Calibri",sans-serif;   mso-ascii-font-family:Calibri;  mso-' &&
                    'ascii-theme-font:minor-latin;  mso-fareast-font-family:Calibri;  mso-fareast-theme-font:minor-latin;   mso-hansi-font-family:Calibri;  mso-hansi-' &&
                    'theme-font:minor-latin;  mso-bidi-font-family:"Times New Roman";   mso-bidi-theme-font:minor-bidi;   mso-fareast-language:EN-US;} a:link, span.Mso' &&
                    'Hyperlink  {mso-style-noshow:yes;  mso-style-priority:99;  color:#0563C1;  mso-themecolor:hyperlink;   text-decoration:underline;  text-under' &&
                    'line:single;} a:visited, span.MsoHyperlinkFollowed   {mso-style-noshow:yes;  mso-style-priority:99;  color:#954F72;  mso-themecolor:followedhyperlink;' &&
                    'text-decoration:underline;   text-underline:single;} span.EmailStyle17   {mso-style-type:personal-compose;   mso-style-noshow:yes;' &&
                    'mso-style-unhide:no;   mso-ansi-font-size:11.0pt;  mso-bidi-font-size:11.0pt;  font-family:"Calibri",sans-serif;   mso-ascii-font-family:Calibri;' &&
                    'mso-ascii-theme-font:minor-latin;  mso-fareast-font-family:Calibri;  mso-fareast-theme-font:minor-latin;   mso-hansi-font-family:Calibri;' &&
                    'mso-hansi-theme-font:minor-latin;  mso-bidi-font-family:"Times New Roman";   mso-bidi-theme-font:minor-bidi;   color:windowtext;} span.Spell' &&
                    'E  {mso-style-name:"";   mso-spl-e:yes;} .MsoChpDefault  {mso-style-type:export-only;  mso-default-props:yes;  font-family:"Calibri",sans-serif;' &&
                    'mso-ascii-font-family:Calibri;   mso-ascii-theme-font:minor-latin;   mso-fareast-font-family:Calibri;  mso-fareast-theme-font:minor-latin;' &&
                    'mso-hansi-font-family:Calibri;   mso-hansi-theme-font:minor-latin;   mso-bidi-font-family:"Times New Roman";   mso-bidi-theme-font:minor-bidi;' &&
                    'mso-fareast-language:EN-US;} @page WordSection1  {size:612.0pt 792.0pt;  margin:70.85pt 3.0cm 70.85pt 3.0cm;   mso-header-margin:36.0pt;' &&
                    'mso-footer-margin:36.0pt;  mso-paper-source:0;} div.WordSection1   {page:WordSection1;}  /* List Definitions */  @list l0  {mso-list-id:34161923;' &&
                    'mso-list-type:hybrid; 	mso-list-template-ids:505813020 68550671 68550681 68550683 68550671 68550681 68550683 68550671 68550681 68550683;} @' &&
                    'list l0:level1   {mso-level-tab-stop:none;   mso-level-number-position:left;   text-indent:-18.0pt;} @list l0:level2   {mso-level-number-format' &&
                    ':alpha-lower;  mso-level-tab-stop:none;  mso-level-number-position:left;   text-indent:-18.0pt;} @list l0:level3   {mso-level-number-format:roman-' &&
                    'lower;   mso-level-tab-stop:none;  mso-level-number-position:right;  text-indent:-9.0pt;} @list l0:level4  {mso-level-tab-stop:none;' &&
                    'mso-level-number-position:left;  text-indent:-18.0pt;} @list l0:level5   {mso-level-number-format:alpha-lower;   mso-level-tab-stop:none;' &&
                    'mso-level-number-position:left;  text-indent:-18.0pt;} @list l0:level6   {mso-level-number-format:roman-lower;   mso-level-tab-stop:none;' &&
                    'mso-level-number-position:right;   text-indent:-9.0pt;} @list l0:level7  {mso-level-tab-stop:none;   mso-level-number-position:left;' &&
                    'text-indent:-18.0pt;} @list l0:level8  {mso-level-number-format:alpha-lower;   mso-level-tab-stop:none;  mso-level-number-position:left;' &&
                    'text-indent:-18.0pt;} @list l0:level9  {mso-level-number-format:roman-lower;   mso-level-tab-stop:none;  mso-level-number-position:right;' &&
                    'text-indent:-9.0pt;} ol  {margin-bottom:0cm;} ul   {margin-bottom:0cm;} --></style><!--[if gte mso 10]><style>/* Style Definitions */  table' &&
                    '.MsoNormalTable  {mso-style-name:"Table Normal";   mso-tstyle-rowband-size:0;  mso-tstyle-colband-size:0;  mso-style-noshow:yes;' &&
                    'mso-style-priority:99;   mso-style-parent:"";  mso-padding-alt:0cm 5.4pt 0cm 5.4pt;  mso-para-margin:0cm;  mso-pagination:widow-orphan;' &&
                    'font-size:11.0pt;  font-family:"Calibri",sans-serif;   mso-ascii-font-family:Calibri;  mso-ascii-theme-font:minor-latin;   mso-hansi-font-' &&
                    'family:Calibri;  mso-hansi-theme-font:minor-latin;   mso-bidi-font-family:"Times New Roman";   mso-bidi-theme-font:minor-bidi;   mso-fareast' &&
                    '-language:EN-US;} table.Tabelanormal   {mso-style-name:"Tabela normal";  mso-tstyle-rowband-size:0;  mso-tstyle-colband-size:0;  mso-style-' &&
                    'noshow:yes; 	mso-style-priority:99; 	mso-style-unhide:no; 	mso-style-parent:""; 	mso-padding-alt:0cm 5.4pt 0cm 5.4pt; 	mso-para-margin:0cm;' &&
                    'mso-pagination:widow-orphan;   font-size:11.0pt;   font-family:"Calibri",sans-serif;   mso-ascii-font-family:Calibri;  mso-ascii-theme-font:' &&
                    ' minor-latin;  mso-fareast-font-family:"Times New Roman";  mso-fareast-theme-font:minor-fareast;   mso-hansi-font-family:Calibri;  mso-hansi-' &&
                    'theme-font:minor-latin;  mso-bidi-font-family:"Times New Roman";   mso-bidi-theme-font:minor-bidi;}</style><![endif]--><!--[if gte mso 9]>' &&
                    '<xml><o:shapedefaults v:ext="edit" spidmax="1026"/></xml><![endif]--><!--[if gte mso 9]><xml><o:shapelayout v:ext="edit"><o:idmap v:ext="edit" data' &&
                    '="1"/></o:shapelayout></xml><![endif]--></head><body lang=PT-BR link="#0563C1" vlink="#954F72" style=' &&
                    |'tab-interval:35.4pt; word-wrap:break-word'><div class=WordSection1><p class=MsoNormal>| &&
                    |<span class=SpellE>Hello</span>, Dear.<o:p/></p><p class=MsoNormal><o:p>&nbsp;</o:p></p>| &&
                    |<p class=MsoNormal><span lang=EN-US style='mso-ansi-language:EN-US'>| &&
                    |Enclosed is the document with the Shortage Report data, taken on 07.21.2022 on S/4HANA.<o:p/></span></p></div></body></html>|.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_body_content
    IMPORTING
      buffer = lv_xstring
    EXCEPTIONS
      failed = 1
      OTHERS = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  TRY .
      DATA(msg) = NEW cl_bcs_message( ).

      WRITE sy-datum TO lv_date .
      msg->set_subject( |S/4 Shortage Report - { lv_date  }| ).

      msg->set_main_doc(
        EXPORTING
          iv_contents_bin = lv_xstring
          iv_doctype      = 'HTM'
          iv_codepage     = 4110 ). "UTF-8"

      SELECT SINGLE FROM puser002
        FIELDS bname, smtp_addr
        WHERE bname EQ @sy-uname
         INTO @DATA(ls_user_data).

      IF NOT ls_user_data-smtp_addr IS INITIAL.
        msg->add_recipient( CONV #( ls_user_data-smtp_addr ) ).
      ELSE.
        msg->add_recipient( |everton.esquia@delaware.com| ).
      ENDIF.

      CLEAR lv_xstring.
      lv_xstring = gr_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

      msg->add_attachment(
        EXPORTING
          iv_doctype      = 'EXT'
          iv_filename     = |Shortage_Report_{ lv_date }.xlsx|
          iv_contents_bin = lv_xstring ).

      msg->set_send_immediately( abap_true ).

      msg->send( ).

      MESSAGE |Shortage Report sent by e-mail.| TYPE 'S'.

    CATCH cx_root INTO DATA(lcx).
      MESSAGE lcx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.

FORM f_get_final_data USING p_taskname.
  DATA: lt_shortage TYPE STANDARD TABLE OF zmm_shortage.

  RECEIVE RESULTS FROM FUNCTION 'ZMM_SHORTAGE_PARALLEL_PROC'
      TABLES
        t_shortage         = lt_shortage.

  APPEND LINES OF lt_shortage TO gt_shortage.
  ADD 1 TO gv_processed.
ENDFORM.
