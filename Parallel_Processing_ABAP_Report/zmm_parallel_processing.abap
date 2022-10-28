*&---------------------------------------------------------------------*
*& Report zmm_parallel_processing
*&---------------------------------------------------------------------*
*& Report to display shortage at a certain date for all products.
*& This one uses parallel processing strategy.
*&---------------------------------------------------------------------*
REPORT zmm_parallel_processing.

INCLUDE zmm_parallel_processing_top.
INCLUDE zmm_parallel_processing_scr.
INCLUDE zmm_parallel_processing_f01.

************************************************************************
* START-OF-SELECTION.
************************************************************************
START-OF-SELECTION.

  PERFORM f_init_data.    "init parameters/select options
  PERFORM f_get_data.     "get MD04 details

************************************************************************
* END-OF-SELECTION.
************************************************************************
END-OF-SELECTION.

  IF gt_shortage[] IS INITIAL.
    MESSAGE |No data found.| TYPE 'S' DISPLAY LIKE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_show_alv.