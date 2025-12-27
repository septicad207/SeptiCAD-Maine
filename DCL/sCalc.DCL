SCALC : dialog { 
          label = "SeptiCAD --- Calculator"; 
: column {

/// START COLUMN XX
    : boxed_column {
      label = "System Details";
              : text {
                key = "DETAILS";
                alignment = left;
                value = "";
                width = 80;
              }
    }
    : boxed_column {
      label = "System Dimensions";
              : text {
                key = "DIMENSIONS";
                alignment = left;
                value = "";
                width = 60;
              }
    }

	: boxed_column {
          label = "System Layout";
              : popup_list {
                key = "SYSTEM";
                label = "";
                width = 10;
                value = "0";
		action = "(sCalcAutoFill)";
              }	      
	     : row {
              : text {
                key = "NUM_ROWS_TEXT";
                alignment = left;
                value = "Number of Rows-> ";
                width = 30;
              }
	      : edit_box {
                key = "NUM_ROWS";
                label = "";
                edit_width = 4;
		action = ""; 
                value = "";
		action = "(sCalcAutoFill)";
              }
            }

	    :row {
              : text {
                key = "NUM_UNITS_TEXT";
                alignment = left;
                value = "Total Length of System [ft]-> ";
                width = 30;
              }
	      : edit_box {
                key = "NUM_UNITS";
                label = "";
                edit_width = 4;
                value = "";
		action = "(sCalcAutoFill)"; 
             }
             }

	     : row {
              : text {
                key = "ROW_SEP_TEXT";
                alignment = left;
                value = "";
                width = 30;
              }
              : edit_box {
                key = "ROW_SEP";
                label = "";
                edit_width = 4;
                value = "";
		action = "(sCalcAutoFill)";
              }
             }
	     : row {
              : text {
                alignment = left;
                value = "Stone Beside [inches] ->";
                width = 30;
              }
              : edit_box {
                key = "STONE_BESIDE";
                label = "";
                edit_width = 4;
                value = "";
		action = "(sCalcAutoFill)";
              }
             }

            }
/// END COLUMN XX

: row {
              : button {
                key = "calculate";
                label = "Calculate";
                is_default = true;
                is_cancel = false;
              }
              : button {
                key = "close";
                label = "Close";
                is_default = false;
                is_cancel = true;
              }
}
  }// end column 
}// end dialog 