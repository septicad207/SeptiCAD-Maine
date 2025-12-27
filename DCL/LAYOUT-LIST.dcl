LAYOUT_LIST : dialog {

  label = " SeptiCAD - Choose Layout  ";
  initial_focus = "LAYOUTS";
  : column {

      : column {
	: text {
            key = "LAYOUTS-TEXT";
	    value = "";
            height = "5";
	    alignment = centered;
        }

        : list_box {
          key = "LAYOUTS";
          multiple_select = false;
          width = 40.0;
	  height = 10.0;
        }
       }


      : row {
        : button {
          key = "okay";
          label = "  Okay  ";
          is_default = true;
          is_cancel = false;
        }
        : button {
          key = "cancel";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
      }


   
}
}