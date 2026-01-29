mapScale : dialog {

  label = "  SeptiCAD --- HHE-200 - Maps and Cross Section Scales ";
	: column {
	: text {
	height = "1.5";
        is_bold = true;
	label = "Maps Scale --- if 1\":100' scale enter 100"; 
	}
	: edit_box {
        key = "page2_scale";
        label = "Scale of Page 2 Map-> ";
        edit_width = 4;
	action = "(checkMapScale)";
        value = "60";
        }
	: edit_box {
        key = "page3_scale";
        label = "Scale of Page 3 Map-> ";
        edit_width = 4;
        value = "20";
	action = "(checkMapScale)";
        }
	} // end column

	: column {
	: text {
	height = "1.5";
        is_bold = true;
	label = "Cross Section --- if 1\":5' scale enter 5"; 
	}
	: edit_box {
        key = "profile_scale";
        label = "Scale of Cross Section-> ";
        edit_width = 4;
        value = "5";
	action = "(checkMapScale)";
        }
	} // end column

      : row {
        : button {
          key = "okay";
          label = "  Okay  ";
          is_default = true;
        }
        : button {
          key = "cancel";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }


   
}
}