pageNotes : dialog {

  label = "  SeptiCAD --- Note Library  ";
  : column {

      : boxed_column {
	: text {
	  key = "DESCRIPTION";
	  value = "";
	  alignment = centered;
	  height = 2;
	}

        : list_box {
          key = "noteList";
          label = "Choose Notes to Insert [use CTRL and SHIFT for multiple]";
          multiple_select = true;
          width = 90.0;
	  height = 20.0;
        }

	: edit_box {
        key = "altText1";
        label = "Enter Other Text-->";
        edit_width = 70.0;
        value = "";
        }
	: edit_box {
        key = "altText2";
        label = "Enter Other Text-->";
        edit_width = 70.0;
        value = "";
        }
     }// end boxed_column



      : row {
        : button {
          key = "okay";
          label = "  Okay  ";
          is_default = true;
        }
        : button {
          key = "cancel";
          label = "  Cancel\Skip  ";
          is_default = false;
          is_cancel = true;
        }
        : button {
          key = "customize";
          label = "  Customize  ";
          is_default = false;
          is_cancel = false;
        }

      }
   
}
}