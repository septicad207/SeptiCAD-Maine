customEdit2 : dialog {

  label = "  SeptiCAD --- Customize Cross-Section Notes  ";
  : column {


   : boxed_column {
	  : boxed_row {
		: text {
		key = "DESCRIPTION";
		value = "";
		alignment = centered;
		height = 2;
		}
	  }// boxed_row
	: boxed_row {
		: column {
			: text {
			value = "System Name";
			is_bold = true;
			alignment = centered;
			height = 1;
			}
	
		        : list_box {
		          key = "NAME";
		          label = "";
		          multiple_select = false;
		          width = 38.0;
			  height = 25.0;
                          action = "(shadowList3 \"NAME\" \"SHORTHAND\" \"NOTE\")";
		        }
		}
		: column {
			: text {
			value = "Shorthand Name";
			is_bold = true;
			alignment = centered;
			height = 1;
			}
	
		        : list_box {
		          key = "SHORTHAND";
		          label = "";
		          multiple_select = false;
		          width = 25.0;
			  height = 25.0;
                          action = "(shadowList3 \"SHORTHAND\" \"NAME\" \"NOTE\")";
		        }
		}
		: column {
			: text {
			value = "Note or Block for System";
			is_bold = true;
			alignment = centered;
			height = 1;
			}
		        : list_box {
		          key = "NOTE";
		          label = "";
		          multiple_select = false;
		          width = 90.0;
			  height = 25.0;
                          action = "(shadowList3 \"NOTE\" \"NAME\" \"SHORTHAND\")";
		        }
		}
	}// boxed_row     
   }/// end boxed column

      : row {
        : button {
          key = "cancel";
          label = "  Ok / Back  ";
          is_default = true;
          is_cancel = true;
        }
        : button {
          key = "notepad";
          label = "  Edit in Notepad  ";
          is_default = false;
          is_cancel = false;
        }
      }
   
}
}