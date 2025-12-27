sblock : dialog {

  label = "  SeptiCAD --- Block Object Insert ";
  : column {
      : boxed_column {
        : list_box {
          key = "list_tile";
          label = "Choose Blocks to Insert";
          multiple_select = false;
          width = 40.0;
	  height = 14.0;
	  action ="(blockText)";
        }
	: edit_box {
        key = "text_description";
        label = "Enter Description-->";
        edit_width = 50.0;
        value = "";
        }
	: text {
	key = "T4";
	label = "Leave BLANK for no Description"; 
	}  
       }
   : boxed_column {
     : row {
	: popup_list {
        key = "scaleList";
        label = "";
        width = 15;
        value = "0";
	}	
      	: edit_box {
        key = "fontSize";
        label = "or Font Size-->";
        edit_width = 4;
        value = "";
        }
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
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
        : button {
          key = "CUSTOM";
          label = "  Customize  ";
          is_default = false;
          is_cancel = false;
        }
      }
   
}
}