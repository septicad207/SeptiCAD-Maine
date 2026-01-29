slabel : dialog {

  label = "  SeptiCAD --- Septic Notes  ";
  initial_focus = "altText1";
  : column {

      : boxed_column {

        : list_box {
          key = "textList";
          label = "Choose Notes to Insert [hold CTRL or Shift to select multiple items]";
          multiple_select = true;
          width = 65.0;
	  height = 15.0;
        }
	: edit_box {
        key = "altText1";
        label = "Enter Other Text-->";
        edit_width = 50.0;
        value = "";
        }
	: edit_box {
        key = "altText2";
        label = "Enter Other Text-->";
        edit_width = 50.0;
        value = "";
        }
	: text {
	    value = "For Newline type \\P [example ->  Septic\\PTank]";
	    alignment = centered;
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
	: image_button {
	          width = 15;
	          height = 3;
		  key = "arrow";
       		  is_default = false;
	} 
	: image_button {
	          width = 15;
	          height = 3;
		  key = "spline";
       		  is_default = false;
	} 
	: image_button {
	          width = 15;
	          height = 3;
		  key = "rText";
       		  is_default = false;
	} 
	: image_button {
	          width = 8;
	          height = 3;
		  key = "mText";
       		  is_default = false;
	} 
      }



      : row {
        : button {
          key = "loadNotes";
          label = "  Load Notes  ";
          is_default = false;
          is_cancel = false;
	  action = "(sLabelPageNotes)";
        }
        : button {
          key = "customize";
          label = "  Customize  ";
          is_default = false;
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