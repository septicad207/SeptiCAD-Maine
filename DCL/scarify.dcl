scarify : dialog {

  label = "  SeptiCAD --- Scarify / Transitional Horizon Tool  ";

  : boxed_row {

		: column {
			: text {
			value = "Scarify \\ Transitional Horizon Note";
			alignment = left;
			height = 1;
			}
		        : list_box {
		          key = "NOTE";
		          label = "";
		          multiple_select = false;
		          width = 140.0;
			  height = 6.0;
		        }
		}
  }

      : row {
       : row {
	: image_button {
	          width = 15;
	          height = 10;
		  key = "platform";
       		  is_default = true;
	} 
	: image_button {
	          width = 15;
	          height = 10;
		  key = "trench";
       		  is_default = false;
	} 
       }
       : column {  

        : button {
          key = "cancel";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
        : button {
          key = "customize";
          label = "  Edit Notes ";
          is_default = false;
          is_cancel = false;
        }
       }
      }


   

}