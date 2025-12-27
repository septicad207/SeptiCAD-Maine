scarifySimple : dialog {

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

         : button {
          key = "label";
          label = "  Add Label  ";
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