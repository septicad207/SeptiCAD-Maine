tankElevation : dialog {

  label = "  Gravity System - Septic Tank Outlet Elevation Calculator";

     : boxed_column {
 
        : row {
	 : text {
           label = "Top of Leachfield Inlet Pipe Elevation ->";
	 }

         : edit_box {
           key = "CHAMBERINVERT";
           label = "";
           edit_width = 6;
           is_enabled = false;
         }
        }




            : row {
                : edit_box {
                key = "DBOXDROP";
                label = "Distribution Box Elevation Drop (positive, use zero if not specified) ->";
                edit_width = 3;
                value = "0";
                }
            }

         : edit_box {
           key = "PIPELENGTH";
           label = "Length of Pipe Required at 1/8 inch/foot (1%) Pitch ->";
           edit_width = 5;
           is_enabled = true;
         }



: boxed_row {
      : button {
        key = "CALCULATE";
        label = "Preview Note";
        height = 3;
        is_default = true;
        is_cancel = false;
      }
     : boxed_column {
           label = "Page 3 Note Will Read";
	 : text {
           key = "NOTE1";
           width = 60;
           height = 2;
	 }
	 : text {
           key = "NOTE2";
           width = 60;
           height = 2;
	 }
     }
}




	: row {
	        : button {
	          key = "LABEL";
	          label = "Include in Page 3 Notes";
	          is_default = false;
	          is_cancel = false;
	        }
	        : button {
	          key = "CLOSE";
	          label = "Ignore / Pumped System";
	          is_default = false;
	          is_cancel = true;
	        }
	}
   }

}