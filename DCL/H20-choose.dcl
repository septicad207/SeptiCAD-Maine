H20 : dialog {

  label = "  SeptiCAD --- Concrete Chambers Standard or H-20 Load Rated";
	: row {
		: text {
		height = "1.5";
	        is_bold = true;
		label = "Choose H-20 Load or Standard Load Rated"; 
		}
	} // end row

      : row {
        : button {
          key = "H20";
          label = "  H-20 Load  ";
          is_default = true;
        }
        : button {
          key = "STANDARD";
          label = "  Standard  ";
          is_default = false;
        }
        : button {
          key = "CANCEL";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
  
}// end button row
}// end dialog