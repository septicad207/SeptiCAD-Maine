TEMPLATE : dialog {

  label = "  Opening SeptiCAD Template File ";
	: column {
		: text {
		key = "TEXT";
		label = "A SeptiCAD Template file is required"; 
                height = 2;
		}
		: text {
		key = "TEXT1";
		label = "To Continue you must Open a SeptiCAD Template";
                height = 2;
		}

	} // end row

      : row {
        : button {
          key = "NEW";
          label = "  Open a SeptiCAD Template ";
          is_default = true;
        }
        : button {
          key = "CANCEL";
          label = "  Exit/Cancel  ";
          is_default = false;
          is_cancel = true;
        }
  
}// end button row
}// end dialog