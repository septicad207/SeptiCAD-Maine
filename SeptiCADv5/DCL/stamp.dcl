STAMP : dialog {
  label = "  SeptiCAD --- Sign HHE-200 ";
	: column {
                 label = "Site Evaluator to Sign HHE-200";
                  : popup_list {
                   key = "DESIGNER";
                   label = "Select Site Evaluator-> ";
                   width = 50;
                   value = "0";/// "0" IS DEFAULT USER      
                  }
	} // end column
      : row {
        : button {
          key = "sign";
          label = "  Sign  ";
          is_default = true;
        }

        : button {
          key = "cancel";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
  
}// end button row
}// end dialog