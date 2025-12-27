CROWN : dialog {

  label = "  Buried System - Final Grade Type ";
	: row {
		: text {
		key = "TEXT";
		label = ""; 
		}
	} // end row

      : row {
        : button {
          key = "left";
          label = "  3% Crown  ";
          is_default = false;
        }
        : button {
          key = "right";
          label = "  Existing Grade = Final Grade  ";
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