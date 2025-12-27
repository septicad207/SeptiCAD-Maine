DBOX : dialog {

  label = "  Choose D-Box Location ";
	: row {
		: text {
		key = "TEXT";
		label = ""; 
		}
	} // end row

      : row {
        : button {
          key = "left";
          label = "  Left Side  ";
          is_default = true;
        }
        : button {
          key = "right";
          label = "  Right Side  ";
          is_default = false;
        }
        : button {
          key = "cancel";
          label = "  Cancel  ";
          is_default = false;
          is_cancel = true;
        }
  
}// end button row
}// end dialog