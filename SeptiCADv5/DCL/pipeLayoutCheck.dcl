PIPELAYOUTCHECK : dialog {

  label = "  Distribution Pipe Layout  ";
	: row {
		: text {
		label = " Is the Distribution Pipe Layout Correct ? "; 
		}
	} // end row

      : row {
        : button {
          key = "okay";
          label = "  Okay/Continue  ";
          is_default = true;
          is_cancel = false;
        }
        : button {
          key = "redo";
          label = "  Redo Pipe Layout  ";
          is_default = false;
          is_cancel = true;
        }
  
}// end button row
}// end dialog