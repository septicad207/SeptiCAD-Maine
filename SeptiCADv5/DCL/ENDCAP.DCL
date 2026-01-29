ENDCAP : dialog {

  label = "  Choose Quick 4 Plus Endcap Type ";
	: row {
		: text {
		key = "TEXT";
		label = ""; 
		}
	} // end row

      : row {
        : button {
          key = "left";
          label = "  Quick4 Plus 8 Endcap (4.5 inches long) ";
          is_default = true;
        }
        : button {
          key = "right";
          label = "  Quick4 Plus All-in-One 8 Endcap (13.3 inches long) ";
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