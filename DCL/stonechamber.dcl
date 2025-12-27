STONECHAMBER : dialog {

  label = "  SeptiCAD --- High Capacity Chambers ";
	: row {
		: text {
		height = "1.5";
	        is_bold = true;
		label = "Place 3\" of crushed stone beside each chambers."; 
		}
	} // end row

      : row {
        : button {
          key = "yes";
          label = "  Yes  ";
          is_default = true;
        }
        : button {
          key = "no";
          label = "  No  ";
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