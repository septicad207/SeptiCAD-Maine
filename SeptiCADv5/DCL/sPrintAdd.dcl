sPrintAdd : dialog {

  label = "  SeptiCAD Add\Modify Preset Printers ";

: column {

///start big row
: row {

: boxed_row {
       label = "  Modify List of Printers ";

: column {
		 : text {
		  value = "The DEFAULT printer is the first item in the list.";
		  alignment = left;	
		  }
              : list_box {
                key = "PRINTER";
		height = 7;
                width = 10;
              }	 
 }
    : column {
	        : button {
	          key = "MOVEUP";
	          label = "  Move Up ";
	          is_default = false;
	          is_cancel = false;
	        }
	        : button {
	          key = "MOVEDOWN";
	          label = "  Move Down ";
	          is_default = false;
	          is_cancel = false;
	        }
	        : button {
	          key = "DELETE";
	          label = "  Delete Item ";
	          is_default = false;
	          is_cancel = false;
	        } 
      }



}



: boxed_column {
    : text {
        value ="ADD A PRINTER TO SeptiCAD";
        alignment = centered;	
    }



              : popup_list {
                key = "PRINTER_FIELD";
                width = 25;
                value = "0";
                label = "Printer ->";
                action = "(sPrintAddResetPaperDCL)";
              }
	      : popup_list {
                key = "PAPER_FIELD";
                label = "Paper ->";
              }


	        : button {
	          key = "ADD";
	          label = "  Add Printer  ";
	          is_default = false;
	          is_cancel = false;
	        }

}


}// end big row


	: row {
	        : button {
	          key = "SAVE";
	          label = "  Save Changes  ";
	          is_default = true;
	          is_cancel = true;
	        }

	        : button {
	          key = "CANCEL";
	          label = "  Discard Changes  ";
	          is_default = false;
	          is_cancel = true;
	        }
	}

}


}