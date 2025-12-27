sPrint : dialog {

  label = "  SeptiCAD --- HHE-200 --- Printing ";

: column {




: boxed_column {

              : popup_list {
                key = "PRINTER";
                label = "Printer/Plotter --> ";
                width = 10;
              }	 


      : column {
	: row {

		: toggle {
	          key = "PAGE1";
		  label = "Page 1";
		  value = "0";
		}
		: toggle {
	          key = "PAGE2";
		  label = "Page 2";
		  value = "0";
		}
		: toggle {
	          key = "PAGE3";
		  label = "Page 3";
		  value = "0";
		}
	}

      }
}// column


	: row {
	        : button {
	          key = "PRINT";
	          label = "  Print Selected Pages";
	          is_default = true;
	          is_cancel = false;
	        }
	        : button {
	          key = "PRINTALL";
	          label = "  Print All HHE-200 ";
	          is_default = false;
	          is_cancel = false;
	        }
	}


	: row {
	        : button {
	          key = "CANCEL";
	          label = "  Cancel  ";
	          is_default = false;
	          is_cancel = true;
	        }
	        : button {
	          key = "ADD";
	          label = "  Add\Modify Printers  ";
	          is_default = false;
	          is_cancel = false;
	        }
	}


}
}