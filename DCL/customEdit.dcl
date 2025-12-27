customEdit : dialog {

  label = "  SeptiCAD --- Customize SeptiCAD Libary  ";
  : column {


	: boxed_column {
		: text {
		key = "DESCRIPTION";
		value = "";
		is_bold = true;
		alignment = centered;
		height = 2;
		}

	        : list_box {
	          key = "textList";
	          label = "";
	          multiple_select = false;
	          width = 65.0;
		  height = 20.0;
	        }
       
	}/// end boxed column

	: row {
        : button {
	  key = "delete";
	  label = "  Delete Note  ";
	  is_default = false;
	}
        : button {
	  key = "moveup";
	  label = "  Move Up  ";
	  is_default = false;
	}
        : button {
	  key = "movedown";
	  label = "  Move Down  ";
	  is_default = false;
	}


	}// end row


	: column {
	  label = "Add Note";
		: row {
			: edit_box {
	                key = "textField";
	                label = "Note to Add --> ";
	                edit_width = 60;
	                value = "";
	              	}
		        : button {
		          key = "add";
		          label = "  Add Note  ";
		          is_default = false;
		        }
		}// end row
			: text {
			    value = "For Newline type \\P [example ->  Septic\\PTank]";
			    alignment = centered;
		        }
	}// end row

	: column {
	  label = "Edit Note";
	     : row {
			: edit_box {
	                key = "editField";
	                label = "Note to Edit --> ";
			is_enabled = false;
	                edit_width = 60;
	                value = "";
	              	}
		        : button {
		          key = "edit";
		          label = "  Edit Note  ";
		          is_default = false;
		        }
	     }

	     : column {
		: row {
			: edit_box {
	                key = "newEditField";
	                label = "Edited Note --> ";
			is_enabled = false;
	                edit_width = 60;
	                value = "";
	              	}
		        : button {
		          key = "editMake";
		          label = "  Make Changes  ";
		          is_default = false;
		        }
		}// row
			: text {
			    value = "For Newline type \\P [example ->  Septic\\PTank]";
			    alignment = centered;
		        }
	     }
	}// end row




      : row {
        : button {
          key = "save";
          label = "  Save Changes  ";
          is_default = true;
          is_cancel = false;
        }
        : button {
          key = "cancel";
          label = "  Discard Changes  ";
          is_default = false;
          is_cancel = true;
        }
        : button {
          key = "notepad";
          label = "  Open in Notepad  ";
          is_default = false;
          is_cancel = false;
        }
      }
   
}
}