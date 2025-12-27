sBlockCustom : dialog {

  label = "  SeptiCAD -- Customize Block Library ";
  : column {
      : text {
	value = "Customize the Block Library";
	height = 2;
	alignment = centered;
	is_bold = true;
      }
      : boxed_column {
        : list_box {
          key = "list_tile";
          multiple_select = false;
          width = 40.0;
	  height = 20.0;
        }
       }


	:  row {
	        : button {
	          key = "ADD";
	          label = "  Add ";
	          is_default = false;
	          is_cancel = false;
	        }
	        : button {
	          key = "EDIT";
	          label = "  Edit ";
	          is_default = false;
	          is_cancel = false;
	        }
	        : button {
	          key = "DELETE";
	          label = "  Delete ";
	          is_default = false;
	          is_cancel = false;
	        }
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
	}


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