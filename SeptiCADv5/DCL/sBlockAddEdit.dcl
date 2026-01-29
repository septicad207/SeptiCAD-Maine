sBlockAddEdit : dialog {

  label = "  SeptiCAD - Customize Block Library Item ";


// Block Identifier - Appears in Form    ---do not delete
// Block Text Default - Appears in Form  ---do not delete
// Block file location                   ---do not delete


//Rotate block on insert [Y or N]       ---do not delete
//Explode block on insert [Y or N]      ---do not delete
//Scale block on insert [Y or N]        ---do not delete




: boxed_column {

  : text {
  key = "TITLE";
  value ="";
  alignment = centered;	
  height = 2;
  }


// start big row
: row {
         : column {
            : boxed_column {
	    label = " Block Name and Design Label";
		: edit_box {
		  key = "ID";
		  label = "Block Name for List ->";
		  edit_width = 40;
		  value = "";
		}
		: edit_box {
		  key = "TEXT";
		  label = "Design Label ->";
		  edit_width = 40;
		  value = "";
		}
            }
            : boxed_column {
	      label = " Block File Name";
		: edit_box {
		  key = "FILE";
		  label = "";
		  edit_width = 60;
		  value = "";
		}
	        : button {
	          key = "FILEUPDATE";
	          label = "  Browse for File  ";
	          is_default = false;
	          is_cancel = false;
	        }
            }
          }


	: boxed_column {
	    label = " Do on Insert";
		: toggle {
	          key = "ROTATE";
		  label = "Rotate ->";
		  value = "0";
		}
		: toggle {
	          key = "EXPLODE";
		  label = "Explode ->";
		  value = "0";
		}
		: toggle {
	          key = "SCALE";
		  label = "Scale ->";
		  value = "0";
		}
	}// row
  }// big row

}// column


	: row {
	        : button {
	          key = "SAVE";
	          label = "  Okay  ";
	          is_default = true;
	          is_cancel = false;
	        }
	        : button {
	          key = "CANCEL";
	          label = "  Cancel  ";
	          is_default = false;
	          is_cancel = true;
	        }

	}

}