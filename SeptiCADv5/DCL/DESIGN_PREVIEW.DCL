DESIGN_PREVIEW : dialog {
label = "  SeptiCAD --- Design Preview ";
: column {
 : row {

  /// START LEFT PANE
  : column {
    : boxed_column {
      label = " System Dimensions and Layout";
      : text {
       key = "systemInfoMain-1";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-2";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-3";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-4";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-5";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-6";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-7";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-8";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-9";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-10";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoMain-11";
       height = "1";
       width = "42";
      }
     }
    : boxed_column {
      label = " Final Grade and Elevation Calculation Method ";
      : text {
       key = "systemInfoGrades-1";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoGrades-2";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoGrades-3";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoGrades-4";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoGrades-5";
       height = "1";
       width = "42";
      }
     }
    : boxed_column {
      label = " Fill Extension Shoulders and Grades ";
      : text {
       key = "systemInfoShoulders-1";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoShoulders-2";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoShoulders-3";
       height = "1";
       width = "42";
      }
      : text {
       key = "systemInfoShoulders-4";
       height = "1";
       width = "42";
      }


     }
   }
/// END LEFT PANE



/// START RIGHT PANE
   : column {
    width = "20";
    : boxed_column {
      label = "Map and Cross Section Scales";
	: text {
	height = "1.5";
	label = "Example: if 1\":100' scale enter 100"; 
	}
        : row {
	 : edit_box {
         key = "page2_scale";
         label = "Scale of Page 2 Map    ->  1 inch = ";
         edit_width = 4;
	 action = "(checkMapScale)";
         value = "100";
         }
         : text {
         label = " Feet";
         }
        }

        : row {
	 : edit_box {
         key = "page3_scale";
         label = "Scale of Page 3 Map    ->  1 inch = ";
         edit_width = 4;
         value = "20";
 	 action = "(checkMapScale)";
         }
         : text {
         label = " Feet";
         }
        }
        : row {
	 : edit_box {
         key = "profile_scale";
         label = "Scale of Cross Section ->  1 inch = ";
         edit_width = 4;
         value = "5";
	 action = "(checkMapScale)";
         }
         : text {
         label = " Feet";
         }
        }
    }

    // START CHAMBER ELEVATION EDIT
    : boxed_column {
      label = "Revise Chamber Elevations";
	: list_box {
        key = "SHELFLIST";
        height = "8";
        action = "(updateShelftoEdit)";
        }	

        : boxed_column {
          label = "Change Row/Shelf Elevation";
           : text {
           key = "SHELFTOEDIT";
           width = "2";
           }	
           : row {
             : edit_box {
             key = "NEWELEVATION";
             value = "";
             }	
             : button {
             key = "edit";
             label = "Edit Elevation";
             is_default = false;
             is_cancel = false;
             }
           }
        }
     }
    // END CHAMBER ELEVATION EDIT
  }
  //end RIGHT PANE
 }
 // end ROW

      : row {
        : button {
          key = "okay-button";
          label = "Continue";
          is_default = true;
        }
        : button {
          key = "cancel-button";
          label = "Go Back";
          is_default = false;
          is_cancel = true;
        }
      }
   }
}