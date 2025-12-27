SEPTIC : dialog { 
          label = "SeptiCAD --- Main Design Window"; 
: column {

//////// START row for CENTER PANE (LEFT/RIGHT)
 : row {

  //////////START LEFT PANE
      : column {

/// START COLUMN XX

	: boxed_column {
          label = "System Type/Layout/Orientation";
              : popup_list {
                key = "SYSTEM";
                label = "";
                width = 10;
                value = "2";
		action = "(ifSystemAutoFillDCL)";
              }	      
	     : row {
              : text {
                key = "NUM_ROWS_TEXT";
                alignment = left;
                value = "Number of Rows-> ";
                width = 30;
              }
	      : edit_box {
                key = "NUM_ROWS";
                label = "";
                edit_width = 4;
		action = ""; 
                value = "";
              }
            }

	    :row {
              : text {
                key = "NUM_UNITS_TEXT";
                alignment = left;
                value = "Total Length of System [ft]-> ";
                width = 30;
              }
	      : edit_box {
                key = "NUM_UNITS";
                label = "";
                edit_width = 4;
                value = "";
		action = "";
              }
             }

	     : row {
              : text {
                key = "ROW_SEP_TEXT";
                alignment = left;
                value = "";
                width = 30;
              }
              : edit_box {
                key = "ROW_SEP";
                label = "";
                edit_width = 4;
		is_enabled = true;
                value = "";
		action = "";
              }
             }

	     : row {
              : text {
                key = "STONE_BELOW_TEXT";
                alignment = left;
                value = "Stone Below Chambers [in]-> ";
                width = 30;
              }
              : edit_box {
                key = "STONE_BELOW";
                label = "";
                edit_width = 4;
		is_enabled = false;
                value = "";
              }
             }

	     : row {
              : text {
                key = "STONE_BESIDE_TEXT";
                alignment = left;
                value = "Stone Beside Chambers [in]-> ";
                width = 30;
              }
              : edit_box {
                key = "STONE_BESIDE";
                label = "";
                edit_width = 4;
		is_enabled = false;
                value = "";
              }
             }


	     : row {
              : text {
                key = "FILL_ABOVE_TEXT";
                alignment = left;
                value = "Fill Above System [in]-> ";
                width = 30;
              }
              : edit_box {
                key = "FILL_ABOVE";
                label = "";
                edit_width = 4;
		is_enabled = true;
                value = "8";
              }
             }

	     : row {
              : text {
                key = "BEARING_TEXT";
                alignment = left;
                value = "Compass Bearing [see below]-> ";
                width = 30;
              }
              : edit_box {
                key = "BEARING";
                label = "";
                edit_width = 4;
		is_enabled = true;
                value = "0";
              }
             }


              : toggle {
                key = "ROTATESYSTEM";
                label = "Rotate Plan View Drawings";
                value = "0";
              }

            }
/// END COLUMN XX




/// START COLUMN AA
      : column {
        label = "Chambers on Shelfs";

        : row {
              : popup_list {
                key = "SHELF_TOGGLE";
                label = "";
                width = 15;
                value = "0";
		action = "(ifShelfDCL)";
              }	
             : button {
               key = "SHELF_HELP";
               label = "Shelf Help";
               is_default = false;
               is_cancel = false;
             }
        }

              : edit_box {
                key = "SHELF_SEP";
                label = "Distance btw Shelfs [in]-> ";
                edit_width = 4;
		is_enabled = false;
                value = "";
		action = "";
              }

         	: edit_box {
        	 key = "SHELF_SEQUENCE";
        	 label = "Shelf Sequence ->";
        	 edit_width = 20;
		 is_enabled = false;
         	 value = "";
        	 }

      }
/// END COLUMN AA





/// START COLUMN XXX
  : column {
    label = "Corner Elevations";
      : row {
              : column {
		width = 5;
			: edit_box {
                	  key = "U1";
                	  label = "";
                	  edit_width = 4;
                	  value = "-50";
             		}
			: edit_box {
                	  key = "D1";
                	  label = "";
                	  edit_width = 4;
                	  value = "-65";
              		}
		}
	     	
	////// IMAGE INSERT
		: row {
 		 alignment = centered;
			: spacer {
			          fixed_width = true;
			          width = "0.5";
			}
				 : image { 
				   key = "field"; 
				   fixed_height = true;
				   fixed_width = true;
				   height = 5; 
	          		   width = 25;
				  // color = 5;
				 } 
		}// row
	////// IMAGE INSERT

		:column {
		   width = 5;
			: edit_box {
                	  key = "U2";
                	  label = "";
                	  edit_width = 4;
                	  value = "-50";
              		}
			: edit_box {
                	  key = "D2";
                	  label = "";
                	  edit_width = 4;
                	  value = "-70";
              		}
		}
	}// end row

        : row {
              : toggle {
                key = "COMPOUND";
                label = "Input Compound Grades";
                value = "0";
              }
              : button {
                key = "elv_help";
                label = "HELP";
                is_default = false;
                is_cancel = false;
              }
         }
  }
/// END COLUMN XXX

         : row {
            label = " Cross Section Side ";
             : radio_row {
               key = "PROFILE";
                 : radio_button {
                   key = "AUTO";
                   label = "Auto";
                   value = "1";
                 }
                 : radio_button {
                   key = "LEFT";
                   label = "Left";
                   value = "0";
                 }
                 : radio_button {
                   key = "RIGHT";
                   label = "Right";
                   value = "0";
                 }
              }// end radio_row
          }// end row




 }  //////////END LEFT PANE
///////////////////////////





///////////////////////////////
//////////START RIGHT PANE
	: boxed_column {
          : column {
				 : image { 
				   key = "titlebar"; 
//				   fixed_height = true;
//				   fixed_width = true;
				   height = 5; 
	          		   width = 60;
				   color = 5;
				 } 
          }

////// END START with left-> shoulders AND right-> final grade type
         : row {
	    : boxed_column {
	      label = " Fill Extensions - Grade and Width [inches] ";

		     : row {
            		: popup_list {
               		  key = "UP_SLOPE";
              		  label = "Up Slope      ->";
             		  width = 1;
            		  value = "1";
                          action = "(ifUP_SLOPE-DCL)";
            		  }
			: edit_box {
                	  key = "UP_WIDTH";
                	  label = "";
                	  edit_width = 3;
                	  value = "36";
			  is_enabled = true;
              		}
                     }

		     : row {
              		: popup_list {
               		  key = "LEFT_SLOPE";
              		  label = "Left             ->";
             		  width = 1;
            		  value = "1";
            		  }
			: edit_box {
                	  key = "LEFT_WIDTH";
                	  label = "";
                	  edit_width = 3;
                	  value = "36";
			  is_enabled = true;
              		}
		     }

		     : row {
            		: popup_list {
               		  key = "RIGHT_SLOPE";
              		  label = "Right             ->";
             		  width = 1;
            		  value = "1";
            		  }
			: edit_box {
                	  key = "RIGHT_WIDTH";
                	  label = "";
                	  edit_width = 3;
                	  value = "36";
			  is_enabled = true;
              		}
		     }

                      : row {
              		: popup_list {
               		  key = "DOWN_SLOPE";
              		  label = "Down Slope ->";
             		  width = 1;
            		  value = "1";
			  is_enabled = true;
            		  }
			: edit_box {
                	  key = "DOWN_WIDTH";
                	  label = "";
                	  edit_width = 3;
                	  value = "36";
			  is_enabled = true;
              		}
                      }


  		   }// end boxed_column



	        : boxed_column {
	         label = " Final Grade Type ";
			: radio_column {
		       	 key = "DESIGN";
                         action = "(ifSlopeing3-DCL)";
			  : radio_button {
		          key = "AUTODESIGN";
		          label = "Auto";
		          value = "1";
	  		  }
			  : radio_button {
		          key = "CROWN";
		          label = "3% Crown";
		          value = "0";
	  		  }
			  : radio_button {
		          key = "SLOPING";
		          label = "Sloping";
		          value = "0";
	  		  }
			  : radio_button {
		          key = "SLOPING3";
		          label = "Sloping 3%";
		          value = "0";
	  		  }

	               	}
		}
            }
////// END row with left-> shoulders AND right-> final grade type


//////START Calculation Methods Column
	 	: boxed_column {
		  label = " System Elevation Calculation Methods "; 

		    : row {
			: popup_list {
			  key = "CALC_METHOD";
			  action = "(ifForceAutoDCL)";
			}
                        : boxed_row { 
		        label = " Data Entry Convention Diagrams "; 
			   : button {
			     key = "autostep_help";
			     label = "Auto-Step Help";
			     is_default = false;
			     is_cancel = false;
		   	   }
			   : button {
			     key = "forcestep_help";
			     label = "Force-Step Help";
			     is_default = false;
			     is_cancel = false;
			   }
                        }
		   }
                
	

		        : column { // start column A

				: boxed_column {
				label = "Auto-Step";

					: edit_box {
			                  key = "LIM_SEP";
			                  label = "Vertical Separation from Limiting Factor (e.g. 12\", 18\" 24\")->";
			                  edit_width = 5;
			                  value = "12";
					}

					: edit_box {
		                	  key = "DEPTH_TOP";
		                	  label = "Up-Slope Edge - Elevation of Limiting Factor (e.g. -80\") ->";
		                	  edit_width = 5;
		                	  value = "-65";
		              		}
					: edit_box {
		                	  key = "DEPTH_BOT";
		                	  label = "Down-Slope Edge - Elevation of Limiting Factor (e.g. -90\") ->";
		                	  edit_width = 5;
		                	  value = "-75";
		              		}
 
				}// end boxed_column

				: boxed_column {
				label = "Force-Step";


	     : row {
              : text {
                key = "DEPTH_STEP_TEXT";
                alignment = left;
                value = "Bottom of Row #1 or Stone Elevation (e.g. -55\")->";
                width = 50;
              }
              : edit_box {
                key = "DEPTH_STEP";
                label = "";
                edit_width = 5;
                value = "";
              }
             }
	     : row {
              : text {
                key = "STEP_SIZE_TEXT";
                alignment = left;
                value = "Elevation Drop between Rows (e.g. 2\")->";
                width = 50;
              }
              : edit_box {
                key = "STEP_SIZE";
                label = "";
                edit_width = 5;
                value = "";
              }
             }

				} 
	              } // end column A

	          }	
//////END Calculation Methods Column

  } // end boxed_column
///////////////////////////////
//////////END RIGHT PANE


 }
//////// END row for CENTER PANE (LEFT/RIGHT)






















///////////////////START ROW OF BUTTONS///////////////////////
    : row {
              : button {
                key = "designonly";
                label = "Generate Design";
                is_default = true;
                is_cancel = false;
              }
              : button {
                key = "reset";
                label = "Reset Form";
                is_default = false;
                is_cancel = false;
              }
              : button {
                key = "cancel";
                label = "Cancel";
                is_default = false;
                is_cancel = true;
              }
              : button {
                key = "help";
                label = "HELP";
                is_default = false;
                is_cancel = false;
              }
    }// end button row button

  }// end column 
}// end dialog 
