swingtie : dialog {
  label = "SeptiCAD --- Swing Tie";

: column {
   : boxed_column { // end boxed_column 1

	 : text {
          key = "DESCRIPTION1";
          alignment = centered;
	  value ="";
	 }
	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 1;
	  width = 1;
	}
	 : text {
          key = "DESCRIPTION2";
          alignment = centered;
	  value ="";
	 }
	 : text {
          key = "DESCRIPTION3";
          alignment = centered;
	  value ="";
	 }
	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 1;
	  width = 1;
	}
         : edit_box {
         key = "TXT1";
         label = "Label Line 1";
         edit_width = 30;
         value = "";
         }
         : edit_box {
         key = "TXT2";
         label = "Label Line 2";
         edit_width = 30;
         value = "";
         }
	 : popup_list {
         key = "RP_TYPE";
         label = "Reference Point Object Type->";
         width = 10;
         value = "0";
         }
   } // end boxed_column 1

   : boxed_row {  // start row with distances (3 columns total)
     label = "Swing Tie Distances and Side";
      : column {
	: row {
		: edit_box {
	        key = "TL";
	        edit_width = 3.0;
	        }
		: spacer {
				
			fixed_height = true;
			fixed_width = true;
			height = 1;
			width = 2;
		}
	}// end row

	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 4;
	  width = 1;
	}
              : toggle {
                key = "LEFT";
        alignment = centered;	
                action ="(swingtieToggleDCL \"LEFT\")";
              }
	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 4;
	  width = 1;
	}
	: row {
		: edit_box {
	        key = "BL";
	        edit_width = 3.0;
	        }
		: spacer {
				
			fixed_height = true;
			fixed_width = true;
			height = 1;
			width = 2;
		}
	}// end row

      } // end column









	/// insert SLIDE
      : column {
			: spacer {
			
			          fixed_height = true;
			          fixed_width = true;
			          height = 1;
			          width = 1;
			}
              : toggle {
               alignment = centered;	
                key = "UP";
                action = "(swingtieToggleDCL \"UP\")";
              }
	////// IMAGE INSERT

				 : image { 
                                   alignment = centered;	
				   key = "field"; 
				   fixed_height = true;
				   fixed_width = true;
				   height = 10; 
				   width =  50;  
				   color = 5;
				 } 
	////// IMAGE INSERT

              : toggle {
               alignment = centered;	
                key = "DOWN";
                action ="(swingtieToggleDCL \"DOWN\")";
              }

      } // end column









      : column {
	: row {
		: edit_box {
	        key = "TR";
	        edit_width = 3.0;
	        }
		: spacer {
				
			fixed_height = true;
			fixed_width = true;
			height = 1;
			width = 2;
		}
	}// end row

	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 4;
	  width = 1;
	}
              : toggle {
                key = "RIGHT";
        alignment = centered;	
                action ="(swingtieToggleDCL \"RIGHT\")";
              }

	: spacer {
	  fixed_height = true;
	  fixed_width = true;
	  height = 4;
	  width = 1;
	}

	: row {
		: edit_box {
	        key = "BR";
	        edit_width = 3.0;
	        }
		: spacer {
				
			fixed_height = true;
			fixed_width = true;
			height = 1;
			width = 2;
		}
	}// end row

      } // end column
     } // end column 2








      : row { // start button row
        : button {
          key = "okay";
          label = "  Okay  ";
          is_default = true;
        }
        : button {
          key = "cancel";
          label = "  Cancel/Skip  ";
          is_default = false;
          is_cancel = true;
        }
      } // end button row


   
}// end boxed column
}// end dialog
