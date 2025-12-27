PAGE1B : dialog { 
          label = "SeptiCAD --- HHE-200 -- Page 1  --  Part B"; 

: column {


  : row { // START ROW WITH FORM INFO IN IT 

// START COLUMN LEFT
	: column {
	: column { // Start column "this application requires"
	label = " This Application Requires ";             	 
		: toggle {
              	  key = "C1";
        	  label = "No Rule Variance";
                  value = "0";  
                 }
             	 : toggle {
              	  key = "C2";
        	  label = "First Time System Variance";
                  value = "0";  
                 }

		: row{
		 spacer_1;		 

                 : column {
		  : toggle {
              	    key = "C3";
        	    label = "Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C3\" \"C2\")";
                   }
		
            	   : toggle {
              	    key = "C4";
        	    label = "State and Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C4\" \"C2\")";
                   }
                  }
                 }


             	 : toggle {
              	  key = "C5";
        	  label = "Replacement System Variance";
                  value = "0";  
                 }

		: row{
		 spacer_1;	
                 : column {
             	   : toggle {
                    key = "C6";
        	    label = "Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C6\" \"C5\")";
                   } 

            	 : toggle {
              	  key = "C7";
        	  label = "State and Local Plumbing Inspector Approval";
                  value = "0";  
		  action = "(changeDCLcheckbox \"C7\" \"C5\")";
                 }
                }
                }


            	 : toggle {
              	  key = "C8";
        	  label = "Minimum Lot Size Variance";
                  value = "0";  
                 }
            	 : toggle {
              	  key = "C9";
        	  label = "Seasonal Conversion Permit";
                  value = "0";  
                 }
	} // end column " this application requires "


	: column { // Start column "Disposal system to serve"
	label = " Disposal System to Serve "; 
		: row {
            	 : toggle {
              	  key = "C10";
        	  label = "Single Family Dwelling";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "C11";
               	  label = "No. of Bedrooms";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
		: row {
            	 : toggle {
              	  key = "C12";
        	  label = "Multiple Family Dwelling";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "C13";
               	  label = "No. of Units";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
		: row {
            	 : toggle {
              	  key = "C14";
        	  label = "Other";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "C15";
               	  label = "Specify (other)";
               	  edit_width = 20;
               	  value = "";
                 } 
		}
               : radio_row {
              	key = "CURRENTUSE";
		label = " Current Use "; 
              	
			: radio_button {
                	key = "C16";
                	label = "Seasonal";
              		}
     	         	: radio_button {
                	key = "C17";
                	label = "Year Round";
			}
     	         	: radio_button {
                	key = "C18";
                	label = "Undeveloped";
			}
                }
	} // end column " Disposal System to Serve "
	}// END COLUMN LEFT



// START COLUMN RIGHT
: column { // START DISPOSAL FIELD COMPONENTS
		 label = " Disposal System Components ";
            
		 : toggle {
              	  key = "D1";
        	  label = "Complete Non-Engineered System";
                  value = "0";  
                 }
		 : toggle {
              	  key = "D2";
        	  label = "Primitive System (graywater & alt. toilet)";
                  value = "0";  
                 }
		: row {
            	 : toggle {
              	  key = "D3";
        	  label = "Alternative Toilet";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "D4";
               	  label = "";
               	  edit_width = 20;
               	  value = "Specify Type Here";
                 } 
		}
		 : toggle {
              	  key = "D5";
        	  label = "Non-engineered Treatment Tank (only)";
                  value = "0";  
                 }
		: row {
            	 : toggle {
              	  key = "D6";
        	  label = "Holding Tank";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "D7";
               	  label = "";
               	  edit_width = 20;
               	  value = "Specify Gallons Here";
                 } 
		}
		 : toggle {
              	  key = "D8";
        	  label = "Non-engineered Disposal Field (only)";
                  value = "0";  
                 }
		 : toggle {
              	  key = "D9";
        	  label = "Separated Laundry System";
                  value = "0";  
                 }
		 : toggle {
              	  key = "D10";
        	  label = "Complete Engineered System (>= 2000gpd)";
                  value = "0";  
                 }
		 : toggle {
              	  key = "D11";
        	  label = "Engineered Treatment Tank (only)";
                  value = "0";  
                 }
		 : toggle {
              	  key = "D12";
        	  label = "Engineered Disposal Field (only)";
                  value = "0";  
                 }


		: row {
            	 : toggle {
              	  key = "D13";
        	  label = "Pretreatment";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "D14";
               	  label = "";
               	  edit_width = 20;
               	  value = "Specify Type Here";
                 } 
		}
		:row {

		 : toggle {
              	  key = "D15";
        	  label = "Miscellaneous Components";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "D15a";
               	  label = "";
               	  edit_width = 20;
               	  value = "Specify Type Here";
                 } 
	        } //end row

		// START TYPE OF WATER SUPPLY
		: row {
		label = "Type of Water Supply";
			  	 : popup_list {
		                 key = "WATER";
		                 label = "";
		                 width = 18;
		              	 }
		                 : edit_box {
		              	  key = "WATERNOTE";
		               	  label = "Note->";
		               	  edit_width = 20;
		                 } 
		}// end row


	} // END Column Right




} // END ROW WITH FORM INFO IN IT


// START ROW BUTTONS
: row {
 : column { 
          : row {
              : button {
                key = "accept";
                label = " Okay ";
                is_default = true;
              }
              : button {
                key = "back";
                label = " Go Back ";
                is_default = false;
                is_cancel = false;
              }
              : button {
                key = "cancel";
                label = " Cancel ";
                is_default = false;
                is_cancel = true;
              }
            }
 }
} // end row BUTTONS

} // end column
} // end column