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
              	    key = "C6";
        	    label = "Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C6\" \"C2\")";
                   }
		
            	   : toggle {
              	    key = "C7";
        	    label = "State and Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C7\" \"C2\")";
                   }
                  }
                 }


             	 : toggle {
              	  key = "C3";
        	  label = "Replacement System Variance";
                  value = "0";  
                 }

		: row{
		 spacer_1;	
                 : column {
             	   : toggle {
                    key = "C8";
        	    label = "Local Plumbing Inspector Approval";
                    value = "0";
		    action = "(changeDCLcheckbox \"C8\" \"C3\")";
                   } 

            	 : toggle {
              	  key = "C9";
        	  label = "State and Local Plumbing Inspector Approval";
                  value = "0";  
		  action = "(changeDCLcheckbox \"C9\" \"C3\")";
                 }
                }
                }


            	 : toggle {
              	  key = "C4";
        	  label = "Minimum Lot Size Variance";
                  value = "0";  
                 }
            	 : toggle {
              	  key = "C5";
        	  label = "Seasonal Conversion Permit";
                  value = "0";  
                 }
	} // end column " this application requires "


	: column { // Start column "Disposal system to serve"
	label = " Disposal System to Serve "; 
		: row {
            	 : toggle {
              	  key = "L1";
        	  label = "Single Family Dwelling";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L2";
               	  label = "No. of Bedrooms";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
		: row {
            	 : toggle {
              	  key = "L3";
        	  label = "Multiple Family Dwelling";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L4";
               	  label = "No. of Units";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
		: row {
            	 : toggle {
              	  key = "L5";
        	  label = "Other";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L6";
               	  label = "Specify (other)";
               	  edit_width = 20;
               	  value = "";
                 } 
		}
     
    
	} // end column " Disposal System to Serve "
  
  
      		// START TYPE OF WATER SUPPLY
		: boxed_column {
		label = "Type of Water Supply";



               : radio_row {
              	key = "WATERTYPE";

                      : radio_button {
                        key = "K1";
                        label = "Drilled Well";
                        }
                      : radio_button {
                        key = "K2";
                        label = "Dug Well";
                      }
                      : radio_button {
                        key = "K3";
                        label = "Private";
                      }
                      : radio_button {
                          key = "K4";
                         label = "Public";
                      }
                      : radio_button {
                            key = "K5";
                            label = "Other";
                      }
                     }

                          : edit_box {
                            key = "K6";
                            label = "If Other Specify type: ";
                            //edit_width = 10;
                          } 

                     

		}// end water suipply 
  
  
  
  
	}// END COLUMN LEFT



// START COLUMN RIGHT
:column {
: column { // START DISPOSAL FIELD COMPONENTS
		 label = " Disposal System Components ";
            
		 : toggle {
              	  key = "E1";
        	  label = "Complete Non-Engineered System";
                  value = "0";  
                 }
		 : toggle {
              	  key = "E2";
        	  label = "Primitive System (graywater & alt. toilet)";
                  value = "0";  
                 }
		: row {
            	 : toggle {
              	  key = "E3";
        	  label = "Alternative Toilet";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E12";
               	  label = "Specify: ";
               	  edit_width = 20;
               	  value = "";
                 } 
		}
		 : toggle {
              	  key = "E4";
        	  label = "Non-engineered Treatment Tank (only)";
                  value = "0";  
                 }
                 
   		: row {
             spacer_1;
             spacer_1;
            	 : toggle {
              	  key = "E13";
                  label = "Check if adding multiple tanks | ";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E14";
               	  label = "Specify Gallons: ";
               	  edit_width = 6;
               	  value = "";
                 } 
		}              
                 
                 
                 
           	 : toggle {
              	  key = "E5";
        	  label = "Holding Tank";
                  value = "0";  
                 }

		 : toggle {
              	  key = "E6";
        	  label = "Non-engineered Disposal Field (only)";
                  value = "0";  
                 }



		 : toggle {
              	  key = "E7";
        	  label = "Engineered Treatment Tank (only)";
                  value = "0";  
                 }
		 : toggle {
              	  key = "E8";
        	  label = "Engineered Disposal Field (only)";
                  value = "0";  
                 }

		:row {

                : toggle {
              	  key = "E9";
                  label = "Miscellaneous Components";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E15";
               	  label = "Specify: ";
               	  edit_width = 20;
               	  value = "";
                 } 
	        } //end row

            	 : toggle {
              	  key = "E10";
        	  label = "Pretreatment (tank)";
                  value = "0";  
                 }
                 
            	 : toggle {
              	  key = "E11";
        	  label = "Pretreatment (miscellaneous component)";
                  value = "0";  
                 }
        


	} // END Column Right

	: column { // Start column "this application requires"
      label = " Pre- / Advanced Treatment Systems "; 
  
            : boxed_column {
              :row {
                 : edit_box {
              	  key = "Q1";
               	  label = "Make: ";
               	  //edit_width = 6;
               	  value = "";
                 } 
                 : edit_box {
              	  key = "Q2";
               	  label = "Model: ";
               	 // edit_width = 6;
               	  value = "";
                 }                  
                 
              } //end row

                 : edit_box {
              	  key = "Q3";
               	  label = "Notes";
               	 // edit_width = 20;
               	  value = "";
                 } 
            }
                 
            : boxed_column {
                :row {
                 : edit_box {
              	  key = "Q4";
               	  label = "Make: ";
               	//  edit_width = 6;
               	  value = "";
                 } 
                 : edit_box {
              	  key = "Q5";
               	  label = "Model: ";
               	//  edit_width = 6;
               	  value = "";
                 }                  
                 
              } //end row

                 : edit_box {
              	  key = "Q6";
               	  label = "Notes";
               	 // edit_width = 20;
               	  value = "";
                 } 
            }
          }


} // END ROW WITH FORM INFO IN IT
        }

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