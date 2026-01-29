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
              	  key = "L1A";
               	  label = "No. of Bedrooms";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
		: row {
            	 : toggle {
              	  key = "L2";
        	  label = "Multiple Family Dwelling";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L2A";
               	  label = "No. of Units";
               	  edit_width = 5;
               	  value = "";
                 } 
		}
 		: row {
            	 : toggle {
              	  key = "L3";
        	  label = "Accessory Dwelling Units";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L3A";
               	  label = "No. of Units";
               	  edit_width = 5;
               	  value = "";
                 } 
		}   
    
    
    
    
		: row {
            	 : toggle {
              	  key = "L4";
        	  label = "Other";
                  value = "0";  
                 }
                 spacer_1;
                 spacer_1;
                 : edit_box {
              	  key = "L4A";
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
            

   		: row {
            	 : toggle {
              	  key = "E1";
        	  label = "Complete Non-Engineered System";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E1A";
               	  label = "Specify No. of New Tanks: ";
               	  edit_width = 5;
               	  value = "";
                 } 
		  }              
                 
    		: row {
            	 : toggle {
              	  key = "E2";
        	  label = "Primitive/Limited System (Greywater & Alt. Toilet";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E2A";
               	  label = "Specify Type: ";
               	  edit_width = 20;
               	  value = "";
                 } 
		  }                     
                 

		: row {
            	 : toggle {
              	  key = "E3";
        	  label = "Alternative Toilet";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E3A";
               	  label = "Specify Type: ";
               	  edit_width = 20;
               	  value = "";
                 } 
		}


		: row {
            	 : toggle {
              	  key = "E4";
        	  label = "Non-engineered Treatment Tank (only)";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E4A";
               	  label = "Total # of Tank(s): ";
               	  edit_width = 20;
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




              : boxed_column {
            : toggle {
              	  key = "E7";
                  label = "Complete Engineered System";
                  value = "0";  
                 }

              : row {

                 : edit_box {
              	  key = "E7A";
               	  label = "NEW: # of Disposal Fields: ";
               	  edit_width = 5;
               	  value = "";
                 } 
                 
                   : edit_box {
              	  key = "E7B";
               	  label = "# of Tank(s): ";
               	  edit_width = 5;
               	  value = "";
                 } 

                 : edit_box {
              	  key = "E7C";
               	  label = "# of Pumps(s): ";
               	  edit_width = 5;
               	  value = "";
                 } 
              }
                             }


















    		: row {
            	 : toggle {
              	  key = "E8";
        	  label = "Engineered Treatment Tank (only)";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E8A";
               	  label = "Total # of New Tank(s): ";
               	  edit_width = 5;
               	  value = "";
                 } 
		  }   





                 
                 
                 
		 : toggle {
              	  key = "E9";
        	  label = "Engineered Disposal Field (only)";
                  value = "0";  
                 }

		:row {

                : toggle {
              	  key = "E10";
                  label = "Miscellaneous Components";
                  value = "0";  
                 }
                 : edit_box {
              	  key = "E10A";
               	  label = "Specify: ";
               	  edit_width = 20;
               	  value = "";
                 } 
	        } //end row

            	 : toggle {
              	  key = "E11";
        	  label = "Pretreatment (tank)";
                  value = "0";  
                 }
                 
            	 : toggle {
              	  key = "E12";
        	  label = "Pretreatment (miscellaneous component)";
                  value = "0";  
                 }
        


	} // END Column Right
  
  
  
  
  
  
  
  

	: column { // Start column "this application requires"
      label = " Pre- / Advanced Treatment Systems "; 
  
            : boxed_column {
              :row {
                 : edit_box {
              	  key = "Q1A";
               	  label = "Make: ";
               	  //edit_width = 6;
               	  value = "";
                 } 
                 : edit_box {
              	  key = "Q1B";
               	  label = "Model: ";
               	 // edit_width = 6;
               	  value = "";
                 }                  
                 
              } //end row

                 : edit_box {
              	  key = "Q1C";
               	  label = "Notes";
               	 // edit_width = 20;
               	  value = "";
                 } 
                 
             	 : toggle {
              	  key = "Q1D";
                  label = "Maintenance Contract (HHE-300A) Required";
                  value = "0";  
                 }                
                 
            }
                 

            : boxed_column {
              :row {
                 : edit_box {
              	  key = "Q2A";
               	  label = "Make: ";
               	  //edit_width = 6;
               	  value = "";
                 } 
                 : edit_box {
              	  key = "Q2B";
               	  label = "Model: ";
               	 // edit_width = 6;
               	  value = "";
                 }                  
                 
              } //end row

                 : edit_box {
              	  key = "Q2C";
               	  label = "Notes";
               	 // edit_width = 20;
               	  value = "";
                 } 
                 
             	 : toggle {
              	  key = "Q2D";
                  label = "Maintenance Contract (HHE-300A) Required";
                  value = "0";  
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