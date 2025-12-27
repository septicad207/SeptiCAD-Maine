PAGE1A : dialog { 
          label = "SeptiCAD --- HHE-200 -- Page 1  --  Part A"; 

: column {
	: row {  
	       : column {
		label = ""; 

	       : column {
		label = "Designer"; 
                  : popup_list {
                   key = "DESIGNER";
                   label = "Site Evaluator-> ";
                   width = 10;
                   value = "0";/// "0" IS DEFAULT USER      
                  }
                }
			: column {
			label = " Property Location "; 
		
				: edit_box {
		                	key = "I1";
		           		label = "Date On Site";
		        	        edit_width = 20;
		                	value = "";
		                	initial_focus = true;
		              	}
				: edit_box {
		                	key = "A1";
		           		label = "City, Town, Plantation";
		        	        edit_width = 20;
		                	value = "";
		               	}
		              	: edit_box {
		                	key = "A2";
		                	label = "Street or Road";
		                	edit_width = 20;
		                	value = "";
		              	}
			      	: edit_box {
		                	key = "A3";
		                	label = "Subdivision, Lot #";
		                	edit_width = 20;
		                	value = "";
		              	}
			}
			// START NEW COLUMN
			: column {
			label = " Owner/Applicant Information"; 
				: row {
					: edit_box {
		                		key = "A4a";
		           			label = "First: ";
		        	        	edit_width = 20;
		                		value = "";
		            	  	}
					: edit_box {
		                		key = "A4b";
		           			label = "Last, MI: ";
		        	        	edit_width = 20;
		                		value = "";
		            	  	}
				}
					: radio_row {
		       		       	 key = "A5-A6";
		              			: radio_button {
		                		key = "A5";
		                		label = "Owner";
		              			value = "1";
						}
		     	         		: radio_button {
		                		key = "A6";
		                		label = "Applicant";
		        	      		}
		                	}
		
		  	            	: edit_box {
		   	             	key = "A7";
		   	             	label = "Mailing Address: ";
		   	             	edit_width = 25;
		   	             	value = "";
		        	      	}
			      		: edit_box {
		        	        key = "A8";
		        	        label = "Mailing Address: ";
		               		edit_width = 25;
		                	value = "";
		              		}
		
			      		: edit_box {
		                	key = "A9";
		                	label = "Daytime Tel. #: ";
		                	edit_width = 25;
		                	value = "";
		              		}
			}// end column Applicant/Owner Information             	
		} // end column (left)







 // start column right
	: column {
	 : column { // start column 1
		label = " Type of Application ";
              : toggle {
                key = "B1";
                label = "First Time System";
                value = "0";  
              }
              : toggle {
                key = "B2";
                label = "Replacement System";
                value = "0";  
              }
              : edit_box {
              	key = "B3";
               	label = "       Type Replaced";
               	edit_width = 25;
               	value = "";
              } 
              : edit_box {
              	key = "B4";
               	label = "       Year Installed";
               	edit_width = 25;
               	value = "";
              } 

              : column {            
             		 : toggle {
            		    	key = "B5";
        	        	label = "Expanded System";
                                action = "(changeExpansionRadio)"; 
              		}
			: radio_row {
        	      		key = "B6-B7";
     	         		: radio_button {
     		        	key = "B6";
                		label = "< 25% Expansion";
              			}
     	         		: radio_button {
                		key = "B7";
                		label = ">= 25% Expansion";
				}
                	}
              }
              : toggle {
                key = "B8";
                label = "Experimental System";
                value = "0";  
              }
              : toggle {
                key = "B9";
                label = "Seasonal Conversion";
                value = "0";  
              }
          } // end column 1

	      : row {            
		        label = " Size of Property ";
	    	  	: edit_box {
         	     		key = "B10";
        	       		label = "Size of Property";
        	       		edit_width = 6;
        	       		value = "";
       	       		} 
			: radio_column {
        	      		key = "B11-B12";
     	         		: radio_button {
     		        	key = "B11";
                		label = "Square Feet";
              			}
     	         		: radio_button {
                		key = "B12";
                		label = "Acres";
        	      		value = "1";
				}
                	}
               }


               : radio_row {
		label = " Shoreline Zoning ";
              	key = "B13-B14";
              		: radio_button {
                	key = "B13";
                	label = "Shoreline YES";
			value = "0";
              		}
     	         	: radio_button {
                	key = "B14";
                	label = "Shoreline NO";
			value = "1";
			}
                }
	 }
	} // end column (big column both left and right)

} // end row (big row with all form info)


: row {
 : column { 
          : row {
              : button {
                key = "accept";
                label = " Okay ";
                is_default = true;
              }
              : button {
                key = "cancel";
                label = " Cancel ";
                is_default = false;
                is_cancel = true;
              }
            }
 }
}// end row





 
}// end column