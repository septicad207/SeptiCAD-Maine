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
		  : row {
                : edit_box {
		                	key = "A1";
		           		label = "Address # ";
		        	        edit_width = 5;
		                	value = "";
		                	initial_focus = true;
		              	}
		              	: edit_box {
		                	key = "A2";
		                	label = "Street or Road";
		                	edit_width = 20;
		                	value = "";
		              	}
          }            
				: edit_box {
		                	key = "A3";
		           		label = "City, Town, Plantation";
		        	        edit_width = 20;
		                	value = "";
		               	}

				: row {
					: edit_box {
		                		key = "A4";
		           			label = "Municipal Tax Map: ";
		        	        	edit_width = 5;
		                		value = "";
		            	  	}
					: edit_box {
		                		key = "A5";
		           			label = " and Lot: ";
		        	        	edit_width = 5;
		                		value = "";
		            	  	}
				}



			}
			// START NEW COLUMN
			: column {
			label = " Owner/Applicant Information"; 

					: edit_box {
		                		key = "A6";
		           			label = "Owner Name (last, first): ";
		        	        	edit_width = 20;
		                		value = "";
		            	  	}
					: edit_box {
		                		key = "A7";
		           			label = "Applicant Name (last, first): ";
		        	        	edit_width = 20;
		                		value = "";
		            	  	}

		
		  	            	: edit_box {
		   	             	key = "A8";
		   	             	label = "Street: ";
		   	             	edit_width = 25;
		   	             	value = "";
		        	      	}
			      		: edit_box {
		        	        key = "A9";
		        	        label = "City: ";
		               		edit_width = 25;
		                	value = "";
		              		}
		


				: row {
					: edit_box {
		                		key = "A10";
		           			label = "State: ";
		        	        	edit_width = 4;
		                		value = "";
		            	  	}
					: edit_box {
		                		key = "A11";
		           			label = "Zip: ";
		        	        	edit_width = 4;
		                		value = "";
		            	  	}
					: edit_box {
		                		key = "A12";
		           			label = "Phone #: ";
		        	        	edit_width = 12;
		                		value = "";
		            	  	}
				}

			      		: edit_box {
		                	key = "A13";
		                	label = "Email: ";
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
         	     		key = "G1";
        	       		label = "Size of Property";
        	       		edit_width = 6;
        	       		value = "";
       	       		} 
			: radio_column {
        	      		key = "B11-B12";
     	         		: radio_button {
     		        	key = "G2";
                		label = "Square Feet";
              			}
     	         		: radio_button {
                		key = "G3";
                		label = "Acres";
        	      		value = "1";
				}
                	}
               }


               : radio_row {
		label = " Shoreline Zoning ";
              	key = "B13-B14";
              		: radio_button {
                	key = "H1";
                	label = "Shoreline YES";
			value = "0";
              		}
     	         	: radio_button {
                	key = "H2";
                	label = "Shoreline NO";
			value = "1";
			}
                }
                
                               : radio_row {
              	key = "CURRENTUSE";
		label = " Current Use "; 
              	
			: radio_button {
                	key = "I1";
                	label = "Seasonal";
              		}
     	         	: radio_button {
                	key = "I2";
                	label = "Year Round";
                }
     	         	: radio_button {
                	key = "I3";
                	label = "Undeveloped";
                }
                : radio_button {
                	key = "I4";
                	label = "Commercial";
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