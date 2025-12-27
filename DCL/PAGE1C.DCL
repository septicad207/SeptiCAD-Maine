PAGE1C : dialog { 
          label = "SeptiCAD --- HHE-200 -- Page 1  --  Part C"; 

          : column {
	   : row {
              :column { 
	       : boxed_column {
		label = "Treatment Tank";
			: toggle {
                	key = "D1";
                	label = "Concrete";
                	value = "1";
                        action = "(changeConcreteTankRadio)";
              		}

			: radio_row {
              		key = "TANKTYPE";
              		spacer_0;
				: radio_button {
                		key = "D7";
                		label = "Regular";
				value = "1";
				}
     	         		: radio_button {
                		key = "D8";
                		label = "Low Profile";
				value = "0";
        	      		}
                	}



			: toggle {
                	key = "D2";
                	label = "Plastic";
                	value = "0";  
              		}
          : row {
		        : toggle {
                	key = "D3";
                	label = "External Grease Interceptor: ";
                	value = "0";  
              		}
              		: edit_box {
                	key = "D9";
                	label = "Specify:";
                	edit_width = 5;
                	value = "";
              		}
                       }
		       : row {
                  : toggle {
                	key = "D4";
                	label = "Other";
                	value = "0";  
              		}
              		: edit_box {
                	key = "D10";
                	label = "Specify:";
                	edit_width = 15;
                	value = "";
              		}
                       }

             		: edit_box {
                	key = "D5";
                	label = "Tank Capacity (gallons): ";
                	edit_width = 5;
                	value = "1,000";
              		}
             		: edit_box {
                	key = "D6";
                	label = "Total # of (new) tanks: ";
                	edit_width = 5;
                	value = "1";
              		}

             } //end boxed column

	       : boxed_column {
		label = "Soil Type and Limiting Factor (LF)";

          		: edit_box {
                	key = "P3";
                	label = "Test Pit or Boring #";
                	//edit_width = 5;
                	value = "TP-1";
              		}
                  
           : boxed_row {
          		: edit_box {
                	key = "P1";
                	label = "Profile: ";
                	edit_width = 5;
                	value = "";
              		}
             		: edit_box {
                	key = "P2";
                	label = "Condition(s): ";
                	edit_width = 5;
                	value = "";
              		}
            }
             		: edit_box {
                	key = "P4";
                	label = "         Limiting Factor Depth: ";
                	edit_width = 5;
                	value = "";
              		}
             		: edit_box {
                	key = "P5";
                	label = "     Limiting Factor Elevation: ";
                	edit_width = 5;
                	value = "";
              		}

      } //end boxed column         		
	       


                    
	      : boxed_column {
	       label = " Design Flow ";	
		: edit_box {
                key = "O1";
                label = "Design Flow [gpd]";
                edit_width = 6;
                value = "";
              	}
		: popup_list {
                key = "BASEDON";
                width = 10;
              	}
		: edit_box {
                key = "O4";
                label = "Calculations:";
                edit_width = 17;
                value = "";
              	}

               } //end boxed column


            } //end column















	: column {

	       : boxed_column {
	       label = " Disposal Field Type ";		
	       : row {
		: toggle {
                key = "J1";
                label = "Stone Bed";
                value = "0";  
              	}
		: toggle {
                key = "J2";
                label = "Stone Trench";
                value = "0";  
              	}
	       }
		: toggle {
                key = "J3";
                label = "Proprietary Device";
                value = "0";  
              	}
	       : row {
		spacer_0;
		: toggle {
                key = "J4";
                label = "cluster array";
                value = "0";  
              	}
		: toggle {
                key = "J7";
                label = "linear";
                value = "0";  
              	}
		spacer_0;

	       }
	       : row {
		spacer_0;
		: toggle {
                key = "J5";
                label = "regular load";
                value = "0";  
              	}
		: toggle {
                key = "J8";
                label = "H-20 load";
                value = "0";  
              	}
	       }
         
    	       : row {
		spacer_0;
		: toggle {
                key = "J6";
                label = "Eljen";
                value = "0";  
              	}
		: toggle {
                key = "J9";
                label = "Plastic Chamber";
                value = "0";  
              	}
	       }     
         
         
	       : row {
		: toggle {
                key = "J10";
                label = "Other";
                value = "0";  
              	}
             	: edit_box {
                key = "J11";
                label = "Specify: ";
                edit_width = 25;
                value = "";
              	}
	       }
	       : row {
             	: edit_box {
                key = "J12";
                label = "Size:";
                edit_width = 10;
                value = "";
              	}
		: popup_list {
                key = "SIZETYPE";
                width = 10;
              	}
	       }

	       } //end boxed column
	


  : boxed_column {
	       label = " Garbage Disposal Unit ";	
		      : radio_row {
              		key = "GARBAGE";
                : radio_button {
                	key = "N1";
                	label = "No";
                }
     	         	: radio_button {
                	key = "N2";
                	label = "Yes";
        	      	}
     	         	: radio_button {
                	key = "N3";
                	label = "Maybe";
        	      	}
                       }
                       
          : toggle {
             key = "N4";
             label = "Multi-Compartment Tank";
             value = "0";  
            	}

          : row {
          : toggle {
             key = "N5";
             label = "Tanks in Series";
             value = "0";  
            	}
              : edit_box {
                key = "N6";
                label = "      Specify # of tanks: ";
                edit_width = 5;
                value = "";
              	}
            }
            
		: row {
		: toggle {
                key = "N7";
                label = "Increase in Tank Capacity";
                value = "0";  
              	}
		: toggle {
                key = "N8";
                label = "Filter on Tank Outlet";
                value = "0";  
              	}
		}// end row
} //end boxed column






	       : boxed_row {
	       label = " Effluent/Ejector Pump ";	
		: popup_list {
                key = "PUMP";
                width = 20;
              	}
             	: edit_box {
                key = "M4";
                label = "Dose [gallons]:";
                edit_width = 5;
                value = "";
              	}
	       } //end boxed row









	      : boxed_column {
	       label = " Latitude and Longitude @ Center of System ";	
		: row {
		 : edit_box {
                 key = "S1";
                 label = "Latitude [d, m, s.ss]";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "S2";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "S3";
                 label = "";
                 edit_width = 6;
                 value = "";
              	 }
		}
		: row {
		 : edit_box {
                 key = "S4";
                 label = "Longitude [d, m, s.ss]";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "S5";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "S6";
                 label = "";
                 edit_width = 6;
                 value = "";
              	 }
		}
		: edit_box {
                key = "S7";
                label = "Margin of Error: ";
                edit_width = 10;
                value = "20 ft";
              	}
               } //end boxed column



	} //end column


} // end row











: row {

          : column {
            : boxed_column {
                label = " Elevation Reference Point Information ";
              : edit_box {
                 key = "X1";
                 label = "Description: ";
                 edit_width = 30;
                 value = "";
              	 }
              : edit_box {
                key = "X2";
                label = "Reference Elevation: ";
                edit_width = 4;
                value = "0.0\"";
              	}
            }
               
            : boxed_column {
              label = " Additional Notes ";
              : edit_box {
                 key = "R1";
                 label = "Description: ";
                 edit_width = 30;
                 value = "";
              	 }
            }
          }


          : boxed_column {
              : button {
                key = "accept";
                label = "      Ok / Complete Form      ";
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











}	 
}