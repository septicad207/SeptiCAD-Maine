PAGE1C : dialog { 
          label = "SeptiCAD --- HHE-200 -- Page 1  --  Part C"; 

          : column {
	   : row {
              :column { 
	       : boxed_column {
		label = "Treatment Tank";
			: toggle {
                	key = "E1";
                	label = "Concrete";
                	value = "1";
                        action = "(changeConcreteTankRadio)";
              		}

			: radio_row {
              		key = "TANKTYPE";
              		spacer_0;
				: radio_button {
                		key = "E2";
                		label = "Regular";
				value = "1";
				}
     	         		: radio_button {
                		key = "E3";
                		label = "Low Profile";
				value = "0";
        	      		}
                	}



			: toggle {
                	key = "E4";
                	label = "Plastic";
                	value = "0";  
              		}
		       : row {
		        : toggle {
                	key = "E5";
                	label = "Other";
                	value = "0";  
              		}
              		: edit_box {
                	key = "E6";
                	label = "Specify:";
                	edit_width = 15;
                	value = "";
              		}
                       }
		      : row {
             		: edit_box {
                	key = "E7";
                	label = "Capacity:";
                	edit_width = 8;
                	value = "1,000";
              		}
			: text {
			value = "gallons";
			}
                       }// end row

             } //end boxed column

	       : boxed_column {
		label = "Soil Type and Limiting Factor (LF)";


                       : row {
			: text {
			value = "Profile";
			}
			: text {
			value = "Condition 1";
			}
			: text {
			value = "Condition 2";
			}
		       }// end of ROW
                  
                       : row {
			: popup_list {
	                key = "E8";
	                width = 1;
			action = "(loadingRate)";
	              	}
			: popup_list {
	                key = "E9";
	                width = 1;
			action = "";
	              	}
			: popup_list {
	                key = "E9-2";
	                width = 1;
			action = "";
	              	}

		       } // END ROW

                       : row {
			: text {
			value = "Pit/Boring #";
			}
			: text {
			value = "Type of LF";
			}
			: text {
			value = "Depth to LF";
			}
		       }// end of ROW

                       : row {
          		: edit_box {
                	key = "E11";
                	label = "";
                	edit_width = 4;
                	value = "TP-1";
              		}

			: spacer {
		          height = "1";
		          width = "3";
			}

			: popup_list {
                	key = "E13";
                	width = 10;
              		}
             		: edit_box {
                	key = "E12";
                	label = "";
                	edit_width = 3;
                	value = "";
              		}
		       }// end of ROW
               } //end boxed column         		
	       
	       : boxed_column {
	       label = " Disposal Field Sizing ";	
		     : row {
			: popup_list {
	                key = "SIZING";
	                width = 6;
			action = "(designFlow)";
	              	}
			: text {
			value = "(sq.ft/gal)/day";
			}
                     }// end row

	       } //end boxed column

                    
	      : boxed_column {
	       label = " Design Flow ";	
		: edit_box {
                key = "H1";
                label = "Design Flow [gpd]";
                edit_width = 6;
                value = "";
              	}
		: popup_list {
                key = "BASEDON";
                width = 10;
              	}
		: edit_box {
                key = "H12";
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
                key = "F1";
                label = "Stone Bed";
                value = "0";  
              	}
		: toggle {
                key = "F2";
                label = "Stone Trench";
                value = "0";  
              	}
	       }
		: toggle {
                key = "F3";
                label = "Proprietary Device";
                value = "0";  
              	}
	       : row {
		spacer_0;
		: toggle {
                key = "F4";
                label = "cluster array";
                value = "0";  
              	}
		: toggle {
                key = "F6";
                label = "linear";
                value = "0";  
              	}
		spacer_0;

	       }
	       : row {
		spacer_0;
		: toggle {
                key = "F5";
                label = "regular load";
                value = "0";  
              	}
		: toggle {
                key = "F7";
                label = "H-20 load";
                value = "0";  
              	}
	       }
	       : row {
		: toggle {
                key = "F8";
                label = "Other";
                value = "0";  
              	}
             	: edit_box {
                key = "F9";
                label = "Specify: ";
                edit_width = 25;
                value = "";
              	}
	       }
	       : row {
             	: edit_box {
                key = "F10";
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
                	key = "G1";
                	label = "No";
			}
     	         	: radio_button {
                	key = "G2";
                	label = "Yes";
        	      	}
     	         	: radio_button {
                	key = "G3";
                	label = "Maybe";
        	      	}
                       }

               : row {
		: toggle {
                key = "G4";
                label = "Multi-Compartment Tank";
                value = "0";  
              	}
		: edit_box {
                key = "G6";
                label = "";
                edit_width = 3;
                value = "";
              	}
		: text {
		key = "TG6";
		label = "# of Tanks in Series"; 
		}
	       }
		: row {
		: toggle {
                key = "G7";
                label = "Increase in Tank Capacity";
                value = "0";  
              	}
		: toggle {
                key = "G8";
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
                key = "G12";
                label = "Dose [gallons]:";
                edit_width = 5;
                value = "";
              	}
	       } //end boxed row









	      : boxed_column {
	       label = " Latitude and Longitude @ Center of System ";	
		: row {
		 : edit_box {
                 key = "H5";
                 label = "Latitude [d, m, s.ss]";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "H6";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "H7";
                 label = "";
                 edit_width = 6;
                 value = "";
              	 }
		}
		: row {
		 : edit_box {
                 key = "H8";
                 label = "Longitude [d, m, s.ss]";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "H9";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : edit_box {
                 key = "H10";
                 label = "";
                 edit_width = 6;
                 value = "";
              	 }
		}
		: edit_box {
                key = "H11";
                label = "Margin of Error: ";
                edit_width = 10;
                value = "20'";
              	}
               } //end boxed column



	} //end column


} // end row











: row {

               : boxed_column {
	       label = " Elevation Reference Point Information ";
		 : edit_box {
                 key = "ERP1";
                 label = "Description Line 1: ";
                 edit_width = 30;
                 value = "";
              	 }
		 : edit_box {
                 key = "ERP2";
                 label = "Description Line 2: ";
                 edit_width = 30;
                 value = "";
              	 }
		: edit_box {
                key = "ERP3";
                label = "Reference Elevation: ";
                edit_width = 4;
                value = "0.0\"";
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