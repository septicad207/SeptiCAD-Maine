swingtie : dialog {
  label = "SeptiCAD --- Swing Tie";

: column {
   
   
   
   : boxed_column { // end boxed_column 1

	 : text {
          key = "DESCRIPTION1";
          alignment = centered;
	 	 value ="";
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
         label = "Reference Point ->";
         width = 3;
         value = "0";
         }
   } // end boxed_column 1

   : boxed_row {  // start row with distances (3 columns total)
     label = "Swing Tie Distances and Side";



: row { // START COLUMN

		: column {
			
				: edit_box {
				 key = "TL";
				 alignment = centered;	
				 edit_width = 3.0;
				}
				


				: toggle {
				  key = "LEFT";
				  alignment = centered;	
				  action ="(swingtieToggleDCL \"LEFT\")";
				}

				: edit_box {
				 key = "BL";
				 alignment = centered;	
				 edit_width = 3.0;
				}

		}



		: column {
					
			: row {
							: spacer {
								  fixed_height = true;
								  fixed_width = true;
								  height = 1;
								  width = 3;
							}
							: toggle {
							  alignment = centered;	
							  key = "UP";
							  action = "(swingtieToggleDCL \"UP\")";
							}
						
			}
			
			: row {
							: spacer {
								  fixed_height = true;
								  fixed_width = true;
								  height = 1;
								  width = 3;
							}
							: image { 
							  alignment = centered;	
							  key = "field"; 
							  fixed_height = true;
							  fixed_width = true;
							  height = 10; 
							  width =  50;  
							  color = 5;
							} 
			}

			: row {
							: spacer {
								  fixed_height = true;
								  fixed_width = true;
								  height = 1;
								  width = 3;
							}
							: toggle {
							 alignment = centered;	
							  key = "DOWN";
							  action ="(swingtieToggleDCL \"DOWN\")";
							}
			}

		}


		: column {	

				: edit_box {
				 key = "TR";
				 edit_width = 3.0;
				}

				: toggle {
				  key = "RIGHT";
				  alignment = centered;	
				  action ="(swingtieToggleDCL \"RIGHT\")";
				}

				: edit_box {
				 key = "BR";
				 edit_width = 3.0;
				}

		}




}// end ROW






    }


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
