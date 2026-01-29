soillog : dialog { 
          label = "SeptiCAD HHE-200 Soil Log"; 

: column {


				: column {
					 : text {
					 height = "1.5";
					 label = "Enter Soil Profile and press ENTER to Auto-Fill Soil Profile Description.  All depths are in inches.";
					 is_bold = true; 
					 } 
				}

				: row {
				 :boxed_row {
			        	 : popup_list {
			                 key = "tptb";
			                 label = "";
			                 width = 15;
			                 value = "0";
					 }
					 : edit_box {
			                 key = "hole";
			                 label = "Hole #-> ";
			                 edit_width = 8;
			                 value = "TP-1";
			              	 }
				 }
				 : boxed_row {
					 : edit_box {
			                 key = "profile";
			                 label = "Soil Profile-> ";
			                 edit_width = 4;
			                 value = "";
					 action = "(soilAutoFill)";
			              	 }	
			
					 : edit_box {
			                 key = "class";
			                 label = "Drainage Class-> ";
			                 edit_width = 4;
			                 value = "";
					 action = "(classAutoFill)";
			              	 }
			
					 : edit_box {
			                 key = "slope";
			                 label = "Slope-> ";
			                 edit_width = 4;
			                 value = "";
			              	 }
				 }
				}






                                   : row {
					: boxed_row {
					 label = "Type/Depth of Limiting Factor";
					 : popup_list {
			                 key = "lim_type";
			                 label = "";
			                 width = 10;
			                 value = "";
					 }
					 : edit_box {
			                 key = "lim";
			                 label = " at ";
			                 edit_width = 4;
			                 value = "";
		              	 	 }
					 : text {
					 label = " inches"; 
					 } 
					}
					: boxed_row {
				 	 label = "Limit of Excavation";
					 : popup_list {
			                 key = "ex_why";
			                 label = "";
			                 width = 17;
			                 value = "";
					 }
					 : edit_box {
			                 key = "ex_lim";
			                 label = "";
			                 edit_width = 4;
			                 value = "";
					 } 
					 : text {
					 key = "ex_text";
					 label = " inches"; 
					 } 
					}
                                   } // end row




	      : boxed_column {
                label = "Soil Profile Description";
		: row {
		 : edit_box {
		 key = "org";
		 label = "Depth of Organic Horizon-> ";
		 edit_width = 4;
		 value = "";
		 }
  		 : spacer {
		 height = "1";
		 width = "20";
		 }
		 : edit_box {
                 key = "elv";
                 label = "Ground Surface Elevation-> ";
                 edit_width = 4;
                 value = "";
              	 }
		}// end row
		: row {
		 : text {
		 key = "H1";
		 label = "Horizon 1-->"; 
		 } 
		 : edit_box {
                 key = "NOTHING";
                 label = "";
                 edit_width = 4;
                 value = "0";
		 is_enabled = false;
              	 }


		 : popup_list {
                 key = "s1a";
                 label = "";
                 width = 30;
                 value = "";
              	 }
		 : popup_list {
                 key = "s1c";
                 label = "";
                 width = 15;
                 value = "";
		 }
		 : popup_list {
                 key = "s1d";
                 label = "";
                 width = 20;
                 value = "";
              	 }
		} // end row

		: row {
		 : text {
		 key = "H2";
		 label = "Horizon 2-->"; 
		 } 
		 : edit_box {
                 key = "s1b";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : popup_list {
                 key = "s2a";
                 label = "";
                 width = 30;
                 value = "";
              	 }
		 : popup_list {
                 key = "s2c";
                 label = "";
                 width = 15;
                 value = "";
		 }
		 : popup_list {
                 key = "s2d";
                 label = "";
                 width = 20;
                 value = "";
              	 }
		} // end row

		:  row {
		 : text {
		 key = "H3";
		 label = "Horizon 3-->"; 
		 } 
		 : edit_box {
                 key = "s2b";
                 label = "";
                 edit_width = 4;
                 value = "";
              	 }
		 : popup_list {
                 key = "s3a";
                 label = "";
                 width = 30;
                 value = "";
              	 }
		 : popup_list {
                 key = "s3c";
                 label = "";
                 width = 15;
                 value = "";
		 }
		 : popup_list {
                 key = "s3d";
                 label = "";
                 width = 20;
                 value = "";
              	 }
		} // end row
	 }// end boxed_column


         : row {
           label = "Drainage Mottling";

					: row {
					 : popup_list {
			                 key = "mottle1";
			                 label = "";
			                 width = 15;
			                 value = "";
					 action ="(checkMottle)";
					 }
					 : edit_box {
			                 key = "mottle1_depth";
			                 label = "at";
			                 edit_width = 2;
			                 value = "";
					 action ="(checkMottle)";
			              	 }
					 : text {
					 label = " inches         "; 
					 } 
					}
					: row {
					 : popup_list {
			                 key = "mottle2";
			                 label = "";
			                 width = 15;
			                 value = "";
					 action ="(checkMottle)";
					 is_enabled = false;
					 }
					 : edit_box {
			                 key = "mottle2_depth";
			                 label = "at";
			                 edit_width = 2;
			                 value = "";
					 action ="(checkMottle)";
					 is_enabled = false;
			              	 }
					 : text {
					 label = " inches"; 
					 } 
					}
	}





	 : column { 
	          : row {
		: boxed_column {
		      : text {
			value = " Left Side Log ";
			alignment = centered;
			height = 2;
			is_bold = true;
		      }
	              : button {
	                key = "log1";
	                label = " Log Only";
	                is_default = true;
	                is_cancel = false;
	              }
	              : button {
	                key = "log1-2";
	                label = " Log and Locate on Page 2 ";
	                is_default = true;
	                is_cancel = false;
	              }
	              : button {
	                key = "log1-3";
	                label = " Log and Locate on Page 3 ";
	                is_default = true;
	                is_cancel = false;
	              }
		}// column

		: boxed_column {
		      : text {
			value = " Right Side Log ";
			alignment = centered;
			height = 2;
			is_bold = true;
		      }
	              : button {
	                key = "log2";
	                label = " Log Only";
	                is_default = false;
	                is_cancel = false;
	              }
	              : button {
	                key = "log2-2";
	                label = " Log and Locate on Page 2 ";
	                is_default = false;
	                is_cancel = false;
	              }
	              : button {
	                key = "log2-3";
	                label = " Log and Locate on Page 3 ";
	                is_default = false;
	                is_cancel = false;
	              }
		}// column
		: column {
			alignment = centered;
		: column {
                      label = "Set Default Soil Profile";
                      : text {
			value = " Set Default Profile ??? ";
                        key = "default_text";
			alignment = centered;
			height = 2;
	                is_enabled = false;
		      }
			: button {
	                key = "default_button";
	                label = " Set ";
	                is_default = false;
	                is_cancel = false;
			height = 4;
	                is_enabled = false;
	              }
                }
	              : button {
	                key = "cancel";
	                label = " Cancel ";
	                is_default = false;
	                is_cancel = true;
			height = 4;
	              }
		}// column
	           } //end row
	 } // end column


} // end column
}// end dialog