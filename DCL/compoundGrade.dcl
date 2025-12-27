compoundGrade : dialog {
  label = "Compound Grades";

: column {

///////// START TEXT DESCRIPTION
   : column {

	 : text {
	  height = 1.5;
	  alignment = centered;
	  is_bold = true;
	  value =" Define spot elevations using a Length - Elevation pair (Len. [feet] - Elv. [inches]) in the form below.";
	 }
	 : text {
	  height = 1.5;
	  alignment = centered;
	  is_bold = true;
	  value =" The user may enter 4 spot grades based on each corner of the disposal field. ";
	 }
	 : text {
	  height = 1.5;
	  alignment = centered;
	  is_bold = true;
	  value =" All lengths are measured square from the edge of the disposal field. See SeptiCAD Help for more information";
	 }

   }// end column
/////////END TEXT DESCRIPTION



/////////////START COLUMN #1
   : column {  // start column 1

      : row {  ///// START ROW #1
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 10;
		}
		: boxed_row {
			: edit_box {
		        key = "U1U2L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
			: edit_box {
		        key = "U1U2E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
		}// end row
		: spacer {//// MIDDLE		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 40;
		}
		: boxed_row {
			: edit_box {
		        key = "U2U2L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
			: edit_box {
		        key = "U2U2E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
		}// end row	
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 15;
		}

	} ///////// end ROW # 1



      : row {  ///// START ROW #2
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 10;
		}
		: boxed_row {
			: edit_box {
		        key = "U1U1L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			action = "(showGradeInput \"U1U1L\" \"U1U2L\" \"U1U2E\")";
		        }
			: edit_box {
		        key = "U1U1E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
		        }
		}// end row
		: spacer {//// MIDDLE		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 40;
		}
		: boxed_row {
			: edit_box {
		        key = "U2U1L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			action = "(showGradeInput \"U2U1L\" \"U2U2L\" \"U2U2E\")";
		        }
			: edit_box {
		        key = "U2U1E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
		        }
		}// end row	
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 15;
		}

	} ///////// end ROW # 2

} // END COLUMN #1
/////////////////////////////////////



/////////////////////////////////////
////START COLUMN #2
: column { // start #2
	:row{

//// START LEFT-SIDE
		:column {
			: row {
				: boxed_column {
						: edit_box {
					        key = "U1L2L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
						: edit_box {
					        key = "U1L2E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
				}// end column
				: boxed_column {
						: edit_box {
					        key = "U1L1L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						action = "(showGradeInput \"U1L1L\" \"U1L2L\" \"U1L2E\")";
					        }
						: edit_box {
					        key = "U1L1E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
					        }
				}// end column
			}// row
			: spacer {
			
			          fixed_height = true;
			          fixed_width = true;
			          height = 5;
			          width = 1;
			}
			: row {
				: boxed_column {
						: edit_box {
					        key = "D1L2L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
						: edit_box {
					        key = "D1L2E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
				}// end column
				: boxed_column {
						: edit_box {
					        key = "D1L1L";
					        label = "Len.";
					        edit_width = 5.0;
						action = "(showGradeInput \"D1L1L\" \"D1L2L\" \"D1L2E\")";
					        value = "";
					        }
						: edit_box {
					        key = "D1L1E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
					        }
				}// end column
			}// end row
		}// end column

///// END LEFT-SIDE
////////////////////////////////////////////////////////

////// IMAGE INSERT
		: row {
			 : image { 
			   key = "field"; 
			   fixed_height = true;
			   fixed_width = true;
			   height = 13; 
			   width =  63;  
			   color = 5;
			 } 
		}// end row 
////// IMAGE INSERT
//////////////////////////////////////////////////////////

///// START RIGHT-SIDE

		:column {
			: row {
				: boxed_column {
						: edit_box {
					        key = "U2R1L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						action = "(showGradeInput \"U2R1L\" \"U2R2L\" \"U2R2E\")";					        }
						: edit_box {
					        key = "U2R1E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
					        }
				}// end column
				: boxed_column {
						: edit_box {
					        key = "U2R2L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
						: edit_box {
					        key = "U2R2E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
				}// end column
			}// row
			: spacer {
			
			          fixed_height = true;
			          fixed_width = true;
			          height = 5;
			          width = 1;
			}
			: row {
				: boxed_column {
						: edit_box {
					        key = "D2R1L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						action = "(showGradeInput \"D2R1L\" \"D2R2L\" \"D2R2E\")";
					        }
						: edit_box {
					        key = "D2R1E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
					        }
				}// end column
				: boxed_column {
						: edit_box {
					        key = "D2R2L";
					        label = "Len.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
						: edit_box {
					        key = "D2R2E";
					        label = "Elv.";
					        edit_width = 5.0;
					        value = "";
						is_enabled = false; 
					        }
				}// end column
			}// end row
		}// end column
///// END RIGHT-SIDE


	}// end row
} // end column #2
////// END MIDDLE ROW WITH IMAGE














/////////////START COLUMN #3
   : column {  // start column 3

      : row {  ///// START ROW #1
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 10;
		}
		: boxed_row {
			: edit_box {
		        key = "D1D1L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			action = "(showGradeInput \"D1D1L\" \"D1D2L\" \"D1D2E\")";
		        }
			: edit_box {
		        key = "D1D1E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
		        }
		}// end row
		: spacer {//// MIDDLE		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 40;
		}
		: boxed_row {
			: edit_box {
		        key = "D2D1L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			action = "(showGradeInput \"D2D1L\" \"D2D2L\" \"D2D2E\")";
		        }
			: edit_box {
		        key = "D2D1E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
		        }
		}// end row	
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 15;
		}

	} ///////// end ROW # 1



      : row {  ///// START ROW #2
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 10;
		}
		: boxed_row {
			: edit_box {
		        key = "D1D2L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
			: edit_box {
		        key = "D1D2E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
		}// end row
		: spacer {//// MIDDLE		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 40;
		}
		: boxed_row {
			: edit_box {
		        key = "D2D2L";
		        label = "Len.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
			: edit_box {
		        key = "D2D2E";
		        label = "Elv.";
		        edit_width = 5.0;
		        value = "";
			is_enabled = false; 
		        }
		}// end row	
		: spacer {
		
		          fixed_height = true;
		          fixed_width = true;
		          height = 1;
		          width = 15;
		}

	} ///////// end ROW # 2

} // END COLUMN #3
/////////////////////////////////////









  






























      : row { // start button row
        : button {
          key = "okay";
          label = "  Okay  ";
          is_default = true;
        }
        : button {
          key = "cancel";
          label = "  Skip\Cancel  ";
          is_default = false;
          is_cancel = true;
        }
        : button {
          key = "reset";
          label = "  Reset Form  ";
          is_default = false;
          is_cancel = false;
        }
              : button {
                key = "help";
                label = " HELP ";
                is_default = false;
                is_cancel = false;
              }

      } // end button row


}// end column
}// end dialog
