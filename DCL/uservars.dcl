USERVARS : dialog {
  label = "  SeptiCAD --- Preferences  ";

  : column {
     // Start Text Fields
	// New Row1
    : row {


	: column {
	   label = "Designer Infomation";
	      : row {
                : column {
                  : text {
                    alignment = left;
                    value = "";
                    width = 10;
                  }
                  : text {
                    alignment = left;
                    value = "Default User";
                    width = 10;
                  }
                  : text {
                    alignment = left;
                    value = "Alternate";
                    width = 10;
                  }
                  : text {
                    alignment = left;
                    value = "Alternate";
                    width = 10;
                  }
                 }
                : column {
                  : text {
                    alignment = left;
                    value = "     Site Evaluator Name";
                    width = 25;
                  }
                  : edit_box {
	          key = "NAME";
	          edit_width = 15;
	          value = "";
	          }
                  : edit_box {
	          key = "NAME2";
	          edit_width = 15;
	          value = "";
	          }
                  : edit_box {
	          key = "NAME3";
	          edit_width = 15;
	          value = "";
	          }
                 }
                : column {
                  : text {
                    alignment = left;
                    value = "     LSE #";
                    width = 10;
                  }
                  : edit_box {
	          key = "NUMBER";
	          edit_width = 5.0;
	          value = "";
	          }
                  : edit_box {
	          key = "NUMBER2";
	          edit_width = 5.0;
	          value = "";
	          }
                  : edit_box {
	          key = "NUMBER3";
	          edit_width = 5.0;
	          value = "";
	          }
                 }
                : column {
                  : text {
                    alignment = left;
                    value = "   Email Address";
                  }
                  : edit_box {
	          key = "EMAIL";
	          edit_width = 10;
	          value = "";
	          }
                  : edit_box {
	          key = "EMAIL2";
	          edit_width = 10;
	          value = "";
	          }
                  : edit_box {
	          key = "EMAIL3";
	          edit_width = 10;
	          value = "";
	          }
                 }
                : column {
                  : text {
                    alignment = left;
                    value = "        Phone No.";
                  }
                  : edit_box {
	          key = "PHONE";
	          edit_width = 10;
	          value = "";
	          }
                  : edit_box {
	          key = "PHONE2";
	          edit_width = 10;
	          value = "";
	          }
                  : edit_box {
	          key = "PHONE3";
	          edit_width = 10;
	          value = "";
	          }
                 }
              	}
	}// column




	// New Row
	: column {
	   label = "SeptiCAD Infomation";


		: edit_box {
	        key = "USERNAME";
	        label = "User Name -->";
	        edit_width = 15.0;
	        value = "";
		is_enabled = true;
	        }
		: edit_box {
	        key = "SERIAL";
	        label = "Serial Code -->";
	        edit_width = 15.0;
	        value = "";
		is_enabled = true;
	        }

	}


}// ROW1 end top row



	: image { 
	   key = "customize"; 
	   fixed_height = true;
	   height = 2; 
	   color = dialog_background;
	} 
	

	// New Column
        : row {
               : column {
 		: boxed_column {	// Start Buttons
		label = "Customize SeptiCAD Libaries";
		        : button {
		          key = "mapLabels";
		          label = "  Map Labels Library  ";
		          is_default = false;
		        }
		        : button {
		          key = "mapNotes";
		          label = "  Map Notes Libary  ";
		          is_default = false;
		        }
		        : button {
		          key = "blocks";
		          label = "  Block Library  ";
		          is_default = false;
		        }
		        : button {
		          key = "systemCustom";
		          label = "  System Customization  ";
		          is_default = false;
		        }
	              }// end buttons 

                         : boxed_column {
 		          label = "Row Separation Input/Output";
                              : popup_list {
                              key = "SPACING";
                              label = "Input Row Spacing ";
                              width = 30;
                              value = "0";
                              }	
                              : popup_list {
                              key = "SPACING-DIM";
                              label = "Label on Cross Section ";
                              width = 20;
                              value = "0";
                              } 
                          }

                  }



		: column {
                      : row {
                       : column {

                         : boxed_column {
 		          label = "Default System for Main Design Window";
                              : popup_list {
                              key = "SYSTEM";
                              label = "";
                              width = 6;
                              value = "";
                              } 
                          }
                        : boxed_column {             
 		          label = "Existing Grade Elevation Display";
				 : popup_list {
			         key = "CORNERLABELS";
			         width = 20;
			         value = "";
			         }
                        }
     

                          : boxed_column {
 		            label = "Miscellaneous Options";
		              : toggle {
		                key = "TRANSECT";
		                label = "Label Cross-Section on Page 3";
		                value = "0"; 
				is_enabled = true;
		              }
				: toggle {
				key = "DRAWSHOULDERS";
				label = "Draw Shoulders on Page 3 Plan";
				value = "0";  
				is_enabled = true;
				}
		              : toggle {
		                key = "TABLE";
		                label = "Display Elevation Table on Level Systems";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "FILLCALC";
		                label = "Display Fill Volumes On Cross Section";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "NOTES-PRO";
		                label = "Display Page 3 Cross Section Note in Top Left Corner";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "DIVERT";
		                label = "Draw Stormwater Diversion Swale on Page 3 Plan by Default";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "AUTO-SCAR";
		                label = "Prompt for Automaticaly Adding Transitional Horizon on Design (Experimental)";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "SWING";
		                label = "Prompt for Swing Ties on Continue Design";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "TANKELV";
		                label = "Prompt for Septic Tank Outlet Elevation Calculator On Continue Design";
		                value = "0";  
				is_enabled = true;
		              }
		              : toggle {
		                key = "ALLCAPS";
		                label = "Change All Printed Text to CAPITALS after Design and Continue Design";
		                value = "0";  
				is_enabled = true;
		              }








                           }
                       }



















                       }


		}// column



	}// end column
     // End Text Fields
	
	


: row {// model space text/arrow multipliers and Custom Buttons




	// New Column
      : column { 
       : column {
        label = "Model Space Output [Scaled] ";

		: text {
		value = ">--------- See Help for Info on How to Modify  ---------<";
		alignment = centered;
		height = 2;
		}

	     : column {
		: edit_box {
	        key = "TEXT_SIZE_MAP";
	        label = "Label Text Size -->";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }
		: edit_box {
	        key = "ARROWSCALE";
	        label = "Label Arrowhead Size --> ";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
		}
		: edit_box {
	        key = "TEXT_SIZE_MAPNOTE";
	        label = "Page 2/3 Plan Note Text -->";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }  
	   }// end column
	  }// end column
	// New Column
        : column {
        label = "SeptiCAD Label Library Options ";
	     : column {
		: edit_box {
	        key = "SLABEL-TEXT";
	        label = "Text Size in Paperspace";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }
		: edit_box {
	        key = "SLABEL-ARROW";
	        label = "Arrowhead Size in Paperspace";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }
	   }// end column
	  }// end column
      }

	: boxed_column {
	 label = "Above-grade Fill Volume Calculations";

		: edit_box {
	        key = "VOLFUDGE";
	        label = "Correction Factor -->";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }  

		: edit_box {
	        key = "SANDCOMP";
	        label = "Sand Compaction Factor -->";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }
		: edit_box {
	        key = "LOAMCOMP";
	        label = "Loam Compaction Factor -->";
	        edit_width = 5.0;
	        value = "";
		is_enabled = true;
	        }  
	}


   : column {
			: boxed_column {
 		            label = "Page 2 Plan Notes";
		              : toggle {
		                key = "PAGE2NOTES";
		                label = "Ask for Page 2 Plan Notes";
		                value = "0";  
				is_enabled = true;
		              }

				 : popup_list {
			         key = "PAGE2NOTES_LOCATION";
			        // label = "Location -->";
			         width = 20;
			         value = "";
			         }
				: edit_box {
				key = "PAGE2NOTES_WIDTH";
				label = "Width % of Page -->";
				edit_width = 4.0;
				value = "";
				is_enabled = true;
				}
			} 
			: boxed_column {
 		            label = "Page 3 Plan Notes";
		              : toggle {
		                key = "PAGE3NOTES";
		                label = "Ask for Page 3 Plan Notes";
		                value = "0";  
				is_enabled = true;
		              }
				 : popup_list {
			         key = "PAGE3NOTES_LOCATION";
			         //label = "Location -->";
			         width = 20;
			         value = "";
			         }
				: edit_box {
				key = "PAGE3NOTES_WIDTH";
				label = "Width % of Page -->";
				edit_width = 4.0;
				value = "";
				is_enabled = true;
				}
			}
  }

}// end row with paperspace and model space scales for text







	// Start Buttons
	      : row {
	        : button {
	          key = "okay";
	          label = "  Save Changes  ";
	          is_default = true;
	        }
	        : button {
	          key = "cancel";
	          label = "  Discard Changes  ";
	          is_default = false;
	          is_cancel = true;
	        }
	
	      }
   }// End column
}// end dialog