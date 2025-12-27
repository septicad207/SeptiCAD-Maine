GEOREF : dialog {
  label = "  SeptiCAD GeoRef Tool ";

: boxed_column {
//		: text {
//		key = "TEXT";
		label = "Select GeoReferencing Method"; 
//		}

      : boxed_row {


        : button {
          key = "mode1";
          label = "          Scale          ";
          width = "30";
          fixed_width = true;
          is_default = true;
        }
        
                :column {
      		: text {
      		label = "Scale Selected using"; 
      		}
          : text {
      		label = "  *  2 Match Points &";
      		}
          : text {
      		label = "  *  Known Distance";
      		}  
         }
        
        }
        
        
      : boxed_row {

        : button {
          key = "mode2";
          label = "       Move + Rotate     ";
          width = "30";
          fixed_width = true;
          is_default = false;
        }
        
              : column {
          : text {
      		label = "Move and rotate selected objects using:";

      		}
          : text {
      		label = "  *  2 Match Points &";
      		}
          : text {
      		label = "  *  2 Reference Points";
      		}      		
       		}
        
        
        }
        
        : boxed_row {
        : button {
          key = "mode3";
          label = "  Move + Rotate + Scale  ";
          width = "30";
          fixed_width = true;
          is_default = false;
        }
          : column {
          : text {
      		label = "Move, rotate and scale selected objects using:"; 
      		}
          : text {
      		label = "  *  2 Match Points &";
      		}
          : text {
      		label = "  *  2 Reference Points";
      		}      		
				   }
        }
 	: row {

        : button {
          key = "cancel";
          label = "  Cancel  ";
          width = "30";
          fixed_width = true;
          is_default = false;
          is_cancel = true;
        }
	} // end row       
        

}// end button row
}// end column
}// end dialog