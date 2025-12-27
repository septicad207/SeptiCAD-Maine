sDemo : dialog {
label = "  SeptiCAD Trial Version";

: column {

 ////// DESCRIPTION START
 : boxed_column {
 alignment = centered;
	: text {
	value = "How to Register:";
	width = "80";
	}
	: text {
	value = "";
	}
	: text {
	value = "STEP 1: Send email to SEPTICAD@GMAIL.COM and include the following information:";
	}
	: text {
	value = "           * Computer Serial # that appear on the bottom of this window";
	}
	: text {
	value = "           * Type of CAD program-> e.g. BricsCAD v8.2.12, AutoCAD 2007, etc...";
	}
	: text {
	value = "           * Buyer Information - Name/Address/Phone/email";
	}
	: text {
	value = "           * SeptiCAD will contact you about billing options";
	}
	: text {
	value = "";
	}
	: text {
	value = "STEP 2: Enter your USER NAME and SERIAL CODE in SeptiCAD Preferences [PREF toolbar icon]";
	}
	: text {
	value = "";
	}
	: text {
	value = "STEP 3: Run SeptiCAD [Chamber toolbar icon].";
	}

 }// boxed_column
 ////// DESCRIPTION END

      : boxed_column {

	: edit_box {
        key = "serial";
        label = "Computer Serial # -->";
        edit_width = 20.0;
        value = "";
        }
       : row {
        : button {
          key = "okay";
          label = "  Continue  ";
          is_default = true;
          is_cancel = true;
        }

        : button {
          key = "register";
          label = "  Register  ";
          is_default = false;
          is_cancel = false;
        }
       }/// end row   
}/// boxed_column

}// column
}// dialog