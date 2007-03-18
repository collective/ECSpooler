    /* Find the first matches for matchers [model] and [student] and
     * return an [RETestRes] that indicates whether group(0) of both
     * matches are equal.
     */
    private static RETestRes test(String model_pat,
				  String student_pat,
				  String input)
    {
	final int FLAGS		= Pattern.MULTILINE;
	Pattern model_re	= Pattern.compile(model_pat,   FLAGS);
	Pattern student_re	= Pattern.compile(student_pat, FLAGS);
	
	Matcher model_m		= model_re.matcher(input);
	Matcher student_m	= student_re.matcher(input);
	
	boolean have_exp	= model_m.find();
	boolean have_rec	= student_m.find();
	
	String exp_str		= have_exp? model_m.group(0)   : "";
	String rec_str		= have_rec? student_m.group(0) : "";
	
	if (!have_exp && !have_rec) return new RETestRes(true, "", "");
	else return new RETestRes(have_exp && have_rec &&
				  (model_m.start() == student_m.start()) &&
				  exp_str.equals(rec_str),
				  exp_str,
				  rec_str);
    }
