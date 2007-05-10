import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;

class RETestRes
{
    public Boolean eql;
    public String  exp;
    public String  rec;
    RETestRes(boolean eql, String exp, String rec)
    {
	this.eql = eql;
	this.exp = exp;
	this.rec = rec;
    }
}

public class SemanticCheck
{
    ${helpFunctions}
    
    ${testFunction}
    
    private static String read_file(String path)
	throws java.io.FileNotFoundException,  java.io.IOException
    {
	String ret="";
	
	// Reading files is JAVA ist just too easy!  This should
	// use more objects.
	BufferedReader br = new BufferedReader(new FileReader(path));

	while (br.ready()) {
	    ret += br.readLine() + "\n";
	}
	
	br.close();

	return ret;
    }

    public static void main(String[] argv)
	throws java.io.FileNotFoundException,  java.io.IOException
    {
	// Note: argv[0] is not the program name but the first command
	// line arg.

	String input = read_file(argv[2]);
	
	//System.out.println("model: "   + argv[0]);
	//System.out.println("student: " + argv[1]);
	//System.out.println(input);
	
	RETestRes res = test(argv[0], argv[1], input);
	
	System.out.println("isEqual="      + res.eql.toString()
			   + ";;expected=" + res.exp
			   + ";;received=" + res.rec);
	
	//if (!res.eql) System.exit(1);
    }
}
