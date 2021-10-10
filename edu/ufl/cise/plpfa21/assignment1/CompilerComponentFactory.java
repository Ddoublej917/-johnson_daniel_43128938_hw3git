package edu.ufl.cise.plpfa21.assignment1;

import edu.ufl.cise.plpfa21.assignment2.IPLPParser;
import edu.ufl.cise.plpfa21.assignment2.Parser;

public class CompilerComponentFactory{
	
	public static IPLPParser getParser(String input) {
	   	 //Implement this in Assignment 2
	   	 //Your parser will create a lexer.
		IPLPLexer inputLexer = getLexer(input);
		Parser outputParser = null;
		try {
			outputParser = new Parser(inputLexer);
		} catch (LexicalException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return outputParser;
		}

	
	public static IPLPLexer getLexer(String input) {
		Lexer result;
		result = new Lexer(input);
		return result;
	}

}