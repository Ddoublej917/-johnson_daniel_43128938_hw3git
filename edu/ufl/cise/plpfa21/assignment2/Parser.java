package edu.ufl.cise.plpfa21.assignment2;

import java.util.ArrayList;
import java.util.List;

import edu.ufl.cise.plpfa21.assignment1.IPLPLexer;
import edu.ufl.cise.plpfa21.assignment1.IPLPToken;
import edu.ufl.cise.plpfa21.assignment1.LexicalException;
import edu.ufl.cise.plpfa21.assignment1.Token;
import edu.ufl.cise.plpfa21.assignment1.PLPTokenKinds.Kind;
import edu.ufl.cise.plpfa21.assignment3.ast.IExpression;
import edu.ufl.cise.plpfa21.assignment3.ast.IIdentifier;
import edu.ufl.cise.plpfa21.assignment3.ast.IType.TypeKind;
import edu.ufl.cise.plpfa21.assignment3.astimpl.BooleanLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Expression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.FunctionCallExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.IdentExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Identifier__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.IntLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ListSelectorExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ListType__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.NilConstantExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.PrimitiveType__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.StringLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Type__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.UnaryExpression__;

//Step 1: compute predict sets for each production
//Step 2: rewrite grammar so that each non-terminal is on the left side of only 1 production

public class Parser implements IPLPParser {
	IPLPLexer inputLexer;
	int line;
	int charPositionInLine;
	String errorMessage;
	IPLPToken t; // next token
	
	public Parser (IPLPLexer input) throws Exception{
		this.inputLexer = input;
		t = inputLexer.nextToken();
		line = t.getLine();
		charPositionInLine = t.getCharPositionInLine();
		errorMessage = t.getText();
	}
	
	boolean isKind (IPLPToken t, IPLPToken.Kind kind) {
		if(t.getKind().equals(kind))
			return true;
		else
			return false;
	}
	
	boolean isExpStart() throws LexicalException {
		switch(t.getKind()) {
			case KW_NIL -> {
				return true;
			}
			case KW_TRUE -> {
				return true;
			}
			case KW_FALSE -> {
				return true;
			}
			case INT_LITERAL -> {
				return true;
			}
			case STRING_LITERAL -> {
				return true;
			}
			case LPAREN -> {
				return true;
			}
			case IDENTIFIER -> {
				return true;
			}
			case BANG -> {
				return true;
			}
			case MINUS -> {
				return true;
			}
			default -> {
				return false;
			}
			
		}
	}
	
	void consume() throws LexicalException {
		t = inputLexer.nextToken();
	}
	
	void match(IPLPToken.Kind kind) throws LexicalException, SyntaxException {
	       if(isKind(t, kind)){
			   consume();
	       }
	       else {
	    	   throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
	       }  
	}
	
	void program() throws LexicalException, SyntaxException {
		//ADD LOOPING STRUCTURES FOR DECLARATIONS
		while (isKind(t,IPLPToken.Kind.KW_FUN) || isKind(t,IPLPToken.Kind.KW_VAL) || isKind(t,IPLPToken.Kind.KW_VAR)) {
			declaration();
		}
		if(!isKind(t,IPLPToken.Kind.EOF)) {
	    	   throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
	       }  
	}
	
	void declaration() throws LexicalException, SyntaxException {
		switch(t.getKind()) {
			case KW_FUN -> {
				function();
			}
			case KW_VAR -> {
				consume();
				namedef();
				if(isKind(t, IPLPToken.Kind.ASSIGN)) {
					consume();
					expression();
				}
				match(IPLPToken.Kind.SEMI);
			}
			case KW_VAL -> {
				consume();
				namedef();
				match(IPLPToken.Kind.ASSIGN);
				expression();
				match(IPLPToken.Kind.SEMI);
			}
		
			default -> {
				throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
			}
		}
	}
	
	void function() throws LexicalException, SyntaxException {
		match(IPLPToken.Kind.KW_FUN);
		match(IPLPToken.Kind.IDENTIFIER);
		match(IPLPToken.Kind.LPAREN);
		if(isKind(t, IPLPToken.Kind.RPAREN)) {
			consume();
			if(isKind(t, IPLPToken.Kind.COLON)) {
				consume();
				type();
			}
			match(IPLPToken.Kind.KW_DO);
			block();
			match(IPLPToken.Kind.KW_END);
		}
		else {
			namedef();
			while(isKind(t, IPLPToken.Kind.COMMA)) {
				consume();
				namedef();
			}
			if(isKind(t, IPLPToken.Kind.RPAREN)) {
				consume();
				if(isKind(t, IPLPToken.Kind.COLON)) {
					consume();
					type();
				}
				match(IPLPToken.Kind.KW_DO);
				block();
				match(IPLPToken.Kind.KW_END);
			}
			else {
				throw new IllegalArgumentException("Unexpected value: " + t.getKind());
			}
		}
				
		
	}
	
	void block() throws LexicalException, SyntaxException {
		 //FIX LOOP STRUCTURE FOR BLOCKS TO INCLUDE EXPRESSION STATEMENTS
		while (isKind(t,IPLPToken.Kind.KW_LET) || isKind(t,IPLPToken.Kind.KW_SWITCH) || isKind(t,IPLPToken.Kind.KW_IF) || isKind(t,IPLPToken.Kind.KW_WHILE) || isKind(t,IPLPToken.Kind.KW_RETURN) || isExpStart() == true) {
			statement();
		}
	}
	
	void namedef() throws LexicalException, SyntaxException {
		if(isKind(t, IPLPToken.Kind.IDENTIFIER)) {
			consume();
			if(isKind(t, IPLPToken.Kind.COLON)) {
				consume();
				type();
			}
		}
		else {
			throw new IllegalArgumentException("Unexpected value: " + t.getKind());
		}
	}
	
	void statement() throws LexicalException, SyntaxException {
		switch(t.getKind()) {
			case KW_LET -> {
				consume();
				namedef();
				if(isKind(t, IPLPToken.Kind.ASSIGN)){
					  consume();
					  expression();
			    }
				match(IPLPToken.Kind.KW_DO);
				block();
				match(IPLPToken.Kind.KW_END);
			}
			case KW_SWITCH -> {
				consume();
				expression();
				while (isKind(t,IPLPToken.Kind.KW_CASE) || isKind(t,IPLPToken.Kind.COLON)) { //FIX KLEENE STAR!!!
					consume();
					expression();
					match(IPLPToken.Kind.COLON);
					block();
				}
				match(IPLPToken.Kind.KW_DEFAULT);
				block();
				match(IPLPToken.Kind.KW_END);
			}
			case KW_IF -> {
				consume();
				expression();
				match(IPLPToken.Kind.KW_DO);
				block();
				match(IPLPToken.Kind.KW_END);
			}
			case KW_WHILE -> {
				consume();
				expression();
				match(IPLPToken.Kind.KW_DO);
				block();
				match(IPLPToken.Kind.KW_END);
			}
			case KW_RETURN -> {
				consume();
				expression();
				match(IPLPToken.Kind.SEMI);
			}
			default -> {
				
				if(isExpStart()) {
					expression();
					if(isKind(t, IPLPToken.Kind.ASSIGN)) {
						consume();
						expression();
						match(IPLPToken.Kind.SEMI);
					}
					else if(isKind(t, IPLPToken.Kind.SEMI)) {
						consume();
					}
					else {
						throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
					}
				}
				
				else {
					throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
				}	
			}
		}
	}
	
	Expression__ expression() throws LexicalException, SyntaxException {
		logicalExpression();
	}
	
	Expression__ logicalExpression() throws LexicalException, SyntaxException {
		comparisonExpression();
		while (isKind(t,IPLPToken.Kind.AND) || isKind(t,IPLPToken.Kind.OR)) {
			consume();
			comparisonExpression();
		}
	}
	
	Expression__ comparisonExpression() throws LexicalException, SyntaxException {
		additiveExpression();
		while (isKind(t,IPLPToken.Kind.GT) || isKind(t,IPLPToken.Kind.LT) || isKind(t,IPLPToken.Kind.EQUALS) || isKind(t,IPLPToken.Kind.NOT_EQUALS)) {
			consume();
			additiveExpression();
		}
	}
	
	Expression__ additiveExpression() throws LexicalException, SyntaxException {
		multExpression();
		while (isKind(t,IPLPToken.Kind.PLUS) || isKind(t,IPLPToken.Kind.MINUS)) {
			consume();
			multExpression();
		}
	}
	
	Expression__ multExpression() throws LexicalException, SyntaxException {
		unaryExpression();
		while (isKind(t,IPLPToken.Kind.TIMES) || isKind(t,IPLPToken.Kind.DIV)) {
			consume();
			unaryExpression();
		}
	}
	
	Expression__ unaryExpression() throws LexicalException, SyntaxException {
		Expression__ e = null;
		e = primaryExpression();
		switch(t.getKind()) {
			case BANG -> {
				consume();
				return new UnaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e, Token.Kind.BANG);
			}
			case MINUS -> {
				consume();
				return new UnaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e, Token.Kind.MINUS);
			}
			default -> {
				return new UnaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e, null);
			}
		}
	}
	
	Expression__ primaryExpression() throws LexicalException, SyntaxException {
		Expression__ e = null;
		switch(t.getKind()) {
			case KW_NIL -> {
				consume();
				return new NilConstantExpression__(t.getLine(), t.getCharPositionInLine(), t.getText());
			}
			case KW_TRUE -> {
				consume();
				return new BooleanLiteralExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), true);
			}
			case KW_FALSE -> {
				consume();
				return new BooleanLiteralExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), false);
			}
			case INT_LITERAL -> {
				consume();
				return new IntLiteralExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), t.getIntValue());
			}
			case STRING_LITERAL -> {
				consume();
				return new StringLiteralExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), t.getStringValue());
			}
			case LPAREN -> {
				consume();
				e =  expression();
				match(IPLPToken.Kind.RPAREN);
				return e;
			}
			case IDENTIFIER -> {
				consume();
				Identifier__ expIdent = new Identifier__(t.getLine(), t.getCharPositionInLine(), t.getText(), t.getText());
				switch(t.getKind()) {
					case LPAREN -> {
						consume();
						if(isKind(t, IPLPToken.Kind.RPAREN)) {
							match(IPLPToken.Kind.RPAREN);
							return new FunctionCallExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), expIdent, null);
						}
						else {
							List<IExpression> args;
							args = new ArrayList<>();
							e = expression();
							args.add(e);
							while(isKind(t, IPLPToken.Kind.COMMA)) {
								match(IPLPToken.Kind.COMMA);
								e = expression();
								args.add(e);
							}
							match(IPLPToken.Kind.RPAREN);
							return new FunctionCallExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), expIdent, args);
						}
						
					}
					case LSQUARE -> {
						consume();
						e = expression();
						match(IPLPToken.Kind.RSQUARE);
						return new ListSelectorExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), expIdent, e);
					}
					default -> {
						return new IdentExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), expIdent);
					}
				}
			}
			default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
			
		}
	}
	
	Type__ type() throws LexicalException, SyntaxException {
		switch(t.getKind()) {
			case KW_INT -> {
				consume();
				return new PrimitiveType__(t.getLine(), t.getCharPositionInLine(), t.getText(), Type__.TypeKind.INT);
			}
			case KW_STRING -> {
				consume();
				return new PrimitiveType__(t.getLine(), t.getCharPositionInLine(), t.getText(), Type__.TypeKind.STRING);
			}
			case KW_BOOLEAN -> {
				consume();
				return new PrimitiveType__(t.getLine(), t.getCharPositionInLine(), t.getText(), Type__.TypeKind.BOOLEAN);
			}
			case KW_LIST -> {
				consume();
				Type__ element = null;
				switch(t.getKind()) {
					case LSQUARE -> {
						consume();
						if(isKind(t, IPLPToken.Kind.RSQUARE)) {
							consume();
						}
						else {
							element = type();
							match(IPLPToken.Kind.RSQUARE);
						}
						return new ListType__(t.getLine(), t.getCharPositionInLine(), t.getText(), element);
						
					}
					default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
				}
			}
			default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
		}
	}

	@Override
	public void parse() throws Exception {
		program();
		return;
	}

}
