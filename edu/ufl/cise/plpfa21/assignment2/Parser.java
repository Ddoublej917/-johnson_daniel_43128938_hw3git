package edu.ufl.cise.plpfa21.assignment2;

import java.util.ArrayList;
import java.util.List;

import edu.ufl.cise.plpfa21.assignment1.IPLPLexer;
import edu.ufl.cise.plpfa21.assignment1.IPLPToken;
import edu.ufl.cise.plpfa21.assignment1.LexicalException;
import edu.ufl.cise.plpfa21.assignment1.Token;
import edu.ufl.cise.plpfa21.assignment1.PLPTokenKinds.Kind;
import edu.ufl.cise.plpfa21.assignment3.ast.IASTNode;
import edu.ufl.cise.plpfa21.assignment3.ast.IBlock;
import edu.ufl.cise.plpfa21.assignment3.ast.IDeclaration;
import edu.ufl.cise.plpfa21.assignment3.ast.IExpression;
import edu.ufl.cise.plpfa21.assignment3.ast.IIdentifier;
import edu.ufl.cise.plpfa21.assignment3.ast.INameDef;
import edu.ufl.cise.plpfa21.assignment3.ast.IStatement;
import edu.ufl.cise.plpfa21.assignment3.ast.IType;
import edu.ufl.cise.plpfa21.assignment3.ast.IType.TypeKind;
import edu.ufl.cise.plpfa21.assignment3.astimpl.AssignmentStatement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.BinaryExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Block__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.BooleanLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Declaration__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Expression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.FunctionCallExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.FunctionDeclaration__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.IdentExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Identifier__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.IfStatement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ImmutableGlobal__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.IntLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.LetStatement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ListSelectorExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ListType__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.MutableGlobal__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.NameDef__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.NilConstantExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.PrimitiveType__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Program__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.ReturnStatement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Statement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.StringLiteralExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.SwitchStatement__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.Type__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.UnaryExpression__;
import edu.ufl.cise.plpfa21.assignment3.astimpl.WhileStatement__;

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
	
	Program__ program() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		List<IDeclaration> programDeclarations;
		programDeclarations = new ArrayList<>();
		while (isKind(t,IPLPToken.Kind.KW_FUN) || isKind(t,IPLPToken.Kind.KW_VAL) || isKind(t,IPLPToken.Kind.KW_VAR)) {
			programDeclarations.add(declaration());
		}
		if(!isKind(t,IPLPToken.Kind.EOF)) {
	    	   throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
	    }
		else {
			return new Program__(start.getLine(), start.getCharPositionInLine(), start.getText(), programDeclarations);
		}
	}
	
	Declaration__ declaration() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		switch(t.getKind()) {
			case KW_FUN -> {
				return function();
			}
			case KW_VAR -> {
				NameDef__ varNameDef = null;
				Expression__ varExp = null;
				consume();
				varNameDef = namedef();
				if(isKind(t, IPLPToken.Kind.ASSIGN)) {
					consume();
					varExp = expression();
				}
				match(IPLPToken.Kind.SEMI);
				return new MutableGlobal__(start.getLine(), start.getCharPositionInLine(), start.getText(), varNameDef, varExp);
			}
			case KW_VAL -> {
				NameDef__ valNameDef = null;
				Expression__ valExp = null;
				consume();
				valNameDef = namedef();
				match(IPLPToken.Kind.ASSIGN);
				valExp = expression();
				match(IPLPToken.Kind.SEMI);
				return new ImmutableGlobal__(start.getLine(), start.getCharPositionInLine(), start.getText(), valNameDef, valExp);
			}
		
			default -> {
				throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
			}
		}
	}
	
	FunctionDeclaration__ function() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Identifier__ name = null;
		Type__ funType = null;
		Block__ funBlock = null;
		List<INameDef> funNameDefs;
		funNameDefs = new ArrayList<>();
		match(IPLPToken.Kind.KW_FUN);
		if(isKind(t, IPLPToken.Kind.IDENTIFIER)) {
			name = new Identifier__(t.getLine(), t.getCharPositionInLine(), t.getText(), t.getText());
			consume();
		}
		
		match(IPLPToken.Kind.LPAREN);
		if(isKind(t, IPLPToken.Kind.RPAREN)) {
			consume();
			if(isKind(t, IPLPToken.Kind.COLON)) {
				consume();
				funType = type();
			}
			match(IPLPToken.Kind.KW_DO);
			funBlock = block();
			match(IPLPToken.Kind.KW_END);
			return new FunctionDeclaration__(start.getLine(), start.getCharPositionInLine(), start.getText(), name, funNameDefs, funType, funBlock);
		}
		else {
			funNameDefs.add(namedef());
			while(isKind(t, IPLPToken.Kind.COMMA)) {
				consume();
				funNameDefs.add(namedef());
			}
			if(isKind(t, IPLPToken.Kind.RPAREN)) {
				consume();
				if(isKind(t, IPLPToken.Kind.COLON)) {
					consume();
					funType = type();
				}
				match(IPLPToken.Kind.KW_DO);
				funBlock = block();
				match(IPLPToken.Kind.KW_END);
				return new FunctionDeclaration__(start.getLine(), start.getCharPositionInLine(), start.getText(), name, funNameDefs, funType, funBlock);
			}
			else {
				throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
			}
		}	
	}
	
	Block__ block() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		List<IStatement> blockStatements;
		blockStatements = new ArrayList<>();
		Statement__ s = null;
		while (isKind(t,IPLPToken.Kind.KW_LET) || isKind(t,IPLPToken.Kind.KW_SWITCH) || isKind(t,IPLPToken.Kind.KW_IF) || isKind(t,IPLPToken.Kind.KW_WHILE) || isKind(t,IPLPToken.Kind.KW_RETURN) || isExpStart() == true) {
			s = statement();
			blockStatements.add(s);
		}
		return new Block__(start.getLine(), start.getCharPositionInLine(), start.getText(), blockStatements);
	}
	
	NameDef__ namedef() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		if(isKind(t, IPLPToken.Kind.IDENTIFIER)) {
			Identifier__ nameIdent = new Identifier__(t.getLine(), t.getCharPositionInLine(), t.getText(), t.getText());
			consume();
			if(isKind(t, IPLPToken.Kind.COLON)) {
				consume();
				Type__ nameType = null;
				nameType = type();
				return new NameDef__(start.getLine(), start.getCharPositionInLine(), start.getText(), nameIdent, nameType);
			}
			return new NameDef__(start.getLine(), start.getCharPositionInLine(), start.getText(), nameIdent, null);
		}
		else {
			throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
		}
	}
	
	Statement__ statement() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		switch(t.getKind()) {
			case KW_LET -> {
				NameDef__ letNameDef = null;
				Block__ letBlock = null;
				Expression__ letExp = null;
				consume();
				letNameDef = namedef();
				if(isKind(t, IPLPToken.Kind.ASSIGN)){
					consume();
					letExp = expression();
			    }
				match(IPLPToken.Kind.KW_DO);
				letBlock = block();
				match(IPLPToken.Kind.KW_END);
				return new LetStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), letBlock, letExp, letNameDef);
			}
			case KW_SWITCH -> {
				Expression__ switchExp = null;
				Block__ defaultBlock = null;
				List<IExpression> branchExp;
				List<IBlock> branchBlock;
				branchExp = new ArrayList<>();
				branchBlock = new ArrayList<>();
				consume();
				switchExp = expression();
				while (isKind(t,IPLPToken.Kind.KW_CASE) || isKind(t,IPLPToken.Kind.COLON)) { //FIX KLEENE STAR!!!
					consume();
					branchExp.add(expression());
					match(IPLPToken.Kind.COLON);
					branchBlock.add(block());
				}
				match(IPLPToken.Kind.KW_DEFAULT);
				defaultBlock = block();
				match(IPLPToken.Kind.KW_END);
				return new SwitchStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), switchExp, branchExp, branchBlock, defaultBlock);
			}
			case KW_IF -> {
				Expression__ ifExp = null;
				Block__ ifBlock = null;
				consume();
				ifExp = expression();
				match(IPLPToken.Kind.KW_DO);
				ifBlock = block();
				match(IPLPToken.Kind.KW_END);
				return new IfStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), ifExp, ifBlock);
			}
			case KW_WHILE -> {
				Expression__ whileExp = null;
				Block__ whileBlock = null;
				consume();
				whileExp = expression();
				match(IPLPToken.Kind.KW_DO);
				whileBlock = block();
				match(IPLPToken.Kind.KW_END);
				return new WhileStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), whileExp, whileBlock);
			}
			case KW_RETURN -> {
				Expression__ returnExp = null;
				consume();
				returnExp = expression();
				match(IPLPToken.Kind.SEMI);
				return new ReturnStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), returnExp);
			}
			default -> {
				
				if(isExpStart()) {
					Expression__ leftExp = null;
					Expression__ rightExp = null;
					leftExp = expression();
					if(isKind(t, IPLPToken.Kind.ASSIGN)) {
						consume();
						rightExp = expression();
						match(IPLPToken.Kind.SEMI);
						return new AssignmentStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), leftExp, rightExp);
					}
					else if(isKind(t, IPLPToken.Kind.SEMI)) {
						consume();
						return new AssignmentStatement__(start.getLine(), start.getCharPositionInLine(), start.getText(), leftExp, rightExp);
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
		return logicalExpression();
	}
	
	Expression__ logicalExpression() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Expression__ e1 = null;
		Expression__ e2 = null;
		e1 = comparisonExpression();
		while (isKind(t,IPLPToken.Kind.AND) || isKind(t,IPLPToken.Kind.OR)) {
			Token.Kind op = t.getKind();
			consume();
			e2 = comparisonExpression();
			e1 = new BinaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e1, e2, op);
		}
		return e1;
	}
	
	Expression__ comparisonExpression() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Expression__ e1 = null;
		Expression__ e2 = null;
		e1 = additiveExpression();
		while (isKind(t,IPLPToken.Kind.GT) || isKind(t,IPLPToken.Kind.LT) || isKind(t,IPLPToken.Kind.EQUALS) || isKind(t,IPLPToken.Kind.NOT_EQUALS)) {
			Token.Kind op = t.getKind();
			consume();
			e2 = additiveExpression();
			e1 = new BinaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e1, e2, op);
		}
		return e1;
	}
	
	Expression__ additiveExpression() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Expression__ e1 = null;
		Expression__ e2 = null;
		e1 = multExpression();
		while (isKind(t,IPLPToken.Kind.PLUS) || isKind(t,IPLPToken.Kind.MINUS)) {
			Token.Kind op = t.getKind();
			consume();
			e2 = multExpression();
			e1 = new BinaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e1, e2, op);
		}
		return e1;
	}
	
	Expression__ multExpression() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Expression__ e1 = null;
		Expression__ e2 = null;
		e1 = unaryExpression();
		while (isKind(t,IPLPToken.Kind.TIMES) || isKind(t,IPLPToken.Kind.DIV)) {
			Token.Kind op = t.getKind();
			consume();
			e2 = unaryExpression();
			e1 = new BinaryExpression__(t.getLine(), t.getCharPositionInLine(), t.getText(), e1, e2, op);
		}
		return e1;
	}
	
	Expression__ unaryExpression() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		Expression__ e = null;
		e = primaryExpression();
		switch(t.getKind()) {
			case BANG -> {
				consume();
				return new UnaryExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), e, Token.Kind.BANG);
			}
			case MINUS -> {
				consume();
				return new UnaryExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), e, Token.Kind.MINUS);
			}
			default -> {
				return e;
			}
		}
	}
	
	Expression__ primaryExpression() throws LexicalException, SyntaxException {
		Expression__ e = null;
		IPLPToken start = t;
		switch(t.getKind()) {
			case KW_NIL -> {
				consume();
				return new NilConstantExpression__(start.getLine(), start.getCharPositionInLine(), start.getText());
			}
			case KW_TRUE -> {
				consume();
				return new BooleanLiteralExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), true);
			}
			case KW_FALSE -> {
				consume();
				return new BooleanLiteralExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), false);
			}
			case INT_LITERAL -> {
				consume();
				return new IntLiteralExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), start.getIntValue());
			}
			case STRING_LITERAL -> {
				consume();
				return new StringLiteralExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), start.getStringValue());
			}
			case LPAREN -> {
				consume();
				e =  expression();
				match(IPLPToken.Kind.RPAREN);
				return e;
			}
			case IDENTIFIER -> {
				consume();
				Identifier__ expIdent = new Identifier__(start.getLine(), start.getCharPositionInLine(), start.getText(), start.getText());
				switch(t.getKind()) {
					case LPAREN -> {
						consume();
						if(isKind(t, IPLPToken.Kind.RPAREN)) {
							match(IPLPToken.Kind.RPAREN);
							return new FunctionCallExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), expIdent, null);
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
							return new FunctionCallExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), expIdent, args);
						}
						
					}
					case LSQUARE -> {
						consume();
						e = expression();
						match(IPLPToken.Kind.RSQUARE);
						return new ListSelectorExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), expIdent, e);
					}
					default -> {
						return new IdentExpression__(start.getLine(), start.getCharPositionInLine(), start.getText(), expIdent);
					}
				}
			}
			default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
			
		}
	}
	
	Type__ type() throws LexicalException, SyntaxException {
		IPLPToken start = t;
		switch(t.getKind()) {
			case KW_INT -> {
				consume();
				return new PrimitiveType__(start.getLine(), start.getCharPositionInLine(), start.getText(), Type__.TypeKind.INT);
			}
			case KW_STRING -> {
				consume();
				return new PrimitiveType__(start.getLine(), start.getCharPositionInLine(), start.getText(), Type__.TypeKind.STRING);
			}
			case KW_BOOLEAN -> {
				consume();
				return new PrimitiveType__(start.getLine(), start.getCharPositionInLine(), start.getText(), Type__.TypeKind.BOOLEAN);
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
						return new ListType__(start.getLine(), start.getCharPositionInLine(), start.getText(), element);
						
					}
					default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
				}
			}
			default -> throw new SyntaxException(t.getText(), t.getLine(), t.getCharPositionInLine());
		}
	}

	@Override
	public IASTNode parse() throws Exception {
		return program();
	}

}
