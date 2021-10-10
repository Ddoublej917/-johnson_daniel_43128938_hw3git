package edu.ufl.cise.plpfa21.assignment1;

public class Lexer implements IPLPLexer {
	char[] chars; //holds characters with 0 at end
	char EOFchar = 0;
	int startPos = 0;
	int startPosInLine = 0;
	int startLine = 1;
	int pos = 0; // position in char array. Starts at zero
	int line = 1; // line number of token in source. Starts at 1
	int posInLine = 0; // position in line of source. Starts at 1
	String txt = ""; //temporary string to hold text of tokens
	int numChars;
	Token head;
	Token current;
	String lexerInput;
	boolean errorflag;
	
	public Lexer (String input){
		head = null;
		current = null;
		EOFchar = 0;
		startPos = 0;
		startPosInLine = 0;
		startLine = 1;
		pos = 0; // position in char array. Starts at zero
		line = 1; // line number of token in source. Starts at 1
		posInLine = 0; // position in line of source. Starts at 1
		txt = "";
		lexerInput = input;
		numChars = lexerInput.length();
		chars = new char[numChars + 1]; 
		System.arraycopy(input.toCharArray(), 0, chars, 0, numChars);
		chars[numChars] = EOFchar;
		errorflag = false;
		lex(input);
	}
	
	public void add (Token t) {
		Token newToken  = new Token(t.token_kind, t.token_pos, t.token_length, t.token_line, t.token_posInLine, t.token_text);
		if(head == null) {
			head = newToken;
			current = head;
			return;
		}
		else {
			current = head;
			while(current.next != null) {
				current = current.next;
			}
				
			current.next = newToken;
		}
		
	}

	@Override
	public IPLPToken nextToken() throws LexicalException {
		current = current.next;
		if (current.token_kind == Token.Kind.ERROR) {
			throw new LexicalException(current.errorMessage, current.token_line,current.token_posInLine);
		}
		else {
			return current;
		}
	}
	
	public void lex(String input) {
	
		this.head = new Token(Token.Kind.SEMI, 999, 999, 999, 999, "DUMMYHEAD");
		enum State {START, HAVE_EQUAL, DIGITS, IDENT_PART, AND, OR, STRING_LITERAL, COMMENT}
		
		State state = State.START;
		
		while(pos < chars.length) {
			char ch = chars[pos]; //get current char
			switch (state) {
				case START-> {
					startLine = line;
					startPos = pos;
					startPosInLine = posInLine;
					errorflag = false;
					switch (ch) {
						case ' ', '\t'-> {
							pos++;
							posInLine++;
						}
						case '\n'-> {
							pos++;
							line++;
							posInLine = 0;
						}
						case '\r'-> {
							pos++;
							posInLine = 0;
						}
						case '('-> {
							txt = "(";
							this.add(new Token(Token.Kind.LPAREN, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case ')'-> {
							txt = ")";
							this.add(new Token(Token.Kind.RPAREN, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '['-> {
							txt = "[";
							this.add(new Token(Token.Kind.LSQUARE, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case ']'-> {
							txt = "]";
							this.add(new Token(Token.Kind.RSQUARE, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '+'-> {
							txt = "+";
							this.add(new Token(Token.Kind.PLUS, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '-'-> {
							txt = "-";
							this.add(new Token(Token.Kind.MINUS, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '/'-> {
							switch (chars[pos+1]) {
								case '*'-> {
									txt = txt + ch + chars[pos+1];
									pos+=2;
									posInLine+=2;
									state = State.COMMENT;
								}
								default -> {
									txt = "/";
									this.add(new Token(Token.Kind.DIV, startPos, 1, startLine, startPosInLine, txt));
									this.current = this.current.next;
									txt = "";
									pos++;
									posInLine++;
								}
							}
							
						}
						case '*'-> {
							txt = "*";
							this.add(new Token(Token.Kind.TIMES, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '<'-> {
							txt = "<";
							this.add(new Token(Token.Kind.LT, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '>'-> {
							txt = ">";
							this.add(new Token(Token.Kind.GT, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case ':'-> {
							txt = ":";
							this.add(new Token(Token.Kind.COLON, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case ';'-> {
							txt = ";";
							this.add(new Token(Token.Kind.SEMI, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case ','-> {
							txt = ",";
							this.add(new Token(Token.Kind.COMMA, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case '&'-> {
							txt = "&";
							pos++;
							posInLine++;
							state = State.AND;
						}
						case '|'-> {
							txt = "|";
							pos++;
							posInLine++;
							state = State.OR;
						}
						case '0'-> {
							txt = "0";
							this.add(new Token(Token.Kind.INT_LITERAL, startPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							pos++;
							posInLine++;
						}
						case'=' -> {
							txt = "=";
							pos++;  
							posInLine++;  
							state = State.HAVE_EQUAL;
						}
						case'!' -> {
							if(chars[pos+1] == '=') {
								txt = "!";
								pos++;  
								posInLine++;  
								state = State.HAVE_EQUAL;
							}
							
							else {
								txt = "!";
								this.add(new Token(Token.Kind.BANG, startPos, 1, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								pos++;
								posInLine++;
							}
						}
						case '\"' -> {
							txt = txt + ch;
							pos++;
							posInLine++;
							state = State.STRING_LITERAL;
						}
						case '\'' -> {
							txt = txt + ch;
							pos++;
							posInLine++;
							state = State.STRING_LITERAL;
						}
						case'1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
							txt = "" + ch;
							pos++;
							posInLine++;
							state = State.DIGITS;
						}
						default -> {
							if (Character.isJavaIdentifierStart(ch)) {
								txt = txt + ch;
								pos++;
								posInLine++;
								state = State.IDENT_PART;
								
							}
							else {
								if(ch != EOFchar) {
									txt = "" + ch;
									this.add(new Token(Token.Kind.ERROR, startPos, 1, startLine, startPosInLine, txt));
									this.current = this.current.next;
									this.current.errorMessage = ch + " is an unrecognized character for this langauge";
								}
								pos++;
							}
						}
					}	
				}
				case HAVE_EQUAL-> {
					int equalsPos = pos;
					switch (ch) {
						case '=' -> {
							if(txt.equals("!")) {
								txt = "!=";
								this.add(new Token(Token.Kind.NOT_EQUALS, equalsPos, 2, startLine, startPosInLine, txt));
								this.current = this.current.next;
								pos++;
								posInLine++;
								txt = "";
								state = State.START;
							}
							else if(txt.equals("=")) {
								txt = "==";
								this.add(new Token(Token.Kind.EQUALS, equalsPos, 2, startLine, startPosInLine, txt));
								this.current = this.current.next;
								pos++;
								posInLine++;
								txt = "";
								state = State.START;
							}
						}
						default -> {
							txt = "=";
							this.add(new Token(Token.Kind.ASSIGN, equalsPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							txt = "";
							state = State.START;
							
						}
					}
					
					
				}
				
				case COMMENT-> {
					switch (ch) {
						case '*'-> {
							if(chars[pos+1] == '/') {
								pos+=2;
								posInLine+=2;
								txt="";
								state = State.START;
							}
							else {
								pos++;
								posInLine++;
							}
						}
						case '\n'-> {
							pos++;
							line++;
							posInLine = 0;
						}
						case '\r'-> {
							pos++;
							posInLine = 0;
						}
						default -> {
							pos++;
							posInLine++;
						}
					}
				}
				/**abd*/
				
				case STRING_LITERAL-> {
					switch (ch) {
						case '\n'-> {
							txt = txt + ch;
							pos++;
							line++;
							posInLine = 0;
						}
						case '\r'-> {
							txt = txt + ch;
							pos++;
							posInLine = 0;
						}
						case ' ', '\t'-> {
							txt = txt + ch;
							pos++;
							posInLine++;
						}
						case '\\' -> {
							if (chars[pos+1] == 'b' || chars[pos+1] == 't' || chars[pos+1] == 'r' || chars[pos+1] == 'f' || chars[pos+1] == '\'' || chars[pos+1] == '\\') {
								txt = txt + ch + chars[pos+1];
								pos+=2;
								posInLine+=2;
							}
							else if(chars[pos+1] == 'n') {
								txt = txt + ch + chars[pos+1];
								pos+=2;
								line++;
								posInLine = 0;
							}
							else {
								txt = txt + ch + chars[pos+1];
								pos+=2;
								posInLine+=2;
								errorflag = true;
							}
							
						}
						case '\'' -> {
							txt = txt + ch;
							if(txt.equals("\'\'")) {
								this.add(new Token(Token.Kind.ERROR, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								this.current.errorMessage = "Empty string literal (\'\')";
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else if(errorflag == true) {
								this.add(new Token(Token.Kind.ERROR, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								this.current.errorMessage = "Invalid string literal including an invalid escape sequence";
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else if(txt.charAt(0) == '\'')  {
								this.add(new Token(Token.Kind.STRING_LITERAL, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else {
								pos++;
								posInLine++;
							}
						}
						case '\"' -> {
							txt = txt + ch;
							if(txt.equals("\"\"")) {
								this.add(new Token(Token.Kind.ERROR, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								this.current.errorMessage = "Empty string literal (\"\")";
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else if(errorflag == true) {
								this.add(new Token(Token.Kind.ERROR, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								this.current.errorMessage = "Invalid string literal including an invalid escape sequence";
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else if(txt.charAt(0) == '\"')  {
								this.add(new Token(Token.Kind.STRING_LITERAL, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								pos++;
								posInLine++;
								state = State.START;
							}
							else {
								pos++;
								posInLine++;
							}
							
						}
						default -> {
							txt = txt + ch;
							pos++;
							posInLine++;
						}
					}
				}
				
				case AND-> {
					int andPos = pos;
					switch (ch) {
						case '&' -> {
							txt = "&&";
							this.add(new Token(Token.Kind.AND, andPos, 2, startLine, startPosInLine, txt));
							this.current = this.current.next;
							pos++;
							posInLine++;
							txt = "";
							state = State.START;
						}
						default -> {
							txt = "" + ch;
							this.add(new Token(Token.Kind.ERROR, andPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							this.current.errorMessage = ch + " is an unrecognized character for this langauge";
							txt = "";
							state = State.START;
							
						}
					}
				}
				
				case OR-> {
					int orPos = pos;
					switch (ch) {
						case '|' -> {
							txt = "||";
							this.add(new Token(Token.Kind.OR, orPos, 2, startLine, startPosInLine, txt));
							this.current = this.current.next;
							pos++;
							posInLine++;
							txt = "";
							state = State.START;
						}
						default -> {
							txt = "" + ch;
							this.add(new Token(Token.Kind.ERROR, orPos, 1, startLine, startPosInLine, txt));
							this.current = this.current.next;
							this.current.errorMessage = ch + " is an unrecognized character for this langauge";
							txt = "";
							state = State.START;
							
						}
					}
				}
	
				case DIGITS-> {
					switch (ch) {
						case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> {
							txt = txt + ch;
							pos++;
							posInLine++;
						}
						default -> {
							try {
							    Integer.parseInt(txt);
							    this.add(new Token(Token.Kind.INT_LITERAL, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							catch (NumberFormatException e) {
								this.add(new Token(Token.Kind.ERROR, startPos, pos - startPos, startLine, startPosInLine, txt));
								txt = "";
								this.current = this.current.next;
								this.current.errorMessage = ch + " is an unrecognized character for this langauge";
								state = State.START;
							}
							 		
						}
					}
				}
					
				case IDENT_PART-> {
					switch (ch) {
						case 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '$', '_' -> {
							txt = txt + ch;						
							pos++;
							posInLine++;
	
						}
						
						default -> {
							if(txt.equals("VAR")) {
								this.add(new Token(Token.Kind.KW_VAR, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("VAL")) {
								this.add(new Token(Token.Kind.KW_VAL, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("FUN")) {
								this.add(new Token(Token.Kind.KW_FUN, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("DO")) {
								this.add(new Token(Token.Kind.KW_DO, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("END")) {
								this.add(new Token(Token.Kind.KW_END, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("LET")) {
								this.add(new Token(Token.Kind.KW_LET, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("SWITCH")) {
								this.add(new Token(Token.Kind.KW_SWITCH, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("CASE")) {
								this.add(new Token(Token.Kind.KW_CASE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("DEFAULT")) {
								this.add(new Token(Token.Kind.KW_DEFAULT, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("IF")) {
								this.add(new Token(Token.Kind.KW_IF, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("ELSE")) {
								this.add(new Token(Token.Kind.KW_ELSE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("WHILE")) {
								this.add(new Token(Token.Kind.KW_WHILE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("WHILE")) {
								this.add(new Token(Token.Kind.KW_WHILE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("RETURN")) {
								this.add(new Token(Token.Kind.KW_RETURN, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("NIL")) {
								this.add(new Token(Token.Kind.KW_NIL, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("TRUE")) {
								this.add(new Token(Token.Kind.KW_TRUE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("FALSE")) {
								this.add(new Token(Token.Kind.KW_FALSE, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("INT")) {
								this.add(new Token(Token.Kind.KW_INT, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("STRING")) {
								this.add(new Token(Token.Kind.KW_STRING, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("BOOLEAN")) {
								this.add(new Token(Token.Kind.KW_BOOLEAN, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else if(txt.equals("LIST")) {
								this.add(new Token(Token.Kind.KW_LIST, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							else {
								this.add(new Token(Token.Kind.IDENTIFIER, startPos, pos - startPos, startLine, startPosInLine, txt));
								this.current = this.current.next;
								txt = "";
								state = State.START; 
							}
							
							//DO NOT INCREMENT pos  
							
						}
					}
				}
			}
		}
		
		//add EOF token
		this.add(new Token(Token.Kind.EOF, pos, 0, startLine, startPosInLine, ""));
		this.current = this.head;

	}
}
