package edu.ufl.cise.plpfa21.assignment1;

public class Token implements IPLPToken {
	Kind token_kind;
	int token_pos;
	int token_length;
	int token_line;
	int token_posInLine;
	int num;
	int flag;
	int len;
	int i;
	String strValue;
	String token_text;
	String errorMessage;
	Token next;
	
	public Token (Kind kind, int pos, int length, int line, int posInLine, String text){
		flag = 0;
		i = 0;
		token_kind = kind;
		token_pos = pos;
		token_length = length;
		token_line = line;
		token_posInLine = posInLine;
		token_text = text;
		strValue = text;
		errorMessage = "";
		next = null;
	}
	
	@Override
	public Kind getKind() {
		return token_kind;
	}


	@Override
	public String getText() {
		return token_text;
	}


	@Override
	public int getLine() {
		return token_line;
	}


	@Override
	public int getCharPositionInLine() {
		return token_posInLine;
	}


	@Override
	public String getStringValue() {
		len = strValue.length();
		if(strValue.charAt(0) == '\"')
			strValue = strValue.replaceAll("\"", "");
		else if(strValue.charAt(0) == '\'')
			strValue = strValue.replaceAll("\'", "");

		strValue = strValue.replaceAll("\\\\\"", "\"");
		strValue = strValue.replaceAll("\\\\\'", "\'");
		strValue = strValue.replaceAll("\\\\n", "\n"); 
		strValue = strValue.replaceAll("\\\\b", "\b");
		strValue = strValue.replaceAll("\\\\t", "\t");
		strValue = strValue.replaceAll("\\\\r", "\r");
		strValue = strValue.replaceAll("\\\\f", "\f");
		strValue = strValue.replaceAll("\\\\", "\\"); 
		return strValue;
	}


	@Override
	public int getIntValue() {
		num = Integer.parseInt(token_text);
		return num;
	
	}
}
