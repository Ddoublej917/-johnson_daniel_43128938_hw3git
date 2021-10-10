package edu.ufl.cise.plpfa21.assignment3.astimpl;

import edu.ufl.cise.plpfa21.assignment3.ast.IStatement;

public abstract class Statement__ extends ASTNode__ implements IStatement{

	public Statement__(int line, int posInLine, String text) {
		super(line, posInLine, text);
	}

}
