package edu.ufl.cise.plpfa21.assignment3.ast;

public interface INameDef extends IASTNode {

	IIdentifier getIdent();
	IType getType();
	void setType(IType type);
}
