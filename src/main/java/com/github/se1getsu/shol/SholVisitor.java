package com.github.se1getsu.shol;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.io.PrintStream;
import com.github.se1getsu.shol.SholParserBaseVisitor;
import com.github.se1getsu.shol.SholParser;

public class SholVisitor extends SholParserBaseVisitor<String> {
    private PrintStream stream;

    public SholVisitor(PrintStream stream) {
        this.stream = stream;
    }

    @Override
    public String visitProgram(SholParser.ProgramContext context) {
        visitChildren(context);

        stream.println("Hello");

        return null;
    }
}
