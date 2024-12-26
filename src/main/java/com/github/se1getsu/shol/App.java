package com.github.se1getsu.shol;
import com.github.se1getsu.shol.SholLexer;
import com.github.se1getsu.shol.SholParser;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class App {
    public static void main( String[] args ) {
        CharStream inputStream = CharStreams.fromString(
                "%print\nHello, world!"
        );
        SholLexer lexer = new SholLexer(inputStream);
        CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
        SholParser parser = new SholParser(commonTokenStream);

        SholParser.ProgramContext fileContext = parser.program();
        SholVisitor visitor = new SholVisitor(System.out);
        visitor.visit(fileContext);
    }
}
