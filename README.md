# An interpreter of Snail Programming Language

This is an exercise for Theory of Compiler. I approached it in traditional ways as implement an Lexer and Parser, and then Interpreter. This is really a tough work. Then I tried [FParsec](http://www.quanttec.com/fparsec/), it's an amazing library, make the whole world better.

## Traditional approach

As *Compilers: Principles, Techniques and Tools* shown, it consists of three piped parts.

- Lexer
- Parser
- Interpreter

The first two parts called Front-end, while the other part called Back-end.

### Implementation of Lexer

There are three steps to build a Lexer.

1. Use Regex to describe each token.
2. Convert Regex expressions to NFA(Nondeterministic Finite Automation).
3. Convert NFA to DFA(Deterministic Finite Automation).

As well-formed structures are easier to parse, you should consider about how to represent tokens. In F#, it is nature to use [Discriminated Unions](http://msdn.microsoft.com/en-us/library/dd233226.aspx).

### Implementation of Parser

There are several steps concluded by me to build a Parser by manual.

1. Eliminates the ambiguity.
2. Eliminates the left recursion of the Grammar. (Determine precedence and rewrite.)
3. Left factoring. (For example. T -> E + E | E - E | INT ==> T -> E T' | INT;; T' -> + E | - E)
4. Write a series of recursive functions to build a [Recursive-descent Parser](http://en.wikipedia.org/wiki/Recursive_descent_parser).

As we are working on tokens which is a kind of [Discriminated Unions](http://msdn.microsoft.com/en-us/library/dd233226.aspx), it is nature to use [Pattern Matching](http://msdn.microsoft.com/en-us/library/dd547125.aspx) to parse them.

**It would be better to realize both Lexer and Parser in [Pull Model](http://en.wikipedia.org/wiki/Push?Cpull_strategy), because it allow to read and process only a piece of source file instead of whole.** In F#, it is nature to use [Sequences](http://msdn.microsoft.com/en-us/library/dd233209.aspx), it would delay the process until it has to happen. There may be a need of token cache in Parser to support lookahead and backtracking.

### Implementation of Interpreter

This part is same as that in FParsec.

## FParsec approach

FParsec couldn't seperate Lexer and Parser, it combines this two process together.

FParsec has a build-in [OperatorPrecedenceParser](http://www.quanttec.com/fparsec/reference/operatorprecedenceparser.html) to process Expression. This is usually the most complex part in Grammar, because it contains Ambiguity, Left Recursion and Left Factor. OperatorPrecedenceParser could deal with it properly.

FParsec has a build-in [identifier parser](http://www.quanttec.com/fparsec/reference/charparsers.html#members.identifier) to process the complex indentifier.

There are many features to make all things easy.

### Build AST

I would prefer to build AST first, because it makes things clear. AST is the most abstract representation of the language.

There may be serveral ways to represent a same meaning.

	let binOp = Plus | Minus | Times | Div;;
	let expr =
		| BinOpExpr of binOp * expr * expr
		| UniOpExpr of uniOp * expr
		| ...

Or

	let expr =
		| PlusOpExpr of expr * expr
		| MinusOpExpr of expr * expr
		| ...

Just take the one you think better.

### Build Interpreter

It is easy to build an Interpreter with [Pattern Matching](http://msdn.microsoft.com/en-us/library/dd547125.aspx). Just build a series of recursive functions to interpret the language through AST down.

Notice that I use an global HashSet for variable environment. It is better to transfer an environment between interpreter functions.

### Build Parser

FParsec allow to build AST from CharStream directly.

Fparsec couldn't deal with left recursion properly. You should the first three steps mentioned in [Implementation of Parser](#implementation-of-parser) also. The good news is it is much easier to deal with lookahead and backtracking in FParsec.

## Specification of Snail Programming Language

This is just an exercise. It's not a really language.

Design an Interpreter for Snail Programming Language. Snail consists of a series of statements, the interpreter should execute them in order. [Section 1](#section-1) describes the statements, while [Section 2](#section-2) describes the Grammar.

### Section 1

Snail is a very simple programming language. The body of a Snail program consists of a sequence of statements. There are three kinds of statements: assign, print, and if. A basic component for all kinds of statements is the expression. The expression and the statements are described below.

- expression

	An expression is any mathematical expression made from identifiers, integers, parenthesis, the arithmetic operators

		+ - * /

	and the comparison operators

		< > <= >= == !=

	For example, this is a valid expression:

		10 + 20 * (10 < 3)

	The value of an expression is obtained by executing all the arithmetic operations in the expression. The result of a comparison operation is 1 if the comparison result is true, and 0 otherwise. For example, the above expression has value 10 (since, 10 < 3 = 0). The value of an identifier is the last value assigned to it in an assign statement. An identifier which hasn??t been assigned a value before cannot be used inside an expression and in this case you should report an error message.

- assign statement

	The assign statement has the form:

		identifier = expression;

	For example, this is a valid assign statement:

		var1 = 20 - 3*2;

	In the assign statement the identifier gets the value of the expression. As an example, in the above assign statement the new value of variable var1 is 14.

- print statement

	The print statement print messages on the screen. The print statement has one of following forms:

		print "string"; //prints the string
		print newline; //prints a newline character
		print expression; //prints the expression value

	For example, the execution of the following statements

		print "The value of 10*5 is ";
		print 10*5;

	produces the output:

		The value of 10*5 is 50

- if statement

	An if statement has two forms:

		if expression then //if-then statement
		statement
		statement
		... //more statements
		endif
		if expression then //if-then-else statement
		statement
		... //more statements
		else
		statement
		... //more statements
		endif

	The "if-then" statement means that if the expression value is not 0 then the sequence of statements between then and endif will be executed. The "if-then-else" statement means that if the expression value is not 0 then the statements between then and else will be executed, and otherwise, if the expression value is 0, the statements between else and endif will be executed. For example, the following is a valid if statement:

		if (x < 10) then
		print "x is smaller than ten";
		x = x - y + 20;
		else
		x = 10 * y;
		endif

A Snail program is a sequence of statements and has the following general form:

	statement
	statement
	...
	statement

We can have comments in a Snail program right after "//" (as in a C++ program). An simple example Snail program is the following:

	x = 10;
	y = 20;
	print "the value of x is "; print x; print newline;
	print "the value of y is "; print y; print newline;
	print "the sum of x and y is "; print x + y; print newline;
	if x < y then //test x < y
	print "x is smaller than y"; // x is smaller
	else
	print "x is bigger or equal than y" // x is not smaller
	endif

The output of the program is:

	the value of x is 10
	the value of y is 20
	the sum of x and y is 30;
	x is smaller than y;

### Section 2

All the Snail programs can be described by the context-free grammar given below. The start variable is program, the grammar variables are in small letters, and the terminals in capital letters.

Notice that this grammar is ambiguous in the **expr** variable; however, all the ambiguities can be removed using the precedence rules.

	program -> stmt_list
	stmt_list -> stmt_list stmt | stmt
	stmt -> assign_stmt | print_stmt | if_stmt
	assign_stmt -> ID = expr ;
	print_stmt -> PRINT expr ;| PRINT STRING ;| PRINT NEWLINE ;
	if_stmt -> IF expr THEN stmt_list ENDIF | IF expr THEN stmt_list ELSE stmt_list ENDIF
	expr -> ( expr )
		| expr + expr
		| expr - expr
		| expr * expr
		| expr / expr
		| expr < expr
		| expr > expr
		| expr <= expr
		| expr >= expr
		| expr == expr
		| expr != expr
		| - expr
		| INT
		| ID
