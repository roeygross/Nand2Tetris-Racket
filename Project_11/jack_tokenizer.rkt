; jack_tokenizer

; parsing the tokens produced by the lexer and converting them into a structured representation of the Jack programming language

#lang racket/base
;import
(require racket/string)
(require parser-tools/yacc)
(require "tokens.rkt")
(require "symbol_table.rkt")
(require "code_writer.rkt")

; Export the main parser function
(provide VMjackParser)
; Define the main parser using yacc
(define VMjackParser
  (parser
   (tokens  stopTokens keywords-tokensC symbol-tokensC INT-tokens STR-tokens ID-tokens)
   (start <class>)
   (end EOF)
   (error (lambda (a b stx)
            (error '"JACK COMPILER fail: at:\n\ttoken_nickname : `~a`\n\ttoken_symbol:  `~a`" b stx)))
   (grammar
    ;grammer rules:

    ;class -> 'class' className '{' classVarDec* subroutineDec* '}'
    [<class>
     [(<CLASS-VM> <classNameDec> <JSYM-LCURLY-VM> <ITERclassVarDec> <ITERsubRoutineDec> <JSYM-RCURLY-VM>)
      (begin (clear-all-class-scope) (string-join $5 "\n") )]]
    ;classNameDec -> ID
    [<classNameDec> [(<ID-VM>) (begin (create-class-scope $1) $1)]]
    ;classVarDec -> ('static' | 'field' ) type VarName (',' varName)* ';'
    [<classVarDec>
     [(<STATIC-VM> <type> <varName> <ITERVarName> <JSYM-DOTCOM-VM>)
      (begin (insert-static $3 $2) (for ([i $4])(insert-static i $2))(void))]
     [(<FIELD-VM> <type> <varName> <ITERVarName> <JSYM-DOTCOM-VM>)
      (begin (insert-field $3 $2) (for ([i $4]) (insert-field i $2)) (void))]]

    ;type -> 'int' | 'char' | 'boolean' | className
    [<type> [(<INT-VM>) $1] [(<CHAR-VM>) $1] [(<BOOLEAN-VM>) $1] [(<className>) $1]]
    ;subRoutineDec -> ('construcntor' | 'function' | 'method' ) ('void' | type ) subroutineName '(' paramterList ') subroutineBody
    [<subRoutineDec>

     [(<CONSTRUCTOR-VM> <type> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-ctor (get-class-scope) $3 loclnum (get-count-field)) "\n" $7)
        )
        )]

     [(<FUNCTION-VM> <type> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
           (clear-scope) (string-append (compile-create-function (get-class-scope) $3 loclnum) "\n" $7)
        )
       )]

     [(<METHOD-VM> <type> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-method (get-class-scope) $3  loclnum) "\n" $7)
        )
        )]

     [(<CONSTRUCTOR-VM> <VOID-VM> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-ctor (get-class-scope) $3 loclnum (get-count-field)) "\n" $7)
        )
        )]

     [(<FUNCTION-VM> <VOID-VM> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-function (get-class-scope) $3 loclnum) "\n" $7)
        )
        )]

     [(<METHOD-VM> <VOID-VM> <subroutineName> <JSYM-LROUND-VM> <parameterList> <JSYM-RROUND-VM> <subroutineBody>)
      (begin
        ;(dumpSymbolTables)
        (let 
          ([loclnum (get-count-local)])
          (clear-scope) (string-append (compile-create-method (get-class-scope) $3  loclnum) "\n" $7)
        )
        )]]

    ;parameterList -> ( (type varName) (','  type VarName)* )?
    [<parameterList>
     [()
      (begin 0)]
     [(<type> <varName> <ITERParamterList>)
      (begin (insert-parameter $2 $1) (for ([i $3]) (insert-parameter (list-ref i 1) (list-ref i 0))) (+ 1 (length $3)))]]
    ;subroutineBody -> '{' varDec* statements '}'
    [<subroutineBody> [(<JSYM-LCURLY-VM> <ITERVarDec> <statements> <JSYM-RCURLY-VM>) $3]]
    ;varDec -> 'var' type VarName (',' varName)* ';'
    [<varDec>
     [(<VAR-VM> <type> <varName> <ITERVarName> <JSYM-DOTCOM-VM>)
      (begin (insert-local-var $3 $2) (for ([i $4]) (insert-local-var i $2)) (void))]]
    ;className -> id
    [<className> [(<ID-VM>) $1]]
    ;subroutineName -> id
    [<subroutineName> [(<ID-VM>) $1]]
    ;varName -> id
    [<varName> [(<ID-VM>) $1]]

    ;statements -> statement*
    [<statements>
     [(<ITERStatement>) (begin (string-join $1 "\n"))]]
    ; statement -> letStatement | ifStatement | whileStatement | doStatement | returnStatement |
    [<statement> [(<letStatement>) $1] [(<ifStatement>) $1] [(<whileStatement>) $1] [(<doStatement>) $1] [(<returnStatement>) $1]]
    ;letStatement -> 'let' varName ('[' expression ']')? '=' expresion ';'
    [<letStatement>
     [(<LET-VM> <varName> <JSYM-LREC-VM> <expression> <JSYM-RREC-VM> <JSYM-EQULAS-VM> <expression> <JSYM-DOTCOM-VM>)
      (begin (compile-let-statement (searchVariable $2) $7 'vararray $4))]
     [(<LET-VM> <varName> <JSYM-EQULAS-VM> <expression> <JSYM-DOTCOM-VM>)
      (begin (compile-let-statement (searchVariable $2) $4 'var))]]
    ;ifStatement -> 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}' ) ?
    [<ifStatement>
     [(<IF-VM> <JSYM-LROUND-VM> <expression> <JSYM-RROUND-VM> <JSYM-LCURLY-VM> <statements> <JSYM-RCURLY-VM>)
      (begin (compile-if-statement $3 $6))]
     [(<IF-VM> <JSYM-LROUND-VM> <expression> <JSYM-RROUND-VM> <JSYM-LCURLY-VM> <statements> <JSYM-RCURLY-VM> <ELSE-VM> <JSYM-LCURLY-VM> <statements> <JSYM-RCURLY-VM>)
      (begin (compile-if-statement $3 $6 $10))]]
    ;whileStatement -> 'while' '(' expression ')' '{' statements '}'
    [<whileStatement>
     [(<WHILE-VM> <JSYM-LROUND-VM> <expression> <JSYM-RROUND-VM> <JSYM-LCURLY-VM> <statements> <JSYM-RCURLY-VM>)
      (begin (compile-while-statement $3 $6))]]
    ;doStatement -> 'do' subroutineCall ';'
    [<doStatement>
     [(<DO-VM> <subroutineCall> <JSYM-DOTCOM-VM>)
      (begin (compile-do-statement $2))]]
    ;ReturnStatement -> 'return' expression? ';'
    [<returnStatement>
     [(<RETURN-VM> <expression> <JSYM-DOTCOM-VM>)
      (begin (string-append $2 "\n" (compile-return-statement #t)))]

     [(<RETURN-VM> <JSYM-DOTCOM-VM>)
      (begin (compile-return-statement))]]

    ;expression -> term (op term)*
    [<expression>
     [(<term> <ITERExpression>)
      (begin (string-join (append (list $1) $2) "\n"))]]
    ;term ->  intgerConstant | stringConstant | keywordConstant | varName | VarName '[' expression ']' |
    ;         subroutineCall | '(' expression ')' | unaryOp term
    [<term>
     [(<INTEGER-VM>) $1]
     [(<STRING-VM>) $1]
     [(<KeyWordConstant>) $1]
     [(<varName>)
      (begin (compile-term (searchVariable $1) 'var))]
     [(<varName> <JSYM-LREC-VM> <expression> <JSYM-RREC-VM>)
      (begin (compile-term (searchVariable $1) 'arrayvar $3))]
     [(<subroutineCall>)  $1]
     [(<JSYM-LROUND-VM> <expression> <JSYM-RROUND-VM>)  $2]
     [(<unaryOp> <term>) (begin (string-append $2 "\n" $1))]]
    ;subroutineCall -> subroutineName '(' expressionList ')' | (className | varName)'.'subroutineName '(' expressionList ')'
    [<subroutineCall>
     [(<subroutineName> <JSYM-LROUND-VM> <expressionList> <JSYM-RROUND-VM>) 
     (begin (compile-subroutine-call (get-class-scope) $1 (list-ref $3 1) (string-join (list-ref $3 0) "\n")))]

     [(<className> <JSYM-DOT-VM> <subroutineName> <JSYM-LROUND-VM> <expressionList> <JSYM-RROUND-VM>)
      (begin
        (cond 
          [(inSymbolTable? $1) (compile-subroutine-var-class-call (searchVariable $1) $3 (list-ref $5 1) (string-join (list-ref $5 0) "\n"))]
          [else (compile-sunroutine-class-call $1 $3 (list-ref $5 1) (string-join (list-ref $5 0) "\n"))]
        )   
      )
    ]

    ]
    ;expressionList -> (expression (',' expression)* )?
    [<expressionList>
     [() (list '() 0)]
     [(<expression> <ITERExpressionList>)
      (begin (list (append (list $1) $2) (add1 (length $2))))]]
    ;op -> '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
    [<op>
     [(<JSYM-PLUS-VM>) $1]
     [(<JSYM-MINUS-VM>) "sub"]
     [(<JSYM-STAR-VM>) $1]
     [(<JSYM-SLASH-VM>) $1]
     [(<JSYM-ANDC-VM>) $1]
     [(<JSYM-ORC-VM>) $1]
     [(<JSYM-SMALLER-VM>) $1]
     [(<JSYM-BIGER-VM>) $1]
     [(<JSYM-EQULAS-VM>) $1]]
    ;unaryOp -> '-' | '~'
    [<unaryOp> [(<JSYM-MINUS-VM>) "neg"] [(<JSYM-NOTC-VM>) $1]]
    ;KeyWordConstant -> 'true' | 'false' | 'null' | 'this'
    [<KeyWordConstant> [(<TRUE-VM>) $1] [(<FALSE-VM>) $1] [(<NULL-VM>) $1] [(<THIS-VM>) $1]]

    ; grammar 
    ;classVarDec*
    [<ITERclassVarDec> [() (void)] [(<classVarDec> <ITERclassVarDec>) (void)]] ;throw result - dont care
    ;subroutineDec*
    [<ITERsubRoutineDec> [() '()] [(<subRoutineDec> <ITERsubRoutineDec>) (append (list $1) $2)]]
    ;(',' varName)*
    [<ITERVarName> [() '()] [(<JSYM-COMMA-VM> <varName> <ITERVarName>) (append (list $2) $3)]]
    ;(','  type VarName)*
    [<ITERParamterList>
     [() '()]
     [(<JSYM-COMMA-VM> <type> <varName> <ITERParamterList>) (append (list (list $2 $3)) $4)]]
    ;VarDec*
    [<ITERVarDec> [() (void)] [(<varDec> <ITERVarDec>) (void)]]
    ;(op term)*
    [<ITERExpression>
     [() '()]
     [(<op> <term> <ITERExpression>)
      (begin
        (append (list $2 $1) $3))]]
    ;(',' expression)*
    [<ITERExpressionList>
     [() '()]
     [(<JSYM-COMMA-VM> <expression> <ITERExpressionList>)
      (begin
        (append (list $2) $3))]]
    ;statement*
    [<ITERStatement>
     [() '()]
     [(<statement> <ITERStatement>)
      (begin
        (append (list $1) $2))]]
    ;VM PARSING
    ; A rule that contains a leaf will be translated to VM 
    [<JSYM-LROUND-VM> [(JSYM-LROUND) (void)]]
    [<JSYM-RROUND-VM> [(JSYM-RROUND) (void)]]
    [<JSYM-LCURLY-VM> [(JSYM-LCURLY) (void)]]
    [<JSYM-RCURLY-VM> [(JSYM-RCURLY) (void)]]
    [<JSYM-LREC-VM> [(JSYM-LREC) (void)]]
    [<JSYM-RREC-VM> [(JSYM-RREC) (void)]]
    [<JSYM-DOT-VM> [(JSYM-DOT) (void)]]
    [<JSYM-COMMA-VM> [(JSYM-COMMA) (void)]]
    [<JSYM-DOTCOM-VM> [(JSYM-DOTCOM) (void)]]
    [<CLASS-VM> [(CLASS) (void)]]
    [<FIELD-VM> [(FIELD) (void)]]
    [<STATIC-VM> [(STATIC) (void)]]
    [<VAR-VM> [(VAR) (void)]]
    [<VOID-VM> [(VOID) (void)]]
    [<LET-VM> [(LET) (void)]]
    [<DO-VM> [(DO) (void)]]
    [<IF-VM> [(IF) (void)]]
    [<ELSE-VM> [(ELSE) (void)]]
    [<WHILE-VM> [(WHILE) (void)]]
    [<RETURN-VM> [(RETURN) (void) ]]
 
    [<ID-VM> [(ID) $1]]
    [<INT-VM> [(INT) "int"]]
    [<CHAR-VM> [(CHAR) "char"]]
    [<BOOLEAN-VM> [(BOOLEAN) "boolean"]]
    [<INTEGER-VM>[(INTEGER) (begin (compile-integer $1))]]
    [<STRING-VM>[(STRING) (begin (compile-string $1))]]
    [<TRUE-VM> [(TRUE)(begin (compile-true))]]
    [<FALSE-VM>[(FALSE) (begin (compile-false))]]
    [<NULL-VM> [(NULL) (begin (compile-null))]]
    [<THIS-VM> [(THIS) (begin (compile-this))]]
    ; unary operations :
    [<JSYM-NOTC-VM> [(JSYM-NOTC) "not"]]
    ;binary operations :
    [<JSYM-PLUS-VM> [(JSYM-PLUS) "add"]]
    [<JSYM-MINUS-VM> [(JSYM-MINUS) (void)]] ; defined in upper levels
    [<JSYM-STAR-VM> [(JSYM-STAR) "call Math.multiply 2"]]
    [<JSYM-SLASH-VM> [(JSYM-SLASH) "call Math.divide 2"]]
    [<JSYM-ANDC-VM> [(JSYM-ANDC) "and"]]
    [<JSYM-ORC-VM> [(JSYM-ORC) "or"]]
    [<JSYM-SMALLER-VM> [(JSYM-SMALLER) "lt"]]
    [<JSYM-BIGER-VM> [(JSYM-BIGER) "gt"]]
    [<JSYM-EQULAS-VM> [(JSYM-EQULAS) "eq"]]
    ;leaf with callback
    [<METHOD-VM> [(METHOD)(begin (add-this-parameter) (void))]]
    [<FUNCTION-VM> [(FUNCTION) (void)]]
    [<CONSTRUCTOR-VM> [(CONSTRUCTOR) (void)]]
    )))