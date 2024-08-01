; lexer

; lexer - responsible for tokenizing the input Jack code
; defines rules for recognizing various types of tokens that can be processed by the parser
; also handles comments, whitespaces

;imports
#lang racket/base
(require parser-tools/lex)
(require parser-tools/lex-sre)
(require "tokens.rkt")

;exports
; the racket lexer
(provide Nand2TetrisLexer)


(define Nand2TetrisLexer
  ;keyword tokens rule
  (lexer 
          ;symbol tokens rule
          ["class" (token-CLASS lexeme)]
          ["constructor" (token-CONSTRUCTOR lexeme)]
          ["function" (token-FUNCTION lexeme)]
          ["method" (token-METHOD lexeme)]
          ["field" (token-FIELD lexeme)]
          ["static" (token-STATIC lexeme)]
          ["var" (token-VAR lexeme)]
          ["int" (token-INT lexeme)]
          ["char" (token-CHAR lexeme)]
          ["boolean" (token-BOOLEAN lexeme)]
          ["void" (token-VOID lexeme)]
          ["true" (token-TRUE lexeme)]
          ["false" (token-FALSE lexeme)]
          ["null" (token-NULL lexeme)]
          ["this" (token-THIS lexeme)]
          ["let" (token-LET lexeme)]
          ["do" (token-DO lexeme)]
          ["if" (token-IF lexeme)]
          ["else" (token-ELSE lexeme)]
          ["while" (token-WHILE lexeme)]
          ["return" (token-RETURN lexeme)]  
          ["(" (token-JSYM-LROUND lexeme)]
          [")" (token-JSYM-RROUND lexeme)]
          ["{" (token-JSYM-LCURLY lexeme)]
          ["}" (token-JSYM-RCURLY lexeme)]
          ["[" (token-JSYM-LREC lexeme)]
          ["]" (token-JSYM-RREC lexeme)]
          ["." (token-JSYM-DOT lexeme)]
          ["," (token-JSYM-COMMA lexeme)] 
          [";" (token-JSYM-DOTCOM lexeme)]
          ["+" (token-JSYM-PLUS lexeme)]
          ["-" (token-JSYM-MINUS lexeme)]
          ["*" (token-JSYM-STAR lexeme)]
          ["/" (token-JSYM-SLASH lexeme)]
          ["&" (token-JSYM-ANDC lexeme)]
          ["|" (token-JSYM-ORC lexeme)]
          ["<" (token-JSYM-SMALLER lexeme)]
          [">" (token-JSYM-BIGER lexeme)]
          ["=" (token-JSYM-EQULAS lexeme)]
          ["~" (token-JSYM-NOTC lexeme)]
         ;number tokens rule between 0 to 32767 - if the number is above it will be splitted to a few tokens ... 
         ;([0-9]|[1-9][0-9]{1,3}|[12][0-9]{4}|3[01][0-9]{3}|32[0-6][0-9]{2}|327[0-5][0-9]|3276[0-7])
         [(or (:(char-range #\0 #\9))
              (: (char-range #\1 #\9) (repetition 1 3 (char-range #\0 #\9)))
              (: (char-range #\1 #\2) (repetition 4 4 (char-range #\0 #\9)))
              (: "3" (char-range #\0 #\1) (repetition 3 3 (char-range #\0 #\9)))
              (: "32" (char-range #\0 #\6) (repetition 2 2 (char-range #\0 #\9)))
              (: "327" (char-range #\0 #\5) (char-range #\0 #\9))
              (: "3276" (char-range #\0 #\7)))
          ;=>
            (token-INTEGER lexeme)]
         ;string tokens rule
         [(: #\" (repetition 0 +inf.0 (~ #\")) #\")
          (begin
            (token-STRING (substring lexeme 1 (sub1 (string-length lexeme)))))]
         ;id tokens rule
         [(: (or lower-case upper-case #\_) (* (or #\_ lower-case upper-case numeric)))
          ;=>
      
            (token-ID lexeme)]
         ; comment type 1 //
          ; Single-line comment rule
         [(: "//" (repetition 0 +inf.0 (~ #\newline)) #\newline) (void)]
         ;comment type 2 \**\
         ; Multi-line comment rule
         [(: "/*" (repetition 0 +inf.0 (~ "/" )) "*/") (void)]
         ;igoring all those chars 
         ; Ignore whitespace characters
         [#\space (void)]
         [#\newline (void)]
         [whitespace (void)]
         [blank (void)]
         ;stop lexing 
          ; End of file token
         [(eof) (token-EOF lexeme)]))
