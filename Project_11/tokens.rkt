; tokens

; defines and exports various tokens for lexer

#lang racket/base
(require parser-tools/lex)	

;export
; declared individual tokens 
(provide token-CLASS token-CONSTRUCTOR token-FUNCTION token-METHOD token-FIELD token-STATIC token-INT token-VAR token-CHAR token-BOOLEAN token-VOID token-TRUE token-FALSE token-NULL token-THIS token-LET token-DO token-IF token-ELSE token-WHILE token-RETURN)
(provide token-JSYM-RROUND token-JSYM-LROUND token-JSYM-LCURLY token-JSYM-RCURLY token-JSYM-LREC token-JSYM-RREC token-JSYM-DOT token-JSYM-COMMA token-JSYM-DOTCOM token-JSYM-PLUS token-JSYM-MINUS token-JSYM-STAR token-JSYM-SLASH token-JSYM-ANDC token-JSYM-ORC token-JSYM-BIGER token-JSYM-SMALLER token-JSYM-EQULAS token-JSYM-NOTC)
(provide token-EOF)
(provide token-INTEGER)
(provide token-STRING)
(provide token-ID)
; groups 
(provide symbol-tokensC   )
(provide keywords-tokensC  )
(provide stopTokens)
(provide INT-tokens)
(provide ID-tokens)
(provide STR-tokens)
(provide stopTokens)
;tokens 
;the 5 types of tokens  
(define-tokens keywords-tokensC (CLASS CONSTRUCTOR FUNCTION METHOD FIELD STATIC INT VAR CHAR BOOLEAN VOID TRUE FALSE NULL THIS LET DO IF ELSE WHILE RETURN))
(define-tokens symbol-tokensC (JSYM-RROUND JSYM-LROUND JSYM-LCURLY JSYM-RCURLY JSYM-LREC JSYM-RREC JSYM-DOT JSYM-COMMA JSYM-DOTCOM JSYM-PLUS JSYM-MINUS JSYM-STAR JSYM-SLASH JSYM-ANDC JSYM-ORC JSYM-BIGER JSYM-SMALLER JSYM-EQULAS JSYM-NOTC))
(define-tokens      INT-tokens           (INTEGER)   )
(define-tokens      STR-tokens           (STRING)    )
(define-tokens      ID-tokens            (ID)        )
(define-tokens      stopTokens           (EOF)       )






  