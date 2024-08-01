; codewriter


; Each function is responsible for compiling a specific language construct

#lang racket/base
(require racket/string)
(require "varinfo.rkt")

;exports - will give to vm file commands by appropriate values
(provide compile-term)
(provide compile-let-statement)
(provide compile-do-statement)
(provide compile-while-statement)
(provide compile-return-statement)
(provide compile-if-statement)
(provide compile-subroutine-call)
(provide compile-sunroutine-class-call)
(provide compile-subroutine-var-class-call)
(provide compile-integer)
(provide compile-true)
(provide compile-null)
(provide compile-false)
(provide compile-string)
(provide compile-this)
(provide compile-create-ctor)
(provide compile-create-function)
(provide compile-create-method)


(define labael-index 0 )
(define (update-labael-index) (set! labael-index (add1 labael-index)) )


;help function - private 
(define push-var "push ~a ~a" )
(define pop-var "pop ~a ~a" )


; Compilation functions
;integer 
(define (compile-integer value) (format "push constant ~a" value))
;true
(define (compile-true) (string-join (list "push constant 0" "not") "\n")) 
;null 
(define (compile-null) "push constant 0")
;false 
(define (compile-false) "push constant 0")
;this 
(define (compile-this) "push pointer 0")
;string
(define (compile-string valuestr )
    (string-join 
    (append 
        (list  
            (compile-integer (string-length valuestr))
            "call String.new 1"
        )
        (map (lambda (z) (string-append
                                (compile-integer (char->integer z))
                                "\n"
                                "call String.appendChar 2"
                        )) (string->list valuestr))

    )
    "\n"
    ) 
)

; Convert kind to segment
(define (kind-to-segment segment)
    (define ht (hash 
        'static 'static
        'local 'local 
        'field 'this 
        'argument 'argument 
    ))
    (hash-ref ht segment segment)
)
; Compile term (variable or array variable)
(define (compile-term value type [arrexpr (void)] )
    (define arrayvarstr  (string-join (list push-var  "~a" "add" "pop pointer 1" "push that 0") "\n") )
    [cond 
        [(equal? type 'var)  (format  push-var (kind-to-segment (varInfo-kind value)) (varInfo-index value)) ]
        [(equal? type 'arrayvar) (format arrayvarstr  (kind-to-segment (varInfo-kind value)) (varInfo-index value) arrexpr   )]
    ]
)

; Compilation functions for constructors, functions, and methods
(define (compile-create-ctor scope name numvalues fields-in-class)
    (define qstrCtor (string-join (list "function ~a.~a ~a"  "~a" "call Memory.alloc 1" "pop pointer 0" ) "\n"))
    (format qstrCtor scope name numvalues (compile-integer fields-in-class))
)
(define (compile-create-function scope name numvalues)
    (define qstrFunc (string-join (list "function ~a.~a ~a") "\n"  ))
    (format qstrFunc scope name numvalues)
)

(define (compile-create-method scope name numvalues)
    (define qstrMeth (string-join (list "function ~a.~a ~a" "push argument 0" "pop pointer 0") "\n"))
    (format qstrMeth scope name numvalues )
)

; types : 1.varray  2.var
; let var = expr ; 
(define (compile-let-statement var expr  type [arrexpr (void)]  )
    
    (define qstrvar (string-join (list  "~a" pop-var ) "\n"))
    (define qstrvararray (string-join (list push-var "~a" "add" "pop pointer 1" "~a" "pop that 0"  ) "\n"))
    (cond 
        [(equal? type 'var) 
            (format 
                                qstrvar 
                                expr 
                                (symbol->string (kind-to-segment (varInfo-kind var)) )
                                (varInfo-index var)
            )
        ]
        [(equal? type 'vararray) (format 
                                qstrvararray
                                (symbol->string (kind-to-segment (varInfo-kind var)) )
                                 (varInfo-index var)
                                arrexpr
                                expr
                            )]
        [else (error 'search_symbol_table "failed because ~a" "let syntax is sucks!!!")]
    
    )
)
(define (compile-do-statement expr)
     (format (string-join (list expr "pop temp 0") "\n"))
)
(define (compile-while-statement expr body)
     (define qstr (string-join (list  "label ~a" "~a" "not" "if-goto ~a" "~a" "goto ~a" "label ~a" ) "\n") ) 
     (define EXP-label  (format "WHILE_EXP_~a" labael-index))
     (define END-label (format "WHILE_END_~a" labael-index))
     (update-labael-index)
     (format qstr EXP-label expr END-label body EXP-label END-label)
)
(define (compile-return-statement [isvalue? #f])
     (cond 
     [isvalue? "return"]
     [else (string-join (list (compile-integer 0) "return"  ) "\n")]
     )
)
(define (compile-if-statement expr body [body2 (void)])
    (define qstrelse (string-join (list "~a" "if-goto ~a" "goto ~a" "label ~a" "~a"  "goto ~a" "label ~a" "~a" "label ~a") "\n"))
    (define qstr (string-join (list "~a" "not" "if-goto ~a"  "~a"  "label ~a") "\n"))
    (define true-label  (format "IF_TRUE_~a" labael-index))
    (define false-label (format "IF_FALSE_~a" labael-index))
    (define END-label  (format "IF_END_~a" labael-index))
    (update-labael-index)
    (cond 
        [(equal? body2 (void)) (format qstr expr END-label body END-label)]
        [else (format qstrelse expr true-label false-label true-label body END-label false-label body2 END-label)]
    )
)
(define (compile-subroutine-call scope name paramsLength parameters) 
     (format (string-join (list "push pointer 0" "~a"  "call ~a.~a ~a" ) "\n") parameters scope name (add1 paramsLength))
)
(define (compile-sunroutine-class-call className funcName paramsLength parameters)
     (format (string-join (list "~a" "call ~a.~a ~a") "\n") parameters className funcName paramsLength)
)
(define (compile-subroutine-var-class-call var name paramsLength parameters)
     (format (string-join (list push-var "~a" "call ~a.~a ~a" ) "\n") 
        (kind-to-segment (varInfo-kind var)) 
        (varInfo-index var)
        parameters
        (varInfo-type var)
        name 
        (add1 paramsLength))
)

