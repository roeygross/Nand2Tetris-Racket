; symbol table 

; managing variables and their scopes in the Jack compiler:
; maintains separate tables for class-level and subroutine-level variables, 
; provides functions for inserting and searching variables,
; keeps track of variable counts for different types

; Well use 2 hash tables. each hash table will contain key : name value : varaibleInfo_struct
; There will be 2 const tables that can be updated in runtime cleared (- the scope is out) or add var

#lang racket/base

;import
(require "varinfo.rkt")

;exports 
(provide searchVariable)
(provide clear-scope)
(provide create-class-scope)
(provide get-class-scope)
(provide clear-all-class-scope)
(provide insert-local-var)
(provide insert-parameter)
(provide insert-field)
(provide insert-static)
(provide add-this-parameter)
(provide get-count-local)
(provide get-count-argument)
(provide get-count-field)
(provide get-count-static)
(provide inSymbolTable?)
;symbol tables 
(define classLevelSymbolTable (make-hash))
(define subroutineLevelSymbolTable (make-hash))

; Counters for different variable types
(define localVarCounter 0)
(define argumentVarCounter 0)
(define fieldVarCounter 0)
(define staticVarCounter 0)
(define class-Scope "")

; Functions for symbol table management
(define (insert-subroutineLevelSymbolTable var) (hash-set! subroutineLevelSymbolTable   (varInfo-name var)  var))
(define (clear-subroutineLevelSymbolTable) (hash-clear!  subroutineLevelSymbolTable))

(define (insert-ClassLevelSymbolTable var) (hash-set! classLevelSymbolTable   (varInfo-name var) var))
(define (clear-ClassLevelSymbolTable) (hash-clear! classLevelSymbolTable))
; Check if a variable is in either symbol table
(define (inSymbolTable? name)
    (or
        (list-ref (search-hash-string subroutineLevelSymbolTable name) 0)
        (list-ref (search-hash-string classLevelSymbolTable name) 0)
    )
)
; Search for a variable in both symbol tables
(define (searchVariable name) 
(let 
    ([results (search-hash-string subroutineLevelSymbolTable name)])
    (cond 
        [(list-ref results 0) (list-ref results 1)]
        [ else
            (let ([results2 (search-hash-string classLevelSymbolTable name)])
                 (cond  [(list-ref results2 0) (list-ref results2 1)] [else  (error 'search_symbol_table "failed because ~a" "find_variale anywhere")])
            )
        ]
    )
)
)


; Functions for managing variable counters
(define (get-count-local) localVarCounter ) 
(define (update-count-local) (set! localVarCounter (add1 localVarCounter)))
(define (clear-count-local) (set! localVarCounter 0 ))

(define (get-count-argument) argumentVarCounter ) 
(define (update-count-argument) (set! argumentVarCounter (add1 argumentVarCounter)))
(define (clear-count-argument) (set! argumentVarCounter 0 ))

(define (get-count-field) fieldVarCounter ) 
(define (update-count-field) (set! fieldVarCounter (add1 fieldVarCounter)))
(define (clear-count-field) (set! fieldVarCounter 0 ))

(define (get-count-static) staticVarCounter ) 
(define (update-count-static) (set! staticVarCounter (add1 staticVarCounter)))
(define (clear-count-static) (set! staticVarCounter 0 ))

; Functions for inserting different types of variables
(define (insert-local-var name type )  (insert-subroutineLevelSymbolTable (varInfo name type 'local (get-count-local)  'subroutine)) (update-count-local))
(define (insert-parameter name type)  (insert-subroutineLevelSymbolTable (varInfo name type 'argument (get-count-argument)  'subroutine)) (update-count-argument) )
(define (create-class-scope name) (set! class-Scope name))
(define (get-class-scope) class-Scope  )
(define (add-this-parameter) (insert-parameter "this" (get-class-scope)))
(define (insert-field name type)  (insert-ClassLevelSymbolTable (varInfo name type 'field (get-count-field)  'class)) (update-count-field) )
(define (insert-static name type)  (insert-ClassLevelSymbolTable (varInfo name type 'static (get-count-static)  'class)) (update-count-static) )
; Functions for clearing scopes
(define (clear-scope) (clear-count-local) (clear-count-argument) (clear-subroutineLevelSymbolTable) ) 
(define (clear-all-class-scope) (clear-count-static) (clear-count-field) (clear-ClassLevelSymbolTable))

; Custom hash function for string search
; This is used because the default hash equality check can't compare strings without expensive casting
(define (search-hash-string hasht valt)
    (define res (void)) (define found #f)
    (hash-for-each hasht (lambda (x y) (cond [(string=? x valt) (set! found #t )(set! res y)] )))
    (list found res)
)


