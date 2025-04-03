#lang racket/base

#|
    File Name: main.scm
    Author: Trevor Mee
    COP4020 Project 3
    Date: 04/11/2025
|#


#|
    @brief function that looks to see if an identifer(symbol?) is bounded in the env

    @param(s) id:  the identifier(symbol) we are trying to lookup
              env: the binding environment defining identifiers

    @return if an identifier is bound in the env, return it
            if an identifer is not bound in the env, report an error message
|#
(define (lookupId id env)
    (define errMsg1 "Error: ")
    (define errMsg2 " is not bound")
    (cond 
        ; If identifier is not bound, report an error
        ((null? env) (error (string-append errMsg1 (symbol->string id) errMsg2)))

        ; If identifier is bound, return the value of it
        ((eq? id (caar env)) (cdar env))

        ; Keep searching
        (else (lookupId id (cdr env)))
    )
)


#|
    @brief handles user defined function test cases
    @param(s) exp: the expression representing a user defined function
              args: the user defined arguments
              env: the binding environment
    
    @return the reult of evaluating the function body 

    @details 
        - params (cadr exp) stores the list of parameters
            -- exp is a list where the 1st element is a function name, 
               the 2nd element is a list of parameters (cadr), and the 
               third element is the function body (caddr)
        - body (caddr exp) extracts the function body
        - updatedEnv creates a new environment where...
            -- map (lambda (x) (evalu8 x env)) args evaluates each arg in curr env
            -- map const params (...) pairs each paramter with its evaluated arg
            -- append (...) updates the curr env with the new parameters
        - (evalu8 body updatedEnv) evalutes the function body with the updated env
|#
(define (userFunction exp args env)
    (let* ((params (cadr exp))      
            (body (caddr exp))     
            (updatedEnv (append (map cons params (map (lambda (x) (evalu8 x env)) args)) env)))
        (evalu8 body updatedEnv)
    )
)



#|
    @brief replacement to the built in eval() function

    @param(s) exp: the expression to be evaluated
              env: the binding environment
    
    @return If exp is a list, recursively call evalu8 and return the result
            If exp is a numerical literal, return it
            If exp is an identifer name, look it up in the binding environment, return the value
|#
(define (evalu8 exp env)
    (cond 
        ; If expression is a list, evalu8 it
        ((pair? exp)       
            (let ((op (car exp)))
                (cond
                    ; Recursively call evalu8 on each operation and returning the result
                    ((eq? op '+) (+ (evalu8 (cadr exp) env) (evalu8 (caddr exp) env)))           ; addition operation
                    ((eq? op '*) (* (evalu8 (cadr exp) env) (evalu8 (caddr exp) env)))           ; multiplication operation
                    ((eq? op 'equal?) (eq? (evalu8 (cadr exp) env) (evalu8 (caddr exp) env)))    ; "equal to" operation
                    ((eq? op 'less?) (< (evalu8 (cadr exp) env) (evalu8 (caddr exp) env)))       ; "less than" operation
                    ((eq? op '/) (/ (evalu8 (cadr exp) env) (evalu8 (caddr exp) env)))           ; division
                    
                    ; Handle user-defined function calls
                    ((symbol? op)
                        (let ((function (lookupId op env)))
                            (if (pair? function)
                                (userFunction function (cdr exp) env)
                                (error "Error: Not a function!")
                            )
                        )
                    )

                    ; Handle lambda expressions
                    ((eq? (car op) 'lambda) (userFunction op (cdr exp) env))

                )
            )
        )

        ; If exp is not a list, it is either a number or symbol...

        ; If exp is a numerical literal, return it
        ((number? exp) exp)

        ; If exp is an identifier, look it up in the binding environment
        ((symbol? exp) (lookupId exp env))
  )
)
        
; Define a binding enviroment
(define env '((a . 12)(b . 5.2)(foo . (lambda (x y) (/ ( + x y) 2)))))

; Test cases
(display (evalu8 '(+ a 5) env))         ; expect 17
(newline)
(display (evalu8 '(* (+ a 5) 3) env))   ; expect 51
(newline)
;(display (evalu8 '(* (+ a 5) c) env))   ; expect "Error: C is not bound"
(newline)
(display (evalu8 '(foo 5 9) env))       ; expect 7
(newline)
(display (evalu8 '((lambda (x y) (* x (+ x y))) 2 3) env))  ; expect 10
(newline)