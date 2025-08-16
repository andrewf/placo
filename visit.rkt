#lang racket

(provide visitor visit-expr visit-toplevel)

(require "parse.rkt" "env.rkt")

(struct visitor (funcall
                 fundef
                 ifexpr
                 lit
                 ident))

; eval a list of exprs, return last value
(define (visit-expr-list exprs env v)
  (if (empty? exprs)
      (error "need at least one expression to evaluate")
      (if (empty? (cdr exprs))
          ; actual base case is last element of non-empty list
          (visit-expr (car exprs) env v)
          (begin
            (visit-expr (car exprs) env v)  ; eval for side-effects, presumably
            (visit-expr-list (cdr exprs) env v)))))

(define (visit-expr syntax env v)
  (cond
    [(funcall? syntax)
     ((visitor-funcall v)
      (visit-expr (funcall-fun syntax) env v)
      (map (lambda (arg-expr) (visit-expr arg-expr env v))
           (funcall-args syntax))
      env v)]
    [(fundef? syntax)
     (visit-fundef syntax env v)]
    [(ifexpr? syntax)
     ; eagerly eval condition, pass branches as thunks env->result
     ((visitor-ifexpr v)
      (visit-expr (ifexpr-condition syntax) env v)
      (lambda (env) (visit-expr (ifexpr-true syntax) env v))
      (if (ifexpr-else syntax)
          (lambda (env) (visit-expr (ifexpr-else syntax) env v))
          #f)
      env v)]
    [(lit? syntax)
     ((visitor-lit v) (lit-value syntax) env v)]
    [(ident? syntax)
     ((visitor-ident v) (ident-name syntax) env v)]
    [else (error (format "invalid expression: ~v" syntax))]))

(define (visit-fundef syntax env v)
  ; body as thunk env->result
  ((visitor-fundef v)
   (map ident-name (fundef-args syntax))
   (lambda (env) (visit-expr-list (fundef-body syntax) env v))
   env v))

; evaluate a toplevel into an env itself
; take for granted the usual toplevel scoping logic
(define (visit-toplevel syntax env v)
  (let loopy ([toplevel-remaining syntax]
              [curr-env env])
    (if (empty? toplevel-remaining)
        curr-env  ; done
        (let ([item (car toplevel-remaining)])
          (cond
            [(toplevel-let? item)
             (let ([var   (ident-name (toplevel-let-name item))]
                   [value (visit-expr (toplevel-let-value item) curr-env v)])
               (loopy (cdr toplevel-remaining)
                      (bind-env var value curr-env)))]
            [(toplevel-def? item)
             (let ([id (ident-name (toplevel-def-name item))]
                   [f (toplevel-def-value item)])
               (loopy (cdr toplevel-remaining)
                      (bind-env id (visit-fundef f curr-env v) curr-env)))])))))
