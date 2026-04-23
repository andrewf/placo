#lang racket

(provide visitor visit-expr visit-toplevel)

(require "parse.rkt" "env.rkt")

(struct visitor (funcall ; (fun-result arg-results env v) -> result
                 fundef  ; (fundef? env v) -> result
                 ifexpr  ; (cond-result true-thunk else-thunk-or-false env v) -> result
                 lit     ; (lit-value env v) -> result
                 ident   ; (name env v)
                 expr-list-init    ; () -> acc, initial acc for reduce-expr-list
                 reduce-expr-list  ; (expr-result acc) -> acc
                 ))

; eval a list of exprs, accumulating results into an accumulator via reduce-expr-list
(define (visit-expr-list exprs env v)
  (let* ((reducer (visitor-reduce-expr-list v))
         (init-value ((visitor-expr-list-init v)))
         (actual-reduce (lambda (expr-syntax acc)
                          (reducer (visit-expr expr-syntax env v)
                                   acc))))
    (foldl actual-reduce init-value exprs)))

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

(module+ main
  (define (print-funcall fun-result arg-result env visitor)
    (format "~a ~a" fun-result arg-result))
  (define (print-fundef args body-thunk env v)
    (format "fun ~a ~a end" args (body-thunk env)))
  (define (print-ifexpr cond-result true-thunk else-thunk-or-false env v)
    (let ((else-result (if (false? else-thunk-or-false)
                           ""
                           (string-append "else " (else-thunk-or-false env)))))
      (format "if(~a) ~a ~a" cond-result (true-thunk env) else-result)))
  (define (print-lit lit-value env v)
    (format "~a" lit-value))
  (define (print-ident name env v)
    (format "~a" name))

  (define printer (visitor print-funcall
                           print-fundef
                           print-ifexpr
                           print-lit
                           print-ident
                           (lambda () "")
                           (lambda (expr-result acc) (string-append acc expr-result))
                           ))

  (define parsed (parse (open-input-string "let abc = if x then (3) else print(5) end let x= fun(z) print(3) plus(3 2) end let t = 2")))
  (define toplevel-result (visit-toplevel parsed '() printer))
  (let loop ((remaining toplevel-result))
    (if (not (empty? remaining))
        (let ((curr (car remaining)))
          (loop (cdr remaining))
          (println (format "let ~a = ~a" (car curr) (cdr curr))))
        #f))

)
