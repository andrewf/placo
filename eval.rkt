#lang racket

(provide eval-toplevel eval-expr
         bind-env empty-env)

(require "parse.rkt")

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
        (let* ([item (car toplevel-remaining)])
          (cond
            [(toplevel-let? item)
             (let* ([var   (ident-name (toplevel-let-name item))]
                    [value (visit-expr (toplevel-let-value item) curr-env v)])
               (loopy (cdr toplevel-remaining)
                      (bind-env var value curr-env)))]
            [(toplevel-def? item)
             (let* ([id (ident-name (toplevel-def-name item))]
                    [f (toplevel-def-value item)])
               (loopy (cdr toplevel-remaining)
                      (bind-env id (visit-fundef f curr-env v) curr-env)))])))))

(define (falsy t)
  (and (integer? t) (= t 0)))

(define (truthy t) (not (falsy t)))

(define (empty-env) '())

(define (lookup env var)
  (if (empty? env)
      (error (format "missing var ~a" var))
      (let* ([curr-binding (car env)]
             [parent-env (cdr env)]
             [curr-var (car curr-binding)]
             [curr-val (cdr curr-binding)])
        (if (equal? var curr-var)
            curr-val
            (lookup parent-env var)))))

(define (bind-env var value env)
  (cons (cons var value) env))

; bind-env but multiple times
; zip arg-names and arg-values
(define (bind-env-names arg-names arg-values env)
  (cond
    [(and (empty? arg-names) (empty? arg-values))
     env]
    [(and (not (empty? arg-names)) (not (empty? arg-values)))
     (bind-env-names (cdr arg-names)
                     (cdr arg-values)
                     (bind-env (car arg-names) (car arg-values) env))]
    [else (error ("mismatching argument lengths"))]))

(define (eval-visit-funcall fun arg-values env v)
    (if (procedure? fun)
        (fun arg-values)
        (error (format "trying to call non-function ~a" fun))))

(define (eval-visit-fundef arg-names body-thunk env-at-def v)
  ; just need to capture lexical context
  ; we can do that with closure in host language. lol.
    (lambda (arg-values)
      (let ([actual-env (bind-env-names arg-names arg-values env-at-def)])
          (body-thunk actual-env))))

(define (eval-visit-ifexpr condition true-branch else-branch env v)
    (if (truthy condition)
        (true-branch env)
        (if else-branch
            (else-branch env)
            ; empty else case
            'no-else)))

(define (eval-visit-lit value env v)
  value)

(define (eval-visit-ident name env v)
  (lookup env name))

(define eval-visitor (visitor
                      eval-visit-funcall
                      eval-visit-fundef
                      eval-visit-ifexpr
                      eval-visit-lit
                      eval-visit-ident))

(define (eval-expr expr env)
  (visit-expr expr env eval-visitor))

(define (eval-toplevel expr [starting-env (empty-env)])
  (visit-toplevel expr starting-env eval-visitor))


(module+ test
  (require rackunit))

(module+ test
  (check-equal? (eval-expr (ident "f")
                         (bind-env "f" 12 (empty-env)))
                12
                "trivial var")

  (check-equal? (eval-expr (ifexpr (lit 0) (lit 1) (lit 2)) (empty-env))
                2
                "trivial if false")

  (check-equal? (eval-expr (ifexpr (lit 42) (lit 1) (lit 2)) (empty-env))
                1
                "trivial if true")

  (check-equal? (eval-expr (ifexpr (ident "f") (lit 1) (lit 2))
                         (bind-env "f" 0 (empty-env)))
                2
                "if var")

  (check-equal? (eval-expr (ident "f")
                           (eval-toplevel (parse-string
                              "let g = 1 let g = 2 let f = g")))
                2
                "sequential let")

  (check-equal? (eval-expr (ident "f")
                           (eval-toplevel (parse-string
                              "def g(a) if a then 42 else 17 end end let f = g(1)")))
                42
                "basic fun call")

  (check-equal? (eval-expr (funcall (ident "g") '())
                           (eval-toplevel (parse-string "let g = fun() (17) end")))
                17
                "fun call no args")

  (check-equal? (eval-expr (funcall (ident "g") '())
                           (eval-toplevel (parse-string "let g = fun() if 4 then 3 else 5 end 42 end")))
                42
                "fun call multiple body expressions")

  (check-equal? (eval-expr (funcall (ident "g") '())
                           (eval-toplevel (parse-string
                                           "def h(x) fun() x end end
                                            let g = h(19)"))) ; g = fun() 19 end
                19
                "closure")

  (check-equal? (eval-expr (funcall (ident "g") (list (lit 3) (ident "x")))
                           (eval-toplevel (parse-string
                                           "def h(x) fun(a b) b end end
                                            let x = 5
                                            let g = h(19)"))) ; g = fun(a b) b end
                5
                "multiple args")
)
