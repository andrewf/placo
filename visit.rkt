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
                 toplevel-init     ; () -> acc
                 toplevel-reduce   ; (name, expr-result, acc) -> acc
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
      (map (lambda (arg-expr)
             (visit-expr arg-expr env v))
           (funcall-args syntax))
      env v)]
    [(fundef? syntax)
     (visit-fundef syntax env v)]
    [(ifexpr? syntax)
     ((visitor-ifexpr v)
      (visit-expr (ifexpr-condition syntax) env v)
      (visit-expr (ifexpr-true syntax) env v)
      (if (ifexpr-else syntax)
          (visit-expr (ifexpr-else syntax) env v)
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
   (visit-expr-list (fundef-body syntax) env v)
   env v))

; evaluate a toplevel into an env itself
; take for granted the usual toplevel scoping logic
(define (visit-toplevel syntax env v)
  (let loopy ([toplevel-remaining syntax]
              [curr-env ((visitor-toplevel-init v))])
    (if (empty? toplevel-remaining)
        curr-env  ; done
        (let ([item (car toplevel-remaining)])
          (cond
            [(toplevel-let? item)
             (let ([var   (ident-name (toplevel-let-name item))]
                   [value (visit-expr (toplevel-let-value item) curr-env v)])
               (loopy (cdr toplevel-remaining)
                      ((visitor-toplevel-reduce v) var value curr-env)))]
            [(toplevel-def? item)
             (let* ([var (ident-name (toplevel-def-name item))]
                   [f (toplevel-def-value item)]
                   [value (visit-fundef f curr-env v)])
               (loopy (cdr toplevel-remaining)
                      ((visitor-toplevel-reduce v) var value curr-env)))])))))

(module+ main

  (define (incr n) (+ n 1))

  (define (indent n) (make-string (* 2 n) #\space))

  (define (print-funcall fun-result arg-results env visitor)
    (lambda (out depth)
      (display (indent depth) out)
      (fun-result out depth)
      (display "(\n" out)
      (map (lambda (x) (x out (incr depth))) arg-results)
      (display (indent depth) out)
      (display ")\n" out)
      ))

  (define (print-fundef args body-thunk env v)
    (lambda (out depth)
      (display (format "fun ~a\n" args) out)
      (body-thunk out (incr depth))
      (display (format "~aend\n" (indent depth)) out)))

  (define (print-ifexpr cond-result true-thunk else-thunk-or-false env v)
    (lambda (out depth)
        (display (format "~aif(" (indent depth)) out)
        (cond-result out depth)
        (display ") then\n" out)
        (true-thunk out (incr depth))
        (if (not (false? else-thunk-or-false))
            (begin
              (display (format  "~aelse\n" (indent depth)) out)
              (else-thunk-or-false out (incr depth)))
            (void))
        (display (format "~aend\n" (indent depth)) out)))

  (define (print-lit lit-value env v)
    (lambda (out depth) (display (format "~a~a\n" (indent depth) lit-value) out)))

  (define (print-ident name env v)
    (lambda (out depth) (display name out)))

  (define printer (visitor print-funcall
                           print-fundef
                           print-ifexpr
                           print-lit
                           print-ident
                           ; expr-list reduce init
                           (lambda () (lambda (out depth) (void)))
                           ; expr list reduce
                           (lambda (expr-result acc)
                             (lambda (out depth)
                               (acc out depth)
                               (expr-result out depth)))
                           ; toplevel init
                           (lambda () (lambda (out depth) (void)))
                           ; toplevel reduce
                           (lambda (name item-thunk remaining-thunk)
                             (lambda (out depth)
                               (display (format "~alet ~a = " (indent depth) name))
                               (item-thunk out depth)
                               (remaining-thunk out depth)
                             ))
                           ))

  (define parsed (parse (open-input-string "let abc = if x then (3) else print(5) end let x= fun(z) print(3) plus(3 plus(2 1)) end let t = 2")))
  (define toplevel-result (visit-toplevel parsed (empty-env) printer))

  (toplevel-result (current-output-port) 0)

;;   (let loop ((remaining toplevel-result))
;;     (if (not (empty? remaining))
;;         (let ((curr (car remaining)))
;;           (loop (cdr remaining))
;;           (display (format "let ~a = " (car curr)))
;;           ((cdr curr) (current-output-port) 0))
;;         #f))

)
