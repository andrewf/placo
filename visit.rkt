#lang racket

(provide visitor visit-expr visit-toplevel)

(require "parse.rkt")

(struct visitor (funcall ; (fun-result arg-results v) -> result
                 opcall  ; (op arg-results v) -> result
                 fundef  ; (fundef? v) -> result
                 ifexpr  ; (cond-result true-thunk else-thunk-or-false v) -> result
                 lit     ; (lit-value v) -> result
                 ident   ; (name v)
                 expr-list-init    ; () -> acc, initial acc for reduce-expr-list
                 reduce-expr-list  ; (expr-result acc) -> acc
                 toplevel-init     ; () -> acc
                 toplevel-reduce   ; (name, expr-result, acc) -> acc
                 ))

; eval a list of exprs, accumulating results into an accumulator via reduce-expr-list
(define (visit-expr-list exprs v)
  (let* ((reducer (visitor-reduce-expr-list v))
         (init-value ((visitor-expr-list-init v)))
         (actual-reduce (lambda (expr-syntax acc)
                          (reducer (visit-expr expr-syntax v)
                                   acc))))
    (foldl actual-reduce init-value exprs)))

(define (visit-expr syntax v)
  (cond
    [(funcall? syntax)
     ((visitor-funcall v)
      (visit-expr (funcall-fun syntax) v)
      (map (lambda (arg-expr)
             (visit-expr arg-expr v))
           (funcall-args syntax))
      v)]
    [(opcall? syntax)
     ((visitor-opcall v)
      (opcall-op syntax)
      (map (lambda (arg-expr)
             (visit-expr arg-expr v))
           (opcall-args syntax))
      v)]
    [(fundef? syntax)
     (visit-fundef syntax v)]
    [(ifexpr? syntax)
     ((visitor-ifexpr v)
      (visit-expr (ifexpr-condition syntax) v)
      (visit-expr (ifexpr-true syntax) v)
      (if (ifexpr-else syntax)
          (visit-expr (ifexpr-else syntax) v)
          #f)
      v)]
    [(lit? syntax)
     ((visitor-lit v) (lit-value syntax) v)]
    [(ident? syntax)
     ((visitor-ident v) (ident-name syntax) v)]
    [else (error (format "invalid expression: ~v" syntax))]))

(define (visit-fundef syntax v)
  ((visitor-fundef v)
   (map ident-name (fundef-args syntax))
   (visit-expr-list (fundef-body syntax) v)
   v))

; evaluate a toplevel a reduced value as
; specified by visitor
(define (visit-toplevel syntax v [initial-acc #f])
  (let loopy ([toplevel-remaining syntax]
              [acc (or initial-acc ((visitor-toplevel-init v)))])
    (if (empty? toplevel-remaining)
        acc  ; done
        (let ([item (car toplevel-remaining)])
          (cond
            [(toplevel-let? item)
             (let ([var   (ident-name (toplevel-let-name item))]
                   [value (visit-expr (toplevel-let-value item) v)])
               (loopy (cdr toplevel-remaining)
                      ((visitor-toplevel-reduce v) var value acc)))]
            [(toplevel-def? item)
             (let* ([var (ident-name (toplevel-def-name item))]
                   [fun (toplevel-def-value item)]
                   [value (visit-fundef fun v)])
               (loopy (cdr toplevel-remaining)
                      ((visitor-toplevel-reduce v) var value acc)))])))))

(module+ main

  (define (incr n) (+ n 1))

  (define (indent n) (make-string (* 2 n) #\space))

  (define (print-funcall fun-result arg-results visitor)
    (lambda (out depth)
      (display (indent depth) out)
      (fun-result out depth)
      (display "(\n" out)
      (map (lambda (x) (x out (incr depth))) arg-results)
      (display (indent depth) out)
      (display ")\n" out)
      ))

  (define (print-opcall op arg-results v)
    (let ([lhs (car arg-results)]
          [rhs (cdr arg-results)])
      (lambda (out depth)
        (display (indent depth) out)
        (display "(\n")
        (lhs out (incr depth))
        (display (format "~a~a\n" (indent (incr depth)) op) out)
        (if (not (empty? rhs))
            ((car rhs) out (incr depth))
            (void))
        (display (format "~a)\n" (indent depth))))))

  (define (print-fundef args body-thunk v)
    (lambda (out depth)
      (display (format "fun ~a\n" args) out)
      (body-thunk out (incr depth))
      (display (format "~aend\n" (indent depth)) out)))

  (define (print-ifexpr cond-result true-thunk else-thunk-or-false v)
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

  (define (print-lit lit-value v)
    (lambda (out depth) (display (format "~a~a\n" (indent depth) lit-value) out)))

  (define (print-ident name v)
    (lambda (out depth) (display name out)))

  (define printer (visitor print-funcall
                           print-opcall
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
                               (remaining-thunk out depth)
                               (display (format "~alet ~a = " (indent depth) name))
                               (item-thunk out depth)
                             ))
                           ))

  (define parsed (parse (open-input-string "let abc = if x then (3) else print(5) end let x= fun(z) print(3 + 2) 3 * 2 + 1 end let t = 2")))
  (define toplevel-result (visit-toplevel parsed printer))

  (toplevel-result (current-output-port) 0)
)
