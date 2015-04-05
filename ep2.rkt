#lang racket

(require racket/set)
;; HELPERS

(define (regexp-index sub str)
  (car (first (regexp-match-positions (regexp sub) str))))

; EXERCICIO 1
(define (recur-list f coll)
  (if (empty? coll)
    ""
    (let ([felem (first coll)]
          [rest-coll (rest coll)])
      (f felem)
      (recur-list f rest-coll))))

; EXERCICIO 2

;; Grammar
(define terms
  '("0" "1"))

(define non-terms
  '("T" "S"))

(define initial
  "S")

(define rules
  (list (cons "T" "0T") (cons "T" "1") (cons "S" "01T")))

(define grammar
  (hash "terms" terms "non-terms" non-terms "initial" initial "rules" rules))

(define (terminal? str)
  (= (string-length str) (string-length (first (regexp-match #rx"[0|1]*" str)))));; TODO: remover terminais harcodados

(define (min-elem-length arr)
  (apply min (map string-length arr)))

(define (generate-language g size)
  (let ([_terms   (hash-ref g "terms")]
        [_initial (hash-ref g "initial")])
    (generate-language-rec g size (min-elem-length _terms) (set _initial) '())))
;; left-most derivation
(define (generate-language-rec g size curr_size strs terms)
  (let ([_rules (hash-ref g "rules")])
    (if (>= curr_size size) terms ; stop condition
      (let* ([new_strs (set-subtract (list->set (apply append (map (lambda (str) (map (lambda (p) (string-replace str (car p) (cdr p) #:all? #f)) _rules)) (set->list strs)))) strs)]
            [new_terms (filter terminal? (set->list new_strs))]
            [new_curr_size (if (empty? new_terms) curr_size (min-elem-length (set->list new_strs)))])
        (generate-language-rec g size new_curr_size new_strs (remove-duplicates (append terms new_terms)))))))

; TESTE 2
;(print (generate-language grammar 8))

;; EXERCICIO 3

(define (recognizable-by-grammar? g str)
  (list? (member str (generate-language g (string-length str)))))

; TESTE 3
;(print (recognizable-by-grammar? grammar "01000010"))
