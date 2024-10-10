#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))
(require (lib "trace.ss"))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

; w1, w2 = cuvinte => liste de caractere
(define (longest-common-prefix w1 w2)
  (longest-common-prefix-helper w1 w2 '()))

(define (longest-common-prefix-helper w1 w2 acc)
  (if (or (null? w1) (null? w2)) (list acc w1 w2)
      ; daca unul dintre cuvinte s a terminat returneaza prefixul comun
      ; din acc, altfel verifica in continuare prefixul comun
      (if (equal? (first w1) (first w2))
          ; daca prima litera din fiecare cuvant este egala o pune in acc,
          ; altfel inseamna ca a gasit cel mai lung prefix comun si creaza
          ; o lista cu acc si restul de litere ramase in w1 si w2
          (longest-common-prefix-helper (rest w1) (rest w2) (append acc (list (first w1))))
          (list acc w1 w2))))

; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (if (collection-empty? (collection-rest words)) (collection-first words)
      (longest-common-prefix-of-collection-helper words (car (longest-common-prefix (collection-first words) (collection-first (collection-rest words)))))))
                     

(define (longest-common-prefix-of-collection-helper words first-prefix)
  (cond
    ((collection-empty? words) first-prefix)
    ((equal? first-prefix (car first-prefix)) first-prefix) ; daca prefixul e doar prima litera, sigur e prefixul final
    (else (longest-common-prefix-of-collection-helper
                     (collection-rest words) (car (longest-common-prefix (collection-first words) first-prefix))))))


(define (match-pattern-with-label st pattern)
  (if (st-empty? st) '(#f ())
    (let* ((branch (first-branch st)) (label (get-branch-label branch)))
       (if (equal? (car label) (car pattern))
           ; verific daca prima litera a etichetei si prima litera a sablonului sunt egale
           (cond
               ((equal? label pattern) #t) 
               ((equal? (car (longest-common-prefix label pattern)) pattern) #t) ; pattern-ul face parte din eticheta
               ((equal? (car (longest-common-prefix label pattern)) label) ; eticheta face parte din pattern => trebuie verificat subarborele
                (list label (new-pattern pattern label) (get-branch-subtree branch)))
               (else (list #f (car (longest-common-prefix label pattern))))) 
           (match-pattern-with-label (other-branches st) pattern)))))

(define (new-pattern pattern label)
  (drop pattern (length label)))

(define (st-has-pattern? st pattern)
  (let* ((result (match-pattern-with-label st pattern)))
    (cond
      ((equal? result #t) #t) 
      ((not (or (car result) #f)) #f)
      (else (st-has-pattern? (caddr result) (cadr result))))))
      ; in cazul in care eticheta ramurii face parte din pattern,
      ; trebuie verificata ramura in continuare, folosind argumentele
      ; functiei precedente (subarborele si restul pattern-ului)

(define (get-suffixes text)
  (if (null? text) empty-st
      (collection-cons text (get-suffixes (cdr text)))))
; pastreaza textul initial si apeleaza functia de fiecare data pe textul
; ramas fara prima litera (lista de caractere)

(define (get-ch-words words ch)
  (collection-filter (λ (word) (and (not (null? word)) (equal? (car word) ch))) words))
; filtreaza colectia de cuvinte cu ajutorul functiei anonime care verifica
; daca prima litera a fiecarui cuvant si caracterul cerut sunt egale

(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map cdr suffixes)))
; pereche intre prima litera a primului sufix din colectie (toate incep
; cu aceeasi litera) si restul literelor din fiecare sufix

(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons prefix (collection-map (λ (suffix) (drop suffix (length prefix))) suffixes))))
; pereche intre cel mai lung prefix comun al cuvintelor din colectie si lista de
; caractere ramase din fiecare cuvant dupa ce este eliminat prefixul comun

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (letrec
      ((get-branches-for-char
        (λ (char)
          (let* ((words (get-ch-words suffixes char)) ; collection
                 (branch (if (collection-empty? words) '()
                             (labeling-func words)))) ;lista
            (if (null? branch) '()
                (cons (car branch) (suffixes->st labeling-func (cdr branch) alphabet)))))))
    (if (collection-empty? suffixes) empty-st
        (collection-filter (λ(x) (not (null? x))) (collection-map get-branches-for-char alphabet)))))
; filtrez ramurile nenule obtinute pentru fiecare char din alfabet cu functia get-branches-for-char


; nu uitați să convertiți alfabetul într-un flux
(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define text->st
  (λ (labeling-func)
    (λ (text)
      (let* ((newtext (append text '(#\$)))
             (suffixes (get-suffixes newtext)) ; obtine sufixele cu $ la final
             (alphabet (list->stream (remove-duplicates (sort newtext char<?))))) ; obtine alfabetul
        (suffixes->st labeling-func suffixes alphabet))))) ; apeleaza suffixes->st

(define text->ast
  (λ (text)
    ((text->st ast-func) text)))


(define text->cst
  (λ (text)
    ((text->st cst-func) text)))

; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let ((st (text->ast text)))
        (st-has-pattern? st pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    (let iter-branches ((st st) (prefix '()))
            (if (st-empty? st) #f
                (if (>= (length prefix) len) (take prefix len)
                    (let* ((branch (first-branch st))
                          (label (get-branch-label branch))
                          (subtree (get-branch-subtree branch)))
                      (if (st-empty? subtree)
                          ; nu e nod intern, trece la urmatoarele ramuri ale st initial
                          (iter-branches (other-branches st) prefix)
                          (if (iter-branches subtree (append prefix label))
                              ; verifica daca subarborele contine un prefix destul de lung
                              ; (ori intoarce un prefix ori #f)
                              (iter-branches subtree (append prefix label))
                              (iter-branches (other-branches st) prefix)))))))))