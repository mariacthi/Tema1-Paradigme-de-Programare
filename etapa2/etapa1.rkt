#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

(require (lib "trace.ss"))
(define stree-1
  '(((#\$))
    ((#\a) ((#\$))
           ((#\n #\a) ((#\$))
                      ((#\n #\a #\$))))
    ((#\b #\a #\n #\a #\n #\a #\$))
    ((#\n #\a) ((#\$))
               ((#\n #\a #\$)))))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.

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

; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.

(define (longest-common-prefix-of-list words)
  (if (null? (cdr words)) (car words)
      (longest-common-prefix-of-list-helper words (car (longest-common-prefix (car words) (cadr words))))))
                     

(define (longest-common-prefix-of-list-helper words first-prefix)
  (cond
    ((null? words) first-prefix)
    ((equal? first-prefix (car first-prefix)) first-prefix) ; daca prefixul e doar prima litera, sigur e prefixul final
    (else (longest-common-prefix-of-list-helper
                     (cdr words) (car (longest-common-prefix (car words) first-prefix))))))

;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.

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

; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.

(define (st-has-pattern? st pattern)
  (let* ((result (match-pattern-with-label st pattern)))
    (cond
      ((equal? result #t) #t) 
      ((not (or (car result) #f)) #f)
      (else (st-has-pattern? (caddr result) (cadr result))))))
      ; in cazul in care eticheta ramurii face parte din pattern,
      ; trebuie verificata ramura in continuare, folosind argumentele
      ; functiei precedente (subarborele si restul pattern-ului