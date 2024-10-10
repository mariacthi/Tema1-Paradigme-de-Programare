#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let ((st (text->ast text)))
        (st-has-pattern? st pattern)))

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (longest-common-substring text1 text2)
  (let ((st1 (text->ast text1))
    ; named let pentru a obtine lista de suffixe pentru text2 (suffixes2)
    (suffixes2
     (let get-suff ((text2 text2) (suffixes '()))
      (if (null? text2)
          (reverse suffixes)
          (get-suff (cdr text2) (cons text2 suffixes))))))
    ; named let pentru a calcula cel mai lung substring pentru fiecare
    ; sufix gasit
    (let iter-suff ((st1 st1) (suffixes2 suffixes2) (substring '()))
      (if (null? suffixes2) substring
          (let* ((first-suff (car suffixes2))
             (result (match-suffix-with-label st1 first-suff '()))
             (l1 (length result))
             (l2 (length substring)))
            (if (> l1 l2)
                (iter-suff st1 (cdr suffixes2) result)
                (iter-suff st1 (cdr suffixes2) substring)))))))
      
; functie asemanatoare cu implementarea pentru match-pattern-with-label
; facuta pentru prima etapa (am schimbat ce returneaza, dar cazurile
; sunt aceleasi)
 (define (match-suffix-with-label st suffix substring)
  (if (st-empty? st) substring
    (let* ((branch (first-branch st))
           (label (get-branch-label branch))
           (prefix (car (longest-common-prefix label suffix))))
       (if (equal? (car label) (car suffix))
           ; verific daca prima litera a etichetei si prima litera a sufixului sunt egale
           ; => sunt pe ramura potrivita
           (cond
               ((equal? label suffix) (append substring label))
               ((equal? prefix suffix) (append substring suffix)) ; sufixul face parte din eticheta
               ((equal? prefix label) ; eticheta face parte din sufix => trebuie verificat subarborele
                (match-suffix-with-label (get-branch-subtree branch) (new-pattern suffix label) (append substring prefix)))
               (else (append substring prefix))) 
           (match-suffix-with-label (other-branches st) suffix substring)))))

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.

(define (repeated-substring-of-given-length text len)
  (let ((st (text->cst text)))
    (let iter-branches ((st st) (prefix '()))
            (if (st-empty? st) #f
                (if (>= (length prefix) len) (take prefix len)
                    (let* ((branch (first-branch st))
                          (label (get-branch-label branch))
                          (subtree (get-branch-subtree branch)))
                      (if (null? subtree)
                          ; nu e nod intern, trece la urmatoarele ramuri ale st initial
                          (iter-branches (other-branches st) prefix)
                          (if (iter-branches subtree (append prefix label))
                              ; verifica daca subarborele contine un prefix destul de lung
                              ; (ori intoarce un prefix ori #f)
                              (iter-branches subtree (append prefix label))
                              (iter-branches (other-branches st) prefix)))))))))