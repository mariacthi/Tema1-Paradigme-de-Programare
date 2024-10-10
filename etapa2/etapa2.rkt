#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; În această etapă definim algoritmul de construcție a unui
;; arbore de sufixe (atât cel compact, cât și cel atomic) pe
;; baza unui text și a unui alfabet dat. Se știe că textul
;; utilizează doar simboluri prezente în alfabet.
;; Abordarea este următoarea:
;; 1. obținem toate sufixele textului
;; 2. pentru fiecare caracter din alfabet, determinăm sufixele
;;    care încep cu acel caracter: ele vor fi grupate în câte
;;    o ramură a arborelui de sufixe (exceptând caracterele
;;    care nu apar in text - acestea nu generează ramuri)
;; 3. pentru fiecare listă de sufixe care încep cu un același
;;    caracter, determinăm eticheta ramurii, respectiv noile
;;    sufixe care vor genera subarborele de sub etichetă
;;    - în cazul unui AST (atomic suffix tree), eticheta este
;;      chiar primul caracter, iar noile sufixe sunt vechile
;;      sufixe fără primul caracter
;;    - în cazul unui CST (compact suffix tree), eticheta este
;;      cel mai lung prefix comun al sufixelor, iar sufixele
;;      noi se obțin din cele vechi prin îndepărtarea acestui
;;      prefix
;; 4. transformăm fiecare rezultat de la pasul 3 într-o ramură
;;    - eticheta este deja calculată
;;    - calculul subarborilor se realizează repetând pașii 2-4
;;      pentru noile sufixe


; TODO 1
; Implementați recursiv o funcție care primește un text (listă 
; de caractere) și determină lista tuturor sufixelor acestuia
; (de la cel mai lung la cel mai scurt).
; Se știe că textul se va termina cu caracterul special "$", 
; și cu acest caracter trebuie să se termine și toate sufixele
; din lista rezultat (de la întreg textul urmat de "$" până la
; sufixul vid - reprezentat de un singur "$").
; ex:
; (get-suffixes '(#\w #\h #\y #\$))
; => '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
; Folosiți recursivitate pe stivă.

(define (get-suffixes text)
  (if (null? text) '()
      (cons text (get-suffixes (cdr text)))))
; pastreaza textul initial si apeleaza functia de fiecare data pe textul
; ramas fara prima litera 

; TODO 2
; Implementați o funcție care primește o listă de cuvinte 
; și un caracter ch și întoarce acele cuvinte din listă care 
; încep cu caracterul ch.
; Atenție, este posibil ca unele cuvinte să fie vide.
; Folosiți funcționale (și nu folosiți recursivitate explicită).

(define (get-ch-words words ch)
  (filter (λ (word) (and (not (null? word)) (equal? (car word) ch))) words))
; filtreaza lista de cuvinte cu ajutorul functiei anonime care verifica
; daca prima litera a fiecarui cuvant si caracterul cerut sunt egale

; TODO 3
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă AST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un AST eticheta este chiar primul caracter
; (dar sub formă de listă de caractere, pentru a putea fi 
; procesată la fel ca etichetele mai complexe), iar noile sufixe 
; se obțin din cele vechi prin eliminarea acestui caracter.
; Nu folosiți recursivitate explicită.

(define (ast-func suffixes)
  (cons (list (caar suffixes)) (map cdr suffixes)))
; pereche intre prima litera a primului sufix din lista (toate incep
; cu aceeasi litera) si restul literelor din fiecare sufix

; TODO 4
; Implementați o funcție care primește o listă nevidă de sufixe 
; care încep cu același caracter și calculează perechea
; (etichetă CST pentru aceste sufixe, lista noilor sufixe).
; Reamintim că pentru un CST eticheta este cel mai lung prefix
; comun, iar noile sufixe se obțin din cele vechi prin eliminarea 
; acestui prefix.
; Nu folosiți recursivitate explicită.

(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-list suffixes)))
    (cons prefix (map (λ (suffix) (drop suffix (length prefix))) suffixes))))
; pereche intre cel mai lung prefix comun al cuvintelor din lista si lista de
; caractere ramase din fiecare cuvant dupa ce este eliminat prefixul comun

; TODO 5
; Implementați funcția suffixes->st care construiește un
; arbore de sufixe pe baza unei liste de sufixe, a unui 
; alfabet (listă de caractere care include toate caracterele
; din sufixe), și a unei funcții care indică modul de
; etichetare (atomic sau compact).
; Când argumentul funcție va fi ast-func, vom obține un AST.
; Când argumentul funcție va fi cst-func, vom obține un CST.
; Obs: Funcția surprinde pașii 2-4 descriși mai sus.
; Funcția suffixes->st poate fi recursivă explicit, dar
; pentru celelalte prelucrări (pașii 2 și 3) trebuie să
; folosiți funcționale.

(define (suffixes->st labeling-func suffixes alphabet)
  (letrec ((get-branches-for-char
            (λ (char acc)
    (let* ((words (get-ch-words suffixes char))
           (branch (if (null? words) '()
                    (labeling-func words))))
           ; apelez functia de etichetare pentru a obtine
           ; perechea eticheta - subarbore pentru cuvintele
           ; care incep cu litera curenta din alfabet
      (if (null? branch) acc
          (cons (cons (car branch) (suffixes->st labeling-func (cdr branch) alphabet)) acc))))))
      ; cons dinautru => pastreaza eticheta si apeleaza recursiv functia pentru a putea
      ; apela din nou labeling-func pe subarborele obtinut anterior
  (if (null? suffixes) '()
      (foldr (λ (char acc) (get-branches-for-char char acc)) '() alphabet))))
      ; foldr pentru a pastra ordinea initiala a alfabetului
      ; in afisarea finala (se schimba ordinea datorita cons)(define (suffixes->st labeling-func suffixes alphabet)
'(1)

; TODO 6
; Această sarcină constă în implementarea a trei funcții:
; text->st, text->ast, text->cst, unde text->ast și text->cst
; trebuie obținute ca aplicații parțiale ale lui text->st.
; În acest scop, funcția text->st trebuie să fie curry.

; a) Implementați funcția text->st care primește un text
; (listă de caractere) și o funcție de etichetare și întoarce
; arborele de sufixe corespunzând acestui text cu această
; metodă de etichetare.
; Pași:
; - obținem sufixele textului la care adăugăm marcajul final $
; - obținem alfabetul sortat asociat textului prin utilizarea
;   funcțiilor de bibliotecă sort, remove-duplicates, char<?
;   (inclusiv caracterul $)
; - apelăm corespunzător funcția suffixes->st
; Vă veți defini singuri parametrii funcției text->st și ordinea 
; acestora, dar funcția trebuie să fie curry într-un mod care 
; facilitează derivarea funcțiilor text->ast și text->cst de 
; mai jos prin aplicație parțială a funcției text->st.
; Obs: Din acest motiv, checker-ul testează doar funcțiile
; text->ast și text->cst.

(define text->st
  (λ (labeling-func)
    (λ (text)
      (let* ((newtext (append text '(#\$)))
             (suffixes (get-suffixes newtext)) ; obtine sufixele cu $ la final
             (alphabet (remove-duplicates (sort newtext char<?)))) ; obtine alfabetul
        (suffixes->st labeling-func suffixes alphabet))))) ; apeleaza suffixes->st
        
              

; b) Din funcția text->st derivați funcția text->ast care
; primește un text (listă de caractere) și întoarce AST-ul
; asociat textului.

(define text->ast
  (λ (text)
    ((text->st ast-func) text)))

; c) Din funcția text->st derivați funcția text->cst care
; primește un text (listă de caractere) și întoarce CST-ul
; asociat textului.

(define text->cst
  (λ (text)
    ((text->st cst-func) text)))
