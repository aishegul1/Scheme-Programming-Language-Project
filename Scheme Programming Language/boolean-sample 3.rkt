(define nums (list 1 2 3 4 5 6 7))
(define face (list #\J #\Q #\K))
(define symbols (list #\H #\C #\S #\D))
(require racket/list) 

(define (is-inside? val lst)
  (list? (member val lst)))
;To check is the given value inside the given list

(define (card? card)
  (and
   (is-inside? (cdr card) symbols);First condition, cdr card is inside symbols
   (or (is-inside? (car card) face) (is-inside? (car card) nums));To check car card inside nums or face
   ))
;To check given card is valid card

;(define (name parameter) (body))
(define (suite x) (cdr x));returns the second value
(define (numeral y) (car y));returns the first value

(define (face? card) (is-inside? (car card) face)); Checks if the given pair's first value inside face
(define (value card) (if (is-inside? (car card) nums) (car card) 0.5)); IF card is face return 0.5 else first number of the card

(define (card->string card) (string-append
                             (if (face? card) (string (car card)) (number->string (car card)))
                             (string (cdr card))))
;Makes first value of the card string then makes second string and appends them together

(define (deck? lst) (cond
                      [(null? lst) #t])
                      [(card? (car lst)) (deck? (cdr lst))]
                      [else #f]
  )
;checks if the list's first value is valid card
; if it is checks the rest
;until list empty

(define (valueOf lst)
  (if (null? lst) 0 (+ (value (car lst)) (valueOf (cdr lst)))))
;first adds the first value of the list then asks for rest of it



(define (do-suite symbol)
  (define (make-cons-list lst symbol final)
  (cond
    [(null? lst) final]
    [else (make-cons-list (cdr lst) symbol (append final (list (cons (car lst) symbol))))]
    ))
  (make-cons-list (append nums face) symbol null))
;make cons from first value of the list with the given symbol and append it to final
;until list is empty
;if list is empty return final
  
(define deck (append (do-suite #\H) (do-suite #\C) (do-suite #\S) (do-suite #\D)))

(define (deck->strings deck)
  
(define (make-string-deck deck total)
  (cond
    [(null? deck) total]
    [else (make-string-deck (cdr deck) (append total (list (card->string (car deck)))))]))
  (make-string-deck deck null))

(define (get-probability comp num deck total)
  (if (null? deck)
      total
      (if
       (comp (value (car deck)) num)
       (get-probability comp num (cdr deck) (+ total 1))
       (get-probability comp num (cdr deck) total)
       )
  )    )
  

(define (probability comp num deck) (get-probability comp num deck 0))

;made internal function in order to make it tail recursion
;compare number with the first value of the given
;if comparison returns true invrement total
;do this until list is empty
;if it is empty return total

(define cheat #f)

;; F4.- Game.
;; DO NO CHANGE THE  FUNCTIONS BELOW THIS LINE
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
(define (show-statistics deck hand)
  (let
    ([toCheck (- 7.5 (valueOf hand))])
    (display
     (format
      "P(>7.5):~a/~a\nP(<7.5):~a/~a\nP(=7.5):~a/~a\nHAND:~a~nVALUE:~a\nDECK:~a...\n"
      (probability > toCheck deck)
      (length deck)
      (probability < toCheck deck)
      (length deck)
      (probability = toCheck deck)
      (length deck)                     
      (deck->strings hand)
      (valueOf hand)
      (if cheat (deck->strings (take deck
                                   (max 0 
                                    (min 4 (length deck) )))) "****")
      )
     )))
  
;; Human interaction.
(define (play deck hand)
  (begin      
      (show-statistics deck hand)
      ;; Control
      (cond
      [(= (valueOf hand) 7.5) (display "WIN")]
      [(> (valueOf hand) 7.5) (display "LOST")]
      [(empty? deck) (display "NO CARDS LEFT") ]
      [(let
           ([ command (read)])
           (cond
             [(equal? command 'accept)
               (play (rest deck) (cons (first deck) hand))]
             [(equal? command 'pass)
               (play (drop deck 1) hand)]
             [(equal? command 'end) (void)]
             [else (play deck hand)]))])))
