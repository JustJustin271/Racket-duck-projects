; A list-of-number is either
  ;;   - empty
  ;;   - (cons number list-of-number)

;; wall-height : list of nums -> num
;; Add 30 to the max number of bricks
(define (wall-height bricks)
  (+ 30 (find-max bricks)))

(check-expect (wall-height (cons 10 (cons 70 (cons 40 empty)))) 100)
(check-expect (wall-height (cons 0 (cons 7 (cons 0 empty)))) 37)


;; find-max : list of nums -> num
;; Find the maximum value in the given list
;; To be used to be added with 30 as a helper function
(define (find-max bricks)
  (cond
    [(empty? bricks) 0]
    [else (max (car bricks) (find-max (cdr bricks)))]))

(check-expect (find-max (cons 10 (cons 70 (cons 40 empty)))) 70)
(check-expect (find-max (cons 1 (cons 7 (cons 0 empty)))) 7)


;; aligned-bricks : list of nums -> bool
;; Check through the list and returns whether there is a
;; Non-integer in the list
(define (aligned-bricks? brick)
  (cond
    [(empty? brick) #true]
    [(not(integer? (car brick))) #false]
    [else (aligned-bricks? (cdr brick))]))

(check-expect (aligned-bricks? (cons 10 (cons 70 (cons 40 empty)))) #true)
(check-expect (aligned-bricks? (cons 10.2 empty)) #false)


;; align-bricks : list of nums -> list of ints
;; Rounds all the numbers in the given list
(define (align-bricks bricks)
  (cond
    [(empty? bricks) empty]
    [else (cons (round (car bricks)) (align-bricks (cdr bricks)))]))

(check-expect (align-bricks (cons 10.1 (cons 70 (cons 39.6 empty)))) (list 10 70 40))
(check-expect (align-bricks (cons 3.14 (cons 2.71 (cons 1.41 empty)))) (cons 3 (cons 3 (cons 1 empty))))

;; search-and-delete : num bricks -> list of nums
;; Takes in a num, repersenting a brick's height position
;; And removes all bricks that are 30 or less than that that num
(define (search-and-delete range bricks)
  (cond
    [(empty? bricks) empty]
    [(<= (car bricks) range) (search-and-delete range (cdr bricks))]
    [else (cons (car bricks) (search-and-delete range (cdr bricks)))]))

(check-expect (search-and-delete 40 (cons 10 (cons 69 (cons 40 empty)))) (cons 69 empty))
(check-expect  (search-and-delete 30 (cons 10 (cons 67 (cons 40 empty)))) (cons 67 (cons 40 empty)))


;; insert : num int list of nums -> list of nums
;; Takes in a number to insert, the position of the number, and the list to insert that new number
;; Like trying to insert the number 3 in the 3rd slot with a list like: (list 1 2 4)
;; (insert 3 2 (list 1 2 4)) - > (list 1 2 3 4)
;; The reason I put 2 and not 3 is because computers start counting at 0 :)
(define (insert n pos lon)
  (cond
    [(empty? lon) (cons n empty)]
    [(= pos 0) (cons n lon)]
    [else (cons (first lon) 
                (insert n (sub1 pos) (rest lon)))]))

(check-expect (insert 40 5 empty) (cons 40 empty))
(check-expect (insert 40 2 (list 10 70 100)) (list 10 70 40 100))
(check-expect (insert 5 0 (list 1 2)) (list 5 1 2))
(check-expect (insert 3 2 (list 1 2 4)) (list 1 2 3 4))

(check-expect (insert 40 2 (cons 10 (cons 70 (cons 100 empty)))) (cons 10 (cons 70 (cons 40 (cons 100 empty)))))


