#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)
(require "../include/cs151-image.rkt")

;; Homework 5
;;
;; Kyu Park
;; 12198215



;; Problem 1

;; description should be String since it will contain multiple words describing what the activity is
(define-type Description String)

;; Day-of-Week can be Symbol since all of them  can be described in a single word and there are only 7 possible Day-of-Week
(define-type Day-of-Week (U 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday 'sunday))

;; Location should be String since location might need multiple words
(define-type Location String)

;; Activity is a structure since it contains description, day-of-week and location
(define-struct Activity
  ([description : Description]
   [day-of-week : Day-of-Week]
   [location : Location]))

;; Calendar is a list of activity, so it must be a type of activity list
(define-type Calendar (Listof Activity))


(: firstlecture : Activity)
(define firstlecture (Activity "regular lectures" 'monday "Zoom")) 

(: secondlecture : Activity)
(define secondlecture (Activity "regular lectures" 'wednesday "Zoom")) 

(: thirdlecture : Activity)
(define thirdlecture (Activity "regular lectures" 'friday "Zoom"))

(: lab : Activity)
(define lab (Activity "lab session" 'wednesday "Zoom"))

(: cs151 : Calendar)
(define cs151 (list firstlecture secondlecture thirdlecture lab))




(: weekday? : Activity -> Boolean)
;; checks if the day of week of the given activity is on weekday
;; returns false if it is on either saturday or sunday
;; returns true otherwise
(define (weekday? activity)
  (not (or (symbol=? (Activity-day-of-week activity) 'saturday) (symbol=? (Activity-day-of-week activity) 'sunday))))
      
(: weekday-activities : Calendar -> Calendar)
;; filters only activities that are on weekdays
;; from a given calendar
;; returns a filtered calendar that only has weekday activities
(define (weekday-activities cal)
  (filter weekday? cal))

(check-expect (weekday-activities cs151) cs151)



(: back-one-activity : Activity -> Activity)
;; creates a new activity that has the date moved back a day
(define (back-one-activity act)
  (Activity (Activity-description act) (back-a-day (Activity-day-of-week act)) (Activity-location act)))

(: back-a-day : Day-of-Week -> Day-of-Week)
;; moves the day-of-week back a day
;; outputs a new day-of-week
(define (back-a-day day)
  (match day
    ['monday 'sunday]
    ['tuesday 'monday]
    ['wednesday 'tuesday]
    ['thursday 'wednesday]
    ['friday 'thursday]
    ['saturday 'friday]
    ['sunday 'saturday]))

(: back-one : Calendar -> Calendar)
;; moves all activities back a day
;; in a given calendar
;; and outputs a new calendar with new date
(define (back-one cal)
  (map back-one-activity cal))



(check-expect (back-one cs151) (list
 (Activity "regular lectures" 'sunday "Zoom")
 (Activity "regular lectures" 'tuesday "Zoom")
 (Activity "regular lectures" 'thursday "Zoom")
 (Activity "lab session" 'tuesday "Zoom")))



(: in-ry251? : Activity -> Boolean)
;; checks if the location is in ryerson 251
;; from a given activity
;; returns true if the activity is on ryerson 251
;; returns false otherwise
(define (in-ry251? act)
  (string=? "Ryerson 251" (Activity-location act)))

(: ry251 : Calendar -> Integer)
;; counts how many activities from a given calendar
;; have location in ryerson 251
;; returns an integer value of the quantity of activities in ryerson 251
(define (ry251 cal)
  (length (filter in-ry251? cal)))


(check-expect (ry251 cs151) 0)


;; Problem 2


;; (: apply-one-or-the-other : (All (A B) (A -> Real) (B-> Real) (U A B) -> Real))

;; (: find-maximizer : (All (A) (Listof A) (A -> Real) -> A))

;; (: combine-lists : (All (A B) (Listof A) (Listof A) (A A -> B) -> (Listof B))) 