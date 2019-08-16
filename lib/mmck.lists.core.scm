;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Lists
;;;Contents: core lists handling functions
;;;Date: Aug 16, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the core lists handling functions.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(declare (unit mmck.lists.core)
	 (uses mmck.lists.assertions)
	 (emit-import-library mmck.lists.core))

(module (mmck.lists.core)
    (
     ;; unsafe operations
     $car
     $cdr
     $set-car!
     $set-cdr!
     $set-car-immediate!
     $set-cdr-immediate!

     ;; constructors
     cons*
     make-list

     ;; predicates
     list-of-lists? list-of-nulls? list-of-pairs?
     lists-of-equal-length? lists-of-lists-of-equal-length?

     ;; getters
     cars-and-cdrs
     gather-cars-in-reverse
     butlast-and-last

     ;; iteration and searching
     fold-left fold-right
     find exists for-all
     map-in-order for-each-in-order

     ;; unsafe iteration and searching
     $fold-left/1
     $fold-left/2
     $fold-left/3
     $fold-left/list
     ;;
     $fold-right/1
     $fold-right/2
     $fold-right/3
     $fold-right/list
     ;;
     $map/1
     $map/2
     $map/3
     $map/list
     ;;
     $for-each/1
     $for-each/2
     $for-each/3
     $for-each/list
     ;;
     $for-each-in-order/1
     $for-each-in-order/2
     $for-each-in-order/3
     $for-each-in-order/list
     ;;
     $map-in-order/1
     $map-in-order/2
     $map-in-order/3
     $map-in-order/list
     ;;
     $for-all/1
     $for-all/2
     $for-all/3
     $for-all/list
     ;;
     $exists/1
     $exists/2
     $exists/3
     $exists/list
     ;;
     $find

     ;; exceptional-condition object-types
     &list-is-empty
     make-list-is-empty-condition
     condition-list-is-empty?
     raise-exception-list-is-empty
     ;;
     &lists-are-of-different-length
     make-lists-are-of-different-length-condition
     condition-lists-are-of-different-length?
     raise-exception-lists-are-of-different-length
     ;;
     &lists-are-empty-or-of-different-length
     make-lists-are-empty-or-of-different-length-condition
     condition-lists-are-empty-or-of-different-length?
     raise-exception-lists-are-empty-or-of-different-length
     )
  (import (scheme)
	  (only (chicken type)
		:)
	  (only (chicken base)
		butlast
		void)
	  (only (chicken fixnum)
		fx=)
	  (mmck lists assertions)
	  (mmck lang)
	  (mmck exceptional-conditions))


;;;; helpers

(define-syntax define-list-folder
  (syntax-rules ()
    ((_ ?who ?list-folder/1 ?list-folder/2 ?list-folder/3 ?list-folder/list)
     (case-define ?who
       ((combine nil ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell     2))
	(?list-folder/1 combine nil ell))

       ((combine nil ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-folder/2 combine nil ell1 ell2))

       ((combine nil ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3    4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-folder/3 combine nil ell1 ell2 ell3))

       ((combine nil ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? combine 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1    2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2    3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3    4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-folder/list combine nil (cons* ell1 ell2 ell3 ell*)))))
    ))

(define-syntax define-list-mapper
  (syntax-rules ()
    ((_ ?who ?list-mapper/1 ?list-mapper/2 ?list-mapper/3 ?list-mapper/list)
     (case-define ?who
       ((func ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell  2))
	(?list-mapper/1 func ell))

       ((func ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-mapper/2 func ell1 ell2))

       ((func ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-mapper/3 func ell1 ell2 ell3))

       ((func ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? func 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-mapper/list func (cons* ell1 ell2 ell3 ell*)))))
    ))

(define-syntax define-list-searcher
  (syntax-rules ()
    ((_ ?who ?list-searcher/1 ?list-searcher/2 ?list-searcher/3 ?list-searcher/list)
     (case-define ?who
       ((pred ell)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell  2))
	(?list-searcher/1 pred ell))

       ((pred ell1 ell2)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2))
	(?list-searcher/2 pred ell1 ell2))

       ((pred ell1 ell2 ell3)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3))
	(?list-searcher/3 pred ell1 ell2 ell3))

       ((pred ell1 ell2 ell3 . ell*)
	(begin-checks
	  (assert-argument-type (quote ?who) "procedure" procedure? pred 1)
	  (assert-argument-type (quote ?who) "list"      list?      ell1 2)
	  (assert-argument-type (quote ?who) "list"      list?      ell2 3)
	  (assert-argument-type (quote ?who) "list"      list?      ell3 4)
	  (assert-argument-type/rest (quote ?who) "list of lists" list-of-lists? ell*)
	  (assert-lists-of-equal-length (quote ?who) ell1 ell2 ell3 ell*))
	(?list-searcher/list pred (cons* ell1 ell2 ell3 ell*)))))
    ))


;;;; exceptional-condition object-types

(define-condition-type &lists-are-of-different-length
    &assertion
  make-lists-are-of-different-length-condition
  condition-lists-are-of-different-length?)

(define (raise-exception-lists-are-of-different-length who list-of-lists)
  (raise
   (condition (make-lists-are-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, lists are of different length")
	      (make-irritants-condition (list list-of-lists)))))

;;; --------------------------------------------------------------------

(define-condition-type &list-is-empty
    &assertion
  make-list-is-empty-condition
  condition-list-is-empty?)

(define (raise-exception-list-is-empty who obj)
  (raise
   (condition (make-list-is-empty-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid operand, expected non-empty list")
	      (make-irritants-condition (list obj)))))

;;; --------------------------------------------------------------------

(define-condition-type &lists-are-empty-or-of-different-length
    &assertion
  make-lists-are-empty-or-of-different-length-condition
  condition-lists-are-empty-or-of-different-length?)

(define (raise-exception-lists-are-empty-or-of-different-length who list-of-lists)
  (raise
   (condition (make-lists-are-empty-or-of-different-length-condition)
	      (make-who-condition who)
	      (make-message-condition "invalid arguments, lists are empty or of different length")
	      (make-irritants-condition (list list-of-lists)))))


;;;; unsafe operations

(define-syntax-rule ($car ?pair)
  (##sys#slot ?pair 0))

(define-syntax-rule ($cdr ?pair)
  (##sys#slot ?pair 1))

(define-syntax-rule ($set-car! ?pair ?new-value)
  (##sys#setslot ?pair 0 ?new-value))

(define-syntax-rule ($set-cdr! ?pair ?new-value)
  (##sys#setslot ?pair 1 ?new-value))

(define-syntax-rule ($set-car-immediate! ?pair ?new-value)
  (##sys#setislot ?pair 0 ?new-value))

(define-syntax-rule ($set-cdr-immediate! ?pair ?new-value)
  (##sys#setislot ?pair 1 ?new-value))


;;;; predicates

(define (list-of-lists? objs)
  ;;Return true  if OBJS  is a  (possibly empty) list  of (possibly  empty) lists;  otherwise return
  ;;false.  Notice that this function returns false if OBJS is not null or a proper list of pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (list? (car objs))
	   (list-of-lists? (cdr objs)))))

(define (list-of-nulls? objs)
  ;;Return true if OBJS is a (possibly empty)  list of null objects; otherwise return false.  Notice
  ;;that this function returns false if OBJS is not null or a proper list of nulls.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (null? (car objs))
	   (list-of-nulls? (cdr objs)))))

(define (list-of-pairs? objs)
  ;;Return true if OBJS is a (possibly empty)  list of pair objects; otherwise return false.  Notice
  ;;that this function returns false if OBJS is not null or a proper list of pairs.
  ;;
  (or (null? objs)
      (and (pair? objs)
	   (pair? (car objs))
	   (list-of-pairs? (cdr objs)))))

(define (lists-of-lists-of-equal-length? ell*)
  ;;Return true if ELL* is a list of (possibly empty) lists of equal length; otherwise return false.
  ;;Notice that this function returns false if ELL* is not null or a proper list of pairs.
  ;;
  (or (null? ell*)
      (and (pair? ell*)
	   (list? (car ell*))
	   (let loop ((len1 (length (car ell*)))
		      (ell* (cdr ell*)))
	     (or (null? ell*)
		 (and (pair? ell*)
		      (list? (car ell*))
		      (= len1 (length (car ell*)))
		      (loop len1 (cdr ell*))))))))

(case-define lists-of-equal-length?
  ;;Return true if  all the arguments are  (possibly empty) lists of equal  length; otherwise return
  ;;false.  Notice that this function returns false if one  of the arguments is not null or a proper
  ;;list of pairs.
  ;;
  ((ell1)
   (list? ell1))

  ((ell1 ell2)
   (let loop ((ell1 ell1)
	      (ell2 ell2))
     (or (and (null? ell1)
	      (null? ell2))
	 (and (pair? ell1)
	      (pair? ell2)
	      (loop (cdr ell1)
		    (cdr ell2))))))

  ((ell1 ell2 ell3)
   (let loop ((ell1 ell1)
	      (ell2 ell2)
	      (ell3 ell3))
     (or (and (null? ell1)
	      (null? ell2)
	      (null? ell3))
	 (and (pair? ell1)
	      (pair? ell2)
	      (pair? ell3)
	      (loop (cdr ell1)
		    (cdr ell2)
		    (cdr ell3))))))

  ((ell1 ell2 ell3 . ell*)
   (let loop ((ell1 ell1)
	      (ell2 ell2)
	      (ell3 ell3)
	      (ell* ell*))
     (or (and (null? ell1)
	      (null? ell2)
	      (null? ell3)
	      ($for-all/1 null? ell*))
	 (and (pair? ell1)
	      (pair? ell2)
	      (pair? ell3)
	      ($for-all/1 pair? ell*)
	      (loop (cdr ell1)
		    (cdr ell2)
		    (cdr ell3)
		    (map cdr ell*))))))
  #| end of CASE-DEFINE |# )


;;;; special exceptional-condition raisers

(case-define assert-lists-of-equal-length
  ((who ell1 ell2)
   (unless (lists-of-equal-length? ell1 ell2)
     (raise-exception-lists-are-of-different-length who (list ell1 ell2))))
  ((who ell1 ell2 ell3)
   (unless (lists-of-equal-length? ell1 ell2 ell3)
     (raise-exception-lists-are-of-different-length who (list ell1 ell2 ell3))))
  ((who ell1 ell2 ell3 ell*)
   (unless (lists-of-lists-of-equal-length? (cons* ell1 ell2 ell3 ell*))
     (raise-exception-lists-are-of-different-length who (cons* ell1 ell2 ell3 ell*)))))


;;;; constructors

(case-define cons*
  ((item)
   item)
  ((item ell)
   (cons item ell))
  ((item1 item2 ell)
   (cons item1 (cons item2 ell)))
  ((item . rest)
   (let loop ((item	item)
	      (rest	rest))
     (if (null? rest)
	 item
       (cons item (loop (car rest) (cdr rest)))))))

(case-define make-list
  ((len)
   (make-list len (void)))
  ((len fill)
   (if (fx= 0 len)
       '()
     (cons fill (make-list (- len 1) fill)))))


;;;; getters

(case-define* cars-and-cdrs
  ((ell*)
   (cars-and-cdrs ell* '()))
  ((ell* car*-tail)
   ;;The argument ELL* must be a list of non-empty  lists.  Return two values: a list of the CARs of
   ;;the lists in ELL*; a list of the CDRs of the lists in ELL*.
   ;;
   ;;The optional argument CAR*-TAIL must be a list: it is appended to the cars list; it defaults to
   ;;null.
   ;;
   ;;NOTE Let's avoid doing this with a non-tail recursion!!!
   ;;
   (when (or (null? ell*)
	     (list-of-nulls? ell*))
     (raise-exception-lists-are-empty-or-of-different-length (__who__) ell*))
   (let loop ((lle* (reverse ell*))
	      (car* car*-tail)
	      (cdr* '()))
     (if (pair? lle*)
	 (let ((next-sublist (car lle*)))
	   (if (pair? next-sublist)
	       (loop (cdr lle*)
		     (cons (car next-sublist) car*)
		     (cons (cdr next-sublist) cdr*))
	     (raise-exception-lists-are-empty-or-of-different-length (__who__) ell*)))
       (values car* cdr*)))))

(case-define gather-cars-in-reverse
  ((ell*)
   (gather-cars-in-reverse ell* '()))
  ((ell* car*-tail)
   ;;The argument ELL* must be a list of lists of equal length.
   ;;
   ;;The call:
   ;;
   ;;   (gather-cars-in-reverse '((1 2 3)
   ;;                             (4 5 6)
   ;;                             (7 8 9))
   ;;
   ;;returns:
   ;;
   ;;   ((3 6 9)
   ;;    (2 5 8)
   ;;    (1 4 7))
   ;;
   (let loop ((car**	'())
	      (ell*	ell*))
     (if (list-of-nulls? ell*)
	 car**
       (receive (car* cdr*)
	   (cars-and-cdrs ell* car*-tail)
	 (loop (cons car* car**) cdr*))))))

(define* (butlast-and-last ell)
  ;;The argument ELL must be a proper list of 1 or more items. Return two values:
  ;;
  ;;1. A new list holding all the items in ELL, but the last one.
  ;;
  ;;2. The last item in ELL.
  ;;
  (cond ((null? ell)
	 (raise-exception-list-is-empty (__who__) ell))
	((pair? ell)
	 (let ((next (cdr ell)))
	   (if (pair? next)
	       (let* ((head-items (cons (car ell) '())))
		 (let loop ((ell	next)
			    (last-pair  head-items))
		   (if (pair? ell)
		       (let ((next (cdr ell)))
			 (if (pair? next)
			     (let ((P (cons (car ell) '())))
			       ($set-cdr! last-pair P)
			       (loop next P))
			   (values head-items (car ell))))
		     (values head-items (void)))))
	     (values '() (car ell)))))
	(else
	 (assertion-violation (__who__) "expected proper list as argument" ell))))


;;;; folding functions

(define ($fold-left/1 combine knil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell)
      (if (null? (cdr ell))
	  ;;Perform a tail call to COMBINE for the last element.
	  (combine knil (car ell))
	($fold-left/1 combine (combine knil (car ell)) (cdr ell)))
    knil))

(define ($fold-left/2 combine knil ell1 ell2)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell1)
      (if (null? (cdr ell1))
	  ;;Perform a tail call to COMBINE for the last element.
	  (combine knil (car ell1) (car ell2))
	($fold-left/2 combine (combine knil (car ell1) (car ell2)) (cdr ell1) (cdr ell2)))
    knil))

(define ($fold-left/3 combine knil ell1 ell2 ell3)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (pair? ell1)
      (if (null? (cdr ell1))
	  ;;Perform a tail call to COMBINE for the last element.
	  (combine knil (car ell1) (car ell2) (car ell3))
	($fold-left/3 combine
	  (combine knil (car ell1) (car ell2) (car ell3))
	  (cdr ell1) (cdr ell2) (cdr ell3)))
    knil))

(define ($fold-left/list combine knil ell*)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (if (or (null? ell*)
	  (null? (car ell*)))
      knil
    (receive (car* cdr*)
	(cars-and-cdrs ell*)
      (if (or (null? cdr*)
	      (null? (car cdr*)))
	  ;;Perform a tail call to COMBINE for the last elements.
	  (apply combine knil car*)
	($fold-left/list combine
	  (apply combine knil car*)
	  cdr*)))))

;;; --------------------------------------------------------------------

(define ($fold-right/1 combine knil ell)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((knil	knil)
	     (rev-ell	(reverse ell)))
    (if (pair? rev-ell)
	(if (null? (cdr rev-ell))
	    ;;Perform a tail call to COMBINE for the last element.
	    (combine (car rev-ell) knil)
	  (loop (combine (car rev-ell) knil)
		(cdr rev-ell)))
      knil)))

(define ($fold-right/2 combine knil ell1 ell2)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((knil	knil)
	     (cars*	(gather-cars-in-reverse (list ell1 ell2))))
    (if (pair? cars*)
	(if (null? (cdr cars*))
	    ;;Perform a tail call to COMBINE for the last element.
	    (apply combine (append (car cars*) (list knil)))
	  (loop (apply combine (append (car cars*) (list knil)))
		(cdr cars*)))
      knil)))

(define ($fold-right/3 combine knil ell1 ell2 ell3)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((knil	knil)
	     (cars*	(gather-cars-in-reverse (list ell1 ell2 ell3))))
    (if (pair? cars*)
	(if (null? (cdr cars*))
	    ;;Perform a tail call to COMBINE for the last element.
	    (apply combine (append (car cars*) (list knil)))
	  (loop (apply combine (append (car cars*) (list knil)))
		(cdr cars*)))
      knil)))

(define ($fold-right/list combine knil ell*)
  ;;NOTE Let's avoid doing this with a non-tail recursion!!!
  ;;
  (let loop ((knil	knil)
	     (cars*	(gather-cars-in-reverse ell*)))
    (if (pair? cars*)
	(if (null? (cdr cars*))
	    ;;Perform a tail call to COMBINE for the last element.
	    (apply combine (append (car cars*) (list knil)))
	  (loop (apply combine (append (car cars*) (list knil)))
		(cdr cars*)))
      knil)))

;;; --------------------------------------------------------------------

(define-list-folder fold-left
  $fold-left/1
  $fold-left/2
  $fold-left/3
  $fold-left/list)

(define-list-folder fold-right
  $fold-right/1
  $fold-right/2
  $fold-right/3
  $fold-right/list)


;;;; mapping functions

(define ($map/1 func ell)
  ($fold-right/1 (lambda (item nil)
		   (cons (func item) nil))
    '() ell))

(define ($map/2 func ell1 ell2)
  ($fold-right/2 (lambda (item1 item2 nil)
		   (cons (func item1 item2) nil))
    '() ell1 ell2))

(define ($map/3 func ell1 ell2 ell3)
  ($fold-right/3 (lambda (item1 item2 item3 nil)
		   (cons (func item1 item2 item3) nil))
    '() ell1 ell2 ell3))

(define ($map/list func ell*)
  ($fold-right/list (lambda args
		      (receive (item* nil)
			  (butlast-and-last args)
			(cons (apply func item*)
			      nil)))
    '() ell*))

;;; --------------------------------------------------------------------

(define ($for-each/1 func ell)
  ($fold-left/1 (lambda (knil item)
		  (func item)
		  knil)
    (void) ell))

(define ($for-each/2 func ell1 ell2)
  ($fold-left/2 (lambda (knil item1 item2)
		  (func item1 item2)
		  knil)
    (void) ell1 ell2))

(define ($for-each/3 func ell1 ell2 ell3)
  ($fold-left/3 (lambda (knil item1 item2 item3)
		  (func item1 item2 item3)
		  knil)
    (void) ell1 ell2 ell3))

(define ($for-each/list func ell*)
  ($fold-left/list (lambda (knil . item*)
		     (apply func item*)
		     knil)
    (void) ell*))

;;; --------------------------------------------------------------------

(define $for-each-in-order/1	$for-each/1)
(define $for-each-in-order/2	$for-each/2)
(define $for-each-in-order/3	$for-each/3)
(define $for-each-in-order/list	$for-each/list)

;;; --------------------------------------------------------------------

(define ($map-in-order/1 func ell)
  (reverse ($fold-left/1 (lambda (knil item)
			   (cons (func item) knil))
	     '() ell)))

(define ($map-in-order/2 func ell1 ell2)
  (reverse ($fold-left/2 (lambda (knil item1 item2)
			   (cons (func item1 item2) knil))
	     '() ell1 ell2)))

(define ($map-in-order/3 func ell1 ell2 ell3)
  (reverse ($fold-left/3 (lambda (knil item1 item2 item3)
			   (cons (func item1 item2 item3) knil))
	     '() ell1 ell2 ell3)))

(define ($map-in-order/list func ell*)
  (reverse ($fold-left/list (lambda (knil . item*)
			      (cons (apply func item*) knil))
	     '() ell*)))

;;; --------------------------------------------------------------------

(define-list-mapper map-in-order
  $map-in-order/1
  $map-in-order/2
  $map-in-order/3
  $map-in-order/list)

(define-list-mapper for-each-in-order
  $for-each-in-order/1
  $for-each-in-order/2
  $for-each-in-order/3
  $for-each-in-order/list)


;;;; search functions

(define ($for-all/1 pred ell)
  (or (null? ell)
      (if (null? (cdr ell))
	  ;;Perform a tail call for the last item.
	  (if (pred (car ell))
	      #t
	    #f)
	(and (pred (car ell))
	     ($for-all/1 pred (cdr ell))))))

(define ($for-all/2 pred ell1 ell2)
  (or (null? ell1)
      (and (pair? ell1)
	   (if (null? (cdr ell1))
	       ;;Perform a tail call for the last item.
	       (if (pred (car ell1) (car ell2))
		   #t
		 #f)
	     (and (pred (car ell1) (car ell2))
		  ($for-all/2 pred (cdr ell1) (cdr ell2)))))))

(define ($for-all/3 pred ell1 ell2 ell3)
  (or (null? ell1)
      (and (pair? ell1)
	   (if (null? (cdr ell1))
	       ;;Perform a tail call for the last item.
	       (if (pred (car ell1) (car ell2) (car ell3))
		   #t
		 #f)
	     (and (pred (car ell1) (car ell2) (car ell3))
		  ($for-all/3 pred (cdr ell1) (cdr ell2) (cdr ell3)))))))

(define ($for-all/list pred ell*)
  (or (null? ell*)
      (null? (car ell*))
      (receive (car* cdr*)
	  (cars-and-cdrs ell*)
	(if (null? (car cdr*))
	    ;;Perform a tail call for the last item.
	    (if (apply pred car*)
		#t
	      #f)
	  (and (apply pred car*)
	       ($for-all/list pred cdr*))))))

;;; --------------------------------------------------------------------

(define ($exists/1 pred ell)
  (if (null? ell)
      #f
    (and (pair? ell)
	 (if (null? (cdr ell))
	     ;;Perform a tail call for the last item.
	     (pred (car ell))
	   (or (pred (car ell))
	       ($exists/1 pred (cdr ell)))))))

(define ($exists/2 pred ell1 ell2)
  (if (null? ell1)
      #f
    (and (pair? ell1)
	 (if (null? (cdr ell1))
	     ;;Perform a tail call for the last items.
	     (pred (car ell1) (car ell2))
	   (or (pred (car ell1) (car ell2))
	       ($exists/2 pred (cdr ell1) (cdr ell2)))))))

(define ($exists/3 pred ell1 ell2 ell3)
  (if (null? ell1)
      #f
    (and (pair? ell1)
	 (if (null? (cdr ell1))
	     ;;Perform a tail call for the last item.
	     (pred (car ell1) (car ell2) (car ell3))
	   (or (pred (car ell1) (car ell2) (car ell3))
	       ($exists/3 pred (cdr ell1) (cdr ell2) (cdr ell3)))))))

(define ($exists/list pred ell*)
  (if (or (null? ell*)
	  (null? (car ell*)))
      #f
    (receive (car* cdr*)
	(cars-and-cdrs ell*)
      (if (null? (car cdr*))
	  ;;Perform a tail call for the last item.
	  (apply pred car*)
	(or (apply pred car*)
	    ($exists/list pred cdr*))))))

;;; --------------------------------------------------------------------

(define ($find pred ell)
  (and (pair? ell)
       (let ((item (car ell)))
	 (if (pred item)
	     item
	   ($find pred (cdr ell))))))

;;; --------------------------------------------------------------------

(define* (find pred ell)
  (begin-checks
    (assert-argument-type (__who__) "procedure" procedure? pred 1)
    (assert-argument-type (__who__) "list"      list?      ell  2))
  ($find pred ell))

(define-list-searcher for-all
  $for-all/1
  $for-all/2
  $for-all/3
  $for-all/list)

(define-list-searcher exists
  $exists/1
  $exists/2
  $exists/3
  $exists/list)


;;;; done

#| end of module |# )

;;; end of file
