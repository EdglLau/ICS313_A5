;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-                       ;;;; Name: Edgar Lau                           Date: 2/28/14                                ;;;; Course: ICS313        Assignment:  4b                                                  ;;;; File: laue2_4.lisp        

(defparameter +ID+ "Edgar Lau")

;;;Prints name, ics course number, and assignments.
(defun ID (course assignment)
  "Prints name, ics course number, and assignment number"
  (cond 
   ((not (numberp course))
    (format t "Must be a number ~%")
    (terpri)
    ())
   ((not (numberp assignment))
    (format t "Must be a number ~%")
    (terpri)
    ())
   
   ((format t "Name: ~S~%Course: ICS ~D~%Assignment #~D"
          +ID+ course assignment))
))


;;;the game repl which takes users input, until user calls quit
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))

;;;reads what the user types
(defun game-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;;;commands which are allowed in game
(defparameter *allowed-commands* '(look walk pickup inventory))

;;;checks if the input is valid
(defun game-eval (sexp)
    (if (member (car sexp) *allowed-commands*)
        (eval sexp)
        '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

;;;prints things out to string
(defun game-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))

;;;Areas of the map
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))
                        (bedroom (you are in the bedroom.
                            there is a bed in front of you.))))

;;;Describes the location of the current location
(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;;;Shows the neighboring areas
(defparameter *edges* '((living-room (garden west door)  
                                     (attic upstairs ladder)
                                     (bedroom east door))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))
                        (bedroom (living-room west door))))

;;;Prints the path to other areas.
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;;Prints and appends all the locations nearby.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;;Objects lying in the game world.
(defparameter *objects* '(whiskey bucket frog chain key gem blade hilt scabbard))

;;;Location of the objects in the game world.
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)
                                   (key bedroom)
                                   (blade garden)
                                   (scabbard living-room)
                                   (gem attic)
                                   (hilt bedroom)))

;;;Checks if the object is at the location.
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;;Prints where the object is.
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

;;;Describes the location around the player, the paths, and the objects lying around
(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)   ;describes the paths
          (describe-objects *location* *objects* *object-locations*)))  ;describes the objects

;;;Moves player from one area to different one. 
(defun walk (direction)
  (labels ((correct-way (edge)
             (eq (cadr edge) direction)))
    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
      (if next 
          (progn (setf *location* (car next)) 
                 (look))
          '(you cannot go that way.)))))  ;if direction is invalid

;;;Pick an item up and add it to the inventory, then remove it from the game world.
(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.)))) ;if object doesn't exist

;;;Prints the items the player is holding.
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;;Checks if the player is holding an item. Returns nil if not.
(defun have (object) 
    (member object (cdr (inventory))))