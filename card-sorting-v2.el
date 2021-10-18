(defun sort-open ()
  (setq openlist (sort openlist #'(lambda (x y)  ;; <== Calling 'sort' fcn.
				    (<= (get-attribute x :h)
					(get-attribute y :h))))));; end of sort-open

(defun find-duplicate-child (st)
  (setq dup nil)
  (dolist (i openlist)
    (setq temp (get-attribute i :state))
    (if (equal temp st)
	(setq dup t)))
  (dolist (i closedlist)
    (setq temp (get-attribute i :state))
    (if (equal temp st)
	(setq dup t)))
  (equal dup t));; end of find-duplicate-child

(defun make-node (mother heur st)
  (list :id (gensym "n") :mom mother :h heur :state st));; end of make-node

(defun get-attribute (rnode att)
  ;;(message "get-attribute")
  (cadr (member att rnode)));; end of get-attribute

;; moves cards between rows
;; will only be called to execute legal moves
(defun move-card (src dest)
  (setq r1 (car current-state))
  (setq r2 (cadr current-state))
  (setq r3 (caddr current-state))
  
  (cond ((equal src 1)
	 (cond ((equal dest 2)
		(progn
		  (setq r2 (cons (car r1) r2))
		  (setq r1 (cdr r1))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))
	       ((equal dest 3)
		(progn
		  (setq r3 (cons (car r1) r3))
		  (setq r1 (cdr r1))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))))
	((equal src 2)
	 (cond ((equal dest 1)
		(progn
		  (setq r1 (cons (car r2) r1))
		  (setq r2 (cdr r2))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))
	       ((equal dest 3)
		(progn
		  (setq r3 (cons (car r2) r3))
		  (setq r2 (cdr r2))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))))
	((equal src 3)
	 (cond ((equal dest 2)
		(progn
		  (setq r2 (cons (car r3) r2))
		  (setq r3 (cdr r3))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))
	       ((equal dest 1)
		(progn
		  (setq r1 (cons (car r3) r1))
		  (setq r3 (cdr r3))
		  (setq newstate (cons r1 (cons r2 (cons r3 ()))))
		  (if (not (find-duplicate-child newstate))
		      (push (make-node nodename (heuristic newstate allcards) newstate) openlist))))))));; end of move-card

(defun find-legal-moves ()
  (setq numnil 0)
  (setq top1 (aref (symbol-name (car (car current-state))) 0))
  (setq top2 (aref (symbol-name (car (cadr current-state))) 0))
  (setq top3 (aref (symbol-name (car (caddr current-state))) 0))

  (dolist (i current-state) ;; find number of nil rows
    (if (equal (car i) nil)
	(setq numnil (+ numnil 1))))

  (cond ((equal numnil 0)                ; 0 empty rows
	 (if (> top1 top2)
	     (move-card 1 2))
	 (if (> top1 top3)
	     (move-card 1 3))
	 (if (> top2 top1)
	     (move-card 2 1))
	 (if (> top2 top3)
	     (move-card 2 3))
	 (if (> top3 top1)
	     (move-card 3 1))
	 (if (> top3 top2)
	     (move-card 3 2)))
	((equal numnil 1)                ; 1 empty rows
	 (if (equal 110 top1)
	     (progn
	       (move-card 2 1)
	       (move-card 3 1)
	       (if (> top2 top3)
		   (move-card 2 3)
		 (move-card 3 2))))
	 (if (equal 110 top2)
	     (progn
	       (move-card 1 2)
	       (move-card 3 2)
	       (if (> top1 top3)
		   (move-card 1 3)
		 (move-card 3 1))))
	 (if (equal 110 top3)
	     (progn
	       (move-card 1 3)
	       (move-card 2 3)
	       (if (> top1 top2)
		   (move-card 1 2)
		 (move-card 2 1)))))
	((equal numnil 2)                ; 2 empty rows | not necessary?????
	 (if (not (equal 110 top1))
	     (move-card 1 2)
	   (move-card 1 3))
	 (if (not (equal 110 top2))
	     (move-card 2 1)
	   (move-card 2 3))
	 (if (not (equal 110 top3))
	     (move-card 3 1)
	   (move-card 3 2)))));; end of find-legal-moves

(defun compare-positions (c rowstate rowgoal) ;; check if card is in final position
  (setq currpos 0)
  (setq goalpos 0)
  
  (setq found nil)
  (dolist (i (reverse rowstate))
    (if (not found)
	(progn
	  (setq found (equal i c))
	  (if (not found)
	      (setq currpos (+ currpos 1))))))
  
  (setq found nil)
  (dolist (i (reverse rowgoal))
    (if (not found)
	(progn
	  (setq found (equal i c))
	  (if (not found)
	      (setq goalpos (+ goalpos 1))))))
  (equal goalpos currpos));; end of compare-positions

(defun heuristic (s a) ;; s = state, a = allcards
  (setq ro1 (car s))
  (setq ro2 (cadr s))
  (setq ro3 (caddr s))
  (setq ac a) ;; get list of all cards
  
  (setq totalscore 0)
  (setq score 0)
  ;;(setq currpos 0)
  ;;(setq goalpos 0)
  (setq found nil)
  (setq rowfound ())
  
  (dolist (card ac) ;; check position for each card
    (if (not (member card ro1)) ;; if not in final row
	(setq score 1)
      (if (compare-positions card ro1 (car goal-state))
	  (setq score 0)
	(setq score 2)))

    ;;increment score if not 0
    (setq found nil)
    (if (not (equal score 0))
	(setq rowfound (find-row card ro1 ro2 ro3))
	(if (> (length rowfound) 1) ;;only increment if list is longer than 1
	    (progn
	      (dolist (i (reverse rowfound))
		;;(setq found (equal i card))
		(if (equal i card)
		    (setq found t))
		(if (and found (not (equal i card)))
		    (setq score (+ score 1)))))))
    
    (setq totalscore (+ totalscore score))
    (setq score 0))
  (+ totalscore 0));; end of heuristic

(defun find-row (c rowa rowb rowc) ;; returns the row that c is currently in
  (setq result ())
  (if (member c rowa)
      (setq result rowa)
    (if (member c rowb)
	(setq result rowb)
      (setq result rowc)))
  result);;find-row

(defun find-mom (node some-list)
  (setq mommy())
  (dolist (i some-list)
    (if (equal (get-attribute i :id) (get-attribute node :mom))
	(setq mommy i)))
  mommy)

(defun main (n start goal)
  (setq openlist ())
  (setq closedlist ())
  (setq current-node ())
  (setq ncards n)
  (if (equal n 5)
      (setq allcards '(A B C D E))
    (if (equal n 6)
	(setq allcards '(A B C D E F))
      (if (equal n 7)
	  (setq allcards '(A B C D E F G)))))
  (setq goal-state goal)

  ;; make mom node
  (setq current-state start)
  (setq heur (heuristic current-state allcards))

  ;; add first mom to open list
  (push (make-node nil heur current-state) openlist)

  (message (format "\nSorting %s Cards" n))

  (while (not (equal current-state goal-state))
    (setq current-node (car openlist)) ;; obtain top of open list
    (setq openlist (cdr openlist)) ;; remove top of open list
    (push current-node closedlist) ;; push top of open list to closed list

    (setq current-state (get-attribute current-node :state))
    (setq nodename (get-attribute current-node :id))
    
    (find-legal-moves) ;; find legal moves, create child nodes and push them onto openlist
    (sort-open) ;; sort openlist in ascending order by :h
    );; end of while
  ;;find all steps
  (setq steps ())
  (setq currentstep (car closedlist))
  (push currentstep steps)
  (while (get-attribute currentstep :mom)
    (setq m (find-mom currentstep closedlist))
    (push m steps)
    (setq currentstep m))
  
  (dolist (i steps)
    (message "%s" (get-attribute i :state)))
  (message "Nice! we found the goal\n")
  );; end of main

(main 5 '((C A)(E B)(D)) '((E D C B A)()()))
(main 6 '((F C A)(E B)(D)) '((F E D C B A)()()))
(main 7 '((E C A)(G B)(F D)) '((G F E D C B A)()()))



