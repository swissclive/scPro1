; Change AutoCAD Dimension Styles.  By Clive Hunt, June 2014.
(defun dimfix ()
	(command "CMDECHO" "0")
	(if (ssget "X" '((0 . "DIMENSION")))
		(progn
			(setq entList (ssget "X" '((0 . "DIMENSION"))))
			(setq count -1)
			(while
				(setq ent (ssname entList (setq count (1+ count))))
					(setq entField (entget ent))
					;get dimension style of object
					(setq dimSty (cdr (assoc 3 entField)))
					;set current dimension style to that of object (as parsing text style used by selected dimension only shows text style used by current dimension style)
					(command "-DIMSTYLE" "R" dimSty)
					;find text style used by current dimension style (and selected object)
					(setq texSty (cdr (assoc 2 (entget (cdr (assoc 340 (tblsearch "dimstyle" dimSty)))))))
					(setq texHgt (cdr (assoc 40 (tblsearch "STYLE" texSty))))
					;calculate viewport scale based on the ratio of the text height sent from Revit and the printed height of 1/8"
					(setq exScl (/ texHgt 0.125))
					(print exScl)
					;find nearest match to inaccurate scale exported from Revit
					(setq nrScl (sort exScl))
					(setq newStyle (cons 3 "ST-Architectural"))
					(setq entField (subst newStyle (assoc 3 entField) entField))
					(entmod entField)
					(command "-DIMSTYLE" "R" "ST-Architectural")
					;set scale factor for selected dim to required viewport value
					(command "DIMSCALE" nrScl)
					(command "DIM" "UPDATE" ent "" "EXIT")					
			)
			(command "CMDECHO" "1")
			princ
		)
	)
)


;script to find nearest match to a given list of scales.
(defun sort (nu)
	(setq li (list 1 4 8 12 16 24 48 96 240 360 600))
	;preset difference between actual scale and list value to a high figure so it's immediately overwritten.
	(setq diff 1000)
	;set counter to 0
	(setq ct 0)
	(while (< ct (length li))
		(setq d (abs (- nu (nth ct li))))
		(if (< d diff)
			(setq near (nth ct li) diff d)
		)
		(setq ct (1+ ct))
		)
	(princ near)
)
