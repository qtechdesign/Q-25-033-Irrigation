;;; ============================================================================
;;; QIRRI - Utility Functions (Mac + Windows Compatible)
;;; Pure AutoLISP - no ActiveX/COM dependencies
;;; Copyright (c) 2026 QTech Design - www.qtech.hr
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; UNIT DETECTION & CONVERSION
;;; ----------------------------------------------------------------------------

(setq *qirri-units* nil)           ; Detected units ("mm", "cm", "m", "in", "ft")
(setq *qirri-scale-factor* 1.0)    ; Scale to convert drawing units to meters

(defun qtech:detect-units (/ insunits lunits)
  "Detect drawing units and set scale factor"
  (setq insunits (getvar "INSUNITS"))
  (setq lunits (getvar "LUNITS"))
  
  ;; INSUNITS values:
  ;; 0 = Unitless, 1 = Inches, 2 = Feet, 3 = Miles
  ;; 4 = Millimeters, 5 = Centimeters, 6 = Meters, 7 = Kilometers
  
  (cond
    ((= insunits 1)  ; Inches
     (setq *qirri-units* "in")
     (setq *qirri-scale-factor* 0.0254))    ; 1 inch = 0.0254 m
    ((= insunits 2)  ; Feet
     (setq *qirri-units* "ft")
     (setq *qirri-scale-factor* 0.3048))    ; 1 foot = 0.3048 m
    ((= insunits 4)  ; Millimeters
     (setq *qirri-units* "mm")
     (setq *qirri-scale-factor* 0.001))     ; 1 mm = 0.001 m
    ((= insunits 5)  ; Centimeters
     (setq *qirri-units* "cm")
     (setq *qirri-scale-factor* 0.01))      ; 1 cm = 0.01 m
    ((= insunits 6)  ; Meters
     (setq *qirri-units* "m")
     (setq *qirri-scale-factor* 1.0))
    ((= insunits 7)  ; Kilometers
     (setq *qirri-units* "km")
     (setq *qirri-scale-factor* 1000.0))
    (T  ; Unitless or unknown - try to auto-detect from extents
     (qtech:auto-detect-units))
  )
  
  (princ (strcat "\nUnits detected: " *qirri-units* 
                 " (scale factor: " (rtos *qirri-scale-factor* 2 6) ")"))
  *qirri-scale-factor*
)

(defun qtech:auto-detect-units (/ ext-min ext-max width height max-dim)
  "Try to guess units from drawing extents"
  (setq ext-min (getvar "EXTMIN"))
  (setq ext-max (getvar "EXTMAX"))
  
  (if (and ext-min ext-max)
    (progn
      (setq width (- (car ext-max) (car ext-min)))
      (setq height (- (cadr ext-max) (cadr ext-min)))
      (setq max-dim (max width height))
      
      (cond
        ;; If max dimension > 1000, likely millimeters
        ((> max-dim 1000)
         (setq *qirri-units* "mm")
         (setq *qirri-scale-factor* 0.001)
         (princ "\nAuto-detected: Millimeters (extents > 1000)"))
        ;; If max dimension > 100, likely centimeters
        ((> max-dim 100)
         (setq *qirri-units* "cm")
         (setq *qirri-scale-factor* 0.01)
         (princ "\nAuto-detected: Centimeters (extents > 100)"))
        ;; Otherwise, assume meters
        (T
         (setq *qirri-units* "m")
         (setq *qirri-scale-factor* 1.0)
         (princ "\nAuto-detected: Meters"))
      )
    )
    ;; Can't detect - default to meters
    (progn
      (setq *qirri-units* "m")
      (setq *qirri-scale-factor* 1.0)
    )
  )
)

(defun qtech:to-meters (value)
  "Convert drawing units to meters"
  (* value *qirri-scale-factor*)
)

(defun qtech:from-meters (value)
  "Convert meters to drawing units"
  (/ value *qirri-scale-factor*)
)

(defun qtech:format-area (area-sq-drawing-units / area-m2)
  "Format area in appropriate units"
  (setq area-m2 (* area-sq-drawing-units (* *qirri-scale-factor* *qirri-scale-factor*)))
  (cond
    ((>= area-m2 10000)
     (strcat (rtos (/ area-m2 10000.0) 2 2) " ha"))
    (T
     (strcat (rtos area-m2 2 1) " m²"))
  )
)

(defun qtech:format-length (length-drawing-units / length-m)
  "Format length in appropriate units"
  (setq length-m (* length-drawing-units *qirri-scale-factor*))
  (strcat (rtos length-m 2 2) " m")
)

(defun c:QIRRUNITS (/ choice)
  "Set drawing units manually"
  (princ "\n=== DRAWING UNITS ===")
  (princ (strcat "\nCurrent: " (if *qirri-units* *qirri-units* "Not set")))
  (princ (strcat " (scale: " (rtos *qirri-scale-factor* 2 6) ")"))
  (princ "\n")
  (princ "\n1. Millimeters (mm)")
  (princ "\n2. Centimeters (cm)")
  (princ "\n3. Meters (m)")
  (princ "\n4. Inches (in)")
  (princ "\n5. Feet (ft)")
  (princ "\n6. Auto-detect")
  (princ "\n")
  
  (setq choice (getstring "\nSelect [1-6]: "))
  
  (cond
    ((= choice "1")
     (setq *qirri-units* "mm" *qirri-scale-factor* 0.001))
    ((= choice "2")
     (setq *qirri-units* "cm" *qirri-scale-factor* 0.01))
    ((= choice "3")
     (setq *qirri-units* "m" *qirri-scale-factor* 1.0))
    ((= choice "4")
     (setq *qirri-units* "in" *qirri-scale-factor* 0.0254))
    ((= choice "5")
     (setq *qirri-units* "ft" *qirri-scale-factor* 0.3048))
    ((= choice "6")
     (qtech:detect-units))
    (T (princ "\nCancelled."))
  )
  
  (if *qirri-units*
    (princ (strcat "\nUnits set to: " *qirri-units* 
                   " (1 " *qirri-units* " = " (rtos *qirri-scale-factor* 2 6) " m)"))
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Layer Management
;;; ----------------------------------------------------------------------------

(defun qtech:create-layers (/ layer-data)
  "Create standard irrigation layers"
  (setq layer-data
    '(("IRR-AREA"       8  "Continuous")
      ("IRR-OBSTACLE"   1  "Continuous")
      ("IRR-SPRINKLER"  3  "Continuous")
      ("IRR-PATTERN"    4  "Continuous")
      ("IRR-PIPE-MAIN"  5  "Continuous")
      ("IRR-PIPE-LAT"   6  "Continuous")
      ("IRR-VALVE"      2  "Continuous")
      ("IRR-ZONE-1"     30 "Continuous")
      ("IRR-ZONE-2"     40 "Continuous")
      ("IRR-ZONE-3"     50 "Continuous")
      ("IRR-ZONE-4"     60 "Continuous")
      ("IRR-ZONE-5"     70 "Continuous")
      ("IRR-TEXT"       7  "Continuous")
      ("IRR-DIM"        7  "Continuous")
      ("IRR-GRID"       251 "Continuous")
    )
  )
  (foreach lyr layer-data
    (qtech:make-layer (nth 0 lyr) (nth 1 lyr) (nth 2 lyr))
  )
  (princ)
)

(defun qtech:make-layer (name color ltype)
  "Create a layer if it doesn't exist"
  (if (not (tblsearch "LAYER" name))
    (command "._-LAYER" "_Make" name "_Color" color name "_LType" ltype name "")
  )
)

(defun qtech:set-layer (name)
  "Set current layer"
  (if (not (tblsearch "LAYER" name))
    (qtech:make-layer name 7 "Continuous")
  )
  (setvar "CLAYER" name)
)

;;; ----------------------------------------------------------------------------
;;; Point and Geometry Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:point-2d (pt)
  "Convert to 2D point"
  (list (car pt) (cadr pt))
)

(defun qtech:distance-2d (p1 p2)
  "2D distance between points"
  (sqrt (+ (expt (- (car p2) (car p1)) 2)
           (expt (- (cadr p2) (cadr p1)) 2)))
)

(defun qtech:midpoint (p1 p2)
  "Midpoint between two points"
  (list (/ (+ (car p1) (car p2)) 2.0)
        (/ (+ (cadr p1) (cadr p2)) 2.0))
)

(defun qtech:angle-2d (p1 p2)
  "Angle from p1 to p2 in radians"
  (atan (- (cadr p2) (cadr p1))
        (- (car p2) (car p1)))
)

(defun qtech:polar-point (pt ang dist)
  "Point at angle and distance"
  (list (+ (car pt) (* dist (cos ang)))
        (+ (cadr pt) (* dist (sin ang))))
)

(defun qtech:radians-to-degrees (rad)
  (* rad (/ 180.0 pi))
)

(defun qtech:degrees-to-radians (deg)
  (* deg (/ pi 180.0))
)

(defun qtech:normalize-angle (ang)
  "Normalize angle to 0-2pi"
  (while (< ang 0) (setq ang (+ ang (* 2 pi))))
  (while (>= ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
  ang
)

;;; ----------------------------------------------------------------------------
;;; Polyline Utilities - Pure AutoLISP (Mac Compatible)
;;; ----------------------------------------------------------------------------

(defun qtech:get-pline-vertices (ent / ed pts vertex-data i coords)
  "Get vertices of LWPOLYLINE using entity data"
  (setq ed (entget ent))
  (setq pts '())
  
  (if (= (cdr (assoc 0 ed)) "LWPOLYLINE")
    ;; LWPOLYLINE - get all 10 codes
    (foreach item ed
      (if (= (car item) 10)
        (setq pts (append pts (list (cdr item))))
      )
    )
    ;; Old POLYLINE - traverse vertices
    (if (= (cdr (assoc 0 ed)) "POLYLINE")
      (progn
        (setq ent (entnext ent))
        (while (and ent 
                    (setq ed (entget ent))
                    (= (cdr (assoc 0 ed)) "VERTEX"))
          (setq pts (append pts (list (cdr (assoc 10 ed)))))
          (setq ent (entnext ent))
        )
      )
    )
  )
  
  ;; Convert to 2D if needed
  (mapcar 'qtech:point-2d pts)
)

(defun qtech:pline-length (ent / ed)
  "Get polyline length using AREA command"
  (command "._AREA" "_Object" ent)
  (getvar "PERIMETER")
)

(defun qtech:pline-area (ent / ed)
  "Get polyline area"
  (command "._AREA" "_Object" ent)
  (getvar "AREA")
)

(defun qtech:is-closed-pline (ent / ed flags)
  "Check if polyline is closed"
  (setq ed (entget ent))
  (cond
    ((= (cdr (assoc 0 ed)) "LWPOLYLINE")
     ;; Flag 70: bit 1 = closed
     (setq flags (cdr (assoc 70 ed)))
     (= (logand flags 1) 1))
    ((= (cdr (assoc 0 ed)) "POLYLINE")
     (setq flags (cdr (assoc 70 ed)))
     (= (logand flags 1) 1))
    (T nil)
  )
)

(defun qtech:pline-centroid (vertices / n sumx sumy)
  "Calculate centroid"
  (setq n (length vertices))
  (setq sumx 0.0 sumy 0.0)
  (foreach v vertices
    (setq sumx (+ sumx (car v)))
    (setq sumy (+ sumy (cadr v)))
  )
  (list (/ sumx n) (/ sumy n))
)

(defun qtech:pline-bounds (vertices / minx miny maxx maxy)
  "Get bounding box"
  (setq minx (caar vertices) miny (cadar vertices))
  (setq maxx minx maxy miny)
  (foreach v vertices
    (if (< (car v) minx) (setq minx (car v)))
    (if (> (car v) maxx) (setq maxx (car v)))
    (if (< (cadr v) miny) (setq miny (cadr v)))
    (if (> (cadr v) maxy) (setq maxy (cadr v)))
  )
  (list (list minx miny) (list maxx maxy))
)

;;; ----------------------------------------------------------------------------
;;; Point-in-Polygon Test
;;; ----------------------------------------------------------------------------

(defun qtech:point-in-polygon (pt vertices / n i j x y xi yi xj yj inside)
  "Test if point is inside polygon (ray casting)"
  (setq n (length vertices))
  (setq inside nil)
  (setq x (car pt) y (cadr pt))
  (setq j (1- n) i 0)
  
  (while (< i n)
    (setq xi (car (nth i vertices)) yi (cadr (nth i vertices)))
    (setq xj (car (nth j vertices)) yj (cadr (nth j vertices)))
    
    (if (and (or (and (<= yi y) (< y yj))
                 (and (<= yj y) (< y yi)))
             (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
      (setq inside (not inside))
    )
    (setq j i)
    (setq i (1+ i))
  )
  inside
)

;;; ----------------------------------------------------------------------------
;;; Grid Generation
;;; ----------------------------------------------------------------------------

(defun qtech:generate-grid (bounds resolution / minpt maxpt x y pts)
  "Generate grid of points"
  (setq minpt (car bounds) maxpt (cadr bounds))
  (setq pts '())
  (setq y (cadr minpt))
  (while (<= y (cadr maxpt))
    (setq x (car minpt))
    (while (<= x (car maxpt))
      (setq pts (append pts (list (list x y))))
      (setq x (+ x resolution))
    )
    (setq y (+ y resolution))
  )
  pts
)

(defun qtech:filter-points-in-polygon (pts vertices)
  "Filter points inside polygon"
  (vl-remove-if-not
    '(lambda (pt) (qtech:point-in-polygon pt vertices))
    pts
  )
)

;;; ----------------------------------------------------------------------------
;;; Entity Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:get-entity-type (ent)
  "Get entity type"
  (cdr (assoc 0 (entget ent)))
)

(defun qtech:is-polyline (ent)
  "Check if entity is polyline"
  (member (qtech:get-entity-type ent) '("LWPOLYLINE" "POLYLINE"))
)

(defun qtech:select-closed-polyline (prompt / ent)
  "Select a closed polyline"
  (princ (strcat "\n" prompt))
  (setq ent (car (entsel)))
  (cond
    ((not ent) 
     (princ "\nNo selection.") nil)
    ((not (qtech:is-polyline ent))
     (princ "\nNot a polyline.") nil)
    ((not (qtech:is-closed-pline ent))
     (princ "\nPolyline must be closed.") nil)
    (T ent)
  )
)

;;; ----------------------------------------------------------------------------
;;; Math Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:round (num decimals / mult)
  "Round number"
  (setq mult (expt 10 decimals))
  (/ (fix (+ (* num mult) 0.5)) (float mult))
)

(defun qtech:clamp (val minval maxval)
  "Clamp value"
  (max minval (min maxval val))
)

(defun qtech:lerp (a b t-val)
  "Linear interpolation"
  (+ a (* t-val (- b a)))
)

;;; ----------------------------------------------------------------------------
;;; List Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:list-sum (lst)
  "Sum list"
  (if lst (apply '+ lst) 0)
)

(defun qtech:list-avg (lst)
  "Average of list"
  (if lst (/ (qtech:list-sum lst) (float (length lst))) 0.0)
)

(defun qtech:list-min (lst)
  "Min of list"
  (if lst (apply 'min lst) 0)
)

(defun qtech:list-max (lst)
  "Max of list"
  (if lst (apply 'max lst) 0)
)

(defun qtech:list-take (lst n / result i)
  "Take first n elements"
  (setq result '() i 0)
  (while (and (< i n) (nth i lst))
    (setq result (append result (list (nth i lst))))
    (setq i (1+ i))
  )
  result
)

;;; ----------------------------------------------------------------------------
;;; String Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:string-split (str delim / pos result)
  "Split string"
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

(defun qtech:string-trim (str)
  "Trim whitespace"
  (vl-string-trim " \t\n\r" str)
)

(defun qtech:string-pad (str len pad-char / result)
  "Pad string"
  (setq result str)
  (while (< (strlen result) len)
    (setq result (strcat result pad-char))
  )
  result
)

;;; ----------------------------------------------------------------------------
;;; File Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:read-csv-file (filepath / f line result)
  "Read CSV file"
  (setq result '())
  (if (setq f (open filepath "r"))
    (progn
      (while (setq line (read-line f))
        (setq result (append result (list (qtech:string-split line ","))))
      )
      (close f)
    )
  )
  result
)

(defun qtech:write-csv-file (filepath data / f row)
  "Write CSV file"
  (if (setq f (open filepath "w"))
    (progn
      (foreach row data
        (write-line (qtech:list-to-csv-line row) f)
      )
      (close f)
      T
    )
    nil
  )
)

(defun qtech:list-to-csv-line (lst / result item)
  "List to CSV"
  (setq result "")
  (foreach item lst
    (if (/= result "") (setq result (strcat result ",")))
    (setq result (strcat result (qtech:value-to-csv item)))
  )
  result
)

(defun qtech:value-to-csv (val)
  "Value to CSV string"
  (cond
    ((= (type val) 'REAL) (rtos val 2 4))
    ((= (type val) 'INT) (itoa val))
    ((= (type val) 'STR)
     (if (vl-string-search "," val)
       (strcat "\"" val "\"")
       val
     ))
    (T (vl-princ-to-string val))
  )
)

;;; ============================================================================
;;; End of qirri-utils.lsp
;;; ============================================================================

;; Auto-detect units when loaded
(qtech:detect-units)

(princ "\n  qirri-utils.lsp loaded")
(princ)
