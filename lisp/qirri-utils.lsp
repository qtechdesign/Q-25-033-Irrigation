;;; ============================================================================
;;; QTECH IRRIGATION - Utility Functions
;;; Common utilities, layer management, and helper functions
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Layer Management
;;; ----------------------------------------------------------------------------

(defun qtech:create-layers (/ layer-data)
  "Create standard irrigation layers"
  (setq layer-data
    '(("IRR-AREA"       8  "Continuous" "Irrigation area boundaries")
      ("IRR-OBSTACLE"   1  "Continuous" "Obstacles and exclusion zones")
      ("IRR-SPRINKLER"  3  "Continuous" "Sprinkler head symbols")
      ("IRR-PATTERN"    4  "Continuous" "Spray patterns")
      ("IRR-PIPE-MAIN"  5  "Continuous" "Main line piping")
      ("IRR-PIPE-LAT"   6  "Continuous" "Lateral piping")
      ("IRR-VALVE"      2  "Continuous" "Valves and controllers")
      ("IRR-ZONE-1"     30 "Continuous" "Zone 1 elements")
      ("IRR-ZONE-2"     40 "Continuous" "Zone 2 elements")
      ("IRR-ZONE-3"     50 "Continuous" "Zone 3 elements")
      ("IRR-ZONE-4"     60 "Continuous" "Zone 4 elements")
      ("IRR-ZONE-5"     70 "Continuous" "Zone 5 elements")
      ("IRR-TEXT"       7  "Continuous" "Labels and annotations")
      ("IRR-DIM"        7  "Continuous" "Dimensions")
    )
  )
  (foreach lyr layer-data
    (qtech:make-layer (nth 0 lyr) (nth 1 lyr) (nth 2 lyr))
  )
)

(defun qtech:make-layer (name color ltype / layer-obj)
  "Create a layer if it doesn't exist"
  (if (not (tblsearch "LAYER" name))
    (progn
      (command "._-LAYER" "_Make" name "_Color" color name "_LType" ltype name "")
      T
    )
    nil
  )
)

(defun qtech:set-layer (name)
  "Set current layer, creating if necessary"
  (if (not (tblsearch "LAYER" name))
    (qtech:make-layer name 7 "Continuous")
  )
  (setvar "CLAYER" name)
)

;;; ----------------------------------------------------------------------------
;;; Point and Geometry Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:point-2d (pt)
  "Convert a 3D point to 2D"
  (list (car pt) (cadr pt))
)

(defun qtech:distance-2d (p1 p2)
  "Calculate 2D distance between two points"
  (sqrt (+ (expt (- (car p2) (car p1)) 2)
           (expt (- (cadr p2) (cadr p1)) 2)))
)

(defun qtech:midpoint (p1 p2)
  "Calculate midpoint between two points"
  (list (/ (+ (car p1) (car p2)) 2.0)
        (/ (+ (cadr p1) (cadr p2)) 2.0))
)

(defun qtech:angle-2d (p1 p2)
  "Calculate angle from p1 to p2 in radians"
  (atan (- (cadr p2) (cadr p1))
        (- (car p2) (car p1)))
)

(defun qtech:polar-point (pt ang dist)
  "Calculate point at angle and distance from base point"
  (list (+ (car pt) (* dist (cos ang)))
        (+ (cadr pt) (* dist (sin ang))))
)

(defun qtech:radians-to-degrees (rad)
  "Convert radians to degrees"
  (* rad (/ 180.0 pi))
)

(defun qtech:degrees-to-radians (deg)
  "Convert degrees to radians"
  (* deg (/ pi 180.0))
)

(defun qtech:normalize-angle (ang)
  "Normalize angle to 0-2pi range"
  (while (< ang 0) (setq ang (+ ang (* 2 pi))))
  (while (>= ang (* 2 pi)) (setq ang (- ang (* 2 pi))))
  ang
)

;;; ----------------------------------------------------------------------------
;;; Polyline Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:get-pline-vertices (ent / obj pts i)
  "Get all vertices of a polyline entity"
  (setq obj (vlax-ename->vla-object ent))
  (setq pts '())
  (if (vlax-property-available-p obj 'Coordinates)
    (progn
      (setq coords (vlax-get obj 'Coordinates))
      (setq i 0)
      (while (< i (length coords))
        (setq pts (append pts (list (list (nth i coords) (nth (1+ i) coords)))))
        (setq i (+ i 2))
      )
    )
  )
  pts
)

(defun qtech:pline-length (ent / obj)
  "Get the length of a polyline"
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'Length)
    (vlax-get obj 'Length)
    0.0
  )
)

(defun qtech:pline-area (ent / obj)
  "Get the area enclosed by a polyline"
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'Area)
    (vlax-get obj 'Area)
    0.0
  )
)

(defun qtech:is-closed-pline (ent / obj)
  "Check if polyline is closed"
  (setq obj (vlax-ename->vla-object ent))
  (if (vlax-property-available-p obj 'Closed)
    (= (vlax-get obj 'Closed) :vlax-true)
    nil
  )
)

(defun qtech:pline-centroid (vertices / n sumx sumy)
  "Calculate centroid of a polygon defined by vertices"
  (setq n (length vertices))
  (setq sumx 0.0 sumy 0.0)
  (foreach v vertices
    (setq sumx (+ sumx (car v)))
    (setq sumy (+ sumy (cadr v)))
  )
  (list (/ sumx n) (/ sumy n))
)

(defun qtech:pline-bounds (vertices / minx miny maxx maxy)
  "Get bounding box of polygon vertices"
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
  "Test if a point is inside a polygon using ray casting algorithm"
  (setq n (length vertices))
  (setq inside nil)
  (setq x (car pt) y (cadr pt))
  (setq j (1- n))
  (setq i 0)
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
;;; Line and Edge Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:offset-point (pt prev-pt next-pt offset-dist / ang1 ang2 bisect-ang)
  "Calculate offset point at a vertex (inward)"
  (setq ang1 (qtech:angle-2d pt prev-pt))
  (setq ang2 (qtech:angle-2d pt next-pt))
  (setq bisect-ang (/ (+ ang1 ang2) 2.0))
  ;; Offset inward (perpendicular to bisector)
  (qtech:polar-point pt (+ bisect-ang (/ pi 2)) offset-dist)
)

(defun qtech:perpendicular-offset (p1 p2 pt offset-dist / ang)
  "Calculate perpendicular offset from a line segment"
  (setq ang (qtech:angle-2d p1 p2))
  (qtech:polar-point pt (+ ang (/ pi 2)) offset-dist)
)

(defun qtech:line-intersection (p1 p2 p3 p4 / x1 y1 x2 y2 x3 y3 x4 y4 denom t1)
  "Find intersection point of two line segments"
  (setq x1 (car p1) y1 (cadr p1))
  (setq x2 (car p2) y2 (cadr p2))
  (setq x3 (car p3) y3 (cadr p3))
  (setq x4 (car p4) y4 (cadr p4))
  (setq denom (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4))))
  (if (not (zerop denom))
    (progn
      (setq t1 (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) denom))
      (list (+ x1 (* t1 (- x2 x1)))
            (+ y1 (* t1 (- y2 y1))))
    )
    nil
  )
)

;;; ----------------------------------------------------------------------------
;;; Grid Generation
;;; ----------------------------------------------------------------------------

(defun qtech:generate-grid (bounds resolution / minpt maxpt x y pts)
  "Generate a grid of points within bounds"
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
  "Filter grid points to only those inside the polygon"
  (vl-remove-if-not
    '(lambda (pt) (qtech:point-in-polygon pt vertices))
    pts
  )
)

;;; ----------------------------------------------------------------------------
;;; Entity Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:get-entity-type (ent)
  "Get the type of an entity"
  (cdr (assoc 0 (entget ent)))
)

(defun qtech:is-polyline (ent)
  "Check if entity is a polyline"
  (member (qtech:get-entity-type ent) '("LWPOLYLINE" "POLYLINE"))
)

(defun qtech:select-closed-polyline (prompt / ent etype)
  "Prompt user to select a closed polyline"
  (princ (strcat "\n" prompt))
  (setq ent (car (entsel)))
  (if ent
    (if (qtech:is-polyline ent)
      (if (qtech:is-closed-pline ent)
        ent
        (progn (princ "\nPolyline must be closed.") nil)
      )
      (progn (princ "\nPlease select a polyline.") nil)
    )
    (progn (princ "\nNo entity selected.") nil)
  )
)

;;; ----------------------------------------------------------------------------
;;; Attribute Utilities (for blocks)
;;; ----------------------------------------------------------------------------

(defun qtech:get-block-attributes (ent / obj atts result att-name att-val)
  "Get all attributes from a block as association list"
  (setq obj (vlax-ename->vla-object ent))
  (setq result '())
  (if (= (vlax-get obj 'HasAttributes) :vlax-true)
    (progn
      (setq atts (vlax-invoke obj 'GetAttributes))
      (foreach att (vlax-safearray->list (vlax-variant-value atts))
        (setq att-name (vlax-get att 'TagString))
        (setq att-val (vlax-get att 'TextString))
        (setq result (append result (list (cons att-name att-val))))
      )
    )
  )
  result
)

(defun qtech:set-block-attribute (ent tag-name new-value / obj atts att)
  "Set a specific attribute value in a block"
  (setq obj (vlax-ename->vla-object ent))
  (if (= (vlax-get obj 'HasAttributes) :vlax-true)
    (progn
      (setq atts (vlax-invoke obj 'GetAttributes))
      (foreach att (vlax-safearray->list (vlax-variant-value atts))
        (if (= (strcase (vlax-get att 'TagString)) (strcase tag-name))
          (vlax-put att 'TextString new-value)
        )
      )
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Math Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:round (num decimals / mult)
  "Round a number to specified decimal places"
  (setq mult (expt 10 decimals))
  (/ (fix (+ (* num mult) 0.5)) (float mult))
)

(defun qtech:clamp (val minval maxval)
  "Clamp a value between min and max"
  (max minval (min maxval val))
)

(defun qtech:lerp (a b t-val)
  "Linear interpolation between a and b"
  (+ a (* t-val (- b a)))
)

;;; ----------------------------------------------------------------------------
;;; List Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:list-sum (lst)
  "Sum all numbers in a list"
  (apply '+ lst)
)

(defun qtech:list-avg (lst)
  "Calculate average of a list"
  (if lst
    (/ (qtech:list-sum lst) (float (length lst)))
    0.0
  )
)

(defun qtech:list-min (lst)
  "Find minimum value in a list"
  (apply 'min lst)
)

(defun qtech:list-max (lst)
  "Find maximum value in a list"
  (apply 'max lst)
)

(defun qtech:flatten (lst)
  "Flatten a nested list"
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    (T (append (qtech:flatten (car lst)) (qtech:flatten (cdr lst))))
  )
)

;;; ----------------------------------------------------------------------------
;;; String Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:string-split (str delim / pos result)
  "Split a string by delimiter"
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos))))
    (setq str (substr str (+ pos 2)))
  )
  (append result (list str))
)

(defun qtech:string-trim (str)
  "Trim whitespace from both ends of a string"
  (vl-string-trim " \t\n\r" str)
)

(defun qtech:string-pad (str len pad-char / result)
  "Pad a string to specified length"
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
  "Read a CSV file into a list of lists"
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
  "Write a list of lists to a CSV file"
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
  "Convert a list to a CSV line"
  (setq result "")
  (foreach item lst
    (if (/= result "") (setq result (strcat result ",")))
    (setq result (strcat result (qtech:value-to-csv item)))
  )
  result
)

(defun qtech:value-to-csv (val)
  "Convert a value to CSV-safe string"
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

;;; ----------------------------------------------------------------------------
;;; Progress Indicator
;;; ----------------------------------------------------------------------------

(defun qtech:progress-init (msg total)
  "Initialize progress indicator"
  (setq *qtech-progress-msg* msg)
  (setq *qtech-progress-total* total)
  (setq *qtech-progress-current* 0)
  (princ (strcat "\n" msg " [0%]"))
)

(defun qtech:progress-update (current / pct)
  "Update progress indicator"
  (setq *qtech-progress-current* current)
  (setq pct (fix (* 100.0 (/ (float current) *qtech-progress-total*))))
  (princ (strcat "\r" *qtech-progress-msg* " [" (itoa pct) "%]"))
)

(defun qtech:progress-complete ()
  "Complete progress indicator"
  (princ (strcat "\r" *qtech-progress-msg* " [100%] - Complete\n"))
)

;;; ============================================================================
;;; End of qtech-utils.lsp
;;; ============================================================================

(princ)

