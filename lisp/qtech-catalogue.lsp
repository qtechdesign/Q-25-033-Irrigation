;;; ============================================================================
;;; QTECH IRRIGATION - Sprinkler Catalogue
;;; Sprinkler data management and selection functions
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Catalogue Data Structure
;;; Each entry: (brand model nozzle radius flow precip pressure arcs efficiency)
;;; Units: radius(m), flow(m³/h), precip(mm/hr), pressure(bar)
;;; ----------------------------------------------------------------------------

(defun qtech:get-default-catalogue ()
  "Return default built-in sprinkler catalogue"
  '(
    ;; Rain Bird 5000 Series Rotors
    ("Rain Bird" "5004" "1.0"  4.6  0.08  12.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "1.5"  5.5  0.11  14.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "2.0"  6.4  0.15  15.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "3.0"  7.6  0.23  16.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "4.0"  8.5  0.32  18.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "5.0"  9.4  0.42  19.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "6.0"  10.4 0.53  20.0 3.0 (90 180 270 360) "standard")
    ("Rain Bird" "5004" "8.0"  11.3 0.76  24.0 3.0 (90 180 270 360) "standard")
    
    ;; Rain Bird 5000 High-Efficiency
    ("Rain Bird" "5004" "HE-VAN-08"  2.4  0.04  10.0 2.1 (90 180 270 360) "high")
    ("Rain Bird" "5004" "HE-VAN-10"  3.0  0.05  10.0 2.1 (90 180 270 360) "high")
    ("Rain Bird" "5004" "HE-VAN-12"  3.7  0.06  10.0 2.1 (90 180 270 360) "high")
    ("Rain Bird" "5004" "HE-VAN-15"  4.6  0.08  10.0 2.1 (90 180 270 360) "high")
    
    ;; Rain Bird 3500 Series
    ("Rain Bird" "3504" "0.5"  3.0  0.05  11.0 2.8 (90 180 270 360) "standard")
    ("Rain Bird" "3504" "1.0"  4.0  0.07  12.0 2.8 (90 180 270 360) "standard")
    ("Rain Bird" "3504" "1.5"  4.9  0.10  14.0 2.8 (90 180 270 360) "standard")
    ("Rain Bird" "3504" "2.0"  5.8  0.14  15.0 2.8 (90 180 270 360) "standard")
    
    ;; Rain Bird R-VAN Rotary Nozzles (High Efficiency)
    ("Rain Bird" "R-VAN" "14"   2.4-4.3 0.06  10.0 2.1 (90 180 270 360) "high")
    ("Rain Bird" "R-VAN" "18"   4.0-5.5 0.08  10.0 2.1 (90 180 270 360) "high")
    ("Rain Bird" "R-VAN" "24"   5.5-7.3 0.11  10.0 2.1 (90 180 270 360) "high")
    
    ;; Rain Bird 1800 Pop-up Spray Heads
    ("Rain Bird" "1804" "4VAN"   1.2  0.02  35.0 2.1 (0 90 180 270 360) "standard")
    ("Rain Bird" "1804" "6VAN"   1.8  0.03  35.0 2.1 (0 90 180 270 360) "standard")
    ("Rain Bird" "1804" "8VAN"   2.4  0.05  38.0 2.1 (0 90 180 270 360) "standard")
    ("Rain Bird" "1804" "10VAN"  3.0  0.07  40.0 2.1 (0 90 180 270 360) "standard")
    ("Rain Bird" "1804" "12VAN"  3.7  0.10  42.0 2.1 (0 90 180 270 360) "standard")
    ("Rain Bird" "1804" "15VAN"  4.6  0.14  45.0 2.1 (0 90 180 270 360) "standard")
    
    ;; Hunter PGP Series Rotors
    ("Hunter" "PGP-ADJ" "1.0"  4.9  0.10  13.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "1.5"  5.8  0.14  14.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "2.0"  6.7  0.17  15.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "3.0"  8.2  0.27  16.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "4.0"  9.4  0.38  17.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "5.0"  10.4 0.50  18.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "6.0"  11.6 0.65  20.0 3.5 (50 360) "standard")
    ("Hunter" "PGP-ADJ" "8.0"  13.1 0.91  22.0 3.5 (50 360) "standard")
    
    ;; Hunter PGJ Series (Smaller Rotors)
    ("Hunter" "PGJ" "Red"    5.2  0.12  14.0 2.8 (50 360) "standard")
    ("Hunter" "PGJ" "Green"  6.4  0.17  15.0 2.8 (50 360) "standard")
    ("Hunter" "PGJ" "Black"  7.6  0.23  16.0 2.8 (50 360) "standard")
    ("Hunter" "PGJ" "Gray"   8.5  0.30  17.0 2.8 (50 360) "standard")
    
    ;; Hunter MP Rotator (High Efficiency)
    ("Hunter" "MP Rotator" "MP1000-90"   2.4-4.5 0.04 10.0 2.8 (90) "high")
    ("Hunter" "MP Rotator" "MP1000-210"  2.4-4.5 0.04 10.0 2.8 (210) "high")
    ("Hunter" "MP Rotator" "MP1000-360"  2.4-4.5 0.04 10.0 2.8 (360) "high")
    ("Hunter" "MP Rotator" "MP2000-90"   4.0-6.4 0.07 10.0 2.8 (90) "high")
    ("Hunter" "MP Rotator" "MP2000-210"  4.0-6.4 0.07 10.0 2.8 (210) "high")
    ("Hunter" "MP Rotator" "MP2000-360"  4.0-6.4 0.07 10.0 2.8 (360) "high")
    ("Hunter" "MP Rotator" "MP3000-90"   6.4-9.1 0.11 10.0 2.8 (90) "high")
    ("Hunter" "MP Rotator" "MP3000-210"  6.4-9.1 0.11 10.0 2.8 (210) "high")
    ("Hunter" "MP Rotator" "MP3000-360"  6.4-9.1 0.11 10.0 2.8 (360) "high")
    ("Hunter" "MP Rotator" "MP3500-90"   9.1-10.7 0.14 10.0 2.8 (90) "high")
    ("Hunter" "MP Rotator" "MP3500-210"  9.1-10.7 0.14 10.0 2.8 (210) "high")
    ("Hunter" "MP Rotator" "MP3500-360"  9.1-10.7 0.14 10.0 2.8 (360) "high")
    
    ;; Hunter MP Side Strip
    ("Hunter" "MP Rotator" "MPSS530"  1.5-4.6 0.05 10.0 2.8 (180) "high")
    ("Hunter" "MP Rotator" "MPLCS515" 1.5-4.6 0.04 10.0 2.8 (180) "high")
    ("Hunter" "MP Rotator" "MPRCS515" 1.5-4.6 0.04 10.0 2.8 (180) "high")
    
    ;; Hunter Pro-Spray Fixed Spray
    ("Hunter" "Pro-Spray" "4A"    1.2  0.02  38.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "6A"    1.8  0.03  40.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "8A"    2.4  0.05  42.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "10A"   3.0  0.08  44.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "12A"   3.7  0.11  46.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "15A"   4.6  0.15  48.0 2.1 (360) "standard")
    ("Hunter" "Pro-Spray" "4Q"    1.2  0.01  38.0 2.1 (90) "standard")
    ("Hunter" "Pro-Spray" "6Q"    1.8  0.02  40.0 2.1 (90) "standard")
    ("Hunter" "Pro-Spray" "8Q"    2.4  0.03  42.0 2.1 (90) "standard")
    ("Hunter" "Pro-Spray" "4H"    1.2  0.01  38.0 2.1 (180) "standard")
    ("Hunter" "Pro-Spray" "6H"    1.8  0.02  40.0 2.1 (180) "standard")
    ("Hunter" "Pro-Spray" "8H"    2.4  0.03  42.0 2.1 (180) "standard")
    
    ;; Hunter I-20 Series (Large Rotors)
    ("Hunter" "I-20" "Blue"   8.5  0.25  14.0 3.5 (50 360) "standard")
    ("Hunter" "I-20" "Green"  10.1 0.36  15.0 3.5 (50 360) "standard")
    ("Hunter" "I-20" "Yellow" 11.6 0.48  16.0 3.5 (50 360) "standard")
    ("Hunter" "I-20" "Red"    13.1 0.63  17.0 3.5 (50 360) "standard")
    ("Hunter" "I-20" "Gray"   14.3 0.80  18.0 3.5 (50 360) "standard")
    ("Hunter" "I-20" "Black"  15.5 1.00  19.0 3.5 (50 360) "standard")
  )
)

;;; ----------------------------------------------------------------------------
;;; Catalogue Loading and Management
;;; ----------------------------------------------------------------------------

(defun qtech:load-catalogue ()
  "Load sprinkler catalogue (built-in + external if available)"
  (setq *qtech-catalogue* (qtech:get-default-catalogue))
  ;; Try to load external CSV files
  (qtech:load-external-catalogue "rainbird-catalogue.csv")
  (qtech:load-external-catalogue "hunter-catalogue.csv")
  *qtech-catalogue*
)

(defun qtech:load-external-catalogue (filename / filepath data header)
  "Load additional catalogue entries from CSV file"
  (setq filepath (strcat (qtech:get-path) "\\..\\data\\" filename))
  (if (findfile filepath)
    (progn
      (setq data (qtech:read-csv-file filepath))
      (if (> (length data) 1)  ; Skip header row
        (progn
          (foreach row (cdr data)
            (if (>= (length row) 9)
              (setq *qtech-catalogue* 
                    (append *qtech-catalogue* 
                            (list (qtech:csv-row-to-entry row))))
            )
          )
          (princ (strcat "\n  Loaded external: " filename))
          T
        )
      )
    )
  )
)

(defun qtech:csv-row-to-entry (row)
  "Convert CSV row to catalogue entry"
  (list
    (nth 0 row)                        ; brand
    (nth 1 row)                        ; model
    (nth 2 row)                        ; nozzle
    (atof (nth 3 row))                 ; radius
    (atof (nth 4 row))                 ; flow
    (atof (nth 5 row))                 ; precip
    (atof (nth 6 row))                 ; pressure
    (qtech:parse-arc-options (nth 7 row)) ; arcs
    (nth 8 row)                        ; efficiency
  )
)

(defun qtech:parse-arc-options (str / parts)
  "Parse arc options string like '90,180,270,360' into list"
  (setq parts (qtech:string-split (vl-string-translate "\"" "" str) ","))
  (mapcar 'atoi parts)
)

;;; ----------------------------------------------------------------------------
;;; Catalogue Access Functions
;;; ----------------------------------------------------------------------------

(defun qtech:cat-brand (entry) (nth 0 entry))
(defun qtech:cat-model (entry) (nth 1 entry))
(defun qtech:cat-nozzle (entry) (nth 2 entry))
(defun qtech:cat-radius (entry) (nth 3 entry))
(defun qtech:cat-flow (entry) (nth 4 entry))
(defun qtech:cat-precip (entry) (nth 5 entry))
(defun qtech:cat-pressure (entry) (nth 6 entry))
(defun qtech:cat-arcs (entry) (nth 7 entry))
(defun qtech:cat-efficiency (entry) (nth 8 entry))

(defun qtech:cat-full-name (entry)
  "Get full name of catalogue entry"
  (strcat (qtech:cat-brand entry) " " 
          (qtech:cat-model entry) " " 
          (qtech:cat-nozzle entry))
)

(defun qtech:cat-is-high-efficiency (entry)
  "Check if entry is high-efficiency"
  (= (strcase (qtech:cat-efficiency entry)) "HIGH")
)

(defun qtech:cat-supports-arc (entry arc)
  "Check if entry supports given arc angle"
  (or (member arc (qtech:cat-arcs entry))
      (member 0 (qtech:cat-arcs entry)))  ; 0 means adjustable
)

;;; ----------------------------------------------------------------------------
;;; Catalogue Search and Filtering
;;; ----------------------------------------------------------------------------

(defun qtech:filter-by-brand (catalogue brand)
  "Filter catalogue entries by brand"
  (if (= (strcase brand) "ANY")
    catalogue
    (vl-remove-if-not
      '(lambda (e) (= (strcase (qtech:cat-brand e)) (strcase brand)))
      catalogue
    )
  )
)

(defun qtech:filter-by-radius-range (catalogue min-r max-r)
  "Filter catalogue entries by radius range"
  (vl-remove-if-not
    '(lambda (e) 
       (and (>= (qtech:cat-radius e) min-r)
            (<= (qtech:cat-radius e) max-r)))
    catalogue
  )
)

(defun qtech:filter-by-pressure (catalogue max-pressure)
  "Filter catalogue entries by maximum pressure"
  (vl-remove-if-not
    '(lambda (e) (<= (qtech:cat-pressure e) max-pressure))
    catalogue
  )
)

(defun qtech:filter-by-arc (catalogue arc)
  "Filter catalogue entries that support given arc"
  (vl-remove-if-not
    '(lambda (e) (qtech:cat-supports-arc e arc))
    catalogue
  )
)

(defun qtech:filter-high-efficiency (catalogue)
  "Filter to only high-efficiency entries"
  (vl-remove-if-not 'qtech:cat-is-high-efficiency catalogue)
)

(defun qtech:sort-by-radius (catalogue ascending)
  "Sort catalogue by radius"
  (vl-sort catalogue
    (if ascending
      '(lambda (a b) (< (qtech:cat-radius a) (qtech:cat-radius b)))
      '(lambda (a b) (> (qtech:cat-radius a) (qtech:cat-radius b)))
    )
  )
)

(defun qtech:sort-by-precip (catalogue ascending)
  "Sort catalogue by precipitation rate"
  (vl-sort catalogue
    (if ascending
      '(lambda (a b) (< (qtech:cat-precip a) (qtech:cat-precip b)))
      '(lambda (a b) (> (qtech:cat-precip a) (qtech:cat-precip b)))
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Nozzle Selection Algorithm
;;; ----------------------------------------------------------------------------

(defun qtech:select-best-nozzle (target-radius arc system-pressure target-precip prefer-brand prefer-eff
                                  / candidates scored best)
  "Select the best nozzle for given requirements"
  ;; Start with full catalogue
  (setq candidates *qtech-catalogue*)
  
  ;; Filter by pressure
  (setq candidates (qtech:filter-by-pressure candidates system-pressure))
  
  ;; Filter by arc support
  (setq candidates (qtech:filter-by-arc candidates arc))
  
  ;; Filter by brand preference
  (if (and prefer-brand (/= (strcase prefer-brand) "ANY"))
    (progn
      (setq brand-filtered (qtech:filter-by-brand candidates prefer-brand))
      (if brand-filtered (setq candidates brand-filtered))
    )
  )
  
  ;; Filter by radius range (allow some flexibility)
  (setq candidates 
        (qtech:filter-by-radius-range candidates 
                                       (* target-radius 0.7) 
                                       (* target-radius 1.3)))
  
  ;; If efficiency preference, try high-efficiency first
  (if prefer-eff
    (progn
      (setq eff-filtered (qtech:filter-high-efficiency candidates))
      (if eff-filtered (setq candidates eff-filtered))
    )
  )
  
  ;; Score and select best
  (if candidates
    (progn
      (setq scored (mapcar 
        '(lambda (e) 
           (cons (qtech:score-nozzle e target-radius target-precip) e))
        candidates))
      (setq sorted (vl-sort scored '(lambda (a b) (> (car a) (car b)))))
      (cdar sorted)  ; Return best entry (without score)
    )
    nil
  )
)

(defun qtech:score-nozzle (entry target-radius target-precip / score radius-match precip-match)
  "Score a nozzle entry (higher is better)"
  (setq score 100.0)
  
  ;; Radius match (closer is better, max 40 points)
  (setq radius-match (- 1.0 (abs (/ (- (qtech:cat-radius entry) target-radius) 
                                    target-radius))))
  (setq score (+ score (* 40.0 (max 0.0 radius-match))))
  
  ;; Precipitation match (closer to target is better, max 30 points)
  (setq precip-match (- 1.0 (abs (/ (- (qtech:cat-precip entry) target-precip) 
                                    target-precip))))
  (setq score (+ score (* 30.0 (max 0.0 precip-match))))
  
  ;; Efficiency bonus (20 points)
  (if (qtech:cat-is-high-efficiency entry)
    (setq score (+ score 20.0))
  )
  
  ;; Lower flow bonus (10 points for water savings)
  (setq score (+ score (* 10.0 (/ 1.0 (1+ (qtech:cat-flow entry))))))
  
  score
)

;;; ----------------------------------------------------------------------------
;;; Catalogue Browser Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRCATALOGUE (/ brand filter-rad entries i entry)
  "Browse sprinkler catalogue"
  (princ "\n\n=== QTECH SPRINKLER CATALOGUE ===\n")
  (princ (strcat "Total entries: " (itoa (length *qtech-catalogue*)) "\n\n"))
  
  ;; Filter options
  (princ "Filter by brand (ENTER for all): ")
  (setq brand (getstring))
  (if (= brand "") (setq brand "any"))
  
  (princ "Filter by max radius in meters (ENTER for all): ")
  (setq filter-rad (getstring))
  
  ;; Apply filters
  (setq entries (qtech:filter-by-brand *qtech-catalogue* brand))
  (if (and filter-rad (/= filter-rad ""))
    (setq entries (qtech:filter-by-radius-range entries 0.0 (atof filter-rad)))
  )
  
  ;; Sort by radius
  (setq entries (qtech:sort-by-radius entries T))
  
  ;; Display results
  (princ (strcat "\nShowing " (itoa (length entries)) " entries:\n"))
  (princ "─────────────────────────────────────────────────────────────────────────\n")
  (princ "Brand       Model       Nozzle       Radius  Flow    Precip  Press  Eff\n")
  (princ "                                     (m)     (m³/h)  (mm/h)  (bar)\n")
  (princ "─────────────────────────────────────────────────────────────────────────\n")
  
  (setq i 0)
  (foreach entry (if (> (length entries) 30) 
                   (qtech:list-take entries 30) 
                   entries)
    (qtech:print-catalogue-entry entry)
    (setq i (1+ i))
  )
  
  (if (> (length entries) 30)
    (princ (strcat "\n... and " (itoa (- (length entries) 30)) " more entries\n"))
  )
  
  (princ "─────────────────────────────────────────────────────────────────────────\n")
  (princ)
)

(defun qtech:print-catalogue-entry (entry)
  "Print a single catalogue entry"
  (princ 
    (strcat 
      (qtech:string-pad (qtech:cat-brand entry) 12 " ")
      (qtech:string-pad (qtech:cat-model entry) 12 " ")
      (qtech:string-pad (qtech:cat-nozzle entry) 13 " ")
      (qtech:string-pad (rtos (qtech:cat-radius entry) 2 1) 8 " ")
      (qtech:string-pad (rtos (qtech:cat-flow entry) 2 2) 8 " ")
      (qtech:string-pad (rtos (qtech:cat-precip entry) 2 1) 8 " ")
      (qtech:string-pad (rtos (qtech:cat-pressure entry) 2 1) 7 " ")
      (if (qtech:cat-is-high-efficiency entry) "HIGH" "STD")
      "\n"
    )
  )
)

(defun qtech:list-take (lst n / result i)
  "Take first n elements of a list"
  (setq result '() i 0)
  (while (and (< i n) (nth i lst))
    (setq result (append result (list (nth i lst))))
    (setq i (1+ i))
  )
  result
)

;;; ============================================================================
;;; End of qtech-catalogue.lsp
;;; ============================================================================

(princ)

