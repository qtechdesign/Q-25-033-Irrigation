;;; ============================================================================
;;; QTECH IRRIGATION - Bill of Quantities (BOQ)
;;; Generate material lists and cost estimates
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; BOQ Generation Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRBOQ (/ mode)
  "Generate bill of quantities"
  
  (if (not *qtech-placements*)
    (progn
      (princ "\nNo sprinklers placed. Run QIRRPLACE first.")
      (princ)
      (exit)
    )
  )
  
  (princ "\n\n=== BILL OF QUANTITIES ===\n")
  (princ "Options:\n")
  (princ "  1. Display BOQ summary\n")
  (princ "  2. Detailed BOQ by zone\n")
  (princ "  3. Export BOQ to CSV\n")
  (princ "  4. Insert BOQ table in drawing\n")
  (princ "  5. Cancel\n")
  
  (setq mode (getint "\nSelect option [1-5]: "))
  
  (cond
    ((= mode 1) (qtech:boq-summary))
    ((= mode 2) (qtech:boq-by-zone))
    ((= mode 3) (qtech:boq-export-csv))
    ((= mode 4) (qtech:boq-insert-table))
    (T (princ "\nCancelled."))
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; BOQ Summary
;;; ----------------------------------------------------------------------------

(defun qtech:boq-summary (/ grouped total-heads total-flow)
  "Display BOQ summary grouped by sprinkler type"
  
  ;; Group heads by model/nozzle
  (setq grouped (qtech:group-by-type *qtech-placements*))
  
  (princ "\n")
  (princ "╔════════════════════════════════════════════════════════════════════════════╗\n")
  (princ "║                        BILL OF QUANTITIES - SUMMARY                        ║\n")
  (princ "╠════════════════════════════════════════════════════════════════════════════╣\n")
  (princ "║ Brand       Model        Nozzle       Qty   Radius  Flow    Precip  Arc   ║\n")
  (princ "║                                             (m)     (m³/h)  (mm/h)        ║\n")
  (princ "╠════════════════════════════════════════════════════════════════════════════╣\n")
  
  (setq total-heads 0)
  (setq total-flow 0.0)
  
  (foreach group grouped
    (setq type-key (car group))
    (setq heads (cdr group))
    (setq qty (length heads))
    (setq sample (car heads))
    
    (setq total-heads (+ total-heads qty))
    (setq total-flow (+ total-flow (* qty (cdr (assoc "flow" sample)))))
    
    (princ (strcat "║ "
      (qtech:string-pad (cdr (assoc "brand" sample)) 12 " ")
      (qtech:string-pad (cdr (assoc "model" sample)) 13 " ")
      (qtech:string-pad (cdr (assoc "nozzle" sample)) 13 " ")
      (qtech:string-pad (itoa qty) 6 " ")
      (qtech:string-pad (rtos (cdr (assoc "radius" sample)) 2 1) 8 " ")
      (qtech:string-pad (rtos (cdr (assoc "flow" sample)) 2 3) 8 " ")
      (qtech:string-pad (rtos (cdr (assoc "precip" sample)) 2 1) 8 " ")
      (qtech:string-pad (itoa (cdr (assoc "arc" sample))) 6 " ")
      "║\n"))
  )
  
  (princ "╠════════════════════════════════════════════════════════════════════════════╣\n")
  (princ (strcat "║ TOTALS:" 
    (qtech:string-pad "" 37 " ")
    (qtech:string-pad (itoa total-heads) 6 " ")
    "        "
    (qtech:string-pad (rtos total-flow 2 3) 8 " ")
    "              ║\n"))
  (princ "╚════════════════════════════════════════════════════════════════════════════╝\n")
  
  ;; Area coverage summary
  (if *qtech-area-data*
    (progn
      (setq area (cdr (assoc "area" *qtech-area-data*)))
      (princ (strcat "\nIrrigation Area: " (rtos area 2 2) " m²"))
      (princ (strcat "\nHeads per 100m²: " (rtos (/ (* total-heads 100) area) 2 1)))
      (princ (strcat "\nAvg Precip Rate: " 
                     (rtos (/ (* total-flow 1000) area) 2 1) " mm/hr"))
    )
  )
  (princ "\n")
)

(defun qtech:group-by-type (heads / groups key existing)
  "Group heads by brand/model/nozzle"
  (setq groups '())
  (foreach head heads
    (setq key (strcat (cdr (assoc "brand" head)) "|"
                      (cdr (assoc "model" head)) "|"
                      (cdr (assoc "nozzle" head)) "|"
                      (itoa (cdr (assoc "arc" head)))))
    
    (setq existing (assoc key groups))
    (if existing
      (setq groups (subst 
                    (cons key (append (cdr existing) (list head)))
                    existing
                    groups))
      (setq groups (append groups (list (cons key (list head)))))
    )
  )
  groups
)

;;; ----------------------------------------------------------------------------
;;; BOQ by Zone
;;; ----------------------------------------------------------------------------

(defun qtech:boq-by-zone (/ zones)
  "Display BOQ broken down by zone"
  (setq zones (qtech:get-unique-zones *qtech-placements*))
  (setq zones (vl-sort zones '<))
  
  (foreach zone zones
    (if zone
      (progn
        (princ (strcat "\n\n─── ZONE " (itoa zone) " ───\n"))
        (setq zone-heads (qtech:get-heads-in-zone *qtech-placements* zone))
        (qtech:print-zone-boq zone-heads)
      )
    )
  )
  
  ;; Unassigned
  (setq unassigned (qtech:get-heads-in-zone *qtech-placements* nil))
  (if unassigned
    (progn
      (princ "\n\n─── UNASSIGNED ───\n")
      (qtech:print-zone-boq unassigned)
    )
  )
)

(defun qtech:print-zone-boq (heads / grouped)
  "Print BOQ for a set of heads"
  (setq grouped (qtech:group-by-type heads))
  
  (princ "Type                              Qty    Flow Each   Total Flow\n")
  (princ "──────────────────────────────────────────────────────────────\n")
  
  (setq zone-total 0.0)
  (foreach group grouped
    (setq type-heads (cdr group))
    (setq sample (car type-heads))
    (setq qty (length type-heads))
    (setq flow-each (cdr (assoc "flow" sample)))
    (setq total (* qty flow-each))
    (setq zone-total (+ zone-total total))
    
    (princ (strcat 
      (qtech:string-pad (strcat (cdr (assoc "brand" sample)) " "
                               (cdr (assoc "model" sample)) " "
                               (cdr (assoc "nozzle" sample))) 34 " ")
      (qtech:string-pad (itoa qty) 7 " ")
      (qtech:string-pad (rtos flow-each 2 3) 12 " ")
      (rtos total 2 3)
      "\n"))
  )
  
  (princ "──────────────────────────────────────────────────────────────\n")
  (princ (strcat "Zone Total Flow: " (rtos zone-total 2 3) " m³/h\n"))
)

;;; ----------------------------------------------------------------------------
;;; BOQ Export to CSV
;;; ----------------------------------------------------------------------------

(defun c:QIRREXPORT ()
  "Export data to CSV file"
  (qtech:boq-export-csv)
)

(defun qtech:boq-export-csv (/ filepath data grouped)
  "Export BOQ to CSV file"
  
  ;; Get file path
  (setq filepath (getfiled "Export BOQ" "" "csv" 1))
  
  (if filepath
    (progn
      (princ (strcat "\nExporting to: " filepath))
      
      ;; Build CSV data
      (setq data '())
      
      ;; Header row
      (setq data (append data 
        (list '("Brand" "Model" "Nozzle" "Arc" "Qty" "Radius_m" "Flow_m3h" 
                "Precip_mmhr" "Unit_Flow" "Total_Flow"))))
      
      ;; Data rows
      (setq grouped (qtech:group-by-type *qtech-placements*))
      
      (foreach group grouped
        (setq heads (cdr group))
        (setq sample (car heads))
        (setq qty (length heads))
        
        (setq data (append data
          (list (list
            (cdr (assoc "brand" sample))
            (cdr (assoc "model" sample))
            (cdr (assoc "nozzle" sample))
            (itoa (cdr (assoc "arc" sample)))
            (itoa qty)
            (rtos (cdr (assoc "radius" sample)) 2 2)
            (rtos (cdr (assoc "flow" sample)) 2 4)
            (rtos (cdr (assoc "precip" sample)) 2 2)
            (rtos (cdr (assoc "flow" sample)) 2 4)
            (rtos (* qty (cdr (assoc "flow" sample))) 2 4)
          ))))
      )
      
      ;; Write file
      (if (qtech:write-csv-file filepath data)
        (princ "\nExport successful!")
        (princ "\nExport failed!")
      )
    )
    (princ "\nExport cancelled.")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; BOQ Table in Drawing
;;; ----------------------------------------------------------------------------

(defun qtech:boq-insert-table (/ insert-pt grouped row-height col-widths x y)
  "Insert BOQ as a table in the drawing"
  
  (setq insert-pt (getpoint "\nSelect table insertion point: "))
  
  (if insert-pt
    (progn
      (qtech:set-layer "IRR-TEXT")
      
      (setq row-height 0.5)
      (setq col-widths '(3.0 3.0 3.0 1.5 1.5 2.0 2.0))  ; Column widths
      (setq x (car insert-pt))
      (setq y (cadr insert-pt))
      
      ;; Title
      (command "._TEXT" (list x (+ y row-height)) 0.4 0 
               "IRRIGATION BOQ")
      (setq y (- y row-height))
      
      ;; Header row
      (qtech:draw-table-row (list x y) col-widths row-height
        '("Brand" "Model" "Nozzle" "Arc" "Qty" "Flow" "Precip"))
      (setq y (- y row-height))
      
      ;; Separator line
      (command "._LINE" (list x y) 
               (list (+ x (qtech:list-sum col-widths)) y) "")
      (setq y (- y (* row-height 0.2)))
      
      ;; Data rows
      (setq grouped (qtech:group-by-type *qtech-placements*))
      
      (foreach group grouped
        (setq heads (cdr group))
        (setq sample (car heads))
        (setq qty (length heads))
        
        (qtech:draw-table-row (list x y) col-widths row-height
          (list
            (cdr (assoc "brand" sample))
            (cdr (assoc "model" sample))
            (cdr (assoc "nozzle" sample))
            (itoa (cdr (assoc "arc" sample)))
            (itoa qty)
            (rtos (cdr (assoc "flow" sample)) 2 2)
            (rtos (cdr (assoc "precip" sample)) 2 1)
          ))
        (setq y (- y row-height))
      )
      
      ;; Border
      (command "._RECTANGLE" insert-pt 
               (list (+ x (qtech:list-sum col-widths)) y))
      
      (princ "\nTable inserted.")
    )
    (princ "\nCancelled.")
  )
)

(defun qtech:draw-table-row (start-pt col-widths row-height values / x y i)
  "Draw a single table row"
  (setq x (car start-pt))
  (setq y (cadr start-pt))
  (setq i 0)
  
  (foreach val values
    (if (< i (length col-widths))
      (progn
        (command "._TEXT" 
                 (list (+ x 0.1) (- y (* row-height 0.7)))
                 (* row-height 0.6) 0 
                 (if (= (type val) 'STR) val (vl-princ-to-string val)))
        (setq x (+ x (nth i col-widths)))
        (setq i (1+ i))
      )
    )
  )
)

;;; ----------------------------------------------------------------------------
;;; Project Summary Report
;;; ----------------------------------------------------------------------------

(defun qtech:generate-project-report (/ report)
  "Generate comprehensive project report data"
  (setq report '())
  
  ;; Project info
  (setq report (append report
    (list (cons "project_name" (getvar "DWGNAME"))
          (cons "date" (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD)"))
          (cons "units" "Meters (SI)"))))
  
  ;; Area info
  (if *qtech-area-data*
    (setq report (append report
      (list (cons "area_m2" (cdr (assoc "area" *qtech-area-data*)))
            (cons "perimeter_m" (cdr (assoc "perimeter" *qtech-area-data*))))))
  )
  
  ;; Head counts
  (setq report (append report
    (list (cons "total_heads" (length *qtech-placements*))
          (cons "perimeter_heads" 
                (length (vl-remove-if-not 
                  '(lambda (h) (= (cdr (assoc "type" h)) "perimeter"))
                  *qtech-placements*)))
          (cons "interior_heads"
                (length (vl-remove-if-not
                  '(lambda (h) (= (cdr (assoc "type" h)) "interior"))
                  *qtech-placements*))))))
  
  ;; Flow totals
  (setq total-flow (qtech:list-sum 
                    (mapcar '(lambda (h) (cdr (assoc "flow" h))) 
                            *qtech-placements*)))
  (setq report (append report
    (list (cons "total_flow_m3h" total-flow))))
  
  ;; Zone count
  (setq zones (qtech:get-unique-zones *qtech-placements*))
  (setq report (append report
    (list (cons "num_zones" (length (vl-remove 'nil zones))))))
  
  report
)

;;; ----------------------------------------------------------------------------
;;; Detailed Position Export
;;; ----------------------------------------------------------------------------

(defun qtech:export-positions-csv (/ filepath data head)
  "Export all head positions with full details"
  
  (setq filepath (getfiled "Export Positions" "" "csv" 1))
  
  (if filepath
    (progn
      (setq data '())
      
      ;; Header
      (setq data (append data
        (list '("ID" "X" "Y" "Brand" "Model" "Nozzle" "Arc" "Radius" 
                "Flow" "Precip" "Zone" "Type"))))
      
      ;; Data
      (setq id 1)
      (foreach head *qtech-placements*
        (setq pos (cdr (assoc "position" head)))
        (setq data (append data
          (list (list
            (itoa id)
            (rtos (car pos) 2 4)
            (rtos (cadr pos) 2 4)
            (cdr (assoc "brand" head))
            (cdr (assoc "model" head))
            (cdr (assoc "nozzle" head))
            (itoa (cdr (assoc "arc" head)))
            (rtos (cdr (assoc "radius" head)) 2 2)
            (rtos (cdr (assoc "flow" head)) 2 4)
            (rtos (cdr (assoc "precip" head)) 2 2)
            (if (cdr (assoc "zone" head)) 
                (itoa (cdr (assoc "zone" head))) "")
            (cdr (assoc "type" head))
          ))))
        (setq id (1+ id))
      )
      
      (if (qtech:write-csv-file filepath data)
        (princ (strcat "\nExported " (itoa (1- (length data))) " positions."))
        (princ "\nExport failed!")
      )
    )
  )
  (princ)
)

;;; ============================================================================
;;; End of qtech-boq.lsp
;;; ============================================================================

(princ)

