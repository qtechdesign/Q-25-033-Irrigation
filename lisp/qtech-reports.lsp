;;; ============================================================================
;;; QTECH IRRIGATION - Water Savings & Performance Reports
;;; Comprehensive reporting and analysis functions
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; Water Savings Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRSAVINGS (/ area baseline-flow opt-flow runtime savings-m3 savings-pct
                        annual-savings cost-per-m3 annual-cost-savings)
  "Generate water savings analysis report"
  
  (if (not *qtech-placements*)
    (progn (princ "\nNo sprinklers placed.") (princ) (exit))
  )
  
  (if (not *qtech-sim-results*)
    (c:QIRRVALIDATE)
  )
  
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════════╗\n")
  (princ "║              QTECH WATER SAVINGS ANALYSIS REPORT                ║\n")
  (princ "╚══════════════════════════════════════════════════════════════════╝\n")
  
  ;; Get area
  (if *qtech-area-data*
    (setq area (cdr (assoc "area" *qtech-area-data*)))
    (setq area 100.0)
  )
  
  ;; Calculate flows
  (setq opt-flow (qtech:total-system-flow *qtech-placements*))
  (setq baseline-flow (qtech:calculate-baseline-flow area))
  
  ;; Default runtime assumptions
  (princ "\nEnter daily runtime in minutes [30]: ")
  (setq runtime (getreal))
  (if (not runtime) (setq runtime 30.0))
  
  (princ "Enter irrigation days per year [180]: ")
  (setq days-per-year (getint))
  (if (not days-per-year) (setq days-per-year 180))
  
  (princ "Enter water cost per m³ in your currency [2.50]: ")
  (setq cost-per-m3 (getreal))
  (if (not cost-per-m3) (setq cost-per-m3 2.50))
  
  ;; Calculate savings
  (setq runtime-hrs (/ runtime 60.0))
  (setq daily-baseline (* baseline-flow runtime-hrs))
  (setq daily-optimized (* opt-flow runtime-hrs))
  (setq daily-savings (- daily-baseline daily-optimized))
  
  (setq annual-baseline (* daily-baseline days-per-year))
  (setq annual-optimized (* daily-optimized days-per-year))
  (setq annual-savings (- annual-baseline annual-optimized))
  (setq savings-pct (if (> baseline-flow 0) 
                      (* 100.0 (/ (- baseline-flow opt-flow) baseline-flow))
                      0.0))
  
  (setq annual-cost-savings (* annual-savings cost-per-m3))
  
  ;; Display report
  (princ "\n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (princ "                        SYSTEM OVERVIEW                           \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (princ (strcat "  Irrigation Area:        " (rtos area 2 1) " m²\n"))
  (princ (strcat "  Total Sprinkler Heads:  " (itoa (length *qtech-placements*)) "\n"))
  (princ (strcat "  Daily Runtime:          " (rtos runtime 2 0) " minutes\n"))
  (princ (strcat "  Irrigation Season:      " (itoa days-per-year) " days/year\n"))
  
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "                        FLOW ANALYSIS                             \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (princ (strcat "  Baseline Flow (traditional):  " (rtos baseline-flow 2 3) " m³/h\n"))
  (princ (strcat "  Optimized Flow (QTech):       " (rtos opt-flow 2 3) " m³/h\n"))
  (princ (strcat "  Flow Reduction:               " (rtos (- baseline-flow opt-flow) 2 3) " m³/h\n"))
  (princ (strcat "  Efficiency Gain:              " (rtos savings-pct 2 1) "%\n"))
  
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "                       WATER CONSUMPTION                          \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (princ "                            Daily        Annual\n")
  (princ "  ────────────────────────────────────────────────\n")
  (princ (strcat "  Baseline:                " 
                 (qtech:string-pad (strcat (rtos daily-baseline 2 2) " m³") 12 " ")
                 (rtos annual-baseline 2 0) " m³\n"))
  (princ (strcat "  Optimized:               " 
                 (qtech:string-pad (strcat (rtos daily-optimized 2 2) " m³") 12 " ")
                 (rtos annual-optimized 2 0) " m³\n"))
  (princ (strcat "  SAVINGS:                 " 
                 (qtech:string-pad (strcat (rtos daily-savings 2 2) " m³") 12 " ")
                 (rtos annual-savings 2 0) " m³\n"))
  
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "                        COST ANALYSIS                             \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (princ (strcat "  Water Cost:                   " (rtos cost-per-m3 2 2) " per m³\n"))
  (princ (strcat "  Annual Baseline Cost:         " (rtos (* annual-baseline cost-per-m3) 2 2) "\n"))
  (princ (strcat "  Annual Optimized Cost:        " (rtos (* annual-optimized cost-per-m3) 2 2) "\n"))
  (princ (strcat "  ══════════════════════════════════════════════════════════\n"))
  (princ (strcat "  ANNUAL COST SAVINGS:          " (rtos annual-cost-savings 2 2) "\n"))
  (princ (strcat "  ══════════════════════════════════════════════════════════\n"))
  
  ;; Uniformity summary
  (if *qtech-sim-results*
    (progn
      (princ "\n═══════════════════════════════════════════════════════════════════\n")
      (princ "                     UNIFORMITY METRICS                           \n")
      (princ "═══════════════════════════════════════════════════════════════════\n")
      (princ (strcat "  Christiansen's CU:     " 
                     (rtos (cdr (assoc "cu" *qtech-sim-results*)) 2 1) "%\n"))
      (princ (strcat "  Distribution DU:       " 
                     (rtos (cdr (assoc "du" *qtech-sim-results*)) 2 1) "%\n"))
      (princ (strcat "  Coverage:              " 
                     (rtos (cdr (assoc "coverage" *qtech-sim-results*)) 2 1) "%\n"))
    )
  )
  
  ;; Efficiency breakdown
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "                    EFFICIENCY BREAKDOWN                          \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (qtech:print-efficiency-breakdown *qtech-placements*)
  
  ;; Recommendations
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "                      RECOMMENDATIONS                             \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  (qtech:print-recommendations *qtech-placements* *qtech-sim-results* savings-pct)
  
  (princ "\n═══════════════════════════════════════════════════════════════════\n")
  (princ "              Report generated by QTech Design 2026              \n")
  (princ "═══════════════════════════════════════════════════════════════════\n")
  
  ;; Offer export
  (princ "\nExport report to file? [Y/N]: ")
  (if (= "Y" (strcase (getstring)))
    (qtech:export-savings-report area opt-flow baseline-flow runtime days-per-year 
                                  cost-per-m3 annual-savings annual-cost-savings)
  )
  
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Supporting Calculations
;;; ----------------------------------------------------------------------------

(defun qtech:total-system-flow (heads)
  "Calculate total flow from all heads"
  (apply '+ (mapcar '(lambda (h) 
                       (if (cdr (assoc "flow" h)) (cdr (assoc "flow" h)) 0)) 
                    heads))
)

(defun qtech:calculate-baseline-flow (area / heads-per-100m2 baseline-head-flow)
  "Calculate baseline flow for traditional design (comparison)"
  ;; Traditional design assumptions:
  ;; - Grid spacing with standard rotors
  ;; - Higher precipitation rates (35-45 mm/hr vs 10-15 mm/hr)
  ;; - More overspray, less uniformity
  (setq heads-per-100m2 4.0)  ; Typical for 5m spacing
  (setq baseline-head-flow 0.35)  ; m³/h average for standard rotor
  
  (* (/ area 100.0) heads-per-100m2 baseline-head-flow)
)

(defun qtech:print-efficiency-breakdown (heads / he-count std-count total he-pct)
  "Print breakdown of high-efficiency vs standard heads"
  (setq he-count 0 std-count 0)
  
  (foreach h heads
    (setq entry (qtech:find-catalogue-entry 
                  (cdr (assoc "brand" h))
                  (cdr (assoc "model" h))
                  (cdr (assoc "nozzle" h))))
    (if (and entry (qtech:cat-is-high-efficiency entry))
      (setq he-count (1+ he-count))
      (setq std-count (1+ std-count))
    )
  )
  
  (setq total (+ he-count std-count))
  (setq he-pct (if (> total 0) (* 100.0 (/ (float he-count) total)) 0))
  
  (princ (strcat "  High-Efficiency Nozzles: " (itoa he-count) 
                 " (" (rtos he-pct 2 0) "%)\n"))
  (princ (strcat "  Standard Nozzles:        " (itoa std-count) 
                 " (" (rtos (- 100 he-pct) 2 0) "%)\n"))
  
  (if (> he-pct 70)
    (princ "  ✓ Excellent use of high-efficiency technology\n")
    (princ "  → Consider more MP Rotators or HE-VAN nozzles\n")
  )
)

(defun qtech:print-recommendations (heads results savings-pct / cu du)
  "Print optimization recommendations"
  (if results
    (progn
      (setq cu (cdr (assoc "cu" results)))
      (setq du (cdr (assoc "du" results)))
      
      (if (< cu 90)
        (princ "  • Run QIRROPTIMIZE to improve uniformity (target CU >90%)\n")
      )
      (if (< du 85)
        (princ "  • Check for dry spots - DU below 85% target\n")
      )
    )
  )
  
  (if (< savings-pct 30)
    (princ "  • Consider switching more heads to high-efficiency nozzles\n")
  )
  
  (if (> (length heads) (/ (cdr (assoc "area" *qtech-area-data*)) 15))
    (princ "  • Head density is high - verify spacing isn't too tight\n")
  )
  
  (princ "  • Verify pressure regulator matches system pressure (bar)\n")
  (princ "  • Consider smart controller with weather-based adjustment\n")
  (princ "  • Zone by plant type and sun exposure for additional savings\n")
)

;;; ----------------------------------------------------------------------------
;;; Report Export
;;; ----------------------------------------------------------------------------

(defun qtech:export-savings-report (area opt-flow baseline-flow runtime days 
                                     cost savings-m3 savings-cost / filepath f)
  "Export savings report to text file"
  (setq filepath (getfiled "Save Report" "" "txt" 1))
  
  (if filepath
    (progn
      (setq f (open filepath "w"))
      
      (write-line "═══════════════════════════════════════════════════════════" f)
      (write-line "        QTECH IRRIGATION - WATER SAVINGS REPORT           " f)
      (write-line "═══════════════════════════════════════════════════════════" f)
      (write-line "" f)
      (write-line (strcat "Report Date: " (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD)")) f)
      (write-line (strcat "Project: " (getvar "DWGNAME")) f)
      (write-line "" f)
      (write-line "SYSTEM PARAMETERS:" f)
      (write-line (strcat "  Area: " (rtos area 2 1) " m²") f)
      (write-line (strcat "  Heads: " (itoa (length *qtech-placements*))) f)
      (write-line (strcat "  Runtime: " (rtos runtime 2 0) " min/day") f)
      (write-line (strcat "  Season: " (itoa days) " days") f)
      (write-line "" f)
      (write-line "WATER SAVINGS:" f)
      (write-line (strcat "  Baseline Flow: " (rtos baseline-flow 2 3) " m³/h") f)
      (write-line (strcat "  Optimized Flow: " (rtos opt-flow 2 3) " m³/h") f)
      (write-line (strcat "  Annual Savings: " (rtos savings-m3 2 0) " m³") f)
      (write-line (strcat "  Cost Savings: " (rtos savings-cost 2 2)) f)
      (write-line "" f)
      
      (if *qtech-sim-results*
        (progn
          (write-line "UNIFORMITY:" f)
          (write-line (strcat "  CU: " (rtos (cdr (assoc "cu" *qtech-sim-results*)) 2 1) "%") f)
          (write-line (strcat "  DU: " (rtos (cdr (assoc "du" *qtech-sim-results*)) 2 1) "%") f)
        )
      )
      
      (write-line "" f)
      (write-line "═══════════════════════════════════════════════════════════" f)
      (write-line "         Generated by QTech Design 2026 - Irrigation       " f)
      
      (close f)
      (princ (strcat "\nReport saved to: " filepath "\n"))
    )
    (princ "\nExport cancelled.\n")
  )
)

;;; ----------------------------------------------------------------------------
;;; Full Project Report
;;; ----------------------------------------------------------------------------

(defun c:QIRRREPORT ()
  "Generate comprehensive project report"
  (princ "\n=== GENERATING COMPREHENSIVE REPORT ===\n")
  
  ;; Run validation if needed
  (if (not *qtech-sim-results*)
    (c:QIRRVALIDATE)
  )
  
  ;; Display BOQ
  (qtech:boq-summary)
  
  ;; Display zone summary if zones exist
  (if (qtech:get-unique-zones *qtech-placements*)
    (qtech:zone-summary)
  )
  
  ;; Display water savings
  (c:QIRRSAVINGS)
  
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Quick Stats Display
;;; ----------------------------------------------------------------------------

(defun c:QIRRSTATS ()
  "Quick statistics display"
  (if (not *qtech-placements*)
    (progn (princ "\nNo sprinklers placed.") (princ) (exit))
  )
  
  (princ "\n┌────────────────────────────────────┐\n")
  (princ "│       QUICK PROJECT STATS          │\n")
  (princ "├────────────────────────────────────┤\n")
  (princ (strcat "│ Heads:     " (qtech:string-pad (itoa (length *qtech-placements*)) 22 " ") "│\n"))
  (princ (strcat "│ Total Flow: " (qtech:string-pad (strcat (rtos (qtech:total-system-flow *qtech-placements*) 2 3) " m³/h") 21 " ") "│\n"))
  
  (if *qtech-area-data*
    (princ (strcat "│ Area:      " (qtech:string-pad (strcat (rtos (cdr (assoc "area" *qtech-area-data*)) 2 1) " m²") 22 " ") "│\n"))
  )
  
  (if *qtech-sim-results*
    (progn
      (princ (strcat "│ CU:        " (qtech:string-pad (strcat (rtos (cdr (assoc "cu" *qtech-sim-results*)) 2 1) "%") 22 " ") "│\n"))
      (princ (strcat "│ DU:        " (qtech:string-pad (strcat (rtos (cdr (assoc "du" *qtech-sim-results*)) 2 1) "%") 22 " ") "│\n"))
    )
  )
  
  (princ "└────────────────────────────────────┘\n")
  (princ)
)

;;; ============================================================================
;;; End of qtech-reports.lsp
;;; ============================================================================

(princ)

