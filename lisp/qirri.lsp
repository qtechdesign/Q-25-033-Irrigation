;;; ============================================================================
;;; QIRRI - Intelligent Irrigation Planner
;;; Professional Irrigation Design Automation for AutoCAD
;;; Copyright (c) 2026 QTech Design - All Rights Reserved
;;; Contact: info@qtech.hr | www.qtech.hr
;;; ============================================================================

;;; Load COM support only on Windows (not available on Mac)
(if (not (wcmatch (strcase (getvar "PLATFORM")) "*MAC*"))
  (vl-load-com)
)

;;; ----------------------------------------------------------------------------
;;; Global Variables
;;; ----------------------------------------------------------------------------

(setq *qirri-version* "1.0.0")
(setq *qirri-path* nil)
(setq *qirri-catalogue* nil)
(setq *qirri-settings* nil)
(setq *qirri-current-area* nil)
(setq *qirri-placements* nil)
(setq *qirri-area-data* nil)
(setq *qirri-grid* nil)
(setq *qirri-sim-results* nil)

;; Aliases for backward compatibility
(setq *qtech-version* *qirri-version*)
(setq *qtech-catalogue* nil)
(setq *qtech-settings* nil)
(setq *qtech-current-area* nil)
(setq *qtech-placements* nil)
(setq *qtech-area-data* nil)
(setq *qtech-grid* nil)
(setq *qtech-sim-results* nil)

;;; Default Settings
(setq *qirri-default-settings*
  '(("pressure" . 3.0)
    ("max-flow" . 2.0)
    ("spacing-factor" . 0.55)
    ("perimeter-offset" . 0.20)
    ("grid-resolution" . 0.5)
    ("min-coverage" . 85.0)
    ("target-precip" . 15.0)
    ("soil-infiltration" . 25.0)
    ("et-rate" . 5.0)
    ("preferred-brand" . "any")
    ("efficiency-priority" . T)
    ("target-cu" . 90.0)
    ("target-du" . 85.0)
    ("wind-factor" . 1.0)
  )
)
(setq *qtech-default-settings* *qirri-default-settings*)

;;; ----------------------------------------------------------------------------
;;; Path Detection - Simple and Robust
;;; ----------------------------------------------------------------------------

(defun qirri:get-path (/ found-path)
  "Get the Qirri installation path"
  (if *qirri-path*
    *qirri-path*
    (progn
      (setq found-path (findfile "qirri.lsp"))
      (if found-path
        (progn
          (setq *qirri-path* (vl-filename-directory found-path))
          *qirri-path*
        )
        nil
      )
    )
  )
)

(defun qtech:get-path () (qirri:get-path))

;;; ----------------------------------------------------------------------------
;;; Module Loading - Direct Approach
;;; ----------------------------------------------------------------------------

(defun qirri:load-module (filename / full-path base-path)
  "Load a Qirri module"
  (setq base-path (qirri:get-path))
  
  (cond
    ;; Try with base path + filename
    ((and base-path 
          (setq full-path (findfile (strcat base-path "/" filename))))
     (load full-path)
     (princ (strcat "\n  [OK] " filename))
     T)
    
    ;; Try just the filename (if in search path)
    ((setq full-path (findfile filename))
     (load full-path)
     (princ (strcat "\n  [OK] " filename " (from search path)"))
     T)
    
    ;; Not found
    (T
     (princ (strcat "\n  [!!] NOT FOUND: " filename))
     nil)
  )
)

(defun qtech:load-module (f) (qirri:load-module f))

(defun qirri:load-all-modules ()
  "Load all Qirri modules"
  (princ "\n\nLoading modules...")
  (qirri:load-module "qirri-utils.lsp")
  (qirri:load-module "qirri-catalogue.lsp")
  (qirri:load-module "qirri-simulation.lsp")
  (qirri:load-module "qirri-placement.lsp")
  (qirri:load-module "qirri-genetic.lsp")
  (qirri:load-module "qirri-patterns.lsp")
  (qirri:load-module "qirri-zones.lsp")
  (qirri:load-module "qirri-boq.lsp")
  (qirri:load-module "qirri-reports.lsp")
  (princ "\n")
)

(defun qtech:load-all-modules () (qirri:load-all-modules))

;;; ----------------------------------------------------------------------------
;;; Utility Functions (defined here so commands work even if utils fails)
;;; ----------------------------------------------------------------------------

(defun qirri:copy-alist (alist)
  "Copy association list"
  (mapcar '(lambda (p) (cons (car p) (cdr p))) alist)
)

(defun qtech:copy-alist (a) (qirri:copy-alist a))

(defun qirri:string-pad (str len pad-char / result)
  "Pad string to length"
  (setq result str)
  (while (< (strlen result) len)
    (setq result (strcat result pad-char))
  )
  result
)

(defun qtech:string-pad (s l p) (qirri:string-pad s l p))

(defun qirri:value-to-string (val)
  "Value to string"
  (cond
    ((= (type val) 'REAL) (rtos val 2 2))
    ((= (type val) 'INT) (itoa val))
    ((= (type val) 'STR) val)
    ((= val T) "Yes")
    ((null val) "No")
    (T (vl-princ-to-string val))
  )
)

(defun qtech:value-to-string (v) (qirri:value-to-string v))

(defun qirri:parse-value (str old-val)
  "Parse string value"
  (cond
    ((= (type old-val) 'REAL) (atof str))
    ((= (type old-val) 'INT) (atoi str))
    ((= (type old-val) 'STR) str)
    ((or (= old-val T) (null old-val))
     (member (strcase str) '("YES" "Y" "TRUE" "T" "1")))
    (T str)
  )
)

(defun qtech:parse-value (s o) (qirri:parse-value s o))

;;; ----------------------------------------------------------------------------
;;; MAIN MENU COMMAND - QIRR
;;; ----------------------------------------------------------------------------

(defun c:QIRR (/ choice)
  "Main Qirri menu - type QIRR to start"
  (qirri:print-menu)
  (initget "1 2 3 4 5 6 7 8 9 A B C S K Q")
  (setq choice (strcase (getstring "\nEnter option: ")))
  (cond
    ((= choice "1") (if (fboundp 'c:QIRRAREA) (c:QIRRAREA) (princ "\nModule not loaded.")))
    ((= choice "2") (if (fboundp 'c:QIRRPLACE) (c:QIRRPLACE) (princ "\nModule not loaded.")))
    ((= choice "3") (if (fboundp 'c:QIRROPTIMIZE) (c:QIRROPTIMIZE) (princ "\nModule not loaded.")))
    ((= choice "4") (c:QIRRFULL))
    ((= choice "5") (if (fboundp 'c:QIRRMANUAL) (c:QIRRMANUAL) (princ "\nModule not loaded.")))
    ((= choice "6") (if (fboundp 'c:QIRRPATTERN) (c:QIRRPATTERN) (princ "\nModule not loaded.")))
    ((= choice "7") (if (fboundp 'c:QIRRVALIDATE) (c:QIRRVALIDATE) (princ "\nModule not loaded.")))
    ((= choice "8") (if (fboundp 'c:QIRRCOVERAGE) (c:QIRRCOVERAGE) (princ "\nModule not loaded.")))
    ((= choice "9") (if (fboundp 'c:QIRRZONE) (c:QIRRZONE) (princ "\nModule not loaded.")))
    ((= choice "A") (if (fboundp 'c:QIRRBOQ) (c:QIRRBOQ) (princ "\nModule not loaded.")))
    ((= choice "B") (if (fboundp 'c:QIRRSAVINGS) (c:QIRRSAVINGS) (princ "\nModule not loaded.")))
    ((= choice "C") (if (fboundp 'c:QIRREXPORT) (c:QIRREXPORT) (princ "\nModule not loaded.")))
    ((= choice "S") (c:QIRRSETTINGS))
    ((= choice "K") (if (fboundp 'c:QIRRCATALOGUE) (c:QIRRCATALOGUE) (princ "\nModule not loaded.")))
    ((or (= choice "Q") (= choice "")) (princ "\nGoodbye!"))
    (T (princ "\nInvalid option."))
  )
  (princ)
)

(defun c:QIRRFULL ()
  "Full optimization"
  (princ "\n=== FULL AUTO OPTIMIZATION ===\n")
  (if (fboundp 'c:QIRRPLACE)
    (progn
      (princ "\nPhase 1: Greedy placement...")
      (c:QIRRPLACE)
      (if (fboundp 'c:QIRROPTIMIZE)
        (progn
          (princ "\nPhase 2: GA optimization...")
          (c:QIRROPTIMIZE)
        )
      )
      (princ "\nDone.")
    )
    (princ "\nModules not loaded.")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Menu Display
;;; ----------------------------------------------------------------------------

(defun qirri:print-menu ()
  "Display menu"
  (princ "\n")
  (princ "+--------------------------------------------+\n")
  (princ "|          QIRRI - MAIN MENU                 |\n")
  (princ "+--------------------------------------------+\n")
  (princ "|  1. Select Irrigation Area                 |\n")
  (princ "|  2. Auto Place Sprinklers (Greedy)         |\n")
  (princ "|  3. Optimize Placement (Genetic)           |\n")
  (princ "|  4. Full Auto (Greedy + GA)                |\n")
  (princ "|  5. Manual Placement                       |\n")
  (princ "+--------------------------------------------+\n")
  (princ "|  6. Draw Spray Patterns                    |\n")
  (princ "|  7. Validate Uniformity (CU/DU)            |\n")
  (princ "|  8. Show Coverage Heatmap                  |\n")
  (princ "+--------------------------------------------+\n")
  (princ "|  9. Zone Management                        |\n")
  (princ "|  A. Generate BOQ                           |\n")
  (princ "|  B. Water Savings Report                   |\n")
  (princ "|  C. Export Data                            |\n")
  (princ "+--------------------------------------------+\n")
  (princ "|  S. Settings  |  K. Catalogue  |  Q. Quit  |\n")
  (princ "+--------------------------------------------+\n")
)

(defun qtech:print-menu () (qirri:print-menu))

;;; ----------------------------------------------------------------------------
;;; Settings
;;; ----------------------------------------------------------------------------

(defun c:QIRRSETTINGS (/ key val)
  "Settings"
  (princ "\n=== SETTINGS ===\n")
  (foreach pair *qirri-settings*
    (princ (strcat "  " (car pair) ": " (qirri:value-to-string (cdr pair)) "\n"))
  )
  (princ "\nEnter setting name to change (ENTER to exit): ")
  (setq key (getstring))
  (if (and key (/= key ""))
    (if (assoc key *qirri-settings*)
      (progn
        (princ (strcat "Current: " (qirri:value-to-string (cdr (assoc key *qirri-settings*)))))
        (princ "\nNew value: ")
        (setq val (getstring))
        (if (/= val "")
          (progn
            (setq *qirri-settings*
              (subst (cons key (qirri:parse-value val (cdr (assoc key *qirri-settings*))))
                     (assoc key *qirri-settings*)
                     *qirri-settings*))
            (setq *qtech-settings* *qirri-settings*)
            (princ "\nUpdated.")
          )
        )
        (c:QIRRSETTINGS)
      )
      (progn (princ "\nUnknown setting.") (c:QIRRSETTINGS))
    )
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Help
;;; ----------------------------------------------------------------------------

(defun c:QIRRHELP ()
  "Help"
  (princ "\n")
  (princ "=== QIRRI COMMANDS ===\n")
  (princ "  QIRR          - Main menu\n")
  (princ "  QIRRAREA      - Select irrigation area\n")
  (princ "  QIRRPLACE     - Auto placement\n")
  (princ "  QIRROPTIMIZE  - GA optimization\n")
  (princ "  QIRRFULL      - Full auto\n")
  (princ "  QIRRMANUAL    - Manual placement\n")
  (princ "  QIRRPATTERN   - Draw patterns\n")
  (princ "  QIRRVALIDATE  - CU/DU check\n")
  (princ "  QIRRCOVERAGE  - Heatmap\n")
  (princ "  QIRRZONE      - Zones\n")
  (princ "  QIRRBOQ       - Bill of quantities\n")
  (princ "  QIRRSAVINGS   - Water savings\n")
  (princ "  QIRREXPORT    - Export CSV\n")
  (princ "  QIRRSETTINGS  - Settings\n")
  (princ "  QIRRCATALOGUE - Sprinklers\n")
  (princ "  QIRRHELP      - This help\n")
  (princ "  QIRRVERSION   - Version info\n")
  (princ "\nContact: info@qtech.hr | www.qtech.hr\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Version
;;; ----------------------------------------------------------------------------

(defun c:QIRRVERSION ()
  "Version"
  (princ "\n")
  (princ "===========================================\n")
  (princ "  QIRRI - Intelligent Irrigation Planner\n")
  (princ (strcat "  Version " *qirri-version* "\n"))
  (princ "  Copyright 2026 QTech Design\n")
  (princ "  info@qtech.hr | www.qtech.hr\n")
  (princ "===========================================\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Initialization
;;; ----------------------------------------------------------------------------

(defun qirri:initialize ()
  "Initialize Qirri"
  (princ "\n")
  (princ "============================================\n")
  (princ "  QIRRI - Intelligent Irrigation Planner\n")
  (princ (strcat "  Version " *qirri-version* "\n"))
  (princ "  QTech Design - www.qtech.hr\n")
  (princ "============================================\n")
  
  ;; Initialize settings
  (setq *qirri-settings* (qirri:copy-alist *qirri-default-settings*))
  (setq *qtech-settings* *qirri-settings*)
  
  ;; Show path
  (princ (strcat "\nPath: " (if (qirri:get-path) (qirri:get-path) "Not found") "\n"))
  
  ;; Load modules
  (qirri:load-all-modules)
  
  ;; Create layers if utils loaded
  (if (fboundp 'qtech:create-layers)
    (qtech:create-layers)
  )
  
  ;; Load catalogue if available
  (if (fboundp 'qtech:load-catalogue)
    (progn
      (qtech:load-catalogue)
      (setq *qtech-catalogue* *qirri-catalogue*)
      (if *qirri-catalogue*
        (princ (strcat "\nCatalogue: " (itoa (length *qirri-catalogue*)) " sprinklers"))
      )
    )
  )
  
  (princ "\n\n============================================\n")
  (princ "  Type QIRR to start\n")
  (princ "  Type QIRRHELP for commands\n")
  (princ "============================================\n")
  (princ)
)

(defun qtech:initialize () (qirri:initialize))

;;; ----------------------------------------------------------------------------
;;; Auto-run initialization
;;; ----------------------------------------------------------------------------

(qirri:initialize)

;;; ============================================================================
;;; End of qirri.lsp
;;; ============================================================================
