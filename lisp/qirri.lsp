;;; ============================================================================
;;; QIRRI - Intelligent Irrigation Planner
;;; Professional Irrigation Design Automation for AutoCAD
;;; Copyright (c) 2026 QTech Design - All Rights Reserved
;;; Contact: info@qtech.hr | www.qtech.hr
;;; ============================================================================

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

;; Aliases
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
;;; Path Separator (Mac vs Windows)
;;; ----------------------------------------------------------------------------

(defun qirri:get-separator ()
  "Get path separator for current OS"
  (if (wcmatch (getvar "platform") "*Mac*")
    "/"
    "\\"
  )
)

(defun qtech:get-separator () (qirri:get-separator))

;;; ----------------------------------------------------------------------------
;;; Function Existence Check (Mac Compatible)
;;; ----------------------------------------------------------------------------

(defun qirri:function-exists (func-name)
  "Check if function exists - works on Mac and Windows"
  (and (not (null func-name))
       (or (and (boundp func-name) (eval func-name))
           (not (null (eval (list 'quote func-name))))))
)

;;; ----------------------------------------------------------------------------
;;; Path Setup - Ask User if Not Found
;;; ----------------------------------------------------------------------------

(defun qirri:setup-path (/ found-path user-path)
  "Setup path - try findfile first, then ask user"
  (if *qirri-path*
    *qirri-path*
    (progn
      ;; Try findfile
      (setq found-path (findfile "qirri.lsp"))
      (if found-path
        (setq *qirri-path* (vl-filename-directory found-path))
        (progn
          ;; Ask user for path
          (princ "\nQirri path not found automatically.")
          (princ "\nPlease enter the full path to the lisp folder")
          (princ "\n(e.g., /Users/yourname/Qirri/lisp): ")
          (setq user-path (getstring T))
          (if (and user-path (/= user-path ""))
            (setq *qirri-path* user-path)
          )
        )
      )
      *qirri-path*
    )
  )
)

(defun qirri:get-path () *qirri-path*)
(defun qtech:get-path () *qirri-path*)

;;; ----------------------------------------------------------------------------
;;; Module Loading
;;; ----------------------------------------------------------------------------

(defun qirri:load-module (filename / full-path)
  "Load a module"
  (setq full-path nil)
  
  ;; Try with path
  (if *qirri-path*
    (setq full-path (findfile (strcat *qirri-path* "/" filename)))
  )
  
  ;; Try just filename
  (if (not full-path)
    (setq full-path (findfile filename))
  )
  
  ;; Load if found
  (if full-path
    (progn
      (load full-path)
      (princ (strcat "\n  [OK] " filename))
      T
    )
    (progn
      (princ (strcat "\n  [!!] NOT FOUND: " filename))
      nil
    )
  )
)

(defun qtech:load-module (f) (qirri:load-module f))

(defun qirri:load-all-modules ()
  "Load all modules"
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
;;; Utility Functions (so menu works even without modules)
;;; ----------------------------------------------------------------------------

(defun qirri:copy-alist (alist)
  (mapcar '(lambda (p) (cons (car p) (cdr p))) alist)
)
(defun qtech:copy-alist (a) (qirri:copy-alist a))

(defun qirri:string-pad (str len pad-char / result)
  (setq result str)
  (while (< (strlen result) len)
    (setq result (strcat result pad-char))
  )
  result
)
(defun qtech:string-pad (s l p) (qirri:string-pad s l p))

(defun qirri:value-to-string (val)
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
;;; MAIN MENU - QIRR
;;; ----------------------------------------------------------------------------

(defun c:QIRR (/ choice)
  "Main menu"
  (qirri:print-menu)
  (setq choice (strcase (getstring "\nEnter option: ")))
  (cond
    ((= choice "1") (qirri:run-if-exists 'c:QIRRAREA))
    ((= choice "2") (qirri:run-if-exists 'c:QIRRPLACE))
    ((= choice "3") (qirri:run-if-exists 'c:QIRROPTIMIZE))
    ((= choice "4") (c:QIRRFULL))
    ((= choice "5") (qirri:run-if-exists 'c:QIRRMANUAL))
    ((= choice "6") (qirri:run-if-exists 'c:QIRRPATTERN))
    ((= choice "7") (qirri:run-if-exists 'c:QIRRVALIDATE))
    ((= choice "8") (qirri:run-if-exists 'c:QIRRCOVERAGE))
    ((= choice "9") (qirri:run-if-exists 'c:QIRRZONE))
    ((= choice "A") (qirri:run-if-exists 'c:QIRRBOQ))
    ((= choice "B") (qirri:run-if-exists 'c:QIRRSAVINGS))
    ((= choice "C") (qirri:run-if-exists 'c:QIRREXPORT))
    ((= choice "S") (c:QIRRSETTINGS))
    ((= choice "K") (qirri:run-if-exists 'c:QIRRCATALOGUE))
    ((or (= choice "Q") (= choice "")) (princ "\nGoodbye!"))
    (T (princ "\nInvalid option."))
  )
  (princ)
)

(defun qirri:run-if-exists (func-sym / func)
  "Run function if it exists"
  (setq func (eval func-sym))
  (if func
    (func)
    (princ "\nModule not loaded. Run QIRRSETPATH first.")
  )
)

(defun c:QIRRFULL ()
  "Full optimization"
  (princ "\n=== FULL AUTO OPTIMIZATION ===\n")
  (if (eval 'c:QIRRPLACE)
    (progn
      (princ "\nPhase 1: Greedy placement...")
      (c:QIRRPLACE)
      (if (eval 'c:QIRROPTIMIZE)
        (progn
          (princ "\nPhase 2: GA optimization...")
          (c:QIRROPTIMIZE)
        )
      )
    )
    (princ "\nModules not loaded.")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Menu Display
;;; ----------------------------------------------------------------------------

(defun qirri:print-menu ()
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
;;; Set Path Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRSETPATH (/ new-path)
  "Set Qirri path manually"
  (princ "\nCurrent path: ")
  (princ (if *qirri-path* *qirri-path* "Not set"))
  (princ "\n\nEnter full path to lisp folder: ")
  (setq new-path (getstring T))
  (if (and new-path (/= new-path ""))
    (progn
      (setq *qirri-path* new-path)
      (princ (strcat "\nPath set to: " *qirri-path*))
      (princ "\n\nReloading modules...")
      (qirri:load-all-modules)
    )
    (princ "\nCancelled.")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Settings
;;; ----------------------------------------------------------------------------

(defun c:QIRRSETTINGS (/ key val)
  (princ "\n=== SETTINGS ===\n")
  (foreach pair *qirri-settings*
    (princ (strcat "  " (car pair) ": " (qirri:value-to-string (cdr pair)) "\n"))
  )
  (princ "\nEnter setting name (ENTER to exit): ")
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
      (progn (princ "\nUnknown.") (c:QIRRSETTINGS))
    )
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Help
;;; ----------------------------------------------------------------------------

(defun c:QIRRHELP ()
  (princ "\n=== QIRRI COMMANDS ===\n")
  (princ "  QIRR          - Main menu\n")
  (princ "  QIRRSETPATH   - Set path to modules\n")
  (princ "  QIRRAREA      - Select area\n")
  (princ "  QIRRPLACE     - Auto placement\n")
  (princ "  QIRROPTIMIZE  - GA optimization\n")
  (princ "  QIRRFULL      - Full auto\n")
  (princ "  QIRRMANUAL    - Manual placement\n")
  (princ "  QIRRPATTERN   - Patterns\n")
  (princ "  QIRRVALIDATE  - CU/DU check\n")
  (princ "  QIRRCOVERAGE  - Heatmap\n")
  (princ "  QIRRZONE      - Zones\n")
  (princ "  QIRRBOQ       - BOQ\n")
  (princ "  QIRRSAVINGS   - Water savings\n")
  (princ "  QIRREXPORT    - Export\n")
  (princ "  QIRRSETTINGS  - Settings\n")
  (princ "  QIRRCATALOGUE - Catalogue\n")
  (princ "\nwww.qtech.hr\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Version
;;; ----------------------------------------------------------------------------

(defun c:QIRRVERSION ()
  (princ "\n=== QIRRI ===\n")
  (princ (strcat "Version " *qirri-version* "\n"))
  (princ "QTech Design 2026\n")
  (princ "www.qtech.hr\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Initialization
;;; ----------------------------------------------------------------------------

(defun qirri:initialize ()
  (princ "\n")
  (princ "============================================\n")
  (princ "  QIRRI - Intelligent Irrigation Planner\n")
  (princ (strcat "  Version " *qirri-version* "\n"))
  (princ "  QTech Design - www.qtech.hr\n")
  (princ "============================================\n")
  
  ;; Initialize settings
  (setq *qirri-settings* (qirri:copy-alist *qirri-default-settings*))
  (setq *qtech-settings* *qirri-settings*)
  
  ;; Setup path
  (qirri:setup-path)
  (princ (strcat "\nPath: " (if *qirri-path* *qirri-path* "Not set")))
  
  ;; Load modules if path exists
  (if *qirri-path*
    (progn
      (qirri:load-all-modules)
      
      ;; Create layers
      (if (eval 'qtech:create-layers)
        (qtech:create-layers)
      )
      
      ;; Load catalogue
      (if (eval 'qtech:load-catalogue)
        (progn
          (qtech:load-catalogue)
          (setq *qtech-catalogue* *qirri-catalogue*)
          (if *qirri-catalogue*
            (princ (strcat "\nCatalogue: " (itoa (length *qirri-catalogue*)) " sprinklers"))
          )
        )
      )
    )
    (princ "\n\n** Run QIRRSETPATH to set the path and load modules **")
  )
  
  (princ "\n\n============================================\n")
  (princ "  Type QIRR to start\n")
  (princ "  Type QIRRSETPATH if modules failed\n")
  (princ "============================================\n")
  (princ)
)

(defun qtech:initialize () (qirri:initialize))

;;; ----------------------------------------------------------------------------
;;; Auto-run
;;; ----------------------------------------------------------------------------

(qirri:initialize)

;;; ============================================================================
