;;; ============================================================================
;;; QIRRI - Intelligent Irrigation Planner
;;; Professional Irrigation Design Automation for AutoCAD
;;; Copyright (c) 2026 QTech Design - All Rights Reserved
;;; Contact: info@qtech.hr | www.qtech.hr
;;; ============================================================================

(vl-load-com)

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

;; Aliases for backward compatibility with qtech- functions
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
  '(("pressure" . 3.0)           ; Operating pressure (bar)
    ("max-flow" . 2.0)           ; Max flow per zone (m³/h)
    ("spacing-factor" . 0.55)    ; Head-to-head spacing factor (55% of radius)
    ("perimeter-offset" . 0.20)  ; Inward offset from boundary (m)
    ("grid-resolution" . 0.5)    ; Coverage grid resolution (m)
    ("min-coverage" . 85.0)      ; Minimum coverage percentage
    ("target-precip" . 15.0)     ; Target precipitation rate (mm/hr)
    ("soil-infiltration" . 25.0) ; Soil infiltration rate (mm/hr)
    ("et-rate" . 5.0)            ; Evapotranspiration rate (mm/day)
    ("preferred-brand" . "any")  ; "rainbird", "hunter", or "any"
    ("efficiency-priority" . T)  ; Prioritize high-efficiency nozzles
    ("target-cu" . 90.0)         ; Target CU percentage
    ("target-du" . 85.0)         ; Target DU percentage
    ("wind-factor" . 1.0)        ; Wind reduction factor (0.8-1.0)
  )
)
(setq *qtech-default-settings* *qirri-default-settings*)

;;; ----------------------------------------------------------------------------
;;; Path Detection and File Loading (Cross-Platform)
;;; ----------------------------------------------------------------------------

(defun qirri:get-separator ()
  "Get the path separator for current OS"
  (if (wcmatch (getenv "COMPUTERNAME") "*")
    "\\"  ; Windows
    "/"   ; Mac/Linux
  )
)

(defun qirri:get-path (/ path)
  "Get the Qirri installation path"
  (if *qirri-path*
    *qirri-path*
    (progn
      ;; Try to find path from loaded file location
      (setq path (findfile "qirri.lsp"))
      (if path
        (progn
          (setq *qirri-path* (vl-filename-directory path))
          *qirri-path*
        )
        ;; Default paths
        (if (wcmatch (getenv "COMPUTERNAME") "*")
          "C:\\Qirri\\lisp"
          "/Users/Shared/Qirri/lisp"
        )
      )
    )
  )
)

;; Alias for backward compatibility
(defun qtech:get-path () (qirri:get-path))

(defun qirri:load-module (filename / filepath sep)
  "Load a Qirri LISP module"
  (setq sep (qirri:get-separator))
  (setq filepath (strcat (qirri:get-path) sep filename))
  
  ;; Try with detected path
  (cond
    ((findfile filepath)
     (load filepath)
     (princ (strcat "\n  [OK] " filename))
     T)
    ;; Try in same directory as qirri.lsp
    ((findfile filename)
     (load (findfile filename))
     (princ (strcat "\n  [OK] " filename))
     T)
    ;; Not found
    (T
     (princ (strcat "\n  [!!] Cannot find: " filename))
     nil)
  )
)

;; Alias
(defun qtech:load-module (f) (qirri:load-module f))

(defun qirri:load-all-modules ()
  "Load all Qirri modules"
  (princ "\n\nLoading Qirri modules...")
  (qirri:load-module "qirri-utils.lsp")
  (qirri:load-module "qirri-catalogue.lsp")
  (qirri:load-module "qirri-simulation.lsp")
  (qirri:load-module "qirri-placement.lsp")
  (qirri:load-module "qirri-genetic.lsp")
  (qirri:load-module "qirri-patterns.lsp")
  (qirri:load-module "qirri-zones.lsp")
  (qirri:load-module "qirri-boq.lsp")
  (qirri:load-module "qirri-reports.lsp")
  (princ "\n\nModules loaded.\n")
)

(defun qtech:load-all-modules () (qirri:load-all-modules))

;;; ----------------------------------------------------------------------------
;;; Initialization
;;; ----------------------------------------------------------------------------

(defun qirri:initialize ()
  "Initialize Qirri Irrigation Planner"
  (princ "\n")
  (princ "==============================================================\n")
  (princ "        QIRRI - Intelligent Irrigation Planner\n")
  (princ (strcat "                    Version " *qirri-version* "\n"))
  (princ "            Professional Design for AutoCAD\n")
  (princ "               by QTech Design (qtech.hr)\n")
  (princ "==============================================================\n")
  
  ;; Load settings
  (setq *qirri-settings* (qirri:copy-alist *qirri-default-settings*))
  (setq *qtech-settings* *qirri-settings*)
  
  ;; Load modules
  (qirri:load-all-modules)
  
  ;; Create layers (only if qirri-utils loaded successfully)
  (if (fboundp 'qtech:create-layers)
    (qtech:create-layers)
    (princ "\n  Note: Layers will be created when needed.\n")
  )
  
  ;; Load catalogue
  (if (fboundp 'qtech:load-catalogue)
    (progn
      (if (qtech:load-catalogue)
        (progn
          (setq *qtech-catalogue* *qirri-catalogue*)
          (princ (strcat "\n  Catalogue: " 
                         (itoa (length *qirri-catalogue*)) 
                         " sprinklers loaded\n"))
        )
        (princ "\n  Note: Using built-in catalogue.\n")
      )
    )
  )
  
  (princ "\n==============================================================\n")
  (princ "  Type QIRR to start\n")
  (princ "  Type QIRRHELP for all commands\n")
  (princ "==============================================================\n\n")
  (princ)
)

(defun qtech:initialize () (qirri:initialize))

(defun qirri:copy-alist (alist)
  "Deep copy an association list"
  (mapcar '(lambda (pair) (cons (car pair) (cdr pair))) alist)
)

(defun qtech:copy-alist (a) (qirri:copy-alist a))

;;; ----------------------------------------------------------------------------
;;; Main Menu Command
;;; ----------------------------------------------------------------------------

(defun c:QIRR (/ choice)
  "Main Qirri menu"
  (qirri:print-menu)
  (setq choice (strcase (getstring "\nEnter option [1-9/A-C/S/K/Q]: ")))
  (cond
    ((= choice "1") (c:QIRRAREA))
    ((= choice "2") (c:QIRRPLACE))
    ((= choice "3") (c:QIRROPTIMIZE))
    ((= choice "4") (c:QIRRFULL))
    ((= choice "5") (c:QIRRMANUAL))
    ((= choice "6") (c:QIRRPATTERN))
    ((= choice "7") (c:QIRRVALIDATE))
    ((= choice "8") (c:QIRRCOVERAGE))
    ((= choice "9") (c:QIRRZONE))
    ((= choice "A") (c:QIRRBOQ))
    ((= choice "B") (c:QIRRSAVINGS))
    ((= choice "C") (c:QIRREXPORT))
    ((= choice "S") (c:QIRRSETTINGS))
    ((= choice "K") (c:QIRRCATALOGUE))
    ((or (= choice "Q") (= choice "")) (princ "\nGoodbye from Qirri!"))
    (T (princ "\nInvalid option.") (c:QIRR))
  )
  (princ)
)

(defun c:QIRRFULL ()
  "Run full optimization: Greedy placement + GA refinement"
  (princ "\n")
  (princ "==============================================================\n")
  (princ "              QIRRI FULL AUTO OPTIMIZATION\n")
  (princ "==============================================================\n")
  (princ "\n> Phase 1: Greedy placement...\n")
  (c:QIRRPLACE)
  (princ "\n> Phase 2: Genetic algorithm refinement...\n")
  (c:QIRROPTIMIZE)
  (princ "\n[DONE] Full optimization complete.\n")
  (princ)
)

(defun qirri:print-menu ()
  "Print the main menu"
  (princ "\n")
  (princ "+----------------------------------------------+\n")
  (princ "|            QIRRI - MAIN MENU                 |\n")
  (princ "+----------------------------------------------+\n")
  (princ "|  1. Select Irrigation Area                   |\n")
  (princ "|  2. Auto Place Sprinklers (Greedy)           |\n")
  (princ "|  3. Optimize Placement (Genetic Algorithm)   |\n")
  (princ "|  4. Full Auto (Greedy + GA) [RECOMMENDED]    |\n")
  (princ "|  5. Manual Placement Mode                    |\n")
  (princ "+----------------------------------------------+\n")
  (princ "|  6. Draw Spray Patterns                      |\n")
  (princ "|  7. Validate Uniformity (CU/DU)              |\n")
  (princ "|  8. Show Coverage Heatmap                    |\n")
  (princ "+----------------------------------------------+\n")
  (princ "|  9. Zone Management                          |\n")
  (princ "|  A. Generate BOQ                             |\n")
  (princ "|  B. Water Savings Report                     |\n")
  (princ "|  C. Export Data                              |\n")
  (princ "+----------------------------------------------+\n")
  (princ "|  S. Settings  |  K. Catalogue  |  Q. Quit    |\n")
  (princ "+----------------------------------------------+\n")
)

(defun qtech:print-menu () (qirri:print-menu))

;;; ----------------------------------------------------------------------------
;;; Settings Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRSETTINGS (/ key val)
  "Display and modify project settings"
  (princ "\n\n=== QIRRI SETTINGS ===\n")
  (princ "Current settings:\n")
  (foreach pair *qirri-settings*
    (princ (strcat "  " (car pair) ": " (qirri:value-to-string (cdr pair)) "\n"))
  )
  (princ "\nEnter setting name to change (or ENTER to exit): ")
  (setq key (getstring))
  (if (and key (/= key ""))
    (progn
      (if (assoc key *qirri-settings*)
        (progn
          (princ (strcat "Current value: " (qirri:value-to-string (cdr (assoc key *qirri-settings*)))))
          (princ "\nEnter new value: ")
          (setq val (getstring))
          (if (and val (/= val ""))
            (progn
              (setq *qirri-settings* 
                    (subst (cons key (qirri:parse-value val (cdr (assoc key *qirri-settings*))))
                           (assoc key *qirri-settings*)
                           *qirri-settings*))
              (setq *qtech-settings* *qirri-settings*)
              (princ "\n[OK] Setting updated.")
            )
          )
        )
        (princ "\n[!!] Unknown setting.")
      )
      (c:QIRRSETTINGS)
    )
  )
  (princ)
)

(defun qirri:value-to-string (val)
  "Convert a value to string for display"
  (cond
    ((= (type val) 'REAL) (rtos val 2 2))
    ((= (type val) 'INT) (itoa val))
    ((= (type val) 'STR) val)
    ((= val T) "Yes")
    ((= val nil) "No")
    (T (vl-princ-to-string val))
  )
)

(defun qtech:value-to-string (v) (qirri:value-to-string v))

(defun qirri:parse-value (str old-val)
  "Parse a string value based on the type of the old value"
  (cond
    ((= (type old-val) 'REAL) (atof str))
    ((= (type old-val) 'INT) (atoi str))
    ((= (type old-val) 'STR) str)
    ((or (= old-val T) (= old-val nil))
     (member (strcase str) '("YES" "Y" "TRUE" "T" "1")))
    (T str)
  )
)

(defun qtech:parse-value (s o) (qirri:parse-value s o))

;;; ----------------------------------------------------------------------------
;;; Help Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRHELP ()
  "Display help information"
  (princ "\n")
  (princ "==============================================================\n")
  (princ "                     QIRRI COMMANDS\n")
  (princ "==============================================================\n")
  (princ "  MAIN\n")
  (princ "  ----\n")
  (princ "  QIRR          - Main menu\n")
  (princ "  QIRRAREA      - Select irrigation area\n")
  (princ "  QIRRPLACE     - Greedy placement algorithm\n")
  (princ "  QIRROPTIMIZE  - Genetic algorithm optimization\n")
  (princ "  QIRRFULL      - Full auto (greedy + GA) [BEST]\n")
  (princ "  QIRRMANUAL    - Manual placement\n")
  (princ "--------------------------------------------------------------\n")
  (princ "  ANALYSIS\n")
  (princ "  --------\n")
  (princ "  QIRRVALIDATE  - CU/DU uniformity metrics\n")
  (princ "  QIRRCOVERAGE  - Precipitation heatmap\n")
  (princ "  QIRRPATTERN   - Draw spray patterns\n")
  (princ "  QIRRGRID      - Show simulation grid\n")
  (princ "--------------------------------------------------------------\n")
  (princ "  REPORTS\n")
  (princ "  -------\n")
  (princ "  QIRRZONE      - Zone management\n")
  (princ "  QIRRBOQ       - Bill of quantities\n")
  (princ "  QIRRSAVINGS   - Water savings analysis\n")
  (princ "  QIRREXPORT    - Export to CSV\n")
  (princ "  QIRRSTATS     - Quick statistics\n")
  (princ "--------------------------------------------------------------\n")
  (princ "  UTILITIES\n")
  (princ "  ---------\n")
  (princ "  QIRRSETTINGS  - Settings\n")
  (princ "  QIRRCATALOGUE - Sprinklers\n")
  (princ "  QIRRLAYERS    - Layers\n")
  (princ "  QIRRUNITS     - Units check\n")
  (princ "  QIRRVERSION   - Version\n")
  (princ "  QIRRHELP      - This help\n")
  (princ "==============================================================\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Version Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRVERSION ()
  "Display version information"
  (princ "\n")
  (princ "==============================================================\n")
  (princ "        QIRRI - Intelligent Irrigation Planner\n")
  (princ (strcat "                    Version " *qirri-version* "\n"))
  (princ "\n")
  (princ "  * Hybrid Greedy-Genetic Algorithm Optimization\n")
  (princ "  * Target CU >90%, DU >85%\n")
  (princ "  * Water Savings 30-50%\n")
  (princ "\n")
  (princ "  Copyright (c) 2026 QTech Design\n")
  (princ "  info@qtech.hr | www.qtech.hr\n")
  (princ "==============================================================\n")
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Units Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRUNITS (/ lunits insunits)
  "Check and set drawing units to meters"
  (setq lunits (getvar "LUNITS"))
  (setq insunits (getvar "INSUNITS"))
  
  (princ "\n=== Drawing Units Check ===\n")
  (princ (strcat "Linear units: " (nth lunits '("Scientific" "Decimal" "Engineering" "Architectural" "Fractional")) "\n"))
  (princ (strcat "Insertion units: " (qirri:get-unit-name insunits) "\n"))
  
  (if (/= insunits 6) ; 6 = Meters
    (progn
      (princ "\n[!!] WARNING: Drawing units are not set to Meters!")
      (princ "\nQirri requires metric units (meters).")
      (if (= "Y" (strcase (getstring "\nSet units to Meters? [Y/N]: ")))
        (progn
          (setvar "INSUNITS" 6)
          (setvar "LUNITS" 2) ; Decimal
          (princ "\n[OK] Units set to Meters (Decimal).")
        )
      )
    )
    (princ "\n[OK] Units are correctly set to Meters.")
  )
  (princ)
)

(defun qirri:get-unit-name (code)
  "Get unit name from INSUNITS code"
  (nth code '("Unspecified" "Inches" "Feet" "Miles" "Millimeters" 
              "Centimeters" "Meters" "Kilometers" "Microinches"
              "Mils" "Yards" "Angstroms" "Nanometers" "Microns"
              "Decimeters" "Dekameters" "Hectometers" "Gigameters"
              "Astronomical" "Light Years" "Parsecs"))
)

(defun qtech:get-unit-name (c) (qirri:get-unit-name c))

;;; ----------------------------------------------------------------------------
;;; Layers Command
;;; ----------------------------------------------------------------------------

(defun c:QIRRLAYERS ()
  "Create standard irrigation layers"
  (if (fboundp 'qtech:create-layers)
    (progn
      (qtech:create-layers)
      (princ "\n[OK] Irrigation layers created/verified.\n")
    )
    (princ "\n[!!] Layer function not loaded. Load all modules first.\n")
  )
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Auto-load on startup
;;; ----------------------------------------------------------------------------

(princ "\nInitializing Qirri...\n")
(qirri:initialize)

;;; ============================================================================
;;; End of qirri.lsp
;;; ============================================================================
