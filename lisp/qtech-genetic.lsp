;;; ============================================================================
;;; QTECH DESIGN 2026 - Genetic Algorithm Optimizer
;;; Multi-objective GA for sprinkler placement optimization
;;; Copyright (c) 2026 QTech Design - All Rights Reserved
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; GA Configuration
;;; ----------------------------------------------------------------------------

(setq *qtech-ga-config*
  '(("population-size" . 30)      ; Number of chromosomes
    ("generations" . 50)          ; Max generations
    ("mutation-rate" . 0.08)      ; 8% mutation probability
    ("crossover-rate" . 0.7)      ; 70% crossover probability
    ("elitism-rate" . 0.1)        ; Keep top 10%
    ("tournament-size" . 3)       ; Tournament selection size
    ("position-mutation" . 0.5)   ; Position nudge distance (m)
    ("stagnation-limit" . 10)     ; Generations without improvement
    ("improvement-threshold" . 0.02) ; 2% minimum improvement
  )
)

;;; Fitness weights (multi-objective)
(setq *qtech-fitness-weights*
  '(("uniformity" . 0.50)         ; CU/DU weight
    ("flow" . 0.30)               ; Water usage weight
    ("overspray" . 0.15)          ; Waste minimization
    ("cost" . 0.05)               ; Head count/cost
  )
)

;;; ----------------------------------------------------------------------------
;;; Main GA Command
;;; ----------------------------------------------------------------------------

(defun c:QIRROPTIMIZE (/ population best-fitness gen stagnation improved)
  "Run genetic algorithm optimization on current placement"
  
  (if (not *qtech-placements*)
    (progn (princ "\nNo sprinklers to optimize. Run QIRRPLACE first.") (princ) (exit))
  )
  
  (if (not *qtech-area-data*)
    (progn (princ "\nNo area selected.") (princ) (exit))
  )
  
  (princ "\n")
  (princ "╔══════════════════════════════════════════════════════════════╗\n")
  (princ "║           QTECH GENETIC ALGORITHM OPTIMIZER                  ║\n")
  (princ "║        Multi-Objective Sprinkler Placement Optimization      ║\n")
  (princ "╚══════════════════════════════════════════════════════════════╝\n")
  
  ;; Ensure simulation grid exists
  (if (not *qtech-grid*)
    (qtech:generate-simulation-grid 
      (cdr (assoc "vertices" *qtech-area-data*))
      (cdr (assoc "grid-resolution" *qtech-settings*)))
  )
  
  ;; Initialize population from current placement
  (princ "\nInitializing population...")
  (setq population (qtech:ga-init-population *qtech-placements*))
  (princ (strcat " " (itoa (length population)) " chromosomes created.\n"))
  
  ;; Evaluate initial fitness
  (princ "Evaluating initial fitness...\n")
  (setq population (qtech:ga-evaluate-all population))
  (setq best-fitness (qtech:ga-best-fitness population))
  
  (princ (strcat "Initial best fitness: " (rtos best-fitness 2 4) "\n"))
  (princ "\nStarting evolution...\n")
  (princ "Gen    Best     Avg      CU%    DU%    Improved\n")
  (princ "─────────────────────────────────────────────────\n")
  
  ;; Evolution loop
  (setq gen 0)
  (setq stagnation 0)
  (setq max-gens (cdr (assoc "generations" *qtech-ga-config*)))
  (setq stag-limit (cdr (assoc "stagnation-limit" *qtech-ga-config*)))
  
  (while (and (< gen max-gens) (< stagnation stag-limit))
    ;; Selection
    (setq parents (qtech:ga-selection population))
    
    ;; Crossover
    (setq offspring (qtech:ga-crossover parents))
    
    ;; Mutation
    (setq offspring (qtech:ga-mutation offspring))
    
    ;; Evaluate offspring
    (setq offspring (qtech:ga-evaluate-all offspring))
    
    ;; Create new population (elitism + offspring)
    (setq population (qtech:ga-next-generation population offspring))
    
    ;; Track improvement
    (setq new-best (qtech:ga-best-fitness population))
    (setq improved (> new-best (* best-fitness (+ 1.0 (cdr (assoc "improvement-threshold" *qtech-ga-config*))))))
    
    (if improved
      (progn
        (setq best-fitness new-best)
        (setq stagnation 0)
      )
      (setq stagnation (1+ stagnation))
    )
    
    ;; Progress report
    (setq best-chromo (qtech:ga-get-best population))
    (setq metrics (cdr (assoc "metrics" best-chromo)))
    
    (princ (strcat 
      (qtech:string-pad (itoa gen) 4 " ") "  "
      (qtech:string-pad (rtos new-best 2 3) 8 " ") " "
      (qtech:string-pad (rtos (qtech:ga-avg-fitness population) 2 3) 8 " ") " "
      (qtech:string-pad (rtos (cdr (assoc "cu" metrics)) 2 1) 6 " ") " "
      (qtech:string-pad (rtos (cdr (assoc "du" metrics)) 2 1) 6 " ") " "
      (if improved "  YES" "  -")
      "\n"
    ))
    
    (setq gen (1+ gen))
  )
  
  (princ "─────────────────────────────────────────────────\n")
  
  ;; Apply best solution
  (setq best-chromo (qtech:ga-get-best population))
  (princ (strcat "\nOptimization complete after " (itoa gen) " generations.\n"))
  (princ (strcat "Final fitness: " (rtos (cdr (assoc "fitness" best-chromo)) 2 4) "\n"))
  
  ;; Convert best chromosome back to placements
  (setq *qtech-placements* (qtech:ga-chromosome-to-heads (cdr (assoc "genes" best-chromo))))
  
  ;; Show final metrics
  (c:QIRRVALIDATE)
  
  ;; Redraw
  (princ "\nRedrawing optimized layout...")
  (qtech:clear-sprinklers)
  (qtech:draw-all-heads *qtech-placements*)
  (princ " Done.\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------------
;;; Population Initialization
;;; ----------------------------------------------------------------------------

(defun qtech:ga-init-population (base-heads / pop-size population i chromo)
  "Initialize GA population from base placement"
  (setq pop-size (cdr (assoc "population-size" *qtech-ga-config*)))
  (setq population '())
  
  ;; First chromosome is the original
  (setq chromo (qtech:ga-heads-to-chromosome base-heads))
  (setq population (list (list (cons "genes" chromo) (cons "fitness" 0))))
  
  ;; Generate variants through mutation
  (setq i 1)
  (while (< i pop-size)
    (setq variant (qtech:ga-mutate-chromosome chromo 0.3))  ; Higher initial mutation
    (setq population (append population 
                      (list (list (cons "genes" variant) (cons "fitness" 0)))))
    (setq i (1+ i))
  )
  
  population
)

(defun qtech:ga-heads-to-chromosome (heads)
  "Convert head placements to chromosome (list of genes)"
  (mapcar 
    '(lambda (h)
       (list
         (car (cdr (assoc "position" h)))    ; x
         (cadr (cdr (assoc "position" h)))   ; y
         (cdr (assoc "brand" h))             ; brand
         (cdr (assoc "model" h))             ; model
         (cdr (assoc "nozzle" h))            ; nozzle
         (cdr (assoc "arc" h))               ; arc
         (cdr (assoc "line" h))              ; line
         (cdr (assoc "zone" h))              ; zone
       )
    )
    heads
  )
)

(defun qtech:ga-chromosome-to-heads (genes)
  "Convert chromosome genes back to head placements"
  (mapcar
    '(lambda (gene / nozzle)
       (setq nozzle (qtech:find-catalogue-entry 
                      (nth 2 gene) (nth 3 gene) (nth 4 gene)))
       (if nozzle
         (list
           (cons "position" (list (nth 0 gene) (nth 1 gene)))
           (cons "brand" (nth 2 gene))
           (cons "model" (nth 3 gene))
           (cons "nozzle" (nth 4 gene))
           (cons "radius" (qtech:cat-radius nozzle))
           (cons "flow" (qtech:cat-flow nozzle))
           (cons "precip" (qtech:cat-precip nozzle))
           (cons "pressure" (qtech:cat-pressure nozzle))
           (cons "arc" (nth 5 gene))
           (cons "rotation" 0)
           (cons "line" (nth 6 gene))
           (cons "zone" (nth 7 gene))
           (cons "type" "optimized")
           (cons "locked" nil)
         )
         nil
       )
    )
    genes
  )
)

(defun qtech:find-catalogue-entry (brand model nozzle)
  "Find matching catalogue entry"
  (car (vl-remove-if-not
    '(lambda (e)
       (and (= (strcase (qtech:cat-brand e)) (strcase brand))
            (= (strcase (qtech:cat-model e)) (strcase model))
            (= (strcase (qtech:cat-nozzle e)) (strcase nozzle))))
    *qtech-catalogue*
  ))
)

;;; ----------------------------------------------------------------------------
;;; Fitness Evaluation
;;; ----------------------------------------------------------------------------

(defun qtech:ga-evaluate-all (population)
  "Evaluate fitness for all chromosomes"
  (mapcar 'qtech:ga-evaluate-chromosome population)
)

(defun qtech:ga-evaluate-chromosome (chromo / genes heads metrics fitness)
  "Evaluate fitness of a single chromosome"
  (setq genes (cdr (assoc "genes" chromo)))
  (setq heads (qtech:ga-chromosome-to-heads genes))
  (setq heads (vl-remove 'nil heads))
  
  (if (not heads)
    (list (cons "genes" genes) (cons "fitness" -9999) (cons "metrics" nil))
    (progn
      ;; Run simulation
      (qtech:simulate-coverage heads)
      (setq metrics (qtech:calculate-uniformity *qtech-grid*))
      
      ;; Calculate multi-objective fitness
      (setq fitness (qtech:ga-calculate-fitness heads metrics))
      
      (list 
        (cons "genes" genes) 
        (cons "fitness" fitness)
        (cons "metrics" metrics)
      )
    )
  )
)

(defun qtech:ga-calculate-fitness (heads metrics / cu du coverage total-flow 
                                    max-flow flow-penalty fitness)
  "Calculate multi-objective fitness score"
  (setq cu (cdr (assoc "cu" metrics)))
  (setq du (cdr (assoc "du" metrics)))
  (setq coverage (cdr (assoc "coverage" metrics)))
  
  ;; Calculate total flow
  (setq total-flow (apply '+ (mapcar '(lambda (h) (cdr (assoc "flow" h))) heads)))
  (setq max-flow (cdr (assoc "max-flow" *qtech-settings*)))
  
  ;; Base fitness from uniformity (higher is better)
  (setq fitness (* (+ cu du) 0.5))  ; Average of CU and DU
  
  ;; Coverage bonus/penalty
  (if (< coverage 80)
    (setq fitness (* fitness 0.1))  ; Severe penalty for poor coverage
    (setq fitness (* fitness (/ coverage 100.0)))
  )
  
  ;; Flow efficiency bonus (less flow = higher score)
  (setq flow-score (- 1.0 (min 1.0 (/ total-flow (* (length heads) 0.3)))))
  (setq fitness (+ fitness (* flow-score 20)))
  
  ;; Head count efficiency (fewer heads for same coverage)
  (setq head-efficiency (/ coverage (float (length heads))))
  (setq fitness (+ fitness (* head-efficiency 5)))
  
  ;; Penalty for exceeding max flow per zone (would need zone analysis)
  
  fitness
)

;;; ----------------------------------------------------------------------------
;;; Selection
;;; ----------------------------------------------------------------------------

(defun qtech:ga-selection (population / selected tournament-size n-select)
  "Tournament selection"
  (setq tournament-size (cdr (assoc "tournament-size" *qtech-ga-config*)))
  (setq n-select (fix (* (length population) 
                         (cdr (assoc "crossover-rate" *qtech-ga-config*)))))
  (setq selected '())
  
  (repeat n-select
    (setq winner (qtech:ga-tournament population tournament-size))
    (setq selected (append selected (list winner)))
  )
  
  selected
)

(defun qtech:ga-tournament (population size / contestants best)
  "Run tournament and return winner"
  (setq contestants '())
  (repeat size
    (setq contestants (append contestants 
                        (list (nth (rem (abs (getvar "MILLISECS")) (length population)) 
                                   population))))
  )
  
  (setq best (car contestants))
  (foreach c (cdr contestants)
    (if (> (cdr (assoc "fitness" c)) (cdr (assoc "fitness" best)))
      (setq best c)
    )
  )
  best
)

;;; ----------------------------------------------------------------------------
;;; Crossover
;;; ----------------------------------------------------------------------------

(defun qtech:ga-crossover (parents / offspring i p1 p2 child1 child2)
  "Single-point crossover between parent pairs"
  (setq offspring '())
  (setq i 0)
  
  (while (< i (1- (length parents)))
    (setq p1 (cdr (assoc "genes" (nth i parents))))
    (setq p2 (cdr (assoc "genes" (nth (1+ i) parents))))
    
    ;; Crossover point
    (setq point (1+ (rem (abs (getvar "MILLISECS")) (1- (min (length p1) (length p2))))))
    
    ;; Create children
    (setq child1 (append (qtech:list-take p1 point) 
                         (qtech:list-drop p2 point)))
    (setq child2 (append (qtech:list-take p2 point) 
                         (qtech:list-drop p1 point)))
    
    (setq offspring (append offspring 
                      (list (list (cons "genes" child1) (cons "fitness" 0))
                            (list (cons "genes" child2) (cons "fitness" 0)))))
    (setq i (+ i 2))
  )
  
  offspring
)

(defun qtech:list-drop (lst n / result i)
  "Drop first n elements from list"
  (setq result '() i 0)
  (foreach item lst
    (if (>= i n)
      (setq result (append result (list item)))
    )
    (setq i (1+ i))
  )
  result
)

;;; ----------------------------------------------------------------------------
;;; Mutation
;;; ----------------------------------------------------------------------------

(defun qtech:ga-mutation (population / rate)
  "Apply mutation to population"
  (setq rate (cdr (assoc "mutation-rate" *qtech-ga-config*)))
  
  (mapcar 
    '(lambda (chromo)
       (if (< (/ (rem (abs (getvar "MILLISECS")) 1000) 1000.0) rate)
         (list (cons "genes" (qtech:ga-mutate-chromosome 
                               (cdr (assoc "genes" chromo)) rate))
               (cons "fitness" 0))
         chromo
       )
    )
    population
  )
)

(defun qtech:ga-mutate-chromosome (genes rate / mutated pos-mutation)
  "Apply various mutations to chromosome"
  (setq pos-mutation (cdr (assoc "position-mutation" *qtech-ga-config*)))
  
  (mapcar
    '(lambda (gene / rand)
       (setq rand (/ (rem (abs (getvar "MILLISECS")) 100) 100.0))
       (cond
         ;; Position mutation (nudge x,y)
         ((< rand 0.4)
          (list 
            (+ (nth 0 gene) (* pos-mutation (- (qtech:random-01) 0.5) 2))
            (+ (nth 1 gene) (* pos-mutation (- (qtech:random-01) 0.5) 2))
            (nth 2 gene) (nth 3 gene) (nth 4 gene) (nth 5 gene) 
            (nth 6 gene) (nth 7 gene)
          ))
         ;; Arc mutation
         ((< rand 0.6)
          (list 
            (nth 0 gene) (nth 1 gene) (nth 2 gene) (nth 3 gene) (nth 4 gene)
            (nth (rem (abs (getvar "MILLISECS")) 4) '(90 180 270 360))
            (nth 6 gene) (nth 7 gene)
          ))
         ;; Nozzle swap (within same brand/model)
         ((< rand 0.8)
          (qtech:ga-mutate-nozzle gene))
         ;; No mutation
         (T gene)
       )
    )
    genes
  )
)

(defun qtech:ga-mutate-nozzle (gene / brand model alternatives new-nozzle)
  "Swap nozzle with alternative from same model"
  (setq brand (nth 2 gene))
  (setq model (nth 3 gene))
  
  (setq alternatives (vl-remove-if-not
    '(lambda (e)
       (and (= (strcase (qtech:cat-brand e)) (strcase brand))
            (= (strcase (qtech:cat-model e)) (strcase model))))
    *qtech-catalogue*
  ))
  
  (if alternatives
    (progn
      (setq new-entry (nth (rem (abs (getvar "MILLISECS")) (length alternatives)) 
                           alternatives))
      (list (nth 0 gene) (nth 1 gene) 
            (qtech:cat-brand new-entry) 
            (qtech:cat-model new-entry)
            (qtech:cat-nozzle new-entry)
            (nth 5 gene) (nth 6 gene) (nth 7 gene))
    )
    gene
  )
)

(defun qtech:random-01 ()
  "Generate pseudo-random number 0-1"
  (/ (rem (abs (getvar "MILLISECS")) 1000) 1000.0)
)

;;; ----------------------------------------------------------------------------
;;; Next Generation
;;; ----------------------------------------------------------------------------

(defun qtech:ga-next-generation (old-pop offspring / elite-count elite new-pop)
  "Create next generation with elitism"
  (setq elite-count (max 1 (fix (* (length old-pop) 
                                   (cdr (assoc "elitism-rate" *qtech-ga-config*))))))
  
  ;; Sort old population by fitness (descending)
  (setq sorted-old (vl-sort old-pop
    '(lambda (a b) (> (cdr (assoc "fitness" a)) (cdr (assoc "fitness" b))))))
  
  ;; Keep elite
  (setq elite (qtech:list-take sorted-old elite-count))
  
  ;; Fill rest with offspring
  (setq new-pop (append elite 
                  (qtech:list-take offspring (- (length old-pop) elite-count))))
  
  new-pop
)

;;; ----------------------------------------------------------------------------
;;; GA Utilities
;;; ----------------------------------------------------------------------------

(defun qtech:ga-best-fitness (population)
  "Get best fitness in population"
  (apply 'max (mapcar '(lambda (c) (cdr (assoc "fitness" c))) population))
)

(defun qtech:ga-avg-fitness (population)
  "Get average fitness in population"
  (/ (apply '+ (mapcar '(lambda (c) (cdr (assoc "fitness" c))) population))
     (float (length population)))
)

(defun qtech:ga-get-best (population)
  "Get best chromosome from population"
  (setq best (car population))
  (foreach c (cdr population)
    (if (> (cdr (assoc "fitness" c)) (cdr (assoc "fitness" best)))
      (setq best c)
    )
  )
  best
)

;;; ----------------------------------------------------------------------------
;;; Helper Functions
;;; ----------------------------------------------------------------------------

(defun qtech:clear-sprinklers (/ ss)
  "Clear all sprinklers from drawing"
  (setq ss (ssget "_X" '((8 . "IRR-SPRINKLER"))))
  (if ss (command "._ERASE" ss ""))
)

;;; ============================================================================
;;; End of qtech-genetic.lsp
;;; ============================================================================

(princ)

