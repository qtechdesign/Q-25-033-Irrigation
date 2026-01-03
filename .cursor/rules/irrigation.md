# QTech Design 2026 - Irrigation Planner
## Advanced AutoLISP Application for AutoCAD

## Project Overview
**QTech Design 2026 - Irrigation** is the world's most advanced irrigation planner as an AutoLISP application for AutoCAD. The tool automates sprinkler placement within polyline-defined areas, optimizing for:
- **High Uniformity**: >90% Christiansen's Uniformity Coefficient (CU), >85% Distribution Uniformity (DU)
- **Matched Precipitation**: 10-25 mm/hr to prevent runoff
- **Maximum Water Savings**: 30-50% reduction vs. traditional designs

**Core Philosophy**: Treat irrigation design as a multi-objective optimization problem using a **Hybrid Greedy-Genetic Algorithm** - maximize coverage and uniformity while minimizing water use, overspray, and system cost.

## Units & Standards (All SI)
| Parameter | Unit | Typical Range |
|-----------|------|---------------|
| Length | meters (m) | - |
| Area | square meters (m²) | - |
| Flow | m³/h or L/min | - |
| Precipitation | mm/hr | 10-25 |
| **Pressure** | **bar** | 2.0-5.5 |
| Radius | meters (m) | 1.5-15 |
| Arc | degrees (°) | 90, 180, 270, 360 |

**Pressure Reference**:
- Spray heads: Optimal 2.1 bar, range 1.0-4.8 bar
- Rotors: Optimal 3.1-3.5 bar, range 1.7-7.0 bar
- Regulators: 2.1 bar (PRS30), 2.8 bar (PRS40), 3.1 bar (P45)

## Technology Stack

### AutoLISP Application
- **Platform**: AutoCAD 2018+ with AutoLISP/Visual LISP
- **Algorithms**: Hybrid Greedy + Genetic Algorithm (GA)
- **Simulation**: Grid-based coverage analysis (0.3-0.6m resolution)
- **Data**: Rain Bird + Hunter catalogues as associative lists

### File Structure
```
QTech-Irrigation/
├── lisp/
│   ├── qtech-irrigation.lsp      # Main loader and menu
│   ├── qtech-utils.lsp           # Utility functions
│   ├── qtech-catalogue.lsp       # Sprinkler catalogue data
│   ├── qtech-simulation.lsp      # Grid simulation & CU/DU
│   ├── qtech-placement.lsp       # Greedy placement algorithms
│   ├── qtech-genetic.lsp         # Genetic algorithm optimizer
│   ├── qtech-patterns.lsp        # Spray pattern visualization
│   ├── qtech-zones.lsp           # Zone & flow management
│   ├── qtech-boq.lsp             # Bill of quantities
│   └── qtech-reports.lsp         # Water savings reports
├── data/
│   ├── rainbird-catalogue.csv    # Rain Bird nozzle data
│   └── hunter-catalogue.csv      # Hunter nozzle data
├── blocks/
│   └── sprinkler-blocks.dwg      # Attributed sprinkler blocks
└── docs/
    └── installation.txt          # Installation guide
```

## Core Algorithm: Hybrid Greedy-Genetic Optimizer

### Phase 1: Input Parsing & Preparation
**Inputs**:
- Closed polyline (area boundary)
- Soil type → infiltration rate (mm/hr, e.g., 12 for loam)
- Plant needs → ET rate (mm/day, e.g., 5 for turf)
- Target precipitation rate (mm/hr)
- System pressure (bar)
- Max flow per line (m³/h)
- Wind/humidity factors

**Catalogue Scoring**:
```
Score = (uniformity_fit × 0.4) + (water_efficiency × 0.3) + 
        (cost × 0.2) + (pressure_match × 0.1)
```

**Simulation Grid**:
- Generate dense point grid (0.3-0.6m spacing)
- Ray-casting for containment checks
- Each point tracks cumulative precipitation (mm/hr)

### Phase 2: Perimeter-First Greedy Placement
**Rationale**: Edges are critical - poor edge handling wastes 10-20% water

**Process**:
1. Offset polyline inward 0.15-0.3m (buffer layer)
2. **Corners**: Place quarter/half-circle nozzles (90°, 180°)
3. **Edges**: Space at radius × 0.8-1.0 for head-to-head coverage
4. **Selection**: Test catalogue options, maximize coverage, minimize overspray
5. **Priority**: Low-flow nozzles for water savings
6. **Grid Update**: Mark covered points with precipitation contribution

**Radial Decay Model**:
```
precip(r) = precip_max × (1 - (r/radius)²)
```

### Phase 3: Inward Puzzle-Like Infill
**Rationale**: Treat interior as puzzle - fill gaps progressively from perimeter inward

**Process**:
1. Generate concentric offsets (every 3-6m)
2. For each layer (outer to inner):
   - Identify uncovered grid regions (largest gaps first)
   - Evaluate candidate positions
   - Score: coverage_gain + DU_improvement + precip_match - overspray_penalty
   - Place highest scoring, update grid
3. Backtrack if gaps persist

### Phase 4: Genetic Algorithm Refinement
**Setup** (50-100 generations, population 20-50):

**Chromosome Structure**:
```lisp
((x y model nozzle arc line section) ...)  ; List of head genes
```

**Fitness Function** (Multi-Objective):
- **Obj1 (50%)**: Maximize CU/DU
- **Obj2 (30%)**: Minimize total flow (m³/h)
- **Obj3 (20%)**: Minimize overspray/cost
- **Penalty**: <80% coverage = -∞

**Genetic Operators**:
- **Selection**: Tournament (pick fittest from random subsets)
- **Crossover**: Swap chromosome segments
- **Mutation** (5-10% rate):
  - Nudge positions ±0.6m
  - Swap models/nozzles
  - Adjust arcs
  - Reassign line/section for flow balance
- **Elitism**: Carry top 10% unchanged

**Termination**:
- No improvement >2% in 10 generations
- Max generations reached

### Phase 5: Validation & Output
**Uniformity Metrics**:
```
CU = 100 × (1 - Σ|depth_i - avg| / (n × avg))
DU = 100 × (low_quarter_avg / overall_avg)
```

**Targets**: CU >90%, DU >85%

**Block Attributes**:
| Attribute | Example |
|-----------|---------|
| MODEL | "Rain Bird 5004" |
| NOZZLE | "HE-VAN-12" |
| RADIUS | "4.6" |
| FLOW | "0.15" |
| PRECIP | "15.0" |
| ARC | "180" |
| PRESSURE | "3.0" |
| LINE | "Line 1" |
| SECTION | "Section A" |
| ZONE | "1" |

## Commands

### Main Commands
| Command | Description |
|---------|-------------|
| `QIRR` | Main menu |
| `QIRRAREA` | Select irrigation area |
| `QIRRPARAMS` | Set project parameters |
| `QIRRPLACE` | Run greedy placement |
| `QIRROPTIMIZE` | Run GA optimization |
| `QIRRFULL` | Full auto (greedy + GA) |
| `QIRRPATTERN` | Draw spray patterns |
| `QIRRZONE` | Zone management |
| `QIRRVALIDATE` | Validate CU/DU |
| `QIRRBOQ` | Generate BOQ |
| `QIRRSAVINGS` | Water savings report |

### Utility Commands
| Command | Description |
|---------|-------------|
| `QIRRLAYERS` | Create layers |
| `QIRRGRID` | Show simulation grid |
| `QIRRCOVERAGE` | Show coverage heatmap |
| `QIRRCATALOGUE` | Browse catalogue |
| `QIRRHELP` | Help documentation |

## Layer Structure
| Layer | Color | Description |
|-------|-------|-------------|
| IRR-AREA | 8 | Boundaries |
| IRR-OBSTACLE | 1 | Obstacles |
| IRR-SPRINKLER | 3 | Head symbols |
| IRR-PATTERN | 4 | Spray patterns |
| IRR-PIPE-MAIN | 5 | Main pipes |
| IRR-PIPE-LAT | 6 | Lateral pipes |
| IRR-VALVE | 2 | Valves |
| IRR-GRID | 251 | Simulation grid |
| IRR-COVERAGE | 40-46 | Coverage heatmap |
| IRR-ZONE-n | Varies | Zone elements |

## Water Savings Strategies

### Core Approaches (Target 30-50% savings)
1. **Matched Precipitation**: All heads within ±10% rate
2. **Efficiency Nozzles**: MP Rotators save 33% vs. traditional
3. **Smart Zoning**: Auto-detect sub-areas by soil/exposure
4. **Deficit Optimization**: GA allows 10-15% under-irrigation
5. **Overspray Elimination**: Precise edge fitting

### Savings Calculation
```
Annual_Savings = (Baseline_m³h - Optimized_m³h) × Runtime_hrs × Days
Efficiency_% = (Baseline - Optimized) / Baseline × 100
```

## Catalogue Data Format
```csv
brand,model,nozzle,radius_m,flow_m3h,precip_mmhr,pressure_bar,arc_options,efficiency,score_base
Rain Bird,5004,HE-VAN-12,3.7,0.06,10.0,2.1,"90,180,270,360",high,85
Hunter,MP Rotator,MP2000,5.2,0.07,10.0,2.8,"90,180,210,360",high,88
```

## User Workflow
1. **Select Area**: Draw/select closed polyline
2. **Set Parameters**: Soil, ET, pressure (bar), max flow (m³/h)
3. **Run Optimization**: `QIRRFULL` for complete auto-design
4. **Review**: Check CU/DU metrics, coverage visualization
5. **Refine**: Manual tweaks if needed, re-optimize
6. **Output**: Generate BOQ, water savings report

## Performance Targets
| Metric | Target | Method |
|--------|--------|--------|
| CU | >90% | GA fitness optimization |
| DU | >85% | Low-quarter analysis |
| Coverage | >95% | Grid simulation |
| Water Savings | 30-50% | Matched precip + efficiency nozzles |
| Computation | <5 min | Optimized LISP + smart termination |

## Development Status
- [x] Project specification (irrigation.md)
- [x] Core utilities (qtech-utils.lsp)
- [x] Catalogue system (qtech-catalogue.lsp)
- [x] Grid simulation with CU/DU (qtech-simulation.lsp)
- [x] Greedy placement algorithm (qtech-placement.lsp)
- [x] Genetic algorithm optimizer (qtech-genetic.lsp)
- [x] Spray pattern visualization (qtech-patterns.lsp)
- [x] Zone management (qtech-zones.lsp)
- [x] BOQ generation (qtech-boq.lsp)
- [x] Water savings reporting (qtech-reports.lsp)
- [x] Block attributes & visualization
- [x] Rain Bird + Hunter catalogues

## File Summary
| File | Lines | Description |
|------|-------|-------------|
| qtech-irrigation.lsp | ~450 | Main loader, menu, initialization |
| qtech-utils.lsp | ~420 | Geometry, math, file utilities |
| qtech-catalogue.lsp | ~400 | Sprinkler data, nozzle selection |
| qtech-simulation.lsp | ~350 | Grid sim, CU/DU calculations |
| qtech-placement.lsp | ~500 | Greedy perimeter+infill placement |
| qtech-genetic.lsp | ~450 | GA optimizer (50-100 generations) |
| qtech-patterns.lsp | ~260 | Spray visualization |
| qtech-zones.lsp | ~350 | Zone/flow management |
| qtech-boq.lsp | ~380 | Bill of quantities |
| qtech-reports.lsp | ~300 | Water savings analysis |
| **Total** | **~3,860** | Complete irrigation design system |

## Quick Reference
```
┌──────────────────────────────────────────────────────────────┐
│                    QTECH COMMANDS                            │
├──────────────────────────────────────────────────────────────┤
│ QIRR         │ Main menu                                     │
│ QIRRFULL     │ Full auto (greedy + GA optimization)          │
│ QIRRVALIDATE │ Check CU/DU uniformity metrics                │
│ QIRRSAVINGS  │ Water savings report                          │
│ QIRRBOQ      │ Bill of quantities                            │
└──────────────────────────────────────────────────────────────┘
```

---
*QTech Design 2026 - Irrigation Planner v1.0.0*
*The World's Most Advanced Irrigation Design System*
*Hybrid Greedy-Genetic Algorithm | CU >90% | Water Savings 30-50%*
