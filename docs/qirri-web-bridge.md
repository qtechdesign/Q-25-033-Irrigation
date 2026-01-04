# Qirri Web Bridge - Architecture Specification

> **AutoCAD ↔ JavaScript Optimization Engine**

## Project Summary

**Qirri** is an intelligent irrigation planner that automates sprinkler placement for optimal water distribution uniformity. The current AutoLISP implementation works but is limited by AutoCAD's single-threaded architecture.

**Solution:** Hybrid architecture where AutoCAD handles drawing/input while a JavaScript web app handles heavy computation with GPU acceleration.

---

## Architecture Overview

```
┌─────────────────────┐                    ┌─────────────────────┐
│                     │                    │                     │
│   AutoCAD + LISP    │◄──── Bridge ─────►│   Qirri Web App     │
│   (Drawing Layer)   │                    │   (Compute Layer)   │
│                     │                    │                     │
└─────────────────────┘                    └─────────────────────┘
        │                                          │
        ▼                                          ▼
  • Polyline input                         • GPU optimization
  • User interaction                       • Genetic algorithm
  • DWG output                             • Uniformity simulation
  • BOQ generation                         • Real-time preview
```

---

## Data Flow

### 1. AutoCAD → Web App (Input)

```json
{
  "project_id": "uuid",
  "area": {
    "vertices": [[x1,y1], [x2,y2], ...],
    "area_m2": 150.5,
    "perimeter_m": 52.3,
    "obstacles": [
      {"type": "circle", "center": [x,y], "radius": 2.0},
      {"type": "polygon", "vertices": [[x,y], ...]}
    ]
  },
  "settings": {
    "pressure_bar": 3.0,
    "max_flow_m3h": 2.0,
    "target_cu": 90,
    "target_du": 85,
    "preferred_brand": "rainbird",
    "soil_infiltration_mmh": 25
  },
  "units": "mm"
}
```

### 2. Web App → AutoCAD (Output)

```json
{
  "project_id": "uuid",
  "optimization": {
    "algorithm": "hybrid-greedy-ga",
    "generations": 150,
    "compute_time_ms": 340
  },
  "results": {
    "cu": 92.3,
    "du": 87.1,
    "coverage": 98.5,
    "total_flow_m3h": 1.85
  },
  "sprinklers": [
    {
      "id": 1,
      "position": [1250.5, 2340.2],
      "brand": "RainBird",
      "model": "5000",
      "nozzle": "3.0",
      "radius_m": 4.5,
      "arc": 180,
      "rotation": 45,
      "flow_m3h": 0.23,
      "zone": 1
    }
  ],
  "zones": [
    {"id": 1, "sprinkler_ids": [1,2,3], "total_flow": 0.65}
  ]
}
```

---

## Web App Components

### Frontend (React/Next.js)
- Real-time 2D canvas preview
- Drag-and-drop sprinkler adjustment
- Coverage heatmap visualization
- Settings panel
- Export options

### Backend (Node.js / Python)
- Optimization engine
- Sprinkler catalogue database
- Project storage (Supabase)
- PDF/DXF export

### Compute Engine (GPU-accelerated)
- **WebGPU** for browser-side computation
- **Python + CuPy/PyTorch** for server-side
- Grid simulation (100k+ points)
- Genetic algorithm parallelization

---

## Bridge Options

### Option A: REST API (Simple)
```
LISP → HTTP POST → Server → JSON Response → LISP
```
- Pros: Simple, works everywhere
- Cons: No real-time sync

### Option B: WebSocket (Real-time)
```
LISP ↔ WebSocket ↔ Web App
```
- Pros: Live updates, bidirectional
- Cons: More complex LISP code

### Option C: File Exchange (MVP)
```
LISP → Export JSON file → Web reads → Web writes JSON → LISP imports
```
- Pros: Simplest to implement
- Cons: Manual sync, no automation

**Recommended:** Start with Option C for MVP, migrate to Option A.

---

## Phase 1: MVP (File Exchange)

### AutoCAD Side
```lisp
;; QIRREXPORT - Export area to JSON for web optimization
(defun c:QIRREXPORT () ...)

;; QIRRIMPORT - Import optimized sprinklers from JSON  
(defun c:QIRRIMPORT () ...)
```

### Web Side
- Upload JSON
- Run optimization
- Download result JSON
- Preview in browser

---

## Phase 2: API Integration

### AutoCAD Side
```lisp
;; QIRRSYNC - Send to cloud, receive optimized layout
(defun c:QIRRSYNC () 
  ;; HTTP POST area data
  ;; Receive sprinkler coordinates
  ;; Draw in AutoCAD
)
```

### API Endpoints
```
POST /api/projects              - Create project
POST /api/projects/:id/optimize - Run optimization
GET  /api/projects/:id/results  - Get results
GET  /api/catalogue             - Get sprinkler catalogue
```

---

## Phase 3: Real-time Sync

- WebSocket connection
- Live preview in web app
- Changes sync back to AutoCAD
- Collaborative editing

---

## Tech Stack

| Layer | Technology |
|-------|------------|
| AutoCAD Plugin | AutoLISP + HTTP client |
| Web Frontend | Next.js 14 + React + Canvas/WebGL |
| API | Node.js + tRPC or REST |
| Database | Supabase (PostgreSQL) |
| Compute | WebGPU (browser) / Python (server) |
| Auth | Supabase Auth |
| Storage | Supabase Storage (DWG files) |
| Hosting | Vercel (frontend) + Cloudflare Workers (API) |

---

## Optimization Algorithm

### Hybrid Greedy-Genetic Approach

**Phase 1: Greedy Initialization**
- Perimeter-first placement (edges, corners)
- Triangular grid infill

**Phase 2: Genetic Refinement**
- Population: 50-100 layouts
- Fitness: CU score + coverage - overlap penalty
- Operators: mutation (move/swap), crossover
- Generations: 100-200
- GPU parallel fitness evaluation

**Phase 3: Local Search**
- Fine-tune positions
- Arc optimization
- Zone balancing

---

## Performance Targets

| Metric | AutoLISP (current) | Web App (target) |
|--------|-------------------|------------------|
| 100 m² area | 5-10 sec | <500 ms |
| 1000 m² area | 60+ sec (freezes) | <2 sec |
| 1 hectare | impossible | <10 sec |
| Grid points | 10k max | 1M+ (GPU) |

---

## Next Steps

1. [ ] Create web app scaffold (Next.js + Supabase)
2. [ ] Build JSON export in AutoLISP
3. [ ] Implement basic optimization in JS
4. [ ] Add canvas preview
5. [ ] Build JSON import in AutoLISP
6. [ ] Test full round-trip
7. [ ] Add GPU acceleration
8. [ ] Implement genetic algorithm
9. [ ] Build REST API
10. [ ] Add real-time sync

---

## Repository Structure

```
Q-25-033-Irrigation/
├── lisp/                    # AutoCAD plugin (current)
│   ├── qirri.lsp
│   └── ...
├── web/                     # NEW: Web application
│   ├── app/                 # Next.js app
│   ├── components/          # React components
│   ├── lib/                 # Utilities
│   │   ├── optimizer/       # Optimization engine
│   │   ├── simulation/      # Grid simulation
│   │   └── catalogue/       # Sprinkler data
│   └── api/                 # API routes
├── docs/
│   ├── qirri-web-bridge.md  # This file
│   └── ...
└── data/
    └── catalogues/          # Sprinkler CSVs
```

---

## Contact

**QTech Design**
- Email: info@qtech.hr
- Web: www.qtech.hr

---

*Document created: January 2026*
*Version: 1.0*

