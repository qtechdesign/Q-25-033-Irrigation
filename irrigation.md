# Qirri - Intelligent Irrigation Planner
## Web Bridge Architecture v2.0
### by QTech Design

## Project Overview

**Qirri** is an intelligent irrigation planner that combines:
- **AutoCAD LISP** - Industry-standard drawing input/output
- **Web App** - Real-time preview, optimization, collaboration
- **Compute API** - Heavy optimization (GPU-ready)

**Core Goal**: Automate sprinkler placement for >90% CU, >85% DU, 30-50% water savings.

---

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   AutoCAD       │     │   Web App       │     │   Compute API   │
│   (LISP)        │◄───►│   (Next.js)     │◄───►│   (GPU-ready)   │
│                 │     │                 │     │                 │
│ • Polyline draw │     │ • Preview       │     │ • Optimization  │
│ • DWG export    │     │ • Settings      │     │ • Simulation    │
│ • Block insert  │     │ • Results       │     │ • GA refinement │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        │                       │                       │
        │              Cloudflare Pages          ???????????????
        │                                        (see options below)
        │
   JSON Exchange
```

---

## Tech Stack Options

### Frontend + API Gateway (Decided)

| Component | Technology | Why |
|-----------|------------|-----|
| **Web App** | Next.js 14 | React, static export, great DX |
| **Hosting** | Cloudflare Pages | Fast, free, global CDN |
| **Database** | Supabase | PostgreSQL, auth, realtime |
| **Auth** | Supabase Auth | Simple, secure |

### Compute API (Options to Evaluate)

#### Option 1: Cloudflare Workers ✅ Current
```
Pros:
+ Edge-fast (50ms cold start)
+ Simple deployment
+ Free tier generous
+ Already implemented

Cons:
- NO GPU/CUDA
- 128MB memory limit
- 30s CPU time limit
- Not ideal for heavy GA
```
**Best for**: Small areas (<500m²), basic optimization

---

#### Option 2: Modal.com 🔥 Recommended for GPU
```python
# Example Modal function
@modal.function(gpu="T4")
def optimize_irrigation(polygon, settings):
    # CUDA-accelerated grid simulation
    # PyTorch for GA parallelization
    return sprinklers, results
```
```
Pros:
+ CUDA GPU (T4, A10G, A100)
+ Python ecosystem (NumPy, PyTorch, CuPy)
+ Pay-per-second billing
+ Scale to zero
+ Easy deployment

Cons:
- Cold start ~2-5s for GPU
- Learning curve
- Cost for heavy use (~$0.20/GPU-hour)
```
**Best for**: Large areas, complex optimization, ML features

---

#### Option 3: Replicate
```
Pros:
+ Simple API for ML models
+ Pre-built infrastructure
+ Good for inference

Cons:
- More for ML inference than custom compute
- Less flexible
```
**Best for**: If we add ML-based placement prediction

---

#### Option 4: RunPod Serverless
```
Pros:
+ Cheap GPU ($0.10/hr for T4)
+ Docker-based
+ Good for batch jobs

Cons:
- Less polished than Modal
- Cold starts can be slow
```
**Best for**: Budget-conscious GPU compute

---

#### Option 5: Supabase Edge Functions + External GPU
```
Supabase Edge → triggers → Modal/RunPod
```
```
Pros:
+ Keep everything in Supabase ecosystem
+ Edge functions for fast routing
+ GPU for heavy compute

Cons:
- More complex architecture
- Two services to manage
```

---

#### Option 6: Self-hosted (Fly.io / Railway)
```
Pros:
+ Full control
+ Can add GPU machines
+ Persistent state

Cons:
- Must manage infrastructure
- Fixed cost even when idle
```

---

## Recommendation

### Phase 1 (Now): Cloudflare Workers
- ✅ Already built
- Works for 90% of use cases (residential, small commercial)
- Fast, cheap, simple

### Phase 2 (When needed): Add Modal.com
- When users need: large areas, complex shapes, ML features
- API structure same, just route to Modal for heavy jobs
- GPU acceleration for <1 second optimization

### Architecture with Modal:

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Web App       │────►│ Cloudflare      │────►│  Modal.com      │
│   (Next.js)     │     │ Worker (router) │     │  (GPU compute)  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                               │
                               ▼
                        ┌─────────────────┐
                        │   Supabase      │
                        │   (database)    │
                        └─────────────────┘
```

**Worker decides**:
- Small job (<100 heads) → compute locally
- Large job → send to Modal GPU

---

## Do We Actually Need GPU?

### Current Performance (CPU-only)

| Task | Points/Heads | Time | GPU Benefit |
|------|--------------|------|-------------|
| Grid simulation | 10k points | <100ms | Not needed |
| Coverage calc | 50 heads | <50ms | Not needed |
| GA optimization | 100 gen × 50 pop | 2-5s | Maybe 10x faster |
| Large area | 100k+ points | 30s+ | **YES, 100x faster** |

### When GPU Helps

1. **Very large areas** (>1 hectare, golf courses)
2. **Fine grid resolution** (<0.25m, 100k+ points)
3. **Complex genetic algorithm** (1000+ generations)
4. **Real-time drag-and-drop** (recalc on every move)
5. **ML-based optimization** (neural network placement)

### Verdict

**Start without GPU** - Cloudflare Workers handle 95% of projects.  
**Add GPU later** via Modal.com when:
- Users complain about speed
- We add ML features
- Enterprise/golf course clients need it

---

## Data Flow (JSON Exchange)

### AutoCAD → Web (Export)
```json
{
  "version": "1.0",
  "area": {
    "vertices": [[x1,y1], [x2,y2], ...],
    "area_m2": 150.5,
    "perimeter_m": 52.3
  },
  "obstacles": [],
  "settings": {
    "pressure_bar": 3.0,
    "max_flow_m3h": 2.0,
    "target_cu": 90,
    "target_du": 85,
    "spacing_factor": 0.55,
    "preferred_brand": "any"
  },
  "units": "mm"
}
```

### Web → AutoCAD (Import)
```json
{
  "version": "1.0",
  "sprinklers": [
    {
      "x": 1250.5,
      "y": 2340.2,
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
    {"id": 1, "sprinkler_indices": [0,1,2], "total_flow_m3h": 0.65}
  ],
  "results": {
    "cu": 92.3,
    "du": 87.1,
    "coverage": 98.5,
    "total_flow_m3h": 1.85
  }
}
```

---

## Optimization Algorithm

### Phase 1: Grid Placement (Fast)
- Triangular pattern at radius × spacing_factor × 2
- Point-in-polygon filtering
- O(n) complexity, <100ms

### Phase 2: Uniformity Simulation
- Generate grid (0.5-1.0m resolution)
- Calculate precipitation at each point
- Compute CU/DU metrics
- O(n × m) where n=grid, m=heads

### Phase 3: Genetic Refinement (Optional)
- Population: 50 chromosomes
- Generations: 100-200
- Fitness: CU × coverage - overspray
- GPU parallelizable

---

## Units & Standards

| Parameter | Unit | Range |
|-----------|------|-------|
| Length | meters (m) | - |
| Area | m² | - |
| Flow | m³/h | 0.05-2.0 |
| Precipitation | mm/hr | 10-25 |
| Pressure | bar | 2.0-5.5 |
| Radius | m | 1.5-15 |
| Arc | degrees | 90, 180, 270, 360 |

---

## Project Structure

```
Q-25-033-Irrigation/
├── lisp/                    # AutoCAD LISP plugin
│   ├── qirri.lsp           # Main loader
│   ├── qirri-utils.lsp     # Utilities
│   ├── qirri-catalogue.lsp # Sprinkler data
│   └── ...
│
├── web/                     # Web application
│   ├── src/
│   │   ├── app/            # Next.js pages
│   │   ├── components/     # React UI
│   │   └── lib/
│   │       ├── optimizer/  # JS optimizer
│   │       └── supabase/   # Database
│   └── worker/             # Cloudflare Worker API
│
├── compute/                 # 🆕 GPU compute (future)
│   └── modal_app.py        # Modal.com functions
│
└── docs/
    └── qirri-web-bridge.md # Architecture spec
```

---

## Development Roadmap

### ✅ Phase 1: LISP Plugin (Complete)
- AutoCAD integration
- Local optimization
- Mac + Windows compatible

### ✅ Phase 2: Web App Scaffold (Complete)
- Next.js + Tailwind
- Canvas preview
- JSON import/export
- Cloudflare Worker API

### 🔄 Phase 3: Full Integration (Current)
- [ ] QIRREXPORT command in LISP
- [ ] QIRRIMPORT command in LISP
- [ ] Supabase project storage
- [ ] User authentication
- [ ] Deploy to Cloudflare

### 📋 Phase 4: GPU Compute (Future)
- [ ] Modal.com integration
- [ ] Large area optimization
- [ ] ML-based placement
- [ ] Real-time collaboration

---

## Contact

**QTech Design**
- Email: info@qtech.hr
- Web: www.qtech.hr

---

*Qirri v2.0 - Web Bridge Architecture*
*AutoCAD ↔ Web ↔ GPU Compute*

