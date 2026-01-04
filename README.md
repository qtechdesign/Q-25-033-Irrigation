# Qirri

**GPU-Accelerated Irrigation Sprinkler Placement**

```
AutoCAD ← JSON → Web App ← API → Python Compute (CPU/GPU)
```

---

## The Algorithm

Qirri places sprinklers using a **triangular grid pattern** optimized for head-to-head coverage:

```
1. SELECT NOZZLE     → Match area size to optimal throw radius
2. CALCULATE SPACING → radius × 2 × 0.55 (industry standard)
3. PLACE ON GRID     → Equilateral triangle pattern (sin 60° = 0.866)
4. SIMULATE          → Calculate precipitation at 0.5m grid points
5. EVALUATE          → Compute CU/DU uniformity metrics
```

### Uniformity Metrics

| Metric | Formula | Target |
|--------|---------|--------|
| **CU** (Christiansen) | `100 × (1 - Σ|xi - μ| / nμ)` | >90% |
| **DU** (Distribution) | `100 × (avg low quarter / avg all)` | >85% |

### Precipitation Model

```
precip(r) = P × (1 - (r/R)²)

where:
  P = nozzle precipitation rate (mm/hr)
  R = throw radius (m)  
  r = distance from sprinkler (m)
```

---

## Quick Start

### Option 1: Compute Only

```bash
git clone https://github.com/qtechdesign/Q-25-033-Irrigation.git
cd Q-25-033-Irrigation/compute
pip install numpy
python qirri_optimizer.py input.json output.json
```

### Option 2: With GPU

```bash
pip install numpy cupy-cuda12x  # or cupy for Jetson
python qirri_optimizer.py input.json output.json --gpu
```

### Option 3: As API Server

```bash
pip install numpy fastapi uvicorn
uvicorn qirri_optimizer:app --port 8000
curl -X POST http://localhost:8000/optimize \
  -H "Content-Type: application/json" \
  -d '{"area": {"vertices": [[0,0],[10,0],[10,8],[0,8]], "area_m2": 80}}'
```

---

## Input Format

```json
{
  "area": {
    "vertices": [[0,0], [10,0], [10,8], [0,8]],
    "area_m2": 80
  },
  "settings": {
    "pressure_bar": 3.0,
    "max_flow_m3h": 2.0,
    "spacing_factor": 0.55,
    "preferred_brand": "any"
  }
}
```

## Output Format

```json
{
  "sprinklers": [
    {"id": "spr-1", "x": 2.5, "y": 2.2, "brand": "RainBird", "model": "5000", 
     "nozzle": "3.0", "radius_m": 5.0, "arc": 360, "flow_m3h": 0.22, "zone": 1}
  ],
  "zones": [
    {"id": 1, "sprinkler_ids": ["spr-1", "spr-2"], "total_flow_m3h": 0.44}
  ],
  "results": {
    "cu": 92.3,
    "du": 87.1,
    "coverage": 98.5,
    "total_flow_m3h": 1.32,
    "time_ms": 45,
    "device": "cpu"
  }
}
```

---

## Performance

| Area | CPU | GPU | Speedup |
|------|-----|-----|---------|
| 50m² | 50ms | 20ms | 2.5× |
| 500m² | 200ms | 30ms | 6× |
| 5000m² | 3s | 100ms | 30× |
| 5ha | 60s | 1s | 60× |

---

## Project Structure

```
├── compute/
│   ├── qirri_optimizer.py   # Core algorithm (220 lines)
│   ├── modal_deploy.py      # Cloud GPU deployment
│   └── requirements.txt
├── web/                     # Next.js frontend + Cloudflare Worker
├── lisp/                    # AutoCAD plugin (archived: lisp-only branch)
└── data/                    # Sprinkler catalogues
```

---

## Roadmap

- [x] Core algorithm with GPU support
- [x] CLI + API interfaces
- [x] Modal.com cloud deployment
- [x] Web app scaffold
- [ ] Supabase integration
- [ ] AutoCAD export/import commands
- [ ] Genetic algorithm refinement
- [ ] Real-time collaboration

---

## License

MIT © 2026 QTech Design

**Contact:** info@qtech.hr | www.qtech.hr
