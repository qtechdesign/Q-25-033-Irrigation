# Qirri Compute Engine

GPU-accelerated irrigation optimizer.

## Install

```bash
pip install numpy              # CPU only
pip install numpy cupy-cuda12x # GPU (NVIDIA)
pip install numpy cupy         # GPU (Jetson)
```

## Usage

```bash
# CLI
python qirri_optimizer.py input.json output.json [--gpu|--cpu]

# API Server
pip install fastapi uvicorn
uvicorn qirri_optimizer:app --port 8000

# Cloud (Modal.com)
pip install modal
modal deploy modal_deploy.py
```

## API

```bash
POST /optimize  {"area": {...}, "settings": {...}}
GET  /health    {"status": "ok", "gpu": true}
```

## Algorithm

1. **Select** optimal nozzle from catalogue
2. **Place** on triangular grid (spacing = radius × 1.1)
3. **Simulate** precipitation at 0.5m grid points
4. **Calculate** CU/DU uniformity
5. **Zone** by hydraulic flow limits

Core: `qirri_optimizer.py` (~220 lines)
