# Qirri Compute Engine

GPU-accelerated irrigation optimization that runs anywhere:
- 💻 **Local CPU** - Any machine
- 🎮 **Local GPU** - NVIDIA CUDA, Jetson Orin
- ☁️ **Cloud GPU** - Modal.com, RunPod

## Quick Start

### 1. Install Dependencies

```bash
cd compute
pip install -r requirements.txt

# For GPU (optional):
# Desktop NVIDIA: pip install cupy-cuda12x
# Jetson Orin: pip install cupy (uses JetPack CUDA)
```

### 2. Run Optimization

#### As CLI
```bash
python qirri_optimizer.py input.json output.json

# Force GPU
python qirri_optimizer.py input.json output.json --gpu

# Force CPU
python qirri_optimizer.py input.json output.json --cpu
```

#### As Python Library
```python
from qirri_optimizer import optimize

result = optimize(
    polygon_data={
        "vertices": [[0,0], [10,0], [10,8], [0,8]],
        "area_m2": 80
    },
    settings_data={
        "pressure_bar": 3.0,
        "max_flow_m3h": 2.0
    }
)

print(f"Sprinklers: {len(result.sprinklers)}")
print(f"CU: {result.cu:.1f}%")
```

#### As API Server
```bash
pip install fastapi uvicorn
uvicorn qirri_optimizer:app --host 0.0.0.0 --port 8000
```

Then:
```bash
curl -X POST http://localhost:8000/optimize \
  -H "Content-Type: application/json" \
  -d '{"area": {"vertices": [[0,0],[10,0],[10,8],[0,8]], "area_m2": 80}}'
```

### 3. Deploy to Modal.com (Cloud GPU)

```bash
pip install modal
modal token new
modal deploy modal_deploy.py
```

## Input Format

```json
{
  "area": {
    "vertices": [[x1,y1], [x2,y2], ...],
    "area_m2": 150.5,
    "perimeter_m": 52.3
  },
  "settings": {
    "pressure_bar": 3.0,
    "max_flow_m3h": 2.0,
    "spacing_factor": 0.55,
    "grid_resolution": 0.5,
    "preferred_brand": "any"
  }
}
```

## Output Format

```json
{
  "sprinklers": [
    {
      "id": "spr-1",
      "x": 2.5,
      "y": 3.0,
      "brand": "RainBird",
      "model": "5000",
      "nozzle": "3.0",
      "radius_m": 5.0,
      "arc": 360,
      "rotation": 0,
      "flow_m3h": 0.22,
      "zone": 1
    }
  ],
  "zones": [
    {"id": 1, "sprinkler_ids": ["spr-1", "spr-2"], "total_flow_m3h": 0.44}
  ],
  "results": {
    "cu": 92.3,
    "du": 87.1,
    "coverage": 98.5,
    "total_flow_m3h": 1.85,
    "computation_time_ms": 150,
    "device": "gpu"
  }
}
```

## Performance

| Scenario | CPU | GPU (T4) | Speedup |
|----------|-----|----------|---------|
| 50m² garden | 50ms | 20ms | 2.5x |
| 500m² lawn | 200ms | 30ms | 6x |
| 5000m² commercial | 3s | 100ms | 30x |
| 5ha golf course | 60s | 1s | 60x |

## Jetson Orin Setup

```bash
# On Jetson (JetPack 5.x)
sudo apt-get install python3-pip
pip3 install numpy scipy

# CuPy for Jetson (uses JetPack's CUDA)
pip3 install cupy

# Verify
python3 -c "import cupy; print('GPU:', cupy.cuda.Device().name)"
```

## Files

```
compute/
├── qirri_optimizer.py   # Main optimizer (CPU/GPU)
├── modal_deploy.py      # Modal.com cloud deployment
├── requirements.txt     # Dependencies
└── README.md           # This file
```

## License

MIT - QTech Design 2026

