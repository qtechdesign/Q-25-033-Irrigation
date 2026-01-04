"""
Qirri Optimizer - GPU-Accelerated Irrigation Placement
======================================================
QTech Design (www.qtech.hr) - MIT License

Supports: CPU (NumPy), GPU (CuPy/CUDA), Jetson Orin, Modal.com

Usage:
    python qirri_optimizer.py input.json output.json [--gpu|--cpu]
    uvicorn qirri_optimizer:app --port 8000
"""

import json
import math
import time
from dataclasses import dataclass
from typing import List, Tuple, Optional, Dict
import numpy as np

# GPU Support
try:
    import cupy as cp
    GPU_AVAILABLE = True
except ImportError:
    cp = None
    GPU_AVAILABLE = False

# =============================================================================
# DATA TYPES
# =============================================================================

@dataclass
class Sprinkler:
    id: str
    x: float
    y: float
    brand: str
    model: str
    nozzle: str
    radius_m: float
    arc: int
    rotation: float
    flow_m3h: float
    precip_mmh: float
    pressure_bar: float
    zone: int = 0

@dataclass
class Settings:
    pressure_bar: float = 3.0
    max_flow_m3h: float = 2.0
    target_cu: float = 90.0
    target_du: float = 85.0
    spacing_factor: float = 0.55
    preferred_brand: str = "any"
    grid_resolution: float = 0.5

# Sprinkler Catalogue
CATALOGUE = [
    {"brand": "RainBird", "model": "5000", "nozzle": "2.0", "radius_m": 4.0, "flow_m3h": 0.15, "precip_mmh": 12, "pressure_bar": 3.0},
    {"brand": "RainBird", "model": "5000", "nozzle": "3.0", "radius_m": 5.0, "flow_m3h": 0.22, "precip_mmh": 14, "pressure_bar": 3.0},
    {"brand": "RainBird", "model": "5000", "nozzle": "4.0", "radius_m": 6.0, "flow_m3h": 0.30, "precip_mmh": 15, "pressure_bar": 3.0},
    {"brand": "Hunter", "model": "PGP", "nozzle": "2.0", "radius_m": 4.2, "flow_m3h": 0.16, "precip_mmh": 12, "pressure_bar": 3.0},
    {"brand": "Hunter", "model": "PGP", "nozzle": "3.0", "radius_m": 5.2, "flow_m3h": 0.24, "precip_mmh": 14, "pressure_bar": 3.0},
    {"brand": "Hunter", "model": "PGP", "nozzle": "4.0", "radius_m": 6.3, "flow_m3h": 0.32, "precip_mmh": 15, "pressure_bar": 3.0},
    {"brand": "Hunter", "model": "MP", "nozzle": "1000", "radius_m": 3.0, "flow_m3h": 0.08, "precip_mmh": 10, "pressure_bar": 2.8},
    {"brand": "Hunter", "model": "MP", "nozzle": "2000", "radius_m": 4.5, "flow_m3h": 0.12, "precip_mmh": 10, "pressure_bar": 2.8},
    {"brand": "Hunter", "model": "MP", "nozzle": "3000", "radius_m": 6.0, "flow_m3h": 0.18, "precip_mmh": 10, "pressure_bar": 2.8},
]

# =============================================================================
# CORE ALGORITHM
# =============================================================================

def point_in_polygon_batch(points: np.ndarray, vertices: np.ndarray, use_gpu: bool = False) -> np.ndarray:
    """Ray casting point-in-polygon test - vectorized for GPU acceleration."""
    xp = cp if (use_gpu and GPU_AVAILABLE) else np
    if use_gpu and GPU_AVAILABLE:
        points = cp.asarray(points)
        vertices = cp.asarray(vertices)
    
    px, py = points[:, 0], points[:, 1]
    inside = xp.zeros(len(points), dtype=bool)
    n = len(vertices)
    j = n - 1
    
    for i in range(n):
        xi, yi = vertices[i]
        xj, yj = vertices[j]
        cond = ((yi > py) != (yj > py)) & (px < (xj - xi) * (py - yi) / (yj - yi + 1e-10) + xi)
        inside = xp.where(cond, ~inside, inside)
        j = i
    
    return cp.asnumpy(inside) if (use_gpu and GPU_AVAILABLE) else inside


def generate_grid(bounds: Tuple, resolution: float, vertices: np.ndarray, use_gpu: bool = False) -> np.ndarray:
    """Generate simulation grid points inside polygon."""
    min_x, min_y, max_x, max_y = bounds
    xs = np.arange(min_x, max_x + resolution, resolution)
    ys = np.arange(min_y, max_y + resolution, resolution)
    xx, yy = np.meshgrid(xs, ys)
    points = np.column_stack([xx.ravel(), yy.ravel()])
    inside = point_in_polygon_batch(points, vertices, use_gpu)
    return points[inside]


def simulate_coverage(grid: np.ndarray, sprinklers: List[Sprinkler], use_gpu: bool = False) -> np.ndarray:
    """Calculate precipitation at each grid point using quadratic decay model."""
    xp = cp if (use_gpu and GPU_AVAILABLE) else np
    if use_gpu and GPU_AVAILABLE:
        grid = cp.asarray(grid)
    
    precip = xp.zeros(len(grid))
    
    for spr in sprinklers:
        dx, dy = grid[:, 0] - spr.x, grid[:, 1] - spr.y
        dist = xp.sqrt(dx * dx + dy * dy)
        in_range = dist <= spr.radius_m
        contribution = xp.maximum(spr.precip_mmh * (1 - (dist / spr.radius_m) ** 2), 0)
        precip = xp.where(in_range, precip + contribution, precip)
    
    return cp.asnumpy(precip) if (use_gpu and GPU_AVAILABLE) else precip


def calculate_uniformity(precip: np.ndarray) -> Tuple[float, float, float]:
    """
    Calculate irrigation uniformity metrics:
    - CU: Christiansen's Uniformity (how even is the distribution)
    - DU: Distribution Uniformity (low quarter vs average)
    - Coverage: Percentage of area receiving water
    """
    wet = precip[precip > 0]
    if len(wet) == 0:
        return 0.0, 0.0, 0.0
    
    avg = np.mean(wet)
    cu = 100 * (1 - np.sum(np.abs(wet - avg)) / (len(wet) * avg))
    
    lq = max(1, len(wet) // 4)
    du = 100 * np.mean(np.sort(wet)[:lq]) / avg
    
    coverage = 100 * len(wet) / len(precip)
    
    return float(np.clip(cu, 0, 100)), float(np.clip(du, 0, 100)), float(coverage)


def select_nozzle(area_m2: float, settings: Settings) -> Dict:
    """Select optimal nozzle based on area size and pressure."""
    target_r = 3.0 if area_m2 < 50 else 4.0 if area_m2 < 200 else 5.0 if area_m2 < 500 else 6.0
    
    compatible = [n for n in CATALOGUE if abs(n["pressure_bar"] - settings.pressure_bar) < 1.0] or CATALOGUE
    
    if settings.preferred_brand != "any":
        brand_match = [n for n in compatible if n["brand"].lower() == settings.preferred_brand.lower()]
        if brand_match:
            compatible = brand_match
    
    return min(compatible, key=lambda n: abs(n["radius_m"] - target_r))


def place_sprinklers(vertices: np.ndarray, bounds: Tuple, nozzle: Dict, settings: Settings) -> List[Sprinkler]:
    """Place sprinklers on triangular grid for optimal head-to-head coverage."""
    min_x, min_y, max_x, max_y = bounds
    spacing = nozzle["radius_m"] * 2 * settings.spacing_factor
    margin = spacing / 2
    
    sprinklers = []
    y, row, spr_id = min_y + margin, 0, 1
    
    while y <= max_y - margin:
        offset = 0 if row % 2 == 0 else spacing / 2
        x = min_x + margin + offset
        
        while x <= max_x - margin:
            # Simple point-in-polygon check
            n, inside, j = len(vertices), False, len(vertices) - 1
            for i in range(n):
                xi, yi = vertices[i]
                xj, yj = vertices[j]
                if ((yi > y) != (yj > y)) and (x < (xj - xi) * (y - yi) / (yj - yi) + xi):
                    inside = not inside
                j = i
            
            if inside:
                sprinklers.append(Sprinkler(
                    id=f"spr-{spr_id}", x=x, y=y, brand=nozzle["brand"], model=nozzle["model"],
                    nozzle=nozzle["nozzle"], radius_m=nozzle["radius_m"], arc=360, rotation=0,
                    flow_m3h=nozzle["flow_m3h"], precip_mmh=nozzle["precip_mmh"], pressure_bar=nozzle["pressure_bar"]
                ))
                spr_id += 1
            x += spacing
        
        y += spacing * 0.866  # sin(60°) for equilateral triangle grid
        row += 1
    
    return sprinklers


def assign_zones(sprinklers: List[Sprinkler], max_flow: float) -> Tuple[List[Sprinkler], List[Dict]]:
    """Group sprinklers into zones based on hydraulic flow limits."""
    zones, zone_id, flow, ids = [], 1, 0, []
    
    for spr in sprinklers:
        if flow + spr.flow_m3h > max_flow and ids:
            zones.append({"id": zone_id, "sprinkler_ids": ids.copy(), "total_flow_m3h": flow})
            zone_id, flow, ids = zone_id + 1, 0, []
        
        spr.zone = zone_id
        ids.append(spr.id)
        flow += spr.flow_m3h
    
    if ids:
        zones.append({"id": zone_id, "sprinkler_ids": ids, "total_flow_m3h": flow})
    
    return sprinklers, zones


# =============================================================================
# MAIN API
# =============================================================================

def optimize(polygon: Dict, settings: Dict = None, use_gpu: Optional[bool] = None) -> Dict:
    """
    Main optimization function.
    
    Input:
        polygon: {"vertices": [[x,y]...], "area_m2": float}
        settings: {"pressure_bar": 3.0, "max_flow_m3h": 2.0, ...}
    
    Returns:
        {"sprinklers": [...], "zones": [...], "results": {"cu", "du", "coverage", ...}}
    """
    start = time.time()
    
    use_gpu = GPU_AVAILABLE if use_gpu is None else (use_gpu and GPU_AVAILABLE)
    
    vertices = np.array(polygon["vertices"])
    area_m2 = polygon.get("area_m2", 100)
    
    s = Settings(**(settings or {})) if isinstance(settings, dict) else Settings()
    
    bounds = (vertices[:, 0].min(), vertices[:, 1].min(), vertices[:, 0].max(), vertices[:, 1].max())
    
    # Algorithm: Select → Place → Zone → Simulate → Evaluate
    nozzle = select_nozzle(area_m2, s)
    sprinklers = place_sprinklers(vertices, bounds, nozzle, s)
    sprinklers, zones = assign_zones(sprinklers, s.max_flow_m3h)
    grid = generate_grid(bounds, s.grid_resolution, vertices, use_gpu)
    precip = simulate_coverage(grid, sprinklers, use_gpu)
    cu, du, coverage = calculate_uniformity(precip)
    
    return {
        "sprinklers": [
            {"id": sp.id, "x": sp.x, "y": sp.y, "brand": sp.brand, "model": sp.model,
             "nozzle": sp.nozzle, "radius_m": sp.radius_m, "arc": sp.arc, "rotation": sp.rotation,
             "flow_m3h": sp.flow_m3h, "zone": sp.zone}
            for sp in sprinklers
        ],
        "zones": zones,
        "results": {
            "cu": round(cu, 1),
            "du": round(du, 1),
            "coverage": round(coverage, 1),
            "total_flow_m3h": round(sum(sp.flow_m3h for sp in sprinklers), 3),
            "sprinkler_count": len(sprinklers),
            "zone_count": len(zones),
            "time_ms": round((time.time() - start) * 1000, 1),
            "device": "gpu" if use_gpu else "cpu"
        }
    }


# =============================================================================
# CLI & API
# =============================================================================

def main():
    import sys
    if len(sys.argv) < 3:
        print("Qirri Optimizer - GPU-Accelerated Irrigation Placement")
        print(f"GPU: {'Available' if GPU_AVAILABLE else 'Not available (install cupy)'}")
        print("\nUsage: python qirri_optimizer.py input.json output.json [--gpu|--cpu]")
        return
    
    with open(sys.argv[1]) as f:
        data = json.load(f)
    
    use_gpu = True if "--gpu" in sys.argv else False if "--cpu" in sys.argv else None
    
    result = optimize(data.get("area", data.get("polygon", {})), data.get("settings", {}), use_gpu)
    
    with open(sys.argv[2], 'w') as f:
        json.dump(result, f, indent=2)
    
    r = result["results"]
    print(f"✓ {r['sprinkler_count']} sprinklers, {r['zone_count']} zones")
    print(f"  CU: {r['cu']}% | DU: {r['du']}% | Coverage: {r['coverage']}%")
    print(f"  Flow: {r['total_flow_m3h']} m³/h | Time: {r['time_ms']}ms ({r['device']})")


# FastAPI Server
try:
    from fastapi import FastAPI
    from fastapi.middleware.cors import CORSMiddleware
    
    app = FastAPI(title="Qirri Optimizer", version="1.0")
    app.add_middleware(CORSMiddleware, allow_origins=["*"], allow_methods=["*"], allow_headers=["*"])
    
    @app.get("/health")
    def health():
        return {"status": "ok", "gpu": GPU_AVAILABLE}
    
    @app.post("/optimize")
    def api_optimize(data: Dict):
        return optimize(data.get("area", {}), data.get("settings", {}), data.get("use_gpu"))

except ImportError:
    app = None


if __name__ == "__main__":
    main()
