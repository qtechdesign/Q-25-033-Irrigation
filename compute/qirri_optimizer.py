"""
Qirri Optimizer - Irrigation Placement Engine
==============================================
Supports: CPU, NVIDIA GPU (CUDA), Jetson Orin

Usage:
    # As library
    from qirri_optimizer import optimize
    result = optimize(polygon, settings)
    
    # As CLI
    python qirri_optimizer.py input.json output.json
    
    # As API server
    uvicorn qirri_optimizer:app --host 0.0.0.0 --port 8000

Author: QTech Design (www.qtech.hr)
"""

import json
import math
import time
from dataclasses import dataclass, field
from typing import List, Tuple, Optional, Dict, Any

# Try to import GPU libraries
try:
    import cupy as cp
    GPU_AVAILABLE = True
    print("✓ GPU acceleration available (CuPy)")
except ImportError:
    cp = None
    GPU_AVAILABLE = False
    print("○ Running on CPU (install cupy for GPU)")

import numpy as np

# ============================================================================
# Data Types
# ============================================================================

@dataclass
class Point:
    x: float
    y: float

@dataclass
class Polygon:
    vertices: List[Point]
    area_m2: float
    perimeter_m: float

@dataclass
class Settings:
    pressure_bar: float = 3.0
    max_flow_m3h: float = 2.0
    target_cu: float = 90.0
    target_du: float = 85.0
    spacing_factor: float = 0.55
    preferred_brand: str = "any"
    grid_resolution: float = 0.5  # meters

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
class OptimizationResult:
    sprinklers: List[Sprinkler]
    zones: List[Dict]
    cu: float
    du: float
    coverage: float
    total_flow_m3h: float
    computation_time_ms: float
    device: str  # "cpu" or "gpu"

# ============================================================================
# Sprinkler Catalogue
# ============================================================================

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

# ============================================================================
# Core Algorithms
# ============================================================================

def point_in_polygon(px: float, py: float, vertices: np.ndarray) -> bool:
    """Ray casting algorithm for point-in-polygon test"""
    n = len(vertices)
    inside = False
    j = n - 1
    for i in range(n):
        xi, yi = vertices[i]
        xj, yj = vertices[j]
        if ((yi > py) != (yj > py)) and (px < (xj - xi) * (py - yi) / (yj - yi) + xi):
            inside = not inside
        j = i
    return inside


def point_in_polygon_vectorized(points: np.ndarray, vertices: np.ndarray, use_gpu: bool = False) -> np.ndarray:
    """Vectorized point-in-polygon for many points (GPU accelerated if available)"""
    xp = cp if (use_gpu and GPU_AVAILABLE) else np
    
    if use_gpu and GPU_AVAILABLE:
        points = cp.asarray(points)
        vertices = cp.asarray(vertices)
    
    n_points = len(points)
    n_vertices = len(vertices)
    
    px = points[:, 0]
    py = points[:, 1]
    
    inside = xp.zeros(n_points, dtype=bool)
    
    j = n_vertices - 1
    for i in range(n_vertices):
        xi, yi = vertices[i]
        xj, yj = vertices[j]
        
        cond1 = (yi > py) != (yj > py)
        cond2 = px < (xj - xi) * (py - yi) / (yj - yi + 1e-10) + xi
        
        inside = xp.where(cond1 & cond2, ~inside, inside)
        j = i
    
    if use_gpu and GPU_AVAILABLE:
        inside = cp.asnumpy(inside)
    
    return inside


def generate_grid(bounds: Tuple[float, float, float, float], resolution: float, 
                  vertices: np.ndarray, use_gpu: bool = False) -> np.ndarray:
    """Generate grid points inside polygon"""
    min_x, min_y, max_x, max_y = bounds
    
    xs = np.arange(min_x, max_x + resolution, resolution)
    ys = np.arange(min_y, max_y + resolution, resolution)
    
    xx, yy = np.meshgrid(xs, ys)
    points = np.column_stack([xx.ravel(), yy.ravel()])
    
    # Filter to points inside polygon
    inside = point_in_polygon_vectorized(points, vertices, use_gpu)
    
    return points[inside]


def simulate_coverage(grid: np.ndarray, sprinklers: List[Sprinkler], 
                      use_gpu: bool = False) -> np.ndarray:
    """Calculate precipitation at each grid point from all sprinklers"""
    xp = cp if (use_gpu and GPU_AVAILABLE) else np
    
    if use_gpu and GPU_AVAILABLE:
        grid = cp.asarray(grid)
    
    n_points = len(grid)
    precip = xp.zeros(n_points)
    
    for spr in sprinklers:
        # Distance from each grid point to sprinkler
        dx = grid[:, 0] - spr.x
        dy = grid[:, 1] - spr.y
        dist = xp.sqrt(dx * dx + dy * dy)
        
        # Points within radius
        in_range = dist <= spr.radius_m
        
        # Quadratic decay model
        ratio = dist / spr.radius_m
        contribution = spr.precip_mmh * (1 - ratio ** 2)
        contribution = xp.maximum(contribution, 0)
        
        precip = xp.where(in_range, precip + contribution, precip)
    
    if use_gpu and GPU_AVAILABLE:
        precip = cp.asnumpy(precip)
    
    return precip


def calculate_uniformity(precip: np.ndarray) -> Tuple[float, float, float]:
    """Calculate CU, DU, and coverage from precipitation array"""
    non_zero = precip[precip > 0]
    
    if len(non_zero) == 0:
        return 0.0, 0.0, 0.0
    
    n = len(non_zero)
    avg = np.mean(non_zero)
    
    # Christiansen's Uniformity (CU)
    deviations = np.sum(np.abs(non_zero - avg))
    cu = 100 * (1 - deviations / (n * avg))
    
    # Distribution Uniformity (DU) - low quarter
    sorted_vals = np.sort(non_zero)
    lq_count = max(1, n // 4)
    lq_avg = np.mean(sorted_vals[:lq_count])
    du = 100 * (lq_avg / avg)
    
    # Coverage
    coverage = 100 * (len(non_zero) / len(precip))
    
    return float(np.clip(cu, 0, 100)), float(np.clip(du, 0, 100)), float(coverage)


def select_nozzle(area_m2: float, settings: Settings) -> Dict:
    """Select best nozzle from catalogue based on area and settings"""
    # Target radius based on area
    if area_m2 < 50:
        target_radius = 3.0
    elif area_m2 < 200:
        target_radius = 4.0
    elif area_m2 < 500:
        target_radius = 5.0
    else:
        target_radius = 6.0
    
    # Filter by pressure
    compatible = [n for n in CATALOGUE if abs(n["pressure_bar"] - settings.pressure_bar) < 1.0]
    if not compatible:
        compatible = CATALOGUE
    
    # Filter by brand preference
    if settings.preferred_brand != "any":
        brand_match = [n for n in compatible if n["brand"].lower() == settings.preferred_brand.lower()]
        if brand_match:
            compatible = brand_match
    
    # Find closest to target radius
    best = min(compatible, key=lambda n: abs(n["radius_m"] - target_radius))
    return best


def place_sprinklers_grid(vertices: np.ndarray, bounds: Tuple, nozzle: Dict, 
                          settings: Settings) -> List[Sprinkler]:
    """Place sprinklers on triangular grid pattern"""
    min_x, min_y, max_x, max_y = bounds
    spacing = nozzle["radius_m"] * 2 * settings.spacing_factor
    margin = spacing / 2
    
    sprinklers = []
    spr_id = 1
    
    y = min_y + margin
    row = 0
    
    while y <= max_y - margin:
        offset = 0 if row % 2 == 0 else spacing / 2
        x = min_x + margin + offset
        
        while x <= max_x - margin:
            if point_in_polygon(x, y, vertices):
                sprinklers.append(Sprinkler(
                    id=f"spr-{spr_id}",
                    x=x, y=y,
                    brand=nozzle["brand"],
                    model=nozzle["model"],
                    nozzle=nozzle["nozzle"],
                    radius_m=nozzle["radius_m"],
                    arc=360,
                    rotation=0,
                    flow_m3h=nozzle["flow_m3h"],
                    precip_mmh=nozzle["precip_mmh"],
                    pressure_bar=nozzle["pressure_bar"],
                ))
                spr_id += 1
            x += spacing
        
        y += spacing * 0.866  # sin(60°) for equilateral triangle
        row += 1
    
    return sprinklers


def assign_zones(sprinklers: List[Sprinkler], max_flow: float) -> Tuple[List[Sprinkler], List[Dict]]:
    """Assign sprinklers to zones based on flow limits"""
    zones = []
    zone_id = 1
    current_flow = 0
    current_ids = []
    
    for spr in sprinklers:
        if current_flow + spr.flow_m3h > max_flow and current_ids:
            zones.append({
                "id": zone_id,
                "sprinkler_ids": current_ids.copy(),
                "total_flow_m3h": current_flow
            })
            zone_id += 1
            current_flow = 0
            current_ids = []
        
        spr.zone = zone_id
        current_ids.append(spr.id)
        current_flow += spr.flow_m3h
    
    # Add final zone
    if current_ids:
        zones.append({
            "id": zone_id,
            "sprinkler_ids": current_ids,
            "total_flow_m3h": current_flow
        })
    
    return sprinklers, zones


# ============================================================================
# Main Optimize Function
# ============================================================================

def optimize(polygon_data: Dict, settings_data: Dict, use_gpu: Optional[bool] = None) -> OptimizationResult:
    """
    Main optimization function.
    
    Args:
        polygon_data: {"vertices": [[x,y], ...], "area_m2": float, "perimeter_m": float}
        settings_data: {"pressure_bar": float, "max_flow_m3h": float, ...}
        use_gpu: Force GPU (True), force CPU (False), or auto-detect (None)
    
    Returns:
        OptimizationResult with sprinklers, zones, and metrics
    """
    start_time = time.time()
    
    # Auto-detect GPU if not specified
    if use_gpu is None:
        use_gpu = GPU_AVAILABLE
    
    device = "gpu" if (use_gpu and GPU_AVAILABLE) else "cpu"
    
    # Parse inputs
    vertices = np.array(polygon_data["vertices"])
    area_m2 = polygon_data["area_m2"]
    
    settings = Settings(
        pressure_bar=settings_data.get("pressure_bar", 3.0),
        max_flow_m3h=settings_data.get("max_flow_m3h", 2.0),
        target_cu=settings_data.get("target_cu", 90.0),
        target_du=settings_data.get("target_du", 85.0),
        spacing_factor=settings_data.get("spacing_factor", 0.55),
        preferred_brand=settings_data.get("preferred_brand", "any"),
        grid_resolution=settings_data.get("grid_resolution", 0.5),
    )
    
    # Calculate bounds
    min_x, min_y = vertices.min(axis=0)
    max_x, max_y = vertices.max(axis=0)
    bounds = (min_x, min_y, max_x, max_y)
    
    # Select nozzle
    nozzle = select_nozzle(area_m2, settings)
    
    # Place sprinklers
    sprinklers = place_sprinklers_grid(vertices, bounds, nozzle, settings)
    
    # Assign zones
    sprinklers, zones = assign_zones(sprinklers, settings.max_flow_m3h)
    
    # Generate simulation grid
    grid = generate_grid(bounds, settings.grid_resolution, vertices, use_gpu)
    
    # Simulate coverage
    precip = simulate_coverage(grid, sprinklers, use_gpu)
    
    # Calculate uniformity
    cu, du, coverage = calculate_uniformity(precip)
    
    # Total flow
    total_flow = sum(s.flow_m3h for s in sprinklers)
    
    computation_time = (time.time() - start_time) * 1000
    
    return OptimizationResult(
        sprinklers=sprinklers,
        zones=zones,
        cu=cu,
        du=du,
        coverage=coverage,
        total_flow_m3h=total_flow,
        computation_time_ms=computation_time,
        device=device,
    )


# ============================================================================
# CLI Interface
# ============================================================================

def main():
    import sys
    
    if len(sys.argv) < 3:
        print("Usage: python qirri_optimizer.py input.json output.json [--gpu|--cpu]")
        print("")
        print("Options:")
        print("  --gpu    Force GPU acceleration")
        print("  --cpu    Force CPU only")
        print("")
        print("Input JSON format:")
        print('  {"area": {"vertices": [[x,y]...], "area_m2": 100}, "settings": {...}}')
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    use_gpu = None
    if "--gpu" in sys.argv:
        use_gpu = True
    elif "--cpu" in sys.argv:
        use_gpu = False
    
    # Load input
    with open(input_file, 'r') as f:
        data = json.load(f)
    
    polygon_data = data.get("area", data.get("polygon", {}))
    settings_data = data.get("settings", {})
    
    print(f"Input: {input_file}")
    print(f"Area: {polygon_data.get('area_m2', 'unknown')} m²")
    print(f"Device: {'GPU' if (use_gpu or (use_gpu is None and GPU_AVAILABLE)) else 'CPU'}")
    print("Optimizing...")
    
    # Run optimization
    result = optimize(polygon_data, settings_data, use_gpu)
    
    # Prepare output
    output = {
        "version": "1.0",
        "sprinklers": [
            {
                "id": s.id,
                "x": s.x,
                "y": s.y,
                "brand": s.brand,
                "model": s.model,
                "nozzle": s.nozzle,
                "radius_m": s.radius_m,
                "arc": s.arc,
                "rotation": s.rotation,
                "flow_m3h": s.flow_m3h,
                "zone": s.zone,
            }
            for s in result.sprinklers
        ],
        "zones": result.zones,
        "results": {
            "cu": round(result.cu, 1),
            "du": round(result.du, 1),
            "coverage": round(result.coverage, 1),
            "total_flow_m3h": round(result.total_flow_m3h, 3),
            "computation_time_ms": round(result.computation_time_ms, 1),
            "device": result.device,
        }
    }
    
    # Save output
    with open(output_file, 'w') as f:
        json.dump(output, f, indent=2)
    
    print(f"\nResults:")
    print(f"  Sprinklers: {len(result.sprinklers)}")
    print(f"  Zones: {len(result.zones)}")
    print(f"  CU: {result.cu:.1f}%")
    print(f"  DU: {result.du:.1f}%")
    print(f"  Coverage: {result.coverage:.1f}%")
    print(f"  Total Flow: {result.total_flow_m3h:.3f} m³/h")
    print(f"  Time: {result.computation_time_ms:.1f}ms ({result.device})")
    print(f"\nOutput: {output_file}")


# ============================================================================
# FastAPI Server (optional)
# ============================================================================

try:
    from fastapi import FastAPI
    from fastapi.middleware.cors import CORSMiddleware
    from pydantic import BaseModel
    
    app = FastAPI(title="Qirri Optimizer API", version="1.0.0")
    
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_methods=["*"],
        allow_headers=["*"],
    )
    
    class OptimizeRequest(BaseModel):
        area: Dict
        settings: Dict = {}
        use_gpu: Optional[bool] = None
    
    @app.get("/health")
    def health():
        return {"status": "ok", "gpu_available": GPU_AVAILABLE}
    
    @app.post("/optimize")
    def api_optimize(req: OptimizeRequest):
        result = optimize(req.area, req.settings, req.use_gpu)
        return {
            "sprinklers": [
                {"id": s.id, "x": s.x, "y": s.y, "brand": s.brand, "model": s.model,
                 "nozzle": s.nozzle, "radius_m": s.radius_m, "arc": s.arc,
                 "rotation": s.rotation, "flow_m3h": s.flow_m3h, "zone": s.zone}
                for s in result.sprinklers
            ],
            "zones": result.zones,
            "results": {
                "cu": result.cu,
                "du": result.du,
                "coverage": result.coverage,
                "total_flow_m3h": result.total_flow_m3h,
                "computation_time_ms": result.computation_time_ms,
                "device": result.device,
            }
        }
    
except ImportError:
    app = None


if __name__ == "__main__":
    main()

