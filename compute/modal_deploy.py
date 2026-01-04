"""
Qirri Optimizer - Modal.com GPU Deployment
==========================================
Deploy to Modal.com for cloud GPU acceleration.

Setup:
    pip install modal
    modal token new
    modal deploy modal_deploy.py

Usage:
    # From Python
    from modal import Function
    optimize = Function.lookup("qirri", "optimize")
    result = optimize.remote(polygon_data, settings_data)
    
    # Or via web endpoint
    POST https://qirri--optimize.modal.run
    {"area": {...}, "settings": {...}}

Author: QTech Design (www.qtech.hr)
"""

import modal

# Create Modal app
app = modal.App("qirri")

# Define container image with GPU support
image = modal.Image.debian_slim(python_version="3.11").pip_install(
    "numpy>=1.24.0",
    "cupy-cuda12x",  # GPU acceleration
    "scipy>=1.11.0",
)

@app.function(
    image=image,
    gpu="T4",  # Options: "T4", "A10G", "A100"
    timeout=60,
    memory=1024,
)
def optimize(polygon_data: dict, settings_data: dict = None) -> dict:
    """
    GPU-accelerated irrigation optimization.
    
    Args:
        polygon_data: {"vertices": [[x,y], ...], "area_m2": float}
        settings_data: {"pressure_bar": float, ...}
    
    Returns:
        {"sprinklers": [...], "zones": [...], "results": {...}}
    """
    # Import here to ensure GPU context
    import time
    import numpy as np
    
    try:
        import cupy as cp
        gpu_available = True
    except ImportError:
        cp = None
        gpu_available = False
    
    start_time = time.time()
    
    if settings_data is None:
        settings_data = {}
    
    # Parse inputs
    vertices = np.array(polygon_data["vertices"])
    area_m2 = polygon_data.get("area_m2", 100)
    
    pressure_bar = settings_data.get("pressure_bar", 3.0)
    max_flow_m3h = settings_data.get("max_flow_m3h", 2.0)
    spacing_factor = settings_data.get("spacing_factor", 0.55)
    grid_resolution = settings_data.get("grid_resolution", 0.5)
    
    # Catalogue
    CATALOGUE = [
        {"brand": "RainBird", "model": "5000", "nozzle": "3.0", "radius_m": 5.0, "flow_m3h": 0.22, "precip_mmh": 14, "pressure_bar": 3.0},
        {"brand": "Hunter", "model": "PGP", "nozzle": "3.0", "radius_m": 5.2, "flow_m3h": 0.24, "precip_mmh": 14, "pressure_bar": 3.0},
        {"brand": "Hunter", "model": "MP", "nozzle": "2000", "radius_m": 4.5, "flow_m3h": 0.12, "precip_mmh": 10, "pressure_bar": 2.8},
    ]
    
    # Select nozzle
    target_radius = 3.0 if area_m2 < 50 else 4.0 if area_m2 < 200 else 5.0 if area_m2 < 500 else 6.0
    nozzle = min(CATALOGUE, key=lambda n: abs(n["radius_m"] - target_radius))
    
    # Calculate bounds
    min_x, min_y = vertices.min(axis=0)
    max_x, max_y = vertices.max(axis=0)
    
    # Place sprinklers on triangular grid
    spacing = nozzle["radius_m"] * 2 * spacing_factor
    margin = spacing / 2
    
    sprinklers = []
    spr_id = 1
    y = min_y + margin
    row = 0
    
    while y <= max_y - margin:
        offset = 0 if row % 2 == 0 else spacing / 2
        x = min_x + margin + offset
        
        while x <= max_x - margin:
            # Point in polygon (simple version)
            n = len(vertices)
            inside = False
            j = n - 1
            for i in range(n):
                xi, yi = vertices[i]
                xj, yj = vertices[j]
                if ((yi > y) != (yj > y)) and (x < (xj - xi) * (y - yi) / (yj - yi) + xi):
                    inside = not inside
                j = i
            
            if inside:
                sprinklers.append({
                    "id": f"spr-{spr_id}",
                    "x": float(x),
                    "y": float(y),
                    "brand": nozzle["brand"],
                    "model": nozzle["model"],
                    "nozzle": nozzle["nozzle"],
                    "radius_m": nozzle["radius_m"],
                    "arc": 360,
                    "rotation": 0,
                    "flow_m3h": nozzle["flow_m3h"],
                    "zone": 1,
                })
                spr_id += 1
            x += spacing
        
        y += spacing * 0.866
        row += 1
    
    # Generate grid for simulation (GPU accelerated)
    xs = np.arange(min_x, max_x + grid_resolution, grid_resolution)
    ys = np.arange(min_y, max_y + grid_resolution, grid_resolution)
    xx, yy = np.meshgrid(xs, ys)
    grid = np.column_stack([xx.ravel(), yy.ravel()])
    
    # GPU simulation if available
    if gpu_available:
        grid_gpu = cp.asarray(grid)
        precip = cp.zeros(len(grid_gpu))
        
        for spr in sprinklers:
            dx = grid_gpu[:, 0] - spr["x"]
            dy = grid_gpu[:, 1] - spr["y"]
            dist = cp.sqrt(dx * dx + dy * dy)
            in_range = dist <= spr["radius_m"]
            ratio = dist / spr["radius_m"]
            contrib = spr["flow_m3h"] * 1000 * (1 - ratio ** 2)
            contrib = cp.maximum(contrib, 0)
            precip = cp.where(in_range, precip + contrib, precip)
        
        precip = cp.asnumpy(precip)
    else:
        precip = np.zeros(len(grid))
        for spr in sprinklers:
            dx = grid[:, 0] - spr["x"]
            dy = grid[:, 1] - spr["y"]
            dist = np.sqrt(dx * dx + dy * dy)
            in_range = dist <= spr["radius_m"]
            ratio = dist / spr["radius_m"]
            contrib = spr["flow_m3h"] * 1000 * (1 - ratio ** 2)
            contrib = np.maximum(contrib, 0)
            precip = np.where(in_range, precip + contrib, precip)
    
    # Calculate uniformity
    non_zero = precip[precip > 0]
    if len(non_zero) > 0:
        avg = np.mean(non_zero)
        deviations = np.sum(np.abs(non_zero - avg))
        cu = 100 * (1 - deviations / (len(non_zero) * avg))
        sorted_vals = np.sort(non_zero)
        lq_count = max(1, len(non_zero) // 4)
        lq_avg = np.mean(sorted_vals[:lq_count])
        du = 100 * (lq_avg / avg)
        coverage = 100 * (len(non_zero) / len(precip))
    else:
        cu, du, coverage = 0, 0, 0
    
    # Assign zones
    zones = []
    zone_id = 1
    current_flow = 0
    current_ids = []
    
    for spr in sprinklers:
        if current_flow + spr["flow_m3h"] > max_flow_m3h and current_ids:
            zones.append({"id": zone_id, "sprinkler_ids": current_ids.copy(), "total_flow_m3h": current_flow})
            zone_id += 1
            current_flow = 0
            current_ids = []
        spr["zone"] = zone_id
        current_ids.append(spr["id"])
        current_flow += spr["flow_m3h"]
    
    if current_ids:
        zones.append({"id": zone_id, "sprinkler_ids": current_ids, "total_flow_m3h": current_flow})
    
    computation_time = (time.time() - start_time) * 1000
    
    return {
        "sprinklers": sprinklers,
        "zones": zones,
        "results": {
            "cu": float(np.clip(cu, 0, 100)),
            "du": float(np.clip(du, 0, 100)),
            "coverage": float(coverage),
            "total_flow_m3h": sum(s["flow_m3h"] for s in sprinklers),
            "computation_time_ms": computation_time,
            "device": "gpu" if gpu_available else "cpu",
        }
    }


@app.function(image=image)
@modal.web_endpoint(method="POST")
def optimize_endpoint(data: dict) -> dict:
    """Web endpoint for optimization API"""
    polygon_data = data.get("area", data.get("polygon", {}))
    settings_data = data.get("settings", {})
    return optimize.remote(polygon_data, settings_data)


@app.local_entrypoint()
def main():
    """Test the optimization locally"""
    test_polygon = {
        "vertices": [[0, 0], [10, 0], [10, 8], [5, 8], [5, 5], [0, 5]],
        "area_m2": 65,
    }
    test_settings = {
        "pressure_bar": 3.0,
        "max_flow_m3h": 2.0,
    }
    
    print("Testing Qirri optimizer on Modal.com GPU...")
    result = optimize.remote(test_polygon, test_settings)
    
    print(f"\nResults:")
    print(f"  Sprinklers: {len(result['sprinklers'])}")
    print(f"  Zones: {len(result['zones'])}")
    print(f"  CU: {result['results']['cu']:.1f}%")
    print(f"  DU: {result['results']['du']:.1f}%")
    print(f"  Time: {result['results']['computation_time_ms']:.1f}ms ({result['results']['device']})")

