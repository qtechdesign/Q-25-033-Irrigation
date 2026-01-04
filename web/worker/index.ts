/**
 * Qirri API - Cloudflare Worker
 * Irrigation optimization endpoint
 */

import type { 
  OptimizeRequest, 
  OptimizeResponse,
  Point,
  Polygon,
  Sprinkler,
  ProjectSettings,
  CatalogueEntry
} from '../src/types/irrigation'

// CORS headers
const corsHeaders = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type, Authorization',
}

// Default catalogue
const CATALOGUE: CatalogueEntry[] = [
  { brand: 'RainBird', model: '5000', nozzle: '2.0', radius_m: 4.0, flow_m3h: 0.15, precip_mmh: 12, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'RainBird', model: '5000', nozzle: '3.0', radius_m: 5.0, flow_m3h: 0.22, precip_mmh: 14, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'RainBird', model: '5000', nozzle: '4.0', radius_m: 6.0, flow_m3h: 0.30, precip_mmh: 15, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'PGP', nozzle: '3.0', radius_m: 5.2, flow_m3h: 0.24, precip_mmh: 14, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'MP', nozzle: '2000', radius_m: 4.5, flow_m3h: 0.12, precip_mmh: 10, pressure_bar: 2.8, arc_options: [90, 180, 270, 360] },
]

export default {
  async fetch(request: Request, env: any): Promise<Response> {
    // Handle CORS preflight
    if (request.method === 'OPTIONS') {
      return new Response(null, { headers: corsHeaders })
    }

    const url = new URL(request.url)

    try {
      // Health check
      if (url.pathname === '/health' || url.pathname === '/') {
        return json({ status: 'ok', service: 'qirri-api', version: '1.0.0' })
      }

      // Get catalogue
      if (url.pathname === '/api/catalogue') {
        return json({ catalogue: CATALOGUE })
      }

      // Optimize endpoint
      if (url.pathname === '/api/optimize' && request.method === 'POST') {
        const body = await request.json() as OptimizeRequest
        const result = optimize(body)
        return json(result)
      }

      // 404
      return json({ error: 'Not found' }, 404)

    } catch (error: any) {
      console.error('Error:', error)
      return json({ error: error.message || 'Internal error' }, 500)
    }
  }
}

function json(data: any, status = 200): Response {
  return new Response(JSON.stringify(data), {
    status,
    headers: {
      'Content-Type': 'application/json',
      ...corsHeaders
    }
  })
}

// ============================================================================
// Optimization Engine (inline for Worker)
// ============================================================================

function optimize(req: OptimizeRequest): OptimizeResponse {
  const startTime = Date.now()
  
  try {
    const { area, settings } = req
    
    // Select nozzle
    const nozzle = selectNozzle(area.area_m2, settings)
    
    // Place sprinklers
    const sprinklers = placeSprinklers(area, nozzle, settings)
    
    // Calculate uniformity
    const grid = generateGrid(area, 1.0)
    const precip = simulateCoverage(grid, sprinklers)
    const uniformity = calculateUniformity(precip)
    
    // Assign zones
    const zones = assignZones(sprinklers, settings.max_flow_m3h)
    
    return {
      success: true,
      sprinklers,
      zones,
      results: {
        cu: uniformity.cu,
        du: uniformity.du,
        coverage: uniformity.coverage,
        total_flow_m3h: sprinklers.reduce((s, h) => s + h.flow_m3h, 0),
        computation_time_ms: Date.now() - startTime,
        algorithm: 'grid-v1'
      }
    }
  } catch (error: any) {
    return {
      success: false,
      sprinklers: [],
      zones: [],
      results: {
        cu: 0, du: 0, coverage: 0, total_flow_m3h: 0,
        computation_time_ms: Date.now() - startTime,
        algorithm: 'error'
      },
      error: error.message
    }
  }
}

function selectNozzle(area_m2: number, settings: ProjectSettings): CatalogueEntry {
  const target = area_m2 < 50 ? 3 : area_m2 < 200 ? 4 : area_m2 < 500 ? 5 : 6
  
  let best = CATALOGUE[0]
  let bestDiff = Math.abs(best.radius_m - target)
  
  for (const n of CATALOGUE) {
    if (Math.abs(n.pressure_bar - settings.pressure_bar) > 1) continue
    const diff = Math.abs(n.radius_m - target)
    if (diff < bestDiff) {
      best = n
      bestDiff = diff
    }
  }
  
  return best
}

function placeSprinklers(area: Polygon, nozzle: CatalogueEntry, settings: ProjectSettings): Sprinkler[] {
  const vertices = area.vertices
  const bounds = getBounds(vertices)
  const spacing = nozzle.radius_m * 2 * settings.spacing_factor
  
  const sprinklers: Sprinkler[] = []
  let id = 1
  
  const margin = spacing / 2
  let row = 0
  let y = bounds.min.y + margin
  
  while (y <= bounds.max.y - margin) {
    const offset = (row % 2 === 0) ? 0 : spacing / 2
    let x = bounds.min.x + margin + offset
    
    while (x <= bounds.max.x - margin) {
      if (pointInPolygon({ x, y }, vertices)) {
        sprinklers.push({
          id: `spr-${id++}`,
          position: { x, y },
          brand: nozzle.brand,
          model: nozzle.model,
          nozzle: nozzle.nozzle,
          radius_m: nozzle.radius_m,
          arc: 360,
          rotation: 0,
          flow_m3h: nozzle.flow_m3h,
          precip_mmh: nozzle.precip_mmh,
          pressure_bar: nozzle.pressure_bar,
        })
      }
      x += spacing
    }
    y += spacing * 0.866
    row++
  }
  
  return sprinklers
}

function generateGrid(area: Polygon, resolution: number): Point[] {
  const vertices = area.vertices
  const bounds = getBounds(vertices)
  const points: Point[] = []
  
  const numX = Math.min(100, Math.ceil((bounds.max.x - bounds.min.x) / resolution))
  const numY = Math.min(100, Math.ceil((bounds.max.y - bounds.min.y) / resolution))
  const stepX = (bounds.max.x - bounds.min.x) / numX
  const stepY = (bounds.max.y - bounds.min.y) / numY
  
  for (let iy = 0; iy <= numY; iy++) {
    for (let ix = 0; ix <= numX; ix++) {
      const pt = { x: bounds.min.x + ix * stepX, y: bounds.min.y + iy * stepY }
      if (pointInPolygon(pt, vertices)) points.push(pt)
    }
  }
  
  return points
}

function simulateCoverage(grid: Point[], sprinklers: Sprinkler[]): number[] {
  return grid.map(pt => {
    let total = 0
    for (const s of sprinklers) {
      const dx = pt.x - s.position.x
      const dy = pt.y - s.position.y
      const dist = Math.sqrt(dx * dx + dy * dy)
      if (dist <= s.radius_m) {
        total += s.precip_mmh * (1 - (dist / s.radius_m) ** 2)
      }
    }
    return total
  })
}

function calculateUniformity(precip: number[]): { cu: number; du: number; coverage: number } {
  const nonZero = precip.filter(p => p > 0)
  if (nonZero.length === 0) return { cu: 0, du: 0, coverage: 0 }
  
  const n = nonZero.length
  const avg = nonZero.reduce((a, b) => a + b, 0) / n
  const dev = nonZero.reduce((s, p) => s + Math.abs(p - avg), 0)
  const cu = 100 * (1 - dev / (n * avg))
  
  const sorted = [...nonZero].sort((a, b) => a - b)
  const lqCount = Math.max(1, Math.floor(n / 4))
  const lqAvg = sorted.slice(0, lqCount).reduce((a, b) => a + b, 0) / lqCount
  const du = 100 * (lqAvg / avg)
  
  const coverage = 100 * (n / precip.length)
  
  return {
    cu: Math.min(100, Math.max(0, cu)),
    du: Math.min(100, Math.max(0, du)),
    coverage
  }
}

function assignZones(sprinklers: Sprinkler[], maxFlow: number) {
  const zones: { id: number; sprinkler_ids: string[]; total_flow_m3h: number }[] = []
  let zoneId = 1, flow = 0, ids: string[] = []
  
  for (const s of sprinklers) {
    if (flow + s.flow_m3h > maxFlow && ids.length > 0) {
      zones.push({ id: zoneId++, sprinkler_ids: [...ids], total_flow_m3h: flow })
      flow = 0
      ids = []
    }
    s.zone = zoneId
    ids.push(s.id)
    flow += s.flow_m3h
  }
  if (ids.length > 0) {
    zones.push({ id: zoneId, sprinkler_ids: ids, total_flow_m3h: flow })
  }
  
  return zones
}

function getBounds(vertices: Point[]): { min: Point; max: Point } {
  let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity
  for (const v of vertices) {
    if (v.x < minX) minX = v.x
    if (v.x > maxX) maxX = v.x
    if (v.y < minY) minY = v.y
    if (v.y > maxY) maxY = v.y
  }
  return { min: { x: minX, y: minY }, max: { x: maxX, y: maxY } }
}

function pointInPolygon(pt: Point, vertices: Point[]): boolean {
  let inside = false
  const n = vertices.length
  for (let i = 0, j = n - 1; i < n; j = i++) {
    const xi = vertices[i].x, yi = vertices[i].y
    const xj = vertices[j].x, yj = vertices[j].y
    if (((yi > pt.y) !== (yj > pt.y)) && (pt.x < (xj - xi) * (pt.y - yi) / (yj - yi) + xi)) {
      inside = !inside
    }
  }
  return inside
}

