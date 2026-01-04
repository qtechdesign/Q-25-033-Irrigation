import type { Point, Polygon, Sprinkler } from '@/types/irrigation'

/**
 * Generate simulation grid points inside polygon
 */
export function generateGrid(
  polygon: Polygon,
  resolution: number,
  maxPoints: number = 50000
): Point[] {
  const vertices = polygon.vertices
  const bounds = getBounds(vertices)
  
  // Calculate grid dimensions
  const width = bounds.max.x - bounds.min.x
  const height = bounds.max.y - bounds.min.y
  let numX = Math.ceil(width / resolution)
  let numY = Math.ceil(height / resolution)
  
  // Adjust resolution if too many points
  while (numX * numY > maxPoints) {
    resolution *= 1.5
    numX = Math.ceil(width / resolution)
    numY = Math.ceil(height / resolution)
  }
  
  const points: Point[] = []
  
  for (let iy = 0; iy <= numY; iy++) {
    const y = bounds.min.y + iy * resolution
    for (let ix = 0; ix <= numX; ix++) {
      const x = bounds.min.x + ix * resolution
      const pt = { x, y }
      if (pointInPolygon(pt, vertices)) {
        points.push(pt)
      }
    }
  }
  
  return points
}

/**
 * Get bounding box of polygon
 */
export function getBounds(vertices: Point[]): { min: Point; max: Point } {
  let minX = Infinity, minY = Infinity
  let maxX = -Infinity, maxY = -Infinity
  
  for (const v of vertices) {
    if (v.x < minX) minX = v.x
    if (v.x > maxX) maxX = v.x
    if (v.y < minY) minY = v.y
    if (v.y > maxY) maxY = v.y
  }
  
  return {
    min: { x: minX, y: minY },
    max: { x: maxX, y: maxY }
  }
}

/**
 * Ray casting point-in-polygon test
 */
export function pointInPolygon(point: Point, vertices: Point[]): boolean {
  const { x, y } = point
  const n = vertices.length
  let inside = false
  
  for (let i = 0, j = n - 1; i < n; j = i++) {
    const xi = vertices[i].x, yi = vertices[i].y
    const xj = vertices[j].x, yj = vertices[j].y
    
    if (((yi > y) !== (yj > y)) && 
        (x < (xj - xi) * (y - yi) / (yj - yi) + xi)) {
      inside = !inside
    }
  }
  
  return inside
}

/**
 * Calculate distance between two points
 */
export function distance(p1: Point, p2: Point): number {
  const dx = p2.x - p1.x
  const dy = p2.y - p1.y
  return Math.sqrt(dx * dx + dy * dy)
}

/**
 * Calculate precipitation at a grid point from all sprinklers
 */
export function calculatePrecipitation(
  point: Point,
  sprinklers: Sprinkler[]
): number {
  let total = 0
  
  for (const spr of sprinklers) {
    const dist = distance(point, spr.position)
    
    if (dist <= spr.radius_m && isInArc(point, spr)) {
      // Quadratic decay model
      const ratio = dist / spr.radius_m
      const contrib = spr.precip_mmh * (1 - ratio * ratio)
      total += contrib
    }
  }
  
  return total
}

/**
 * Check if point is within sprinkler arc
 */
function isInArc(point: Point, sprinkler: Sprinkler): boolean {
  if (sprinkler.arc === 360) return true
  
  const dx = point.x - sprinkler.position.x
  const dy = point.y - sprinkler.position.y
  let angle = Math.atan2(dy, dx) * 180 / Math.PI
  if (angle < 0) angle += 360
  
  const halfArc = sprinkler.arc / 2
  const startAngle = (sprinkler.rotation - halfArc + 360) % 360
  const endAngle = (sprinkler.rotation + halfArc) % 360
  
  if (startAngle <= endAngle) {
    return angle >= startAngle && angle <= endAngle
  } else {
    return angle >= startAngle || angle <= endAngle
  }
}

/**
 * Simulate coverage on grid
 */
export function simulateCoverage(
  grid: Point[],
  sprinklers: Sprinkler[]
): number[] {
  return grid.map(pt => calculatePrecipitation(pt, sprinklers))
}

/**
 * Calculate uniformity metrics
 */
export function calculateUniformity(precipValues: number[]): {
  cu: number
  du: number
  coverage: number
  avgPrecip: number
  minPrecip: number
  maxPrecip: number
} {
  // Filter non-zero values
  const nonZero = precipValues.filter(p => p > 0)
  
  if (nonZero.length === 0) {
    return { cu: 0, du: 0, coverage: 0, avgPrecip: 0, minPrecip: 0, maxPrecip: 0 }
  }
  
  const n = nonZero.length
  const total = nonZero.reduce((a, b) => a + b, 0)
  const avg = total / n
  
  // Christiansen's Uniformity (CU)
  const deviations = nonZero.reduce((sum, p) => sum + Math.abs(p - avg), 0)
  const cu = 100 * (1 - deviations / (n * avg))
  
  // Distribution Uniformity (DU) - low quarter
  const sorted = [...nonZero].sort((a, b) => a - b)
  const lqCount = Math.max(1, Math.floor(n / 4))
  const lqSum = sorted.slice(0, lqCount).reduce((a, b) => a + b, 0)
  const lqAvg = lqSum / lqCount
  const du = 100 * (lqAvg / avg)
  
  // Coverage
  const coverage = 100 * (nonZero.length / precipValues.length)
  
  return {
    cu: Math.min(100, Math.max(0, cu)),
    du: Math.min(100, Math.max(0, du)),
    coverage,
    avgPrecip: avg,
    minPrecip: sorted[0],
    maxPrecip: sorted[sorted.length - 1]
  }
}

