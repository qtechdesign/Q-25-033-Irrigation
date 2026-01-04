import type { Point, Polygon, Sprinkler, ProjectSettings, CatalogueEntry } from '@/types/irrigation'
import { getBounds, pointInPolygon } from './grid'

/**
 * Place sprinklers using triangular grid pattern
 */
export function placeSprinklersGrid(
  polygon: Polygon,
  nozzle: CatalogueEntry,
  settings: ProjectSettings
): Sprinkler[] {
  const vertices = polygon.vertices
  const bounds = getBounds(vertices)
  
  // Calculate spacing (55% of diameter by default)
  const spacing = nozzle.radius_m * 2 * settings.spacing_factor
  
  const sprinklers: Sprinkler[] = []
  let id = 1
  
  // Add margin from edges
  const margin = spacing / 2
  const minX = bounds.min.x + margin
  const maxX = bounds.max.x - margin
  const minY = bounds.min.y + margin
  const maxY = bounds.max.y - margin
  
  // Triangular grid pattern
  let row = 0
  let y = minY
  
  while (y <= maxY) {
    // Offset every other row
    const offset = (row % 2 === 0) ? 0 : spacing / 2
    let x = minX + offset
    
    while (x <= maxX) {
      const pt: Point = { x, y }
      
      if (pointInPolygon(pt, vertices)) {
        sprinklers.push({
          id: `spr-${id++}`,
          position: pt,
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
    
    // Row spacing for equilateral triangle pattern (sin 60° ≈ 0.866)
    y += spacing * 0.866
    row++
  }
  
  return sprinklers
}

/**
 * Select best nozzle from catalogue for given area
 */
export function selectBestNozzle(
  area_m2: number,
  catalogue: CatalogueEntry[],
  settings: ProjectSettings
): CatalogueEntry | null {
  // Target radius based on area size
  let targetRadius: number
  if (area_m2 < 50) targetRadius = 3
  else if (area_m2 < 200) targetRadius = 4
  else if (area_m2 < 500) targetRadius = 5
  else targetRadius = 6
  
  // Filter by pressure
  const compatible = catalogue.filter(n => 
    Math.abs(n.pressure_bar - settings.pressure_bar) < 1.0
  )
  
  if (compatible.length === 0) return catalogue[0] || null
  
  // Filter by brand preference
  let preferred = compatible
  if (settings.preferred_brand && settings.preferred_brand !== 'any') {
    const brandMatch = compatible.filter(n => 
      n.brand.toLowerCase() === settings.preferred_brand.toLowerCase()
    )
    if (brandMatch.length > 0) preferred = brandMatch
  }
  
  // Find closest to target radius
  let best = preferred[0]
  let bestDiff = Math.abs(best.radius_m - targetRadius)
  
  for (const n of preferred) {
    const diff = Math.abs(n.radius_m - targetRadius)
    if (diff < bestDiff) {
      best = n
      bestDiff = diff
    }
  }
  
  return best
}

/**
 * Auto-assign zones based on flow limits
 */
export function assignZones(
  sprinklers: Sprinkler[],
  maxFlowPerZone: number
): { sprinklers: Sprinkler[]; zones: { id: number; sprinkler_ids: string[]; total_flow_m3h: number }[] } {
  const zones: { id: number; sprinkler_ids: string[]; total_flow_m3h: number }[] = []
  const updated = [...sprinklers]
  
  let currentZone = 1
  let currentFlow = 0
  let currentIds: string[] = []
  
  for (const spr of updated) {
    if (currentFlow + spr.flow_m3h > maxFlowPerZone && currentIds.length > 0) {
      // Start new zone
      zones.push({
        id: currentZone,
        sprinkler_ids: [...currentIds],
        total_flow_m3h: currentFlow
      })
      currentZone++
      currentFlow = 0
      currentIds = []
    }
    
    spr.zone = currentZone
    currentIds.push(spr.id)
    currentFlow += spr.flow_m3h
  }
  
  // Add final zone
  if (currentIds.length > 0) {
    zones.push({
      id: currentZone,
      sprinkler_ids: currentIds,
      total_flow_m3h: currentFlow
    })
  }
  
  return { sprinklers: updated, zones }
}

