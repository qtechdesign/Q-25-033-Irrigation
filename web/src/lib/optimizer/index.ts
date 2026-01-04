import type { 
  Polygon, 
  Sprinkler, 
  ProjectSettings, 
  OptimizationResult,
  CatalogueEntry 
} from '@/types/irrigation'
import { generateGrid, simulateCoverage, calculateUniformity } from './grid'
import { placeSprinklersGrid, selectBestNozzle, assignZones } from './placer'

// Default catalogue (subset)
export const DEFAULT_CATALOGUE: CatalogueEntry[] = [
  { brand: 'RainBird', model: '5000', nozzle: '2.0', radius_m: 4.0, flow_m3h: 0.15, precip_mmh: 12, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'RainBird', model: '5000', nozzle: '3.0', radius_m: 5.0, flow_m3h: 0.22, precip_mmh: 14, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'RainBird', model: '5000', nozzle: '4.0', radius_m: 6.0, flow_m3h: 0.30, precip_mmh: 15, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'PGP', nozzle: '2.0', radius_m: 4.2, flow_m3h: 0.16, precip_mmh: 12, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'PGP', nozzle: '3.0', radius_m: 5.2, flow_m3h: 0.24, precip_mmh: 14, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'PGP', nozzle: '4.0', radius_m: 6.3, flow_m3h: 0.32, precip_mmh: 15, pressure_bar: 3.0, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'MP', nozzle: '1000', radius_m: 3.0, flow_m3h: 0.08, precip_mmh: 10, pressure_bar: 2.8, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'MP', nozzle: '2000', radius_m: 4.5, flow_m3h: 0.12, precip_mmh: 10, pressure_bar: 2.8, arc_options: [90, 180, 270, 360] },
  { brand: 'Hunter', model: 'MP', nozzle: '3000', radius_m: 6.0, flow_m3h: 0.18, precip_mmh: 10, pressure_bar: 2.8, arc_options: [90, 180, 270, 360] },
]

export interface OptimizeOptions {
  polygon: Polygon
  settings: ProjectSettings
  catalogue?: CatalogueEntry[]
}

export interface OptimizeResult {
  sprinklers: Sprinkler[]
  zones: { id: number; sprinkler_ids: string[]; total_flow_m3h: number }[]
  results: OptimizationResult
}

/**
 * Main optimization function
 */
export function optimize(options: OptimizeOptions): OptimizeResult {
  const startTime = performance.now()
  
  const { polygon, settings } = options
  const catalogue = options.catalogue || DEFAULT_CATALOGUE
  
  // 1. Select best nozzle
  const nozzle = selectBestNozzle(polygon.area_m2, catalogue, settings)
  if (!nozzle) {
    throw new Error('No suitable nozzle found in catalogue')
  }
  
  // 2. Place sprinklers on grid
  let sprinklers = placeSprinklersGrid(polygon, nozzle, settings)
  
  // 3. Generate simulation grid (1m resolution)
  const grid = generateGrid(polygon, 1.0, 10000)
  
  // 4. Simulate coverage
  const precip = simulateCoverage(grid, sprinklers)
  
  // 5. Calculate uniformity
  const uniformity = calculateUniformity(precip)
  
  // 6. Assign zones
  const zoned = assignZones(sprinklers, settings.max_flow_m3h)
  
  const endTime = performance.now()
  
  // 7. Build results
  const results: OptimizationResult = {
    cu: uniformity.cu,
    du: uniformity.du,
    coverage: uniformity.coverage,
    total_flow_m3h: sprinklers.reduce((sum, s) => sum + s.flow_m3h, 0),
    computation_time_ms: Math.round(endTime - startTime),
    algorithm: 'grid-placement-v1'
  }
  
  return {
    sprinklers: zoned.sprinklers,
    zones: zoned.zones,
    results
  }
}

/**
 * Re-calculate uniformity for existing sprinklers
 */
export function validate(
  polygon: Polygon,
  sprinklers: Sprinkler[]
): OptimizationResult {
  const startTime = performance.now()
  
  const grid = generateGrid(polygon, 1.0, 10000)
  const precip = simulateCoverage(grid, sprinklers)
  const uniformity = calculateUniformity(precip)
  
  const endTime = performance.now()
  
  return {
    cu: uniformity.cu,
    du: uniformity.du,
    coverage: uniformity.coverage,
    total_flow_m3h: sprinklers.reduce((sum, s) => sum + s.flow_m3h, 0),
    computation_time_ms: Math.round(endTime - startTime),
    algorithm: 'validation'
  }
}

