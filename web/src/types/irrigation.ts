// Qirri Irrigation Types
// Shared between AutoCAD LISP and Web App

export interface Point {
  x: number
  y: number
}

export interface Polygon {
  vertices: Point[]
  area_m2: number
  perimeter_m: number
}

export interface Obstacle {
  type: 'circle' | 'polygon'
  center?: Point
  radius?: number
  vertices?: Point[]
}

export interface ProjectSettings {
  pressure_bar: number
  max_flow_m3h: number
  target_cu: number
  target_du: number
  target_coverage: number
  preferred_brand: string
  soil_infiltration_mmh: number
  spacing_factor: number
}

export interface Sprinkler {
  id: string
  position: Point
  brand: string
  model: string
  nozzle: string
  radius_m: number
  arc: 90 | 180 | 270 | 360
  rotation: number
  flow_m3h: number
  precip_mmh: number
  pressure_bar: number
  zone?: number
}

export interface Zone {
  id: number
  sprinkler_ids: string[]
  total_flow_m3h: number
  color: string
}

export interface OptimizationResult {
  cu: number
  du: number
  coverage: number
  total_flow_m3h: number
  computation_time_ms: number
  algorithm: string
  generations?: number
}

export interface Project {
  id: string
  name: string
  created_at: string
  updated_at: string
  user_id: string
  area: Polygon
  obstacles: Obstacle[]
  settings: ProjectSettings
  sprinklers: Sprinkler[]
  zones: Zone[]
  results?: OptimizationResult
  units: 'mm' | 'cm' | 'm' | 'in' | 'ft'
}

// API Request/Response types

export interface OptimizeRequest {
  area: Polygon
  obstacles: Obstacle[]
  settings: ProjectSettings
  units: string
}

export interface OptimizeResponse {
  success: boolean
  sprinklers: Sprinkler[]
  zones: Zone[]
  results: OptimizationResult
  error?: string
}

// AutoCAD Exchange Format (JSON)

export interface AutoCADExport {
  version: string
  exported_at: string
  area: {
    vertices: [number, number][]
    area_m2: number
    perimeter_m: number
  }
  obstacles: {
    type: string
    data: any
  }[]
  settings: ProjectSettings
  units: string
}

export interface AutoCADImport {
  version: string
  sprinklers: {
    x: number
    y: number
    brand: string
    model: string
    nozzle: string
    radius_m: number
    arc: number
    rotation: number
    flow_m3h: number
  }[]
  zones: {
    id: number
    sprinkler_indices: number[]
  }[]
  results: {
    cu: number
    du: number
    coverage: number
    total_flow_m3h: number
  }
}

// Sprinkler Catalogue

export interface CatalogueEntry {
  brand: string
  model: string
  nozzle: string
  radius_m: number
  flow_m3h: number
  precip_mmh: number
  pressure_bar: number
  arc_options: number[]
}

