'use client'

import { useState, useCallback } from 'react'
import { Canvas } from '@/components/Canvas'
import { Sidebar } from '@/components/Sidebar'
import { Header } from '@/components/Header'
import { ImportExport } from '@/components/ImportExport'
import type { Project, Sprinkler, OptimizationResult } from '@/types/irrigation'
import { optimize } from '@/lib/optimizer'

const DEFAULT_SETTINGS = {
  pressure_bar: 3.0,
  max_flow_m3h: 2.0,
  target_cu: 90,
  target_du: 85,
  target_coverage: 95,
  preferred_brand: 'any',
  soil_infiltration_mmh: 25,
  spacing_factor: 0.55,
}

export default function Home() {
  const [project, setProject] = useState<Partial<Project>>({
    name: 'New Project',
    settings: DEFAULT_SETTINGS,
    sprinklers: [],
    zones: [],
    units: 'mm',
  })
  
  const [results, setResults] = useState<OptimizationResult | null>(null)
  const [isOptimizing, setIsOptimizing] = useState(false)
  const [showImport, setShowImport] = useState(false)
  
  const handleImport = useCallback((data: any) => {
    // Convert AutoCAD export format to project
    const vertices = data.area.vertices.map((v: [number, number]) => ({
      x: v[0],
      y: v[1]
    }))
    
    setProject(prev => ({
      ...prev,
      area: {
        vertices,
        area_m2: data.area.area_m2,
        perimeter_m: data.area.perimeter_m,
      },
      settings: { ...DEFAULT_SETTINGS, ...data.settings },
      units: data.units || 'mm',
      sprinklers: [],
      zones: [],
    }))
    
    setResults(null)
    setShowImport(false)
  }, [])
  
  const handleOptimize = useCallback(async () => {
    if (!project.area) {
      alert('Please import an area first')
      return
    }
    
    setIsOptimizing(true)
    
    try {
      // Run optimization (local for now, can switch to API)
      const result = optimize({
        polygon: project.area,
        settings: project.settings!,
      })
      
      setProject(prev => ({
        ...prev,
        sprinklers: result.sprinklers,
        zones: result.zones,
      }))
      
      setResults(result.results)
    } catch (error: any) {
      alert('Optimization failed: ' + error.message)
    } finally {
      setIsOptimizing(false)
    }
  }, [project.area, project.settings])
  
  const handleExport = useCallback(() => {
    if (!project.sprinklers?.length) {
      alert('No sprinklers to export')
      return
    }
    
    const exportData = {
      version: '1.0',
      sprinklers: project.sprinklers.map(s => ({
        x: s.position.x,
        y: s.position.y,
        brand: s.brand,
        model: s.model,
        nozzle: s.nozzle,
        radius_m: s.radius_m,
        arc: s.arc,
        rotation: s.rotation,
        flow_m3h: s.flow_m3h,
      })),
      zones: project.zones?.map(z => ({
        id: z.id,
        sprinkler_indices: z.sprinkler_ids.map(id => 
          project.sprinklers!.findIndex(s => s.id === id)
        ),
      })),
      results: results ? {
        cu: results.cu,
        du: results.du,
        coverage: results.coverage,
        total_flow_m3h: results.total_flow_m3h,
      } : null,
    }
    
    const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `qirri-export-${Date.now()}.json`
    a.click()
    URL.revokeObjectURL(url)
  }, [project.sprinklers, project.zones, results])
  
  return (
    <main className="h-screen flex flex-col">
      <Header 
        projectName={project.name || 'New Project'}
        onImport={() => setShowImport(true)}
        onExport={handleExport}
      />
      
      <div className="flex-1 flex overflow-hidden">
        {/* Canvas Area */}
        <div className="flex-1 p-4">
          <Canvas 
            area={project.area}
            sprinklers={project.sprinklers || []}
            units={project.units || 'mm'}
          />
        </div>
        
        {/* Sidebar */}
        <Sidebar 
          settings={project.settings || DEFAULT_SETTINGS}
          results={results}
          sprinklerCount={project.sprinklers?.length || 0}
          zoneCount={project.zones?.length || 0}
          isOptimizing={isOptimizing}
          onOptimize={handleOptimize}
          onSettingsChange={(settings) => setProject(prev => ({ ...prev, settings }))}
        />
      </div>
      
      {/* Import Modal */}
      {showImport && (
        <ImportExport 
          onImport={handleImport}
          onClose={() => setShowImport(false)}
        />
      )}
    </main>
  )
}

