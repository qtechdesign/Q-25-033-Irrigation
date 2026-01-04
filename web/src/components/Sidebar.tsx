'use client'

import type { ProjectSettings, OptimizationResult } from '@/types/irrigation'

interface SidebarProps {
  settings: ProjectSettings
  results: OptimizationResult | null
  sprinklerCount: number
  zoneCount: number
  isOptimizing: boolean
  onOptimize: () => void
  onSettingsChange: (settings: ProjectSettings) => void
}

export function Sidebar({ 
  settings, 
  results, 
  sprinklerCount,
  zoneCount,
  isOptimizing, 
  onOptimize,
  onSettingsChange 
}: SidebarProps) {
  
  const updateSetting = (key: keyof ProjectSettings, value: any) => {
    onSettingsChange({ ...settings, [key]: value })
  }
  
  const getResultClass = (value: number, target: number) => {
    if (value >= target) return 'stat-good'
    if (value >= target - 5) return 'stat-warning'
    return 'stat-bad'
  }
  
  return (
    <aside className="w-80 bg-gray-900 border-l border-gray-800 p-4 overflow-y-auto">
      {/* Optimize Button */}
      <button 
        onClick={onOptimize}
        disabled={isOptimizing}
        className="w-full btn-primary mb-4 py-3 text-lg flex items-center justify-center gap-2"
      >
        {isOptimizing ? (
          <>
            <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
              <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
              <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
            </svg>
            Optimizing...
          </>
        ) : (
          <>
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 10V3L4 14h7v7l9-11h-7z" />
            </svg>
            Optimize Layout
          </>
        )}
      </button>
      
      {/* Results */}
      {results && (
        <div className="card mb-4">
          <h3 className="text-sm font-medium text-gray-400 mb-3">Results</h3>
          <div className="space-y-2">
            <div className="flex justify-between">
              <span className="text-gray-500">Uniformity (CU)</span>
              <span className={getResultClass(results.cu, settings.target_cu)}>
                {results.cu.toFixed(1)}%
              </span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-500">Distribution (DU)</span>
              <span className={getResultClass(results.du, settings.target_du)}>
                {results.du.toFixed(1)}%
              </span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-500">Coverage</span>
              <span className={getResultClass(results.coverage, settings.target_coverage)}>
                {results.coverage.toFixed(1)}%
              </span>
            </div>
            <div className="border-t border-gray-700 pt-2 mt-2">
              <div className="flex justify-between">
                <span className="text-gray-500">Sprinklers</span>
                <span className="text-white">{sprinklerCount}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-500">Zones</span>
                <span className="text-white">{zoneCount}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-500">Total Flow</span>
                <span className="text-white">{results.total_flow_m3h.toFixed(2)} m³/h</span>
              </div>
            </div>
            <div className="text-xs text-gray-600 mt-2">
              Computed in {results.computation_time_ms}ms
            </div>
          </div>
        </div>
      )}
      
      {/* Settings */}
      <div className="card">
        <h3 className="text-sm font-medium text-gray-400 mb-3">Settings</h3>
        
        <div className="space-y-3">
          <div>
            <label className="text-xs text-gray-500">Pressure (bar)</label>
            <input 
              type="number"
              value={settings.pressure_bar}
              onChange={(e) => updateSetting('pressure_bar', parseFloat(e.target.value))}
              className="input w-full mt-1"
              step="0.5"
              min="1"
              max="6"
            />
          </div>
          
          <div>
            <label className="text-xs text-gray-500">Max Flow per Zone (m³/h)</label>
            <input 
              type="number"
              value={settings.max_flow_m3h}
              onChange={(e) => updateSetting('max_flow_m3h', parseFloat(e.target.value))}
              className="input w-full mt-1"
              step="0.5"
              min="0.5"
              max="10"
            />
          </div>
          
          <div>
            <label className="text-xs text-gray-500">Spacing Factor</label>
            <input 
              type="number"
              value={settings.spacing_factor}
              onChange={(e) => updateSetting('spacing_factor', parseFloat(e.target.value))}
              className="input w-full mt-1"
              step="0.05"
              min="0.4"
              max="0.7"
            />
          </div>
          
          <div>
            <label className="text-xs text-gray-500">Preferred Brand</label>
            <select 
              value={settings.preferred_brand}
              onChange={(e) => updateSetting('preferred_brand', e.target.value)}
              className="input w-full mt-1"
            >
              <option value="any">Any</option>
              <option value="rainbird">RainBird</option>
              <option value="hunter">Hunter</option>
            </select>
          </div>
          
          <div className="border-t border-gray-700 pt-3 mt-3">
            <h4 className="text-xs text-gray-500 mb-2">Targets</h4>
            <div className="grid grid-cols-3 gap-2">
              <div>
                <label className="text-xs text-gray-600">CU %</label>
                <input 
                  type="number"
                  value={settings.target_cu}
                  onChange={(e) => updateSetting('target_cu', parseInt(e.target.value))}
                  className="input w-full mt-1 text-sm"
                  min="70"
                  max="100"
                />
              </div>
              <div>
                <label className="text-xs text-gray-600">DU %</label>
                <input 
                  type="number"
                  value={settings.target_du}
                  onChange={(e) => updateSetting('target_du', parseInt(e.target.value))}
                  className="input w-full mt-1 text-sm"
                  min="70"
                  max="100"
                />
              </div>
              <div>
                <label className="text-xs text-gray-600">Cov %</label>
                <input 
                  type="number"
                  value={settings.target_coverage}
                  onChange={(e) => updateSetting('target_coverage', parseInt(e.target.value))}
                  className="input w-full mt-1 text-sm"
                  min="80"
                  max="100"
                />
              </div>
            </div>
          </div>
        </div>
      </div>
      
      {/* Footer */}
      <div className="mt-4 text-center text-xs text-gray-600">
        <a href="https://www.qtech.hr" target="_blank" rel="noopener" className="hover:text-qirri-500">
          QTech Design
        </a>
        {' · '}
        <a href="mailto:info@qtech.hr" className="hover:text-qirri-500">
          info@qtech.hr
        </a>
      </div>
    </aside>
  )
}

