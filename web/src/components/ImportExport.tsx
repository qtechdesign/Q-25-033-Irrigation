'use client'

import { useCallback, useState } from 'react'

interface ImportExportProps {
  onImport: (data: any) => void
  onClose: () => void
}

export function ImportExport({ onImport, onClose }: ImportExportProps) {
  const [dragOver, setDragOver] = useState(false)
  const [error, setError] = useState<string | null>(null)
  
  const handleFile = useCallback(async (file: File) => {
    setError(null)
    
    try {
      const text = await file.text()
      const data = JSON.parse(text)
      
      // Validate structure
      if (!data.area?.vertices) {
        throw new Error('Invalid format: missing area.vertices')
      }
      
      onImport(data)
    } catch (e: any) {
      setError(e.message || 'Failed to parse file')
    }
  }, [onImport])
  
  const handleDrop = useCallback((e: React.DragEvent) => {
    e.preventDefault()
    setDragOver(false)
    
    const file = e.dataTransfer.files[0]
    if (file) handleFile(file)
  }, [handleFile])
  
  const handleFileInput = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0]
    if (file) handleFile(file)
  }, [handleFile])
  
  const handlePaste = useCallback((e: React.ClipboardEvent) => {
    const text = e.clipboardData.getData('text')
    if (text) {
      try {
        const data = JSON.parse(text)
        if (data.area?.vertices) {
          onImport(data)
        }
      } catch {
        // Not valid JSON, ignore
      }
    }
  }, [onImport])
  
  // Demo data
  const loadDemo = useCallback(() => {
    const demoData = {
      version: '1.0',
      area: {
        vertices: [
          [0, 0],
          [10000, 0],
          [10000, 5000],
          [5000, 5000],
          [5000, 8000],
          [0, 8000],
        ],
        area_m2: 65, // L-shaped area
        perimeter_m: 46,
      },
      settings: {
        pressure_bar: 3.0,
        max_flow_m3h: 2.0,
      },
      units: 'mm',
    }
    onImport(demoData)
  }, [onImport])
  
  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div 
        className="bg-gray-900 border border-gray-700 rounded-xl p-6 w-full max-w-lg"
        onPaste={handlePaste}
      >
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold">Import from AutoCAD</h2>
          <button onClick={onClose} className="text-gray-500 hover:text-white">
            <svg className="w-5 h-5" fill="none" viewBox="0 0 24 24" stroke="currentColor">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </div>
        
        {/* Drop Zone */}
        <div 
          className={`border-2 border-dashed rounded-lg p-8 text-center transition-colors ${
            dragOver ? 'border-qirri-500 bg-qirri-500/10' : 'border-gray-700'
          }`}
          onDragOver={(e) => { e.preventDefault(); setDragOver(true) }}
          onDragLeave={() => setDragOver(false)}
          onDrop={handleDrop}
        >
          <svg className="w-12 h-12 mx-auto text-gray-600 mb-3" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} 
              d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12" />
          </svg>
          <p className="text-gray-400 mb-2">
            Drop JSON file here or click to browse
          </p>
          <input 
            type="file" 
            accept=".json"
            onChange={handleFileInput}
            className="hidden"
            id="file-input"
          />
          <label 
            htmlFor="file-input"
            className="btn-secondary cursor-pointer inline-block"
          >
            Choose File
          </label>
        </div>
        
        {error && (
          <div className="mt-3 p-3 bg-red-900/50 border border-red-700 rounded text-red-300 text-sm">
            {error}
          </div>
        )}
        
        {/* Instructions */}
        <div className="mt-4 p-4 bg-gray-800 rounded-lg text-sm">
          <h4 className="font-medium text-gray-300 mb-2">How to export from AutoCAD:</h4>
          <ol className="text-gray-500 space-y-1 list-decimal list-inside">
            <li>Run <code className="bg-gray-700 px-1 rounded">QIRRAREA</code> to select your area</li>
            <li>Run <code className="bg-gray-700 px-1 rounded">QIRREXPORT</code> to save JSON</li>
            <li>Import the JSON file here</li>
          </ol>
        </div>
        
        {/* Demo */}
        <div className="mt-4 flex justify-between items-center">
          <button 
            onClick={loadDemo}
            className="text-sm text-qirri-500 hover:text-qirri-400"
          >
            Load demo area →
          </button>
          <span className="text-xs text-gray-600">
            or paste JSON (Ctrl+V)
          </span>
        </div>
      </div>
    </div>
  )
}

