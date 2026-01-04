'use client'

interface HeaderProps {
  projectName: string
  onImport: () => void
  onExport: () => void
}

export function Header({ projectName, onImport, onExport }: HeaderProps) {
  return (
    <header className="h-14 bg-gray-900 border-b border-gray-800 flex items-center justify-between px-4">
      {/* Logo */}
      <div className="flex items-center gap-3">
        <div className="w-8 h-8 bg-qirri-600 rounded-lg flex items-center justify-center">
          <svg className="w-5 h-5 text-white" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} 
              d="M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707" />
          </svg>
        </div>
        <div>
          <h1 className="text-lg font-semibold text-white">Qirri</h1>
          <p className="text-xs text-gray-500">Intelligent Irrigation Planner</p>
        </div>
      </div>
      
      {/* Project Name */}
      <div className="text-gray-400">
        {projectName}
      </div>
      
      {/* Actions */}
      <div className="flex items-center gap-2">
        <button 
          onClick={onImport}
          className="btn-secondary flex items-center gap-2"
        >
          <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} 
              d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12" />
          </svg>
          Import
        </button>
        <button 
          onClick={onExport}
          className="btn-secondary flex items-center gap-2"
        >
          <svg className="w-4 h-4" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} 
              d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4" />
          </svg>
          Export
        </button>
        <a 
          href="https://www.qtech.hr" 
          target="_blank" 
          rel="noopener"
          className="text-xs text-gray-500 hover:text-qirri-500 ml-4"
        >
          QTech Design
        </a>
      </div>
    </header>
  )
}

