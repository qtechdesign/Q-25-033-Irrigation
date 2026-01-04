'use client'

import { useEffect, useRef, useMemo } from 'react'
import type { Polygon, Sprinkler } from '@/types/irrigation'

interface CanvasProps {
  area?: Polygon
  sprinklers: Sprinkler[]
  units: string
}

export function Canvas({ area, sprinklers, units }: CanvasProps) {
  const canvasRef = useRef<HTMLCanvasElement>(null)
  
  // Calculate scale and offset to fit area in canvas
  const transform = useMemo(() => {
    if (!area?.vertices.length) {
      return { scale: 1, offsetX: 0, offsetY: 0, width: 0, height: 0 }
    }
    
    const xs = area.vertices.map(v => v.x)
    const ys = area.vertices.map(v => v.y)
    const minX = Math.min(...xs)
    const maxX = Math.max(...xs)
    const minY = Math.min(...ys)
    const maxY = Math.max(...ys)
    
    const width = maxX - minX
    const height = maxY - minY
    
    // Target canvas size (with padding)
    const canvasWidth = 800
    const canvasHeight = 600
    const padding = 50
    
    const scaleX = (canvasWidth - 2 * padding) / width
    const scaleY = (canvasHeight - 2 * padding) / height
    const scale = Math.min(scaleX, scaleY)
    
    const offsetX = padding + (canvasWidth - 2 * padding - width * scale) / 2 - minX * scale
    const offsetY = padding + (canvasHeight - 2 * padding - height * scale) / 2 - minY * scale
    
    return { scale, offsetX, offsetY, width, height }
  }, [area])
  
  // Draw canvas
  useEffect(() => {
    const canvas = canvasRef.current
    if (!canvas) return
    
    const ctx = canvas.getContext('2d')
    if (!ctx) return
    
    // Clear
    ctx.fillStyle = '#0f0f1a'
    ctx.fillRect(0, 0, canvas.width, canvas.height)
    
    // Draw grid
    ctx.strokeStyle = '#1a1a2e'
    ctx.lineWidth = 1
    for (let x = 0; x < canvas.width; x += 50) {
      ctx.beginPath()
      ctx.moveTo(x, 0)
      ctx.lineTo(x, canvas.height)
      ctx.stroke()
    }
    for (let y = 0; y < canvas.height; y += 50) {
      ctx.beginPath()
      ctx.moveTo(0, y)
      ctx.lineTo(canvas.width, y)
      ctx.stroke()
    }
    
    if (!area?.vertices.length) {
      // No area - show placeholder
      ctx.fillStyle = '#666'
      ctx.font = '16px system-ui'
      ctx.textAlign = 'center'
      ctx.fillText('Import an area from AutoCAD to begin', canvas.width / 2, canvas.height / 2)
      ctx.font = '12px system-ui'
      ctx.fillStyle = '#444'
      ctx.fillText('Use the Import button above', canvas.width / 2, canvas.height / 2 + 25)
      return
    }
    
    const { scale, offsetX, offsetY } = transform
    
    // Helper to transform points
    const tx = (x: number) => x * scale + offsetX
    const ty = (y: number) => canvas.height - (y * scale + offsetY) // Flip Y
    
    // Draw area polygon
    ctx.beginPath()
    ctx.moveTo(tx(area.vertices[0].x), ty(area.vertices[0].y))
    for (let i = 1; i < area.vertices.length; i++) {
      ctx.lineTo(tx(area.vertices[i].x), ty(area.vertices[i].y))
    }
    ctx.closePath()
    
    // Fill
    ctx.fillStyle = 'rgba(34, 197, 94, 0.1)'
    ctx.fill()
    
    // Stroke
    ctx.strokeStyle = '#22c55e'
    ctx.lineWidth = 2
    ctx.stroke()
    
    // Draw sprinklers
    for (const spr of sprinklers) {
      const cx = tx(spr.position.x)
      const cy = ty(spr.position.y)
      const r = spr.radius_m * scale
      
      // Spray coverage (semi-transparent)
      ctx.beginPath()
      if (spr.arc === 360) {
        ctx.arc(cx, cy, r, 0, Math.PI * 2)
      } else {
        const startAngle = (-spr.rotation - spr.arc / 2) * Math.PI / 180
        const endAngle = (-spr.rotation + spr.arc / 2) * Math.PI / 180
        ctx.moveTo(cx, cy)
        ctx.arc(cx, cy, r, startAngle, endAngle)
        ctx.closePath()
      }
      ctx.fillStyle = 'rgba(59, 130, 246, 0.15)'
      ctx.fill()
      ctx.strokeStyle = 'rgba(59, 130, 246, 0.3)'
      ctx.lineWidth = 1
      ctx.stroke()
      
      // Head symbol
      ctx.beginPath()
      ctx.arc(cx, cy, 4, 0, Math.PI * 2)
      ctx.fillStyle = '#3b82f6'
      ctx.fill()
      ctx.strokeStyle = '#fff'
      ctx.lineWidth = 1
      ctx.stroke()
    }
    
    // Stats overlay
    ctx.fillStyle = 'rgba(0,0,0,0.7)'
    ctx.fillRect(10, 10, 150, 60)
    ctx.fillStyle = '#888'
    ctx.font = '11px monospace'
    ctx.textAlign = 'left'
    ctx.fillText(`Area: ${area.area_m2.toFixed(1)} m²`, 20, 30)
    ctx.fillText(`Sprinklers: ${sprinklers.length}`, 20, 45)
    ctx.fillText(`Units: ${units}`, 20, 60)
    
  }, [area, sprinklers, transform, units])
  
  return (
    <div className="canvas-container w-full h-full flex items-center justify-center">
      <canvas 
        ref={canvasRef}
        width={800}
        height={600}
        className="max-w-full max-h-full"
      />
    </div>
  )
}

