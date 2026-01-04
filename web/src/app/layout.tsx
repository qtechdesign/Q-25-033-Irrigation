import type { Metadata } from 'next'
import './globals.css'

export const metadata: Metadata = {
  title: 'Qirri - Intelligent Irrigation Planner',
  description: 'Professional irrigation design optimization by QTech Design',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className="min-h-screen">
        {children}
      </body>
    </html>
  )
}

