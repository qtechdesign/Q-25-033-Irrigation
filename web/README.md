# Qirri Web App

Web-based irrigation optimization engine with real-time preview.

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Next.js App   │────►│ Cloudflare API  │────►│    Supabase     │
│  (Static Site)  │     │    (Worker)     │     │   (Database)    │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        │                       │
   Cloudflare             Optimization
     Pages                  Engine
```

## Quick Start

```bash
# Install dependencies
npm install

# Run development server
npm run dev
```

Open [http://localhost:3000](http://localhost:3000)

## Deployment

### Frontend (Cloudflare Pages)

```bash
npm run build
# Deploy out/ folder to Cloudflare Pages
```

### API (Cloudflare Worker)

```bash
cd worker
npm run worker:deploy
```

## Environment Variables

Copy `.env.example` to `.env.local`:

```bash
cp .env.example .env.local
```

Then fill in your Supabase credentials.

## Features

- 📥 **Import** - Load areas from AutoCAD JSON export
- ⚡ **Optimize** - GPU-ready grid placement algorithm
- 👁️ **Preview** - Real-time canvas visualization
- 📊 **Analysis** - CU/DU uniformity metrics
- 📤 **Export** - JSON format for AutoCAD import

## Data Flow

### Import (AutoCAD → Web)

```json
{
  "area": {
    "vertices": [[x1,y1], [x2,y2], ...],
    "area_m2": 150.5,
    "perimeter_m": 52.3
  },
  "settings": { ... },
  "units": "mm"
}
```

### Export (Web → AutoCAD)

```json
{
  "sprinklers": [
    { "x": 1000, "y": 2000, "brand": "RainBird", ... }
  ],
  "zones": [...],
  "results": { "cu": 92.3, "du": 87.1, ... }
}
```

## Tech Stack

- **Framework**: Next.js 14 (App Router)
- **Styling**: Tailwind CSS
- **State**: Zustand
- **Backend**: Cloudflare Workers
- **Database**: Supabase
- **Canvas**: HTML5 Canvas API

## Project Structure

```
web/
├── src/
│   ├── app/            # Next.js app router
│   ├── components/     # React components
│   ├── lib/
│   │   ├── optimizer/  # Placement algorithms
│   │   └── supabase/   # Database client
│   └── types/          # TypeScript types
├── worker/             # Cloudflare Worker API
└── public/             # Static assets
```

## License

MIT - QTech Design 2026

