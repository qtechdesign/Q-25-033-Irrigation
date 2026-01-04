import { createClient } from '@supabase/supabase-js'

const supabaseUrl = process.env.NEXT_PUBLIC_SUPABASE_URL!
const supabaseAnonKey = process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY!

export const supabase = createClient(supabaseUrl, supabaseAnonKey)

// Database types will be generated from Supabase
export type Database = {
  public: {
    Tables: {
      projects: {
        Row: {
          id: string
          created_at: string
          updated_at: string
          user_id: string
          name: string
          data: any
        }
        Insert: {
          id?: string
          created_at?: string
          updated_at?: string
          user_id: string
          name: string
          data: any
        }
        Update: {
          id?: string
          created_at?: string
          updated_at?: string
          user_id?: string
          name?: string
          data?: any
        }
      }
      catalogue: {
        Row: {
          id: string
          brand: string
          model: string
          nozzle: string
          radius_m: number
          flow_m3h: number
          precip_mmh: number
          pressure_bar: number
          arc_options: number[]
        }
        Insert: {
          id?: string
          brand: string
          model: string
          nozzle: string
          radius_m: number
          flow_m3h: number
          precip_mmh: number
          pressure_bar: number
          arc_options: number[]
        }
        Update: {
          brand?: string
          model?: string
          nozzle?: string
          radius_m?: number
          flow_m3h?: number
          precip_mmh?: number
          pressure_bar?: number
          arc_options?: number[]
        }
      }
    }
  }
}

