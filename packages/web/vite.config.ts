/// <reference types="vite/client" />
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  base: '/circuit-sim/',
  build: {
    outDir: '../../dist',
  },
  plugins: [react(),tailwindcss()],
})
