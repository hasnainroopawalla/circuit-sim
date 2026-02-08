/// <reference types="vite/client" />
import { defineConfig } from 'vite'
import rootPkg from '../../package.json'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  base: process.env.BASE_PATH,
  define: {
    APP_VERSION: JSON.stringify(rootPkg.version || process.env.npm_package_version || 'dev'),
  },
  build: {
    outDir: '../../dist',
  },
  plugins: [react(),tailwindcss()],
})
