/// <reference types="vite/client" />
import { defineConfig } from 'vite'
import rootPkg from '../../package.json'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'


console.log(getAppVersion())
export default defineConfig({
  base: isLocalBuild() ? process.env.BASE_PATH : "/flux/",
  define: {
    APP_VERSION: JSON.stringify(getAppVersion()),
  },
  build: {
    outDir: '../../dist',
  },
  plugins: [react(),tailwindcss()],
})


function getAppVersion(): string {
  return isLocalBuild() ? "(local)" : rootPkg.version
}

function isLocalBuild(): boolean{
  return process.env.NODE_ENV === "development" 
}