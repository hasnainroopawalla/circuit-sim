import { defineConfig } from "vitest/config";
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,          // allows describe/it/expect without imports
    environment: "jsdom",   // simulate browser DOM
    setupFiles: "./setupTests.ts",
    coverage: {
      provider: "c8",
      reporter: ["text", "lcov"],
    },
  },
});
