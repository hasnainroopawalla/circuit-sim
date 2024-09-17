import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./playwright",
  snapshotPathTemplate: "{testDir}/__screenshots__/{testFilePath}/{arg}{ext}",

  /* Run tests in files in parallel */
  fullyParallel: true,
  /* Fail the build on CI if you accidentally left test.only in the source code. */
  forbidOnly: !!process.env.CI,
  use: {
    baseURL: "http://localhost:8080/",
    video: "on-first-retry",
    screenshot: "only-on-failure",
  },

  /* Configure projects for major browsers */
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],

  webServer: {
    command: "yarn start:prod",
    url: "http://localhost:8080/",
    timeout: 120000,
    reuseExistingServer: !process.env.CI,
  },
});
