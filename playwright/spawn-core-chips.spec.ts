import { test, expect } from "@playwright/test";

test.describe("Spawn core chips", () => {
  test.beforeEach(async ({ page }) => {
    await page.setViewportSize({
      width: 640,
      height: 480,
    });
    await page.goto("/");
  });

  test("Spawns AND chip on button click", async ({ page }) => {
    await expect(page).toHaveTitle(/Circuit Sim/);

    await page.getByTestId("toolbar-btn-AND").click();

    await page.mouse.move(200, 200);
    await page.mouse.click(200, 200);

    await expect(page).toHaveScreenshot("and-chip.png");
  });

  test("Spawns multiple AND chips on button click", async ({ page }) => {
    await expect(page).toHaveTitle(/Circuit Sim/);

    await page.getByTestId("toolbar-btn-AND").click();
    await page.getByTestId("toolbar-btn-AND").click();
    await page.getByTestId("toolbar-btn-AND").click();

    await page.mouse.move(200, 300);
    await page.mouse.click(200, 300);

    await expect(page).toHaveScreenshot("and-multiple-chip.png");
  });

  test("Spawns OR chip on button click", async ({ page }) => {
    await expect(page).toHaveTitle(/Circuit Sim/);

    await page.getByTestId("toolbar-btn-OR").click();

    await page.mouse.move(200, 400);
    await page.mouse.click(200, 400);

    await expect(page).toHaveScreenshot("or-chip.png");
  });

  test("Spawns NOT chip on button click", async ({ page }) => {
    await expect(page).toHaveTitle(/Circuit Sim/);

    await page.getByTestId("toolbar-btn-NOT").click();

    await page.mouse.move(200, 300);
    await page.mouse.click(200, 300);

    await expect(page).toHaveScreenshot("not-chip.png");
  });
});
