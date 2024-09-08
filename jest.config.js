/* eslint-disable no-undef */

module.exports = {
    preset: 'ts-jest',
    testEnvironment: 'jsdom',
    transform: {
        '^.+\\.ts?$': 'ts-jest',
    },
    transformIgnorePatterns: ['<rootDir>/node_modules/'],
    collectCoverage: false,
    collectCoverageFrom: [
        "packages/**/*.{js,jsx,ts,tsx}",
        "!<rootDir>/node_modules/"
    ],
    coverageThreshold: {
        global: {
            "lines": 0,
            "statements": 0
        }
    },
    setupFiles: [
        "jest-canvas-mock"
    ],
    setupFilesAfterEnv: [
        "<rootDir>/packages/web/src/setupTests.ts"
    ],
    moduleNameMapper: {
        "\\.(css|scss)$": "<rootDir>/packages/__mocks__/styleMock.js"
    }
};