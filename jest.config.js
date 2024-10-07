/* eslint-disable no-undef */

module.exports = {
    preset: 'ts-jest',
    testEnvironment: 'jsdom',
    transform: {
        '^.+\\.tsx?$': 'ts-jest',
        "^.+\\.[t|j]sx?$": "babel-jest"
    },
    transformIgnorePatterns: ["<rootDir>/node_modules/(?!(power-mixin)/)"],
    modulePathIgnorePatterns: ["<rootDir>/playwright"],
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
        "<rootDir>/packages/web/src/setup-tests.ts"
    ],
    "moduleNameMapper": {
        "\\.(css|scss)$": "identity-obj-proxy"
    }
};