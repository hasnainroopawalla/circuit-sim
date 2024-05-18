/* eslint-disable no-undef */
/* eslint-disable @typescript-eslint/no-var-requires */

const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
    mode: "development",
    entry: './index.tsx',
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                use: 'ts-loader',
                exclude: /node_modules/,
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ],
    },
    resolve: {
        extensions: ['.tsx', '.ts', '.js'],
    },
    output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, '../../dist'),
    },
    devServer: {
        static: './dist',
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: './index.html',
        }),
        new webpack.DefinePlugin({
            VERSION: JSON.stringify(require("../../package.json").version),
        }),
    ],
};