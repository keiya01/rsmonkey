const path = require("path");
const HTMLWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const isDev = process.env.NODE_ENV !== "production";

const devServer = isDev ? { 
  contentBase: path.resolve(__dirname, 'dist'),
  compress: true,
} : {};

module.exports = {
  mode: isDev ? "development" : "production",
  entry: path.resolve("src", "index.js"),
  output: {
    filename: "[name].bundle.js",
    chunkFilename: "[id].[contenthash:7].js",
    path: path.resolve(__dirname, "dist"),
  },
  experiments: { asyncWebAssembly: true },
  devServer,
  plugins: [
    new CleanWebpackPlugin(),
    new HTMLWebpackPlugin({
      title: "rsmonkey Editor",
    }),
    new WasmPackPlugin({
      crateDirectory: path.resolve("..", "app"),
      outDir: path.resolve(__dirname, "src", "wasm"),
    }),
  ],
};
