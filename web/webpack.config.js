const path = require("path");
const HTMLWebpackPlugin = require("html-webpack-plugin");
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");
const MiniCSSExtractPlugin = require("mini-css-extract-plugin");

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
  module: {
    rules: [
      {
        test: /\.css$/i,
        use: [MiniCSSExtractPlugin.loader, "css-loader"],
      },
    ],
  },
  resolve: {
    extensions: [".js", ".css", ".json"]
  },
  plugins: [
    new CleanWebpackPlugin(),
    new HTMLWebpackPlugin({
      title: "The rsmonkey Playground",
    }),
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "crate"),
      outDir: path.resolve(__dirname, "src", "wasm"),
    }),
    new MiniCSSExtractPlugin(),
  ],
};
