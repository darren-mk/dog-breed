var path = require("path");

module.exports = {
    mode: "development",
    entry: "./src/App/App.fs.js",
    output: {
        path: path.join(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8080,
    },
}
