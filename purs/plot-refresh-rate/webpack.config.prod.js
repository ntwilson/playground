const path = require('path');

module.exports = {
  entry: './src/app.js',
  mode: 'production',
  devServer: {
    contentBase: path.join(__dirname, 'public'),
    compress: true,
    port: 9000
  },
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'public')
  }
};