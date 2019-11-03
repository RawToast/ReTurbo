const path = require('path');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  plugins: [
    new CopyPlugin([
      { from: 'public/assets', 
      to: 'assets',
      ignore: ['*.gif'] },
      { from: 'public/index.html', 
      to: 'index.html' }
    ]),
  ],
  mode: "development",
  entry: "./lib/js/src/index.js"
}
