module.exports.getWebpackConfig = (config, options) => {
  config.externals = [
    'jquery.flot',
    'jquery.flot.crosshair',
    'jquery.flot.selection',
    'jquery.flot.stack',
    'jquery.flot.time',
  ].concat(config.externals);

  return config;
};
