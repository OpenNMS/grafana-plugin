module.exports.getWebpackConfig = (config, options) => {
  config.externals = config.externals.concat([
    'jquery.flot',
    'jquery.flot.crosshair',
    'jquery.flot.selection',
    'jquery.flot.stack',
    'jquery.flot.time',
    'app/core/config',
    'app/core/core_module',
    'app/core/utils/kbn',
    'app/plugins/sdk',
  ]);

  return config;
};
