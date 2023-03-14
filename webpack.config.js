module.exports.getWebpackConfig = (config, options) => {
  config.output.hashFunction = 'sha256';

  return config;
};
