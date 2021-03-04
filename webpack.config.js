const _ = require('lodash');
const path = require('path');
const childProcess = require('child_process');

const argv = require('yargs').argv;
const isProduction = argv.mode === 'production';

const CopyPlugin = require('copy-webpack-plugin');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

const rootdir = __dirname; // eslint-disable-line no-undef
const srcdir = path.resolve(rootdir, 'src');
const distdir = path.resolve(rootdir, 'dist');

const createVariants = require('parallel-webpack').createVariants;

const plugins = [
  'datasources/entity-ds',
  'datasources/flow-ds',
  'datasources/perf-ds',
  'panels/alarm-histogram',
  'panels/alarm-table',
  'panels/filter-panel',
  'panels/flow-histogram',
];

const baseconfig = {
  devtool: 'source-map',
  entry: {},
  output: {
    filename: '[name].js',
    chunkFilename: '[name].js',
    libraryTarget: 'amd',
    path: distdir,
  },
  externals: [
    '@grafana/data',
    '@grafana/ui',
    'angular',
    'jquery',
    'jquery.flot',
    'jquery.flot.crosshair',
    'jquery.flot.selection',
    'jquery.flot.stack',
    'jquery.flot.time',
    'lodash',
    'moment',
    function (_context, request, callback) {
      // automatically pass-through grafana built-in paths
      const prefixes = ['app/', 'grafana/'];
      for (let prefix of prefixes) {
        if (request.indexOf(prefix) === 0) {
          return callback(null, request);
        }
      }
      callback();
    }
  ],
  module: {
    rules: [
      {
        test: /\.(js|ts)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            cacheDirectory: true,
            /*
            plugins: [
              '@babel/plugin-transform-runtime',
            ],
            presets: ['@babel/preset-env'],
            */
          },
        },
      },
      {
        test: /\.(ttf|otf|eot|woff2?|svg)(\?v=.+)?$/,
        use: [ 'url-loader' ],
      },
      {
        test: /\.(sa|sc|c)ss$/,
        exclude: /\.(dark|light)\./, // these are handled in the root module
        use: [
          {
            loader: 'style-loader',
          },
          {
            loader: 'css-loader',
            options: {
              sourceMap: true,
            },
          },
          {
            loader: 'sass-loader',
          },
        ],
      },
    ],
  },
  plugins: [
  ],
  resolve: {
    extensions: [ '.ts', '.js' ],
    alias: {
      src: srcdir,
    },
  },
};

function createConfig(options) {
  const config = _.cloneDeep(baseconfig);

  if (options.type === 'root') {
    config.entry = {
      'module': [ '@grafana/ui', '@grafana/data', 'src/module.js' ],
      'datasources/perf-ds/css/opennms.dark': 'src/datasources/perf-ds/sass/opennms.dark.scss',
      'datasources/perf-ds/css/opennms.light': 'src/datasources/perf-ds/sass/opennms.light.scss',
      'panels/alarm-table/css/table.dark': 'src/panels/alarm-table/sass/table.dark.scss',
      'panels/alarm-table/css/table.light': 'src/panels/alarm-table/sass/table.light.scss',
      'panels/filter-panel/css/filter.dark': 'src/panels/filter-panel/sass/filter.dark.scss',
      'panels/filter-panel/css/filter.light': 'src/panels/filter-panel/sass/filter.light.scss',
      'panels/flow-histogram/css/legend.dark': 'src/panels/flow-histogram/sass/legend.dark.scss',
      'panels/flow-histogram/css/legend.light': 'src/panels/flow-histogram/sass/legend.light.scss',
    };

    config.module.rules = config.module.rules.concat([
      {
        test: /\.(sa|sc|c)ss$/,
        include: /\.(dark|light)\./,
        use: [
          {
            loader: MiniCssExtractPlugin.loader,
            options: {
              // sourceMap: true,
            },
          },
          {
            loader: 'css-loader',
            options: {
              sourceMap: true,
            },
          },
          {
            loader: 'sass-loader',
          },
        ],
      },
    ]);

    config.plugins = config.plugins.concat([
      new MiniCssExtractPlugin({
        // Options similar to the same options in webpackOptions.output
        // both options are optional
        filename: "[name].css",
        chunkFilename: "[id].css"
      }),
      new CopyPlugin([
        {
          from: '*.md',
          to: distdir,
          transform: (content, path) => {
            if (path.indexOf('README.md') > 0) {
              content += '\n'
                + '## Build Information\n'
                + '\n'
                + '- Build Date: ' + new Date().toISOString() + '\n'
                // eslint-disable-next-line no-sync
                + '- Git Revision: ' + childProcess.execSync('git rev-parse HEAD') + '\n'
            }
            return content;
          }
        },
        {
          from: '**/*.json',
          to: distdir,
          context: 'src',
        },
        {
          from: '**/*.svg',
          to: distdir,
          context: 'src',
        },
        {
          from: '**/*.html',
          to: distdir,
          context: 'src',
        },
        {
          from: '**/img/*',
          to: distdir,
          context: 'src',
        },
      ]),
    ]);
  } else if (options.type === 'plugins') {
    config.output.filename = '[name]/module.js';
    config.output.libraryTarget = 'amd';

    for (let plugin of plugins) {
      config.entry[plugin] = ['@grafana/ui', '@grafana/data', 'angular', path.resolve(rootdir, 'src', plugin, 'module.js')];
    }
  } else {
    console.log('unhandled type: ' + options.type);
  }

  return config;
}

module.exports = createVariants({}, { type: ['root', 'plugins' ]}, createConfig);
