import type { Configuration } from 'webpack'
import { merge } from 'webpack-merge'
import CopyWebpackPlugin from 'copy-webpack-plugin'
import ReplaceInFileWebpackPlugin from 'replace-in-file-webpack-plugin'
import grafanaConfig from './.config/webpack/webpack.config'
import { DIST_DIR } from './.config/webpack/constants'
import { getPluginJson } from './.config/webpack/utils'

const config = async (env): Promise<Configuration> => {
  const baseConfig = await grafanaConfig(env)
  const pluginJson = getPluginJson()

  return merge(baseConfig, {
    // Add custom config here...
    output: {
      hashFunction: 'sha256',
    },
    plugins: [
      // add README.md to datasources, this is what is displayed when clicking the "?" next to
      // a datasource in the query editor.
      // Grafana looks for 'README.md', but our 'README.md' is meant to be read by developers, not
      // end users. This copies content meant for end user datasource help.
      new CopyWebpackPlugin({
        patterns: [
          { from: 'datasources/entity-ds/help-README.md', to: 'datasources/entity-ds/README.md' },
          { from: 'datasources/flow-ds/help-README.md', to: 'datasources/flow-ds/README.md' },
          { from: 'datasources/perf-ds/help-README.md', to: 'datasources/perf-ds/README.md' },
        ]
      }),
      // Replace certain template-variables in the README and plugin.json
      new ReplaceInFileWebpackPlugin([
        {
          dir: DIST_DIR,
          files: [
            'datasources/entity-ds/README.md',
            'datasources/flow-ds/README.md',
            'datasources/perf-ds/README.md'
          ],
          rules: [
            {
              search: /\%OPG_DOCS_BASE_URL\%/g,
              replace: 'https://docs.opennms.com/helm/latest'
            }
          ]
        }
      ])
    ]
  })
}

export default config
