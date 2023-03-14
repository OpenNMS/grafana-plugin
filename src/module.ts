//import { OpenNMSHelmAppConfigCtrl } from './components/config';
import { AppPlugin } from '@grafana/data'
import { App } from './components/App'
import { AppConfig } from './components/AppConfig'

//import { initializeCss } from 'lib/utils'

//initializeCss();

//export { OpenNMSHelmAppConfigCtrl as ConfigCtrl };

export const plugin = new AppPlugin<{}>().setRootPage(App).addConfigPage({
  title: 'Configuration',
  icon: 'cog',
  body: AppConfig,
  id: 'configuration'
})
