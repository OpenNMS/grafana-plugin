import { AppPlugin } from '@grafana/data';
import { Helm } from './components/Helm';
import { HelmConfig } from './components/HelmConfig';

export const plugin = new AppPlugin<{}>().setRootPage(Helm).addConfigPage({
  title: 'Configuration',
  icon: 'cog',
  body: HelmConfig,
  id: 'configuration',
});
