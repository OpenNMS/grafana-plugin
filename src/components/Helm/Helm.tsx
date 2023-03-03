import * as React from 'react';
import { AppRootProps } from '@grafana/data';
import { PluginPropsContext } from '../../lib/utils.plugin';
// import { Routes } from '../Routes';

export class Helm extends React.PureComponent<AppRootProps> {
  render() {
    return (
      <PluginPropsContext.Provider value={this.props}>

      </PluginPropsContext.Provider>
    );
  }
}
