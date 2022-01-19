import { assignModelProperties, setOptionAsCurrent, setOptionFromUrl } from './utils';

export class TextBoxVariable {
  type = 'textbox';
  name = '';
  label = '';
  hide = 0;
  query = '';
  current = {} as any;
  options = [] as any[];
  skipUrlSync = false;

  defaults = {
    type: 'textbox',
    name: '',
    label: '',
    hide: 0, // VariableHide.dontHide
    query: '',
    current: {} as any, // as VariableOption
    options: [] as any[],
    skipUrlSync: false,
  };

  /** @ngInject **/
  constructor(public model, public filterColumn, public filterState, public dashboardSrv, public datasourceSrv, public templateSrv) {
    assignModelProperties(this, model, this.defaults);
  }

  getSaveModel() {
    assignModelProperties(this.model, this, this.defaults);
    return this.model;
  }

  setValue(option) {
    setOptionAsCurrent(this, option);
  }

  updateOptions() {
    this.options = [{ text: this.query.trim(), value: this.query.trim(), selected: false }];
    this.current = this.options[0];
    return Promise.resolve();
  }

  dependsOn() {
    return false;
  }

  setValueFromUrl(urlValue) {
    this.query = urlValue;
    return setOptionFromUrl(this, urlValue);
  }

  getValueForUrl() {
    return this.current.value;
  }
}
