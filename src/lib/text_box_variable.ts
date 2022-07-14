import { assignModelProperties, getValueOrFirstElement, setOptionAsCurrent, setOptionFromUrl } from './utils';

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
    const query = (getValueOrFirstElement<string>(this.query) || '').trim();

    this.options = [{ text: query, value: query, selected: false }];
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
