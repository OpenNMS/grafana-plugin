import _ from 'lodash';
import {TableModel} from './table_model';

let transformers = {};

transformers['table'] = {
  description: 'Table',
  getColumns: function(data) {
    if (!data || data.length === 0) {
      return [];
    }
    return data[0].columns;
  },

  transform: function(data, panel, model) {
    if (!data || data.length === 0) {
      return;
    }

    if (data[0].type !== 'table') {
      throw {message: 'Query result is not in table format, try using another transform.'};
    }

    if (panel.columns && panel.columns.length > 0) {
      // Convert the rows to columns
      let cellsByColumnIndex = [];
      _.each(data[0].columns, col => cellsByColumnIndex.push([]));

      // For every row
      _.each(data[0].rows, row => {
        // And every cell in the row
        for (let i = 0; i < row.length; i++) {
          // Append the cell to the appropriate column list
          cellsByColumnIndex[i].push(row[i]);
        }
      });

      // Now reorder the columns according the list of columns in the panel definition
      let cellsByPanelColumnIndex = [];
      for (let j = 0; j < panel.columns.length; j++) {
        let colDef = panel.columns[j];
        // Determine the column's index in the given data
        let idx = _.findIndex(data[0].columns, col => {
          return col === colDef.text || col.text === colDef.text;
        });
        if (idx < 0) {
          throw {message: 'The column named "' + colDef.text + '" was specified in the panel definition,' +
          ' but was not found in the given data. The available columns include: ' + JSON.stringify(data[0].columns)};
        }
        // Re-order
        cellsByPanelColumnIndex[j] = cellsByColumnIndex[idx];
      }

      // Convert the columns back to rows
      model.columns = panel.columns.slice();
      model.rows = [];
      for (let k = 0; k < cellsByPanelColumnIndex[0].length; k++) {
        let row = [];
        for (let l = 0; l < panel.columns.length; l++) {
          row.push(cellsByPanelColumnIndex[l][k]);
        }
        model.rows.push(row);
      }
    } else {
      // Use all the columns as-is
      model.columns = data[0].columns;
      model.rows = data[0].rows;
    }
  }
};

function transformDataToTable(data, panel) {
  let model = new TableModel();

  if (!data || data.length === 0) {
    return model;
  }

  let transformer = transformers[panel.transform];
  if (!transformer) {
    throw {message: 'Transformer ' + panel.transform + ' not found'};
  }

  transformer.transform(data, panel, model);
  return model;
}

export {transformers, transformDataToTable};
