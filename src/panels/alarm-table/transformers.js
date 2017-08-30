import _ from 'lodash';
import {TableModel} from './table_model';

let transformers = {};

transformers['table'] = {
  description: 'Table',
  getColumns: function(data) {
    if (!data || data.length === 0) {
      return [];
    }

    // Create a list containing the column names from each table
    let columns = [];
    _.forEach(data, (table) => {
      columns.push(table.columns);
    });
    // Return the intersection of all column names only returning
    // those that are present in each table
    // NOTE: _.intersection([array]) should work here, but appears to be broken
    return columns.shift().filter(function(v) {
      return columns.every(function(a) {
        return a.indexOf(v) !== -1;
      });
    });
  },

  transformTable: function(data, columnsToInclude) {
    let model = new TableModel();

    // Convert the rows to columns
    let cellsByColumnIndex = [];
    _.each(data.columns, col => cellsByColumnIndex.push([]));

    // For every row
    _.each(data.rows, row => {
      // And every cell in the row
      for (let i = 0; i < row.length; i++) {
        // Append the cell to the appropriate column list
        cellsByColumnIndex[i].push(row[i]);
      }
    });

    // Used when no matching column is found
    let emptyColumn = new Array(data.rows.length);

    // Now reorder the columns according the list of columns in the panel definition
    let cellsByPanelColumnIndex = [];
    for (let j = 0; j < columnsToInclude.length; j++) {
      let colDef = columnsToInclude[j];
      // Determine the column's index in the given data
      let idx = _.findIndex(data.columns, col => {
        return col === colDef.text || col.text === colDef.text;
      });
      if (idx < 0) {
        // The column does not exist
        cellsByPanelColumnIndex[j] = emptyColumn;
        continue;
      }
      // Re-order
      cellsByPanelColumnIndex[j] = cellsByColumnIndex[idx];
    }

    // Convert the columns back to rows
    model.columns = columnsToInclude;
    model.rows = [];
    for (let k = 0; k < cellsByPanelColumnIndex[0].length; k++) {
      let row = [];
      for (let l = 0; l < columnsToInclude.length; l++) {
        row.push(cellsByPanelColumnIndex[l][k]);
      }
      // Preserve the meta-data, if any
      if (data.rows[k].meta !== undefined) {
        row.meta = data.rows[k].meta;
      }
      model.rows.push(row);
    }
    return model;
  },

  mergeTables: function(tables) {
    let model = new TableModel();
    // Use the list of columns from the first table:
    //  transformTable() will ensure that all tables have the same columns
    model.columns = tables[0].columns;
    // Concatenate the rows
    _.each(tables, table => {
      model.rows = _.concat(model.rows, table.rows);
    });
    // De-duplicate by (source, alarm.id) tuple
    model.rows = _.uniqBy(model.rows, row => {
      if (row.meta && row.meta.alarm) {
        return JSON.stringify({
          source: row.meta.source,
          alarmId: row.meta.alarm.id
        });
      } else {
        return row;
      }
    });
    return model;
  },

  transform: function(data, panel, model) {
    if (!data || data.length === 0) {
      return;
    }

    // Determine the list of columns to include
    let columnsToInclude = [];
    if (panel.columns && panel.columns.length > 0) {
      // One or more columns were specified in the column definition, use these
      columnsToInclude = panel.columns;
    } else {
      // Use all the available columns
      columnsToInclude = this.getColumns(data);
    }

    // Transform each of the datasource results individually
    let transformedTables = [];
    _.each(data, (dat) => {
      if (dat.type !== 'table') {
        throw {message: 'Query result is not in table format, try using another transform.'};
      }
      transformedTables.push(this.transformTable(dat, columnsToInclude));
    });

    // Merge the results and update the model
    let mergedTables = this.mergeTables(transformedTables);
    model.columns = mergedTables.columns;
    model.rows = mergedTables.rows;
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
