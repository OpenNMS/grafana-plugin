'use strict';

System.register(['lodash', './table_model'], function (_export, _context) {
  "use strict";

  var _, TableModel, transformers;

  function transformDataToTable(data, panel) {
    var model = new TableModel();

    if (!data || data.length === 0) {
      return model;
    }

    var transformer = transformers[panel.transform];
    if (!transformer) {
      throw { message: 'Transformer ' + panel.transform + ' not found' };
    }

    transformer.transform(data, panel, model);
    return model;
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_table_model) {
      TableModel = _table_model.TableModel;
    }],
    execute: function () {
      _export('transformers', transformers = {});

      transformers['table'] = {
        description: 'Table',
        getColumns: function getColumns(data) {
          if (!data || data.length === 0) {
            return [];
          }

          // Create a list containing the column names from each table
          var columns = [];
          _.forEach(data, function (table) {
            columns.push(table.columns);
          });
          // Return the intersection of all column names only returning
          // those that are present in each table
          // NOTE: _.intersection([array]) should work here, but appears to be broken
          return columns.shift().filter(function (v) {
            return columns.every(function (a) {
              return a.indexOf(v) !== -1;
            });
          });
        },

        transformTable: function transformTable(data, columnsToInclude) {
          var model = new TableModel();

          // Convert the rows to columns
          var cellsByColumnIndex = [];
          _.each(data.columns, function (col) {
            return cellsByColumnIndex.push([]);
          });

          // For every row
          _.each(data.rows, function (row) {
            // And every cell in the row
            for (var i = 0; i < row.length; i++) {
              // Append the cell to the appropriate column list
              cellsByColumnIndex[i].push(row[i]);
            }
          });

          // Now reorder the columns according the list of columns in the panel definition
          var cellsByPanelColumnIndex = [];

          var _loop = function _loop(j) {
            var colDef = columnsToInclude[j];
            // Determine the column's index in the given data
            var idx = _.findIndex(data.columns, function (col) {
              return col === colDef.text || col.text === colDef.text;
            });
            if (idx < 0) {
              throw { message: 'The column named "' + colDef.text + '" was specified in the panel definition,' + ' but was not found in the given data. The available columns include: ' + JSON.stringify(data.columns) };
            }
            // Re-order
            cellsByPanelColumnIndex[j] = cellsByColumnIndex[idx];
          };

          for (var j = 0; j < columnsToInclude.length; j++) {
            _loop(j);
          }

          // Convert the columns back to rows
          model.columns = columnsToInclude;
          model.rows = [];
          for (var k = 0; k < cellsByPanelColumnIndex[0].length; k++) {
            var row = [];
            for (var l = 0; l < columnsToInclude.length; l++) {
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

        mergeTables: function mergeTables(tables) {
          var model = new TableModel();
          // Use the list of columns from the first table:
          //  transformTable() will ensure that all tables have the same columns
          model.columns = tables[0].columns;
          // Concatenate the rows
          _.each(tables, function (table) {
            model.rows = _.concat(model.rows, table.rows);
          });
          // De-duplicate by (source, alarm.id) tuple
          model.rows = _.uniqBy(model.rows, function (row) {
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

        transform: function transform(data, panel, model) {
          var _this = this;

          if (!data || data.length === 0) {
            return;
          }

          // Determine the list of columns to include
          var columnsToInclude = [];
          if (panel.columns && panel.columns.length > 0) {
            // One or more columns were specified in the column definition, use these
            columnsToInclude = panel.columns;
          } else {
            // Use all the available columns
            columnsToInclude = this.getColumns(data);
          }

          // Transform each of the datasource results individually
          var transformedTables = [];
          _.each(data, function (dat) {
            if (dat.type !== 'table') {
              throw { message: 'Query result is not in table format, try using another transform.' };
            }
            transformedTables.push(_this.transformTable(dat, columnsToInclude));
          });

          // Merge the results and update the model
          var mergedTables = this.mergeTables(transformedTables);
          model.columns = mergedTables.columns;
          model.rows = mergedTables.rows;
        }
      };
      _export('transformers', transformers);

      _export('transformDataToTable', transformDataToTable);
    }
  };
});
//# sourceMappingURL=transformers.js.map
