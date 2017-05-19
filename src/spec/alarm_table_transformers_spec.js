import {transformers} from '../panels/alarm-table/transformers';
import {TableModel} from '../panels/alarm-table/table_model';

describe('Table transformer', function() {
  let transformer;

  beforeEach(function() {
    transformer = transformers['table'];
  });

  describe('Listing the columns', function() {
    it('should return an empty list of columns when no data is present', function() {
      expect(transformer.getColumns([])).to.have.length(0);
    });

    it('should return an empty list of columns when the given table has no columns', function() {
      let table = new TableModel();
      expect(transformer.getColumns([table])).to.have.length(0);
    });
  });

  describe('Transforming the data', function() {
    it('should return all columns if none are specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push("A");

      let row = [1];
      table.rows.push(row);

      let panel = {};
      let model = new TableModel();

      transformer.transform([table], panel, model);
      expect(model.columns).to.eql(table.columns);
      expect(model.rows).to.eql(table.rows);
    });

    it('should filter the columns if one or more are specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push('A', 'B', 'C');

      let actualRow = [1,2,3];
      let metadata = {
        'alarm': 'abc'
      };
      actualRow.meta = metadata;
      table.rows.push(actualRow);

      let panel = {columns: [{
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      // The meta-data that was on the original row should also be present on the
      // transformed row
      let expectedRow = [2];
      expectedRow.meta = metadata;

      expect(model.columns).to.eql(panel.columns);
      expect(model.rows).to.eql([expectedRow]);
    });

    it('should re-order the columns according the order specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push('A', 'B', 'C');
      table.rows.push([1,2,3]);

      let panel = {columns: [{
        'text': 'C'
      }, {
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      expect(model.columns).to.eql(panel.columns);
      expect(model.rows).to.eql([[3, 2]]);
    });
  });

});