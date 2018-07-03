'use strict';

var _interpolate = require('../datasources/perf-ds/interpolate');

describe('OpenNMSPMDatasource :: interpolate', function () {
  var query = { 'resource': '$node', 'metric': '$x.$y' };

  it('should return the same object when the list of attributes is empty', function () {
    expect((0, _interpolate.interpolate)(query, [], [])).to.deep.equal([query]);
  });

  it('should return the same object when the list of variables is empty', function () {
    expect((0, _interpolate.interpolate)(query, ['resource'], [])).to.deep.equal([query]);
  });

  it('should return the same object when no matching variables are referenced', function () {
    expect((0, _interpolate.interpolate)(query, ['resource'], [{ name: '!node', value: ['1'] }])).to.deep.equal([query]);
  });

  it('should be able to interpolate a single variable in a single attribute', function () {
    var interpolated = (0, _interpolate.interpolate)(query, ['resource'], [{ name: 'node', value: ['1', '2'] }]);
    expect(interpolated).to.deep.equal([{ 'resource': '1', 'metric': '$x.$y' }, { 'resource': '2', 'metric': '$x.$y' }]);
  });

  it('should be able to interpolate multiple variables in a single attribute', function () {
    var interpolated = (0, _interpolate.interpolate)(query, ['metric'], [{ name: 'x', value: ['x1', 'x2'] }, { name: 'y', value: ['y1', 'y2'] }]);
    expect(interpolated).to.deep.equal([{ 'resource': '$node', 'metric': 'x1.y1' }, { 'resource': '$node', 'metric': 'x1.y2' }, { 'resource': '$node', 'metric': 'x2.y1' }, { 'resource': '$node', 'metric': 'x2.y2' }]);
  });

  it('should be able to interpolate multiple variables in multiple attributes', function () {
    var interpolated = (0, _interpolate.interpolate)(query, ['resource', 'metric'], [{ name: 'node', value: ['1', '2'] }, { name: 'x', value: ['x1', 'x2'] }, { name: 'y', value: ['y1', 'y2'] }]);
    expect(interpolated).to.deep.equal([{ 'resource': '1', 'metric': 'x1.y1' }, { 'resource': '1', 'metric': 'x1.y2' }, { 'resource': '1', 'metric': 'x2.y1' }, { 'resource': '1', 'metric': 'x2.y2' }, { 'resource': '2', 'metric': 'x1.y1' }, { 'resource': '2', 'metric': 'x1.y2' }, { 'resource': '2', 'metric': 'x2.y1' }, { 'resource': '2', 'metric': 'x2.y2' }]);
  });

  it('should support interpolating a special variable named $index which is unique for every row', function () {
    var queryWithIndex = { 'resource': 'node', 'metric': '$x.$y', 'label': '$index' };

    var interpolated = (0, _interpolate.interpolate)(queryWithIndex, ['resource', 'metric', 'label'], [{ name: 'x', value: ['x1', 'x2'] }, { name: 'y', value: ['y1', 'y2'] }]);

    expect(interpolated).to.deep.equal([{ 'resource': 'node', 'metric': 'x1.y1', 'label': 'idx0' }, { 'resource': 'node', 'metric': 'x1.y2', 'label': 'idx1' }, { 'resource': 'node', 'metric': 'x2.y1', 'label': 'idx2' }, { 'resource': 'node', 'metric': 'x2.y2', 'label': 'idx3' }]);
  });

  it('should be able to interpolate multiple variables with the same name in a single attribute', function () {
    var queryWithMultipleVariables = { 'resource': '$node', 'metric': '$x-var + $x-var' };
    var interpolated = (0, _interpolate.interpolate)(queryWithMultipleVariables, ['resource', 'metric'], [{ name: 'x-var', value: ['x1', 'x2'] }]);
    expect(interpolated).to.deep.equal([{ 'resource': '$node', 'metric': 'x1 + x1' }, { 'resource': '$node', 'metric': 'x2 + x2' }]);
  });
});
//# sourceMappingURL=perf_ds_interpolate_spec.js.map
