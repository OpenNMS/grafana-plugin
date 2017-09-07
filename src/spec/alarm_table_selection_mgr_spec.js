import _ from 'lodash';
import {SelectionMgr} from '../panels/alarm-table/selection_mgr';

describe('Selection Manager', function() {
  let currentSelection;
  let mgr;

  beforeEach(function() {
    currentSelection = [];
    mgr = new SelectionMgr(
      (from, to) => _.range(from, to + 1),
      () => {currentSelection = mgr.getSelectedRows();}
      );
    expect(mgr.getSelectedRows()).to.have.length(0);
  });

  describe('clicks without ctrl or shift', function() {
    it('should change the selected row', function() {
      mgr.handleRowClick(1, false, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(2, false, false);
      expect(currentSelection).to.eql([2]);
    });
  });

  describe('clicks with ctrl and without shift', function() {
    it('should add rows to the current selection', function() {
      mgr.handleRowClick(1, true, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(2, true, false);
      expect(currentSelection).to.eql([1,2]);
      mgr.handleRowClick(5, true, false);
      expect(currentSelection).to.eql([1,2,5]);
    });

    it('should reset the selection when ctrl is not pressed', function() {
      mgr.handleRowClick(1, true, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(2, true, false);
      expect(currentSelection).to.eql([1,2]);
      mgr.handleRowClick(5, false, false);
      expect(currentSelection).to.eql([5]);
    });
  });

  describe('clicks with shift and without ctrl', function() {
    it('should select the range', function() {
      mgr.handleRowClick(1, false, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(5, false, true);
      expect(currentSelection).to.eql([1,2,3,4,5]);
    });
  });

  describe('clicks with shift and with ctrl', function() {
    it('should add rows to the current selection', function() {
      mgr.handleRowClick(1, true, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(10, true, false);
      expect(currentSelection).to.eql([1,10]);
      mgr.handleRowClick(15, true, true);
      expect(currentSelection).to.eql([1,10,11,12,13,14,15]);
    });

    it('should reset the selection when ctrl is not pressed', function() {
      mgr.handleRowClick(1, true, false);
      expect(currentSelection).to.eql([1]);
      mgr.handleRowClick(10, true, false);
      expect(currentSelection).to.eql([1,10]);
      mgr.handleRowClick(15, true, true);
      expect(currentSelection).to.eql([1,10,11,12,13,14,15]);
      mgr.handleRowClick(17, false, true);
      expect(currentSelection).to.eql([15,16,17]);
    });
  });

  describe('selection', function() {
    it('should support objects', function() {
      mgr.handleRowClick({source: 'a', index: 1}, false, false);
      expect(currentSelection).to.eql([{source: 'a', index: 1}]);
      expect(mgr.isRowSelected({source: 'a', index: 1})).to.equal(true);
    });
  });

});
