class ModalCtrl {
  constructor($scope) {
    this.$scope = $scope;
    this.query = "";
    this.selectedRow = null;

    this.searchForRows();
  }

  searchForRows() {
    var self = this;
    this.searching = true;
    this.$scope.search(this.query)
      .then(function (results) {
        // Reset the selected row
        self.selectedRow = null;
        // Add the results to the scope
        self.rows = results.rows;
        self.count = results.count;
        self.totalCount = results.totalCount;
        // We're done
        self.searching = false;
      }, function () {
        self.searching = false;
      });
  }

  setClickedRow(index) {
    if (this.selectedRow === index) {
      this.selectedRow = null;
    } else {
      this.selectedRow = index;
      // Keep a reference to the row when the selection is made
      this.row = this.rows[this.selectedRow];
    }
  }

  cancel() {
    this.$scope.result.reject();
    this.$scope.dismiss();
  }

  ok() {
    if (this.selectedRow !== null) {
      this.$scope.result.resolve(this.row);
    } else {
      this.$scope.result.reject();
    }
    this.$scope.dismiss();
  }
}

if (typeof angular !== 'undefined') {
  angular.module('grafana.controllers')
    .controller('OpenNMSModalSelectionCtrl', ModalCtrl);
}
