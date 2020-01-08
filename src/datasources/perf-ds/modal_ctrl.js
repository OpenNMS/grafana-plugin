class ModalCtrl {
  /** @ngInject */
  constructor($scope) {
    this.$scope = $scope;
    this.query = "";
    this.offset = 0;
    this.selectedRow = null;
    this.currentPage = 0;
    this.pageSize = 25;
    this.searchForRows();
  }

  searchForRows() {
    var self = this;
    this.searching = true;
    
    this.$scope.search(this.query, this.offset)
      .then(function (results) {
        // Reset the selected row
        self.selectedRow = null;
        // Add the results to the scope
        self.rows = results.rows;
        self.allRows = results.rows;
        self.count = results.count;
        self.totalCount = results.totalCount;
        self.numberOfPages= Math.ceil(self.totalCount/self.pageSize);                

        // We're done
        self.searching = false;
      }, function () {
        self.searching = false;
      });
  }

  getPageData(rows, startFrom){
    return rows.slice(startFrom)
  }

  startFrom(currentPage, pageSize) {
    return currentPage*pageSize;
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

  nextPage(){
    if((this.currentPage + 1) == this.$scope.ctrl.numberOfPages){
      return;
    }
    this.offset += 25;
    this.currentPage++;
    this.searchForRows();
  }

  prevPage(){
    if(this.currentPage == 0){
      return;
    }
    this.offset -= 25;
    this.currentPage--;
    this.searchForRows();
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
