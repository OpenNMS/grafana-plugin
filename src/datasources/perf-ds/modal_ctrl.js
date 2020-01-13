class ModalCtrl {
  /** @ngInject */
  constructor($scope) {
    this.$scope = $scope;
    this.query = "";
    this.offset = 0;
    this.selectedRow = null;
    this.currentPage = 0;
    this.pageSize = 25;
    this.selfPagination = false; // Flag for client-side pagination
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
        self.allResult = results.rows;

        // Apply client-side pagination if result is > pageSize
        if(results.rows.length > self.pageSize){
          self.selfPagination = true;
          self.count = 0;
          self.updateSelfPageinatedData();
        }
        else {
          self.rows = results.rows;  // When paginated or limited result received
          self.count = (results.count ? results.count : self.rows.length) + self.offset;
        }

        self.totalCount = results.totalCount;
        self.numberOfPages= Math.ceil(self.totalCount/self.pageSize);                

        // We're done
        self.searching = false;
      }, function () {
        self.searching = false;
      });
  }

  updateSelfPageinatedData(){
    this.rows =  this.allResult.slice(this.offset, (this.offset + this.pageSize));
    this.count += this.rows.length;
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
      return; // Maximum pages reached
    }

    this.offset += this.pageSize;
    this.currentPage++;
    
    if(this.selfPagination){
      this.updateSelfPageinatedData();
    }
    else{
      this.searchForRows();
    }
  }

  prevPage(){
    if(this.currentPage == 0){
      return;
    }
    this.offset -= this.pageSize;
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
