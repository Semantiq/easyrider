app.directive("navigator", function() {
	return {
	    transclude: true,
		restrict: "E",
		templateUrl: "/directives/navigator/template.html",
		scope: { },
		controller: ["$scope", "hotkeys", function($scope, hotkeys) {
            var columns = $scope.columns = [ ];
            this.addColumn = function(column) {
                if (columns.length === 0) {
                    column.list = column.entries({parent: null});
                    column.active = true;
                } else {
                    column.list = [];
                }
                column.index = columns.length;
                column.select = function(index) {
                    column.selectedRowIndex = index;
                    $scope.columns[$scope.selectedColumnIndex].active = false;
                    column.active = true;
                    $scope.selectedColumnIndex = column.index;
                    $scope.onMove();
                };
                column.selectedRowIndex = 0;
                columns.push(column);
            };
            $scope.selectedColumnIndex = 0;

            $scope.onMove = function() {
                var newColumn = $scope.selectedColumnIndex;
                if (newColumn + 1 < $scope.columns.length) {
                    var list = $scope.columns[newColumn + 1].entries({ parent: $scope.columns[newColumn].list[$scope.columns[newColumn].selectedRowIndex] });
                    $scope.columns[newColumn + 1].list = list;
                }
                for (var i = newColumn + 2; i < $scope.columns.length; i++) {
                    $scope.columns[i].list = [];
                }
            };

            hotkeys.bindTo($scope).add({
                combo: "left",
                description: "Navigate left",
                callback: function() {
                    var oldColumn = $scope.selectedColumnIndex;
                    var newColumn = Math.max(0, oldColumn - 1);
                    $scope.columns[oldColumn].active = false;
                    $scope.columns[newColumn].active = true;
                    $scope.selectedColumnIndex = newColumn;
                }
            }).add({
                combo: "right",
                description: "Navigate right",
                callback: function() {
                    var oldColumn = $scope.selectedColumnIndex;
                    var newColumn = Math.min($scope.columns.length - 1, oldColumn + 1);
                    if (oldColumn != newColumn && $scope.columns[newColumn].list.length > 0) {
                        $scope.columns[oldColumn].active = false;
                        $scope.columns[newColumn].active = true;
                        $scope.columns[newColumn].selectedRowIndex = 0;
                        $scope.selectedColumnIndex = newColumn;
                        $scope.onMove();
                    }
                }
            }).add({
                combo: "up",
                description: "Navigate up",
                callback: function() {
                    var newIndex = $scope.columns[$scope.selectedColumnIndex].selectedRowIndex - 1;
                    $scope.columns[$scope.selectedColumnIndex].selectedRowIndex = Math.max(newIndex, 0);
                    $scope.onMove();
                }
            }).add({
                combo: "down",
                description: "Navigate down",
                callback: function() {
                    $scope.columns[$scope.selectedColumnIndex].selectedRowIndex = Math.min($scope.columns[$scope.selectedColumnIndex].selectedRowIndex + 1, $scope.columns[$scope.selectedColumnIndex].list.length - 1);
                    $scope.onMove();
                }
            }).add({
                combo: "enter",
                description: "See details of selected item",
                callback: function() {
                    var selectedColumn = $scope.columns[$scope.selectedColumnIndex];
                    var entry = selectedColumn.list[selectedColumn.selectedRowIndex];
                    selectedColumn.details({entry: entry});
                }
            }).add({
                combo: "ctrl+n",
                description: "Add item",
                callback: function() {
                    var selectedColumn = $scope.columns[$scope.selectedColumnIndex];
                    var parent = null;
                    if ($scope.selectedColumnIndex > 0) {
                        var parentColumn = $scope.columns[$scope.selectedColumnIndex - 1];
                        parent = parentColumn.list[parentColumn.selectedRowIndex];
                    }
                    selectedColumn.add({parent: parent});
                }
            });
		}]
	};
});
