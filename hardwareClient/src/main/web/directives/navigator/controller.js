app.directive("navigator", function() {
	return {
	    transclude: true,
		restrict: "E",
		templateUrl: "/directives/navigator/template.html",
		scope: { },
		controller: ["$scope", "hotkeys", function($scope, hotkeys) {
            var columns = $scope.columns = [ ];
            this.addColumn = function(column) {
                columns.push(column);
                column.list = column.entries({parent: null});
            };
		}]
	};
});
