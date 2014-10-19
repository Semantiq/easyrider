app.directive("navigator", function() {
	return {
	    transclude: true,
		restrict: "E",
		templateUrl: "/directives/navigator/template.html",
		scope: { },
		controller: ["$scope", "hotkeys", function($scope, hotkeys) {
            var columns = $scope.columns = [ "default" ];
            this.addColumn = function(column) {
                columns.push(column);
            };
		}]
	};
});
