app.directive("selectNode", function() {
	return {
		restrict: "E",
		templateUrl: "/directives/selectNode/template.html",
		scope: {
			model: "="
		},
		controller: ["$scope", "Nodes", function($scope, Nodes) {
			$scope.Nodes = Nodes;
		}]
	};
});
