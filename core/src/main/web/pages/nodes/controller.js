app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/nodes", {
		templateUrl: "/pages/nodes/template.html",
		controller: ["$scope", "Nodes", function($scope, Nodes) {
			$scope.Nodes = Nodes;
		}]
	});
}]);
