app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/", {
		templateUrl: "/pages/index/template.html",
		controller: ["$scope", "Search", function($scope, Search) {
			$scope.Search = new Search.Query();
		}]
	});
}]);