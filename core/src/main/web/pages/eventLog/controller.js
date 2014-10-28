app.config(["$routeProvider", function($routeProvider) {
	$routeProvider.when("/eventLog", {
		templateUrl: "/pages/eventLog/template.html",
		controller: ["$scope", "$routeParams", function($scope, $routeParams) {
		}]
	});
}]);
